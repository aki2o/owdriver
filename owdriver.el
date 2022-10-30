;;; owdriver.el --- Quickly perform various actions on other windows

;; Copyright (C) 2014  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aki2o/owdriver
;; Version: 0.2.0
;; Package-Requires: ((log4e "0.2.0") (yaxception "0.2.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; This extension provides the function for doing various action to
;; other windows quickly in multi window situation.
;; In default, that's move, scroll and isearch.
;; Moreover, you can add the action what you want.
;; Enjoy!!!

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'log4e)
(require 'yaxception)

(defgroup owdriver nil
  "Quickly perform various actions on other windows."
  :group 'convenience
  :prefix "owdriver-")

(defcustom owdriver-prefix-key "M-o"
  "String of the prefix keystroke for `owdriver-mode-map'."
  :type 'string
  :group 'owdriver)

(defcustom owdriver-next-window-prefer-pophint t
  "Whether to prefer to use `pophint:do' for `owdriver-next-window'."
  :type 'boolean
  :group 'owdriver)


(log4e:deflogger "owdriver" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                      (error . "error")
                                                      (warn  . "warn")
                                                      (info  . "info")
                                                      (debug . "debug")
                                                      (trace . "trace")))
(owdriver--log-set-level 'trace)


(defvar owdriver--window nil "Current window drived by the command of `owdriver-mode-map'.")
(defvar owdriver--move-window-amount nil)


;;;;;;;;;;;;;
;; Utility

(cl-defun owdriver--show-message (msg &rest args)
  (apply 'message (concat "[OWDRIVER] " msg) args)
  nil)

(defmacro owdriver--awhen (test &rest body)
  (declare (indent 1))
  `(let ((it ,test)) (when it ,@body)))

(defmacro owdriver--with-selected-window (command force-next-window &rest body)
  (declare (indent 2))
  `(yaxception:$
     (yaxception:try
       (owdriver--trace "start with select window : wnd[%s] force-next-window[%s]"
                        owdriver--window ,force-next-window)
       (let ((owdriver--move-window-amount 1)
             (owdriver-keep-driving-function (lambda (c) nil)))
         (owdriver-start ,force-next-window)
         ,@body))
     (yaxception:catch 'error e
       (owdriver--show-message "Failed %s : %s" ',command (yaxception:get-text e))
       (owdriver--error "failed %s : %s\n%s"
                       ',command
                       (yaxception:get-text e)
                       (yaxception:get-stack-trace-string e)))))

(defun owdriver--get-binding-keys (cmd)
  (owdriver--trace "start get binding keys : %s" cmd)
  (cl-loop for b in (where-is-internal cmd global-map)
           for bindkey = (or (ignore-errors (key-description b))
                             "")
           if (and (not (string= bindkey ""))
                   (not (string-match "\\`<menu-bar>" bindkey))
                   (not (string-match "\\`<[^>]*mouse[^>]*>" bindkey)))
           collect (progn (owdriver--trace "found binding : %s" bindkey)
                          bindkey)))

(defun owdriver--get-keybind (cmd)
  (owdriver--trace "start get keybind : %s" cmd)
  (cl-loop with ret = nil
           for k in (owdriver--get-binding-keys cmd)
           if (or (not ret)
                  (< (length k) (length ret)))
           do (setq ret k)
           finally return (progn (owdriver--trace "got keybind : %s" ret)
                                 ret)))


;;;;;;;;;;
;; Mode

(defvar owdriver-keep-driving-commands '(owdriver-start owdriver-next-window owdriver-previous-window))
(defvar owdriver-keep-driving-command-prefixes '("scroll-" "next-" "previous-" "forward-" "backward-" "beginning-of-" "end-of-" "move-" "switch-to-" "xref-" "find-" "isearch-" "project-" "projectile-"))
(defvar owdriver-keep-driving-command-regexp nil)
(defvar owdriver-keep-driving-function 'owdriver--keep-driving-with-default)

;;;###autoload
(defvar owdriver-mode-map (make-sparse-keymap))

(defvar owdriver--window-configuration nil)
(defvar owdriver--marker nil)
(defvar owdriver--keep-driving-function nil)

;;;###autoload
(define-minor-mode owdriver-mode
  "Quickly perform various actions on other windows."
  :init-value nil
  :lighter " OW"
  :keymap owdriver-mode-map
  :global t
  :group 'owdriver
  (if owdriver-mode
      (progn
        (setq owdriver--marker (set-marker (make-marker) (point) (current-buffer)))
        (setq owdriver--window-configuration (current-window-configuration))
        (setq owdriver--keep-driving-function owdriver-keep-driving-function)
        (add-hook 'pre-command-hook 'owdriver--cleanup))
    (when owdriver--marker
      (when  (and (not (window-live-p (get-buffer-window (marker-buffer owdriver--marker))))
                  owdriver--window-configuration)
        (set-window-configuration owdriver--window-configuration))
      (select-window (get-buffer-window (marker-buffer owdriver--marker)))
      (goto-char (marker-position owdriver--marker)))
    (setq owdriver--marker nil)
    (setq owdriver--window-configuration nil)
    (setq owdriver--keep-driving-function nil)
    (remove-hook 'pre-command-hook 'owdriver--cleanup)))


;;;;;;;;;;;;;;;;;;
;; User Command

(defun owdriver--keep-driving-with-default (command)
  (when (not owdriver-keep-driving-command-regexp)
    (setq owdriver-keep-driving-command-regexp
          (rx-to-string `(and bos (regexp ,(regexp-opt owdriver-keep-driving-command-prefixes))))))
  (or (memq command owdriver-keep-driving-commands)
      (string-match owdriver-keep-driving-command-regexp (symbol-name command))))

(defun owdriver--cleanup ()
  (when (and owdriver-mode
             (not (active-minibuffer-window))
             (functionp owdriver--keep-driving-function)
             (not (funcall owdriver--keep-driving-function this-command)))
    (message "start cleanup. this-command[%s]" this-command)
    (owdriver-mode 0)))

;;;###autoload
(defun owdriver-start (&optional force-next-window)
  "Start driving the window of `owdriver--window'."
  (interactive)
  (owdriver-mode 1)
  (when (or force-next-window
            (not (window-live-p owdriver--window)))
    (owdriver-next-window))
  (when (not (eq (get-buffer-window) owdriver--window))
    (select-window owdriver--window)))

;;;###autoload
(defun owdriver-next-window (&optional reverse)
  "Change the window of `owdriver--window'."
  (interactive)
  (yaxception:$
    (yaxception:try
      (let* ((actwnd (get-buffer-window))
             (currwnd (if (window-live-p owdriver--window) owdriver--window actwnd))
             (move-amount (or owdriver--move-window-amount
                              (when (window-live-p owdriver--window) 1)
                              2))
             (is-nextable-window (lambda (w)
                                   (and (window-live-p w)
                                        (not (eq w actwnd))
                                        (not (eq w currwnd))
                                        (not (minibufferp (window-buffer w))))))
             nextwnd popwnd wndloc)
        (select-window currwnd)
        (owdriver--trace "start %s window. currwnd[%s] move-amount[%s]"
                         (if reverse "previous" "next") (selected-window) move-amount)
        ;; Move to next target window
        (if (and (and owdriver-next-window-prefer-pophint
                      (featurep 'pophint)
                      (boundp 'pophint--next-window-source)
                      (>= (cl-loop for w in (window-list) count (funcall is-nextable-window w)) 2)))
            (setq nextwnd (when-let ((hint (pophint:do :source pophint--next-window-source :allwindow t)))
                            (pophint:hint-window hint)))
          (while (and (> move-amount 0)
                      (not (eq nextwnd currwnd)))
            (other-window (if reverse -1 1))
            (setq nextwnd (get-buffer-window))
            (owdriver--trace "selected next window : %s" nextwnd)
            (when (funcall is-nextable-window nextwnd)
              (cl-decf move-amount)
              (owdriver--trace "decremented move-amount[%s]" move-amount))))
        ;; Blink target window after move
        (when (not (eq nextwnd currwnd))
          (owdriver--trace "start blink window : %s" nextwnd)
          (let ((ov (make-overlay (window-start) (window-end))))
            (yaxception:$
              (yaxception:try
                (overlay-put ov 'face 'highlight)
                (select-window actwnd)
                (sit-for 0.1)
                (select-window nextwnd))
              (yaxception:catch 'error e
                (yaxception:throw e))
              (yaxception:finally
                (delete-overlay ov)))))
        (setq owdriver--window nextwnd)
        (owdriver--show-message "Drived window is '%s'" owdriver--window)))
    (yaxception:catch 'error e
      (owdriver--show-message "Failed next window : %s" (yaxception:get-text e))
      (owdriver--error "failed next window : %s\n%s"
                      (yaxception:get-text e)
                      (yaxception:get-stack-trace-string e)))))

;;;###autoload
(defun owdriver-previous-window ()
  "Change the window of `owdriver--window'."
  (interactive)
  (owdriver-next-window t))

;;;###autoload
(defun owdriver-focus-window ()
  "Quit driving `owdriver--window' and move to `owdriver--window'."
  (interactive)
  (when (window-live-p owdriver--window)
    (select-window owdriver--window)
    (keyboard-quit)))

;;;###autoload
(defun owdriver-quit ()
  "Quit driving `owdriver--window'."
  (interactive)
  (keyboard-quit))


;;;;;;;;;;;;;;;
;; For Setup

;;;###autoload
(defun owdriver-add-keymap (keystroke command)
  "Add the keymap of `owdriver-mode-map'."
  (owdriver--trace "start add keymap. keystroke[%s] command[%s]" keystroke command)
  (when (and (stringp keystroke)
             (not (string= keystroke ""))
             (commandp command))
    (define-key owdriver-mode-map (read-kbd-macro keystroke) command)))

;;;###autoload
(defmacro owdriver-define-command (command &rest body)
  "Define the command for driving `owdriver--window' from COMMAND.

The command named `owdriver-do-COMMAND' is defined by this function.
BODY is sexp. If COMMAND is used in `owdriver--window' actually, this value is no need."
  (declare (indent 2))
  (let* ((body (or body `((call-interactively ',command))))
         (cmdnm (symbol-name command))
         (ncommand (intern (concat "owdriver-do-" cmdnm)))
         (fcommand (intern (concat "owdriver-do-" cmdnm "-on-next-window"))))
    `(progn
       (owdriver--trace "start define command[%s]" ,cmdnm)
       ;;;###autoload
       (defun ,ncommand (&optional arg)
         ,(format "Do `%s' in `owdriver--window'.\n\nIf prefix argument is given, do `owdriver-next-window' before that." cmdnm)
         (interactive "p")
         (let ((force-next-window (and arg (> arg 1))))
           (owdriver--with-selected-window ,ncommand force-next-window
             ,@body)))
       ;;;###autoload
       (defun ,fcommand ()
         ,(format "Do `%s' in `owdriver--window' with `owdriver-next-window'." cmdnm)
         (interactive)
         (owdriver--with-selected-window ,fcommand t
           ,@body)))))

;;;###autoload
(defun owdriver-config-default ()
  "Do the recommended configuration."
  ;; Own command
  (owdriver-add-keymap "C-o"     'owdriver-next-window)
  (owdriver-add-keymap "C-S-o"   'owdriver-previous-window)
  (owdriver-add-keymap "C-c C-k" 'owdriver-focus-window)
  (owdriver-add-keymap "C-c C-c" 'owdriver-quit)
  ;; Basic command
  (owdriver-define-command scroll-up)
  (owdriver-define-command scroll-up-command)
  (owdriver-define-command scroll-down)
  (owdriver-define-command scroll-down-command)
  (owdriver-define-command scroll-left (scroll-left 10 t))
  (owdriver-define-command scroll-right (scroll-right 10 t))
  (owdriver-define-command move-beginning-of-line)
  (owdriver-define-command move-end-of-line)
  (owdriver-define-command beginning-of-buffer)
  (owdriver-define-command end-of-buffer)

  ;; Patch for Emacs 26.1
  (when (>= emacs-major-version 26)
    (defun owdriver--patch-on-26-1 ()
      "Function to patch the trouble on GNU Emacs 26.1 (build 1, x86_64-apple-darwin14.5.0, NS appkit-1348.17 Version 10.10.5 (Build 14F2511)) of 2018-05-31,
which emacs seems to not refresh a screen when `scroll-left', `scroll-right' with `with-selected-window'."
      (let ((wnd (get-buffer-window))
            (pt (with-selected-window owdriver--window (point))))
        (select-window owdriver--window)
        (forward-char)
        (when (= (point) pt) (backward-char))
        (select-window wnd)
        (select-window owdriver--window)
        (goto-char pt)
        (select-window wnd)))

    (defadvice owdriver-do-scroll-left (after owdriver-patch activate) (owdriver--patch-on-26-1))
    (defadvice owdriver-do-scroll-right (after owdriver-patch activate) (owdriver--patch-on-26-1))))


(provide 'owdriver)
;;; owdriver.el ends here
