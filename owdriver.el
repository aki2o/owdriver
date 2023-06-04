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
  "Whether to prefer to use `pophint:do' for `owdriver-find-next-window'."
  :type 'boolean
  :group 'owdriver)

(defcustom owdriver-next-window-function 'owdriver-find-next-window
  "Function to find a next window handled by owdriver."
  :type 'function
  :group 'owdriver)

(defcustom owdriver-keep-driving-commands '(owdriver-start owdriver-next-window owdriver-previous-window)
  "List of command kept handling by `owdriver-keep-driving-p'."
  :type (list 'function)
  :group 'owdriver)

(defcustom owdriver-keep-driving-command-prefixes '("scroll-" "next-" "previous-" "forward-" "backward-" "beginning-of-" "end-of-" "move-" "switch-to-" "xref-" "find-" "isearch-" "project-" "projectile-")
  "List of command prefix kept handling by `owdriver-keep-driving-p'.
This value will be ignored if set `owdriver-keep-driving-command-regexp' non-nil."
  :type (list 'string)
  :group 'owdriver)

(defcustom owdriver-keep-driving-command-regexp nil
  "Regexp for matching commands kept handling by `owdriver-keep-driving-p'."
  :type 'regexp
  :group 'owdriver)

(defcustom owdriver-keep-driving-function 'owdriver-keep-driving-p
  "Function to judge to keep handling by owdriver."
  :type 'function
  :group 'owdriver)


(log4e:deflogger "owdriver" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                      (error . "error")
                                                      (warn  . "warn")
                                                      (info  . "info")
                                                      (debug . "debug")
                                                      (trace . "trace")))
(owdriver--log-set-level 'trace)


(defvar owdriver--window nil "Current window drived by the command of `owdriver-mode-map'.")
(defvar owdriver--start-location nil)
(defvar owdriver--keep-driving-function nil)


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
       (let ((owdriver-keep-driving-function nil))
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

(defun owdriver--cleanup ()
  (when (and owdriver-mode
             (or (not (functionp owdriver--keep-driving-function))
                 (not (funcall owdriver--keep-driving-function this-command))))
    (owdriver--trace "start cleanup. this-command[%s]" this-command)
    (owdriver-mode 0)))


;;;;;;;;;;;;;;
;; Function

(defun owdriver-find-next-window (reverse)
  (let* ((actwnd (get-buffer-window))
         (currwnd (if (window-live-p owdriver--window) owdriver--window actwnd))
         (is-nextable-window (lambda (w)
                               (and (window-live-p w)
                                    (not (eq w actwnd))
                                    (not (eq w currwnd))
                                    (not (minibufferp (window-buffer w)))))))
    (or (and owdriver-next-window-prefer-pophint
             (featurep 'pophint)
             (boundp 'pophint--next-window-source)
             (>= (cl-loop for w in (window-list) count (funcall is-nextable-window w)) 2)
             (when-let ((hint (pophint:do :source pophint--next-window-source :allwindow t)))
               (pophint:hint-window hint)))
        (progn
          (select-window currwnd)
          (other-window (if reverse -1 1))
          (selected-window)))))

(defun owdriver-keep-driving-p (command)
  (when (not owdriver-keep-driving-command-regexp)
    (setq owdriver-keep-driving-command-regexp
          (rx-to-string `(and bos (regexp ,(regexp-opt owdriver-keep-driving-command-prefixes))))))
  (or (memq command owdriver-keep-driving-commands)
      (string-match owdriver-keep-driving-command-regexp (symbol-name command))))


;;;;;;;;;;
;; Mode

;;;###autoload
(defvar owdriver-mode-map (make-sparse-keymap))

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
        (setq owdriver--start-location `(:window ,(selected-window) :point ,(window-point), :config ,(current-window-configuration)))
        (setq owdriver--keep-driving-function owdriver-keep-driving-function)
        (add-hook 'pre-command-hook 'owdriver--cleanup))
    (when owdriver--start-location
      (when (not (window-live-p (plist-get owdriver--start-location :window)))
        (set-window-configuration (plist-get owdriver--start-location :config)))
      (select-window (plist-get owdriver--start-location :window))
      (set-window-point (plist-get owdriver--start-location :window) (plist-get owdriver--start-location :point))
      (setq owdriver--start-location nil))
    (setq owdriver--keep-driving-function nil)
    (remove-hook 'pre-command-hook 'owdriver--cleanup)))


;;;;;;;;;;;;;;;;;;
;; User Command

;;;###autoload
(defun owdriver-start (&optional force-next-window)
  "Start driving the window of `owdriver--window'."
  (interactive)
  (owdriver-mode 1)
  (when (or force-next-window
            (not (window-live-p owdriver--window)))
    (owdriver-next-window))
  (when (not (eq (selected-window) owdriver--window))
    (select-window owdriver--window)))

;;;###autoload
(defun owdriver-next-window (&optional reverse)
  "Change the window of `owdriver--window'."
  (interactive)
  (yaxception:$
    (yaxception:try
      (setq owdriver--window (funcall owdriver-next-window-function reverse))
      (select-window owdriver--window)
      (lexical-let ((ov (make-overlay (point-min) (point-max))))
        (overlay-put ov 'face 'highlight)
        (run-with-idle-timer 0.1 nil (lambda () (when ov (delete-overlay ov))))
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
