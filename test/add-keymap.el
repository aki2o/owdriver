(require 'owdriver)
(require 'el-expectations)

(expectations
  (desc "add-keymap do nothing from not string")
  (expect nil
    (let ((owdriver-prefix-key "M-o")
          (owdriver--keymap-alist nil))
      (owdriver-add-keymap (current-buffer) 'owdriver-next-window)
      owdriver--keymap-alist))
  (desc "add-keymap do nothing from empty string")
  (expect nil
    (let ((owdriver-prefix-key "M-o")
          (owdriver--keymap-alist nil))
      (owdriver-add-keymap "" 'owdriver-next-window)
      owdriver--keymap-alist))
  (desc "add-keymap do nothing from not command")
  (expect nil
    (let ((owdriver-prefix-key "M-o")
          (owdriver--keymap-alist nil))
      (owdriver-add-keymap "C-o" 'owdriver-config-default)
      owdriver--keymap-alist))
  (desc "add-keymap add alist")
  (expect '(("C-o" . owdriver-next-window))
    (let ((owdriver-prefix-key "M-o")
          (owdriver--keymap-alist nil))
      (owdriver-add-keymap "C-o" 'owdriver-next-window)
      owdriver--keymap-alist))
  (desc "add-keymap add define key")
  (expect '("M-o C-o")
    (member "M-o C-o"
            (mapcar 'key-description
                    (where-is-internal 'owdriver-next-window owdriver-mode-map))))
  (desc "add-keymap update alist")
  (expect '(("C-o" . owdriver-previous-window)
            ("C-q" . owdriver-quit))
    (let ((owdriver-prefix-key "M-o")
          (owdriver--keymap-alist '(("C-o" . owdriver-next-window)
                                    ("C-q" . owdriver-quit))))
      (owdriver-add-keymap "C-o" 'owdriver-previous-window)
      owdriver--keymap-alist))
  (desc "add-keymap update define key")
  (expect t
    (and (not (member "M-o C-o" (mapcar 'key-description
                                        (where-is-internal 'owdriver-next-window owdriver-mode-map))))
         (member "M-o C-o" (mapcar 'key-description
                                   (where-is-internal 'owdriver-previous-window owdriver-mode-map)))
         t))
  )

