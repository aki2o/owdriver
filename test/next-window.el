(require 'owdriver)
(require 'el-expectations)

(expectations
  (desc "next-window one window")
  (expect t
    (let ((owdriver--window nil)
          (owdriver-next-window-prefer-pophint nil)
          (cnf (current-window-configuration))
          wnd)
      (delete-other-windows)
      (setq wnd (get-buffer-window))
      (owdriver-next-window)
      (set-window-configuration cnf)
      (eq wnd owdriver--window)))
  )

