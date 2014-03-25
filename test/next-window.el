(require 'owdriver)
(require 'ert-expectations)

(expectations
  (desc "next-window one window")
  (expect t
    (let ((owdriver--window nil)
          (cnf (current-window-configuration))
          wnd)
      (delete-other-windows)
      (setq wnd (get-buffer-window))
      (owdriver-next-window)
      (set-window-configuration cnf)
      (eq wnd owdriver--window)))
  )

