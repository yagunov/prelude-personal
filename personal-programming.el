;;; personal-programming.el --- UI settings (colors and so on).

;; Copyright (C) 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2013-03-05 16:50:50 IRKT
;; Updated: 2013-03-17 23:40:07 IRKT

;;; Code:

(defun personal-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(NOTE\\|WARN\\|WARNING\\):"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'personal-add-watchwords)

(use-package git-gutter-fringe
  :config
  (progn
    (setq git-gutter-fr:side 'right-fringe)
    (set-face-foreground 'git-gutter-fr:modified "orange")
    (set-face-foreground 'git-gutter-fr:added    "forest green")
    (set-face-foreground 'git-gutter-fr:deleted  "red")
    (global-git-gutter-mode t)))

;;; Local Variables:
;;; coding: utf-8
;;; time-stamp-time-zone: UTC
;;; time-stamp-format: "%:y-%02m-%02d %02H:%02M:%02S %Z"
;;; time-stamp-start: ";; Updated: "
;;; time-stamp-end: "$"
;;; time-stamp-line-limit: 20
;;; End:

(provide 'personal-programming)

;;; personal-programming.el ends here
