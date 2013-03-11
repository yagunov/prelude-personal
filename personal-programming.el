;;; personal-programming.el --- UI settings (colors and so on).

;; Copyright (C) 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2013-03-05 16:50:50 IRKT
;; Updated: 2013-03-05 16:50:56 IRKT

;;; Code:

(defun personal-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(NOTE\\|WARN\\|WARNING\\):"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'personal-add-watchwords)

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
