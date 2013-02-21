;;; files.el --- Work with files and directories.

;; Copyright (C) 2012, 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2012-12-26 12:58:05 UTC
;; Updated: 2013-02-18 14:04:09 IRKT

;;; Commentary:

;; TODO: add long description here.

;;; Code:

(eval-when-compile
  (require 'ido)
  (require 'recentf))

(defun ido-find-file-ignore-at-point ()
  "Ignore thing at point when "
  (interactive)
  (let ((ido-use-url-at-point nil)
        (ido-use-filename-at-point nil))
    (ido-find-file)))

;;; Local Variables:
;;; coding: utf-8
;;; time-stamp-time-zone: UTC
;;; time-stamp-format: "%:y-%02m-%02d %02H:%02M:%02S %Z"
;;; time-stamp-start: ";; Updated: "
;;; time-stamp-end: "$"
;;; time-stamp-line-limit: 20
;;; End:

(provide 'lib/files)

;;; files.el ends here
