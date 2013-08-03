;;; personal-general.el --- Built-in features configuration.

;; Copyright (C) 2012, 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2012-06-04 04:35:13 UTC
;; Updated: 2013-07-02 13:07:37 UTC

;;; Code:

(require 'use-package)
(require 'bind-key)
(require 'lib/helpers)

;; always end a file with a newline
(setq require-final-newline t)
;; use only spaces for indent
(setq-default indent-tabs-mode nil)
(setq visible-bell nil
      mouse-yank-at-point t)
;; set maximum line's length for auto-fill-mode
(setq-default fill-column 80)
;; set monday as the first day of week
(setq calendar-week-start-day 1)
;; read-only prompt in REPL buffers (i.e. R, SQL and so on)
(setq-default comint-prompt-read-only t)
;; remain only last line after buffer truncation (with `comint-truncate-buffer'
;; and `eshell-truncate-buffer')
(setq comint-buffer-maximum-size 0
      eshell-buffer-maximum-lines 0)
;; don't clutter up directories with files~
(setq backup-directory-alist (list (cons "." (path-user "backups"))))

;; Set language environment and coding system
(set-language-environment "Russian")
(set-default-coding-systems 'utf-8)
(set-coding-system-priority 'utf-8 'koi8-r 'cp1251 'cp866)

(setq-default tab-width 4)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)
;; I always forget the name of this function.
(defalias 'run-elisp 'ielm)

;; Seed the random-number generator.
(random t)

;;; Local Variables:
;;; coding: utf-8
;;; time-stamp-time-zone: UTC
;;; time-stamp-format: "%:y-%02m-%02d %02H:%02M:%02S %Z"
;;; time-stamp-start: ";; Updated: "
;;; time-stamp-end: "$"
;;; time-stamp-line-limit: 20
;;; End:

(provide 'personal-general)

;;; personal-general.el ends here
