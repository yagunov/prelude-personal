;;; personal-cc-mode.el --- C/C++/Java/... settings

;; Copyright (C) 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2013-02-20 16:27:05 IRKT
;; Updated: 2013-08-08 04:52:08 UTC

;;; Code:

(require 'cc-mode)

(defun personal-c-style-comments ()
  (setq comment-start "/* ")
  (setq comment-end " */"))

(defun personal-cc-mode-indent-style ()
  "My tweaks to CC-mode indent style."
  ;; Long function arguments indentation like in python-mode.
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0)
  ;; Do not indent lines inside 'extern "C"' constructs.
  (c-set-offset 'inextern-lang 0))

(defun personal-c-mode-common-hook ()
  ;; always use spaces
  (setq indent-tabs-mode nil)
  (cwarn-mode t)
  (c-subword-mode t)
  (personal-cc-mode-indent-style)
  ;; new line behavior
  (c-toggle-auto-newline +1)
  (local-set-key (kbd "RET") 'newline-and-indent))

;; replace Prelude's default hook with mine
(setq prelude-c-mode-common-hook 'personal-c-mode-common-hook)

(setq c-default-style '((java  . "java")
                        (awk   . "awk")
                        (other . "k&r")))

(add-hook 'c++-mode-hook
          '(lambda()
             (setq indent-prev-line nil)
             (personal-c-style-comments)
             ;; do not indent inline functions
             (c-set-offset 'inline-open '0)))

;; additional keywords (Qt)
(font-lock-add-keywords
 'c++-mode
 (list '("\\<\\(foreach\\)\\>" . font-lock-keyword-face)))

(add-hook 'java-mode-hook
          '(lambda()
             (c-set-style "java")
             (personal-c-arglist)
             (personal-c-style-comments)))

;;; Local Variables:
;;; coding: utf-8
;;; time-stamp-format: "%:y-%02m-%02d %02H:%02M:%02S %Z"
;;; time-stamp-start: ";; Updated: "
;;; time-stamp-end: "$"
;;; time-stamp-line-limit: 20
;;; End:

(provide 'personal-cc-mode)

;;; personal-cc-mode.el ends here
