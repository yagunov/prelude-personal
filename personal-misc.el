;;; personal-misc.el --- Miscellaneous addons

;; Copyright (C) 2012, 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2013-02-19 13:16:36 IRKT
;; Updated: 2013-08-08 16:46:34 UTC

;;; Code:

(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.s$" . asm-mode))
(add-to-list 'auto-mode-alist '("\\.l[i]?st$" . asm-mode))

(defun english-text-p ()
  (let ((beg (point)))
    (save-excursion
      (save-restriction
        (if (region-active-p)
            (narrow-to-region (region-beginning) (region-end))
          (backward-word)
          (narrow-to-region beg (point)))
        (beginning-of-buffer)
        (looking-at ".*[A-Za-z]+.*")))))

(defun smart-google-translate ()
  (interactive)
  (if (english-text-p)
      (let ((google-translate-default-source-language "en")
            (google-translate-default-target-language "ru"))
        (call-interactively 'google-translate-at-point))
    (call-interactively 'google-translate-at-point)))

(use-package google-translate
  :init (require 'google-translate)
  :config (setq google-translate-enable-ido-completion t
                google-translate-default-source-language "auto"
                google-translate-default-target-language "en")
  :bind (("C-x t" . smart-google-translate)))

(provide 'personal-misc)

;;; personal-misc.el ends here
