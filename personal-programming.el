;;; personal-programming.el --- UI settings (colors and so on).

;; Copyright (C) 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2013-03-05 16:50:50 IRKT
;; Updated: 2013-04-06 08:53:30 UTC

;;; Code:

(defun personal-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(NOTE\\|WARN\\|WARNING\\|NB\\):"
          1 font-lock-warning-face t))))

(defun personal-prog-mode-defaults ()
  (unless (eq major-mode 'org-mode)
    (prelude-add-watchwords)))

(add-hook 'prog-mode-hook 'personal-prog-mode-defaults)

(use-package git-gutter-fringe
  :config
  (progn
    (setq git-gutter-fr:side 'right-fringe)
    (set-face-foreground 'git-gutter-fr:modified "orange")
    (set-face-foreground 'git-gutter-fr:added    "forest green")
    (set-face-foreground 'git-gutter-fr:deleted  "red")
    (global-git-gutter-mode t)))

(use-package semantic
  :config
  (progn
    (add-to-list 'semantic-default-submodes
                 'global-semantic-decoration-mode)
    (add-to-list 'semantic-default-submodes
                 'global-semantic-idle-summary-mode)
    (add-to-list 'semantic-default-submodes
                 'global-semantic-idle-local-symbol-highlight-mode)
    (add-to-list 'semantic-default-submodes
                 'global-semantic-mru-bookmark-mode))
  :init (semantic-mode +1))

(when (ignore-errors (require 'auto-complete-config nil t))
  (ac-config-default)
  (ac-flyspell-workaround)

  (eval-after-load "semantic"
    '(setq-default ac-sources
                   (cons 'ac-source-semantic ac-sources))))

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
