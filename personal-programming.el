;;; personal-programming.el --- UI settings (colors and so on).

;; Copyright (C) 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2013-03-05 16:50:50 IRKT
;; Updated: 2013-10-25 15:07:58 UTC

;;; Code:

;; auto-scroll compilation output
(setq compilation-scroll-output t)

(defun personal-font-lock-comment-annotations ()
  (font-lock-add-keywords
   nil '(("\\<\\(NOTE\\|WARN\\|WARNING\\|NB\\):"
          1 font-lock-warning-face t))))

(defun personal-prog-mode-defaults ()
  (unless (eq major-mode 'org-mode)
    (personal-font-lock-comment-annotations))
  (local-set-key (kbd "<return>") 'newline-and-indent))

(add-hook 'prog-mode-hook 'personal-prog-mode-defaults)

(use-package git-gutter-fringe
  :diminish git-gutter-mode
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

(when (executable-find "ipython")
  (eval-after-load 'python
    '(setq python-shell-interpreter "ipython"
           python-shell-interpreter-args ""
           python-shell-prompt-regexp "In \\[[0-9]+\\]: "
           python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
           python-shell-completion-setup-code
           "from IPython.core.completerlib import module_completion"
           python-shell-completion-module-string-code
           "';'.join(module_completion('''%s'''))\n"
           python-shell-completion-string-code
           "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))

(provide 'personal-programming)

;;; personal-programming.el ends here
