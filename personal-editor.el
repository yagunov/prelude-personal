;;; personal-editor.el --- Editing functions.

;; Copyright (C) 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2013-02-19 14:03:11 IRKT
;; Updated: 2013-09-25 07:44:30 UTC

;;; Code:

(require 'use-package)
(require 'key-chord)

(require 'lib/editing)
(require 'lib/files)

;; do not suspend emacs frame from keyboard
(unbind-key "C-z")
(unbind-key "C-x C-z")

;; too easy to press accidentally
(unbind-key "C-x C-c")
(bind-key "C-x <f10>" 'save-buffers-kill-emacs)

(bind-key "C-w" 'delete-word-or-region)
(bind-key "C-x \\" 'align-regexp)

;; Open files with root privileges
(bind-key "C-x F" 'find-file-as-root)

;; more visual goto-line
(bind-key "M-g g" 'goto-line-with-feedback)
(bind-key "M-g M-g" 'goto-line-with-feedback)

;; leave C-h and M-h for deletion
(bind-key "C-h" 'delete-backward-char)
(bind-key "M-h" 'backward-kill-word)
(key-chord-define-global "DD" help-map)

;; diff buffer and file on disk or use VCS diff
(bind-key "C-x v =" 'dwim-diff)

;; Search:
(bind-key "C-o" 'isearch-occur isearch-mode-map)
(bind-key* "M-o" 'smart-occur)
(bind-key "C-x i" 'imenu)
(bind-key "C-x C-i" 'imenu)

(use-package aok
  ;; TODO: Write a wrapper for automatic default current mode and text
  ;;   selection.
  :bind (("M-O" . occur-select)))

(defun personal-anzu-update-func (here total)
  (propertize (format "<%d/%d>" here total)
              'face '((:foreground "yellow" :weight bold))))

(use-package anzu
  :config (setq anzu-mode-line-update-function 'personal-anzu-update-func)
  :init (global-anzu-mode))

;; Text navigation
(bind-key "C-a" 'smart-beginning-of-line)
(bind-key "C-e" 'smart-end-of-line)
(bind-key "C-<return>" 'jump-out-of-block)
(bind-key "M-N" 'next-error)
(bind-key "M-P" 'previous-error)

;; Selection:
(bind-key "C-SPC" 'smart-set-mark-command)

;; avoid accidental insertion of more then one space
(bind-key "SPC" 'just-one-space)
(bind-key "M-SPC" (lambda () (interactive) (insert " ")))

;; Save stuff with C-x s ...:
(defvar ctl-x-s-map (make-sparse-keymap)
  "Keymap for subcommands of C-x s (saveing stuff).")
(defalias 'ctl-x-s-prefix ctl-x-s-map)
(define-key ctl-x-map "s" 'ctl-x-s-prefix)
(bind-key "C-x s r" 'write-region)
(bind-key "C-x s a" 'save-some-buffers)


;; Update timestamps and copyright in file headers.
(require 'time-stamp)
(require 'copyright)
(add-hook 'before-save-hook
          '(lambda ()
             (copyright-update nil t)
             (let ((time-stamp-time-zone "UTC"))
               (time-stamp))))

(use-package whitespace
  :init (progn
          (setq whitespace-style '(face trailing lines-tail tabs)
                whitespace-line-column fill-column)
          (add-to-list 'prog-mode-hook 'whitespace-mode)))

(use-package diff-mode
  :config
  (progn
    ;; git like output for diff
    (setq diff-switches "-Nau")
    (add-hook 'diff-mode-hook
              '(lambda () (read-only-mode t)))))

;; Recently used files.
(use-package recentf
  :defer t
  :bind ("C-x f" . prelude-recentf-ido-find-file)
  :init
  (progn
    (setq recentf-max-saved-items 1000
          recentf-exclude '("/tmp/" "/ssh:"))
    (recentf-mode)))

(use-package dired
  :config
  (progn
    ;; show human readable file sizes
    (setq dired-listing-switches "-lh")
    ;; delete recursively without asking
    (setq dired-recursive-deletes 'always)
    ;; move between subdirs
    (bind-key "(" 'backward-page dired-mode-map)
    (bind-key ")" 'forward-page dired-mode-map)
    (bind-key "'" 'dired-hide-subdir dired-mode-map)))

(eval-after-load 'flyspell
  '(define-key flyspell-mode-map (kbd "C-;") nil))

(use-package accelerate
  :init
  (progn
    (accelerate previous-line 5)
    (accelerate next-line 5)
    (accelerate backward-char 3)
    (accelerate forward-char 3)
    (accelerate dired-previous-line 4)
    (accelerate dired-next-line 4)))

(use-package second-sel
  :bind (("C-M-y" . secondary-dwim)
         ("C-SPC" . smart-set-mark-command)))

(use-package anchored-transpose
  :bind ("C-t" . smart-transpose))

(use-package browse-kill-ring
  :requires browse-kill-ring+           ; TODO: read more about it's features
  :bind ("M-y" . browse-kill-ring)
  :config (setq browse-kill-ring-quit-action
                '(lambda ()
                   (bury-buffer)
                   (other-window 1))))

(use-package tramp
  :config (setq tramp-persistency-file-name (expand-file-name "tramp" prelude-savefile-dir)
                tramp-backup-directory-alist backup-directory-alist
                tramp-auto-save-directory (cdar backup-directory-alist)))

(use-package highlight-symbol
  :bind (("C-c m"   . highlight-symbol-at-point)
         ("C-c M"   . highlight-symbol-remove-all)
         ("C-c M-m" . highlight-symbol-remove-all)
         ("C-c C-r" . highlight-symbol-query-replace)
         ("C-S-n"   . highlight-symbol-next)
         ("C-S-p"   . highlight-symbol-prev))
  :config (setq highlight-symbol-colors
                '("orange" "brown" "dark cyan" "MediumPurple1" "dark green"
                  "DarkOrange" "HotPink1" "RoyalBlue1" "OliveDrab")))

(use-package direx
  :bind (("C-c j"     . direx:jump-to-directory)
         ("C-c C-j"   . direx:jump-to-directory-other-window)
         ("C-c p j"   . projectile-direx-jump)
         ("C-c p C-j" . projectile-direx-jump-other-window)))

(setq projectile-known-projects-file
      (expand-file-name "projectile-bookmarks.eld"
                        prelude-savefile-dir))

;; Extend projectile commands:
(bind-key "C-c p !" 'projectile-shell-command)
(bind-key "C-c p &" 'projectile-async-shell-command)
(bind-key "C-c p m" 'projectile-eshell)
(bind-key "C-c p M-m" 'projectile-shell)
(bind-key "C-c p x" 'projectile-mx)
(bind-key* "C-c p s" 'projectile-switch-project-fix)
(bind-key "C-c p c" 'projectile-compile-project-manual)

(provide 'personal-editor)

;;; personal-editor.el ends here
