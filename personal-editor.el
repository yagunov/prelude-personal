;;; personal-editor.el --- Editing functions.

;; Copyright (C) 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2013-02-19 14:03:11 IRKT
;; Updated: 2013-02-20 21:43:37 IRKT

;;; Code:

(require 'use-package)

(require 'lib/editing)
(require 'lib/files)

;; do not suspend emacs frame from keyboard
(unbind-key "C-z")
(unbind-key "C-x C-z")

;; too easy to press accidentally
(unbind-key "C-x C-c")

(bind-key "C-w" 'delete-word-or-region)
(bind-key "C-x \\" 'align-regexp)

;; more visual goto-line
(bind-key "M-g g" 'goto-line-with-feedback)
(bind-key "M-g M-g" 'goto-line-with-feedback)

;; leave C-h and M-h for deletion
(bind-key "C-h" 'delete-backward-char)
(bind-key "M-h" 'backward-kill-word)
(bind-key "C-x ?" help-map)

;; diff buffer and file on disk or use VCS diff
(bind-key "C-x v =" 'dwim-diff)

;; search
(bind-key "C-o" 'isearch-occur isearch-mode-map)
(bind-key "C-x i" 'imenu)
(bind-key "C-x C-i" 'imenu)

(bind-key "C-SPC" 'smart-set-mark-command)

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
  :require browse-kill-ring+            ; TODO: read more about it's features
  :bind ("M-y" . browse-kill-ring)
  :config (setq browse-kill-ring-quit-action
                '(lambda ()
                   (bury-buffer)
                   (other-window 1))))

;;; Local Variables:
;;; coding: utf-8
;;; time-stamp-format: "%:y-%02m-%02d %02H:%02M:%02S %Z"
;;; time-stamp-start: ";; Updated: "
;;; time-stamp-end: "$"
;;; time-stamp-line-limit: 20
;;; End:

(provide 'personal-editor)

;;; personal-editor.el ends here
