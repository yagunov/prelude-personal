;;; personal-lookandfeel.el --- UI settings (colors and so on).

;; Copyright (C) 2012, 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2012-06-04 04:31:13 UTC
;; Updated: 2013-08-09 04:25:31 UTC

;;; Code:

(require 'use-package)

(scroll-bar-mode 0)
;; set window side margin in X
(set-fringe-mode '(10 . 5))             ; left — 10px, right — 5px
(setq-default indicate-buffer-boundaries 'left
              indicate-empty-lines t)
;; fixed width for line numbers bar
(use-package linum+)

(when window-system
  ;; what to display in window title
  (setq frame-title-format '("Emacs: "
                             (:eval (abbreviate-file-name buffer-file-name)
                                    "%f" ("%b"))))
  ;; make emacs use the clipboard
  (setq x-select-enable-primary t
        x-select-enable-clipboard t))

(defun personal-set-fonts (frame fonts)
  "Set custom fonts."
  (with-selected-frame frame
    (set-frame-font (car fonts))
    (dolist (f (cdr fonts))
      (if (listp f)
          (set-face-font (car f) (cdr f))))))

;;; Set font based on hostname
(lexical-let
    ((fonts
      (cond ((string= system-name "andrey-desktop")
             '("Consolas 10"
               (mode-line . "Consolas 9")
               (mode-line-inactive . "Consolas 9")))
            ((string= system-name "fusion")
             '("Consolas 8"
               (mode-line . "Consolas 7")
               (mode-line-inactive . "Consolas 7"))))))
  (personal-set-fonts (selected-frame) fonts)
  (defun personal-set-frame-fonts-hook (frame)
    (personal-set-fonts frame fonts))
  (add-hook 'after-make-frame-functions 'personal-set-frame-fonts-hook))

;; Solarized config:
(setq solarized-italic nil
      solarized-contrast 'high
      solarized-diff-mode 'high)

;; FIXME: Quick workaround for load-theme problem.
(setq .solarized-dark-loaded nil
      .solarized-light-loaded nil)

(setq .current-theme nil)

(defun dark ()
  "Enable dark color theme."
  (interactive)
  (unless .solarized-dark-loaded
    (load (path (el-get-package-directory 'solarized-theme)
                "solarized-dark-theme.el"))
    (setq .solarized-dark-loaded t))
  (enable-theme 'solarized-dark)
  (custom-set-faces
   '(font-lock-doc-string-face ((t (:background "#00323E" :foreground "#657B83"))) t)
   '(secondary-selection ((t (:background "#00261C"))))
   '(semantic-tag-boundary-face ((t (:overline "saddle brown"))))
   '(which-func ((t (:background "black" :foreground "green3"))))
   '(diff-refine-added ((t (:background "black" :foreground "turquoise"))))
   '(diff-refine-removed ((t (:background "black" :foreground "salmon")))))
  (setq .current-theme 'dark))

(defun light ()
  "Enable light color theme."
  (interactive)
  (unless .solarized-light-loaded
    (load (path (el-get-package-directory 'solarized-theme)
                "solarized-light-theme.el"))
    (setq .solarized-light-loaded t))
  (enable-theme 'solarized-light)
  (custom-set-faces
   '(font-lock-doc-string-face ((t (:background "#DED8C7" :foreground "#657B83"))) t)
   '(secondary-selection ((t (:background "#D0B263"))))
   '(semantic-tag-boundary-face ((t (:overline "saddle brown"))))
   '(which-func ((t (:background "black" :foreground "green3"))))
   '(diff-refine-added ((t (:background "black" :foreground "turquoise"))))
   '(diff-refine-removed ((t (:background "black" :foreground "salmon")))))
  (setq .current-theme 'light))

(defun toggle-theme ()
  "Toggle between dark and light color themes."
  (interactive)
  (if (eq .current-theme 'dark)
      (light)
    (dark)))

;; Load dark theme on start-up
(dark)

;; Increase font in minibuffer when it's active
(add-hook 'minibuffer-setup-hook
          '(lambda ()
             (text-scale-increase +1)))

;; Use ido as much as possible.
(use-package ido
  :requires ido-ubiquitous
  :requires ido-yes-or-no
  :requires lib/files
  :init (progn
          (ido-mode t)
          (ido-everywhere t)
          (ido-ubiquitous-mode t)
          (ido-yes-or-no-mode t))
  :bind ("C-x M-f" . ido-find-file-ignore-at-point)
  :config
  (setq
   ido-show-dot-for-dired t
   ;; show buffers from the past in buffer list
   ido-use-virtual-buffers t
   ;; try to use thing at point as an argument
   ido-use-url-at-point t
   ido-use-filename-at-point 'guess
   ;; display ido results vertically, rather than horizontally
   ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]"
                     " [Matched]" " [Not readable]" " [Too big]"
                     " [Confirm]")))

(use-package smex
  :init (smex-initialize)
  :bind (("M-x"     . smex)
         ("M-X"     . smex-major-mode-commands)
         ("C-x C-m" . smex))
  :config (setq smex-save-file (expand-file-name "smex"
                                                 prelude-savefile-dir)))

(use-package ibuffer
  :requires ibuf-ext
  :bind (("C-x C-b" . ibuffer-bs-show)
         ("C-x M-b" . ibuffer))
  :init
  ;; highlight current line in buffer menu
  (add-hook 'ibuffer-mode-hook 'hl-line-mode))

;; Hide uninformative minor modes indicators from mode-line
(use-package diminish
  :disabled t                           ; FIXME: executed before all modes are
                                        ; loaded
  :config
  (-each '(eldoc-mode elisp-slime-nav-mode flyspell-mode
                      paredit-mode prelude-mode projectile-mode
                      rainbow-mode ruby-block-mode undo-tree-mode
                      volatile-highlights-mode whitespace-mode workgroups-mode
                      yas-minor-mode)
         (lambda (mode) (diminish mode))))

(provide 'personal-lookandfeel)

;;; personal-lookandfeel.el ends here
