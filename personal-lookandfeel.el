;;; personal-lookandfeel.el --- UI settings (colors and so on).

;; Copyright (C) 2012, 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2012-06-04 04:31:13 UTC
;; Updated: 2013-02-19 20:09:02 IRKT

;;; Code:

(require 'use-package)

(scroll-bar-mode 0)
;; set 5-pixel margin in x
(fringe-mode 5)

(when window-system
  ;; what to display in window title
  (setq frame-title-format '("Emacs: "
                             (:eval (abbreviate-file-name buffer-file-name)
                                    "%f" ("%b"))))
  ;; make emacs use the clipboard
  (setq x-select-enable-primary t
        x-select-enable-clipboard t)
  ;; FIXME: Choose font based on DPI or resolution.
  (set-frame-font "Ubuntu Mono 10")
  (set-face-font 'mode-line "Consolas 8")
  (set-face-font 'mode-line-inactive "Consolas 8"))

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
  (setq .current-theme 'dark))

(defun light ()
  "Enable light color theme."
  (interactive)
  (unless .solarized-light-loaded
    (load (path (el-get-package-directory 'solarized-theme)
                "solarized-light-theme.el"))
    (setq .solarized-light-loaded t))
  (enable-theme 'solarized-light)
  (setq .current-theme 'light))

(defun toggle-theme ()
  "Toggle between dark and light color themes."
  (interactive)
  (if (eq .current-theme 'dark)
      (light)
    (dark)))

;; Load dark theme on start-up
(dark)

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
                      volatile-highlights-mode workgroups-mode yas-minor-mode)
         (lambda (mode) (diminish mode))))

;;; Local Variables:
;;; coding: utf-8
;;; time-stamp-time-zone: UTC
;;; time-stamp-format: "%:y-%02m-%02d %02H:%02M:%02S %Z"
;;; time-stamp-start: ";; Updated: "
;;; time-stamp-end: "$"
;;; time-stamp-line-limit: 20
;;; End:

(provide 'personal-lookandfeel)

;;; personal-lookandfeel.el ends here
