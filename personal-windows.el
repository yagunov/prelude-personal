;;; personal-windows.el --- Emacs window management.

;; Copyright (C) 2012, 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2012-12-30 15:21:39 UTC
;; Updated: 2013-03-05 21:26:58 IRKT

;;; Code:

(require 'use-package)
(require 'lib/windows)

;; minimal window height
(setq window-min-height 2
      window-safe-min-height 1)

;; more convenient keys to move back and forth through the windows
(bind-key* "C-o" 'other-window)
(bind-key* "M-o" 'other-window-back)
(bind-key* "C-M-o" 'other-window-back)
;; adding and removing windows
(bind-key* "C-;" (split-window-func-with-other-buffer
                  'split-window-horizontally-and-balance))
(bind-key* "C-'" (split-window-func-with-other-buffer
                  'split-window-vertically-and-balance))
(bind-key* "C-x 1" 'delete-other-windows-vertically-or-all)
;; swap `balance-windows' and `what-cursor-position' bindings
(bind-key* "C-x =" 'balance-windows)
(bind-key* "C-x +" 'what-cursor-position)

(bind-key* "C-x o" 'swap-window-next)
(bind-key* "C-x C-o" 'swap-window-previous)

;; Managing multiple groups of windows.
(use-package workgroups
  :init (progn
          (setq wg-morph-on nil
                wg-mode-line-on nil)
          ;; HACK: Work around for the incapability problem with `smart-mode-line'.
          (flet ((wg-mode-line-add-display nil))
            (workgroups-mode))
          (when window-system
            ;; what to display in window title if workgroups are used
            (setq frame-title-format
                  '("["
                    (:eval (number-to-string
                            (position (wg-current-workgroup t) (wg-list t))))
                    ":"
                    (:eval (wg-name (wg-current-workgroup t)))
                    "] "
                    (:eval (abbreviate-file-name buffer-file-name)
                           "%f" ("%b"))))))
  :config (progn
            (wg-load (expand-file-name "workgroups" prelude-savefile-dir))
            (defun personal-wg-save-hook ()
              (wg-update-all-workgroups)
              (wg-save wg-file))
            (add-hook 'kill-emacs-hook 'personal-wg-save-hook))
  :diminish workgroups-mode
  :bind (("C-z z"   . wg-switch-to-previous-workgroup)
         ("C-z C-z" . wg-switch-to-previous-workgroup)
         ("C-z w"   . ido-jump-to-window)
         ("C-z f"   . maximize-window-horizontally)
         ("C-z F"   . minimize-window-horizontally)
         ("C-z ="   . balance-windows)))

;; Undoing changes in window configurations.
(use-package winner
  :init (progn
          (setq winner-dont-bind-my-keys t)
          (winner-mode))
  :bind (("C-z /"    . winner-undo)
         ("C-z C-/"  . winner-undo)
         ("C-z \\"   . winner-redo)
         ("C-z C-\\" . winner-redo)))

(use-package switch-window
  :bind ("C-z C-w" . switch-window))

(use-package scratch
  :bind ("C-z s" . scratch)
  :config (add-to-list 'scratch-mode-alist '(ess-mode . R-mode)))

;; FIXME: Consumes too much CPU.
;; (use-package popwin
;;   :init (progn
;;           (require 'popwin)
;;           (setq display-buffer-function 'popwin:display-buffer)
;;           (global-set-key (kbd "C-S-z") popwin:keymap)
;;           (push "*Local Variables*" popwin:special-display-config))
;;   :bind ("C-`" . popwin:close-popup-window))

;;; Local Variables:
;;; coding: utf-8
;;; time-stamp-time-zone: UTC
;;; time-stamp-format: "%:y-%02m-%02d %02H:%02M:%02S %Z"
;;; time-stamp-start: ";; Updated: "
;;; time-stamp-end: "$"
;;; time-stamp-line-limit: 20
;;; End:

(provide 'personal-windows)

;;; personal-windows.el ends here
