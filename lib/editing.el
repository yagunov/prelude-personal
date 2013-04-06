;;; editing.el --- Edeting stuff.

;; Copyright (C) 2012, 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2012-12-26 12:57:32 UTC
;; Updated: 2013-04-06 17:57:41 UTC

;;; Commentary:

;; TODO: add long description here.

;;; Code:

(require 'thingatpt)

(defun delete-word-or-region (&optional arg)
  "Delete word if mark isn't active and region otherwise."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

;; from http://whattheemacsd.com/key-bindings.el-01.html
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun dwim-diff ()
  "Do what I mean diff."
  (interactive)
  (if (buffer-modified-p)
      (diff-buffer-with-file)
    (vc-diff)))

(defun smart-beginning-of-line ()
  (interactive)
  (if (eq last-command 'smart-beginning-of-line)
      (beginning-of-line)
    (beginning-of-line-text)))

(require 'subword)

(defun mark-whole-word ()
  "Mark whole word."
  (interactive)
  (when (not (looking-at "\\<"))
    (subword-backward))
  (set-mark (point))
  (subword-forward))

(defun smart-set-mark-command (arg)
  "Smart implementation of `set-mark-command'.
Run `set-mark-command' on just one call without prefix argument.
If prefix argument is given then deactivate secondary selection,
if mark is active and point inside of region then convert region
to secondary selection, if called twice run `mark-whole-word'."
  (interactive "P")
  (cond (arg
         (delete-overlay mouse-secondary-overlay))
        ((and (eq last-command this-command)
              mark-active
              (= (region-beginning)
                 (region-end)))
         (mark-whole-word))
        ((and mark-active
              (>= (point) (region-beginning))
              (<= (point) (region-end)))
         (primary-to-secondary (region-beginning)
                               (region-end))
         (setq deactivate-mark t))
        (t
         (set-mark-command nil))))

(defun smart-transpose ()
  "Transpose region and secondary selection in they active or characters otherwise."
  (interactive)
  (if (and mark-active (overlay-buffer mouse-secondary-overlay))
      (call-interactively 'anchored-transpose)
    (call-interactively 'transpose-chars)))

(defun smart-occur (&optional prefix)
  "Show list of matching lines in buffer.
Numeric prefix defines how many lines surrounding match will be
displayed."
  (interactive "P")
  (let ((list-matching-lines-default-context-lines (or prefix 0)))
    (if (region-active-p)
        (occur (buffer-substring-no-properties (region-beginning) (region-end)))
      (occur (thing-at-point 'symbol)))))

(defun jump-out-of-block ()
  "Jump out of code parentheses pair or code block."
  (interactive)
  (when (and (looking-back "^[[:space:]]*")
             (looking-at "[\n\t ]*\\(}\\|break;\\|return;\\)"))
    (delete-blank-lines))
  (search-forward-regexp "\\(break;\\|return;\\|[])}\"'>]\\)"))

(defun projectile-shell-command ()
  "Run shell command in project's root directory."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively 'shell-command)))

(defun projectile-async-shell-command ()
  "Run shell command in project's rood directory asynchronously."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively 'async-shell-command)))

(defun projectile-eshell ()
  "Start/switch to eshell buffer in project's root directory."
  (interactive)
  (let* ((default-directory (projectile-project-root))
         (eshell-buffer-name (format "*eshell:%s*" default-directory)))
    (eshell)))

(defun projectile-shell ()
  "Start/switch to shell buffer in project's root directory."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (shell (format "*shell:%s*" default-directory))))

(defun projectile-mx ()
  "Execute Emacs command from project's root directory.

May be useful for starting interpreters, e.g. `run-python'."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively (key-binding (kbd "M-x")))))

;; `projectile-switch-project' refuses to switch to project when I call it from
;; buffer not belonging to any project, this is a quick fix:
(defun projectile-switch-project-fix ()
  "Switch to a project we have seen before event if we are not in another project."
  (interactive)
  (if (projectile-project-p)
      (call-interactively 'projectile-switch-project)
    (flet ((projectile-prepend-project-name (prompt) prompt))
      (call-interactively 'projectile-switch-project))))

(defun projectile-compile-project-manual ()
  "Execute compilation command from project's root directory."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively 'compile)))

;;; Local Variables:
;;; coding: utf-8
;;; time-stamp-time-zone: UTC
;;; time-stamp-format: "%:y-%02m-%02d %02H:%02M:%02S %Z"
;;; time-stamp-start: ";; Updated: "
;;; time-stamp-end: "$"
;;; time-stamp-line-limit: 20
;;; End:

(provide 'lib/editing)

;;; editing.el ends here
