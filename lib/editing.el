;;; editing.el --- Edeting stuff.

;; Copyright (C) 2012, 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2012-12-26 12:57:32 UTC
;; Updated: 2013-02-20 15:46:12 IRKT

;;; Commentary:

;; TODO: add long description here.

;;; Code:

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
