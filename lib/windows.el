;;; windows.el --- Handling emacs windows.

;; Copyright (C) 2012, 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2012-12-26 12:59:31 UTC
;; Updated: 2013-01-02 16:33:48 UTC

;;; Commentary:

;; TODO: add long description here.

;;; Code:

(require 'ido)

;; Slightly modified code form EmacsWiki (http://www.emacswiki.org/emacs/WindowNavigation)
(defvar ido-jump-to-window-last nil
  "Window last visited with `ido-jump-to-window'.")
(defun ido-jump-to-window ()
  "Jump to the window with selected buffer."
  (interactive)
  (let* ((visible (mapcar #'(lambda (w) (buffer-name (window-buffer w)))
                          (window-list)))
         (last (and (window-live-p ido-jump-to-window-last)
                    (buffer-name (window-buffer ido-jump-to-window-last))))
         (buffer-name (ido-completing-read
                       "Window: "
                       (ido-chop visible (or (and last
                                                  (member last visible)
                                                  (not (eq last (car visible)))
                                                  last)
                                             (cadr visible))))))
    (if (not (member buffer-name visible))
        (error "'%s' does not have a visible window" buffer-name)
      (setq ido-jump-to-window-last (selected-window))
      (select-window (get-buffer-window buffer-name)))))

(defun other-window-fit (count &optional all-frames)
  ""
  (interactive "p")
  (other-window count all-frames)
  (fit-window-to-buffer-recenter))

(defun other-window-back (count &optional all-frames)
  "Same as `other-window' with negative prfix argument."
  (interactive "p")
  (other-window (- count) all-frames))

(defun minimize-window-horizontally ()
  ""
  (interactive)
  (window-resize (selected-window) (+ (- (window-height)) 2) nil t))

(defun maximize-window-horizontally ()
  ""
  (interactive)
  (let ((old-size (window-height)))
    (fit-window-to-buffer)
    (when (/= (window-height) old-size)
      (recenter))))

;; from https://github.com/purcell/emacs.d
(defun split-window-func-with-other-buffer (split-function)
  "When splitting window, show (other-buffer) in the new window."
  (lexical-let ((s-f split-function))
    (lambda ()
      (interactive)
      (funcall s-f)
      (set-window-buffer (next-window) (other-buffer)))))

(defun split-window-horizontally-and-balance (&optional size)
  "Split window horizontal and balance all windows"
  (interactive "P")
  (split-window-horizontally size)
  (unless size
    (balance-windows)))

(defun split-window-vertically-and-balance (&optional size)
  "$$$$$ TODO: no docstring $$$$$"
  (interactive "P")
  (split-window-vertically size)
  (unless size
    (balance-windows)))

(defun delete-other-windows-vertically-or-all ()
  "Delete all other windows in current column or if there is non delete all other windows."
  (interactive)
  (or (delete-other-windows-vertically)
      (delete-other-windows)))

(defun swap-windows (window1 window2)
  "Swap the contents (buffer associated with window) of WINDOW1 and WINDOW2."
  (let ((buffer1 (window-buffer window1))
        (buffer2 (window-buffer window2)))
    (select-window window1)
    (switch-to-buffer buffer2)
    (select-window window2)
    (switch-to-buffer buffer1)
    (if (null current-prefix-arg)
        (select-window window1))
    (list buffer1 buffer2 current-prefix-arg)))

(defun swap-window-next ()
  "Swap content of current window with the next one (`next-window')."
  (interactive)
  (unless (one-window-p)
    (swap-windows (selected-window) (next-window))))

(defun swap-window-previous ()
  "Swap content of current window with the previous one (`previous-window')."
  (interactive)
  (unless (one-window-p)
    (swap-windows (selected-window) (previous-window))))

;;; Local Variables:
;;; coding: utf-8
;;; time-stamp-time-zone: UTC
;;; time-stamp-format: "%:y-%02m-%02d %02H:%02M:%02S %Z"
;;; time-stamp-start: ";; Updated: "
;;; time-stamp-end: "$"
;;; time-stamp-line-limit: 20
;;; End:

(provide 'lib/windows)

;;; windows.el ends here
