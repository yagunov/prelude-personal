;;; files.el --- Work with files and directories.

;; Copyright (C) 2012, 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2012-12-26 12:58:05 UTC
;; Updated: 2013-08-08 16:45:50 UTC

;;; Commentary:

;; TODO: add long description here.

;;; Code:

(eval-when-compile
  (require 'ido)
  (require 'recentf))

(defun ido-find-file-ignore-at-point ()
  "Ignore thing at point when "
  (interactive)
  (let ((ido-use-url-at-point nil)
        (ido-use-filename-at-point nil))
    (ido-find-file)))

;; Mix of http://emacs-fu.blogspot.ru/2013/03/editing-with-root-privileges-once-more.html
;; and `prelude-sudo-edit'.
(defun find-file-as-root (&optional arg)
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable
by user. When called with argument tries to reopen current file
with root-privileges unless it's already writable."
  (interactive "P")
  (if (and arg buffer-file-name)
      (unless (file-writable-p buffer-file-name)
        (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))
    (let ((file (ido-read-file-name "Edit as root: ")))
      (unless (file-writable-p file)
        (setq file (concat "/sudo:root@localhost:" file)))
      (find-file file))))

(provide 'lib/files)

;;; files.el ends here
