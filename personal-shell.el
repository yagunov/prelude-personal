;;; personal-shell.el --- Running external commands and shells.

;; Copyright (C) 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2013-01-03 03:01:49 UTC
;; Updated: 2013-08-08 16:47:23 UTC

;;; Code:

(require 'bind-key)
(require 'grep)

(bind-key "C-x M-g" 'grep-find)
;; default M-& is too hard to press
(bind-key "C-x M-!" 'async-shell-command)

;;; ----------------------------------------------------------------------------
;;; Show directory shell-command in prompts:

(defun prompt-add-current-directory (text)
  "Add current directory to the prompt string."
  (let ((path (abbreviate-file-name (directory-file-name default-directory))))
    (concat text
            (unless (string= text "") " ")
            "[" (if (require 'smart-mode-line nil t)
                    (sml/replacer path)
                  path) "]$ ")))

(defadvice read-shell-command
  (before read-shell-command-replace-prompt activate)
  "Replace default uncustomizable prompt in `shell-command' and alike."
  (setq prompt
        (cond ((string= prompt "Shell command: ")
               (prompt-add-current-directory ""))
              ((string= prompt "Async shell command: ")
               (prompt-add-current-directory "Async run"))
              ((string= prompt "Run grep (like this): ")
               (prompt-add-current-directory ""))
              ((string= prompt "Run find (like this): ")
               (prompt-add-current-directory ""))
              (t prompt))))

(defadvice shell-command-on-region
  (before shell-command-on-region-or-buffer activate)
  "Execute shell command on the entire buffer if mark is not set."
  (interactive (let (string beg end)
                 (if mark-active
                     (setq string (read-shell-command
                                   (prompt-add-current-directory "Run on region"))
                           beg (region-beginning)
                           end (region-end))
                   (setq string (read-shell-command
                                 (prompt-add-current-directory "Run on buffer"))
                         beg (point-min)
                         end (point-max)))
                 (list beg end
                       string
                       current-prefix-arg
                       current-prefix-arg
                       shell-command-default-error-buffer
                       t))))

(provide 'personal-shell)

;;; personal-shell.el ends here
