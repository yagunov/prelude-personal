;;; helpers.el --- Miscellaneous functions and macros.

;; Copyright (C) 2012, 2013 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2012-12-19 16:13:19 UTC
;; Updated: 2013-08-08 16:46:00 UTC

;;; Code:

(defun path (&rest chunks)
  "Build a pathname from chunks."
  (apply 'concat (append (mapcar 'file-name-as-directory (butlast chunks))
                         (last chunks))))

(defmacro path-user (&rest chunks)
  "Full path to the file in user directory (default: ~/.emacs.d/)."
  `(path user-emacs-directory ,@chunks))

(defmacro path-config (&rest chunks)
  "Full path to the file in configs directory."
  `(path config-directory ,@chunks))

(provide 'lib/helpers)

;;; helpers.el ends here
