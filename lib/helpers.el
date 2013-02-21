;;; helpers.el --- Miscellaneous functions and macros.

;; Copyright (C) 2012 by Andrey Yagunov

;; Author:  Andrey Yagunov <yagunov86@gmail.com>
;; License: WTFPL
;; Created: 2012-12-19 16:13:19 UTC
;; Updated: 2012-12-26 13:02:08 UTC

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

;;; Local Variables:
;;; coding: utf-8
;;; time-stamp-time-zone: UTC
;;; time-stamp-format: "%:y-%02m-%02d %02H:%02M:%02S %Z"
;;; time-stamp-start: ";; Updated: "
;;; time-stamp-end: "$"
;;; time-stamp-line-limit: 20
;;; End:

(provide 'lib/helpers)

;;; helpers.el ends here
