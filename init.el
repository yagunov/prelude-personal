(defun add-prefix (prefix symbol)
  "Make new symbol from old by adding a prefix."
  (intern (concat prefix (symbol-name symbol))))

(setq prelude-guru nil
      prelude-flycheck nil)

;; load Prelude modules:
(-each '(c clojure common-lisp css emacs-lisp erlang haskell
           js lisp markdown org perl python ruby scala scheme xml)
       (lambda (module)
         (require (add-prefix "prelude-" module))))

;; add personal configuration to load path
(add-to-list 'load-path prelude-personal-dir)
(add-to-list 'load-path (expand-file-name "lib" prelude-personal-dir))

;; my packages:
(setq el-get-sources
      (append
       '((:name accelerate-24
                :description "Pump numeric arg for auto-repeated interactive commands"
                :type github :pkgname "yagunov/accelerate.el"))
       el-get-sources))

(prelude-ensure-module-deps
 '(use-package workgroups accelerate-24 scratch ess smex ido-ubiquitous
               ido-yes-or-no switch-window solarized-theme diminish
               highlight-symbol second-sel browse-kill-ring
               git-gutter-fringe auto-complete wgrep))

;; load my configuration modules
(-each '(general lookandfeel editor windows programming shell misc cc-mode)
       (lambda (conf)
         (require (add-prefix "personal-" conf))))

;; Start emacs server
(server-start)
