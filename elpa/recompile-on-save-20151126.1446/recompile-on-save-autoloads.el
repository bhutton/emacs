;;; recompile-on-save-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "recompile-on-save" "recompile-on-save.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from recompile-on-save.el

(autoload 'recompile-on-save "recompile-on-save" "\


\(fn CBUF)" t nil)

(autoload 'reset-recompile-on-save "recompile-on-save" "\


\(fn)" t nil)

(autoload 'recompile-on-save-advice "recompile-on-save" "\


\(fn FUNCTION)" nil t)

(autoload 'recompile-on-save-mode "recompile-on-save" "\
Trigger recompilation on file save.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "recompile-on-save" '("recompile-on-save-list" "ros--recompile-on-save")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; recompile-on-save-autoloads.el ends here
