;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/"))
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Add ELPA packages to the loadpp path
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))


;; Ensure use-package is installed and loaded
(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

;; Set to true to have use-package install all packages mentioned if
;; they're not already installed.
(setq use-package-always-ensure nil)


(add-to-list 'load-path "~/.emacs.d/lisp/")

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(setq use-package-always-ensure t)
(setq-default truncate-lines t)

(setq scroll-step 1)

(setq mark-ring-max 6)
(setq global-mark-ring-max 6)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic))))))

(use-package smartparens)
(require 'smartparens-config)

(use-package swiper)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Draws a line between the beginning and ending of block indents
(use-package highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'bitmap)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(setq package-list '(better-defaults chyla))

(defun my-highlighter (level responsive display)
  (if (> 1 level)
      nil
    (highlight-indent-guides--highlighter-default level responsive display)))

(setq highlight-indent-guides-highlighter-function 'my-highlighter)

(cua-selection-mode 1)
(use-package yasnippet)
(require 'yasnippet)
(yas-global-mode 1)
(scroll-bar-mode 1)
(smartparens-global-mode 1)
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(setq scroll-preserve-screen-position 1)
(global-set-key (kbd "M-<up>") #'scroll-down-line)
(global-set-key (kbd "M-<down>") #'scroll-up-line)
(global-set-key (kbd "s-<up>") #'scroll-down-line)
(global-set-key (kbd "s-<down>") #'scroll-up-line)
(global-set-key (kbd "M->") (kbd "C-x ^"))
(global-set-key (kbd "M-<") (kbd "C-u -1 C-x ^"))
(global-set-key (kbd "M-s-<down>") (kbd "C-x ^"))
(global-set-key (kbd "M-s-<up>") (kbd "C-u -1 C-x ^"))

(global-set-key (kbd "M-x") #'helm-M-x)
;; (global-set-key (kbd "s-t") #'helm-projectile-find-file-dwim)
(global-set-key (kbd "M-b") #'xref-find-definitions)
(global-set-key (kbd "M-r") #'replace-string)
(global-set-key (kbd "C-M-l") #'lsp-format-buffer)
(global-set-key (kbd "C-f") #'projectile-find-file)
(global-set-key (kbd "M-F") #'helm-projectile-ag)
(global-set-key (kbd "M-s") #'company-yasnippet)

(global-set-key (kbd "C-z") #'undo)
(global-set-key (kbd "C-c C-c") #'cua-copy-region)
(global-set-key (kbd "C-v") #'cua-paste)
(global-set-key (kbd "C-s") #'save-buffer)
(global-set-key (kbd "C-M-s") #'swiper)
(global-set-key (kbd "s-f") #'swiper)
(global-set-key (kbd "s-F") #'helm-projectile-grep)


;; (add-hook 'isearch-mode-hook
;;   (lambda ()
;;   (define-key isearch-mode-map (kbd "M-f") 'isearch-repeat-forward)
;;   )
;; )

;; ;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(fset 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tkj-insert-serial-version-uuid()
  (interactive)
  (insert "private static final long serialVersionUID = 1L;"))

(defun tkj-default-code-style-hook()
  (setq c-basic-offset 2
        c-label-offset 0
        tab-width 2
        indent-tabs-mode nil
        compile-command "mvn -q -o -f ~/src/content-engine/engine/engine-core/pom.xml test -DtrimStackTrace=false"
        require-final-newline nil))
(add-hook 'java-mode-hook 'tkj-default-code-style-hook)

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

(super-save-mode +1)
(setq super-save-auto-save-when-idle t)
(setq auto-save-default nil)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines)
  )

(use-package flycheck
  :init
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.15))))

(setq xref-prompt-for-identifier nil)

(defun my-java-mode-hook ()
  (auto-fill-mode)
  (flycheck-mode)
  (git-gutter+-mode)
  (gtags-mode)
  (subword-mode)
  (yas-minor-mode)
  (set-fringe-style '(8 . 0))
  (define-key c-mode-base-map (kbd "C-M-j") 'tkj-insert-serial-version-uuid)
  (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
  (define-key c-mode-base-map (kbd "S-<f7>") 'gtags-find-tag-from-here)

  ;; Fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0))

  ;; Indent arguments on the next line as indented body.
  (c-set-offset 'arglist-intro '++))
(add-hook 'java-mode-hook 'my-java-mode-hook)

(use-package projectile :ensure t)
(use-package yasnippet :ensure t)

(setq lsp-headerline-breadcrumb-enable-diagnostics nil)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-ui-doc-show-with-cursor nil)
(setq lsp-modeline-code-actions-enable t)

(use-package lsp-mode
  :defer t
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1024))
  (lsp-eldoc-hook nil)
  :bind (:map lsp-mode-map ("C-t" . test-suite))
  :bind (:map lsp-mode-map ("C-M-l" . format-and-save))
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook ((python-mode go-mode lsp-java ccls
          js-mode js2-mode typescript-mode web-mode javascript-mode rjsx-mode c-mode c++-mode) . lsp))

(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp))

(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "/usr/local/bin/ccls")
  (setq lsp-prefer-flymake nil)
  (setq ccls-initialization-options
        '(:clang (:extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
                              "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                              "-isystem/usr/local/include"
                              "-isystem/usr/local/include/gtest"
                              "-isystem/usr/local/lib"
                              "-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/12.0.0/include"
                              "-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
                              "-isystem/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include"
                              "-isystem/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/System/Library/Frameworks"]
                   :resourceDir "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/12.0.0")))
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (setq flycheck-gcc-include-path '("/usr/local/include"))
  (setq flycheck-clang-include-path '("/usr/local/include"))
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :mode "\\.js\\'"
  :mode "\\.tsx\\'"
  :mode "\\.jsx\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (require 'dap-node)
  (dap-node-setup))

(use-package go-mode
  :mode "\\.go\\'")

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  )
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; (use-package rjsx-mode
;;   ;; :mode "\\.js\\'"
;;   ;; :mode "\\.jsx\\'"
;;   :hook (rjsx-mode . lsp-deferred)
;;   :config
;;   (setq typescript-indent-level 2)
;;   (require 'dap-node)
;;   (dap-node-setup))

;; (use-package yaml-mode)
;; (require 'yaml-mode)
;;     (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(use-package hydra :ensure t)

(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ("C-c u" . lsp-ui-imenu)
        ("M-i" . lsp-ui-doc-focus-frame))
  (:map lsp-mode-map
        ("M-n" . forward-paragraph)
        ("M-p" . backward-paragraph))
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-doc-border (face-foreground 'default))  ;; Border color of the frame
  (lsp-ui-doc-delay 5)
  :config
  (setq lsp-ui-flycheck-enable t)
  ;; Use lsp-ui-doc-webkit only in GUI
  (if (display-graphic-p)
      (setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(setq lsp-prefer-capf t)

(setenv "JAVA_HOME"  "/Library/Java/JavaVirtualMachines/jdk-11.0.10.jdk/Contents/Home/")
(setq lsp-java-java-path "/Library/Java/JavaVirtualMachines/jdk-11.0.10.jdk/Contents/Home/bin/java")

(use-package lsp-java
  :ensure t
  :config
  (setq lsp-java-vmargs
      (list
  ;;        "-noverify"
  ;;        "-Xmx1G"
  ;;        "-XX:+UseG1GC"
  ;;        "-XX:+UseStringDeduplication"
         "-javaagent:/Users/ben/.m2/repository/org/projectlombok/lombok/1.18.10/lombok-1.18.10.jar"))

  (add-hook 'java-mode-hook #'lsp)
)

(require 'dap-node)
;; (require 'dap-firefox)
(require 'dap-chrome)

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (tooltip-mode 1)
  (dap-register-debug-template
   "localhost:5005"
   (list :type "java"
         :request "attach"
         :hostName "localhost"
         :port 5005))
  (dap-register-debug-template
   "lxd"
   (list :type "java"
         :request "attach"
         :hostName "10.152.112.168"
         :port 5005))
  )

(use-package dap-java
  :ensure nil
  :after (lsp-java)

  :config
  (global-set-key (kbd "<f7>") 'dap-step-in)
  (global-set-key (kbd "<f8>") 'dap-next)
  (global-set-key (kbd "<f9>") 'dap-continue)
  (global-set-key (kbd "M-b") 'lsp-find-implementation)
  (global-set-key (kbd "C-b") 'lsp-find-implementation)
  (setq dap-lldb-debug-program '("/usr/local/Cellar/llvm/12.0.0/bin/lldb-vscode")))

(require 'dap-lldb)
(require 'dap-cpptools)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End Java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key)
(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'cpp-mode-hook 'lsp)

(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.500 ;; clangd is fast
      ;; be more ide-ish
      ;; lsp-headerline-breadcrumb-enable t
      lsp-completion-provider :capf
      company-lsp-cache-candidates 'auto
      lsp-enable-file-watchers nil
      )


(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package back-button)
(require 'back-button)
(back-button-mode 1)

(require 'flycheck)

(use-package web-mode)
(require 'web-mode)
(use-package add-node-modules-path)
(require 'add-node-modules-path)


(defun web-mode-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))

(add-hook 'web-mode-hook  'web-mode-init-prettier-hook)

(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(setq js-indent-level 2)
(setq javascript-indent-level 2)
(setq web-mode-markup-indent-offset 2)

(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
(add-hook 'lsp-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name)))))
(add-hook 'lsp-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name)))))
(add-hook 'lsp-mode-hook
          (lambda ()
            (when (string-equal "ts" (file-name-extension buffer-file-name)))))
(add-hook 'lsp-mode-hook
          (lambda ()
            (when (string-equal "js" (file-name-extension buffer-file-name)))))


(defun format-and-save ()
  (interactive)
  (lsp-format-buffer)
  (save-buffer))

(defun test-suite ()
  (interactive)
  (lsp-format-buffer)
  (projectile-save-project-buffers)
  (with-output-to-temp-buffer "*test-runner*"
    (when(string= (file-name-extension buffer-file-name) "js")
      (npm-test))
    (when(string= (file-name-extension buffer-file-name) "jsx")
      (npm-test))
    (when(string= (file-name-extension buffer-file-name) "ts")
      (npm-test))
    (when(string= (file-name-extension buffer-file-name) "tsx")
      (npm-test))
    (when(string= (file-name-extension buffer-file-name) "go")
      (go-test))
    )
  (when(string= (file-name-extension buffer-file-name) "java")
    (mvn-test))
  )

(defun npm-test()
  (shell-command (concat "npm test &")
                 "*test-runner*"
                 "*Messages*")
  )

(defun mvn-test()
  (shell-command (concat "cd " (projectile-project-root) " && mvn test &")
                 "*test-runner*"
                 "*Messages*")
  )


line-spacing(defun go-test()
  (shell-command (concat "cd " (projectile-project-root) " && go test ./... &")
                 "*test-runner*"
                 "*Messages*")
  )

(defun test-suite-jest ()
  (interactive)
  (with-output-to-temp-buffer "*jest-runner*"
    (shell-command (concat "echo " projectile-project-root " && cd .. && jest " projectile-project-root " &")
                   "*jest-runner*"
                   "*Messages*")
    ))

(use-package company)
(require 'company)
(use-package company-box)
(require 'company-box)
;; (require 'company-tabnine)
(add-hook 'after-init-hook 'global-company-mode)
;; (add-hook 'company-mode-hook 'company-box-mode)
;; (add-to-list 'company-backends #'company-tabnine)

;; workaround for company-transformers
(setq company-tabnine--disable-next-transform nil)
(defun my-company--transform-candidates (func &rest args)
  (if (not company-tabnine--disable-next-transform)
      (apply func args)
    (setq company-tabnine--disable-next-transform nil)
    (car args)))

(defun my-company-tabnine (func &rest args)
  (when (eq (car args) 'candidates)
    (setq company-tabnine--disable-next-transform t))
  (apply func args))

(advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
(advice-add #'company-tabnine :around #'my-company-tabnine)

(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0)
(setq company-lsp-cache-candidates t)
(setq company-show-numbers t)

(use-package prettier-js)
(require 'prettier-js)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

;loads ruby mode when a .rb file is opened.
(setq abg-required-packages
      (list 'xml-rpc 'magit 'gh 'inf-ruby))

(dolist (package abg-required-packages)
  (when (not (package-installed-p package))
     (package-refresh-contents)
    (package-install package)))

(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(setq auto-mode-alist  (cons '(".rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".rhtml$" . html-mode) auto-mode-alist))

(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(use-package multiple-cursors)
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)

(delete-selection-mode 1)

(setq inhibit-splash-screen t
      initial-scratch-message nil)

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

(defun insert-line-below()
  (interactive)
  (move-end-of-line 1)
  (open-line 1)
  (next-line 1)
  (indent-according-to-mode)
  )

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell)
  (setq-default exec-path-from-shell-shell-name "/bin/zsh")
  (exec-path-from-shell-copy-env "PATH")
(exec-path-from-shell-initialize))

(global-set-key (kbd "M-d") 'duplicate-line-or-region)
(global-set-key (kbd "S-<return>") 'insert-line-below)
(global-set-key (kbd "M-C-<right>") 'windmove-right)
(global-set-key (kbd "M-C-<left>") 'windmove-left)
(global-set-key (kbd "M-C-<up>") 'windmove-up)
(global-set-key (kbd "M-C-<down>") 'windmove-down)
(global-set-key (kbd "M-?") 'windmove-delete-down)
(global-set-key (kbd "s-<s-right>") 'move-end-of-line)
(global-set-key (kbd "s-<s-left>") 'smart-line-beginning)
(global-set-key (kbd "C-1") 'treemacs-select-window)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)

(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
(define-key prog-mode-map (kbd "C-t") 'rspec-verify-all)
(define-key prog-mode-map (kbd "s-<s-right>") 'move-end-of-line)
(define-key prog-mode-map (kbd "s-<s-left>") 'smart-line-beginning)
(define-key prog-mode-map (kbd "M-[") 'back-button-global-backward)
(define-key prog-mode-map (kbd "M-]") 'back-button-global-forward)

(require 'treemacs)
(require 'dash)

(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(use-package all-the-icons)
(require 'all-the-icons)

(when (window-system)
  (tool-bar-mode -1)
  (tooltip-mode -1))

(use-package spaceline)
(require 'spaceline-config)
(spaceline-emacs-theme)

(use-package solaire-mode)

;(use-package doom)
(use-package doom-themes
  :config

  ;; Global settings (defaults)
  (solaire-global-mode +1)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; (load-theme 'doom-one-light t)
  (load-theme 'doom-one-light t)

  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(when (memq window-system '(mac ns x))
(exec-path-from-shell-initialize))
;;(projectile-rails-global-mode)

(treemacs)

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-style "box")
  (centaur-tabs-height 36)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "●")
  (centaur-tabs-buffer-groups-function #'centaur-tabs-projectile-buffer-groups)
  (x-underline-at-descent-line t)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-set-bar 'under)

  :bind
  (
   ("s-{" . #'centaur-tabs-backward)
   ("s-}" . #'centaur-tabs-forward)
   ("C-{" . #'centaur-tabs-backward)
   ("C-}" . #'centaur-tabs-forward)
   )
  )


  (setq treemacs-icon-tag-node-open-png   (propertize "− " 'face 'font-lock-keyword-face)
        treemacs-icon-tag-node-closed-png (propertize "+ " 'face 'font-lock-keyword-face)
        treemacs-icon-tag-leaf-png        (propertize "🞄 " 'face 'font-lock-keyword-face))

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "S-s-f") 'helm-projectile-find-file)
(global-set-key (kbd "S-<delete>") 'kill-whole-line)

(use-package multiple-cursors
  :bind (("C-c m m" . #'mc/edit-lines )
         ("C-c m d" . #'mc/mark-all-dwim )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo tree mode                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;turn on everywhere
;; make ctrl-z undo
(global-set-key (kbd "s-z") 'undo)
;; make ctrl-Z redo
(global-set-key (kbd "s-Z") 'undo-redo)

(set-face-attribute 'default nil :height 130)

(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))


(global-set-key (kbd "S-M-<up>") 'move-text-up)
(global-set-key (kbd "S-M-<down>") 'move-text-down)
(global-set-key (kbd "M-w") 'kill-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#282c34" "#99324b" "#4f894c" "#9a7500" "#3b6ea8" "#97365b" "#398eac" "#2a2a2a"])
 '(custom-safe-themes
   '("f4876796ef5ee9c82b125a096a590c9891cec31320569fc6ff602ff99ed73dca" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "f8c30fa07ba7e8fe884f22b428dae6724955fa61ad84a658c3b0164ae391fb52" "8c847a5675ece40017de93045a28ebd9ede7b843469c5dec78988717f943952a" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "84da7b37214b4ac095a55518502dfa82633bee74f64daf6e1785322e77516f96" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "030346c2470ddfdaca479610c56a9c2aa3e93d5de3a9696f335fd46417d8d3e4" "a63355b90843b228925ce8b96f88c587087c3ee4f428838716505fd01cf741c8" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" default))
 '(ensime-sem-high-faces
   '((var :foreground "#000000" :underline
          (:style wave :color "yellow"))
     (val :foreground "#000000")
     (varField :foreground "#600e7a" :slant italic)
     (valField :foreground "#600e7a" :slant italic)
     (functionCall :foreground "#000000" :slant italic)
     (implicitConversion :underline
                         (:color "#c0c0c0"))
     (implicitParams :underline
                     (:color "#c0c0c0"))
     (operator :foreground "#000080")
     (param :foreground "#000000")
     (class :foreground "#20999d")
     (trait :foreground "#20999d" :slant italic)
     (object :foreground "#5974ab" :slant italic)
     (package :foreground "#000000")
     (deprecated :strike-through "#000000")))
 '(fci-rule-color "#9e9e9e")
 '(j dee-db-requested-breakpoint-face-colors)
 '(jdee-db-active-breakpoint-face-colors (cons "#fafafa" "#3b6ea8"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#fafafa" "#bdbdbd"))
 '(line-spacing 0.2)
 '(line-spacing-vertical-center 1)
 '(objed-cursor-color "#99324b")
 '(package-selected-packages
   '(wgrep-helm ivy-searcher swiper git-gutter+ highlight-indent-guides helm idle-highlight-in-visible-buffers-mode smooth-scroll lsp-ui lsp-treemacs lsp-java lsp-mode jest-test-mode yasnippet-snippets clojure-mode-extra-font-locking cider spaceline treemacs-evil jest npm-mode find-file-in-project helm-rg ac-js2 company-flow company-tern tern-auto-complete tern treemacs-magit xref-js2 js2-refactor prettier-js company web-mode yard-mode rubocop kaolin-themes sublimity minimap magit twilight-bright-theme treemacs-projectile treemacs-icons-dired sublime-themes spacemacs-theme solarized-theme seeing-is-believing one-themes mocha material-theme leuven-theme intellij-theme helm-projectile helm-ag flatui-theme exec-path-from-shell espresso-theme emr doom-themes color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized chyla-theme centaur-tabs bundler better-defaults auto-complete-exuberant-ctags apropospriate-theme all-the-icons-dired ag))
 '(vc-annotate-background "#fafafa")
 '(vc-annotate-color-map
   (list
    (cons 20 "#4f894c")
    (cons 40 "#688232")
    (cons 60 "#817b19")
    (cons 80 "#9a7500")
    (cons 100 "#a0640c")
    (cons 120 "#a65419")
    (cons 140 "#ac4426")
    (cons 160 "#a53f37")
    (cons 180 "#9e3a49")
    (cons 200 "#97365b")
    (cons 220 "#973455")
    (cons 240 "#983350")
    (cons 260 "#99324b")
    (cons 280 "#a25467")
    (cons 300 "#ab7784")
    (cons 320 "#b49aa0")
    (cons 340 "#9e9e9e")
    (cons 360 "#9e9e9e")))
 '(vc-annotate-very-old-color nil))


(when(eq system-type 'darwin)
    (ignore-errors(set-frame-font "DejaVu Sans Mono-15"))
  (add-to-list 'default-frame-alist
               (cons 'font "DejaVu Sans Mono-15"))
  (add-to-list 'default-frame-alist
               (cons 'font "DejaVu Sans Mono-15"))
)

(when(eq system-type 'windows-nt)
    (ignore-errors(set-frame-font "Consolas"))
  (add-to-list 'default-frame-alist
               (cons 'font "Consolas"))
  (add-to-list 'default-frame-alist
               (cons 'font "Consolas"))
)

(defun xah-toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height.
URL `http://ergoemacs.org/emacs/emacs_toggle_line_spacing.html'
Version 2017-06-02"
  (interactive)
  (if line-spacing
      (setq line-spacing nil)
    (setq line-spacing 0.5))
  (redraw-frame (selected-frame)))

(setq-default cursor-type 'bar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun spawn-shell (name)
  "Create a new shell buffer
taken from http://stackoverflow.com/a/4116113/446256"

  (interactive "MName of shell buffer to create: ")
  (pop-to-buffer (get-buffer-create (generate-new-buffer-name name)))
  (shell (current-buffer)))

(defun my-shell-mode-hook ()
  (process-send-string (get-buffer-process (current-buffer))
                       "export PAGER=cat\n")
  (process-send-string (get-buffer-process (current-buffer))
                       "uprompt\n\n\n"))(
  add-hook 'shell-mode-hook 'my-shell-mode-hook)

(setq-default explicit-shell-file-name "/bin/bash")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASH settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sh-basic-offset 2
      sh-indentation 2)

;; snippets, please
(add-hook 'sh-mode-hook 'yas-minor-mode)

(use-package git-gutter+)

;; show git changes in the gutter
(add-hook 'sh-mode-hook 'git-gutter+-mode)

;; Allow functions on the form <word>.<rest>(). Without my change,
;; allowing punctuation characters in the function name,, only
;; <rest>() is allowed.
(setq sh-imenu-generic-expression
      (quote
       ((sh
         (nil "^\\s-*function\\s-+\\([[:alpha:]_][[:alnum:]\\s._]*\\)\\s-*\\(?:()\\)?" 1)
         (nil "^\\s-*\\([[:alpha:]_][[:alnum:]\\s._]*\\)\\s-*()" 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpret shell escapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance settings regardless of window system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq frame-background-mode nil
      column-number-mode t
      frame-title-format (concat invocation-name "@" (system-name) " {%f}")
      inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'markdown-mode
      ;; no visible or audible bells, please
      visible-bell nil
      ring-bell-function (lambda nil (message "")))

;; Disable backups/autosaves
(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(delete-selection-mode t)
(column-number-mode)

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically expand these words and characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-default 'abbrev-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open files into this Emacs instance from anywhere using
;; 'emacsclient <file>'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Is this causing '<key> undefined' errors? (server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package compile
  :init
  (setq compilation-ask-about-save nil
        compilation-scroll-output 'next-error
        ;; Don't stop on info or warnings.
        compilation-skip-threshold 2)
  )

;; Taken from https://emacs.stackexchange.com/questions/31493/print-elapsed-time-in-compilation-buffer/56130#56130
(make-variable-buffer-local 'my-compilation-start-time)

(add-hook 'compilation-start-hook #'my-compilation-start-hook)
(defun my-compilation-start-hook (proc)
  (setq my-compilation-start-time (current-time)))

(add-hook 'compilation-finish-functions #'my-compilation-finish-function)
(defun my-compilation-finish-function (buf why)
  (let* ((elapsed  (time-subtract nil my-compilation-start-time))
         (msg (format "Compilation took: %s" (format-time-string "%T.%N" elapsed t))))
    (save-excursion (goto-char (point-max)) (insert msg))
    (message "Compilation %s: %s" (string-trim-right why) msg)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make C-x C-b maximise the buffer list window, this saves two
;; additional shortcuts from the normal behaviour.
(use-package helm
  :init
  (defun tkj-list-buffers()
    (interactive)
    (let ((helm-full-frame t))
      (helm-mini)))

  :bind
  ("C-x C-b" . 'tkj-list-buffers))
(helm-mode 1)

(use-package helm-projectile)
(use-package helm-ag)

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; buffer names and mini buffer
(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator ":"
        uniquify-strip-common-suffix nil
        read-file-name-completion-ignore-case t))

;; Auto scroll the compilation window
(setq compilation-scroll-output t)

;; Scroll up and down while keeping the cursor where it is.
(defun help/scroll-up-one-line ()
  (interactive)
  (scroll-down 1))
(defun help/scroll-down-one-line ()
  (interactive)
  (scroll-up 1))
(global-set-key (kbd "M-p") 'help/scroll-down-one-line)
(global-set-key (kbd "M-n") 'help/scroll-up-one-line)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prefer UTF 8, but don't override current encoding if specified
;; (unless you specify a write hook).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prefer-coding-system 'utf-8-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing VC log messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'log-edit-hook (lambda () (flyspell-mode 1)))

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
