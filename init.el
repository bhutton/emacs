(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/use-package/")
  (require 'use-package))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(add-to-list 'default-frame-alist '(fullscreen . maximized))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(setq use-package-always-ensure t)

; list the packages you want
(setq package-list '
	(better-defaults helm helm-switch-shell helm-projectile helm-ag ruby-electric rvm seeing-is-believing
	chruby inf-ruby ruby-test-mode yasnippet flycheck web-mode js2-refactor xref-js2 prettier-js
	dumb-jump exec-path-from-shell all-the-icons spaceline doom-themes spacemacs-theme projectile-rails
	centaur-tabs undo-tree tide))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq scroll-step 1)

(setq mark-ring-max 6)
(setq global-mark-ring-max 6)

(require
 'better-defaults)
(custom-set-faces
 '(default ((t (:background nil)))))

(require 'smartparens-config)

;; Draws a line between the beginning and ending of block indents
(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;; (setq highlight-indent-guides-bitmap -function 'highlight-indent-guides--bitmap-line)
(setq highlight-indent-guides-method 'bitmap)
;; (setq highlight-indent-guides-auto-enabled 'nil)
;; (set-face-background 'highlight-indent-guides-odd-face "darkgray")
;; (set-face-background 'highlight-indent-guides-even-face "dimgray")
;; (set-face-foreground 'highlight-indent-guides-character-face "dimgray")

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
(yas-global-mode 1)
(scroll-bar-mode 1)
(smartparens-global-mode 1)
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; (use-package tree-sitter
;;   :init (global-tree-sitter-mode)
;;   :hook ((ruby-mode . tree-sitter-hl-mode)
;;          (js-mode . tree-sitter-hl-mode)
;;          (typescript-mode . tree-sitter-hl-mode)
;;          (go-mode . tree-sitter-hl-mode)))
;; (use-package tree-sitter-langs)

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

(add-hook 'isearch-mode-hook
  (lambda ()
  (define-key isearch-mode-map (kbd "M-f") 'isearch-repeat-forward)
  )
)

;; ;typescript
;; (setq create-lockfiles nil)
;; (defun setup-tide-mode ()
;;    (interactive)
;;    (tide-setup)
;;    (flycheck-mode +1)
;;    (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;    (eldoc-mode +1)
;;    (tide-hl-identifier-idle-time +1)
;;    (company-mode +1))


;; ;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; ;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;; (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; (add-hook 'lsp-hook #'setup-tide-mode)

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

(defun my-java-mode-hook ()
  (auto-fill-mode)
  (flycheck-mode)
  (git-gutter+-mode)
  (gtags-mode)
  ;(idle-highlight)
  (subword-mode)
  (yas-minor-mode)
  (set-fringe-style '(8 . 0))
  (define-key c-mode-base-map (kbd "C-M-j") 'tkj-insert-serial-version-uuid)
  (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
  (define-key c-mode-base-map (kbd "S-<f7>") 'gtags-find-tag-from-here)
  (define-key c-mode-base-map (kbd "C-t") #'dap-java-run-test-class)

  ;; Fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0))

  ;; Indent arguments on the next line as indented body.
  (c-set-offset 'arglist-intro '++))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;(use-package vterm :ensure t)
;; (add-to-list 'load-path "~/.emacs.d/emacs-libvterm")
;; (require 'vterm)

(use-package projectile :ensure t)
(use-package yasnippet :ensure t)

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
  :hook ((java-mode python-mode go-mode
          js-mode js2-mode typescript-mode web-mode javascript-mode rjsx-mode
          c-mode c++-mode objc-mode) . lsp))

(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "/usr/local/bin/ccls")
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(require 'flymake-google-cpplint)
(add-hook 'c++-mode-hook 'flymake-google-cpplint-load)

(use-package typescript-mode
  :mode "\\.ts\\'"
  ;; :mode "\\.js\\'"
  :mode "\\.tsx\\'"
  ;; :mode "\\.jsx\\'"
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

(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))

(use-package rjsx-mode
  ;; :mode "\\.js\\'"
  ;; :mode "\\.jsx\\'"
  :hook (rjsx-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (require 'dap-node)
  (dap-node-setup))

(require 'yaml-mode)
    (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(use-package hydra :ensure t)
;; (use-package company-lsp)

(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background "lightgrey"))))
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
  (lsp-ui-doc-delay 2)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (if (display-graphic-p)
      (setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(setq lsp-prefer-capf t)

(use-package lsp-java
  :ensure t
  :init
  (setq lsp-java-vmargs
        (list
         "-noverify"
         "-Xmx2G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         "-javaagent:/Users/ben/.m2//repository/org/projectlombok/lombok/1.18.10/lombok-1.18.10.jar"
         )

        ;; Don't organise imports on save
        lsp-java-save-action-organize-imports nil

        ;; Currently (2019-04-24), dap-mode works best with Oracle
        ;; JDK, see https://github.com/emacs-lsp/dap-mode/issues/31
        ;;
        ;; lsp-java-java-path "~/.emacs.d/oracle-jdk-12.0.1/bin/java"
        lsp-java-java-path "/Library/Java/JavaVirtualMachines/jdk-11.0.7.jdk/Contents/Home/bin/java"
        )

  :config
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

  ;; The :bind here makes use-package fail to lead the dap-java block!
  ;; :bind
  ;; (("C-c R" . dap-java-run-test-class)
  ;;  ("C-c d" . dap-java-debug-test-method)
  ;;  ("C-c r" . dap-java-run-test-method)
  ;;  )

  :config
  (global-set-key (kbd "<f7>") 'dap-step-in)
  (global-set-key (kbd "<f8>") 'dap-next)
  (global-set-key (kbd "<f9>") 'dap-continue)
  (global-set-key (kbd "M-b") 'lsp-find-implementation)
  (global-set-key (kbd "C-b") 'lsp-find-implementation)
  ;; (define-key lsp-mode-map (kbd "C-t") #'dap-java-run-test-class)
  (setq dap-java-test-additional-args '("-n" "\".*(Test|IT).*\""))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End Java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



(require 'back-button)
(back-button-mode 1)

(require 'flycheck)

(require 'web-mode)
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

;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . typescript-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . lsp-mode))
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


;; ;; enable typescript-tslint checker
;; (flycheck-add-mode 'typescript-tslint 'lsp-mode)

;; ;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

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
    (dap-java-run-test-class))
  )

(defun npm-test()
  (shell-command (concat "npm test &")
                 "*test-runner*"
                 "*Messages*")
  )

line-spacing(defun go-test()
  (shell-command (concat "cd " (projectile-project-root) "/test && go test &")
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

(require 'company)
(require 'company-box)

;; (require 'company-web-html)                          ; load company mode html backend
;; (require 'company-web-jade)                          ; load company mode jade backend
;; (require 'company-web-slim)                          ; load company mode slim backend
;; ;; (use-package company-mode
;;   :after lsp-mode
;;   :hook (prog-mode . company-mode)
;;   :bind (:map company-active-map
;;               ("<tab>" . company-completion-selection))
;;   (:map lsp-mode-map
;;         ("<tab>" . company-indent-or-complete-common))
;;   (company-minimum-prefix-length 1)
;;   (company-idle-delay 0.0))

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'company-mode-hook 'company-box-mode)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0)
(setq company-lsp-cache-candidates t)


;; (add-to-list 'company-backends 'company-tern)
;; (add-to-list 'company-backends 'ac-js2-company)
;; (setq ac-js2-evaluate-calls t)
;; (add-to-list 'company-backends 'company-flow)
;; (add-to-list 'company-backends 'company-web)


(require 'prettier-js)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'lsp-mode-hook 'prettier-js-mode)
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

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'ruby-test-mode)

(add-hook 'ruby-mode-hook
          (lambda () (rvm-activate-corresponding-ruby)))
(add-hook 'ruby-mode-hook 'yard-mode)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'ruby-mode-hook 'ruby-refactor-mode)
(add-hook 'ruby-mode-hook 'ruby-test-mode)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)

(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

(require 'rvm)
(rvm-use-default)

(setq seeing-is-believing-prefix "C-.")
(add-hook 'ruby-mode-hook 'seeing-is-believing)
(require 'seeing-is-believing)

(add-hook 'ruby-mode-hook 'auto-complete-mode)


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

(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell)
  (setq-default exec-path-from-shell-shell-name "/bin/zsh")
  (exec-path-from-shell-copy-env "PATH")
(exec-path-from-shell-initialize))

(global-set-key (kbd "C-d") 'duplicate-line-or-region)
(global-set-key (kbd "S-<return>") 'insert-line-below)
;; (global-set-key (kbd "M-]") 'osther-window)
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

(require 'all-the-icons)

(when (window-system)
  (tool-bar-mode -1)
  ;; (scroll-bar-mode -1)
  (tooltip-mode -1))

(require 'spaceline-config)
(spaceline-emacs-theme)

(use-package doom-themes
  :config

  ;; Global settings (defaults)
  (solaire-global-mode +1)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; (load-theme 'doom-solarized-light t)
  ;; (load-theme 'doom-material)
  (load-theme 'doom-one-light t)
  ;; (load-theme 'doom-tomorrow-day t)
  ;; (load-theme 'spacemacs-light t)
  ;; (load-theme 'doom-opera-light t)
  ;;; OPTIONAL

  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))


;; (use-package doom-themes
;;   :config
;;   (let ((chosen-theme 'doom-one-light))
;;     (doom-themes-visual-bell-config)
;;     (doom-themes-org-config)
;;     (setq doom-challenger-deep-brighter-comments t
;;           doom-challenger-deep-brighter-modeline t)
;;     (load-theme chosen-theme)))


(when (memq window-system '(mac ns x))
(exec-path-from-shell-initialize))
;; (exec-path-from-shell-copy-env "GEM_PATH")
(projectile-rails-global-mode)

(treemacs)
;; (treemacs-load-theme "Default")
;; (treemacs-load-theme "Netbeans")
;; (treemacs-load-theme "Idea")


;; (setq centaur-tabs-style "box")
;; (setq centaur-tabs-height 32)
;; (setq centaur-tabs-set-icons t)
;; (setq centaur-tabs-gray-out-icons 'buffer)
;; (setq centaur-tabs-set-bar 'under)
;; (setq x-underline-at-descent-line t)
;; (centaur-tabs-mode)
;; (centaur-tabs-headline-match)
;; (centaur-tabs-group-by-projectile-project)
;; (global-set-key (kbd "s-{") 'centaur-tabs-backward)
;; (global-set-key (kbd "s-}") 'centaur-tabs-forward)

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


;; (scroll-bar-mode)

;(setq treemacs-indentation-string (propertize " ⫶ " 'face 'font-lock-comment-face)
;      treemacs-indentation 1)

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
(global-undo-tree-mode 1)
;; make ctrl-z undo
(global-set-key (kbd "s-z") 'undo)
;; make ctrl-Z redo
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "s-Z") 'redo)

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


;(global-set-key [\M-\S-up] 'move-text-up)
;(global-set-key [\M-\S-down] 'move-text-down)
(global-set-key (kbd "S-M-<up>") 'move-text-up)
(global-set-key (kbd "S-M-<down>") 'move-text-down)
;; (global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-w") 'kill-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#fafafa" "#99324b" "#4f894c" "#9a7500" "#3b6ea8" "#97365b" "#398eac" "#2a2a2a"])
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "f8c30fa07ba7e8fe884f22b428dae6724955fa61ad84a658c3b0164ae391fb52" "8c847a5675ece40017de93045a28ebd9ede7b843469c5dec78988717f943952a" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "84da7b37214b4ac095a55518502dfa82633bee74f64daf6e1785322e77516f96" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "030346c2470ddfdaca479610c56a9c2aa3e93d5de3a9696f335fd46417d8d3e4" "a63355b90843b228925ce8b96f88c587087c3ee4f428838716505fd01cf741c8" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" default))
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
   '(idle-highlight-in-visible-buffers-mode smooth-scroll lsp-ui lsp-treemacs lsp-java lsp-mode jest-test-mode yasnippet-snippets clojure-mode-extra-font-locking cider spaceline treemacs-evil jest npm-mode tide find-file-in-project helm-rg ac-js2 company-flow company-tern tern-auto-complete tern treemacs-magit xref-js2 js2-refactor prettier-js company web-mode yard-mode undo-tree rubocop kaolin-themes sublimity minimap magit enh-ruby-mode twilight-bright-theme treemacs-projectile treemacs-icons-dired sublime-themes spacemacs-theme solarized-theme seeing-is-believing rvm ruby-test-mode ruby-refactor ruby-electric rspec-mode recompile-on-save projectile-rails one-themes mocha material-theme leuven-theme intellij-theme helm-projectile helm-ag flatui-theme exec-path-from-shell espresso-theme emr doom-themes color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized chyla-theme chruby centaur-tabs bundler better-defaults auto-complete-exuberant-ctags apropospriate-theme all-the-icons-dired ag ac-inf-ruby))
 '(safe-local-variable-values '((ruby-test-runner . rspec)))
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


(require 'flymake)

;; I don't like the default colors :)
(set-face-background 'flymake-errline "white")
(set-face-background 'flymake-warnline "white")

;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(add-hook 'ruby-mode-hook
          '(lambda ()

	     ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
	     (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
		 (flymake-mode))
	     ))
;; (package-initialize)


(when(eq system-type 'darwin)
    (ignore-errors(set-frame-font "Menlo-15"))
  (add-to-list 'default-frame-alist
               (cons 'font "Menlo-15"))
  (add-to-list 'default-frame-alist
               (cons 'font "Menlo-15"))
)

(when(eq system-type 'windows-nt)
    (ignore-errors(set-frame-font "Consolas"))
  (add-to-list 'default-frame-alist
               (cons 'font "Consolas"))
  (add-to-list 'default-frame-alist
               (cons 'font "Consolas"))
)

;; (defvar line-padding 3)
;; (defun add-line-padding ()
;;   "Add extra padding between lines"

;;   ; remove padding overlays if they already exist
;;   (let ((overlays (overlays-at (point-min))))
;;     (while overlays
;;       (let ((overlay (car overlays)))
;;         (if (overlay-get overlay 'is-padding-overlay)
;;             (delete-overlay overlay)))
;;       (setq overlays (cdr overlays))))

;;   ; add a new padding overlay
;;   (let ((padding-overlay (make-overlay (point-min) (point-max))))
;;     (overlay-put padding-overlay 'is-padding-overlay t)
;;     (overlay-put padding-overlay 'line-spacing (* .1 line-padding))
;;     (overlay-put padding-overlay 'line-height (+ 1 (* .1 line-padding))))
;;   (setq mark-active nil))


(defun xah-toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height.
URL `http://ergoemacs.org/emacs/emacs_toggle_line_spacing.html'
Version 2017-06-02"
  (interactive)
  (if line-spacing
      (setq line-spacing nil)
    (setq line-spacing 0.5))
  (redraw-frame (selected-frame)))

;; (add-hook 'buffer-list-update-hook 'add-line-padding)

(setq-default cursor-type 'bar)
;; (setq line-spacing 2)

; list the repositories containing them




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

;; on the fly syntax checking
(add-hook 'sh-mode-hook 'flycheck-mode)

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

;; Nice window divider in TTY emacs
(defun my-change-window-divider ()
  (let ((display-table (or buffer-display-table standard-display-table)))
    (set-display-table-slot display-table 5 ?│)
    (set-window-display-table (selected-window) display-table)))
(add-hook 'window-configuration-change-hook 'my-change-window-divider)

;; (defun tkj-presentation-mode()
;;   (interactive)
;;   (when window-system
;;     (progn
;;       (use-package one-themes)
;;       (load-theme 'one-light t)
;;       (set-face-attribute 'default nil
;;                           :family "Source Code Pro"
;;                           :height 140
;;                           :weight 'normal
;;                           :width 'normal))))

;; (defun tkj-left-margin-focus()
;;   (interactive)
;;   (set-window-margins nil 10))

;; (defun tkj-left-margin-zero()
;;   (interactive)
;;   (set-window-margins nil 0))

;; (set-default 'truncate-lines t)

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


;; (toggle-frame-maximized)
