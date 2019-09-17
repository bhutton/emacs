; list the repositories containing them
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
;; (setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))
(add-to-list 'load-path "~/.emacs.d/lisp/")


; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; list the packages you want
(setq package-list '(better-defaults helm helm-projectile helm-ag ruby-electric rvm seeing-is-believing chruby inf-ruby ruby-test-mode))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'better-defaults)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

(setq package-list '(better-defaults chyla))

(cua-selection-mode 1)

(require 'linum)
(defun linum-update-window-scale-fix (win)
  "fix linum for scaled text"
  (set-window-margins win
                      (ceiling (* (if (boundp 'text-scale-mode-step)
                                      (expt text-scale-mode-step
                                            text-scale-mode-amount) 1)
                                  (if (car (window-margins))
                                      (car (window-margins)) 1)
                                  ))))
(advice-add #'linum-update-window :after #'linum-update-window-scale-fix)


;; Typography
                                        ;(set-face-attribute 'default nil
                                        ;                    :family "Source Code Pro"
                                        ;                    :height 150
                                        ;:weight 'normal
                                        ;:width 'normal
                                        ;)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "s-f") #'helm-projectile-ag)
(global-set-key (kbd "s-t") #'helm-projectile-find-file-dwim)
(global-set-key (kbd "s-b") #'dumb-jump-go)
(global-set-key (kbd "s-r") #'replace-string)

;typescript
(setq create-lockfiles nil)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
 (flycheck-mode +1)
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-idle-time 0)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(setq-default typescript-indent-level 2)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'web-mode-hook #'setup-tide-mode)

(eval-after-load "tide"
  '(define-key tide-mode-map (kbd "s-b") 'tide-jump-to-definition))
(eval-after-load "tide"
  '(define-key tide-mode-map (kbd "s-[") 'tide-jump-back))
(eval-after-load "tide"
  '(define-key tide-mode-map (kbd "C-M-l") 'tide-format))


(require 'flycheck)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)
;(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)



;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'web-mode-hook #'setup-tide-mode)
;(require 'ansi-color)
;(defun colorize-compilation-buffer ()
;  (Ansi-color-apply-on-region compilation-filter-start (point-max)))
;(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
;(autoload 'web-mode "web-mode" "Major mode for editing typescript scripts." t)
;(setq auto-mode-alist  (cons '(".tsx$" . web-mode) auto-mode-alist))

(setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))

;JavaScript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
;; (define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

 (require 'rjsx-mode)
 ;; (rjsx-mode)
 (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
 (setq web-mode-content-types-alist
   '(("jsx" . "\\.js[x]?\\'")))

 (add-hook 'rjsx-mode-hook
           (lambda ()
             (setq indent-tabs-mode nil) ;;Use space instead of tab
             (setq js-indent-level 2) ;;space width is 2 (default is 4)
            (setq js2-strict-missing-semi-warning nil))) ;;disable the semicolon warning

;; (add-hook 'js-mode-hook (lambda () (tern-mode t)))

;; (eval-after-load 'tern
;;    '(progn
;;       (require 'tern-auto-complete)
;;       (tern-ac-setup)))

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-tern)
(add-to-list 'company-backends 'ac-js2-company)
(setq ac-js2-evaluate-calls t)
(add-to-list 'company-backends 'company-flow)

(require 'prettier-js)

 (add-hook 'js2-mode-hook 'prettier-js-mode)
 (add-hook 'web-mode-hook 'prettier-js-mode)

 (setq prettier-js-args '(
   "--trailing-comma" "all"
   "--bracket-spacing" "false"
 ))

;; ;; (defun enable-minor-mode (my-pair)
;; ;;   "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
;; ;;   (if (buffer-file-name)
;; ;;       (if (string-match (car my-pair) buffer-file-name)
;; ;;       (funcall (cdr my-pair)))))

;; ;; (add-hook 'web-mode-hook #'(lambda ()
;; ;;                             (enable-minor-mode
;; ;;                              '("\\.jsx?\\'" . prettier-js-mode))))

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

;; ;; (require 'ruby-debug)

;; ;; Autoclose paired syntax elements like parens, quotes, etc
;; ;(add-hook 'ruby-mode-hook 'ruby-electric-mode global-linum-mode global-hl-line-mode)
;;                                         ;(add-hook 'ruby-mode-hook 'ruby-refactor-mode)
(require 'ruby-test-mode)
;; ;; (add-hook 'ruby-mode-hook 'ruby-test-mode)

;; ;; (add-hook 'enh-ruby-mode-hook 'robe-mode)
;; ;; (add-hook 'enh-ruby-mode-hook 'yard-mode)
;; ;; (add-hook 'enh-ruby-mode-hook 'ruby-electric-mode)
;; ;; (add-hook 'enh-ruby-mode-hook 'ruby-refactor-mode)
;; ;; (add-hook 'enh-ruby-mode-hook 'ruby-test-mode)

;; ;; (add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook
          (lambda () (rvm-activate-corresponding-ruby)))
(add-hook 'ruby-mode-hook 'yard-mode)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'ruby-mode-hook 'ruby-refactor-mode)
(add-hook 'ruby-mode-hook 'ruby-test-mode)

(add-hook 'find-file-hook 'linum-mode)
(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)

(add-hook 'javascript-mode-hook 'recompile-on-save-mode)

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

;; ;; (add-hook 'js-mode-hook 'linum-mode)
;; ;; (add-hook 'js-mode-hook 'hl-line-mode)


(delete-selection-mode 1)

 (dumb-jump-mode)
(setq dumb-jump-aggressive nil)
(setq dumb-jump-selector 'ivy)
(setq dumb-jump-force-searcher 'ag)

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'ruby-mode)

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
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

(global-set-key (kbd "s-d") 'duplicate-line-or-region)
(global-set-key (kbd "M-]") 'other-window)
(global-set-key (kbd "M-C-<right>") 'windmove-right)
(global-set-key (kbd "M-C-<left>") 'windmove-left)
(global-set-key (kbd "M-C-<up>") 'windmove-up)
(global-set-key (kbd "M-C-<down>") 'windmove-down)
(global-set-key (kbd "s-<s-right>") 'move-end-of-line)
(global-set-key (kbd "s-<s-left>") 'smart-line-beginning)



(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
(define-key prog-mode-map (kbd "C-t") 'rspec-verify-all)
(define-key prog-mode-map (kbd "s-<s-right>") 'move-end-of-line)
(define-key prog-mode-map (kbd "s-<s-left>") 'smart-line-beginning)
(define-key prog-mode-map (kbd "s-[") 'previous-buffer)
(define-key prog-mode-map (kbd "s-]") 'next-buffer)
(define-key ruby-mode-map (kbd "s-b") 'dumb-jump-go)
(define-key ruby-mode-map (kbd "s-[") 'dumb-jump-back)


(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(define-key js2-mode-map (kbd "s-b") 'dumb-jump-go)
(define-key js2-mode-map (kbd "s-[") 'dumb-jump-back)

(require 'treemacs)
(require 'dash)

(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(require 'all-the-icons)

;; (load-theme 'intellij t)

(require 'spaceline-config)
;; (spaceline-spacemacs-theme)
(spaceline-emacs-theme)

(require 'doom-themes)

;; Global settings (defaults)
;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
       doom-themes-enable-italic t) ; if nil, italics is universally disabled

(load-theme 'doom-solarized-light t)
(load-theme 'spacemacs-light t)
(doom-themes-treemacs-config)
(doom-themes-visual-bell-config)
(doom-themes-org-config)

;; ;; (require 'kaolin-themes)
;; ;; (load-theme 'kaolin-dark t)
;; ;; ;; Apply treemacs customization for Kaolin themes, requires the all-the-icons package.
;; ;; (kaolin-treemacs-theme)


(when (memq window-system '(mac ns x))
(exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "GEM_PATH")
(projectile-rails-global-mode)

(treemacs)
(setq treemacs-no-png-images t)
(setq centaur-tabs-style "box")
(setq centaur-tabs-height 32)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-gray-out-icons 'buffer)
(setq centaur-tabs-set-bar 'over)
(centaur-tabs-mode)
(centaur-tabs-headline-match)
(centaur-tabs-group-by-projectile-project)
(global-set-key (kbd "s-{") 'centaur-tabs-backward)
(global-set-key (kbd "s-}") 'centaur-tabs-forward)


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

;(global-set-key (kbd "S-s-<up>") 'move-line-up)
;(global-set-key (kbd "S-s-<down>") 'move-line-down)
(global-set-key (kbd "S-s-f") 'helm-projectile-find-file)
(global-set-key (kbd "S-<delete>") 'kill-whole-line)


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
(global-set-key (kbd "S-s-<up>") 'move-text-up)
(global-set-key (kbd "S-s-<down>") 'move-text-down)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "s-w") 'kill-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#fafafa" "#99324b" "#4f894c" "#9a7500" "#3b6ea8" "#97365b" "#398eac" "#2a2a2a"])
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "f8c30fa07ba7e8fe884f22b428dae6724955fa61ad84a658c3b0164ae391fb52" "8c847a5675ece40017de93045a28ebd9ede7b843469c5dec78988717f943952a" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "84da7b37214b4ac095a55518502dfa82633bee74f64daf6e1785322e77516f96" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "030346c2470ddfdaca479610c56a9c2aa3e93d5de3a9696f335fd46417d8d3e4" "a63355b90843b228925ce8b96f88c587087c3ee4f428838716505fd01cf741c8" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" default)))
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#000000" :underline
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
     (deprecated :strike-through "#000000"))))
 '(fci-rule-color "#9e9e9e")
 '(j dee-db-requested-breakpoint-face-colors)
 '(jdee-db-active-breakpoint-face-colors (cons "#fafafa" "#3b6ea8"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#fafafa" "#bdbdbd"))
 '(line-spacing 0.2)
 '(objed-cursor-color "#99324b")
 '(package-selected-packages
   (quote
    (spaceline treemacs-evil jest npm-mode tide find-file-in-project helm-rg ac-js2 company-flow company-tern tern-auto-complete tern treemacs-magit rjsx-mode xref-js2 js2-refactor prettier-js company web-mode
               yard-mode undo-tree rubocop kaolin-themes sublimity minimap magit enh-ruby-mode twilight-bright-theme treemacs-projectile treemacs-icons-dired sublime-themes spacemacs-theme solarized-theme seeing-is-believing rvm ruby-test-mode ruby-refactor ruby-electric rspec-mode recompile-on-save projectile-rails one-themes mocha material-theme leuven-theme intellij-theme helm-projectile helm-ag flatui-theme exec-path-from-shell espresso-theme emr dumb-jump doom-themes color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized chyla-theme chruby centaur-tabs bundler better-defaults auto-complete-exuberant-ctags apropospriate-theme all-the-icons-dired ag ac-inf-ruby)))
 '(safe-local-variable-values (quote ((ruby-test-runner . rspec))))
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
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")

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

;; Click [here](https://github.com/hbin/dotfiles-for-emacs) to take a further look.
;; (set-frame-font "Menlo:pixelsize=14")
(set-frame-font "Hack:pixelsize=14")

;; If you use Emacs Daemon mode
(add-to-list 'default-frame-alist
               (cons 'font "Hack:pixelsize=14"))
;; (cons 'font "Menlo:pixelsize=14"))

(defvar line-padding 3)
(defun add-line-padding ()
  "Add extra padding between lines"

  ; remove padding overlays if they already exist
  (let ((overlays (overlays-at (point-min))))
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay 'is-padding-overlay)
            (delete-overlay overlay)))
      (setq overlays (cdr overlays))))

  ; add a new padding overlay
  (let ((padding-overlay (make-overlay (point-min) (point-max))))
    (overlay-put padding-overlay 'is-padding-overlay t)
    (overlay-put padding-overlay 'line-spacing (* .1 line-padding))
    (overlay-put padding-overlay 'line-height (+ 1 (* .1 line-padding))))
  (setq mark-active nil))


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
