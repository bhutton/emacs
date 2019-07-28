(package-initialize)
; list the repositories containing them
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

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
 )

(setq package-list '(better-defaults chyla))

;; Show line numbers
(defun line-numbers ()
  (global-hl-line-mode))

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


;loads ruby mode when a .rb file is opened.
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(setq auto-mode-alist  (cons '(".rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".rhtml$" . html-mode) auto-mode-alist))

;; Autoclose paired syntax elements like parens, quotes, etc
(add-hook 'ruby-mode-hook 'ruby-electric-mode global-linum-mode global-hl-line-mode)
(add-hook 'ruby-mode-hook 'ruby-refactor-mode)
;(add-hook 'enh-ruby-mode-hook 'robe-mode)
;(add-hook 'enh-ruby-mode-hook 'yard-mode)
;(add-hook 'enh-ruby-mode-hook 'ruby-electric-mode global-linum-mode global-hl-line-mode)
;(add-hook 'enh-ruby-mode-hook 'ruby-refactor-mode)

(add-hook 'javascript-mode-hook 'recompile-on-save-mode)

(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

(rvm-use-default)

(setq seeing-is-believing-prefix "C-.")
(add-hook 'ruby-mode-hook 'seeing-is-believing)
(require 'seeing-is-believing)

(add-hook 'ruby-mode-hook 'auto-complete-mode)
(add-hook 'ruby-mode-hook 'linum-mode)
(add-hook 'ruby-mode-hook 'hl-line-mode)

(add-hook 'js-mode-hook 'linum-mode)
(add-hook 'js-mode-hook 'hl-line-mode)

(require 'ruby-test-mode)
(add-hook 'ruby-mode-hook 'ruby-test-mode)

(delete-selection-mode 1)

(dumb-jump-mode)
(setq dumb-jump-aggressive t)
(setq dumb-jump-selector 'helm)
(setq dumb-jump-force-searcher nil)

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

(global-set-key (kbd "C-d") 'duplicate-line)
(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
(define-key prog-mode-map (kbd "C-t") 'rspec-verify-all)
(define-key prog-mode-map (kbd "C-t") 'rspec-verify-all)
(define-key prog-mode-map (kbd "s-b") 'dumb-jump-go)
(define-key prog-mode-map (kbd "s-[") 'dumb-jump-back)
(define-key prog-mode-map (kbd "s-<s-right>") 'move-end-of-line)
(define-key prog-mode-map (kbd "s-<s-left>") 'smart-line-beginning)
;(define-key prog-mode-map (kbd "s-M-l") 'indent-region)

(require 'treemacs)
(require 'dash)

(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(require 'all-the-icons)

(load-theme 'intellij t)

(when (memq window-system '(mac ns x))
(exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "GEM_PATH")
(projectile-rails-global-mode)

(treemacs)
(setq treemacs-no-png-images t)

(centaur-tabs-mode)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a63355b90843b228925ce8b96f88c587087c3ee4f428838716505fd01cf741c8" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" default)))
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
 '(package-selected-packages
   (quote
    (enh-ruby-mode twilight-bright-theme treemacs-projectile treemacs-icons-dired sublime-themes spacemacs-theme solarized-theme seeing-is-believing rvm ruby-test-mode ruby-refactor ruby-electric rspec-mode recompile-on-save projectile-rails one-themes mocha material-theme leuven-theme intellij-theme helm-projectile helm-ag flatui-theme exec-path-from-shell espresso-theme emr dumb-jump doom-themes color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized chyla-theme chruby centaur-tabs bundler better-defaults auto-complete-exuberant-ctags apropospriate-theme all-the-icons-dired ag ac-inf-ruby))))
