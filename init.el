; list the repositories containing them
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; list the packages you want
(setq package-list '(better-defaults solarized-theme helm helm-projectile helm-ag ruby-electric rvm seeing-is-believing chruby inf-ruby ruby-test-mode))


; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


(require 'better-defaults)
(require 'color-theme-sanityinc-tomorrow)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-day)))
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(package-selected-packages
   (quote
    (bundler projectile-rails exec-path-from-shell spacemacs-theme ruby-refactor emr dumb-jump rspec-mode centaur-tabs auto-complete-exuberant-ctags ## auto-complete color-theme-sanityinc-tomorrow better-defaults)))
 '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq package-list '(better-defaults solarized-theme))

;; Show line numbers
(global-linum-mode)

;; Typography
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 150
                    :weight 'normal
                    :width 'normal)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "s-f") #'helm-projectile-ag)
(global-set-key (kbd "s-t") #'helm-projectile-find-file-dwim)

;; Autoclose paired syntax elements like parens, quotes, etc
(add-hook 'ruby-mode-hook 'ruby-electric-mode)

(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

(rvm-use-default)

(setq seeing-is-believing-prefix "C-.")
(add-hook 'ruby-mode-hook 'seeing-is-believing)
(require 'seeing-is-believing)

(add-hook 'ruby-mode-hook 'auto-complete-mode)

(require 'ruby-test-mode)
(add-hook 'ruby-mode-hook 'ruby-test-mode)

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
(global-set-key (kbd "C-d") 'duplicate-line)
(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
(define-key prog-mode-map (kbd "C-t") 'rspec-verify-all)

(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)


(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)

(load-theme 'spacemacs-light t)

(when (memq window-system '(mac ns x))
(exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "GEM_PATH")
(projectile-rails-global-mode)

(treemacs)
