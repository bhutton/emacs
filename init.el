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
(global-linum-mode)
(global-hl-line-mode)

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
(global-set-key (kbd "C-d") 'duplicate-line)
(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
(define-key prog-mode-map (kbd "C-t") 'rspec-verify-all)
(define-key prog-mode-map (kbd "C-t") 'rspec-verify-all)
(define-key prog-mode-map (kbd "s-b") 'dumb-jump-go)
(define-key prog-mode-map (kbd "s-[") 'dumb-jump-back)
(define-key prog-mode-map (kbd "s-<s-right>") 'move-end-of-line)
(define-key prog-mode-map (kbd "s-<s-left>") 'back-to-indentation)

(require 'treemacs)
(require 'dash)
(require 'projectile)
(require 'all-the-icons)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(load-theme 'doom-opera-light t)

(when (memq window-system '(mac ns x))
(exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "GEM_PATH")
(projectile-rails-global-mode)

(treemacs)
(setq treemacs-no-png-images t)

(centaur-tabs-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(custom-safe-themes
   (quote
    ("cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "84da7b37214b4ac095a55518502dfa82633bee74f64daf6e1785322e77516f96" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "d5aec3a39364bc4c6c13f472b2d0cdaebd5cff7a6e4839749be2156fcc075006" "1a1cdd9b407ceb299b73e4afd1b63d01bbf2e056ec47a9d95901f4198a0d2428" "8dce5b23232d0a490f16d62112d3abff6babeef86ae3853241a85856f9b0a6e7" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "6fdaae4be8a6ed9c891f655b25579113e8281d0c8ef27a7d20f9beab8d71fe9c" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" default)))
 '(fci-rule-color "#5B6268")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(nrepl-message-colors
   (quote
    ("#183691" "#888a88" "#539100" "#888a88" "#0086b3" "#183691" "#539100" "#888a88")))
 '(objed-cursor-color "#ff6c6b")
 '(package-selected-packages
   (quote
    (recompile-on-save mocha all-the-icons espresso-theme twilight-bright-theme apropospriate-theme material-theme treemacs-projectile sublime-themes spacemacs-theme solarized-theme seeing-is-believing rvm ruby-test-mode ruby-refactor ruby-electric rspec-mode projectile-rails one-themes leuven-theme helm-projectile helm-ag flatui-theme exec-path-from-shell emr dumb-jump color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized chyla-theme chruby centaur-tabs bundler better-defaults auto-complete-exuberant-ctags ag ac-inf-ruby)))
 '(pdf-view-midnight-colors (quote ("#888a88" . "#edf5dc")))
 '(vc-annotate-background "#d5dec4")
 '(vc-annotate-color-map
   (quote
    ((20 . "#888a88")
     (40 . "#183691")
     (60 . "#888a88")
     (80 . "#888a88")
     (100 . "#888a88")
     (120 . "#539100")
     (140 . "#888a88")
     (160 . "#888a88")
     (180 . "#888a88")
     (200 . "#888a88")
     (220 . "#63a35c")
     (240 . "#0086b3")
     (260 . "#183691")
     (280 . "#888a88")
     (300 . "#0086b3")
     (320 . "#888a88")
     (340 . "#539100")
     (360 . "#888a88"))))
 '(vc-annotate-very-old-color "#888a88"))


