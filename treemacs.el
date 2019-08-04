;; Treemacs
(use-package treemacs
  :ensure t
  :config
  (treemacs-follow-mode t)
  (setq treemacs-width 35
        treemacs-display-in-side-window t
        treemacs-indentation-string (propertize " " 'face 'font-lock-comment-face)
        treemacs-indentation 1)
  (add-hook 'treemacs-mode-hook #'hide-mode-line-mode)
  (add-hook 'treemacs-mode-hook (lambda ()
                                  (linum-mode -1)
                                  (fringe-mode 0)
                                  (setq buffer-face-mode-face `(:background "#211C1C"))
                                  (buffer-face-mode 1)))
  ;; Improve treemacs icons
  (with-eval-after-load 'treemacs
    (with-eval-after-load 'all-the-icons
      (let ((all-the-icons-default-adjust 0)
            (tab-width 1))
        ;; Root icon
        (setq treemacs-icon-root-png
              (concat (all-the-icons-octicon "repo" :height 0.8 :v-adjust -0.2)  " "))
        ;; File icons
        (setq treemacs-icon-open-png
              (concat
               (all-the-icons-octicon "chevron-down" :height 0.8 :v-adjust 0.1)
               "\t"
               (all-the-icons-octicon "file-directory" :v-adjust 0)
               "\t")
              treemacs-icon-closed-png
              (concat
               (all-the-icons-octicon "chevron-right" :height 0.8
                                      :v-adjust 0.1 :face 'font-lock-doc-face)
               "\t"
               (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'font-lock-doc-face)
               "\t"))
        ;; File type icons
        (setq treemacs-icons-hash (make-hash-table :size 200 :test #'equal)
              treemacs-icon-fallback (concat
                                      "\t\t"
                                      (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver
                                                            :height 0.8 :v-adjust 0.0)
                                      "\t")
              treemacs-icon-text treemacs-icon-fallback)

        (dolist (item all-the-icons-icon-alist)
          (let* ((extension (car item))
                 (func (cadr item))
                 (args (append (list (caddr item)) '(:v-adjust -0.05) (cdddr item)))
                 (icon (apply func args))
                 (key (s-replace-all '(("^" . "") ("\\" . "") ("$" . "") ("." . "")) extension))
                 (value (concat "\t\t" icon "\t")))
            (unless (ht-get treemacs-icons-hash (s-replace-regexp "\\?" "" key))
              (ht-set! treemacs-icons-hash (s-replace-regexp "\\?" "" key) value))
            (unless (ht-get treemacs-icons-hash (s-replace-regexp ".\\?" "" key))
              (ht-set! treemacs-icons-hash (s-replace-regexp ".\\?" "" key) value))))))))

(use-package treemacs-projectile
  :ensure t)
