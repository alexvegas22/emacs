;;; init.el --- V34L emacs config  -*- lexical-binding: t; -*-

;;; Code:
;; y/n for  answering yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)

;; Auto-revert mode
(global-auto-revert-mode t)

;;; Commentary:
;;
(setq vc-follow-symlinks t)
(setq fancy-splash-image  "~/.emacs.d/emacs.png")
;; Write backups to ~/.emacs.d/backup/ to no clutter working directories
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

(require 'package)
;; Load custom functions
(load-file "/home/alex/Code/emacs/functions.el")

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(package-initialize)

;; Enable use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c t") 'vterm)

;; Org Reveal
(require 'org-re-reveal)
(setq org-re-reveal-root "file:./reveal.js-master")

(global-set-key (kbd "C-c n r") 'org-re-reveal-export-to-html)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Org Mode Packages and settings
(setq org-startup-with-inline-images t)
(setq org-hide-emphasis-markers t)
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(add-hook 'org-mode-hook 'visual-line-mode)

;; Plant Uml
(setq org-plantuml-jar-path "~/.emacs.d/plantuml/plantuml.jar")

;; Install kotlin-mode
(use-package kotlin-mode
  :mode "\\.kt\\'"
  :config
  (setq kotlin-tab-width 4))
(use-package web-mode
  :ensure t)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)
;; Install Magit
(use-package magit
  :ensure t
  :pin melpa
  )
(global-set-key (kbd "C-x g") 'magit-status)

(add-hook 'yaml-mode-hook 'flymake-yamllint-setup)

;; Install flycheck for error checking
(use-package flycheck
  :ensure t)
(global-flycheck-mode t)
(setq-default flycheck-disabled-checkers '(css-csslint))

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'yaml-yamllint 'yaml-mode))

(use-package flycheck-kotlin
  :ensure t
  :after kotlin-mode
  :config (flycheck-kotlin-setup))

;; Go
(add-to-list 'load-path "~/.emacs.d/go-mode.el")
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(add-hook 'go-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq go-tab-width 4)
            (setq c-basic-offset 4)))

;; CSS
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

;; HTML
(eval-after-load 'html-mode
  '(flycheck-add-mode 'html-tidy 'html-mode))

;; JavaScript
(eval-after-load 'js2-mode
  '(progn
     (setq-default flycheck-disabled-checkers '(javascript-jshint))
     (flycheck-add-mode 'javascript-eslint 'js2-mode)))
(setq js-indent-level 2)
;; Python
(add-hook 'python-mode-hook 'flycheck-mode)

;; C
(add-hook 'c-mode-hook 'flycheck-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(custom-safe-themes
   '(default))
 '(package-selected-packages
   '(2048-game colorful-mode company company-web crdt dired-preview
	       docker-compose-mode dockerfile-mode doom-themes
	       drag-stuff eglot-java elfeed emacsql emacsql-sqlite
	       flycheck-kotlin flycheck-yamllint flymake-yamllint
	       gnuplot gnuplot-mode gptel hexo k8s-mode kotlin-mode
	       kubernetes lsp-completion lsp-eslint lsp-java
	       lsp-tailwindcss magit man-commands move-text nix-mode
	       oer-reveal org-krita org-latex-impatient org-modern
	       org-pretty-tags org-re-reveal-citeproc
	       org-re-reveal-ref org-roam-ui org-transclusion pandoc
	       php-mode plantuml-mode prettier quelpa
	       quelpa-use-package restclient sly smex svelte-mode tide
	       treesit typescript-mode vue-mode wal-mode yaml
	       yaml-mode yasnippet yasnippet-snippets)))

(with-eval-after-load 'lsp-mode
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil))

(add-hook 'view-mode-hook
          (lambda ()
            (if (eq major-mode 'org-mode)
                (setq view-read-only nil) ; Make buffer writable
              (setq view-read-only t)))) ; Ensure view-read-only for other modes

(add-hook 'view-mode-hook
          (lambda ()
            (when (eq major-mode 'org-mode)
              (image-mode 1)))) ; Enable image display in Org mode

(add-hook 'view-mode-hook
          (lambda ()
            (auto-revert-mode 1))) ; Enable auto-revert in view-mode

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'")

(add-hook 'typescript-mode-hook
          (lambda ()
            (setq-default typescript-indent-level 2)))

(use-package web-mode
  :ensure t
  :mode "\\.tsx\\'")

(add-hook 'web-mode-hook
          (lambda ()
            (setq-default typescript-indent-level 2)))

(use-package tide
  :ensure t
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))

;; -------------------- ORG MODE ----------------------

(setq org-agenda-files '("~/Documents/Notes"))

(setq org-latex-packages-alist '(("margin=2cm" "geometry" nil)))
(define-key global-map (kbd "C-c n e") #'org-latex-export-to-pdf)
(use-package org-krita
  :ensure t
  :quelpa (org-krita :fetcher github :repo "lepisma/org-krita" :files ("*.el" "resources"))
  :config
  (add-hook 'org-mode-hook 'org-krita-mode))
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Documents/Notes")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
      :unnarrowed t)
     ("e" "ets" plain
      ""
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: ets")
      :unnarrowed t)
     ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n g" . org-roam-ui-mode)
         ("C-c n a" . org-roam-alias-add)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config

  (require 'org-roam-protocol)
  (org-roam-setup))

(setq warning-minimum-level :error
      warning-minimum-log-level :error)
;; (setq warning-minimum-level :emergency) ; Only show critical errors

(define-key global-map (kbd "<f12>") #'org-transclusion-add)
(define-key global-map (kbd "C-c n t") #'org-transclusion-mode)
(add-hook 'org-mode-hook 'org-transclusion-mode)
(org-roam-db-autosync-mode)

(setq tramp-remote-shell "/bin/bash -l")

;; Check if no file is specified on startup
(when (not (or buffer-file-name noninteractive (car command-line-args)))
  ;; Load the org-init-file if no file is specified
  (let ((org-init-file "/home/alex/.emacs.d/Welcome.org"))
    (when (file-exists-p org-init-file)
      (load-file org-init-file))))

(setq org-startup-with-inline-images t)
(setq org-html-inline-image-rules '((png . data)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot        . t)
   (emacs-lisp . t)
   (python     . t)
   ;; (c          . t)
   (latex      . t)
   (go         . t)
   ;;    (perl       . t)
   ;;    (python     . t)
   ;;    (kotlin     . t)
   ;;    (dot        . t)
   ;;    (css        . t)
   (plantuml . t)))

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)


(require 'man)
(set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
(set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)

(drag-stuff-global-mode 1)
(drag-stuff-define-keys)
;;; Global variables

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:inherit font-lock-comment-face))))
 '(line-number-current-line ((t (:inherit line-number :foreground "pink" :weight bold)))))

(add-hook 'prog-mode-hook 'my-prog-mode-line-numbers)
(setq gptel-default-mode 'org-mode)
(setq ring-bell-function 'ignore)

(provide 'init)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(put 'dired-find-alternate-file 'disabled nil)
;;; init.el ends here
