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

;; Install Ewal (pywal in emacs)
(use-package ewal
  :init (setq ewal-use-built-in-always-p nil))

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
;; Install projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))
(setq projectile-switch-project-action #'projectile-commander)

;; Install flycheck for error checking
(use-package flycheck
  :ensure t)
(global-flycheck-mode t)
(setq-default flycheck-disabled-checkers '(css-csslint))
(with-current-buffer "*scratch*"
  (flycheck-mode -1))

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
 '(package-selected-packages
   '(company company-web crdt dired-preview docker-compose-mode
	     dockerfile-mode drag-stuff eglot-java elfeed emacsql
	     emacsql-sqlite ewal flycheck-kotlin flymake-yamllint
	     gnuplot gnuplot-mode k8s-mode kotlin-mode kubernetes
	     lsp-java lsp-mode magit man-commands move-text nix-mode
	     oer-reveal org-bulletproof org-bullets org-krita
	     org-latex-impatient org-modern org-pretty-tags
	     org-re-reveal-citeproc org-re-reveal-ref org-roam-ui
	     org-super-agenda org-transclusion pandoc php-mode
	     plantuml-mode projectile quelpa quelpa-use-package
	     restclient sly smex svelte-mode tide typescript-mode yaml
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

(defun insert-org-src-block (language)
  "Insert an Org source code block for the specified LANGUAGE with completion."
  (interactive
   (list (completing-read "Enter language: "
                          '("elisp" "python" "plantuml" "shell" "bash" "java" "javascript"
                            "c" "cpp" "rust" "go" "haskell" "ruby" "perl" "r"
                            "latex" "org" "yaml" "json" "html" "css" "sql" "text")
                          nil t)))
  (let ((start (point)))
    (insert (format "#+BEGIN_SRC %s\n  \n#+END_SRC\n" language))
    (goto-char (+ start (length (format "#+BEGIN_SRC %s\n" language)) 2))))

(global-set-key (kbd "C-c n s") 'insert-org-src-block)

(defun paste-image-from-clipboard ()
  "Paste an image from the clipboard as a file in the images/ folder."
  (interactive)
  (let* ((dir "./images/")  ;; Folder to save the image
         (filename (concat dir (make-temp-name "img-") ".png")))
    (unless (file-exists-p dir)
      (make-directory dir))  ;; Create the images folder if it doesn't exist
    (message "Saving image to: %s" filename)
    (shell-command (concat "wl-paste --type image/png > " filename) t)  ;; Use wl-clipboard
    (insert (concat "[[file:" filename "]]"))
    (message "Image saved as %s" filename)))

(global-set-key (kbd "C-c p") 'paste-image-from-clipboard)

(global-set-key (kbd "C-c b e") 'base64-encode-region)
(global-set-key (kbd "C-c b d") 'base64-decode-region)

  ;;Renders Images inline of an org file
  (defun do-org-show-all-inline-images ()
    (interactive)
    (org-display-inline-images t t))
  (global-set-key (kbd "C-c i")
                  'do-org-show-all-inline-images)

(require 'man)
(set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
(set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)

(drag-stuff-global-mode 1)
(drag-stuff-define-keys )
;;; Global variables

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Enable relative line numbers in programming modes
(defun my-prog-mode-line-numbers ()
  (display-line-numbers-mode 1)
  (setq display-line-numbers 'relative))

(add-hook 'prog-mode-hook 'my-prog-mode-line-numbers)
(setq ring-bell-function 'ignore)

(provide 'init)

;;; init.el ends here
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
