;; ~/.emacs.d/functions.el
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
;; Enable line numbers in programming modes
(defun my-prog-mode-line-numbers ()
  (display-line-numbers-mode 1)
  (setq display-line-numbers 'absolute))

(defun show-face-at-point ()
  "Highlight the face at cursor position."
  (interactive)
  (let ((face (get-text-property (point) 'face)))
    (if face
        (message "Face: %S" face)
      (message "No face at point!"))))
(global-set-key (kbd "C-c f") 'show-face-at-point)

(global-set-key (kbd "C-c C-<return>") 'gptel-send)

;; End of functions.el
(provide 'functions)
;;; functions.el ends here
