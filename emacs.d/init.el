;; We need this comment here or Emacs will add this line automatically.
;; (package-initialize)

;; Load the real init file by extracting elisp code from true-init.org and byte
;; compiling it if it's changed since last time we started Emacs.
(let* ((true-init-org (locate-user-emacs-file "true-init.org"))
       (true-init (file-name-sans-extension true-init-org))
       (true-init-el (concat true-init ".el"))
       (true-init-elc (concat true-init ".elc")))
  (when (or (file-newer-than-file-p true-init-org true-init-elc)
            (file-newer-than-file-p user-init-file true-init-elc))
    (require 'ob-tangle)

    ;; Since our true init file is actually a symbolic link in the .emacs.d
    ;; directory, we need to suppress a confirmation message when visiting the
    ;; file to tangle it.
    (let ((vc-follow-symlinks t))
      (org-babel-tangle-file true-init-org true-init-el "emacs-lisp"))

    (byte-compile-file true-init-el)
    (message "Byte-compiled")
    (delete-file true-init-el))
  (load-file true-init-elc))
