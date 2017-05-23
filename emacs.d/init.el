;; We need this comment here or Emacs will add this line automatically.
;; (package-initialize)

;; Load the real init file by extracting elisp code from startup.org and byte
;; compiling it if it's changed since last time we started Emacs.
(let* ((startup-org (locate-user-emacs-file "startup.org"))
       (startup (file-name-sans-extension startup-org))
       (startup-el (concat startup ".el"))
       (startup-elc (concat startup ".elc")))
  (when (or (file-newer-than-file-p startup-org startup-elc)
            (file-newer-than-file-p user-init-file startup-elc))
    (require 'ob-tangle)

    ;; Since our startup file is actually a symbolic link in the .emacs.d
    ;; directory, we need to suppress a confirmation message when visiting the
    ;; file to tangle it.
    (let ((vc-follow-symlinks t))
      (org-babel-tangle-file startup-org startup-el "emacs-lisp"))

    (byte-compile-file startup-el)
    (message "Byte-compiled")
    (delete-file startup-el))
  (load-file startup-elc))
