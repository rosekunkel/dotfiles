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
    (org-babel-tangle-file startup-org startup-el "emacs-lisp")
    (byte-compile-file startup-el)
    (delete-file startup-el))
  (load-file startup-elc))
