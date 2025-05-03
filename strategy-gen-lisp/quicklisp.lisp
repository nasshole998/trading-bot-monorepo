; quicklisp.lisp
; Loads Quicklisp. Run this first if Quicklisp isn't already loaded.

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

; Ensure required systems are loaded
(ql:quickload :cl-protobuf) ; For Protobuf
(ql:quickload :cl-grpc)     ; For gRPC
(ql:quickload :cl-yaml)     ; For YAML config (alternative to Lisp reader config)

; Add directories to ASDF's source registry so it can find our local systems
(push (truename ".") asdf:*central-registry*)

; Load our local systems
(ql:quickload :strategy-gen-lisp) ; This loads all modules defined in strategy-gen-lisp.asd