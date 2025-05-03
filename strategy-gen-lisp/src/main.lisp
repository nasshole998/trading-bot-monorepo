; src/main.lisp
(in-package #:strategy-gen-lisp.main)

; This file primarily serves as the entry point.
; The main logic is in gp/core.lisp

(defun run-generator ()
  "Runs the main strategy generator process from the core module."
  (strategy-gen-lisp.gp.core:run-generator))