; src/config.lisp
(in-package #:strategy-gen-lisp.config)

; Define the *config* variable globally (defined in settings.lisp)
(defparameter *config* nil)

(defun load-config (filepath)
  "Loads configuration from a Lisp file."
  (with-open-file (in filepath :direction :input)
    (setf *config* (read in))))

(defun get-config (key &rest path)
  "Helper function to access configuration values."
  (let ((value (getf *config* key)))
    (if path
        (apply #'get-config value path)
        value)))

; Example usage (will be called from main.lisp)
;(load-config "settings.lisp")
;(format t "Backtester address: ~a~%" (get-config :grpc :backtester-address))