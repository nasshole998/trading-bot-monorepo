; strategy-gen-lisp.asd
; ASDF system definition for the Strategy Generator

(asdf:defsystem #:strategy-gen-lisp
  :description "Common Lisp component for generating, testing, and optimizing trading strategies."
  :version "0.1.0"
  :author "Your Name <your.email@example.com>"
  :license "MIT"
  :depends-on (#:cl-protobuf
               #:cl-grpc
               #:cl-yaml
               #:alexandria ; For utility functions like random-elt, ensure-list
               #:local-time ; For timestamp handling
              )
  :components ((:module "src"
                :components ((:file "package")
                             (:file "config")
                             (:file "main")
                             (:module "dsl"
                               :components ((:file "ast")
                                            (:file "generator")
                                            (:file "serializer")))
                             (:module "grpc"
                               :components ((:module "gen" ; Generated protobuf code
                                             :components ((:file "backtester_pb")
                                                          (:file "strategy_engine_pb")
                                                          (:file "market_data_pb") ; Added
                                                         )))
                                           (:file "backtester")
                                           (:file "strategy-engine")))
                             (:module "gp"
                               :components ((:file "core")))))))

; Define the main entry point function
(defun strategy-gen-lisp:main ()
  (format t "Starting Strategy Generator...~%")
  (strategy-gen-lisp.main:run-generator)
  (format t "Strategy Generator finished.~%"))