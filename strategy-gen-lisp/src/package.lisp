; src/package.lisp
(defpackage #:strategy-gen-lisp
  (:use #:cl)
  (:export #:main))

(defpackage #:strategy-gen-lisp.config
  (:use #:cl)
  (:export #:load-config #:get-config))

(defpackage #:strategy-gen-lisp.dsl.ast
  (:use #:cl)
  (:export ; Export AST node types or accessors
           ;; Basic structure types
           #:ast-node-p
           #:ast-node-type
           #:ast-node-children
           #:make-ast-node

           ;; Specific node predicates/accessors (simplified)
           #:const-p #:const-value
           #:get-indicator-p #:get-indicator-name
           #:get-prediction-p #:get-prediction-name
           #:get-prev-indicator-p #:get-prev-indicator-name
           #:get-prev-prediction-p #:get-prev-prediction-name
           #:get-var-p #:get-var-name
           #:bin-op-p #:bin-op-operator #:bin-op-arg1 #:bin-op-arg2
           #:builtin-func-p #:builtin-func-name #:builtin-func-args
           #:if-stmt-p #:if-cond #:if-then #:if-else
           #:action-p #:action-type #:action-arg
           #:var-decl-p #:var-decl-name #:var-decl-initial
           #:set-var-p #:set-var-name #:set-var-expr

           ;; Value types
           #:value-p #:value-type #:value-numeric-value #:value-boolean-value
           #:make-numeric-value #:make-boolean-value

           ;; Operator/Function names (keywords)
           #:&:add #:&:sub #:&:mul #:&:div
           #:&:eq #:&:neq #:&:lt #:&:gt #:&:leq #:&:geq
           #:&:and #:&:or
           #:&:abs #:&:min #:&:max

           ;; Action types (keywords)
           #:&:buy #:&:sell #:&:hold #:&:log

           ;; Top-level strategy structure accessors (simplified)
           #:strategy-ast-name #:strategy-ast-initial-state #:strategy-ast-logic
           #:make-strategy-ast
           ))

(defpackage #:strategy-gen-lisp.dsl.generator
  (:use #:cl #:alexandria #:strategy-gen-lisp.dsl.ast)
  (:export #:generate-random-strategy-ast
           #:generate-random-expr
           #:generate-random-statement))

(defpackage #:strategy-gen-lisp.dsl.serializer
  (:use #:cl #:strategy-gen-lisp.dsl.ast)
  (:export #:serialize-strategy-ast-to-dsl))

(defpackage #:strategy-gen-lisp.grpc.backtester
  (:use #:cl #:cl-protobuf #:cl-grpc #:backtester-pb #:strategy-gen-lisp.config #:local-time)
  (:export #:backtest-strategy))

(defpackage #:strategy-gen-lisp.grpc.strategy-engine
  (:use #:cl #:cl-protobuf #:cl-grpc #:strategy-engine-pb #:strategy-gen-lisp.config)
  (:export #:reload-strategies))

(defpackage #:strategy-gen-lisp.gp.core
  (:use #:cl #:alexandria #:strategy-gen-lisp.config #:strategy-gen-lisp.dsl.generator #:strategy-gen-lisp.dsl.serializer #:strategy-gen-lisp.grpc.backtester #:strategy-gen-lisp.grpc.strategy-engine)
  (:export #:run-evolution
           #:genome
           #:genome-ast
           #:genome-dsl-code
           #:genome-fitness
           #:genome-metrics
           #:genome-name
           #:copy-genome
           ))

(defpackage #:strategy-gen-lisp.main
  (:use #:cl #:strategy-gen-lisp.config #:strategy-gen-lisp.gp.core)
  (:export #:run-generator))