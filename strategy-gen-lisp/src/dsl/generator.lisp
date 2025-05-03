; src/dsl/generator.lisp
(in-package #:strategy-gen-lisp.dsl.generator)

;; --- Configuration for Generation ---
;; These define the building blocks for the random ASTs
;; Add/remove elements here to change the types of strategies generated

(defparameter *indicator-names* '("SMA_20_BTCUSDT" "SMA_50_BTCUSDT" "RSI_14_BTCUSDT" "current_price")) ; Add actual names available
(defparameter *prediction-names-numeric* '("price_forecast_next_tick_BTCUSDT")) ; Add actual names available
(defparameter *prediction-names-boolean* '("buy_signal_BTCUSDT" "sell_signal_BTCUSDT")) ; Add actual names available

(defparameter *numeric-constants* '(0.001 0.5 1.0 10.0 1000.0))
(defparameter *boolean-constants* '(t nil))

(defparameter *arithmetic-ops* '(:add :sub :mul :div))
(defparameter *comparison-ops* '(:eq :neq :lt :gt :leq :geq))
(defparameter *logical-ops* '(:and :or))
(defparameter *builtin-funcs* '(:abs :min :max))

(defparameter *action-types* '(:buy :sell :hold :log))

;; Weights for choosing node types during random generation (higher weight = more likely)
(defparameter *expr-generation-weights* '((:const 2)
                                          (:get-indicator 3) (:get-prediction 3)
                                          (:get-prev-indicator 2) (:get-prev-prediction 2)
                                          (:get-var 1) ; Assume some default vars might exist or will be added
                                          (:bin-op 4)
                                          (:builtin-func 2)
                                          ))

(defparameter *stmt-generation-weights* '((:action 4)
                                          (:if 3)
                                          (:var-decl 1) ; Generate VAR declarations
                                          (:set-var 2)  ; Generate SET assignments
                                          ))

;; Helper to pick an element based on weights
(defun pick-weighted (weights)
  (let ((total-weight (reduce #'+ weights :key #'second)))
    (let ((random-value (random total-weight)))
      (loop for (item weight) in weights
            do (if (< random-value weight)
                   (return-from pick-weighted item)
                   (decf random-value weight))))))

;; --- Recursive Generation Functions ---

(defun generate-random-expr (current-depth max-depth required-type &optional (type-env (make-hash-table)))
  "Generates a random expression AST node of the required type."
  (let ((is-terminal (or (>= current-depth max-depth) (< (random 1.0) 0.4)))) ; 40% chance of terminal
    (if is-terminal
        ;; Generate Terminal
        (case required-type
          (:numeric (alexandria:random-elt
                     (append (mapcar (lambda (name) (make-ast-node :get-indicator name)) *indicator-names*)
                             (mapcar (lambda (name) (make-ast-node :get-prev-indicator name)) *indicator-names*)
                             (mapcar (lambda (name) (make-ast-node :get-prediction name)) *prediction-names-numeric*)
                              (mapcar (lambda (name) (make-ast-node :get-prev-prediction name)) *prediction-names-numeric*)
                             (mapcar (lambda (value) (make-ast-node :const (make-numeric-value value))) *numeric-constants*)
                             ;; Add existing numeric VARs
                             (loop for name being the hash-key of type-env
                                   for type being the hash-value of type-env
                                   when (eq type :numeric) collect (make-ast-node :get-var name)))))
          (:boolean (alexandria:random-elt
                     (append (mapcar (lambda (name) (make-ast-node :get-prediction name)) *prediction-names-boolean*)
                              (mapcar (lambda (name) (make-ast-node :get-prev-prediction name)) *prediction-names-boolean*)
                             (mapcar (lambda (value) (make-ast-node :const (make-boolean-value value))) *boolean-constants*)
                              ;; Add existing boolean VARs
                             (loop for name being the hash-key of type-env
                                   for type being the hash-value of type-env
                                   when (eq type :boolean) collect (make-ast-node :get-var name)))))
          (t (error "Unknown required-type for terminal: ~a" required-type)))

        ;; Generate Non-Terminal
        (let ((node-type (pick-weighted *expr-generation-weights*)))
          (case node-type
            (:const (case required-type
                      (:numeric (make-ast-node :const (make-numeric-value (alexandria:random-elt *numeric-constants*))))
                      (:boolean (make-ast-node :const (make-boolean-value (alexandria:random-elt *boolean-constants*))))))
            (:get-indicator (if (eq required-type :numeric)
                               (make-ast-node :get-indicator (alexandria:random-elt *indicator-names*))
                               (generate-random-expr current-depth max-depth required-type type-env))) ; Retry if wrong type
             (:get-prediction (let ((name (alexandria:random-elt (append *prediction-names-numeric* *prediction-names-boolean*))))
                                (if (eq required-type (infer-expr-type (make-ast-node :get-prediction name) (make-hash-table))) ; Simplified type check
                                    (make-ast-node :get-prediction name)
                                    (generate-random-expr current-depth max-depth required-type type-env)))) ; Retry if wrong type
            (:get-prev-indicator (if (eq required-type :numeric)
                                    (make-ast-node :get-prev-indicator (alexandria:random-elt *indicator-names*))
                                    (generate-random-expr current-depth max-depth required-type type-env))) ; Retry
            (:get-prev-prediction (let ((name (alexandria:random-elt (append *prediction-names-numeric* *prediction-names-boolean*))))
                                   (if (eq required-type (infer-expr-type (make-ast-node :get-prev-prediction name) (make-hash-table))) ; Simplified type check
                                       (make-ast-node :get-prev-prediction name)
                                       (generate-random-expr current-depth max-depth required-type type-env)))) ; Retry
            (:get-var (let ((var-name (alexandria:random-elt (alexandria:hash-table-keys type-env))))
                        (if (and var-name (eq required-type (gethash var-name type-env)))
                            (make-ast-node :get-var var-name)
                            (generate-random-expr current-depth max-depth required-type type-env)))) ; Retry if no vars or wrong type
            (:bin-op (case required-type
                       (:numeric (make-ast-node :bin-op (alexandria:random-elt *arithmetic-ops*)
                                                (generate-random-expr (+ current-depth 1) max-depth :numeric type-env)
                                                (generate-random-expr (+ current-depth 1) max-depth :numeric type-env)))
                       (:boolean (alexandria:random-elt
                                  `((:bin-op ,(alexandria:random-elt *logical-ops*)
                                             ,(generate-random-expr (+ current-depth 1) max-depth :boolean type-env)
                                             ,(generate-random-expr (+ current-depth 1) max-depth :boolean type-env))
                                    (:bin-op ,(alexandria:random-elt *comparison-ops*)
                                             ,(generate-random-expr (+ current-depth 1) max-depth :numeric type-env) ; Compare numeric
                                             ,(generate-random-expr (+ current-depth 1) max-depth :numeric type-env)))))
                       (t (error "Unknown required-type for bin-op: ~a" required-type))))
            (:builtin-func (let ((func (alexandria:random-elt *builtin-funcs*)))
                             (case func
                               (:abs (if (eq required-type :numeric)
                                         (make-ast-node :builtin-func :abs (generate-random-expr (+ current-depth 1) max-depth :numeric type-env))
                                         (generate-random-expr current-depth max-depth required-type type-env))) ; Retry
                               ((:min :max) (if (eq required-type :numeric)
                                                (make-ast-node :builtin-func func
                                                               (generate-random-expr (+ current-depth 1) max-depth :numeric type-env)
                                                               (generate-random-expr (+ current-depth 1) max-depth :numeric type-env))
                                                (generate-random-expr current-depth max-depth required-type type-env)))))) ; Retry
            (t (error "Unknown AST node type for generation: ~a" node-type))))))


(defun generate-random-action-node (current-depth max-depth type-env)
  "Generates a random action node."
  (let ((action-type (alexandria:random-elt *action-types*)))
    (case action-type
      (:buy (make-ast-node :buy (generate-random-expr (+ current-depth 1) max-depth :numeric type-env))) ; Quantity expression
      (:sell (make-ast-node :sell (generate-random-expr (+ current-depth 1) max-depth :numeric type-env))) ; Quantity expression
      (:hold :hold)
      (:log (make-ast-node :log (generate-random-expr (+ current-depth 1) max-depth (alexandria:random-elt '(:numeric :boolean)) type-env))) ; Log any value
      (t (error "Unknown action type for generation: ~a" action-type)))))


(defun generate-random-statement (current-depth max-depth type-env)
  "Generates a random statement node (including VAR/SET)."
  (let ((stmt-type (pick-weighted *stmt-generation-weights*)))
    (case stmt-type
      (:action (make-ast-node :action (generate-random-action-node current-depth max-depth type-env)))
      (:if (make-ast-node :if (generate-random-expr (+ current-depth 1) max-depth :boolean type-env) ; Condition
                           (list* :then (generate-random-statement-list (+ current-depth 2) max-depth type-env)) ; Then block
                           (list* :else (generate-random-statement-list (+ current-depth 2) max-depth type-env)))) ; Else block
      (:var-decl
       ;; Decide type and generate initial value
       (let ((var-name (gensym "VAR")) ; Generate unique name
             (var-type (alexandria:random-elt '(:numeric :boolean))))
         ;; Note: VAR_DECLs should ideally not be generated within other blocks,
         ;; but the AST structure allows it. The OCaml parser extracts them.
         ;; We'll generate them where possible and rely on extraction.
         (make-ast-node :var-decl var-name (generate-random-expr (+ current-depth 1) max-depth var-type type-env))))
      (:set-var
       ;; Pick an existing variable to set
       (let ((var-names (alexandria:hash-table-keys type-env)))
         (if var-names
             (let ((var-name (alexandria:random-elt var-names)))
               (make-ast-node :set-var var-name (generate-random-expr (+ current-depth 1) max-depth (gethash var-name type-env) type-env)))
             ;; If no variables exist, generate a different statement
             (generate-random-statement current-depth max-depth type-env))))
      (t (error "Unknown statement type for generation: ~a" stmt-type)))))


(defun generate-random-statement-list (current-depth max-depth type-env &optional (max-stmts 3))
  "Generates a list of random statements."
  (let ((num-stmts (random max-stmts)))
    (loop repeat num-stmts
          collect (generate-random-statement current-depth max-depth type-env))))


(defun generate-random-strategy-ast (min-depth max-depth &optional (name (format nil "GeneratedStrategy_~a" (random 100000))))
  "Generates a complete random strategy AST (S-expression)."
  (let ((type-env (make-hash-table)) ; Type environment for variables
        (var-decls nil)
        (logic nil))

    ;; Generate some initial VAR declarations and populate type-env
    (let ((num-initial-vars (random 3))) ; Generate 0 to 2 initial vars
      (loop repeat num-initial-vars do
        (let ((var-name (gensym "InitialVAR"))
              (var-type (alexandria:random-elt '(:numeric :boolean))))
          (let ((var-decl (make-ast-node :var-decl var-name (generate-random-expr 0 3 var-type type-env)))) ; Initial values are simpler expressions
             (push var-decl var-decls)
             (setf (gethash var-name type-env) var-type)))))

    ;; Generate the main logic statements
    (setf logic (generate-random-statement-list 0 max-depth type-env))

    ;; Combine VAR decls and logic (relying on OCaml parser to extract VARs)
    (let* ((all-stmts (append var-decls logic))
           (initial-state-sexp (loop for decl in var-decls collect (list (var-decl-name decl) (var-decl-initial decl))))) ; Collect initial states from generated decls

      (make-strategy-ast name initial-state-sexp all-stmts))))

; Example usage:
;(generate-random-strategy-ast 3 7)