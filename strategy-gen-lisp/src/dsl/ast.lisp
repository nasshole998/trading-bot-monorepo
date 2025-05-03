; src/dsl/ast.lisp
(in-package #:strategy-gen-lisp.dsl.ast)

; --- AST Node Representation ---
; We use a simple list structure where the first element is a keyword
; representing the node type, followed by its children/arguments.

(defun ast-node-p (sexp)
  (and (listp sexp) (keywordp (first sexp))))

(defun ast-node-type (sexp)
  (when (ast-node-p sexp)
    (first sexp)))

(defun ast-node-children (sexp)
  (when (ast-node-p sexp)
    (rest sexp)))

(defun make-ast-node (type &rest children)
  (cons type children))

; --- Value Types ---
(defun value-p (sexp)
  (and (listp sexp)
       (member (first sexp) '(:numeric :boolean))))

(defun value-type (value-sexp) (first value-sexp))
(defun value-numeric-value (value-sexp) (second value-sexp))
(defun value-boolean-value (value-sexp) (second value-sexp))

(defun make-numeric-value (value) (list :numeric (coerce value 'double-float)))
(defun make-boolean-value (value) (list :boolean (if value t nil))) ; Use t/nil for boolean values

; --- Specific Node Predicates and Accessors ---
; Expressions
(defun const-p (sexp) (eq (ast-node-type sexp) :const))
(defun const-value (sexp) (second sexp))

(defun get-indicator-p (sexp) (eq (ast-node-type sexp) :get-indicator))
(defun get-indicator-name (sexp) (second sexp))

(defun get-prediction-p (sexp) (eq (ast-node-type sexp) :get-prediction))
(defun get-prediction-name (sexp) (second sexp))

(defun get-prev-indicator-p (sexp) (eq (ast-node-type sexp) :get-prev-indicator))
(defun get-prev-indicator-name (sexp) (second sexp))

(defun get-prev-prediction-p (sexp) (eq (ast-node-type sexp) :get-prev-prediction))
(defun get-prev-prediction-name (sexp) (second sexp))

(defun get-var-p (sexp) (eq (ast-node-type sexp) :get-var))
(defun get-var-name (sexp) (second sexp))

(defun bin-op-p (sexp) (eq (ast-node-type sexp) :bin-op))
(defun bin-op-operator (sexp) (second sexp))
(defun bin-op-arg1 (sexp) (third sexp))
(defun bin-op-arg2 (sexp) (fourth sexp))

(defun builtin-func-p (sexp) (eq (ast-node-type sexp) :builtin-func))
(defun builtin-func-name (sexp) (second sexp))
(defun builtin-func-args (sexp) (cddr sexp)) ; Rest of the list

; Statements
(defun if-stmt-p (sexp) (eq (ast-node-type sexp) :if))
(defun if-cond (sexp) (second sexp))
(defun if-then (sexp) (third sexp)) ; (:then ...) list
(defun if-else (sexp) (fourth sexp)) ; (:else ...) list

(defun action-p (sexp) (eq (ast-node-type sexp) :action))
(defun action-body (sexp) (second sexp)) ; e.g., (:buy expr)

(defun var-decl-p (sexp) (eq (ast-node-type sexp) :var-decl))
(defun var-decl-name (sexp) (second sexp))
(defun var-decl-initial (sexp) (third sexp))

(defun set-var-p (sexp) (eq (ast-node-type sexp) :set-var))
(defun set-var-name (sexp) (second sexp))
(defun set-var-expr (sexp) (third sexp))


; Action Types (within action body)
(defun buy-action-p (sexp) (eq (first sexp) :buy))
(defun buy-action-quantity (sexp) (second sexp))

(defun sell-action-p (sexp) (eq (first sexp) :sell))
(defun sell-action-quantity (sexp) (second sexp))

(defun hold-action-p (sexp) (eq (first sexp) :hold))

(defun log-action-p (sexp) (eq (first sexp) :log))
(defun log-action-expr (sexp) (second sexp))


; --- Top-level Strategy Structure ---
(defun strategy-ast-p (sexp) (eq (ast-node-type sexp) :strategy))
(defun strategy-ast-name (sexp) (getf (rest sexp) :name))
(defun strategy-ast-initial-state (sexp) (getf (rest sexp) :initial-state))
(defun strategy-ast-logic (sexp) (getf (rest sexp) :logic))

(defun make-strategy-ast (name initial-state logic)
  (list :strategy :name name :initial-state initial-state :logic logic))

; --- Utility for Type Checking ---
; Simplified type inference based on AST node
(defun infer-expr-type (expr-sexp type-env)
  (case (ast-node-type expr-sexp)
    (:const (value-type (const-value expr-sexp)))
    ((:get-indicator :get-prev-indicator) :numeric) ; Assume indicators are numeric
    ((:get-prediction :get-prev-prediction) ; Needs lookup in type-env or heuristic
     (let ((name (second expr-sexp)))
       (case (alexandria:starts-with-subseq "buy" name) (:boolean) ; Simple heuristic
             (t :numeric)))) ; Default prediction type
    (:get-var (gethash (get-var-name expr-sexp) type-env)) ; Get type from environment
    (:bin-op (let ((op (bin-op-operator expr-sexp)))
               (case op
                 ((:add :sub :mul :div) :numeric)
                 ((:eq :neq :lt :gt :leq :geq :and :or) :boolean))))
    (:builtin-func (let ((func (builtin-func-name expr-sexp)))
                     (case func
                       ((:abs :min :max) :numeric)))) ; Assume math functions return numeric
    (t (error "Cannot infer type for AST node: ~a" expr-sexp))))

; Simplified type compatibility check for GP crossover/mutation
(defun types-compatible (type1 type2)
  (eq type1 type2)) ; Simple exact match