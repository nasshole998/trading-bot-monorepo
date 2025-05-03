; src/dsl/serializer.lisp
(in-package #:strategy-gen-lisp.dsl.serializer)

(defun serialize-value (value-sexp)
  "Serializes a value S-expression to DSL text."
  (destructuring-bind (type value) value-sexp
    (case type
      (:numeric (format nil "~a" (coerce value 'double-float))) ; Ensure float format
      (:boolean (if value "true" "false"))
      (t (error "Unknown value type: ~a" type)))))

(defun serialize-expr (expr-sexp)
  "Serializes an expression S-expression to DSL text."
  (case (ast-node-type expr-sexp)
    (:const (serialize-value (const-value expr-sexp)))
    (:get-indicator (format nil "Indicator(\"~a\")" (get-indicator-name expr-sexp)))
    (:get-prediction (format nil "Prediction(\"~a\")" (get-prediction-name expr-sexp)))
    (:get-prev-indicator (format nil "Indicator(\"~a\").prev" (get-prev-indicator-name expr-sexp)))
    (:get-prev-prediction (format nil "Prediction(\"~a\").prev" (get-prev-prediction-name expr-sexp)))
    (:get-var (format nil "~a" (get-var-name expr-sexp)))
    (:bin-op (format nil "(~a ~a ~a)"
                     (serialize-expr (bin-op-arg1 expr-sexp))
                     (case (bin-op-operator expr-sexp)
                       (:add "+") (:sub "-") (:mul "*") (:div "/")
                       (:eq "==") (:neq "!=") (:lt "<") (:gt ">") (:leq "<=") (:geq ">=")
                       (:and "&&") (:or "||")
                       (t (error "Unknown binary operator: ~a" (bin-op-operator expr-sexp)))))
    (:builtin-func (format nil "~a(~a)"
                           (string-upcase (symbol-name (builtin-func-name expr-sexp)))
                           (format nil "~{~a~^, ~}" (mapcar #'serialize-expr (builtin-func-args expr-sexp)))))
    (t (error "Unknown expression type: ~a" (ast-node-type expr-sexp)))))

(defun serialize-action (action-sexp indent-level)
  "Serializes an action S-expression to DSL text."
  (let ((indent (make-string (* indent-level 2) :initial-element #\space)))
    (case (first action-sexp) ; Action type keyword
      (:buy (format nil "~aBUY ~a;" indent (serialize-expr (second action-sexp))))
      (:sell (format nil "~aSELL ~a;" indent (serialize-expr (second action-sexp))))
      (:hold (format nil "~aHOLD;" indent))
      (:log (format nil "~aLog(~a);" indent (serialize-expr (second action-sexp))))
      (t (error "Unknown action type: ~a" (first action-sexp))))))

(defun serialize-statements (stmts-sexp indent-level)
  "Serializes a list of statement S-expressions to DSL text."
  (format nil "~{~a~%~}" (mapcar (lambda (stmt) (serialize-statement stmt indent-level)) stmts-sexp)))

(defun serialize-statement (stmt-sexp indent-level)
  "Serializes a statement S-expression to DSL text."
  (let ((indent (make-string (* indent-level 2) :initial-element #\space))
        (next-indent (+ indent-level 1)))
    (case (ast-node-type stmt-sexp)
      (:if (destructuring-bind (cond then-list else-list) (ast-node-children stmt-sexp)
             (format nil "~aIF ~a THEN {~%~a~a}~a"
                     indent (serialize-expr cond)
                     (serialize-statements (rest then-list) next-indent) ; Remove :then keyword
                     indent
                     (if (rest else-list) ; Check if else-list has statements after :else
                         (format nil " ELSE {~%~a~a}"
                                 (serialize-statements (rest else-list) next-indent) ; Remove :else keyword
                                 indent)
                         ""))))
      (:action (serialize-action (action-body stmt-sexp) indent-level))
      (:var-decl (format nil "~aVAR ~a = ~a;" indent (var-decl-name stmt-sexp) (serialize-expr (var-decl-initial stmt-sexp))))
      (:set-var (format nil "~aSET ~a = ~a;" indent (set-var-name stmt-sexp) (serialize-expr (set-var-expr stmt-sexp))))
      (t (error "Unknown statement type: ~a" (ast-node-type stmt-sexp))))))

(defun serialize-strategy-ast-to-dsl (strategy-ast-sexp)
  "Serializes a complete strategy AST S-expression to OCaml DSL text."
  (destructuring-bind (&key name initial-state logic) (rest strategy-ast-sexp)
    (format nil "STRATEGY ~a;~%~@{VAR ~a = ~a;~%~}~%ON DataUpdate(\"btc_usdt\") {~%~a}~%" ; Simplified ON block and symbol
            name
            (mapcar (lambda (pair) (list (first pair) (serialize-value (second pair)))) initial-state)
            (serialize-statements logic 1)))) ; Start with indent level 1 for logic block