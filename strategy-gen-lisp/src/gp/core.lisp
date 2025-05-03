; src/gp/core.lisp
(in-package #:strategy-gen-lisp.gp.core)

(defstruct genome
  ast      ; The strategy AST (S-expression)
  dsl-code ; Serialized OCaml DSL code
  fitness  ; Performance metric from backtesting (e.g., Sharpe Ratio)
  metrics  ; Full backtest metrics (BacktestResponse)
  name     ; Unique name for the strategy
  )

;; --- Fitness Evaluation ---

(defun evaluate-fitness (genome &optional (symbol (alexandria:random-elt (get-config :strategy-generation :symbols))))
  "Backtests the strategy represented by the genome and calculates its fitness."
  (let* ((strategy-name (genome-name genome))
         (dsl-code (genome-dsl-code genome))
         (start-time (local-time:parse-timestring (get-config :backtest :start-time)))
         (end-time (local-time:parse-timestring (get-config :backtest :end-time)))
         (initial-capital (get-config :strategy-generation :initial-capital))
         (backtest-response (backtester-client:backtest-strategy
                             strategy-name dsl-code symbol start-time end-time initial-capital)))

    (if (backtester-response-success backtest-response)
        ;; Use Sharpe Ratio as fitness (higher is better). Add bonus for trades.
        (let ((sharpe (backtester-response-sharpe-ratio backtest-response))
              (profit (backtester-response-total-profit backtest-response))
              (trades (backtester-response-total-trades backtest-response)))
          ;; Simple fitness function: Sharpe, penalize low trades, maybe bonus profit
          (let ((fitness (if (numberp sharpe)
                             (+ sharpe (* profit 0.001) (if (> trades 0) (min 5.0 (/ trades 10.0)) -2.0)) ; Bonus for trades and profit, penalize no trades
                             -10.0))) ; Use -10.0 for invalid sharpe
             (format t "Evaluated ~a on ~a: Fitness=~a (Sharpe=~a, Profit=~a, Trades=~a)~%"
                     strategy-name symbol fitness sharpe profit trades)
             (setf (genome-fitness genome) fitness)
             (setf (genome-metrics genome) backtest-response)
             genome))
        (progn
          (format t "Backtest failed for ~a on ~a: ~a~%" strategy-name symbol (backtester-response-error-message backtest-response))
          (setf (genome-fitness genome) -20.0) ; Penalize failed backtests heavily
          (setf (genome-metrics genome) backtest-response)
          genome)))))


;; --- Genetic Operators Helpers ---

(defun collect-nodes (ast-sexp node-type-keyword)
  "Collects all nodes of a specific type from an AST (S-expression)."
  (let ((nodes nil))
    (labels ((traverse (sub-ast)
               (when (listp sub-ast)
                 (when (keywordp (first sub-ast))
                   (when (eq (first sub-ast) node-type-keyword)
                     (push sub-ast nodes))
                   (dolist (child (rest sub-ast))
                     (traverse child))))))
      (traverse ast-sexp))
    (nreverse nodes)))

(defun replace-node (ast-sexp old-node new-node)
  "Creates a new AST with old-node replaced by new-node."
  (cond ((eq ast-sexp old-node) new-node)
        ((atom ast-sexp) ast-sexp)
        ((listp ast-sexp) (mapcar (lambda (child) (replace-node child old-node new-node)) ast-sexp))
        (t ast-sexp))) ; Should not happen

(defun node-type-matches (node1 node2)
  "Checks if two AST nodes are of compatible types for crossover/mutation."
  ;; This is a simplified type check based on the node keyword.
  ;; A more rigorous check would use the type-env and infer-expr-type.
  (eq (ast-node-type node1) (ast-node-type node2)))

;; --- Genetic Operators ---

(defun crossover (parent1 parent2)
  "Performs subtree crossover between two genomes."
  (let* ((ast1 (genome-ast parent1))
         (ast2 (genome-ast parent2))
         (nodes1 (collect-nodes ast1 (ast-node-type ast1))) ; Get all nodes (can be refined to specific types)
         (nodes2 (collect-nodes ast2 (ast-node-type ast2))))

    (if (or (null nodes1) (null nodes2))
        ;; Cannot perform crossover if no nodes (shouldn't happen with valid strategies)
        (list (copy-genome parent1) (copy-genome parent2))
        ;; Find a pair of compatible nodes to swap subtrees
        (loop
           repeat 100 ; Try a few times to find compatible nodes
           do (let ((node1 (alexandria:random-elt nodes1))
                    (node2 (alexandria:random-elt nodes2)))
                (when (node-type-matches node1 node2) ; Check compatibility
                  (let* ((new-ast1 (replace-node ast1 node1 node2))
                         (new-ast2 (replace-node ast2 node2 node1))
                         (name1 (format nil "Cross_~a_~a_~a" (genome-name parent1) (genome-name parent2) (random 1000)))
                         (name2 (format nil "Cross_~a_~a_~a" (genome-name parent2) (genome-name parent1) (random 1000))))

                    ;; Re-serialize and create new genomes
                    (return (list (make-genome :ast new-ast1 :dsl-code (dsl.serializer:serialize-strategy-ast-to-dsl new-ast1) :name name1)
                                  (make-genome :ast new-ast2 :dsl-code (dsl.serializer:serialize-strategy-ast-to-dsl new-ast2) :name name2))))))
           finally (format t "Crossover failed to find compatible nodes after 100 tries. Returning copies.~%")
                   (list (copy-genome parent1) (copy-genome parent2))))))


(defun mutate (genome &optional (max-depth (get-config :gp :max-tree-depth)))
  "Mutates a genome by replacing a random subtree."
  (let* ((ast (genome-ast genome))
         (nodes (collect-nodes ast (ast-node-type ast)))) ; Get all nodes
    (if (null nodes)
        genome ; Cannot mutate empty AST
        (let ((node-to-mutate (alexandria:random-elt nodes)))
          ;; Generate a new random subtree of a compatible type
          (let* ((node-type (ast-node-type node-to-mutate))
                 (required-type (case node-type
                                  ((:const :get-indicator :get-prev-indicator :get-prediction :get-prev-prediction :get-var :bin-op :builtin-func) (infer-expr-type node-to-mutate (make-hash-table))) ; Infer type for expressions
                                  ((:if :action :var-decl :set-var) :statement) ; Simplified statement type
                                  (t (error "Cannot determine type for mutation target: ~a" node-to-mutate))))) ; Should not happen
            (let* ((new-subtree (case required-type
                                  (:statement (dsl.generator:generate-random-statement 0 (random max-depth) (make-hash-table))) ; Generate random statement
                                  ((:numeric :boolean) (dsl.generator:generate-random-expr 0 (random max-depth) required-type (make-hash-table))) ; Generate random expression
                                  (t (error "Unexpected required type for new subtree: ~a" required-type))))
                   (new-ast (replace-node ast node-to-mutate new-subtree))
                   (new-name (format nil "Mutated_~a_~a" (genome-name genome) (random 1000))))

              ;; Re-serialize and create new genome
              (make-genome :ast new-ast :dsl-code (dsl.serializer:serialize-strategy-ast-to-dsl new-ast) :name new-name))))))

;; --- Evolution Loop ---

(defun run-evolution ()
  "Runs the Genetic Programming evolution process."
  (let* ((pop-size (get-config :gp :population-size))
         (generations (get-config :gp :generations))
         (crossover-rate (get-config :gp :crossover-rate))
         (mutation-rate (get-config :gp :mutation-rate))
         (tournament-size (get-config :gp :tournament-size))
         (min-depth (get-config :gp :min-tree-depth))
         (max-depth (get-config :gp :max-tree-depth))
         (target-sharpe (get-config :gp :target-sharpe-ratio))
         (current-population (generate-initial-population pop-size min-depth max-depth)))

    ;; Evaluate initial population
    (format t "Evaluating initial population...~%")
    (setf current-population (mapcar #'evaluate-fitness current-population))
    (setf current-population (sort current-population #'> :key #'genome-fitness)) ; Sort by fitness

    (let ((best-genome-overall (copy-genome (first current-population))))
      (format t "Initial population evaluated. Best fitness: ~a (Profit: ~a)~%"
              (genome-fitness best-genome-overall) (backtester-response-total-profit (genome-metrics best-genome-overall)))

      ;; Evolution loop
      (loop for i from 1 to generations
            do (format t "~%--- Generation ~a ---~%" i)
               (let ((next-population nil)
                     (num-elites (floor (* pop-size 0.05)))) ; Simple elitism (top 5%)

                 ;; Add elites to next generation
                 (loop for j from 0 below num-elites
                       do (push (copy-genome (nth j current-population)) next-population))

                 ;; Fill the rest of the population
                 (loop while (< (length next-population) pop-size)
                       do (let ((parents (select-parents current-population 2 tournament-size)))
                            (let ((offspring (if (< (random 1.0) crossover-rate)
                                                 (crossover (first parents) (second parents)) ; Perform crossover
                                                 parents))) ; Or just copy parents (no crossover)

                              (dolist (child offspring)
                                (when (< (random 1.0) mutation-rate)
                                  (setf child (mutate child max-depth))) ; Mutate

                                ;; Ensure we don't exceed population size with offspring
                                (when (< (length next-population) pop-size)
                                   (push child next-population)))))
                       )

                 ;; Evaluate the new population
                 (format t "Evaluating generation ~a population...~%" i)
                 ;; Evaluate on different symbols each generation for diversity
                 (let ((eval-symbol (alexandria:random-elt (get-config :strategy-generation :symbols))))
                    (setf next-population (mapcar (lambda (g) (evaluate-fitness g eval-symbol)) next-population)))

                 (setf current-population (sort next-population #'> :key #'genome-fitness)) ; Sort by fitness

                 (let ((best-genome-this-gen (first current-population)))
                   (format t "Generation ~a evaluated. Best fitness: ~a (Profit: ~a)~%"
                           i (genome-fitness best-genome-this-gen) (backtester-response-total-profit (genome-metrics best-genome-this-gen)))

                   ;; Update overall best strategy
                   (when (> (genome-fitness best-genome-this-gen) (genome-fitness best-genome-overall))
                     (setf best-genome-overall (copy-genome best-genome-this-gen))
                     (format t ">>> New overall best strategy found: ~a with Fitness ~a~%"
                             (genome-name best-genome-overall) (genome-fitness best-genome-overall))))


                 ;; Check termination condition
                 (when (>= (genome-fitness (first current-population)) target-sharpe)
                   (format t "Target fitness (~a) reached by best in current generation! Stopping evolution.~%" target-sharpe)
                   (return))
                 (when (>= (genome-fitness best-genome-overall) target-sharpe)
                    (format t "Target fitness (~a) reached by overall best strategy! Stopping evolution.~%" target-sharpe)
                    (return))
                 )
            finally (format t "~%Evolution finished after ~a generations.~%" generations))

      ;; Return the best strategy found
      (format t "~%Best strategy found: ~a with Fitness ~a (Sharpe: ~a, Profit: ~a, Drawdown: ~a, Trades: ~a)~%"
              (genome-name best-genome-overall)
              (genome-fitness best-genome-overall)
              (if (genome-metrics best-genome-overall) (backtester-response-sharpe-ratio (genome-metrics best-genome-overall)) "N/A")
              (if (genome-metrics best-genome-overall) (backtester-response-total-profit (genome-metrics best-genome-overall)) "N/A")
              (if (genome-metrics best-genome-overall) (backtester-response-max-drawdown (genome-metrics best-genome-overall)) "N/A")
              (if (genome-metrics best-genome-overall) (backtester-response-total-trades (genome-metrics best-genome-overall)) "N/A"))
      best-genome-overall)))

(defun save-strategy (genome output-dir)
  "Saves the genome's DSL code to a file."
  (let ((filepath (merge-pathnames (format nil "~a.strat" (genome-name genome)) output-dir)))
    (format t "Saving best strategy to ~a~%" filepath)
    (ensure-directories-exist filepath)
    (with-open-file (out filepath :direction :output :if-exists :supersede)
      (write-string (genome-dsl-code genome) out))))

(defun run-generator ()
  "Main function to run the strategy generation process."
  ;; Load configuration
  (config:load-config "settings.lisp")

  ;; Ensure required libraries are loaded (done in quicklisp.lisp and Dockerfile)
  ;(ql:quickload :local-time)

  ;; Run the evolution
  (let ((best-strategy (run-evolution)))

    ;; Save the best strategy if backtest succeeded and fitness is reasonable
    (when (and best-strategy (genome-metrics best-strategy) (backtester-response-success (genome-metrics best-strategy)) (> (genome-fitness best-strategy) 0.1)) ; Only save if reasonably fit
      (let ((output-dir (get-config :strategy-generation :output-dir)))
        (save-strategy best-strategy output-dir))

      ;; Trigger strategy reload in OCaml engine
      (strategy-engine-client:reload-strategies))

    (if (and best-strategy (genome-metrics best-strategy) (backtester-response-success (genome-metrics best-strategy)))
        (format t "Generator run complete. Best strategy: ~a (Fitness: ~a). Saved: ~a~%"
                (genome-name best-strategy)
                (genome-fitness best-strategy)
                (if (> (genome-fitness best-strategy) 0.1) "Yes" "No (low fitness)"))
        (format t "Generator run complete. Evolution failed or best strategy had low fitness/failed backtest.~%"))))

;; Helper for tournament selection
(defun select-parents (population count tournament-size)
  (loop repeat count
        collect (let ((tournament (loop repeat tournament-size
                                        collect (alexandria:random-elt population))))
                  (alexandria:extremum tournament #'> :key #'genome-fitness))))