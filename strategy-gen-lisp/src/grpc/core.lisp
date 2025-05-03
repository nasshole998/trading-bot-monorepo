; src/gp/core.lisp
(in-package #:strategy-gen-lisp.gp.core)

(defstruct genome
  ast      ; The strategy AST (S-expression)
  dsl-code ; Serialized OCaml DSL code
  fitness  ; Performance metric from backtesting (e.g., Sharpe Ratio)
  metrics  ; Full backtest metrics (BacktestResponse)
  name     ; Unique name for the strategy
  )

(defun evaluate-fitness (genome)
  "Backtests the strategy represented by the genome and calculates its fitness."
  (let* ((strategy-name (genome-name genome))
         (dsl-code (genome-dsl-code genome))
         (symbol (alexandria:random-elt (get-config :strategy-generation :symbols))) ; Pick a random symbol to backtest on
         (start-time (local-time:parse-timestring (get-config :backtest :start-time)))
         (end-time (local-time:parse-timestring (get-config :backtest :end-time)))
         (initial-capital (get-config :strategy-generation :initial-capital))
         (backtest-response (backtester-client:backtest-strategy
                             strategy-name dsl-code symbol start-time end-time initial-capital)))

    (if (backtester-response-success backtest-response)
        ;; Use Sharpe Ratio as fitness (higher is better)
        (let ((sharpe (backtester-response-sharpe-ratio backtest-response)))
          (format t "Evaluated ~a: Sharpe=~a, Profit=~a~%"
                  strategy-name sharpe (backtester-response-total-profit backtest-response))
          (setf (genome-fitness genome) (if (numberp sharpe) sharpe -1.0)) ; Use -1.0 for invalid sharpe
          (setf (genome-metrics genome) backtest-response)
          genome)
        (progn
          (format t "Backtest failed for ~a: ~a~%" strategy-name (backtester-response-error-message backtest-response))
          (setf (genome-fitness genome) -10.0) ; Penalize failed backtests heavily
          (setf (genome-metrics genome) backtest-response)
          genome)))))


(defun generate-initial-population (size min-depth max-depth)
  "Generates a population of random genomes."
  (format t "Generating initial population of size ~a...~%" size)
  (loop repeat size
        collect (let* ((ast (dsl.generator:generate-random-strategy-ast min-depth max-depth
                                                                       (format nil "Generated_~a" (random 100000)))) ; Unique name
                     (dsl-code (dsl.serializer:serialize-strategy-ast-to-dsl ast)))
                  (make-genome :ast ast :dsl-code dsl-code :name (cadr (getf (rest ast) :name)))))) ; Extract name from AST sexp

(defun select-parents (population count &optional (tournament-size 3))
  "Selects a list of parents using tournament selection."
  (format t "Selecting ~a parents...~%" count)
  (loop repeat count
        collect (let ((tournament (loop repeat tournament-size
                                        collect (alexandria:random-elt population))))
                  (alexandria:extremum tournament #'> :key #'genome-fitness))))

(defun crossover (parent1 parent2)
  "Performs crossover between two genomes (framework - actual implementation is complex)."
  (format t "Performing crossover... (framework only)~%")
  ;; This is a highly simplified placeholder.
  ;; A real implementation involves selecting random subtrees in parent1 and parent2's ASTs
  ;; and swapping them, ensuring the resulting ASTs are still valid (type safety is key).
  ;; For now, just return copies of parents (no crossover).
  (list (copy-genome parent1) (copy-genome parent2)))

(defun mutate (genome &optional (max-depth 10))
  "Mutates a genome (framework - actual implementation is complex)."
  (format t "Mutating genome ~a... (framework only)~%" (genome-name genome))
  ;; A real implementation selects a random node in the AST and replaces it
  ;; with a new randomly generated valid subtree (using dsl.generator).
  ;; For now, just return the original genome.
  (copy-genome genome))

(defun run-evolution ()
  "Runs the Genetic Programming evolution process."
  (let* ((pop-size (get-config :gp :population-size))
         (generations (get-config :gp :generations))
         (crossover-rate (get-config :gp :crossover-rate))
         (mutation-rate (get-config :gp :mutation-rate))
         (min-depth (get-config :gp :min-tree-depth))
         (max-depth (get-config :gp :max-tree-depth))
         (target-sharpe (get-config :gp :target-sharpe-ratio))
         (current-population (generate-initial-population pop-size min-depth max-depth)))

    ;; Evaluate initial population
    (format t "Evaluating initial population...~%")
    (setf current-population (mapcar #'evaluate-fitness current-population))
    (setf current-population (sort current-population #'> :key #'genome-fitness)) ; Sort by fitness

    (format t "Initial population evaluated. Best fitness: ~a~%" (genome-fitness (first current-population)))

    ;; Evolution loop
    (loop for i from 1 to generations
          do (format t "~%--- Generation ~a ---~%" i)
             (let ((next-population nil)
                   (elite (loop repeat (floor (* pop-size 0.05)) ; Simple elitism (top 5%)
                                 collect (copy-genome (nth i current-population)))))

               (setf next-population elite) ; Add elites to next generation

               ;; Fill the rest of the population
               (loop while (< (length next-population) pop-size)
                     do (let ((parents (select-parents current-population 2)))
                          (let ((offspring (if (< (random 1.0) crossover-rate)
                                               (crossover (first parents) (second parents)) ; Perform crossover
                                               parents))) ; Or just copy parents

                            (dolist (child offspring)
                              (when (< (random 1.0) mutation-rate)
                                (mutate child max-depth)) ; Mutate
                              (push child next-population))))
                     )

               ;; Evaluate the new population
               (format t "Evaluating generation ~a population...~%" i)
               (setf next-population (mapcar #'evaluate-fitness next-population))
               (setf current-population (sort next-population #'> :key #'genome-fitness)) ; Sort by fitness

               (let ((best-genome-this-gen (first current-population)))
                 (format t "Generation ~a evaluated. Best fitness: ~a (Profit: ~a)~%"
                         i (genome-fitness best-genome-this-gen) (backtester-response-total-profit (genome-metrics best-genome-this-gen))))

               ;; Check termination condition
               (when (>= (genome-fitness (first current-population)) target-sharpe)
                 (format t "Target fitness (~a) reached! Stopping evolution.~%" target-sharpe)
                 (return)))
          finally (format t "~%Evolution finished after ~a generations.~%" generations))

    ;; Return the best strategy found
    (let ((best-strategy (first current-population)))
      (format t "~%Best strategy found: ~a with Fitness ~a (Profit: ~a, Drawdown: ~a, Trades: ~a)~%"
              (genome-name best-strategy)
              (genome-fitness best-strategy)
              (backtester-response-total-profit (genome-metrics best-strategy))
              (backtester-response-max-drawdown (genome-metrics best-strategy))
              (backtester-response-total-trades (genome-metrics best-strategy)))
      best-strategy)))

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

  ;; Ensure protobuf systems are loaded (done in quicklisp.lisp and Dockerfile)
  ;(ql:quickload :cl-protobuf)
  ;(ql:quickload :cl-grpc)

  ;; Ensure required libraries are loaded
  (ql:quickload :local-time) ; For timestamp handling

  ;; Run the evolution
  (let ((best-strategy (run-evolution)))

    ;; Save the best strategy
    (let ((output-dir (get-config :strategy-generation :output-dir)))
      (save-strategy best-strategy output-dir))

    ;; Trigger strategy reload in OCaml engine
    (strategy-engine-client:reload-strategies)))