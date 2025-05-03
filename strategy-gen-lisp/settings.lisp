; settings.lisp
; Configuration for the Strategy Generator (using Lisp reader syntax)

(defparameter *config*
  '(:grpc
    (:backtester-address "localhost:50054") ; Address of BacktesterService
    (:strategy-engine-address "localhost:50053")) ; Address of StrategyEngineService

    (:strategy-generation
     (:output-dir "../strategies-out/") ; Directory to save generated strategies
     (:symbols ("btc_usdt" "eth_usdt")) ; Symbols to generate/backtest strategies for
     (:initial-capital 10000.0) ; Initial capital for backtests

     (:gp
      (:population-size 100)
      (:generations 50)
      (:crossover-rate 0.8)
      (:mutation-rate 0.1)
      (:tournament-size 5) ; For tournament selection
      (:target-sharpe-ratio 1.5) ; Performance threshold to stop early
      (:min-tree-depth 3) ; For initial random generation
      (:max-tree-depth 10) ; For initial random generation and mutation
     ))

    (:backtest
     (:start-time "2023-01-01T00:00:00Z") ; ISO 8601 format
     (:end-time "2023-12-31T23:59:59Z")
    )
  )
)

; Helper function to access config values
(defun get-config (key &rest path)
  (let ((value (getf *config* key)))
    (if path
        (apply #'get-config value path)
        value)))