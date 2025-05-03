; src/grpc/backtester.lisp
(in-package #:strategy-gen-lisp.grpc.backtester)

(defun backtest-strategy (strategy-name dsl-code symbol start-time end-time initial-capital)
  "Calls the BacktesterService to backtest a strategy and returns the response."
  (let* ((address (get-config :grpc :backtester-address))
         (channel (grpc:make-channel address))) ; Make a new channel for the call
    (unwind-protect
         (let ((request (make-backtest-request
                         :strategy-name strategy-name
                         :strategy-dsl-code dsl-code
                         :symbol symbol
                         :start-time (cl-protobuf.well-known.google.protobuf:make-timestamp :seconds (local-time:timestamp-to-unix start-time)) ; Assuming start-time/end-time are local-time timestamps
                         :end-time (cl-protobuf.well-known.google.protobuf:make-timestamp :seconds (local-time:timestamp-to-unix end-time))
                         :initial-capital (coerce initial-capital 'double-float))))
           (format t "Sending backtest request for ~a on ~a (~a to ~a)...~%"
                   strategy-name symbol start-time end-time)
           (let ((response (grpc:invoke (make-instance 'backtester-service)
                                        request
                                        :method :backtest-strategy
                                        :channel channel)))
             (format t "Backtest response received (success: ~a).~%" (backtest-response-success response))
             response))
      (grpc:close-channel channel)))) ; Ensure channel is closed

; Example usage (requires local-time package and running backtester)
;(ql:quickload :local-time)
;(let ((start (local-time:parse-timestring "2023-01-01T00:00:00Z"))
;      (end (local-time:parse-timestring "2023-01-02T00:00:00Z"))
;      (dsl "STRATEGY Test; VAR pos = 0; ON DataUpdate(\"btc_usdt\") { BUY 0.001; SET pos = 1; }"))
;  (backtest-strategy "TestStrategy1" dsl "btc_usdt" start end 10000.0))