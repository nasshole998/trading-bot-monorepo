; src/grpc/strategy-engine.lisp
(in-package #:strategy-gen-lisp.grpc.strategy-engine)

(defun reload-strategies ()
  "Calls the StrategyEngineService to trigger a strategy reload."
  (let* ((address (get-config :grpc :strategy-engine-address))
         (channel (grpc:make-channel address))) ; Make a new channel for the call
    (unwind-protect
         (let ((request (make-reload-strategies-request))) ; Empty request
           (format t "Sending ReloadStrategies request to Strategy Engine...~%")
           (let ((response (grpc:invoke (make-instance 'strategy-engine-service)
                                        request
                                        :method :reload-strategies
                                        :channel channel)))
             (format t "ReloadStrategies response received (success: ~a, msg: ~a).~%"
                     (reload-strategies-response-success response)
                     (reload-strategies-response-message response))
             response))
      (grpc:close-channel channel)))) ; Ensure channel is closed

; Example usage (requires running OCaml Strategy Engine)
;(reload-strategies)