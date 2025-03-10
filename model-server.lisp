;;;; Copyright 2025 Carnegie Mellon University

(ql:quickload '(:cl-interpol :alexandria :iterate :cl-ppcre
                :cl-json :usocket :uiop :vom)
              :silent t)

(asdf:load-system :usocket-server)

(defpackage :scale-act-up-interface
  (:nicknames :scale)
  (:use :common-lisp :alexandria :iterate :ppcre :json :usocket)
  (:export #:run))

(in-package :scale)

(vom:config t :debug)

(define-constant +default-port+ 21952)

(defun %run-model (parameters raw-data generate-raw-data-p)
  (cond ((fboundp 'run-model)
         (run-model parameters raw-data generate-raw-data-p))
        (t (format t "~&parameters: ~:W~%raw-data: ~:W~%~:[do not ~;~]generate raw data~2%"
                   parameters raw-data generate-raw-data-p)
           "done")))

(defun tcp-handler (stream)
  (labels ((respond (obj)
             (handler-case (let ((response (encode-json-to-string obj)))
                             (vom:debug "Encode response ~S" response)
                             (format stream "~A~%" response))
               (error (e)
                 (vom:error "Error encoding JSON or writing to TCP stream (~S)" obj)
                 (encode-json #?'"write error"' stream)
                 (terpri stream)))
             (finish-output stream)
             (return-from tcp-handler)))
    (respond "foo")
    (macrolet ((with-error-handled ((msg ret) &body body)
                 `(handler-case (progn ,@body)
                    (error (e)
                      (vom:error "~A ~S" ,msg e)
                      #+SBCL (sb-debug:print-backtrace)
                      (respond ,ret)))))
      (let ((line (with-error-handled ("error reading TCP stream" "read error")
                    (read-line stream))))
        (vom:debug "Read line ~S" line)
        (let ((json (with-error-handled ("error parsing JSON" "JSON parse error")
                      (decode-json-from-string line))))
          (vom:debug "Parsed JSON ~S" json)
          (multiple-value-bind (params raw-data generate-raw-data-p)
              (with-error-handled ("error restructuring JSON" "unexpected input data format")
                (restructure-json json))
            (vom:debug "Restructured JSON ~S (~S) ~A"
                       params raw-data generate-raw-data-p)
            (let ((result (with-error-handled ("error running model" "model error")
                            (%run-model params raw-data generate-raw-data-p))))
              (vom:debug "Model returned ~S" result)
              (respond result))))))))

(defun run (&optional(interactive (member :swank *features*)) (port +default-port+))
  (vom:info "Starting SCALE model listener on port ~D" port)
  (labels ((quit (n)
             (unless interactive
               (uiop:quit n))))
    (handler-case (socket-server nil port 'tcp-handler)
      (error (e)
        (vom:error "top level error ~S" e)
        #+SBCL (sb-debug:print-backtrace)
        (quit 1))
      #+SBCL
      (sb-sys:interactive-interrupt ()
        (vom:info "Stopping SCALE model listener")
        (quit 0)))))



#|
((:MODELS
  ((:NAME . "ACT-R")
   (:PARAMETERS
    ((:NAME . "noise") (:VALUE . 0.25) (:UNIT-OF-MEASURE)
     (:PARAMETER-CLASS . "model") (:PARAMETER-SUB-CLASS . "architecture"))
    ((:NAME . "temperature") (:VALUE . 1.0) (:UNIT-OF-MEASURE)
     (:PARAMETER-CLASS . "model") (:PARAMETER-SUB-CLASS . "architecture"))
    ((:NAME . "similarity")
     (:VALUE (:INTEGER . "functionName") (:DOUBLE . "functionName"))
     (:UNIT-OF-MEASURE) (:PARAMETER-CLASS . "model")
     (:PARAMETER-SUB-CLASS . "knowledge"))
    ((:NAME . "utility")
     (:VALUE (:INTEGER . "functionName") (:DOUBLE . "functionName"))
     (:UNIT-OF-MEASURE) (:PARAMETER-CLASS . "model")
     (:PARAMETER-SUB-CLASS . "utility"))
    ((:NAME . "decision") (:VALUE . "functionName") (:UNIT-OF-MEASURE)
     (:PARAMETER-CLASS . "model") (:PARAMETER-SUB-CLASS . "procedure"))
    ((:NAME . "init length") (:VALUE . 10) (:UNIT-OF-MEASURE)
     (:PARAMETER-CLASS . "simulation") (:PARAMETER-SUB-CLASS . "simulation"))
    ((:NAME . "run length") (:VALUE . 100) (:UNIT-OF-MEASURE)
     (:PARAMETER-CLASS . "simulation") (:PARAMETER-SUB-CLASS . "simulation"))
    ((:NAME . "run delay") (:VALUE . 1.0) (:UNIT-OF-MEASURE)
     (:PARAMETER-CLASS . "simulation") (:PARAMETER-SUB-CLASS . "simulation"))
    ((:NAME . "run count") (:VALUE . 100) (:UNIT-OF-MEASURE)
     (:PARAMETER-CLASS . "simulation") (:PARAMETER-SUB-CLASS . "simulation"))
    ((:NAME . "probability threshold") (:VALUE . 0.25) (:UNIT-OF-MEASURE)
     (:PARAMETER-CLASS . "simulation") (:PARAMETER-SUB-CLASS . "policy"))
    ((:NAME . "intensity threshold") (:VALUE . 1) (:UNIT-OF-MEASURE)
     (:PARAMETER-CLASS . "simulation") (:PARAMETER-SUB-CLASS . "policy"))
    ((:NAME . "intensity standard deviation") (:VALUE . 1.0) (:UNIT-OF-MEASURE)
     (:PARAMETER-CLASS . "simulation") (:PARAMETER-SUB-CLASS . "environment")))
   (:RAW-DATA . "jsonFormattedRawData") (:GENERATE-RAW-DATA))
  ((:NAME . "ACT-R")
   (:PARAMETERS
    ((:NAME . "noise") (:VALUE . 0.25) (:UNIT-OF-MEASURE)
     (:PARAMETER-CLASS . "model") (:PARAMETER-SUB-CLASS . "architecture"))
    ((:NAME . "temperature") (:VALUE . 1.0) (:UNIT-OF-MEASURE)
     (:PARAMETER-CLASS . "model") (:PARAMETER-SUB-CLASS . "architecture"))
    ((:NAME . "similarity")
     (:VALUE (:INTEGER . "functionName") (:DOUBLE . "functionName"))
     (:UNIT-OF-MEASURE) (:PARAMETER-CLASS . "model")
     (:PARAMETER-SUB-CLASS . "knowledge")))
   (:RAW-DATA . "jsonFormattedRawData") (:GENERATE-RAW-DATA))))
|#
