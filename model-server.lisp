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

(vom:config t :info)

(define-constant +default-port+ 21952)

(defun %run-model (parameters raw-data generate-raw-data-p)
  (cond ((fboundp 'run-model)
         (run-model parameters raw-data generate-raw-data-p))
        (t (format t "~&parameters: ~:W~%raw-data: ~:W~%~:[do not ~;~]generate raw data~2%"
                   parameters raw-data generate-raw-data-p)
           "done")))

(defun tcp-handler (stream)
  (labels ((respond (obj)
             (encode-json obj stream)
             (terpri stream)
             (finish-output stream)
             (return)))
    (let ((line (read-line stream nil)))
      (unless line
        (vom:error "Empty line read from reasoner")
        (respond "Error: empty line read"))
      (let ((json (handler-case ()
                    (error (e)
                      (vom:error "can't correctly parse JSON: ~S" e)
                      (respond (format nil "Error parsing or manipulating JSON: ~S, ~S" e line))))


  (if-let ((line ))


  (format t "~S~%" )
  (format stream "done~%")
  (finish-output stream))

(defun run (&optional (port +default-port+))
  (vom:info "Starting SCALE model listener on port ~D" port)
  (handler-case (socket-server nil port 'tcp-handler)
    (error (e)
      (vom:error "~S" e)
      #+SBCL (sb-debug:print-backtrace)
      (uiop:quit 1))
    #+SBCL
    (sb-sys:interactive-interrupt ()
      (vom:info "Stopping SCALE model listener")
      (uiop:quit 0 t))))



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
