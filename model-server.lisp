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
  (vom:debug "Calling model on ~S ~S ~S" parameters raw-data generate-raw-data-p)
  (let ((result (cond ((fboundp 'run-model)
                       (funcall (symbol-function 'run-model)
                                parameters raw-data generate-raw-data-p))
                      (t (format t "~&parameters: ~:W~%raw-data: ~:W~%~:[do not ~;~]generate raw data~2%"
                                 parameters raw-data generate-raw-data-p)
                         "done"))))
    (vom:debug "Model returned ~S" result)
    result))

(defun restructure-json (model-data)
  (values (iter (for p :in (cdr (assoc :parameters model-data)))
                (for n := (cdr (assoc :name p)))
                (assert (stringp n))
                (collect (cons (make-keyword (substitute #\- #\Space (string-upcase n)))
                               (iter (for (k . v) :in p)
                                     (assert (keywordp k))
                                     (unless (eq k :name)
                                       (nconcing (list k v)))))))
          (cdr (assoc :raw-data model-data))
          (cdr (assoc :generate-raw-data model-data))))

(defun process-line (line)
  (let ((json (decode-json-from-string line)))
    (vom:debug "Processing JSON ~S" json)
    (assert (listp json))
    (setf json (cdr (assoc :models json)))
    (assert (listp json))
    (vom:debug "Processing models ~S" json)
    (iter (for m :in json)
          (collect (and (string-equal (cdr (assoc :name m)) "ACT-R")
                        (multiple-value-bind (params raw-data generate-raw-data-p)
                            (restructure-json m)
                          (let ((result (encode-json-to-string
                                         (%run-model params raw-data generate-raw-data-p))))
                            (vom:debug "Returning ~S" result)
                            result)))))))

(defun tcp-handler (stream)
  (iter (for line := (read-line stream nil '#0=#:eof))
        (vom:debug "Read line ~S" line)
        (until (eq line '#0#))
        (format stream "~A~%" (handler-case (process-line line)
                                ((and error #+SBCL (not sb-sys:interactive-interrupt)) (e)
                                  (vom:error "~A (while processing ~S)" e line)
                                  (format stream "Error: ~A~%"  e)
                                  (finish-output stream)
                                  (next-iteration))))
        (finish-output stream)))

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
