;;;; Copyright 2025 Carnegie Mellon University

(ql:quickload '(:cl-interpol :alexandria :iterate :cl-json
                :bordeaux-threads :usocket :uiop :vom)
              :silent t)

(asdf:load-system :usocket-server)

(defpackage :scale-act-up-interface
  (:nicknames :scale)
  (:use :common-lisp :alexandria :iterate :json :usocket)
  (:export #:run #:*data-function-name-package*))

(in-package :scale)

(vom:config t :info)

#+SBCL (setf (sb-ext:gc-logfile) "gc.log")

(define-constant +default-port+ 21952)

(define-constant +default-behavior-name+ "evacuate/stay" :test #'string-equal)

(defparameter *data-function-name-package* 'CL-USER)

(defun %run-model (parameters raw-data)
  (vom:debug "Calling model on ~S ~S" parameters raw-data)
  (multiple-value-bind (result name)
      (cl-user::run-model parameters raw-data)
    (vom:debug "Model returned (~A) ~S" name result)
    ;; canonicalize result into a list of lists
    (when (arrayp result)
      (setf result
            (case (array-rank result)
              (1 (coerce result 'list))
              (2 (iter (for i :from 0 :below (array-dimension result 0))
                       (collect
                           (iter (for j :from 0 :below (array-dimension result 1))
                                 (for x := (aref result i j))
                                 (when x
                                   (collect x))))))
              (t (error "Don't know how to process a ~D dimensional array for return to reasoner"
                        (array-rank result))))))
    (values (mapcar (lambda (x) (coerce x 'list)) result) name)))

(defun restructure-input (model-data)
  (values (iter (for p :in (cdr (assoc :parameters model-data)))
                (for n := (cdr (assoc :name p)))
                (assert (stringp n))
                (setf n (make-keyword (substitute #\- #\Space (string-upcase n))))
                (collect (cons n (iter (for (k . v) :in p)
                                       (assert (keywordp k))
                                       (when (and (eq k :value)
                                                  (member n '(:utility :similarity)))
                                         (setf v (iter (for (typ . fn) :in v)
                                                       (nconcing (list typ (intern (string-upcase fn)
                                                                                   *data-function-name-package*))))))
                                       (unless (eq k :name)
                                         (nconcing (list k v)))))))
          (cdr (assoc :raw-data model-data))))

(defun lower-underscore (s)
  (if (typep s 'string-designator)
      (substitute-if #\_ (lambda (c) (member c '(#\Space #\-))) (string-downcase s))
      s))

(defun restructure-output (data)
  (iter (for outer :in data)
        (collect (iter (for inner :in outer)
                       (collect (iter (for (n . rest) :in inner)
                                      (collect `((:name . ,(lower-underscore n))
                                                 ,@(iter (for (k v) :on rest :by #'cddr)
                                                         (collect (cons k (lower-underscore v))))))))))))

(defun process-line (line)
  (let ((json (decode-json-from-string line)))
    (vom:debug "Processing JSON ~S" json)
    (assert (listp json))
    (setf json (cdr (assoc :models json)))
    (assert (listp json))
    (vom:debug "Processing models ~S" json)
    (iter (for m :in json)
          (for model-name := (make-keyword (string-upcase (cdr (assoc :name m)))))
          (for (values runs behavior-name) := (multiple-value-bind (params raw-data)
                                                  (restructure-input m)
                                                (%run-model `((:name ,model-name) ,@params) raw-data)))
          (collect (if runs
                       `((:name . ,model-name)
                         (:behavior . #(((:name . ,(or behavior-name +default-behavior-name+))
                                         (:runs ,@(restructure-output runs))))))
                       (make-hash-table))
            :into results)
          (finally (let ((result `((:models . #(,@results)))))
                     (vom:debug "Encoding ~S" result)
                     (let ((encoded-result (encode-json-to-string result)))
                       (vom:debug "Returning ~S" encoded-result)
                       (return encoded-result)))))))

(defun tcp-handler (stream)
  (vom:info "Connected from ~A" *remote-host*)
  (iter (for line := (read-line stream nil '#0=#:eof))
        (vom:debug "Read line ~S" line)
        (until (eq line '#0#))
        (format stream "~A~%" (handler-case (process-line line)
                                ((and error #+SBCL (not sb-sys:interactive-interrupt)) (e)
                                  (vom:error "~A (while processing ~S)" e line)
                                  (format stream "Error: ~A~%"  e)
                                  (finish-output stream)
                                  (next-iteration))))
        (finish-output stream))
  #+SBCL (sb-ext:gc :full t)
  (vom:info "Finished from ~A" *remote-host*))

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
