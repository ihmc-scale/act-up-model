;;;; Copyright 2025-2026 Carnegie Mellon University

(ql:quickload '(:cl-interpol :alexandria :iterate :com.inuoe.jzon
                :bordeaux-threads :usocket :uiop :vom)
              :silent t)

(asdf:load-system :usocket-server)

(defpackage :scale-act-up-interface
  (:nicknames :scale)
  (:use :common-lisp :alexandria :iterate :usocket)
  (:local-nicknames (:jzon :com.inuoe.jzon) (:v :vom))
  (:import-from common-lisp-user #:run-model #:yes #:no)
  (:export #:run))

(in-package :scale)

(v:config t :info)

(define-constant +default-port+ 21952)
(define-constant +model-package+ (find-package 'common-lisp-user))

(defun symbolify (string &optional (keyword t))
  (intern (substitute-if #\- (rcurry #'find "_ ") (string-upcase string))
          (if keyword 'keyword +model-package+)))

(defparameter *intern-items* '("yes" "no" "evac" "stay"))

(defun listify-jzon (jzon)
  (cond ((member jzon *intern-items* :test #'equalp) (symbolify jzon nil))
        ((stringp jzon) jzon)
        ((vectorp jzon) (map 'list #'listify-jzon jzon))
        ((hash-table-p jzon) (iter (for (k v) :in-hash-table jzon)
                                   (collect (list (symbolify k) (listify-jzon v)))))
        ((floatp jzon) (coerce jzon *read-default-float-format*))
        (t jzon)))

<<<<<<< HEAD
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
                                                                                   *data-name-package*))))))
                                       (unless (eq k :name)
                                         (nconcing (list k v)))))))
          (cdr (assoc :raw-data model-data))))

(defun lower-underscore (s)
  (if (typep s 'string-designator)
      (substitute-if #\_ (lambda (c) (member c '(#\Space #\-))) (string-downcase s))
      s))

(defun model-name-to-lisp (s)
  (assert (typep s 'string-designator))
  (intern (string-upcase (substitute #\- #\Space (remove #\* (remove \+ (camel-case-to-lisp s)))))
          *data-name-package*))

(defun restructure-output (data)
  (iter (for outer :in data)
        (collect (iter (for inner :in outer)
                       (collect (iter (for (n . rest) :in inner)
                                      (collect `((:name . ,(lower-underscore n))
                                                 ,@(iter (for (k v) :on rest :by #'cddr)
                                                         (collect (cons k (lower-underscore v))))))))))))

(defun read-process-files (list)
  (iter (for fname :in list)
        (unless (find #\. fname)
          (setf fname (format nil "~a.lisp" fname)))
        (collect
         (with-open-file (s (merge-pathnames fname +process-directory+))
           (iter (with *package* := *data-name-package*)
                 (for form := (read s nil :eof))
                 (until (eq form :eof))
                 (collect form))))))

=======
(defun read-json (string)
  (let* ((result (listify-jzon (jzon:parse string)))
         (processes (assoc :processes result)))
    (when processes
      (setf (second processes) (mapcar (rcurry #'symbolify nil) (second processes))))
    result))
>>>>>>> 28d73dc (latest ACT-Up stuff and model from Christian; simplify model-server code to match)

(defun handle-line (line)
  (let ((json (read-json line)))
    (vom:debug "Processing JSON ~S" json)
    (assert (listp json))
    "return value"))

(defun tcp-handler (stream)
<<<<<<< HEAD
  (vom:info "Connected from ~A" *remote-host*)
  (iter (for line := (read-line stream nil :eof))
        (vom:debug "Read line ~S" line)
        (until (eq line :eof))
        (format stream "~A~%"
                (handler-case
                    (handle-line line)
                  ((and error #+SBCL (not sb-sys:interactive-interrupt)) (e)
                    (vom:error "~A (while processing ~S)" e line)
                    (format stream "Error: ~A~%" e)
                    (finish-output stream)
                    (next-iteration)))))
  (finish-output stream)
=======
  (v:info "Connected from ~A" *remote-host*)
  (iter (for line := (read-line stream nil '#0=#:eof))
        (v:debug "Read line ~S" line)
        (until (eq line '#0#))
        (format stream "~A~%" (handler-case (handle-line line)
                                ((and error #+SBCL (not sb-sys:interactive-interrupt)) (e)
                                  (v:error "~A (while processing ~S)" e line)
                                  (format stream "Error: ~A~%"  e)
                                  (finish-output stream)
                                  (next-iteration))))
        (finish-output stream))
>>>>>>> 28d73dc (latest ACT-Up stuff and model from Christian; simplify model-server code to match)
  #+SBCL (sb-ext:gc :full t)
  (v:info "Finished from ~A" *remote-host*))


(defun run (&optional(interactive (member :swank *features*)) (port +default-port+))
  (v:info "Starting SCALE model listener on port ~D" port)
  (labels ((quit (n)
             (unless interactive
               (uiop:quit n))))
    (handler-case (socket-server nil port 'tcp-handler)
      (error (e)
        (v:error "top level error ~S" e)
        #+SBCL (sb-debug:print-backtrace)
        (quit 1))
      #+SBCL
      (sb-sys:interactive-interrupt ()
        (v:info "Stopping SCALE model listener")
        (quit 0)))))



#+nil
(progn
  (swank:set-default-directory "/Users/dfm/work/scale/act-up-model")
  (swank:set-package :cl-user)
  (in-package :cl-user)
  (mapc #'load '("act-up-v1_3_3" "ACT-UP procedural module" "evacuation-model-v2.0.lisp" "model-server"))
  (swank:set-package :scale))
