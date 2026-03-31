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

(defun unsymbolify (symbol)
  (substitute #\_ #\- (string-downcase symbol)))

(defparameter *intern-items* '("yes" "no" "evac" "stay"))

(defun listify-jzon (jzon)
  (cond ((member jzon *intern-items* :test #'equalp) (symbolify jzon nil))
        ((stringp jzon) jzon)
        ((vectorp jzon) (map 'list #'listify-jzon jzon))
        ((hash-table-p jzon) (iter (for (k v) :in-hash-table jzon)
                                   (collect (list (symbolify k) (listify-jzon v)))))
        ((floatp jzon) (coerce jzon *read-default-float-format*))
        (t jzon)))

(defun jzonify (thing)
  (cond ((symbolp thing) (unsymbolify thing))
        ((listp thing)
         (if (every (lambda (x) (and (consp x)
                                     (symbolp
                                      (first x))
                                     (rest x)
                                     (null (cddr x))))
                    thing)
             (iter (with result := (make-hash-table :test 'equal))
                   (for (k v) :in thing)
                   (setf (gethash (unsymbolify k) result) (jzonify v))
                   (finally (return result)))
             (map 'vector #'jzonify thing)))
        (t thing)))

(defun read-json (string)
  (let* ((result (listify-jzon (jzon:parse string)))
         (processes (assoc :processes result)))
    (when processes
      (setf (second processes) (mapcar (rcurry #'symbolify nil) (second processes))))
    result))

(defun handle-line (line)
  (let ((json (read-json line)))
    (v:debug "Processing JSON ~S" json)
    (assert (listp json))
    (let ((result (run-model json)))
      (v:debug "run-model returned ~S" result)
      (jzon:stringify (jzonify result)))))

(defun tcp-handler (stream)
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
  #+SBCL (sb-ext:gc :full t)
  (v:info "Finished from ~A" *remote-host*))

(defun run (&optional (interactive (member :swank *features*)) (port +default-port+))
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
