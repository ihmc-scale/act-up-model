;;;; Copyright 2025 Carnegie Mellon University

(ql:quickload '(:cl-interpol :alexandria :iterate :cl-ppcre
                :com.inuoe.jzon :usocket-server :unix-opts :vom))

(defpackage :scale-act-up-interface
  (:nicknames :scale)
  (:use common-lisp :alexandria :iterate :ppcre :usocket)
  (:export #:run))

(in-package :scale)

(vom:config t :info)
