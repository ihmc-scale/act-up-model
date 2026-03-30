#! /bin/sh
# Copyright 2025 Carnegie Mellon University

sbcl --dynamic-space-size 20000 --eval "(in-package :cl-user)" --load act-up-v1_3_3 --load 'ACT-UP procedural module.lisp' --load 'evacuation-model-v2.lisp' --load model-server --eval "(progn (vom:config t :debug) (scale:run nil $1))"
