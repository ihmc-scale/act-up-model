#! /bin/sh
# Copyright 2025 Carnegie Mellon University

sbcl --dynamic-space-size 20000 --eval "(in-package :cl-user)" --load act-up-v1_3_2 --load 'evacuation-model-v0.3.3.lisp' --load model-server --eval "(scale:run nil $1)"
