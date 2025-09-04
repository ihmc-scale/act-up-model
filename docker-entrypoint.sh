#!/bin/bash
exec sbcl --load quicklisp/setup.lisp --load act-up-v1_3_2 --load 'evacuation-model-v1.0.2.lisp' --load model-server.lisp --eval '(scale:run)'
