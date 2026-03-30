#!/bin/bash
exec sbcl --load quicklisp/setup.lisp --load act-up-v1_3_3 --load 'ACT-UP procedural module.lisp' --load 'evacuation-model-v2.0' --load model-server --eval "(scale:run)"
