#!/bin/bash
exec sbcl --load quicklisp/setup.lisp --load model-server.lisp --eval '(scale:run)'
