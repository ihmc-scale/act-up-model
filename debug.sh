#! /bin/sh
# Copyright 2025 Carnegie Mellon University

sbcl --load model-server --eval "(progn (vom:config t :debug) (scale:run nil $1))"
