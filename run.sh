#! /bin/sh
# Copyright 2025 Carnegie Mellon University

sbcl --load model-server --eval "(scale:run nil $1)"
