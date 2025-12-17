#!/usr/bin/env bash
# shellcheck disable=SC2002
ncat 127.0.0.1 21952 < input-sample.json > output.json
