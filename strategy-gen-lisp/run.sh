#!/bin/bash

# run.sh
# Script to load Lisp and execute the strategy generator

# Define the Lisp implementation (e.g., sbcl, ccl)
LISP=sbcl

# Path to Quicklisp setup.lisp (adjust if needed)
QUICKLISP_SETUP="$HOME/quicklisp/setup.lisp"

# Navigate to the directory containing this script
cd "$(dirname "$0")"

# Ensure Quicklisp is loaded and run the main program
# Using --eval to load Quicklisp setup and then load/run our system
$LISP --load "$QUICKLISP_SETUP" \
      --eval "(push (truename \".\") asdf:*central-registry*)" \
      --eval "(ql:quickload :strategy-gen-lisp)" \
      --eval "(strategy-gen-lisp:main)" \
      --quit