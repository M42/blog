#!/bin/bash
# Author: Mario Román (@m42)
# Original by Ignacio Cordón (@ncordon)

INPUT="/tmp/input.org"
OUTPUT="/tmp/input.tex"

(while IFS= read -r line; do printf '%s\n' "$line"; done) > $INPUT

emacs $INPUT --batch -u `id -un` --eval '(load user-init-file)' -f org-beamer-export-to-pdf

cat $OUTPUT
