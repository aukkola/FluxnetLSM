#!/bin/bash

# Simple script to update FluxnetLSM documentation in ./man using devtools.

# Note: This is not needed for install, but is useful when updating the
# repository versions of the documentation.

echo "devtools::document('.')" | R --no-save
