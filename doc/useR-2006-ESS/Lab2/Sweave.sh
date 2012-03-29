#!/bin/sh

echo "library(tools); Sweave(\"$1\")" | R --no-save --no-restore