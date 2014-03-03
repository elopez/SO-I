#!/bin/sh
FILE="$1"
NAME=${FILE%.erl}
exec erl -noshell -s "$NAME" main -s init stop
