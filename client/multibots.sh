#!/bin/bash

for i in `seq 1 $1`; do
	./clientbot 127.0.0.1 20 $((i * 123)) &
done
