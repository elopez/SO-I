#!/bin/sh
WORKER_PIDS=""

for PORT in 8100 8200 8300 8400 8500; do
	./worker $PORT 127.0.0.1:8100 127.0.0.1:8200 127.0.0.1:8300 127.0.0.1:8400 127.0.0.1:8500 &
	WORKER_PIDS="$! $WORKER_PIDS"
done
sleep 1

export WORKER_PIDS
trap "kill -- $WORKER_PIDS" INT TERM EXIT

./dispatcher 127.0.0.1:8100 127.0.0.1:8200 127.0.0.1:8300 127.0.0.1:8400 127.0.0.1:8500
