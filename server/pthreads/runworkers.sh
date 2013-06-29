#!/bin/sh
for PORT in 8100 8200 8300 8400 8500; do
	./worker $PORT 127.0.0.1:8100 127.0.0.1:8200 127.0.0.1:8300 127.0.0.1:8400 127.0.0.1:8500 &
done
sleep 1
./dispatcher 127.0.0.1:8100 127.0.0.1:8200 127.0.0.1:8300 127.0.0.1:8400 127.0.0.1:8500
killall worker valgrind dispatcher
