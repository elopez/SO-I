.PHONY: all pthreads erlang test test-pthreads test-erlang

# Build all targets by default
all: pthreads erlang

pthreads:
	make -C server/pthreads

erlang:
	make -C server/erlang

# pthreads testing
test-pthreads: pthreads
	cd server/pthreads && ./runworkers.sh &
	sleep 3
	make -C tests
	killall -9 dispatcher

# erlang testing
test-erlang: erlang
	cd server/erlang && ./run.sh dispatcher.erl &
	sleep 3
	make -C tests
	killall -9 beam.smp

# Test both
test: test-pthreads test-erlang

# Clean the tree
clean:
	make -C server/pthreads clean
	make -C server/erlang clean
