.PNONY: all clean run

all: dispatcher.beam workers.beam lockserv.beam fdserv.beam

%.beam: %.erl
	erlc $<

clean:
	rm *.beam erl_crash.dump

run: all
	./run.sh dispatcher.erl
