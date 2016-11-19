ERL  := $(wildcard *.erl)
BEAM := $(patsubst %.erl,%.beam,$(ERL))

all:	$(BEAM)

%.beam: %.erl
	erlc $<

run:	all
	erl -noshell -s progress start -s init stop

view:	all
	erl -noshell -s view start -s init stop
