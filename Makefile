all:
	erlc *.erl

run:	all
	erl -noshell -s progress start -s init stop

view:	all
	erl -noshell -s view start -s init stop
