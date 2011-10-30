# A simple Makefile
ERLC_FLAGS=-I /usr/lib64/erlang/lib
SOURCES=$(wildcard src/*.erl)
HEADERS=$(wildcard src/*.hrl)
OBJECTS=$(SOURCES:src/%.erl=ebin/%.beam)
DOCS=$(SOURCES:src/%.erl=doc/%.html)
all: $(OBJECTS) test
ebin/%.beam : src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<

doc/%.html : src/%.erl $(HEADERS) Makefile
	erl -noshell -eval 'edoc:application(tcp_rpc,".", [])' -s init stop

clean:
	-rm $(OBJECTS)

test:
	erl -noshell -pa ebin \
		-eval 'eunit:test("ebin",[verbose])' \
		-s init stop

release: clean
	$(MAKE) ERLC_FLAGS="$(ERLC_FLAGS) -DNOTEST"

doc: $(DOCS)
