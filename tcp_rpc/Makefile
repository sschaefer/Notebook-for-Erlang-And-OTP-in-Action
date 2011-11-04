# An erlang Makefile

empty:=
space:=$(empty) $(empty)
dnotest:=$(space)-DNOTEST

ERLC_FLAGS=-I /usr/lib64/erlang/lib

ifeq ($(ERLC_FLAGS),$(subst $(dnotest),$(empty),$(ERLC_FLAGS)))
# -DNOTEST did not occur in ERLC_FLAGS, so include test code
SOURCES=$(wildcard src/*.erl)
else
# -DNOTEST did occur in ERLC_FLAGS, so exclude test code
test_sources=$(wildcard src/*_test.erl)
all_sources=$(wildcard src/*.erl)
SOURCES=$(filter-out $(test_sources),$(all_sources))
endif

HEADERS=$(wildcard src/*.hrl)
OBJECTS=$(SOURCES:src/%.erl=ebin/%.beam)
DOCS=$(SOURCES:src/%.erl=doc/%.html)

ebin/%.beam : src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<

doc/%.html : src/%.erl $(HEADERS) Makefile
	erl -noshell -eval 'edoc:application(tcp_rpc,".", [])' -s init stop

ifeq ($(ERLC_FLAGS),$(subst $(dnotest),$(empty),$(ERLC_FLAGS)))
# -DNOTEST did not occur in ERLC_FLAGS: test
all: $(OBJECTS) test
else
all: $(OBJECTS)
endif

clean:
	-rm $(OBJECTS)

test:
	erl -noshell -pa ebin \
		-eval 'eunit:test("ebin",[verbose])' \
		-s init stop

release: clean
	$(MAKE) ERLC_FLAGS="$(ERLC_FLAGS) -DNOTEST"

doc: $(DOCS)
