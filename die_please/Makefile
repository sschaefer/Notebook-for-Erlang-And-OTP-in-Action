# An erlang Makefile

APPLICATION=die_please

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

doc/%.html : src/%.erl $(HEADERS) doc/overview.edoc Makefile
	erl -noshell -eval 'edoc:application($(APPLICATION),".", [{private, true}])' -s init stop

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

die_please:
	erl -noshell -pa ebin \
		-eval 'application:start(sasl)' \
		-eval 'die_please:start_link()' \
		-eval 'timer:sleep(5000)' \
		-s init stop

die_please2:
	erl -noshell -pa ebin \
		-eval 'application:start(sasl)' \
		-eval 'proc_lib:spawn(fun die_please2:go/0)' \
		-eval 'timer:sleep(5000)' \
		-s init stop

webtool:
	erl -noshell -pa ebin \
		-eval 'webtool:start()' \
		-eval 'application:start($(APPLICATION))' &

toolbar:
	erl -noshell -pa ebin \
		-eval 'toolbar:start()' \
		-eval 'application:start($(APPLICATION))' &

appmon:
	erl -noshell -pa ebin \
		-eval 'appmon:start()' \
		-eval 'application:start($(APPLICATION))' &

pman:
	erl -noshell -pa ebin \
		-eval 'application:start($(APPLICATION))' \
		-eval 'pman:start()' &

debug: clean
	$(MAKE) ERLC_FLAGS="+debug_info $(ERLC_FLAGS)"
	erl -noshell -pa ebin \
		-eval 'debugger:start()' \
		-eval 'application:start($(APPLICATION))' &

tv:
	erl -noshell -pa ebin \
		-eval 'tv:start()' \
		-eval 'application:start($(APPLICATION))' &
