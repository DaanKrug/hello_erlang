APP_NAME = hello_erlang

D_0 = application:start(eunit)
D_1 = application:start(xmerl)
D_2 = application:start(jiffy)
D_3 = application:start(crypto)
D_4 = application:start(cowlib)
D_5 = application:start(asn1)
D_6 = application:start(public_key)
D_7 = application:start(ssl)
D_8 = application:start(ranch)
D_9 = application:start(cowboy)
D_10 = application:start(bcrypt)

APP = application:start(hello_erlang)

all: compile

compile:
	./rebar3 compile

clean:
	./rebar3 clean

run: compile
	erl -pa _build/default/lib/*/ebin -eval "$(D_0),$(D_1),$(D_2),$(D_3),$(D_4),$(D_5),$(D_6),$(D_7),$(D_8),$(D_9),$(D_10),$(APP)."

test: compile
	./rebar3 eunit