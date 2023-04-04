all: cowlib ranch cowboy zj objects

testdeps:
	@if [ ! -d deps/cowlib ] || [ ! -d deps/ranch ] || [ ! -d deps/cowboy ] || [ ! -d deps/zj ]; \
	then \
	  echo 'You must pull git dependencies with `make pulldeps` before you' \
	    'can use `make all` or `make run`.'; \
	  exit 1; \
	fi

pulldeps: deps/cowlib deps/ranch deps/cowboy deps/zj

deps/cowlib:
	git clone https://github.com/ninenines/cowlib deps/cowlib
deps/ranch:
	git clone https://github.com/ninenines/ranch deps/ranch
deps/cowboy:
	git clone https://github.com/ninenines/cowboy deps/cowboy
deps/zj:
	git clone https://gitlab.com/zxq9/zj deps/zj

vpath %.erl deps/cowlib/src:deps/ranch/src:deps/cowboy/src:deps/zj/src:src

ebin/%.beam: %.erl | ebin
	erlc -I deps -I deps/cowlib/include -o ebin -pa ebin -Werror $<

behaviours: testdeps ebin/ranch_transport.beam ebin/ranch_protocol.beam ebin/cowboy_stream.beam ebin/cowboy_middleware.beam ebin/cowboy_sub_protocol.beam ebin/cowboy_handler.beam
cowlib: testdeps $(patsubst deps/cowlib/src/%.erl,ebin/%.beam,$(wildcard deps/cowlib/src/*.erl))
ranch: behaviours $(patsubst deps/ranch/src/%.erl,ebin/%.beam,$(wildcard deps/ranch/src/*.erl))
cowboy: behaviours $(patsubst deps/cowboy/src/%.erl,ebin/%.beam,$(wildcard deps/cowboy/src/*.erl))
zj: testdeps $(patsubst deps/zj/src/%.erl,ebin/%.beam,$(wildcard deps/zj/src/*.erl))
objects: behaviours $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))

run: all
	erl -pa ebin -name cowboytest@localhost -eval "lists:foreach(fun(X)->application:start(X)end, [crypto, asn1, public_key, ssl, cowlib, ranch, cowboy, zj, cowboytest])."

.PHONY: all behaviours cowlib ranch cowboy objects run distclean clean
clean:
	rm -rf ebin/*.beam
distclean:
	rm -rf ebin/*.beam
	rm -rf deps
