all: cowlib ranch cowboy zj objects

# It's hard to make a Makefile *not* do something when a file is missing, or
# respond two different ways based on what goal it is working towards, so we
# just fall back to manually checking in the shell, and exiting out from there.

testdeps:
	@if [ ! -d deps/cowlib ] || [ ! -d deps/ranch ] || [ ! -d deps/cowboy ] || [ ! -d deps/zj ]; \
	then \
	  echo 'You must pull git dependencies with `make pulldeps` before you' \
	    'can use `make all` or `make run`.'; \
	  exit 1; \
	fi


# Clone all of the git repositories once, and trust them from then on. Once the
# directory exists, you are free to patch or modify stuff for testing, and this
# makefile will just trust you.

pulldeps: deps/cowlib deps/ranch deps/cowboy deps/zj

git-clone = git -c "advice.detachedHead=false" clone

deps/cowlib:
	$(git-clone) https://github.com/ninenines/cowlib deps/cowlib --branch 2.12.1
deps/ranch:
	$(git-clone) https://github.com/ninenines/ranch deps/ranch --branch 2.1.0
deps/cowboy:
	$(git-clone) https://github.com/ninenines/cowboy deps/cowboy --branch 2.9.0
deps/zj:
	$(git-clone) https://gitlab.com/zxq9/zj deps/zj --branch 1.1.0


# Single rule for all beam files: find an erlang file with the same name, and
# compile it. We could split this into each dependency's ebin directory, and
# then copy them all over to some other location, or do some
# application/release bundling, but whatever.

vpath %.erl deps/cowlib/src:deps/ranch/src:deps/cowboy/src:deps/zj/src:src

ebin/%.beam: %.erl | ebin
	erlc -I deps -I deps/cowlib/include -o ebin -pa ebin -Werror $<


# Applications. 'behaviours' is all of the files that need to be compiled
# first, to suppress "behaviour undefined" warnings when compiling ranch,
# cowboy, or user code. After that each target is just a patsubst to find all
# of the beam files that a given application can make. We also make sure that
# each of these calls into testdeps, so that we can display a more useful error
# message if the erlang files haven't been pulled from GitHub/GitLab yet.

# The combination of patsubst and vpath might seem redundant, but vpath lets us
# compile individual modules incrementally in a single rule, as long as we can
# work out what modules we even want. patsubst just lets us say... all of them!

behaviours: testdeps ebin/ranch_transport.beam ebin/ranch_protocol.beam ebin/cowboy_stream.beam ebin/cowboy_middleware.beam ebin/cowboy_sub_protocol.beam ebin/cowboy_handler.beam
cowlib: testdeps $(patsubst deps/cowlib/src/%.erl,ebin/%.beam,$(wildcard deps/cowlib/src/*.erl))
ranch: behaviours $(patsubst deps/ranch/src/%.erl,ebin/%.beam,$(wildcard deps/ranch/src/*.erl))
cowboy: behaviours $(patsubst deps/cowboy/src/%.erl,ebin/%.beam,$(wildcard deps/cowboy/src/*.erl))
zj: testdeps $(patsubst deps/zj/src/%.erl,ebin/%.beam,$(wildcard deps/zj/src/*.erl))
objects: behaviours $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))


# Other PHONY targets;
# `make run`         compiles everything, starts all of the applications
#                    one after another, and puts you in an erlang environment.
#                    i.e. it starts the website!
#
# `make clean`      deletes all .beam files, to quickly get you out of
#                   incremental compilation hell, or just to get rid of old
#                   files.
#
# `make distclean`  like `make clean`, but also deletes dependency repos
#                   entirely, so that you can/have to run `make pulldeps`
#                   again.

run: all
	erl -pa ebin -name ae_voting_app@localhost -eval "lists:foreach(fun(X)->application:start(X)end, [crypto, asn1, public_key, ssl, cowlib, ranch, cowboy, zj, ae_voting_app])."

clean:
	rm -rf ebin/*.beam
distclean:
	rm -rf ebin/*.beam
	rm -rf deps

# More than twice as many phony targets as actual targets, lol.
.PHONY: all testdeps pulldeps behaviours cowlib ranch cowboy objects run distclean clean
