# Simple deps are ones whose source directory is in deps/<name>/src
# ...the rest were made by Craig :-)
SIMPLE_DEPS := cowlib ranch cowboy zj aebytecode base58 enacl aeserialization eblake2 aesophia
DEPS := $(SIMPLE_DEPS) zx vanillae
DEPDIRS := $(patsubst %,deps/%,$(DEPS))

AEB_GENERATED_SRC = deps/aebytecode/src/aeb_fate_opcodes.erl deps/aebytecode/src/aeb_fate_ops.erl deps/aebytecode/include/aeb_fate_opcodes.hrl deps/aebytecode/src/aeb_fate_asm_scan.xrl deps/aebytecode/src/aeb_fate_pp.erl

all: $(DEPS) objects

# It's hard to make a Makefile *not* do something when a file is missing, or
# respond two different ways based on what goal it is working towards, so we
# just fall back to manually checking in the shell, and exiting out from there.

testdeps:
	@for i in $(DEPDIRS); \
	do \
	  if [ ! -d $$i ]; \
	  then \
	    echo 'You must pull git dependencies with `make pulldeps` before you' \
	      'can use `make all` or `make run`.'; \
	    exit 1; \
	  fi \
	done


# Clone all of the git repositories once, and trust them from then on. Once the
# directory exists, you are free to patch or modify stuff for testing, and this
# makefile will just trust you.

pulldeps: $(DEPDIRS)

git-clone = git -c "advice.detachedHead=false" clone

deps/cowlib:
	$(git-clone) https://github.com/ninenines/cowlib deps/cowlib --branch 2.12.1
deps/ranch:
	$(git-clone) https://github.com/ninenines/ranch deps/ranch --branch 2.1.0
deps/cowboy:
	$(git-clone) https://github.com/ninenines/cowboy deps/cowboy --branch 2.9.0
deps/zj:
	$(git-clone) https://gitlab.com/zxq9/zj deps/zj --branch 1.1.2
deps/aebytecode:
	$(git-clone) https://github.com/aeternity/aebytecode.git deps/aebytecode --branch v3.2.0
deps/base58:
	$(git-clone) https://github.com/aeternity/erl-base58 deps/base58
deps/enacl:
	$(git-clone) https://github.com/aeternity/enacl.git deps/enacl
deps/aeserialization:
	$(git-clone) https://github.com/aeternity/aeserialization.git deps/aeserialization --branch v1.0.0
deps/eblake2:
	$(git-clone) https://github.com/aeternity/eblake2 deps/eblake2 --branch v1.0.0
deps/aesophia:
	$(git-clone) https://github.com/aeternity/aesophia.git deps/aesophia --branch v7.1.0
deps/zx:
	$(git-clone) https://gitlab.com/zxq9/zx/ deps/zx
# Using spiveeworks fork of Vanillae, which is made specifically to add the
# features this project needs. The aeternity fork should get those changes
# eventually as well, as pull requests get worked out.
deps/vanillae:
	$(git-clone) https://github.com/spiveeworks/Vanillae.git deps/vanillae


# Single rule for all beam files: find an erlang file with the same name, and
# compile it. We could split this into each dependency's ebin directory, and
# then copy them all over to some other location, or do some
# application/release bundling, but whatever.

vpath %.erl $(patsubst %,deps/%/src,$(SIMPLE_DEPS)) deps/zx/zomp/lib/otpr/zx/0.12.7/src deps/vanillae/bindings/erlang/src src

ebin/%.beam: %.erl | ebin
	@echo Compiling $<
	@export zx_include=. && erlc -I deps -I deps/cowlib/include -I deps/aebytecode/include -I deps/aeserialization/include -I deps/zx/zomp/lib/otpr/zx/0.12.7/include -I include -o ebin -pa ebin $<

# aebytecode has some code generation that we will invoke directly.
ebin/aeb_fate_asm.beam aeb_fate_code.beam: deps/aebytecode/include/aeb_fate_opcodes.hrl
$(AEB_GENERATED_SRC): ebin/aeb_fate_generate_ops.beam deps/aebytecode/src/aeb_fate_asm_scan.template
	@echo Generating code.
	@cd deps/aebytecode && erl -pa ../../ebin/ -noshell -s aeb_fate_generate_ops gen_and_halt src/ include/

priv:
	mkdir priv

priv/enacl_nif.so: deps/enacl/c_src/*.c | priv
	gcc -fPIC -shared -lsodium -I /usr/lib/erlang/erts-13.2/include deps/enacl/c_src/*.c -o priv/enacl_nif.so

# Applications. 'behaviours' is all of the files that need to be compiled
# first, to suppress "behaviour undefined" warnings when compiling ranch,
# cowboy, or user code. After that each target is just a patsubst to find all
# of the beam files that a given application can make. We also make sure that
# each of these calls into testdeps, so that we can display a more useful error
# message if the erlang files haven't been pulled from GitHub/GitLab yet, and
# manually add some dependencies to aebytecode, so that it can compile them
# even if their corresponding source code hasn't been generated yet.

# The combination of patsubst and vpath might seem redundant, but vpath lets us
# compile individual modules incrementally in a single rule, as long as we can
# work out what modules we even want. patsubst just lets us say... all of them!

behaviours: testdeps ebin/ranch_transport.beam ebin/ranch_protocol.beam ebin/cowboy_stream.beam ebin/cowboy_middleware.beam ebin/cowboy_sub_protocol.beam ebin/cowboy_handler.beam
cowlib: testdeps $(patsubst deps/cowlib/src/%.erl,ebin/%.beam,$(wildcard deps/cowlib/src/*.erl))
ranch: behaviours $(patsubst deps/ranch/src/%.erl,ebin/%.beam,$(wildcard deps/ranch/src/*.erl))
cowboy: behaviours $(patsubst deps/cowboy/src/%.erl,ebin/%.beam,$(wildcard deps/cowboy/src/*.erl))
zj: testdeps $(patsubst deps/zj/src/%.erl,ebin/%.beam,$(wildcard deps/zj/src/*.erl))
aebytecode: testdeps $(patsubst deps/aebytecode/src/%.erl,ebin/%.beam,$(wildcard deps/aebytecode/src/*.erl)) ebin/aeb_fate_opcodes.beam ebin/aeb_fate_ops.beam ebin/aeb_fate_pp.beam
base58: testdeps $(patsubst deps/base58/src/%.erl,ebin/%.beam,$(wildcard deps/base58/src/*.erl))
enacl: testdeps $(patsubst deps/enacl/src/%.erl,ebin/%.beam,$(wildcard deps/enacl/src/*.erl)) priv/enacl_nif.so
aeserialization: testdeps $(patsubst deps/aeserialization/src/%.erl,ebin/%.beam,$(wildcard deps/aeserialization/src/*.erl))
eblake2: testdeps $(patsubst deps/eblake2/src/%.erl,ebin/%.beam,$(wildcard deps/eblake2/src/*.erl))
aesophia: testdeps $(patsubst deps/aesophia/src/%.erl,ebin/%.beam,$(wildcard deps/aesophia/src/*.erl))
zx: testdeps ebin/zx_net.beam
vanillae: testdeps $(patsubst deps/vanillae/bindings/erlang/src/%.erl,ebin/%.beam,$(wildcard deps/vanillae/bindings/erlang/src/*.erl))
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
	@echo Running...
	@export ERL_LIBS=sophia_stdlib && erl -pa ebin -name ae_voting_app@localhost -run ae_voting_app

clean:
	rm -rf ebin/*.beam
	rm -f $(AEB_GENERATED_SRC)
	rm -r priv
distclean:
	rm -rf ebin/*.beam
	rm -rf deps

# More than twice as many phony targets as actual targets, lol.
.PHONY: all testdeps pulldeps behaviours $(DEPS) objects run distclean clean
