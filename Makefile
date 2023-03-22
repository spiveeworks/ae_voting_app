PROJECT = cowboytest

DEPS = cowboy
dep_cowboy_commit = 2.6.3
DEP_PLUGINS = cowboy

PROJECT_DESCRIPTION = Testing how to get cowboy set up in an Erlang project.
PROJECT_VERSION = 0.1.0

BUILD_DEPS += relx
include erlang.mk
