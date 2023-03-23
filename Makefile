PROJECT = cowboytest

DEPS = cowboy zj
dep_cowboy_commit = 2.9.0
dep_zj = git https://gitlab.com/zxq9/zj.git 1.1.0
DEP_PLUGINS = cowboy

PROJECT_DESCRIPTION = Testing how to get cowboy set up in an Erlang project.
PROJECT_VERSION = 0.1.0

BUILD_DEPS += relx
include erlang.mk
