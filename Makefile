SHELL_OPTS = -s ${PROJECT} 

PROJECT = taser

DEPS = gun

TEST_DEPS = jsx

LOCAL_DEPS = inets edoc

include erlang.mk
