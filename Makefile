SHELL_OPTS = -s ${PROJECT} 

PROJECT = taser

DEPS = lager gun

LOCAL_DEPS = inets

include erlang.mk

ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}'
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)
