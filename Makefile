
REBAR=$(shell which rebar || echo ./rebar)
TEMPLATE=rebar.config.tmpl
CONFIG=rebar.config

CFLAGS=$(shell pkg-config --cflags libnotify)
LDFLAGS=$(shell pkg-config --libs libnotify)

all: config compile

./rebar:
	erl -noshell -s inets start \
		-eval 'httpc:request(get, {"http://hg.basho.com/rebar/downloads/rebar", []}, [], [{stream, "./rebar"}])' \
		-s init stop
	chmod +x ./rebar

config:
	@sed -e 's:@LDFLAGS@:$(LDFLAGS):' -e 's:@CFLAGS@:$(CFLAGS):' $(TEMPLATE) > $(CONFIG)

compile: $(REBAR)
	$(REBAR) compile

clean: $(REBAR)
	@$(REBAR) clean


