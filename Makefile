
REBAR=$(shell which rebar || echo ./rebar)
TEMPLATE=rebar.config.tmpl
CONFIG=rebar.config

CFLAGS=$(shell pkg-config --cflags libnotify)
LDFLAGS=$(shell pkg-config --libs libnotify)

all: config compile

config:
	@sed -e 's:@LDFLAGS@:$(LDFLAGS):' -e 's:@CFLAGS@:$(CFLAGS):' $(TEMPLATE) > $(CONFIG)

compile:
	$(REBAR) compile

clean:  
	@$(REBAR) clean


