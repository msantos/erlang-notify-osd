REBAR=$(shell which rebar || echo ./rebar)
TEMPLATE=rebar.config.tmpl
CONFIG=rebar.config
DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt

CFLAGS=$(shell pkg-config --cflags libnotify)
LDFLAGS=$(shell pkg-config --libs libnotify)

all: config compile

./rebar:
	erl -noshell -s inets start -s ssl start \
		-eval 'httpc:request(get, {"https://raw.github.com/wiki/rebar/rebar/rebar", []}, [], [{stream, "./rebar"}])' \
		-s inets stop -s init stop
	chmod +x ./rebar

config:
	@sed -e 's:@LDFLAGS@:$(LDFLAGS):' -e 's:@CFLAGS@:$(CFLAGS):' $(TEMPLATE) > $(CONFIG)

compile: $(REBAR)
	$(REBAR) compile

clean: $(REBAR)
	@$(REBAR) clean

.PHONY: dialyzer typer clean distclean

$(DEPSOLVER_PLT):
	@dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
		--apps erts kernel stdlib crypto

dialyzer: $(DEPSOLVER_PLT)
	@dialyzer --plt $(DEPSOLVER_PLT) -Wrace_conditions --src src

typer: $(DEPSOLVER_PLT)
	@typer -I include --plt $(DEPSOLVER_PLT) -r ./src

distclean: clean
	@rm $(DEPSOLVER_PLT)
