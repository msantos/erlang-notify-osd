.PHONY: dialyzer typer clean distclean

REBAR ?= rebar3

all:
	$(REBAR) compile

clean:
	$(REBAR) clean

dialyzer:
	$(REBAR) dialyzer
