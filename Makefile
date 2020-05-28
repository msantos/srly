.PHONY: all compile deps test dialyzer typer clean distclean

REBAR ?= rebar3

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

deps:
	@$(REBAR) get-deps

test: compile
	@$(REBAR) ct

dialyzer:
	@$(REBAR) dialyzer

typer:
	@typer -I include --plt _build/default/*_plt -r ./src
