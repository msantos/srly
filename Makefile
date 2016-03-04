REBAR ?= rebar3

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

deps:
	@$(REBAR) get-deps

test: compile
	@$(REBAR) eunit

.PHONY: test dialyzer typer clean distclean

dialyzer: $(DEPSOLVER_PLT)
	@$(REBAR) dialyzer

typer: $(DEPSOLVER_PLT)
	@typer -I include --plt _build/default/*_plt -r ./src

distclean: clean
	@rm $(DEPSOLVER_PLT)
