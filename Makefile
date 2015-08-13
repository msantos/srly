REBAR ?= rebar3

all: compile

compile: $(REBAR)
	@$(REBAR) compile

clean: $(REBAR)
	@$(REBAR) clean

deps: $(REBAR)
	@$(REBAR) get-deps

test: $(REBAR) compile
	@$(REBAR) eunit

.PHONY: test dialyzer typer clean distclean

dialyzer: $(DEPSOLVER_PLT)
	@$(REBAR) dialyzer

typer: $(DEPSOLVER_PLT)
	@typer -I include --plt _build/default/*_plt -r ./src

distclean: clean
	@rm $(DEPSOLVER_PLT)
