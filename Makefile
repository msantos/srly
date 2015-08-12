REBAR=$(shell which rebar3 || echo ./rebar3)
DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt

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

$(DEPSOLVER_PLT):
	@dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
		--apps erts kernel stdlib crypto

dialyzer: $(DEPSOLVER_PLT)
	@dialyzer $(DIALYZER_FLAGS) --plt $(DEPSOLVER_PLT) -Wrace_conditions --src src examples/*

typer: $(DEPSOLVER_PLT)
	@typer -I include --plt $(DEPSOLVER_PLT) -r ./src

distclean: clean
	@rm $(DEPSOLVER_PLT)
