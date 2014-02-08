REBAR=$(shell which rebar || echo ./rebar)
DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt

all: compile

./rebar:
	erl -noshell -s inets start -s ssl start \
		-eval 'httpc:request(get, {"https://raw.github.com/wiki/rebar/rebar/rebar", []}, [], [{stream, "./rebar"}])' \
		-s inets stop -s init stop
	chmod +x ./rebar

compile: $(REBAR)
	@$(REBAR) compile

clean: $(REBAR)
	@$(REBAR) clean

deps: $(REBAR)
	@$(REBAR) get-deps

.PHONY: test dialyzer typer clean distclean

$(DEPSOLVER_PLT):
	@dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
		--apps erts kernel stdlib crypto

dialyzer: $(DEPSOLVER_PLT)
	@dialyzer --plt $(DEPSOLVER_PLT) -Wrace_conditions --src src examples/*

typer: $(DEPSOLVER_PLT)
	@typer -I include --plt $(DEPSOLVER_PLT) -r ./src

distclean: clean
	@rm $(DEPSOLVER_PLT)
