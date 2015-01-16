-module(serctl_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("srly/include/serctl.hrl").

constant_test() ->
    [ V = serctl:constant(K) || {K,V} <- serctl:constant() ].
