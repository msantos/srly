-module(serctl_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("srly/include/serctl.hrl").

-record(state, {
        fd,
        termios
    }).

constant_test() ->
    [ V = serctl:constant(K) || {K,V} <- serctl:constant() ].

serctl_test_() ->
    case os:getenv("SRLY_TEST_PORT") of
        false ->
            [];
        _ ->
            {setup,
                fun start/0,
                fun stop/1,
                fun run/1}
    end.

start() ->
    Dev = os:getenv("SRLY_TEST_PORT"),
    {ok, FD} = serctl:open(Dev),
    {ok, Termios} = serctl:tcgetattr(FD),
    #state{
        fd = FD,
        termios = serctl:termios(Termios)
    }.

stop(#state{fd = FD}) ->
    serctl:close(FD).

run(State) ->
    [
        getfd(State),
        flow(State),
        mode(State),
        ispeed(State),
        ospeed(State),
        baud(State),
        termios(State)
    ].

getfd(#state{fd = FD}) ->
    N = serctl:getfd(FD),
    ?_assertEqual(true, is_integer(N)).

flow(#state{termios = Termios}) ->
    N = serctl:flow(Termios),
    ?_assertEqual(true, N == true orelse N == false).

mode(#state{}) ->
    N = serctl:mode(raw),
    ?_assertMatch(#termios{}, N).

ispeed(#state{termios = Termios}) ->
    N = serctl:ispeed(Termios),
    ?_assertEqual(true, is_integer(N)).

ospeed(#state{termios = Termios}) ->
    N = serctl:ospeed(Termios),
    ?_assertEqual(true, is_integer(N)).

baud(#state{}) ->
    N = serctl:baud(115200),
    case os:type() of
        {unix,linux} ->
            ?_assertMatch(4098, N);
        _ ->
            []
    end.

termios(#state{}) ->
    Mode = serctl:mode(raw),
    Bin = serctl:termios(Mode),
    ?_assertEqual(Mode, serctl:termios(Bin)).
