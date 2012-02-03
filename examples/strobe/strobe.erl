-module(strobe).
-export([start/0, start/1]).

-define(LEDS, 5).
-define(DEV, "/dev/ttyUSB0").
-define(INTERVAL, 200). % milliseconds


start() ->
    start([]).

start(Opt) ->
    Dev = proplists:get_value(dev, Opt, ?DEV),
    Leds = proplists:get_value(leds, Opt, ?LEDS),
    Interval = proplists:get_value(interval, Opt, ?INTERVAL),

    {ok,FD} = serctl:open(Dev),

    Termios = lists:foldl(
        fun(Fun, Acc) -> Fun(Acc) end,
        serctl:mode(raw),
        [
            fun(N) -> serctl:flow(N, false) end,
            fun(N) -> serctl:ispeed(N, b9600) end,
            fun(N) -> serctl:ospeed(N, b9600) end
        ]
    ),

    ok = serctl:tcsetattr(FD, tcsanow, Termios),

    {ok, Termios1} = serctl:tcgetattr(FD),
    false = serctl:flow(Termios1),

    blink(FD, Interval, lists:seq(0,Leds)).

blink(FD, N, [H|T]) ->
    ok = serctl:write(FD, <<(1 bsl H):4/big-unsigned-integer-unit:8>>),
    timer:sleep(N),
    blink(FD, N, T ++ [H]).
