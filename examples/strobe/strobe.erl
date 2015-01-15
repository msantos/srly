%% Copyright (c) 2012-2015, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%%
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
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
