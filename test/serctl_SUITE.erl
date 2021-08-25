-module(serctl_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("srly/include/serctl.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    constant/1,
    getfd/1,
    flow/1,
    mode/1,
    ispeed/1,
    ospeed/1,
    ispeed_badarg/1,
    ospeed_badarg/1,
    baud/1,
    termios/1
]).

all() ->
    [
        constant,
        getfd,
        flow,
        mode,
        ispeed,
        ospeed,
        ispeed_badarg,
        ospeed_badarg,
        baud,
        termios
    ].

init_per_suite(Config) ->
    Dev =
        case os:getenv("SRLY_TEST_PORT") of
            false ->
                "/dev/tty";
            TTY ->
                TTY
        end,
    {ok, FD} = serctl:open(Dev),
    {ok, Termios} = serctl:tcgetattr(FD),
    [{fd, FD}, {termios, serctl:termios(Termios)} | Config].

end_per_suite(Config) ->
    FD = ?config(fd, Config),
    serctl:close(FD),
    Config.

constant(_Config) ->
    [V = serctl:constant(K) || {K, V} <- serctl:constant()].

getfd(Config) ->
    FD = ?config(fd, Config),
    N = serctl:getfd(FD),
    true = is_integer(N).

flow(Config) ->
    Termios = ?config(termios, Config),
    N = serctl:flow(Termios),
    true = N == true orelse N == false.

mode(_Config) ->
    N = serctl:mode(raw),
    #termios{} = N.

ispeed(Config) ->
    Termios = ?config(termios, Config),
    N = serctl:ispeed(Termios),
    true = is_integer(N).

ospeed(Config) ->
    Termios = ?config(termios, Config),
    N = serctl:ospeed(Termios),
    true = is_integer(N).

ispeed_badarg(Config) ->
    Termios = ?config(termios, Config),
    N = (catch serctl:ispeed(Termios, notexist)),
    {'EXIT', {badarg, _}} = N,
    ok.

ospeed_badarg(Config) ->
    Termios = ?config(termios, Config),
    N = (catch serctl:ospeed(Termios, notexist)),
    {'EXIT', {badarg, _}} = N,
    ok.

baud(_Config) ->
    N = serctl:baud(115200),
    case os:type() of
        {unix, linux} ->
            4098 = N;
        _ ->
            []
    end.

termios(_Config) ->
    Mode = serctl:mode(raw),
    Bin = serctl:termios(Mode),
    Mode = serctl:termios(Bin).
