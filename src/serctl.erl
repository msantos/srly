%% Copyright (c) 2011-2012, Michael Santos <michael.santos@gmail.com>
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
-module(serctl).
-include("serctl.hrl").

-export([
        open/1,
        close/1,

        read/2,
        write/2,

        tcgetattr/1,
        tcsetattr/3,

        cfsetispeed/2,
        cfsetospeed/2,

        constant/0, constant/1,

        termios/1,
        ispeed/1, ispeed/2,
        ospeed/1, ospeed/2,

        attr/2,
        getflag/3,
        flow/1, flow/2,
        mode/1,
        speed/2,
        setattr/2, setattr/3,

        getfd/1,

        offset/2,
        wordalign/1, wordalign/2
    ]).
-export([
        init/0
    ]).


-on_load(on_load/0).


%%--------------------------------------------------------------------
%%% NIF Stubs
%%--------------------------------------------------------------------
init() ->
    on_load().

on_load() ->
    erlang:load_nif(progname(), []).

open(_) ->
    erlang:error(not_implemented).

close(_) ->
    erlang:error(not_implemented).

read(_,_) ->
    erlang:error(not_implemented).

write(_,_) ->
    erlang:error(not_implemented).

tcgetattr(_) ->
    erlang:error(not_implemented).

tcsetattr(_,_,_) ->
    erlang:error(not_implemented).

cfsetispeed(_,_) ->
    erlang:error(not_implemented).

cfsetospeed(_,_) ->
    erlang:error(not_implemented).

constant() ->
    erlang:error(not_implemented).

constant(_) ->
    erlang:error(not_implemented).

getfd(_) ->
    erlang:error(not_implemented).


%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------
setattr(FD, Attr) ->
    setattr(FD, constant(tcsanow), Attr).

setattr(FD, Action, #termios{} = Attr) ->
    setattr(FD, Action, termios(Attr));
setattr(FD, Action, Attr) when is_binary(Attr) ->
    tcsetattr(FD, Action, Attr).


attr(FD, Opt) when is_list(Opt) ->
    case tcgetattr(FD) of
        {ok, Attr} ->
            T = termios(Attr),
            #termios{
                cflag = Cflag,
                lflag = Lflag,
                iflag = Iflag,
                oflag = Oflag
            } = T,

            Cflag1 = attr_1(Cflag, proplists:get_value(cflag, Opt)),
            Lflag1 = attr_1(Lflag, proplists:get_value(lflag, Opt)),
            Iflag1 = attr_1(Iflag, proplists:get_value(iflag, Opt)),
            Oflag1 = attr_1(Oflag, proplists:get_value(oflag, Opt)),

            {ok, T#termios{
                    cflag = Cflag1,
                    lflag = Lflag1,
                    iflag = Iflag1,
                    oflag = Oflag1
                }};
        {error, _} = Error ->
            Error
    end.

attr_1(Val, undefined) ->
    Val;
attr_1(Val, []) ->
    Val;
attr_1(Bin, [{Offset, Val}|Rest]) when is_binary(Bin), Offset >= 0, Val >= 0 ->
    attr_1(offset(Bin, {Offset, Val}), Rest);
attr_1(Val, [{Key, false}|Rest]) ->
    Val1 = Val band bnot constant(Key),
    attr_1(Val1, Rest);
attr_1(Val, [{Key, true}|Rest]) ->
    Val1 = Val bor constant(Key),
    attr_1(Val1, Rest);
attr_1(Val, [Key|Rest]) when is_atom(Key) ->
    attr_1(Val, [{Key, true}|Rest]).


getflag(FD, Flag, Opt) ->
    case tcgetattr(FD) of
        {ok, Attr} ->
            T = termios(Attr),
            getflag_1(Flag, Opt, T);
        {error, _} = Error ->
            Error
    end.

getflag_1(cflag, Opt, #termios{cflag = Flag}) ->
    getflag_2(Opt, Flag);
getflag_1(lflag, Opt, #termios{lflag = Flag}) ->
    getflag_2(Opt, Flag);
getflag_1(iflag, Opt, #termios{iflag = Flag}) ->
    getflag_2(Opt, Flag);
getflag_1(oflag, Opt, #termios{oflag = Flag}) ->
    getflag_2(Opt, Flag).

getflag_2(Opt, Flag) ->
    N = constant(Opt),
    N == Flag band N.

flow(FD) ->
    getflag(FD, cflag, crtscts).
flow(FD, Bool) ->
    case attr(FD, [{cflag, [{crtscts, Bool}]}]) of
        {ok, Attr} ->
            setattr(FD, Attr);
        Error ->
            Error
    end.

mode(raw) ->
    #termios{
        cc = lists:foldl(
            fun({Offset, Val}, Bin) ->
                    offset(Bin, {Offset, Val})
            end,
            <<0:(constant(nccs)*8)>>,   % zero'ed bytes
            [
                {constant(vmin), 1},    % Minimum number of characters
                {constant(vtime), 0}    % Timeout in deciseconds
            ]),

        iflag = constant(ignpar),       % ignore (discard) parity errors

        cflag = constant(cs8)
        bor constant(clocal)
        bor constant(crtscts)
        bor constant(cread)
    }.

speed(FD, Speed) when is_integer(Speed) ->
    speed(FD, list_to_atom("b" ++ integer_to_list(Speed)));
speed(FD, Speed) when is_atom(Speed) ->
    speed_1(FD, constant(Speed)).

speed_1(_FD, undefined) ->
    {error, unsupported};
speed_1(FD, Speed) ->
    case {ispeed(FD, Speed), ospeed(FD, Speed)} of
        {ok, ok} -> ok;
        {ok, Error} -> Error;
        {Error, _} -> Error
    end.

ispeed(FD) ->
    case tcgetattr(FD) of
        {ok, Attr} ->
            #termios{ispeed = Speed} = termios(Attr),
            Speed;
        {error, _} = Error ->
            Error
    end.
ispeed(FD, Speed) ->
    case tcgetattr(FD) of
        {ok, Attr} ->
            setspeed(FD, cfsetispeed(Attr, Speed));
        {error, _} = Error ->
            Error
    end.

ospeed(FD) ->
    case tcgetattr(FD) of
        {ok, Attr} ->
            #termios{ospeed = Speed} = termios(Attr),
            Speed;
        {error, _} = Error ->
            Error
    end.
ospeed(FD, Speed) ->
    case tcgetattr(FD) of
        {ok, Attr} ->
            setspeed(FD, cfsetispeed(Attr, Speed));
        {error, _} = Error ->
            Error
    end.

%% Terminal interface structure
%%
%% struct termios is used to control the behaviour of
%% the serial port. We pass the actual struct between
%% Erlang and C. Sending junk might cause the C side
%% to crash if there is a bug in the terminal lib. Using
%% a NIF resource would help but would require moving
%% some of the logic from Erlang to C (this would help
%% with portability though).
%%
%% Only the first 4 fields of the struct are standardized.
%% A simple way of handling portablity would be to parse
%% the first 4 fields and leave the rest as a binary.
%%
%% Linux:
%% #define NCCS 32
%% struct termios
%%   { 
%%           tcflag_t c_iflag;       /* input mode flags */
%%           tcflag_t c_oflag;       /* output mode flags */
%%           tcflag_t c_cflag;       /* control mode flags */
%%           tcflag_t c_lflag;       /* local mode flags */
%%           cc_t c_line;            /* line discipline */
%%           cc_t c_cc[NCCS];        /* control characters */
%%           speed_t c_ispeed;       /* input speed */
%%           speed_t c_ospeed;       /* output speed */
%%  #define _HAVE_STRUCT_TERMIOS_C_ISPEED 1
%%  #define _HAVE_STRUCT_TERMIOS_C_OSPEED 1
%%  };
%%
%% BSD (Max OS X, FreeBSD):
%% #define NCCS 20
%% struct termios {
%%         tcflag_t    c_iflag;    /* input flags */
%%         tcflag_t    c_oflag;    /* output flags */
%%         tcflag_t    c_cflag;    /* control flags */
%%         tcflag_t    c_lflag;    /* local flags */
%%         cc_t        c_cc[NCCS]; /* control chars */
%%         speed_t     c_ispeed;   /* input speed */
%%         speed_t     c_ospeed;   /* output speed */
%% };
termios(<<
    Iflag:?UINT32,          % input mode flags
    Oflag:?UINT32,          % output mode flags
    Cflag:?UINT32,          % control mode flags
    Lflag:?UINT32,          % local mode flags
    Rest/binary>>) ->

    LineSz = case os() of
        linux -> 8;
        bsd -> 0
    end,

    NCCS = constant(nccs),
    <<
    Line:LineSz,            % line discipline
    Cc:NCCS/bytes,          % control characters
    Rest1/binary
    >> = Rest,

    Pad = wordalign(LineSz div 8 + NCCS, 4),
    <<
    _:Pad,
    Ispeed:?UINT32,         % input speed
    Ospeed:?UINT32          % output speed
    >> = Rest1,
    #termios{
        iflag = Iflag,
        oflag = Oflag,
        cflag = Cflag,
        lflag = Lflag,
        line = Line,
        cc = Cc,
        ispeed = Ispeed,
        ospeed = Ospeed
    };
termios(#termios{
        iflag = Iflag,
        oflag = Oflag,
        cflag = Cflag,
        lflag = Lflag,
        line = Line,
        cc = Cc,
        ispeed = Ispeed,
        ospeed = Ospeed
    }) ->

    LineSz = case os() of
        linux -> 8;
        bsd -> 0
    end,

    NCCS = constant(nccs),

    Cc1 = case Cc of
        <<>> -> <<0:(NCCS*8)>>;
        _ -> Cc
    end,

    Pad = wordalign(LineSz div 8 + NCCS, 4),
    <<
    Iflag:?UINT32,
    Oflag:?UINT32,
    Cflag:?UINT32,
    Lflag:?UINT32,
    Line:LineSz,
    Cc1/binary,
    0:Pad,
    Ispeed:?UINT32,
    Ospeed:?UINT32
    >>.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
progname() ->
    filename:join([
            filename:dirname(code:which(?MODULE)),
            "..",
            "priv",
            ?MODULE
        ]).


% Return pad size in bits
wordalign(Offset) ->
    wordalign(Offset, erlang:system_info({wordsize, external})).
wordalign(Offset, Align) ->
    ((Align - (Offset rem Align)) rem Align) * 8.


setspeed(FD, {ok, Attr}) ->
    setattr(FD, Attr);
setspeed(_FD, Error) ->
    Error.


offset(Cc, {Offset, Val}) when is_binary(Cc) ->
    tuple_to_binary(
        setelement(
            Offset,
            binary_to_tuple(Cc),
            Val
        )
    ).


binary_to_tuple(N) when is_binary(N) ->
    list_to_tuple(binary_to_list(N)).
tuple_to_binary(N) when is_tuple(N) ->
    list_to_binary(tuple_to_list(N)).


os() ->
    case os:type() of
        {unix, linux} -> linux;
        {unix, freebsd} -> bsd;
        {unix, darwin} -> bsd
    end.
