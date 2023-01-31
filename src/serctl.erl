%%% @copyright 2011-2023 Michael Santos <michael.santos@gmail.com>

%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice,
%%% this list of conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its
%%% contributors may be used to endorse or promote products derived from
%%% this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%% PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(serctl).
-include("serctl.hrl").

-export([
    open/1,
    close/1,

    read/2,
    write/2,

    readx/2, readx/3,

    tcgetattr/1,
    tcsetattr/3,
    tcflush/2,

    cfsetispeed/2,
    cfsetospeed/2,

    ioctl/3,

    constant/0, constant/1,

    termios/1,

    setflag/2,
    getflag/3,
    flow/1, flow/2,
    mode/1,
    ispeed/1, ispeed/2,
    ospeed/1, ospeed/2,
    baud/1,

    getfd/1,

    offset/2,
    wordalign/1, wordalign/2
]).
-export([
    init/0
]).

-type fd() :: any().
-type dev() :: iodata() | {'fd', integer()}.
-type errno() :: {'error', file:posix()}.
-type termios() :: #termios{} | binary().

-export_type([fd/0, dev/0, errno/0, termios/0]).

-on_load(on_load/0).

%%--------------------------------------------------------------------
%%% NIF Stubs
%%--------------------------------------------------------------------
init() ->
    on_load().

on_load() ->
    erlang:load_nif(progname(), []).

% @doc Open a serial device
%
% A serial device is a character device such as /dev/ttyUSB0.
%
% A previously opened file descriptor can also be used. The fd should
% be opened with the O_NONBLOCK|O_NOCTTY flags.
-spec open(dev()) -> {'ok', fd()} | errno().
open({fd, FD}) ->
    fdopen(FD);
open(Dev) ->
    open_nif(Dev).

open_nif(_) ->
    erlang:nif_error(not_implemented).

fdopen(_) ->
    erlang:nif_error(not_implemented).

% @doc Explicitly close a serial device
%
% The device is automatically closed if the process holding open
% the serial device exits.
-spec close(fd()) -> {'ok', fd()} | errno().
close(_) ->
    erlang:nif_error(not_implemented).

% @doc Read from a serial device
%
% Size is an unsigned long.
-spec read(fd(), non_neg_integer()) -> {'ok', binary()} | errno().
read(_, _) ->
    erlang:nif_error(not_implemented).

% @doc Write data to a serial device
%
% Partial writes return the number of bytes written.
-spec write(fd(), iodata()) -> 'ok' | {'ok', non_neg_integer()} | errno().
write(FD, Buf) ->
    Size = iolist_size(Buf),
    case write_nif(FD, Buf) of
        {ok, Size} ->
            ok;
        Reply ->
            Reply
    end.

write_nif(_, _) ->
    erlang:nif_error(not_implemented).

% @doc Get the terminal attributes of a serial device
%
% Returns the contents of the system struct termios as a binary.
-spec tcgetattr(fd()) -> {'ok', binary()} | errno().
tcgetattr(_) ->
    erlang:nif_error(not_implemented).

% @doc Sets the terminal attributes of a serial device
%
% 'tcsasoft' is a non-portable, BSD action. tcsetattr/3 will return
% `{error,unsupported}' on other platforms.  Warning: the contents of
% Termios are passed directly to tcsettr(3). If the system tcsettr(3)
% does not perform any validation of the structure, it is possible the
% Erlang VM may crash.
-spec tcsetattr(fd(), [atom()] | atom() | integer(), termios()) ->
    'ok' | errno() | {'error', 'unsupported'}.
tcsetattr(FD, Action, Termios) when is_list(Action) ->
    Option = lists:foldl(
        fun
            (_X, undefined) ->
                undefined;
            (X, N) ->
                case constant(X) of
                    undefined -> undefined;
                    Constant -> Constant bxor N
                end
        end,
        0,
        Action
    ),
    case Option of
        undefined ->
            {error, unsupported};
        N ->
            tcsetattr(FD, N, Termios)
    end;
tcsetattr(FD, Action, Termios) when is_atom(Action) ->
    case constant(Action) of
        undefined ->
            {error, unsupported};
        N ->
            tcsetattr(FD, N, Termios)
    end;
tcsetattr(FD, Action, #termios{} = Termios) ->
    tcsetattr(FD, Action, termios(Termios));
tcsetattr(FD, Action, Termios) ->
    tcsetattr_nif(FD, Action, Termios).

tcsetattr_nif(_, _, _) ->
    erlang:nif_error(not_implemented).

% @doc discards data written but not transmitted or received but not read
%
% The second argument determines whether to flush input, output, or both
-spec tcflush(fd(), atom()) -> 'ok' | errno() | {'error', 'unsupported'}.
tcflush(FD, Discard) when is_atom(Discard) ->
    case constant(Discard) of
        undefined ->
            {error, unsupported};
        N ->
            tcflush_nif(FD, N)
    end.

tcflush_nif(_, _) ->
    erlang:nif_error(not_implemented).

% @doc Set the input speed of a serial device
%
% See the warning for tcsetattr/2.
%
% Failure: badarg if Speed is an invalid atom.
-spec cfsetispeed(termios(), atom() | integer()) -> binary().
cfsetispeed(#termios{} = Termios, Speed) ->
    cfsetispeed(termios(Termios), Speed);
cfsetispeed(Termios, Speed) when is_atom(Speed) ->
    case constant(Speed) of
        undefined ->
            erlang:error(badarg, [Termios, Speed]);
        Constant ->
            cfsetispeed(termios(Termios), Constant)
    end;
cfsetispeed(Termios, Speed) ->
    cfsetispeed_nif(Termios, Speed).

cfsetispeed_nif(_, _) ->
    erlang:nif_error(not_implemented).

% @doc Set the input speed of the serial device.
%
% See the warning for tcsetattr/2.
%
% Failure: badarg if Speed is an invalid atom.
-spec cfsetospeed(termios(), atom() | integer()) -> binary().
cfsetospeed(#termios{} = Termios, Speed) ->
    cfsetospeed(termios(Termios), Speed);
cfsetospeed(Termios, Speed) when is_atom(Speed) ->
    case constant(Speed) of
        undefined ->
            erlang:error(badarg, [Termios, Speed]);
        Constant ->
            cfsetospeed(termios(Termios), Constant)
    end;
cfsetospeed(Termios, Speed) ->
    cfsetospeed_nif(Termios, Speed).

cfsetospeed_nif(_, _) ->
    erlang:nif_error(not_implemented).

% @doc Perform operations controlling a serial device
%
% The In argument is a binary holding the input parameter to the device
% request. The Out parameter will hold the result of the request if the
% ioctl is in/out.
%
% ioctl/3 can be used for implementing most serial operations.
%
% == Examples ==
%
% ```
% -define(TCXONC, 16#540A).
% tcflow(FD, Action) when is_atom(Action) ->
%     case serctl:constant(Action) of
%         undefined ->
%             {error, unsupported};
%         N ->
%             serctl:ioctl(
%                 fd,
%                 ?TCFLSH,
%                 <<N:4/native-unsigned-integer-unit:8>>
%             )
%     end.
% '''
-spec ioctl(fd(), integer(), binary()) -> {'ok', binary()} | errno().
ioctl(_, _, _) ->
    erlang:nif_error(not_implemented).

% @doc Map of atoms representing terminal attribute constants to integers
%
% Varies across platforms.
-spec constant() -> proplists:proplist().
constant() ->
    erlang:nif_error(not_implemented).

-spec constant(atom()) -> integer() | 'undefined'.
constant(_) ->
    erlang:nif_error(not_implemented).

% @doc Returns the file descriptor associated with the NIF resource
%
% The file descriptor can be used with erlang:open_port/2.
-spec getfd(fd()) -> integer().
getfd(_) ->
    erlang:nif_error(not_implemented).

%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------
% @doc Read the specified number of bytes from a serial device
%
% readx/2 will block forever.
%
% readx/3 accepts a timeout value. The behaviour of readx/3 when the timeout
% is reached is to throw away any buffered data and return {error, eintr}
% to the caller, e.g., the caller will not be returned the contents of
% a partial read. (The justification for this behaviour: the caller has
% stated they require a fixed number of bytes so the contents of a partial
% read represents unspecified behaviour.)
-spec readx(fd(), non_neg_integer()) -> {'ok', binary()} | errno().
readx(FD, N) ->
    readx(FD, N, infinity).

-spec readx(fd(), non_neg_integer(), timeout()) -> {'ok', binary()} | errno().
readx(FD, N, Timeout) ->
    Ref = make_ref(),
    Self = self(),
    Pid = spawn(fun() -> poll(Ref, FD, N, Self) end),
    receive
        {Ref, Reply} ->
            Reply
    after Timeout ->
        exit(Pid, kill),
        {error, eintr}
    end.

% @doc Returns an Erlang termios record used for setting the attributes of a serial device
%
% For example, to create attributes that can be used to enable hardware
% flow control on a serial device:
%
% ```
% {ok, FD} = serctl:open("/dev/ttyUSB0"),
% {ok, Termios} = serctl:tcgetattr(FD),
% Termios1 = serctl:setflag(Termios, [{cflag, [{crtscts, true}]}]),
% ok = serctl:tcsetattr(FD, tcsanow, Termios1).
% '''
-spec setflag(binary() | #termios{}, proplists:proplist()) -> #termios{}.
setflag(Termios, Opt) when is_binary(Termios) ->
    setflag(termios(Termios), Opt);
setflag(
    #termios{
        cflag = Cflag0,
        lflag = Lflag0,
        iflag = Iflag0,
        oflag = Oflag0
    } = Termios,
    Opt
) when is_list(Opt) ->
    Cflag = setflag_1(Cflag0, proplists:get_value(cflag, Opt)),
    Lflag = setflag_1(Lflag0, proplists:get_value(lflag, Opt)),
    Iflag = setflag_1(Iflag0, proplists:get_value(iflag, Opt)),
    Oflag = setflag_1(Oflag0, proplists:get_value(oflag, Opt)),

    Termios#termios{
        cflag = Cflag,
        lflag = Lflag,
        iflag = Iflag,
        oflag = Oflag
    }.

setflag_1(Val, undefined) ->
    Val;
setflag_1(Val, []) ->
    Val;
setflag_1(Bin, [{Offset, Val} | Rest]) when is_binary(Bin), Offset >= 0, Val >= 0 ->
    setflag_1(offset(Bin, {Offset, Val}), Rest);
setflag_1(Val, [{Key, false} | Rest]) ->
    Val1 = Val band bnot constant(Key),
    setflag_1(Val1, Rest);
setflag_1(Val, [{Key, true} | Rest]) ->
    Val1 = Val bor constant(Key),
    setflag_1(Val1, Rest);
setflag_1(Val, [Key | Rest]) when is_atom(Key) ->
    setflag_1(Val, [{Key, true} | Rest]).

% @doc Returns whether a flag is enabled
%
% Opt is one of the atoms returned using serctl:constant/0.
-spec getflag(<<_:64, _:_*8>> | #termios{}, 'cflag' | 'iflag' | 'lflag' | 'oflag', atom()) ->
    boolean().
getflag(Termios, Flag, Opt) when is_binary(Termios) ->
    getflag(termios(Termios), Flag, Opt);
getflag(#termios{} = Termios, Flag, Opt) ->
    getflag_1(Termios, Flag, Opt).

getflag_1(#termios{cflag = Flag}, cflag, Opt) ->
    getflag_2(Flag, Opt);
getflag_1(#termios{lflag = Flag}, lflag, Opt) ->
    getflag_2(Flag, Opt);
getflag_1(#termios{iflag = Flag}, iflag, Opt) ->
    getflag_2(Flag, Opt);
getflag_1(#termios{oflag = Flag}, oflag, Opt) ->
    getflag_2(Flag, Opt).

getflag_2(Flag, Opt) ->
    case constant(Opt) of
        undefined -> false;
        N -> N == Flag band N
    end.

% @doc Get/set serial device flow control
%
% flow/1 indicates whether flow control is enabled in a serial device's
% terminal attributes. flow/2 returns a termios structure that can be used
% for setting a serial device's flow control.
-spec flow(<<_:64, _:_*8>> | #termios{}) -> boolean().
flow(Termios) ->
    getflag(Termios, cflag, crtscts).

-spec flow(<<_:64, _:_*8>> | #termios{}, boolean()) -> #termios{}.
flow(Termios, Bool) when Bool == true; Bool == false ->
    setflag(Termios, [{cflag, [{crtscts, Bool}]}]).

% @doc Enable raw mode
%
% Returns an Erlang termios record with attributes that can be used to
% put the serial device into raw mode.
mode(raw) ->
    #termios{
        cc = lists:foldl(
            fun({Offset, Val}, Bin) ->
                offset(Bin, {Offset, Val})
            end,
            % zero'ed bytes
            <<0:(constant(nccs) * 8)>>,
            [
                % Minimum number of characters
                {constant(vmin), 1},
                % Timeout in deciseconds
                {constant(vtime), 0}
            ]
        ),

        % ignore (discard) parity errors
        iflag = constant(ignpar),

        cflag =
            constant(cs8) bor
                constant(clocal) bor
                constant(crtscts) bor
                constant(cread)
    }.

% @doc return the input speed of a serial device
%
% Note the speed returned is the constant defined for the system and
% may differ between platforms.
%
% ispeed/2 returns an Erlang termios record that can be used for setting
% the input speed of the serial device.
%
% Failure: badarg if Speed is an invalid atom.
-spec ispeed(binary() | #termios{}) -> non_neg_integer().
ispeed(Speed) when is_binary(Speed) ->
    ispeed(termios(Speed));
ispeed(#termios{ispeed = Speed}) ->
    Speed.

-spec ispeed(<<_:64, _:_*8>> | #termios{}, atom() | integer()) -> <<_:8, _:_*8>> | #termios{}.
ispeed(Termios, Speed) when is_binary(Termios) ->
    ispeed(termios(Termios), Speed);
ispeed(Termios, Speed) when is_atom(Speed) ->
    case constant(Speed) of
        undefined ->
            erlang:error(badarg, [Termios, Speed]);
        Constant ->
            ispeed(Termios, Constant)
    end;
ispeed(#termios{} = Termios, Speed) when is_integer(Speed) ->
    termios(cfsetispeed(Termios, Speed)).

% @doc return the output speed of a serial device
%
% Note the speed returned is the constant defined for the system and
% may differ between platforms.
%
% ospeed/2 returns an Erlang termios record that can be used for setting
% the output speed of the serial device.
%
% Failure: badarg if Speed is an invalid atom.
-spec ospeed(binary() | #termios{}) -> non_neg_integer().
ospeed(Speed) when is_binary(Speed) ->
    ospeed(termios(Speed));
ospeed(#termios{ospeed = Speed}) ->
    Speed.

-spec ospeed(<<_:64, _:_*8>> | #termios{}, atom() | integer()) -> <<_:8, _:_*8>> | #termios{}.
ospeed(Termios, Speed) when is_binary(Termios) ->
    ospeed(termios(Termios), Speed);
ospeed(Termios, Speed) when is_atom(Speed) ->
    case constant(Speed) of
        undefined ->
            erlang:error(badarg, [Termios, Speed]);
        Constant ->
            ospeed(Termios, Constant)
    end;
ospeed(#termios{} = Termios, Speed) when is_integer(Speed) ->
    termios(cfsetospeed(Termios, Speed)).

% @doc Return the constant defined for the baud rate for the platform
baud(Speed) when is_integer(Speed) ->
    constant(list_to_atom("b" ++ integer_to_list(Speed))).

% doc Convert between a C struct termios and an Erlang record
%
% Terminal interface structure
%
% struct termios is used to control the behaviour of
% the serial port. We pass the actual struct between
% Erlang and C. Sending junk might cause the C side
% to crash if there is a bug in the terminal lib. Using
% a NIF resource would help but would require moving
% some of the logic from Erlang to C (this would help
% with portability though).
%
% Only the first 4 fields of the struct are standardized.
% A simple way of handling portablity would be to parse
% the first 4 fields and leave the rest as a binary.
%
% Linux:
% ```
% #define NCCS 32
% struct termios
%   {
%           tcflag_t c_iflag;       /* input mode flags */
%           tcflag_t c_oflag;       /* output mode flags */
%           tcflag_t c_cflag;       /* control mode flags */
%           tcflag_t c_lflag;       /* local mode flags */
%           cc_t c_line;            /* line discipline */
%           cc_t c_cc[NCCS];        /* control characters */
%           speed_t c_ispeed;       /* input speed */
%           speed_t c_ospeed;       /* output speed */
%  #define _HAVE_STRUCT_TERMIOS_C_ISPEED 1
%  #define _HAVE_STRUCT_TERMIOS_C_OSPEED 1
%  };
% '''
%
% BSD (Max OS X, FreeBSD, OpenBSD, NetBSD, ...):
% ```
% #define NCCS 20
% typedef unsigned int    tcflag_t;
% typedef unsigned char   cc_t;
% typedef unsigned int    speed_t;
%
% struct termios {
%         tcflag_t    c_iflag;    /* input flags */
%         tcflag_t    c_oflag;    /* output flags */
%         tcflag_t    c_cflag;    /* control flags */
%         tcflag_t    c_lflag;    /* local flags */
%         cc_t        c_cc[NCCS]; /* control chars */
%         speed_t     c_ispeed;   /* input speed */
%         speed_t     c_ospeed;   /* output speed */
% };
% '''
%
% On 64-bit Mac OS X:
%
% ```
% typedef unsigned long long   user_tcflag_t;
% typedef unsigned long long   user_speed_t;
%
% Solaris:
% #define NCCS    19
% struct termios {
%         tcflag_t    c_iflag;    /* input modes */
%         tcflag_t    c_oflag;    /* output modes */
%         tcflag_t    c_cflag;    /* control modes */
%         tcflag_t    c_lflag;    /* line discipline modes */
%         cc_t        c_cc[NCCS]; /* control chars */
% };
% '''
-spec termios(binary() | #termios{}) -> binary() | #termios{}.
termios(Termios) ->
    termios(Termios, os:type(), erlang:system_info({wordsize, external})).

termios(
    <<
        % input mode flags
        ?UINT32(Iflag),
        % output mode flags
        ?UINT32(Oflag),
        % control mode flags
        ?UINT32(Cflag),
        % local mode flags
        ?UINT32(Lflag),
        Rest/binary
    >>,
    {unix, linux},
    _
) ->
    NCCS = constant(nccs),
    <<
        % line discipline
        Line:8,
        % control characters
        Cc:NCCS/bytes,
        Rest1/binary
    >> = Rest,

    Pad = wordalign(1 + NCCS, 4),
    <<
        _:Pad,
        % input speed
        ?UINT32(Ispeed),
        % output speed
        ?UINT32(Ospeed)
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
termios(
    <<
        % input mode flags
        ?UINT32(Iflag),
        % output mode flags
        ?UINT32(Oflag),
        % control mode flags
        ?UINT32(Cflag),
        % local mode flags
        ?UINT32(Lflag),
        Rest/binary
    >>,
    {unix, sunos},
    _
) ->
    NCCS = constant(nccs),
    <<
        % control characters
        Cc:NCCS/bytes,
        _/binary
    >> = Rest,

    #termios{
        iflag = Iflag,
        oflag = Oflag,
        cflag = Cflag,
        lflag = Lflag,
        cc = Cc
    };
termios(
    <<
        % input mode flags
        ?UINT64(Iflag),
        % output mode flags
        ?UINT64(Oflag),
        % control mode flags
        ?UINT64(Cflag),
        % local mode flags
        ?UINT64(Lflag),
        Rest/binary
    >>,
    {unix, darwin},
    8
) ->
    NCCS = constant(nccs),
    <<
        % control characters
        Cc:NCCS/bytes,
        Rest1/binary
    >> = Rest,

    Pad = wordalign(NCCS, 8),
    <<
        _:Pad,
        % input speed
        ?UINT64(Ispeed),
        % output speed
        ?UINT64(Ospeed)
    >> = Rest1,
    #termios{
        iflag = Iflag,
        oflag = Oflag,
        cflag = Cflag,
        lflag = Lflag,
        cc = Cc,
        ispeed = Ispeed,
        ospeed = Ospeed
    };
termios(
    <<
        % input mode flags
        ?UINT32(Iflag),
        % output mode flags
        ?UINT32(Oflag),
        % control mode flags
        ?UINT32(Cflag),
        % local mode flags
        ?UINT32(Lflag),
        Rest/binary
    >>,
    {unix, _},
    _
) ->
    NCCS = constant(nccs),
    <<
        % control characters
        Cc:NCCS/bytes,
        Rest1/binary
    >> = Rest,

    Pad = wordalign(NCCS, 4),
    <<
        _:Pad,
        % input speed
        ?UINT32(Ispeed),
        % output speed
        ?UINT32(Ospeed)
    >> = Rest1,
    #termios{
        iflag = Iflag,
        oflag = Oflag,
        cflag = Cflag,
        lflag = Lflag,
        cc = Cc,
        ispeed = Ispeed,
        ospeed = Ospeed
    };
termios(
    #termios{
        iflag = Iflag,
        oflag = Oflag,
        cflag = Cflag,
        lflag = Lflag,
        line = Line,
        cc = Cc,
        ispeed = Ispeed,
        ospeed = Ospeed
    },
    {unix, linux},
    _
) ->
    NCCS = constant(nccs),

    Cc1 =
        case Cc of
            <<>> -> <<0:(NCCS * 8)>>;
            _ -> Cc
        end,

    Pad = wordalign(1 + NCCS, 4),

    <<
        ?UINT32(Iflag),
        ?UINT32(Oflag),
        ?UINT32(Cflag),
        ?UINT32(Lflag),
        Line:8,
        Cc1/binary,
        0:Pad,
        ?UINT32(Ispeed),
        ?UINT32(Ospeed)
    >>;
termios(
    #termios{
        iflag = Iflag,
        oflag = Oflag,
        cflag = Cflag,
        lflag = Lflag,
        cc = Cc
    },
    {unix, sunos},
    _
) ->
    NCCS = constant(nccs),

    Cc1 =
        case Cc of
            <<>> -> <<0:(NCCS * 8)>>;
            _ -> Cc
        end,

    Pad = wordalign(NCCS, 4),

    <<
        ?UINT32(Iflag),
        ?UINT32(Oflag),
        ?UINT32(Cflag),
        ?UINT32(Lflag),
        Cc1/binary,
        0:Pad
    >>;
termios(
    #termios{
        iflag = Iflag,
        oflag = Oflag,
        cflag = Cflag,
        lflag = Lflag,
        cc = Cc,
        ispeed = Ispeed,
        ospeed = Ospeed
    },
    {unix, darwin},
    8
) ->
    NCCS = constant(nccs),

    Cc1 =
        case Cc of
            <<>> -> <<0:(NCCS * 8)>>;
            _ -> Cc
        end,

    Pad = wordalign(NCCS, 8),

    <<
        ?UINT64(Iflag),
        ?UINT64(Oflag),
        ?UINT64(Cflag),
        ?UINT64(Lflag),
        Cc1/binary,
        0:Pad,
        ?UINT64(Ispeed),
        ?UINT64(Ospeed)
    >>;
termios(
    #termios{
        iflag = Iflag,
        oflag = Oflag,
        cflag = Cflag,
        lflag = Lflag,
        cc = Cc,
        ispeed = Ispeed,
        ospeed = Ospeed
    },
    {unix, _},
    _
) ->
    NCCS = constant(nccs),

    Cc1 =
        case Cc of
            <<>> -> <<0:(NCCS * 8)>>;
            _ -> Cc
        end,

    Pad = wordalign(NCCS, 4),

    <<
        ?UINT32(Iflag),
        ?UINT32(Oflag),
        ?UINT32(Cflag),
        ?UINT32(Lflag),
        Cc1/binary,
        0:Pad,
        ?UINT32(Ispeed),
        ?UINT32(Ospeed)
    >>.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec progname() -> binary() | string().
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

poll(Ref, FD, N, Pid) ->
    poll(Ref, FD, N, N, Pid, []).
poll(Ref, FD, Total, N, Pid, Acc) ->
    Size = iolist_size(Acc),
    case read(FD, N) of
        {ok, Buf} when byte_size(Buf) == Total ->
            Pid ! {Ref, {ok, Buf}};
        {ok, Buf} when byte_size(Buf) + Size == Total ->
            Pid ! {Ref, {ok, iolist_to_binary(lists:reverse([Buf | Acc]))}};
        {ok, Buf} ->
            poll(Ref, FD, Total, N - byte_size(Buf), Pid, [Buf | Acc]);
        {error, eagain} ->
            timer:sleep(10),
            poll(Ref, FD, Total, N, Pid, Acc);
        {error, Error} ->
            % XXX throw away away buffered data
            Pid ! {Ref, {error, Error}}
    end.
