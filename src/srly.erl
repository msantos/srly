%% Copyright (c) 2011-2013, Michael Santos <michael.santos@gmail.com>
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
-module(srly).
-behaviour(gen_server).


-export([
        open/1, open/2,
        close/1,

        getfd/1,

        read/2,
        write/2,
        send/2,

        controlling_process/2
    ]).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {
        oattr,      % Original termios attributes
        port,
        pid,        % PID of controlling process
        fd,         % serial dev file descriptor
        dev         % device name
    }).


%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------
open(Dev) ->
    open(Dev, []).

open(Dev, Opt) ->
    start_link(Dev, Opt).

close(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, close).


getfd(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, fd).


read(FD, Len) when is_integer(Len) ->
    serctl:read(FD, Len).

write(FD, Data) when is_binary(Data) ->
    serctl:write(FD, Data).

send(Ref, Data) when is_pid(Ref) ->
    gen_server:call(Ref, {send, Data}).

% FIXME: race condition: events can be delivered out of order
controlling_process(Ref, Pid) when is_pid(Ref), is_pid(Pid) ->
    flush_events(Ref, Pid),
    gen_server:call(Ref, {controlling_process, Pid}),
    flush_events(Ref, Pid).


start_link(Dev, Opt) ->
    Pid = self(),
    gen_server:start_link(?MODULE, [Pid, Dev, Opt], []).


%%--------------------------------------------------------------------
%%% Callbacks
%%--------------------------------------------------------------------
init([Pid, Dev, Opt]) ->
    process_flag(trap_exit, true),

    Speed = proplists:get_value(speed, Opt, b9600),
    Flow = proplists:get_value(flow, Opt, true),
    PortOpt = proplists:get_value(port_options, Opt, [stream,binary]),

    {ok, FD} = serctl:open(Dev),

    {ok, Orig} = serctl:tcgetattr(FD),

    Mode = case proplists:get_value(mode, Opt, raw) of
        raw -> serctl:mode(raw);
        none -> Orig
    end,

    Termios = lists:foldl(
        fun(Fun, Acc) -> Fun(Acc) end,
        Mode,
        [
            fun(N) -> serctl:flow(N, Flow) end,
            fun(N) -> serctl:ispeed(N, Speed) end,
            fun(N) -> serctl:ospeed(N, Speed) end
        ]),

    ok = serctl:tcsetattr(FD, tcsanow, Termios),

    {ok, #state{
            oattr = Orig,
            port = set_active(FD, PortOpt),
            pid = Pid,
            fd = FD,
            dev = Dev
    }}.


%%
%% retrieve/modify gen_server state
%%
handle_call(devname, _From, #state{dev = Dev} = State) ->
    {reply, Dev, State};

handle_call(fd, _From, #state{fd = FD} = State) ->
    {reply, FD, State};

handle_call({send, Data}, _From, #state{port = Port} = State) ->
    Reply = try erlang:port_command(Port, Data) of
        true -> ok
        catch
            error:Error -> {error, Error}
        end,
    {reply, Reply, State};

handle_call(close, _From, State) ->
    {stop, normal, ok, State};

handle_call({controlling_process, Pid}, {Owner,_}, #state{pid = Owner} = State) ->
    {reply, ok, State#state{pid = Pid}}.


handle_cast(_Msg, State) ->
    {noreply, State}.

%%
%% {active, true} mode
%%
handle_info({Port, {data, Data}}, #state{port = Port, pid = Pid} = State) ->
    Pid ! {serial, self(), Data},
    {noreply, State};

% WTF?
handle_info(Info, State) ->
    error_logger:error_report([wtf, Info]),
    {noreply, State}.

terminate(_Reason, #state{fd = FD, port = Port, oattr = Orig}) ->
    if
        is_port(Port) -> catch erlang:port_close(Port);
        true -> ok
    end,
    serctl:tcsetattr(FD, tcsanow, Orig),
    serctl:close(FD),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
set_active(Res, Opt) ->
    FD = serctl:getfd(Res),
    open_port({fd, FD, FD}, Opt).

flush_events(Ref, Pid) ->
    receive
        {serial, Ref, _} = Event ->
            Pid ! Event,
            flush_events(Ref, Pid)
    after
        0 -> ok
    end.
