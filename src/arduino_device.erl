%%%-------------------------------------------------------------------
%%% File    : arduino_device.erl
%%% Author  : Moritz <emoritz@gci-esansone>
%%% Description : 
%%%
%%% Created : 24 Aug 2012 by Moritz <emoritz@gci-esansone>
%%%-------------------------------------------------------------------
-module(arduino_device).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/1, pass/0, fail/0]).

%% gen_server callbacks

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {file, port}).
-define(SERVER, ?MODULE).

%%====================================================================
%% External functions
%%====================================================================
start_link(DeviceFile) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [DeviceFile], []).

pass() ->
    gen_server:cast(?SERVER, pass).

fail() ->
    gen_server:cast(?SERVER, fail).

%%====================================================================
%% Server functions
%%====================================================================
init([DeviceFile]) ->
    Port = start_port(DeviceFile),
    {ok, #state{file=DeviceFile, port=Port}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(pass, State) ->
    port_command(State#state.port, "P"),
    {noreply, State};
handle_cast(fail, State) ->
    port_command(State#state.port, "F"),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    % reload the port
    port_close(State#state.port),
    Port = start_port(State#state.file),
    {ok, State#state{port=Port}}.

%% Internal
start_port(DeviceFile) ->
    Cmd = lists:flatten(["python priv/arduino.py ", DeviceFile]),
    open_port({spawn, Cmd}, [stream]).

    
