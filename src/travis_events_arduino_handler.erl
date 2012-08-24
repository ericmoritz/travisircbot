-module(travis_events_arduino_handler).

-behavior(gen_event).

-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

%% gen_event callbacks
init([DeviceFile]) ->
    Cmd = lists:flatten(["python priv/arduino.py ", DeviceFile]),
    Port = open_port({spawn, Cmd}, [stream]),
    {ok, Port}.

handle_event({status, failed, _}, Port) ->
    port_command(Port, "F"),
    {ok, Port};
handle_event({status, fixed, _}, Port) ->
    port_command(Port, "P"),
    {ok, Port};
handle_event(_, State) ->
    {ok, State}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.
