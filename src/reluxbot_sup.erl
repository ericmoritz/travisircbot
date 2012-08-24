
-module(reluxbot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    EventChild = ?CHILD(travis_events, worker, []),

    Children = case application:get_env(arduino) of
                   {ok, DeviceFile} ->
                       ArduinoChild = ?CHILD(arduino_device, 
                                             worker, [DeviceFile]),
                       [EventChild, ArduinoChild];
                   undefined ->
                       [EventChild]
               end,
    
    {ok, {{one_for_one, 5, 10}, Children}}.

