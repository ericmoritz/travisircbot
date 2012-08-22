-module(reluxbot_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(eirc),
    ReluxBot = {reluxbot, reluxbot, ["reluxbot"]},

    {ok, Client} = eirc:start_client(?MODULE, [{bots, [ReluxBot]}]),
    eirc:connect_and_logon(Client, "irc.freenode.net", 6667, "reluxbot"),

    % install the event handlers
    case reluxbot_sup:start_link() of 
        {ok, Pid} ->
            ok = travis_events:add_handler(travis_events_echo_handler, []),
            ok = travis_events:add_handler(travis_events_say_handler, []),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
