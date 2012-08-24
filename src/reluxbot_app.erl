-module(reluxbot_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(eirc),

    % install the event handlers
    case reluxbot_sup:start_link() of 
        {ok, Pid} ->
            % Install our handlers
            ok = travis_events:add_handler(travis_events_echo_handler, []),
            ok = travis_events:add_handler(travis_events_say_handler, []),
            ok = travis_events:add_handler(travis_events_drama_handler, []),
            ok = travis_events:add_handler(travis_events_arduino_handler, []),

            % Log into IRC
            ReluxBot = {reluxbot, reluxbot, ["reluxbot"]},
            {ok, Client} = eirc:start_client(?MODULE, [{bots, [ReluxBot]}]),
            register(irc_client, Client),
            eirc:connect_and_logon(Client, "irc.freenode.net", 6667, "reluxbot"),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
