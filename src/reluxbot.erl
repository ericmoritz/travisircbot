-module(reluxbot).

-behaviour(gen_eircbot).

-export([start/0, init/2, on_connect/3, on_logon/5, on_logon/1, on_text/4,
	 on_server_notice/3, on_notice/4, on_join/3, on_part/3, on_ctcp/4,
	 on_mode/5, on_topic/4, on_ping/1, on_nick/3, on_raw/3, on_kick/5,
	 on_quit/3, handle_call/3, handle_info/2, terminate/2]).

-compile(export_all).

-record(botstate, { cl, nick }).

-define(VERSION, "EIRC Example Bot 0.2").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% =============================================================================
%% Module API
%% =============================================================================

%% =============================================================================
%% Callbacks
%% =============================================================================
%% This is called to initialise a state and is before a connections is opened to
%% the server
start() ->
    application:start(reluxbot).

init(Client, _Args) ->
    process_flag(trap_exit, true),
    io:format("Initiating bot...~n"),
    {ok, #botstate{ cl = Client }}.

%% Triggers when the bot successfully established a connection to the IRC server
on_connect(IpHost, Port, State) ->
    io:format("Connected to ~p:~p~n", [IpHost, Port]),
    {ok, State}.

%% (note: 5 args) Triggers when the client sends the nick user pass arguments to
%% the server
on_logon(_Pass, Nick, _User, _Name, State) ->
    io:format("Logging in as ~p~n", [Nick]),
    {ok, State#botstate{ nick = Nick }}.

%% (note: 1 arg) Triggers when the bot has successfully logged in (when the
%% clients receives the WELCOME message code from the irc server)
on_logon(State = #botstate{ cl = Client }) ->
    io:format("Login successful~n"),
    eirc:join(Client, "#usat-relux"),
    {ok, State}.

%% Triggered when someone sends a message to the bot. This can be either a user
%% or a channel sending the message
on_text(_, _, "[travis-ci] " ++ Msg, State) ->
    case process_message(Msg) of
        {travis, {status, Status, Props}} ->
            travis_events:status_update(Status, Props);
        _ ->
            pass
    end,
    {ok, State};
on_text(From, To, Text, State) ->
    io:format("TEXT: From (~p) To (~p) - ~p ~n", [From, To, Text]),
    {ok, State}.

%% Triggers when the server sends us a message and not another user
on_server_notice(ServerName, Msg, State) ->
    io:format("NOTICE (~p): ~1000p~n", [ServerName, Msg]),
    {ok, State}.

%% Like on_text but triggers on NOTICE messages instead, sent by a user
on_notice(From, To, Text, State) ->
    io:format("NOTICE: From (~p) To (~p) - ~p ~n", [From, To, Text]),
    {ok, State}.

%% Triggers when someone or the bot joins a channel
on_join(User, Channel, State) ->
    io:format("JOIN: ~p joined ~p~n", [User, Channel]),
    {ok, State}.

%% The opposite of on_join. User could be the bot
on_part(User, Channel, State) ->
    io:format("PART: (~p) ~p parted ~p~n", [State#botstate.nick, User, Channel]),
    {ok, State}.

%% Triggers when a CTCP request is sent to the bot. If these callbacks are *not*
%% implemented then the behaviour will reply with standard responses for the
%% VERSION, TIME and PING CTCP Requests.
on_ctcp(User, "VERSION", _Args, State) ->
    %% The VERSION requests needs a reply thats starts with "VERSION " and then
    %% Any string to tell the version. This "overrides" the behaviours response
    eirc:ctcp(State#botstate.cl, User, "VERSION "++?VERSION),
    {ok, State};
on_ctcp(User, Cmd, Args, State) ->
    io:format("CTCP: ~p:~p - ~p~n", [User, Cmd, Args]),
    {ok, State}.

%% Triggers when the Server or a User sets a mode on either a User or a
%% Channel. It is up to this callback to make sense of the modes. The important
%% modes are +/-b for ban, +/-v for voice and +/-o for operatior.
on_mode(ServerOrNick, TargetChanOrNick, ModeFlags, ModeParameters, State) ->
    io:format("MODE: ~p sets ~p on ~p (parameters: ~p)~n", 
	      [ServerOrNick, ModeFlags, TargetChanOrNick, ModeParameters]),
    {ok, State}.

%% Triggers when a topic is set in a channel that the bot is in
on_topic(Nick, Channel, Topic, State) ->
    io:format("TOPIC: (~p) ~p set topic to: ~p~n", [Channel, Nick, Topic]),
    {ok, State}.

%% Triggers when the servers sends a PING. If the autoping has been set to off
%% then the bot has to reply to the ping here otherwise the client will be
%% kicked out
on_ping(State) ->
    io:format("PING? PONG!~n"),
    {ok, State}.

%% Triggers when a user is kicked from a channel, User could be the bot
on_kick(User, Channel, TargetUser, Reason, State) ->
    io:format("KICK: ~p kicked ~p from ~p, reason: ~p~n", [User, TargetUser, Channel, Reason]),
    {ok, State}.

%% Triggers when someone (that the bot can "see" in a channel) changes nick, The
%% user changing nick could be the bot
on_nick(OldNick, NewNick, State) ->
    io:format("NICK: ~p is now known as ~p~n",[OldNick, NewNick]),
    {ok, State}.

%% Triggers when someone (that the bot can "see" in a channel) quits from the
%% server. The user quitting could be the bot
on_quit(Nick, QuitMsg, State) ->
    io:format("QUIT: ~p quit (~1000p)~n",[Nick, QuitMsg]),
    {ok, State}.

%% Any command that is not picked up by the behaviour can be received using this
%% callback. This callback will be triggered for every event the server sends
on_raw(Cmd, Args, State) ->
    io:format("RAW: ~p; ~1000p~n",[Cmd, Args]),
    {ok, State}.

handle_call(Call, _From, State) ->
    io:format("Unknown call: ~p ~n", [Call]),
    {reply, ok, State}.

handle_info(Msg, State) ->
    io:format("Unknown message: ~p ~n", [Msg]),
    {ok, State}.

terminate(Reason, _State) ->
    io:format("Bot terminating ~p...~n", [Reason]),
    ok.

%% Internal
process_message(Msg) ->
    RE = "(.+)#(.+) \\((.+) - (.+) : (.+)\\): The build (.+).$",
    % this comment fixes by Emacs syntax hilighting " 

    case re:run(Msg, RE, [ungreedy]) of
        {match, [_FullRange|Ranges]} ->
            [Repo, Build, Branch, Commit, Author, Status]
                = lists:map(fun({S, Len}) ->
                                    string:substr(Msg, S+1, Len)
                            end, Ranges),
            StatusAtom = list_to_atom(Status),
            StatusProps = [{repo, Repo},
                           {build, Build},
                           {branch, Branch},
                           {commit, Commit},
                           {author, Author}],
                      
            {travis, {status, StatusAtom, StatusProps}};
        nomatch ->

            nomatch
    end.

-ifdef(TEST).
process_message_test_() ->
    [
     ?_assertEqual({travis, {status, passed, [
                             {repo, "GannettDigital/django-relux"},
                             {build, "947"},
                             {branch, "master"},
                             {commit, "ea69c97"},
                             {author, "Jay Merrifield"}
                            ]}},
                   process_message("GannettDigital/django-relux#947 (master - ea69c97 : Jay Merrifield): The build passed.")
                   )
     ].
-endif.
