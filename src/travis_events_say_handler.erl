-module(travis_events_say_handler).

-behavior(gen_event).

-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

%% gen_event callbacks
init([]) ->
    {ok, state}.

handle_event({status, failed, StatusProps}, State) ->
    Author = proplists:get_value(author, StatusProps),
    os:cmd(["echo ", $", Author, " broke the build", $", " | say"]),
    {ok, State};
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
