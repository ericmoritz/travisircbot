-module(travis_events).

-behavior(gen_event).

-export([start_link/0,
         add_handler/2,
         status_update/2]).

-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

%% API functions

start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

status_update(Status, StatusProps) ->
    gen_event:notify(?MODULE, {status, Status, StatusProps}).

%% gen_event callbacks
init([]) ->
    {ok, state}.

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
