-module(audio).

-export([say/1, clap/0, drama/0]).

say(Msg) ->
    os:cmd(["echo ", $", Msg, $", " | say"]),
    ok.

clap() ->
    os:cmd("afplay priv/clap.mp3"),
    ok.

drama() ->
    os:cmd("afplay priv/drama.mp3"),
    ok.
