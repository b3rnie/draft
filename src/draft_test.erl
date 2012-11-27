-module(draft_test).
-export([test/0]).

-compile([{parse_transform, draft_transform}]).


test() ->

    %% {draft:debug("foo"), 1234},
    Pid = self(),
    draft:warning("foobar~p~n",
                  [arg2,
                   arg4]),
    draft:critical("xxxxx", []),
    blargh(),
    ok.

blargh() ->
    test().


