%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @copyright Bjorn Jensen-Urstad 2012
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(draft).
-compile([{parse_transform, draft_transform}]).

-include_lib("draft/include/draft.hrl").

%%%_* Exports ==========================================================
-export([ debug/2
        , info/2
        , notice/2
        , warning/2
        , error/2
        , critical/2
        , alert/2
        , emergency/2
        ]).

-export([draft_call/4]).

%%%_* Code =============================================================
debug(F, A)     -> draft_call(?debug,     F, A, []).
info(F, A)      -> draft_call(?info,      F, A, []).
notice(F, A)    -> draft_call(?notice,    F, A, []).
warning(F, A)   -> draft_call(?warning,   F, A, []).
error(F, A)     -> draft_call(?error,     F, A, []).
critical(F, A)  -> draft_call(?critical,  F, A, []).
alert(F, A)     -> draft_call(?alert,     F, A, []).
emergency(F, A) -> draft_call(?emergency, F, A, []).

draft_call(S, F, A, Info) ->
  io:format("S: ~p~n~F: ~p~nA: ~p~nInfo: ~p~n", [S, F, A, Info]),
  draft_server:log(S, F, A, Info).

%%%_ * Internal --------------------------------------------------------
%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

draft_test() ->
    draft:debug("foo"),
    draft:info("foo"),
    draft:notice("foo"),
    draft:warning("foo"),
    draft:error("foo"),
    draft:critical("foo"),
    draft:alert("foo"),
    draft:emergency("foo"),
    ok.

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
