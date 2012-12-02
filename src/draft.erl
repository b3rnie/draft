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
-export([call/4]). %parse transform

%%%_* Code =============================================================
call(S, F, A, Info) ->
  io:format("S: ~p~n~F: ~p~nA: ~p~nInfo: ~p~n", [S, F, A, Info]),
  draft_server:log(S, io_lib:format(F, A),
                   [{node, erlang:node()},
                    {pid,erlang:self()},
                    {now, erlang:now()}|Info]).

%%%_ * Internal --------------------------------------------------------
%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

draft_test() ->
  ok = application:start(draft),
  draft:debug("foo"),
  ?debug("foo ~s~n", ["bar"]),
  draft:info("foo"),
  draft:notice("foo"),
  draft:warning("foo"),
  draft:error("foo"),
  draft:critical("foo"),
  draft:alert("foo"),
  draft:emergency("foo"),
  application:stop(draft),
  ok.

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
