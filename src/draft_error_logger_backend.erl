%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @copyright 2012 Bjorn Jensen-Urstad
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(draft_error_logger_backend).
-behaviour(gen_server).

%%%_* Exports ==========================================================
-export([ init/1
        , terminate/2
        , handle_call/2
        , handle_event/2
        , handle_info/2
        , code_change/3
        ]).

%%%_* Includes =========================================================
-include_lib("draft/include/draft.hrl").

%%%_* Macros ===========================================================

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-record(s, {}).


%%%_ * gen_event callbacks ---------------------------------------------
init([]) ->
  {ok, #s{}}.

terminate(_Rsn, S) ->
  ok.

handle_call(sync, S) ->
  {ok, ok, S}.

handle_event({Severity, Format, Args, Info}, S) ->
  {Fun, Msg} = severity_to_function(Severity),
  {ok, Pid}  = assoc(pid, Info),
  {ok, Line} = assoc(line, Info),
  {ok, Mod}  = assoc(module, Info),
  error_logger:Fun(lists:flatten(["~p:~s:~p: ", Msg, ": ", Format, "~n"]),
                                  [Pid, Mod, Line|Args]),
  {ok, S}.

handle_info(Msg, S) ->
  draft:warning("~p", [Msg]),
  {ok, S}.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

%%%_ * Internals -------------------------------------------------------
severity_to_function(debug)     -> {info_msg,    "Debug"};
severity_to_function(info)      -> {info_msg,    "Info"};
severity_to_function(notice)    -> {info_msg,    "Notice"};
severity_to_function(warning)   -> {warning_msg, "Warning"};
severity_to_function(error)     -> {warning_msg, "Error"};
severity_to_function(critical)  -> {error_msg,   "Critical"};
severity_to_function(alert)     -> {error_msg,   "Alert"};
severity_to_function(emergency) -> {error_msg,   "Error"}.

assoc(K, L) ->
  case lists:keyfind(K, 1, L) of
    {K, V} -> {ok, V};
    false  -> {error, notfound}
  end.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
