%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @copyright 2012 Bjorn Jensen-Urstad
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(draft_server).
-behaviour(gen_server).

%%%_* Exports ==========================================================
-export([ start_link/0
        , log/4
        ]).

-export([ init/1
        , terminate/2
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , code_change/3
        ]).

%%%_* Includes =========================================================
-include_lib("draft/include/draft.hrl").

%%%_* Macros ===========================================================

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-record(s, {handlers, pid}).

%%%_ * API -------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

log(S, F, A, Info) ->
  gen_server:call(?MODULE, {log, S, F, A, Info}).

%%%_ * gen_server callbacks --------------------------------------------
init(Args) ->
  {ok, Pid}      = gen_event:start_link(),
  {ok, Handlers} = application:get_env(draft, handlers),
  lists:foreach(fun(Handler) ->
                        gen_event:add_sup_handler(Pid, Handler, [])
                end, Handlers),
  {ok, #s{handlers=Handlers, pid=Pid}}.

terminate(_Rsn, #s{pid=Pid, handlers=Handlers}) ->
  lists:foreach(fun(Handler) ->
                        gen_event:delete_handler(Pid, Handler, [])
                end, Handlers),
  ok = gen_event:stop(Pid).

handle_call({log, Severity, F, A, Info}, From, S) ->
  erlang:spawn_link(fun() ->
                        gen_event:sync_notify(S#s.pid, {log, Severity,
                                                        F, A, Info}),
                        gen_server:reply(From, ok)
                    end),
  {noreply, S};
handle_call(stop, _From, S) ->
  {stop, normal, ok, S}.

handle_cast(_, S) ->
  {stop, bad_cast, S}.

handle_info({gen_event_EXIT, Handler, Rsn}, #s{handlers=Handlers} = S) ->
  %% handler died, make supervisor restart everything
  %% ?hence(lists:member(Handler, Handlers)),
  {stop, Rsn, S};
handle_info(Msg, S) ->
  draft:warning("~p", [Msg]),
  {noreply, S}.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

%%%_ * Internals -------------------------------------------------------
assoc(K, L, D) ->
  case lists:keyfind(K, 1, L) of
    {K, V} -> V;
    false  -> D
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
