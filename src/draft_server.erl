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
-record(s, {backends, pid}).

%%%_ * API -------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

log(S, F, A, Info) ->
  gen_server:call(?MODULE, {log, S, F, A, Info}).

%%%_ * gen_server callbacks --------------------------------------------
init([]) ->
  {ok, Pid}      = gen_event:start_link(),
  {ok, Handlers} = application:get_env(draft, backends),
  lists:foreach(fun({Backend, Args}) ->
                    gen_event:add_sup_handler(Pid, Backend, Args)
                end, Handlers),
  {ok, #s{backends=Backends, pid=Pid}}.

terminate(_Rsn, #s{pid=Pid, backends=Handlers}) ->
  lists:foreach(fun({Backend,Args}) ->
                    gen_event:delete_handler(Pid, Backend, [])
                end, Backends),
  ok = gen_event:stop(Pid).

handle_call({log, Severity, Msg, Info}, From, S) ->
  F = fun() ->
          gen_event:sync_notify(S#s.pid, {log, Severity, Msg, Info}),
          gen_server:reply(From, ok)
      end,
  erlang:spawn_link(F),
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
