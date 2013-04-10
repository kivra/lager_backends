%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @copyright Kivra 2013
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% {lager_backend_email, [info, kivra_core@kivra.com, dev@kivra.com]}
%%

%%%_* Module declaration ===============================================
-module(lager_backend_email).
-behaviour(gen_event).

%%%_* Exports ==========================================================
-export([ init/1
        , handle_call/2
        , handle_event/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%%%_* Includes =========================================================
%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-record(s, { level
           , from
           , to
           }).
%%%_ * gen_event callbacks ---------------------------------------------
init([Level, From, To]) ->
  {ok, #s{level=lager_util:level_to_num(Level), from=From, to=To}}.

terminate(_Rsn, _S) ->
  ok.

handle_call({set_loglevel, Level}, S) ->
  {ok, ok, S#s{level=lager_util:level_to_num(Level)}};
handle_call(get_loglevel, S) ->
  {ok, S#s.level, S}.

handle_event({log, _Dest, Level, {_Date, _Time}, _Msg}, S)
  when Level =< S#s.level -> {ok, S};
handle_event({log, Level, {_Date, _Time}, _Msg}, S)
  when Level > S#s.level -> {ok, S};
handle_event({log, Dest, Level, {Date, Time}, Msg}, S) ->
  [do_log(Level, Date, Time, Msg) ||
    lists:member({?MODULE, ?MODULE}, Dest)],
  {ok, S};
handle_event({log, Level, {Date, Time}, Msg}, S) ->
  do_log(Level, Date, Time, Msg),
  {ok, S}.

handle_info(_Msg, S) ->
  {ok, S}.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

%%%_ * Internals -------------------------------------------------------
do_log(Level, Date, Time, [LevelStr, Location, Msg]) ->
  spawn_link(fun() ->
                 %% [Date, " ", Time, " ", Msg],
                 io:format("Date: ~p~n", [Date]),
                 io:format("Time: ~p~n", [Time]),
                 io:format("Level: ~p~n", [Level]),
                 io:format("Location: ~p~n", [Location]),
                 io:formet("Msg: ~p~n~n", [Msg]),
                 %% email:send(To, From, Subject, Msg)
                 ok
             end).




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
