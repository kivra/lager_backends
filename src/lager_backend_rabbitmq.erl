%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @copyright Bjorn Jensen-Urstad 2012
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(lager_backend_rabbitmq).
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
%%%_* Macros ===========================================================
-define(shaft_params, [ {async,     false}
                      , {mandatory, false}
                      , {immediate, false}
                      , {persist,   true}
                      ]).

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-record(s, { level
           , exchange
           , routing_key
           , pid
           }).

%%%_ * gen_event callbacks ---------------------------------------------
init([Level, Hosts, Username, Password, Exchange, RoutingKey]) ->
  {ok, Pid} = shaft:start_link([{hosts, Hosts},
                                {username, Username},
                                {password, Password}]),
  {ok, #s{level=Level, exchange=Exchange, routing_key=RoutingKey, pid=Pid}}.

terminate(_Rsn, #s{pid=Pid}) ->
  ok = shaft:stop(Pid),
  ok.

handle_call({set_loglevel, Level}, S) ->
  {ok, ok, S#s{level=lager_util:level_to_num(Level)}};
handle_call(get_loglevel, S) ->
  {ok, S#s.level, S}.

handle_event({log, _Dest, Level, {_Date, _Time}, _Msg}, S)
  when Level =< S#s.level -> {ok, S};
handle_event({log, _Dest, Level, {_Date, _Time}, _Msg}, S)
  when Level > S#s.level -> {ok, S};
handle_event({log, Dest, Level, {Date, Time}, Msg}, S) ->
  [publish(Level, Date, Time, Msg, S#s.pid, S#s.exchange, S#s.routing_key) ||
    lists:member({?MODULE, ?MODULE}, Dest)],
  {ok, S};
handle_event({log, Level, {Date, Time}, Msg}, S) ->
  publish(Level, Date, Time, Msg, S#s.pid, S#s.exchange, S#s.routing_key),
  {ok, S}.

handle_info(_Msg, S) ->
  {ok, S}.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.


publish(Level, Date, Time, Msg, Pid, Exchange, RoutingKey) ->
  ok = shaft:publish(Pid, Exchange, RoutingKey, Msg, ?shaft_params).

%%%_ * Types -----------------------------------------------------------
%%%_ * API -------------------------------------------------------------
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
