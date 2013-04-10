%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2012-2014 Kivra
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc Email backend for RabbitMQ
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(lager_backend_rabbitmq).
-behaviour(gen_event).

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([init/1]).
-export([handle_call/2]).
-export([handle_event/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%%_* Macros ===========================================================
-define(shaft_params, [ {async,     false} %synchronous delivery
                      , {mandatory, true}  %routed to atleast one queue
                      , {immediate, false} %no ready consumers needed
                      , {persist,   true}  %persisted to disk
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
  {ok, Pid} = shaft:start_link([{hosts,    Hosts},
                                {username, Username},
                                {password, Password}]),
  {ok, #s{
     level=Level, exchange=Exchange, routing_key=RoutingKey, pid=Pid}}.

terminate(_Rsn, S) ->
  ok = shaft:stop(S#s.pid).

handle_call({set_loglevel, Level}, S) ->
  {ok, ok, S#s{level=lager_util:level_to_num(Level)}};
handle_call(get_loglevel, S) ->
  {ok, S#s.level, S}.

handle_event({log, _Dest, Level, {_Date, _Time}, _Msg}, S)
  when Level =< S#s.level -> {ok, S};
handle_event({log, _Dest, Level, {_Date, _Time}, _Msg}, S)
  when Level > S#s.level -> {ok, S};
handle_event({log, Dest, _Level, {Date, Time}, Msg}, S) ->
  [publish(Date, Time, Msg, S#s.pid, S#s.exchange, S#s.routing_key) ||
    lists:member({?MODULE, ?MODULE}, Dest)],
  {ok, S};
handle_event({log, _Level, {Date, Time}, Msg}, S) ->
  publish(Date, Time, Msg, S#s.pid, S#s.exchange, S#s.routing_key),
  {ok, S}.

handle_info(_Msg, S) ->
  {ok, S}.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

%%%_* Private functions ================================================
publish(Date, Time, [_LevelStr, Location, Msg], Pid, Exchange, RK) ->
  Body = dict:from_list([{node,     erlang:node()},
                         {date,     lists:flatten(Date)},
                         {time,     lists:flatten(Time)},
                         {location, lists:flatten(Location)},
                         {message,  lists:flatten(Msg)}]),
  ok = shaft:publish(Pid, Exchange, RK, Body, ?shaft_params).

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
