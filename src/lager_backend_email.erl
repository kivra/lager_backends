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
%%% @doc Email backend for Lager
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(lager_backend_email).
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
-define(messages_per_minute, 120).

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-record(s, { level
           , from
           , to
           , rate
           , host
           }).

%%%_ * gen_event callbacks ---------------------------------------------
init([Level, From, To]) ->
  {ok, Hostname} = inet:gethostname(),
  {ok, #s{level=lager_util:level_to_num(Level),
          from=From,
          to=To,
          rate=rate_limit_new(?messages_per_minute),
          host=Hostname}}.

terminate(_Rsn, _S) ->
  ok.

handle_call({set_loglevel, Level}, S) ->
  {ok, ok, S#s{level=lager_util:level_to_num(Level)}};
handle_call(get_loglevel, S) ->
  {ok, S#s.level, S}.

%% lager 2.x
handle_event({log, Msg}, S) ->
  case lager_util:is_loggable(Msg, S#s.level, {?MODULE, ?MODULE}) of
    true  -> {ok, maybe_send(S, Msg)};
    false -> {ok, S}
  end.

handle_info(_Msg, S) ->
  {ok, S}.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

%%%_* Private functions ================================================
maybe_send(S, Msg) ->
  S#s{rate=rate_limit_maybe(
             S#s.rate, fun() -> do_send(Msg, S#s.from, S#s.to, S#s.host) end)}.

do_send(Msg, From, To, Host) ->
  %% NOTE: the gen_event is running in the same process as the
  %% event_manager (and every other backend). linking risks taking
  %% everything down.
  erlang:spawn(
    fun() ->
        {Date, Time} = lager_msg:datetime(Msg),
        Severity     = lager_msg:severity(Msg),
        LevelStr     = [$[, atom_to_list(Severity), $], $ ],
        Message      = lager_default_formatter:format(Msg, [message]),
        Metadata     = lager_msg:metadata(Msg),
        Location     = s2_lists:assoc(Metadata, pid, "undefined"),
        Subject0     = lists:flatten(string:strip(LevelStr ++ Message)),
        Subject      = string:substr(Subject0, 1, 128),
        Body         = iolist_to_binary(
                         ["Node: ", atom_to_list(erlang:node()), "\n",
                          "Host: ", Host, "\n",
                          "Date: ", Date, "\n",
                          "Time: ", Time, "\n",
                          "Location: ", io_lib:format("~p~n", [Location]),
                          "Message: ", Message
                         ]),
        email:send(To, From, Subject, Body)
    end).

rate_limit_new(Limit) ->
  {calendar:local_time(), Limit, 0}.

rate_limit_maybe({{{Y,M,D},{Hr,Min,_}} = Prev, Limit, N}, Fun) ->
  case calendar:local_time() of
    {{Y,M,D},{Hr,Min,_}} when N >= Limit -> {Prev, Limit, N};
    {{Y,M,D},{Hr,Min,_}}                 -> Fun(), {Prev, Limit, N+1};
    New                                  -> Fun(), {New, Limit, 1}
  end.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
  ok = application:load(lager),
  ok = application:set_env(lager,
                           handlers,
                           [{lager_backend_email,
                             [info, "foo@bar", "buz@baz"]}]),
  ok = application:set_env(lager, error_logger_redirect, false),
  ok = application:start(lager),
  ok = lager:log(debug, self(), "Test DEBUG message"),
  ok = lager:log(info, self(), "Test INFO message"),
  ok = lager:log(error, self(), "Test ERROR message"),
  ok = lager:set_loglevel(?MODULE, warning),
  warning = lager:get_loglevel(?MODULE),
  ok = lager:log(info, self(), "Test INFO message"),
  ok = lager:log(critical, self(), "Test CRITICAL message"),
  ok.

rate_test() ->
  Ref   = make_ref(),
  Daddy = self(),
  lists:foldl(fun(N, R) ->
                  rate_limit_maybe(R, fun() -> Daddy ! {Ref, N} end)
              end, rate_limit_new(10), lists:seq(1, 30)),
  %% 10 calls must always succeed, more than 20 can never.
  lists:foreach(fun(N) when N =< 10 ->
                    receive {Ref, N} -> ok end;
                   (N) ->
                    receive
                      {Ref, N} -> ?assert(N =< 20)
                    after 0 -> ok
                    end
                end, lists:seq(1, 30)).
-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
