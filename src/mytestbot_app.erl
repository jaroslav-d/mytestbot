%%%-------------------------------------------------------------------
%% @doc mytestbot public API
%% @end
%%%-------------------------------------------------------------------

-module(mytestbot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  ssl:start(),
%%  mnesia:start(),
%%  mytestbot_sup:start_link().
  mnesia:start().

stop(_State) ->
  mnesia:stop(),
  ssl:stop(),
  ok.

%% internal functions
