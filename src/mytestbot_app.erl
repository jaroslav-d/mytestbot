%%%-------------------------------------------------------------------
%% @doc mytestbot public API
%% @end
%%%-------------------------------------------------------------------

-module(mytestbot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  ssl:start(),
  mnesia:start(),
%  manager:start(),
  mytestbot_sup:start_link().

stop(_State) ->
  mnesia:start(),
  ssl:stop(),
  ok.

%% internal functions
