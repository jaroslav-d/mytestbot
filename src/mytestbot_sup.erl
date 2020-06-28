%%%-------------------------------------------------------------------
%% @doc mytestbot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mytestbot_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  SupFlags = #{strategy => one_for_all,
               intensity => 0,
               period => 1},
  ChildSpecs = [
    #{id => manager, 
      start => {manager, start_link, []}, 
      restart => permanent,
      shutdown => brutal_kill
    },
    #{id => worker_sup, 
      start => {worker_sup, start_link, []}, 
      restart => transient,
      type => supervisor
    },
    #{id => base_manager,
      start => {base_manager, start_link, []},
      restart => permanent,
      shutdown => brutal_kill
    }
  ],
%  ChildSpecs = [],
  {ok, {SupFlags, ChildSpecs}}.
%% 
%% internal functions
