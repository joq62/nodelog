%%%-------------------------------------------------------------------
%% @doc org public API
%% @end
%%%-------------------------------------------------------------------

-module(nodelog_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    nodelog_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
