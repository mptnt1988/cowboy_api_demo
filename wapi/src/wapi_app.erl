%%%-------------------------------------------------------------------
%% @doc wapi public API
%% @end
%%%-------------------------------------------------------------------

-module(wapi_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Routes = define_routes(),
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    {ok, _} = cowboy:start_clear(wapi_name,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}),
    wapi_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
define_routes() ->
    [{"/api", wapi_handler, []}].
