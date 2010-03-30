%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the erling_server application.

-module(erling_server_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for erling_server.
start(_Type, _StartArgs) ->
    erling_server_deps:ensure(),
    erling_server_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for erling_server.
stop(_State) ->
    ok.
