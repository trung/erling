%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(erling_server).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the erling_server server.
start() ->
    erling_server_deps:ensure(),
    ensure_started(crypto),
    application:start(erling_server).

%% @spec stop() -> ok
%% @doc Stop the erling_server server.
stop() ->
    Res = application:stop(erling_server),
    application:stop(crypto),
    Res.
