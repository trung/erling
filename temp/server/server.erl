-module(server).
-export([start/0, stop/0, restart/0, do/1]).

-record(init_data,{peername,resolve}).
-record(mod,{init_data,
	     data=[],
	     socket_type=ip_comm,
	     socket,
	     config_db,
	     method,
	     absolute_uri=[],
	     request_uri,
	     http_version,
	     request_line,
	     parsed_header=[],
	     entity_body,
	     connection}).


start() ->
    inets:start(),
    inets:start(
      httpd,
      [{port,8001},
       {server_name,"httpd_test"},
       {bind_address,
	"localhost"},
       {server_root,"."},
       {document_root,"."},
       {modules, [?MODULE]}]).

do(Info) ->
    parse(Info),
    {proceed,
     [{response,
       {200,
	[term_to_binary(Info)]}}
      ]}.

stop() ->
    inets:stop().

restart() ->
    stop(),
    start().

%% Parsing html request
parse(Info) ->
    %% Byte array
    _Data = Info#mod.entity_body,
    io:fwrite("Received: ~p~n", [Info]),
    %% Print byte array in hex
    ok.

    
