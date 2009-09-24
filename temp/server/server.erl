-module(server).
-export([start/0, stop/0, do/1]).

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
    io:fwrite("Received: ~p~n", [Info]),
    {proceed,
     [{response,
       {200,
	[term_to_binary(Info)]}}
      ]}.

stop() ->
    inets:stop().
