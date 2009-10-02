-module(ref_table).
-author("trung@mdkt.org").

-compile(export_all).

%% TODO make sure error handlings are in place

%% delete all objects
clear(TableName) ->
    try ets:delete_all_objects(TableName) of
	true ->
	    _ = ets:insert(TableName, {counter, 0}),
	    {ok}
    catch
	error:Error ->
	    {bad, Error}
    end.

%% Make sure the table exist, if not, create new one
sure_exist(TableName) ->
    case ets:info(TableName) of
	undefined ->
	    ets:new(TableName, [named_table]),
	    _ = ets:insert(TableName, {counter, 0}),
	    {ok, created};
	_ ->
	    {ok, exists}
    end.

%% wrapper of ets:insert, just make sure the table exists
insert(TableName, Obj) ->
    _ = sure_exist(TableName),
    {ok, Count} = read(TableName, counter),
    io:fwrite("Inserting into ~p - index: ~p - value: ~p ~n", [TableName, Count, Obj]),
    _ = ets:insert(TableName, {Count, Obj}),
    _ = ets:insert(TableName, {counter, Count+1}),
    {ok, inserted, Count}.

%% wrapper of ets:lookup
read(TableName, Ref) ->
%%    io:fwrite("Read from ~p with index ~p got value ", [TableName, Ref]),
    case ets:lookup(TableName, Ref) of
	[] ->
	    {bad, {not_found, TableName, Ref}};
	[{counter, Count}|_] ->
	    {ok, Count};
	[{_Idx, Obj}|_] ->
%%	    io:fwrite("~p~n", [Obj]),
	    {ok, Obj}
    end.
