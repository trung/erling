-module (record_utils_test).
-author("trung@mdkt.org").

-include_lib("eunit/include/eunit.hrl").
-include("../include/messages.hrl").

new_record(abstract_message) ->
	#abstract_message{
		clientId = 1, 
		destination = "dest", 
		messageId = "MSG-ID", 
		timestamp = 1000, 
		timeToLive = 2000, 
		headers = [{someheader}], 
		body = "body"
	};
new_record(rpc_message) ->
	#rpc_message{
		parent = new_record(abstract_message),
		remoteUsername = "userName",
		remotePassword = "password1234"
	};
new_record(remoting_message) ->
	#remoting_message{
		parent = new_record(rpc_message),
		source = "source",
		operation = "oper",
		parameters = ["1", "2"]
	};
new_record(async_message) ->
	#async_message{
		parent = new_record(abstract_message),
		correlationId = "1234",
		correlationIdBytes = [12,12,12]
	};
new_record(command_message) ->
	#command_message{
		parent = new_record(async_message),
		operation = ?UNKNOWN_OPERATION
	}.

abstract_message_set_clientId_test() ->
	NewValue = 2,
	{ok, NewR, _} = record_utils:set(new_record(abstract_message), clientId, NewValue),
	?assert(is_record(NewR, abstract_message)),
	?assert(NewR#abstract_message.clientId =:= NewValue).
	
abstract_message_set_destination_test() ->
	NewValue = "new dest",
	{ok, NewR, _} = record_utils:set(new_record(abstract_message), destination, NewValue),
	?assert(is_record(NewR, abstract_message)),
	?assert(NewR#abstract_message.destination =:= NewValue).
	
abstract_message_set_messageId_test() ->
	NewValue = "some value",
	{ok, NewR, _} = record_utils:set(new_record(abstract_message), messageId, NewValue),
	?assert(is_record(NewR, abstract_message)),
	?assert(NewR#abstract_message.messageId =:= NewValue).

abstract_message_set_timestamp_test() ->
	NewValue = 9,
	{ok, NewR, _} = record_utils:set(new_record(abstract_message), timestamp, NewValue),
	?assert(is_record(NewR, abstract_message)),
	?assert(NewR#abstract_message.timestamp =:= NewValue).

abstract_message_set_timeToLive_test() ->
	NewValue = 9,
	{ok, NewR, _} = record_utils:set(new_record(abstract_message), timeToLive, NewValue),
	?assert(is_record(NewR, abstract_message)),
	?assert(NewR#abstract_message.timeToLive =:= NewValue).

abstract_message_set_headers_test() ->
	NewValue = [{newheader1}, {newheader2}],
	{ok, NewR, _} = record_utils:set(new_record(abstract_message), headers, NewValue),
	?assert(is_record(NewR, abstract_message)),
	?assert(NewR#abstract_message.headers =:= NewValue).

abstract_message_set_body_test() ->
	NewValue = ["some body"],
	{ok, NewR, _} = record_utils:set(new_record(abstract_message), body, NewValue),
	?assert(is_record(NewR, abstract_message)),
	?assert(NewR#abstract_message.body =:= NewValue).


