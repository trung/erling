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

%% Test setters
abstract_message_set_clientId_test() ->
	NewValue = 2,
	{ok, NewR, _} = record_utils:set(new_record(abstract_message), clientId, NewValue),
	?assert(is_record(NewR, abstract_message)),
	?assert(NewR#abstract_message.clientId =:= NewValue).