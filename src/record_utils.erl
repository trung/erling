%% This is auto generated file. Please don't edit it

-module(record_utils).
-author("trung@mdkt.org").
-compile(export_all).
-include("../include/messages.hrl").
-include("../include/types.hrl").

fields(abstract_message) -> 
	["clientId", "destination", "messageId", "timestamp", "timeToLive", "headers", "body"];

fields(async_message) -> 
	fields(abstract_message) ++ ["correlationId", "correlationIdBytes"];

fields(command_message) -> 
	fields(async_message) ++ ["operation"];

fields(remoting_message) -> 
	fields(rpc_message) ++ ["source", "operation", "parameters"];

fields(rpc_message) -> 
	fields(abstract_message) ++ ["remoteUsername", "remotePassword"].

fields_atom(abstract_message) -> 
	lists:flatten([clientId, destination, messageId, timestamp, timeToLive, headers, body]);

fields_atom(async_message) -> 
	lists:flatten([fields_atom(abstract_message), correlationId, correlationIdBytes]);

fields_atom(command_message) -> 
	lists:flatten([fields_atom(async_message), operation]);

fields_atom(remoting_message) -> 
	lists:flatten([fields_atom(rpc_message), source, operation, parameters]);

fields_atom(rpc_message) -> 
	lists:flatten([fields_atom(abstract_message), remoteUsername, remotePassword]).

get(Obj, body) when is_record(Obj, abstract_message) -> 
	{ok, Obj#abstract_message.body};

get(Obj, clientId) when is_record(Obj, abstract_message) -> 
	{ok, Obj#abstract_message.clientId};

get(Obj, destination) when is_record(Obj, abstract_message) -> 
	{ok, Obj#abstract_message.destination};

get(Obj, headers) when is_record(Obj, abstract_message) -> 
	{ok, Obj#abstract_message.headers};

get(Obj, messageId) when is_record(Obj, abstract_message) -> 
	{ok, Obj#abstract_message.messageId};

get(Obj, timeToLive) when is_record(Obj, abstract_message) -> 
	{ok, Obj#abstract_message.timeToLive};

get(Obj, timestamp) when is_record(Obj, abstract_message) -> 
	{ok, Obj#abstract_message.timestamp};

get(Obj, PropertyName) when is_record(Obj, asobject) -> 
	Ret = [X || {P, X} <- Obj#asobject.array, P == PropertyName],
	if
		length(Ret) == 0 -> {bad, {"PropertyName not found in the object", Obj, PropertyName}};
		true -> 
			[Value|_] = Ret,
	{ok, Value}
	end;

get(Obj, correlationId) when is_record(Obj, async_message) -> 
	{ok, Obj#async_message.correlationId};

get(Obj, correlationIdBytes) when is_record(Obj, async_message) -> 
	{ok, Obj#async_message.correlationIdBytes};

get(Obj, parent) when is_record(Obj, async_message) -> 
	{ok, Obj#async_message.parent};

get(Obj, ParentProperty) when is_record(Obj, async_message) and is_atom(ParentProperty) -> 
	get(Obj#async_message.parent, ParentProperty);

get(Obj, operation) when is_record(Obj, command_message) -> 
	{ok, Obj#command_message.operation};

get(Obj, parent) when is_record(Obj, command_message) -> 
	{ok, Obj#command_message.parent};

get(Obj, ParentProperty) when is_record(Obj, command_message) and is_atom(ParentProperty) -> 
	get(Obj#command_message.parent, ParentProperty);

get(Obj, data) when is_record(Obj, ecma_array) -> 
	{ok, Obj#ecma_array.data};

get(Obj, data) when is_record(Obj, long_string) -> 
	{ok, Obj#long_string.data};

get(Obj, operation) when is_record(Obj, remoting_message) -> 
	{ok, Obj#remoting_message.operation};

get(Obj, parameters) when is_record(Obj, remoting_message) -> 
	{ok, Obj#remoting_message.parameters};

get(Obj, source) when is_record(Obj, remoting_message) -> 
	{ok, Obj#remoting_message.source};

get(Obj, parent) when is_record(Obj, remoting_message) -> 
	{ok, Obj#remoting_message.parent};

get(Obj, ParentProperty) when is_record(Obj, remoting_message) and is_atom(ParentProperty) -> 
	get(Obj#remoting_message.parent, ParentProperty);

get(Obj, remotePassword) when is_record(Obj, rpc_message) -> 
	{ok, Obj#rpc_message.remotePassword};

get(Obj, remoteUsername) when is_record(Obj, rpc_message) -> 
	{ok, Obj#rpc_message.remoteUsername};

get(Obj, parent) when is_record(Obj, rpc_message) -> 
	{ok, Obj#rpc_message.parent};

get(Obj, ParentProperty) when is_record(Obj, rpc_message) and is_atom(ParentProperty) -> 
	get(Obj#rpc_message.parent, ParentProperty);

get(Obj, data) when is_record(Obj, string) -> 
	{ok, Obj#string.data};

get(Obj, data) when is_record(Obj, string_3) -> 
	{ok, Obj#string_3.data};

get(Obj, data) when is_record(Obj, xml) -> 
	{ok, Obj#xml.data}.

set(Obj, body, Value) when is_record(Obj, abstract_message) -> 
	NewObj = Obj#abstract_message{body = Value},
	{ok, NewObj, {body, Value}};

set(Obj, clientId, Value) when is_record(Obj, abstract_message) -> 
	NewObj = Obj#abstract_message{clientId = Value},
	{ok, NewObj, {clientId, Value}};

set(Obj, destination, Value) when is_record(Obj, abstract_message) -> 
	NewObj = Obj#abstract_message{destination = Value},
	{ok, NewObj, {destination, Value}};

set(Obj, headers, Value) when is_record(Obj, abstract_message) -> 
	NewObj = Obj#abstract_message{headers = Value},
	{ok, NewObj, {headers, Value}};

set(Obj, messageId, Value) when is_record(Obj, abstract_message) -> 
	NewObj = Obj#abstract_message{messageId = Value},
	{ok, NewObj, {messageId, Value}};

set(Obj, timeToLive, Value) when is_record(Obj, abstract_message) -> 
	NewObj = Obj#abstract_message{timeToLive = Value},
	{ok, NewObj, {timeToLive, Value}};

set(Obj, timestamp, Value) when is_record(Obj, abstract_message) -> 
	NewObj = Obj#abstract_message{timestamp = Value},
	{ok, NewObj, {timestamp, Value}};

set(Obj, PropertyName, Value) when is_record(Obj, asobject) -> 
	NewObj = #asobject{array = Obj#asobject.array ++ [{PropertyName, Value}]},
	{ok, NewObj, {array, Value}};

set(Obj, correlationId, Value) when is_record(Obj, async_message) -> 
	NewObj = Obj#async_message{correlationId = Value},
	{ok, NewObj, {correlationId, Value}};

set(Obj, correlationIdBytes, Value) when is_record(Obj, async_message) -> 
	NewObj = Obj#async_message{correlationIdBytes = Value},
	{ok, NewObj, {correlationIdBytes, Value}};

set(Obj, parent, Value) when is_record(Obj, async_message) and is_record(Value, abstract_message) -> 
	NewObj = Obj#async_message{parent = Value},
	{ok, NewObj, {parent, Value}};

set(Obj, ParentProperty, Value) when is_record(Obj, async_message) and is_atom(ParentProperty) -> 
	{ok, NewParentObject, _} = set(Obj#async_message.parent, ParentProperty, Value),
	set(Obj, parent, NewParentObject);

set(Obj, operation, Value) when is_record(Obj, command_message) -> 
	NewObj = Obj#command_message{operation = Value},
	{ok, NewObj, {operation, Value}};

set(Obj, parent, Value) when is_record(Obj, command_message) and is_record(Value, async_message) -> 
	NewObj = Obj#command_message{parent = Value},
	{ok, NewObj, {parent, Value}};

set(Obj, ParentProperty, Value) when is_record(Obj, command_message) and is_atom(ParentProperty) -> 
	{ok, NewParentObject, _} = set(Obj#command_message.parent, ParentProperty, Value),
	set(Obj, parent, NewParentObject);

set(Obj, data, Value) when is_record(Obj, ecma_array) -> 
	NewObj = Obj#ecma_array{data = Value},
	{ok, NewObj, {data, Value}};

set(Obj, data, Value) when is_record(Obj, long_string) -> 
	NewObj = Obj#long_string{data = Value},
	{ok, NewObj, {data, Value}};

set(Obj, operation, Value) when is_record(Obj, remoting_message) -> 
	NewObj = Obj#remoting_message{operation = Value},
	{ok, NewObj, {operation, Value}};

set(Obj, parameters, Value) when is_record(Obj, remoting_message) -> 
	NewObj = Obj#remoting_message{parameters = Value},
	{ok, NewObj, {parameters, Value}};

set(Obj, source, Value) when is_record(Obj, remoting_message) -> 
	NewObj = Obj#remoting_message{source = Value},
	{ok, NewObj, {source, Value}};

set(Obj, parent, Value) when is_record(Obj, remoting_message) and is_record(Value, rpc_message) -> 
	NewObj = Obj#remoting_message{parent = Value},
	{ok, NewObj, {parent, Value}};

set(Obj, ParentProperty, Value) when is_record(Obj, remoting_message) and is_atom(ParentProperty) -> 
	{ok, NewParentObject, _} = set(Obj#remoting_message.parent, ParentProperty, Value),
	set(Obj, parent, NewParentObject);

set(Obj, remotePassword, Value) when is_record(Obj, rpc_message) -> 
	NewObj = Obj#rpc_message{remotePassword = Value},
	{ok, NewObj, {remotePassword, Value}};

set(Obj, remoteUsername, Value) when is_record(Obj, rpc_message) -> 
	NewObj = Obj#rpc_message{remoteUsername = Value},
	{ok, NewObj, {remoteUsername, Value}};

set(Obj, parent, Value) when is_record(Obj, rpc_message) and is_record(Value, abstract_message) -> 
	NewObj = Obj#rpc_message{parent = Value},
	{ok, NewObj, {parent, Value}};

set(Obj, ParentProperty, Value) when is_record(Obj, rpc_message) and is_atom(ParentProperty) -> 
	{ok, NewParentObject, _} = set(Obj#rpc_message.parent, ParentProperty, Value),
	set(Obj, parent, NewParentObject);

set(Obj, data, Value) when is_record(Obj, string) -> 
	NewObj = Obj#string{data = Value},
	{ok, NewObj, {data, Value}};

set(Obj, data, Value) when is_record(Obj, string_3) -> 
	NewObj = Obj#string_3{data = Value},
	{ok, NewObj, {data, Value}};

set(Obj, data, Value) when is_record(Obj, xml) -> 
	NewObj = Obj#xml{data = Value},
	{ok, NewObj, {data, Value}}.

type(Obj) when is_record(Obj, abstract_message) -> abstract_message;

type(Obj) when is_record(Obj, asobject) -> asobject;

type(Obj) when is_record(Obj, async_message) -> async_message;

type(Obj) when is_record(Obj, command_message) -> command_message;

type(Obj) when is_record(Obj, ecma_array) -> ecma_array;

type(Obj) when is_record(Obj, long_string) -> long_string;

type(Obj) when is_record(Obj, remoting_message) -> remoting_message;

type(Obj) when is_record(Obj, rpc_message) -> rpc_message;

type(Obj) when is_record(Obj, string) -> string;

type(Obj) when is_record(Obj, string_3) -> string_3;

type(Obj) when is_record(Obj, xml) -> xml;

type(_) -> undefined.

