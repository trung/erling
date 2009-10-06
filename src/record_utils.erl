%% This is auto generated file. Please don't edit it

-module(record_utils).
-author("trung@mdkt.org").
-compile(export_all).
-include("../include/messages.hrl").
-include("../include/types.hrl").

fields(abstract_message) -> 
	["clientId", "destination", "messageId", "timestamp", "timeToLive", "headers", "body"];

fields(async_message) -> 
	["parent", "correlationId", "correlationIdBytes"];

fields(command_message) -> 
	["parent", "operation"];

fields(remoting_message) -> 
	["parent", "source", "operation", "parameters"];

fields(rpc_message) -> 
	["parent", "remoteUsername", "remotePassword"].

fields_atom(abstract_message) -> 
	[clientId, destination, messageId, timestamp, timeToLive, headers, body];

fields_atom(async_message) -> 
	[parent, correlationId, correlationIdBytes];

fields_atom(command_message) -> 
	[parent, operation];

fields_atom(remoting_message) -> 
	[parent, source, operation, parameters];

fields_atom(rpc_message) -> 
	[parent, remoteUsername, remotePassword].

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

type(Obj) when is_record(Obj, xml) -> xml.

