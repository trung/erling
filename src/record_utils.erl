-module(record_utils).
-author("trung@mdkt.org").
-compile(export_all).

-include("messages.hrl").
-include("flex_classes.hrl").

%% Setters for records defined in messages.hrl file
%% return {ok, NewObject, {propertyName, NewValue}}
set(Obj, clientId, Value) when is_record(Obj, abstract_message) ->
    NewObj = Obj#abstract_message{clientId = Value},
    {ok, NewObj, {clientId, Value}};
set(Obj, destination, Value) when is_record(Obj, abstract_message) ->
    NewObj = Obj#abstract_message{destination = Value},
    {ok, NewObj, {destination, Value}};
set(Obj, messageId, Value) when is_record(Obj, abstract_message) ->
    NewObj = Obj#abstract_message{messageId = Value},
    {ok, NewObj, {messageId, Value}};
set(Obj, timestamp, Value) when is_record(Obj, abstract_message) ->
    NewObj = Obj#abstract_message{timestamp = Value},
    {ok, NewObj, {timestamp, Value}};
set(Obj, timeToLive, Value) when is_record(Obj, abstract_message) ->
    NewObj = Obj#abstract_message{timeToLive = Value},
    {ok, NewObj, {timeToLive, Value}};
set(Obj, headers, Value) when is_record(Obj, abstract_message) ->
    NewObj = Obj#abstract_message{headers = Value},
    {ok, NewObj, {headers, Value}};
set(Obj, body, Value) when is_record(Obj, abstract_message) ->
    NewObj = Obj#abstract_message{body = Value},
    {ok, NewObj, {body, Value}};

set(Obj, remoteUsername, Value) when is_record(Obj, rpc_message) ->
    NewObj = Obj#rpc_message{remoteUsername = Value},
    {ok, NewObj, {remoteUsername, Value}};
set(Obj, remotePassword, Value) when is_record(Obj, rpc_message) ->
    NewObj = Obj#rpc_message{remotePassword = Value},
    {ok, NewObj, {remotePassword, Value}};
set(Obj, parent, Value) when is_record(Obj, rpc_message) and is_record(Value, abstract_message) ->
    NewObj = Obj#rpc_message{parent = Value},
    {ok, NewObj, {parent, Value}};
set(Obj, AbstractMessageProperty, Value) when is_record(Obj, rpc_message) and is_atom(AbstractMessageProperty) ->
    {ok, NewAbstractMessage, _} = set(Obj#rpc_message.parent, AbstractMessageProperty, Value),
    set(Obj, parent, NewAbstractMessage);

set(Obj, parent, Value) when is_record(Obj, remoting_message) and is_record(Value, rpc_message) ->
    NewObj = Obj#remoting_message{parent = Value},
    {ok, NewObj, {parent, Value}};
set(Obj, source, Value) when is_record(Obj, remoting_message) ->
    NewObj = Obj#remoting_message{source = Value},
    {ok, NewObj, {source, Value}};
set(Obj, operation, Value) when is_record(Obj, remoting_message) ->
    NewObj = Obj#remoting_message{operation = Value},
    {ok, NewObj, {operation, Value}};
set(Obj, parameters, Value) when is_record(Obj, remoting_message) ->
    NewObj = Obj#remoting_message{parameters = Value},
    {ok, NewObj, {parameters, Value}};
set(Obj, RpcMessageProperty, Value) when is_record(Obj, remoting_message) and is_atom(RpcMessageProperty) ->
    {ok, NewRpcMessage, _} = set(Obj#remoting_message.parent, RpcMessageProperty, Value),
    set(Obj, parent, NewRpcMessage);

set(Obj, parent, Value) when is_record(Obj, async_message) and is_record(Value, abstract_message) ->
    NewObj = Obj#async_message{parent = Value},
    {ok, NewObj, {parent, Value}};
set(Obj, correlationId, Value) when is_record(Obj, async_message) ->
    NewObj = Obj#async_message{correlationId = Value},
    {ok, NewObj, {correlationId, Value}};
set(Obj, correlationIdBytes, Value) when is_record(Obj, async_message) ->
    NewObj = Obj#async_message{correlationIdBytes = Value},
    {ok, NewObj, {correlationIdBytes, Value}};
set(Obj, AbstractMessageProperty, Value) when is_record(Obj, async_message) and is_atom(AbstractMessageProperty) ->
    {ok, NewAbstractMessage, _} = set(Obj#async_message.parent, AbstractMessageProperty, Value),
    set(Obj, parent, NewAbstractMessage);

set(Obj, parent, Value) when is_record(Obj, command_message) and is_record(Value, async_message) ->
    NewObj = Obj#command_message{parent = Value},
    {ok, NewObj, {parent, Value}};
set(Obj, operation, Value) when is_record(Obj, command_message) ->
    NewObj = Obj#command_message{operation = Value},
    {ok, NewObj, {operation, Value}};
set(Obj, AsyncMessageProperty, Value) when is_record(Obj, command_message) and is_atom(AsyncMessageProperty) ->
    {ok, NewAsyncMessage, _} = set(Obj#command_message.parent, AsyncMessageProperty, Value),
    set(Obj, parent, NewAsyncMessage).


fc_to_record(?FC_REMOTINGMESSAGE) -> {ok, #remoting_message{}};
fc_to_record(?FC_COMMANDMESSAGE) -> {ok, #command_message{}};
fc_to_record(_) -> {ok, undefined}.

%% Convert String to term, Str must be term-like string
to_term(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str), 
    {ok, Term} = erl_parse:parse_term(Tokens ++ [{dot,1}]),
    Term.
