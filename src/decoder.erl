-module(decoder).
-author("trung@mdkt.org").

-export([read/1]).

-include("../include/action_message.hrl").

read_header(Bin) ->
    io:fwrite("Bin = ~n~p~n", [Bin]),
    {ok, HeaderName, BinAfterHeaderName} = amf0:read_string(Bin),
    {ok, MustUnderstand, BinAfterMustUnderstand} = amf0:read_u8(BinAfterHeaderName),
    {ok, _DataLen, DataBin} = amf0:read_u32(BinAfterMustUnderstand),
    %% Don't care about DataLen, just read
    %% {DataBin, NextBin} = split_binary(Rest2, DataLen),
    %% clear ETS tables
    amf0:reset(),
    {ok, Data, NextBin} = amf0:read_object(DataBin),
    Header = #header{headerName = HeaderName, mustUnderstand = case MustUnderstand of 0 -> false; _Other -> true end, data = Data},
    {ok, Header, NextBin}.

read_headers(Bin, Count, Total, Acc) when Count == Total ->
    {ok, Acc, Bin};
read_headers(Bin, Count, Total, Acc) ->
    {ok, Header, NextBin} = read_header(Bin),
    io:fwrite("Parsed header ~p : ~p~n", [Count + 1, Header]),
    read_headers(NextBin, Count + 1, Total, Acc ++ [Header]).

read_body(Bin) ->    
    {ok, TargetUri, BinAfterTargetUri} = amf0:read_string(Bin),
    {ok, ResponseUri, BinAfterResponseUri} = amf0:read_string(BinAfterTargetUri),
    {ok, _DataLen, DataBin} = amf0:read_u32(BinAfterResponseUri),
    %% Don't care about DataLen, just read
    %% {DataBin, NextBin} = split_binary(Rest2, DataLen),
    %% clear ETS tables
    amf0:reset(),
    {ok, Data, NextBin} = amf0:read_object(DataBin),
    Body = #body{targetUri = TargetUri, responseUri = ResponseUri, data=Data},
    {ok, Body, NextBin}.

read_bodies(Bin, Count, Total, Acc) when Count == Total ->
    {ok, Acc, Bin};
read_bodies(Bin, Count, Total, Acc) ->
    {ok, Body, NextBin} = read_body(Bin),
    read_bodies(NextBin, Count + 1, Total, Acc ++ [Body]).

%% Read binary and translate to action_message record
%% Msg = #action_message
%% return {ok, Msg, Rest} or {bad, Reason}
%% ideally, Rest must be <<>>
read(<<?VERSION_1:16, Rest/binary>>) -> read_now(?VERSION_1, Rest);
read(<<?VERSION_3:16, Rest/binary>>) -> read_now(?VERSION_3, Rest);
read(_) ->
    {bad, "Not a valid binary or version not supported"}.

read_now(Version, Bin) ->
    {ok, HeaderCount, BinAfterHeaderCount} = amf0:read_u16(Bin),
    io:fwrite("There are ~p header(s)~n", [HeaderCount]),
    {ok, Headers, BinAfterHeaders} = read_headers(BinAfterHeaderCount, 0, HeaderCount, []),
    {ok, BodyCount, BinAfterBodyCount} = amf0:read_u16(BinAfterHeaders),
    io:fwrite("There are ~p body(s)~n", [BodyCount]),
    {ok, Bodies, BinAfterBodies} = read_bodies(BinAfterBodyCount, 0, BodyCount, []),
    Msg = #action_message{version = Version, headers = Headers, bodies = Bodies},
    {ok, Msg, BinAfterBodies}.
