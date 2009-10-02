-module(amf0).
-author("trung@mdkt.org").

-export([read_object/1, read_u8/1, read_u16/1, read_u32/1, read_string/1, reset/0]).

-include("action_message.hrl").

-define(number_marker, 16#00).
-define(boolean_marker, 16#01).
-define(string_marker, 16#02).
-define(object_marker, 16#03).
-define(movieclip_marker, 16#04). %% reserved, not supported
-define(null_marker, 16#05).
-define(undefined_marker, 16#06).
-define(reference_marker, 16#07).
-define(ecma_array_marker, 16#08).
-define(object_end_marker, 16#09).
-define(strict_array_marker, 16#0A).
-define(date_marker, 16#0B).
-define(long_string_marker, 16#0C).
-define(unsupported_marker, 16#0D).
-define(recordset_marker, 16#0E). %% reserved, not supported
-define(xml_document_marker, 16#0F).
-define(typed_object_marker, 16#10).
-define(avm_plus_object_marker, 16#11). %% define AMF3 must be used

%% Clear ETS tables
reset() ->
    _ = ref_table:clear(?OBJECT_REF_TABLE_AMF0),
    _ = amf3:reset(),
    {ok}.

read_u8(<<Value:8, Rest/binary>>) ->
    {ok, Value, Rest}.

read_u16(<<Value:16, Rest/binary>>) ->
    {ok, Value, Rest}.

read_u32(<<Value:32, Rest/binary>>) ->
    {ok, Value, Rest}.

read_string(Bin) ->
    {ok, StringLen, Rest} = read_u16(Bin),
    {StringBin, Rest1} = split_binary(Rest, StringLen),
    {ok, String} = utf8:from_binary(StringBin),
    {ok, String, Rest1}.

read_number(<<Value/float, Rest/binary>>) ->
    {ok, Value, Rest};
read_number(Something) ->
    {bad, {"Can't read number", Something}}.

read_boolean(<<Value:8, Rest/binary>>) ->
    case Value of
	0 ->
	    {ok, false, Rest};
	_ ->
	    {ok, true, Rest}
    end;
read_boolean(Something) ->
    {bad, {"Can't read boolean", Something}}.

read_strict_array(Bin, Count, Total, Acc) when Count == Total ->
    {ok, Acc, Bin};
read_strict_array(Bin, Count, Total, Acc) ->
    {ok, Obj, NextBin} = read_object(Bin),
    read_strict_array(NextBin, Count + 1, Total, Acc ++ [Obj]).

read_strict_array(Bin) ->
    {ok, Size, BinAfterSize} = read_u32(Bin),
    read_strict_array(BinAfterSize, 0, Size, []).

read_date(Bin) ->
    %% Just read, not use
    {ok, _, BinAfterTimeZone} = read_u16(Bin),
    {ok, TimeInMilli, NextBin} = read_number(BinAfterTimeZone),
    %% convert to erlang date
    Date = utils:milliseconds_to_date(TimeInMilli),
    {ok, Date, NextBin}.

read_long_string(Bin) ->
    {ok, StringLen, BinAfterLen} = read_u32(Bin),
    {StringBin, NextBin} = split_binary(BinAfterLen, StringLen),
    {ok, String} = utf8:from_binary(StringBin),
    {ok, String, NextBin}.

read_xml(Bin) ->
    {bad, {"Not yet implemented", ?MODULE, ?LINE, Bin}}.

read_typed_object(Bin) ->
    {bad, {"Not yet implemented", ?MODULE, ?LINE, Bin}}.

%% return {ok, value/Value, Rest} or {bad, Reason}
read_object(<<?number_marker:8, Rest/binary>>)          -> read_number(Rest);
read_object(<<?boolean_marker:8, Rest/binary>>)         -> read_boolean(Rest);
read_object(<<?string_marker:8, Rest/binary>>)          -> read_string(Rest);
read_object(<<?object_marker:8, Rest/binary>>)          -> {bad, {"Not yet implemented", ?MODULE,?LINE, Rest}};
read_object(<<?movieclip_marker:8, Rest/binary>>)       -> {bad, {"Reserved, not supported", Rest}};
read_object(<<?null_marker:8, Rest/binary>>)            -> {ok, null, Rest};
read_object(<<?undefined_marker:8, Rest/binary>>)       -> {bad, {"Undefined marker", ?MODULE, ?LINE, Rest}};
read_object(<<?reference_marker:8, Rest/binary>>)       -> {bad, {"Not yet implemented", ?MODULE, ?LINE, Rest}};
read_object(<<?ecma_array_marker:8, Rest/binary>>)      -> {bad, {"Not yet implemented", ?MODULE, ?LINE, Rest}};
read_object(<<?object_end_marker:8, Rest/binary>>)      -> {bad, {"Unexpected object end", ?MODULE, ?LINE, Rest}};
read_object(<<?strict_array_marker:8, Rest/binary>>)    -> read_strict_array(Rest);
read_object(<<?date_marker:8, Rest/binary>>)            -> read_date(Rest);
read_object(<<?long_string_marker:8, Rest/binary>>)     -> read_long_string(Rest);
read_object(<<?unsupported_marker:8, Rest/binary>>)     -> {bad, {"Not yet implemented", ?MODULE, ?LINE, Rest}};
read_object(<<?recordset_marker:8, Rest/binary>>)       -> {bad, {"Reserved, not supported", ?MODULE, ?LINE, Rest}};
read_object(<<?xml_document_marker:8, Rest/binary>>)    -> read_xml(Rest);
read_object(<<?typed_object_marker:8, Rest/binary>>)    -> read_typed_object(Rest);
%% switch to AMF3
read_object(<<?avm_plus_object_marker:8, Rest/binary>>) -> amf3:read_object(Rest).
