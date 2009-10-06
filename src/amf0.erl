%% @author Trung Nguyen [trung@mdkt.org]
%% @copyright Trung Nguyen 2009
%% @doc Read/write functions for AMF0 Specification
-module(amf0).
-author("trung@mdkt.org").

-compile(export_all).

-include("../include/action_message.hrl").
-include("../include/types.hrl").

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

len(Str) when is_record(Str, string) -> length(Str#string.data);
len(Str) when is_record(Str, long_string) -> length(Str#long_string.data);
len(Str) -> length(Str).

%% Clear ETS tables
reset() ->
    _ = ref_table:clear(?OBJECT_REF_TABLE_AMF0),
    _ = amf3:reset(),
    {ok}.

%% ===========================================================
%% READ methods
%% ===========================================================

read_u8(<<Value:8, Rest/binary>>) ->
    {ok, Value, Rest}.

read_u16(<<Value:16, Rest/binary>>) ->
    {ok, Value, Rest}.

read_u32(<<Value:32, Rest/binary>>) ->
    {ok, Value, Rest}.

%% Read object from ETS based on the key Ref
%% Return {ok, Obj} or {bad, Reason}
read_object_reference(Ref) ->
    ref_table:read(?OBJECT_REF_TABLE_AMF0, Ref).

write_object_reference(Obj) ->
    {ok, inserted, Ref} = ref_table:insert(?OBJECT_REF_TABLE_AMF0, Obj),
    {ok, Ref, Obj}.

read_string(Bin) ->
    {ok, StringLen, Rest} = read_u16(Bin),
    {StringBin, Rest1} = split_binary(Rest, StringLen),
    {ok, String} = utf8:from_binary(StringBin),
    {ok, #string{data = String}, Rest1}.

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
    {ok, #long_string{data = String}, NextBin}.

%% xml doc is long string
read_xml(Bin) ->
    {ok, LongString, NextBin} = read_long_string(Bin),
	{ok, #xml{data = LongString#long_string.data}, NextBin}.

read_typed_object_property(Bin, Obj) ->
    {ok, PropertyName, BinAfterName} = read_string(Bin),
    case read_object(BinAfterName) of
	{object_end_marker, NextBin, _} ->
	    {ok, Obj, NextBin};
	{ok, ObjValue, NextBin} ->
	    {ok, NewObj, _} = record_utils:set(Obj, PropertyName, ObjValue),
	    read_typed_object_property(NextBin, NewObj)
    end.

read_typed_object(Bin) ->
    {ok, ClassName, BinAfterClassName} = read_string(Bin),
    case registry:fc_to_record(ClassName#string.data) of
	{ok, undefined} ->
	    % create asobject
	    read_typed_object_property(BinAfterClassName, #asobject{});
	{ok, NewObject} ->
	    read_typed_object_property(BinAfterClassName, NewObject)
    end.

read_null(Bin) ->
    {ok, null, Bin}.

read_objects_until_end(Bin, Acc) ->
    {ok, Name, BinAfterName} = read_string(Bin),
    case read_object(BinAfterName) of
	{object_end_marker, NextBin, _} ->
	    {ok, #ecma_array{data = Acc}, NextBin};
	{ok, Obj, NextBin} ->
	    %% Always read value but be careful to ignore erroneous 'length' prop 
	    %% that is sometimes sent by the player. (via BlazeDS)
	    if 
		Name#string.data == "length" ->
		    read_objects_until_end(NextBin, Acc);
		true ->
		    read_objects_until_end(NextBin, Acc ++ [{Name, Obj}])
	    end;
	Other ->
	    Other
    end.

%% The return {ok, [{"name", ValueObject}, ...], <<...>>}
read_ecma_array(Bin) ->
    {ok, _Len, BinAfterLen} = read_u32(Bin),
    %% read until we meet ?object_end_marker
    read_objects_until_end(BinAfterLen, []).

read_reference(Bin) ->
    {ok, Ref, NextBin} = read_u16(Bin),
    {ok, Obj} = read_object_reference(Ref),
    {ok, Obj, NextBin}.

%% return {ok, value/Value, Rest} or {bad, Reason}
read_object(<<?number_marker:8, Rest/binary>>)          -> read_number(Rest);
read_object(<<?boolean_marker:8, Rest/binary>>)         -> read_boolean(Rest);
read_object(<<?string_marker:8, Rest/binary>>)          -> read_string(Rest);
read_object(<<?object_marker:8, Rest/binary>>)          -> {bad, {"Not yet implemented", ?MODULE,?LINE, Rest}};
read_object(<<?movieclip_marker:8, Rest/binary>>)       -> {bad, {"Reserved, not supported", Rest}};
read_object(<<?null_marker:8, Rest/binary>>)            -> read_null(Rest);
read_object(<<?undefined_marker:8, Rest/binary>>)       -> {bad, {"Undefined marker", ?MODULE, ?LINE, Rest}};
read_object(<<?reference_marker:8, Rest/binary>>)       -> read_reference(Rest);
read_object(<<?ecma_array_marker:8, Rest/binary>>)      -> read_ecma_array(Rest);
read_object(<<?object_end_marker:8, Rest/binary>>)      -> {object_end_marker, Rest, {?MODULE, ?LINE}};
read_object(<<?strict_array_marker:8, Rest/binary>>)    -> read_strict_array(Rest);
read_object(<<?date_marker:8, Rest/binary>>)            -> read_date(Rest);
read_object(<<?long_string_marker:8, Rest/binary>>)     -> read_long_string(Rest);
read_object(<<?unsupported_marker:8, Rest/binary>>)     -> {bad, {"Unsupported marker", ?MODULE, ?LINE, Rest}};
read_object(<<?recordset_marker:8, Rest/binary>>)       -> {bad, {"Reserved, not supported", ?MODULE, ?LINE, Rest}};
read_object(<<?xml_document_marker:8, Rest/binary>>)    -> read_xml(Rest);
read_object(<<?typed_object_marker:8, Rest/binary>>)    -> read_typed_object(Rest);
%% switch to AMF3
read_object(<<?avm_plus_object_marker:8, Rest/binary>>) -> amf3:read_object(Rest).

%% ===========================================================
%% WRITE methods
%% Use xxx(marker, Value) to build binary with marker
%% ===========================================================

write_object(null) -> write_null();
write_object(true) -> write_boolean(true);
write_object(false) -> write_boolean(false);
write_object(Number) when is_number(Number) -> write_number(Number);
write_object(String) when is_record(String, string) -> write_string(String);
write_object(LongString) when is_record(LongString, long_string) -> write_long_string(LongString);
write_object(EcmaArray) when is_record(EcmaArray, ecma_array) -> write_ecma_array(EcmaArray);
write_object({{Ye, Mo, Da}, {Ho, Mi, Se}}) -> write_date({{Ye, Mo, Da}, {Ho, Mi, Se}});
write_object(Xml) when is_record(Xml, xml) -> write_xml(Xml);
write_object(Array) when is_list(Array) -> write_strict_array(Array);
write_object(Obj) -> 
	ObjType = record_utils:type(Obj),
    case registry:record_to_fc(ObjType) of
	{ok, undefined} ->
	    {bad, {"Unknown object", ObjType, ?MODULE, ?LINE}};
	{ok, ClassName} ->
	    {ok, ClassNameBin} = write_string(#string{data = ClassName}),
	    case ObjType of
		undefined ->
			{bad, {"Object type not found even it was registered", ClassName, Obj}};
		Type ->
		    Fields = record_utils:fields_atom(Type),
		    ObjBin = list_to_binary([write_object_info(Obj, X) || X <- Fields ]),
		    {ok, ObjEnd} = write_object_end(),
		    write_object_now(?typed_object_marker, [ClassNameBin, ObjBin, ObjEnd])
	    end
    end.

write_object(ref, Ref) -> write_reference(Ref);
write_object(amf3, Obj) -> amf3:write_object(Obj).

write_object_info(Obj, Field) ->
    {ok, FieldBin} = write_string(#string{data = atom_to_list(Field)}),
    case record_utils:get(Obj, Field) of
		{ok, undefined} ->
			[];
		{ok, Value} ->
    		{ok, ValueBin} = write_object(Value),
    		[FieldBin, ValueBin]
	end.

%% return {ok, ReturnBin} or {bad, Reason}
write_u8(Value)  -> {ok, <<Value:8>>}.

write_u16(Value) -> {ok, <<Value:16>>}.

write_u32(Value) -> {ok, <<Value:32>>}.

write_object_now(Marker, Bin) ->
    {ok, list_to_binary([<<Marker:8>>, Bin])}.

write_number(Value) ->
    {ok, Bin} = number_to_binary(Value),
    write_object_now(?number_marker, Bin).

number_to_binary(Value) -> 
    {ok, <<Value/float>>}.

write_string(Value) ->
    {ok, Bin} = string_to_binary(Value),
    write_object_now(?string_marker, Bin).

%% write UTF-8-empty
string_to_binary(#string{data = Value}) when length(Value) == 0 ->
    write_u16(0);

%% write utf8 string to binary
string_to_binary(#string{data = Value}) ->
    Len = length(Value),
    {ok, LenBin} = write_u16(Len),
    {ok, StringBin} = utf8:to_binary(Value),
    {ok, list_to_binary([LenBin, StringBin])}.

write_long_string(Value) ->
    {ok, Bin} = long_string_to_binary(Value),
    write_object_now(?long_string_marker, Bin).

%% write utf8 long string to binary
long_string_to_binary(#long_string{data = Value}) ->
    Len = length(Value),
    {ok, LenBin} = write_u32(Len),
    {ok, StringBin} = utf8:to_binary(Value),
    {ok, list_to_binary([LenBin, StringBin])}.

write_xml(Value) ->
    {ok, Bin} = xml_to_binary(Value),
    write_object_now(?xml_document_marker, Bin).

xml_to_binary(#xml{data = Value}) -> 
    long_string_to_binary(#long_string{data = Value}).

write_date(Value) ->
    {ok, Bin} = date_to_binary(Value),
    write_object_now(?date_marker, Bin).

date_to_binary({Date, Time}) -> date_to_binary(utils:date_to_milliseconds({Date, Time}));
date_to_binary(Milliseconds) when is_number(Milliseconds) ->
    {ok, TimezoneBin} = write_u16(0), %% just for reserved, not used
    {ok, DateBin} = number_to_binary(Milliseconds),
    {ok, list_to_binary([TimezoneBin, DateBin])}.

write_boolean(Value) ->
    {ok, Bin} = boolean_to_binary(Value),
    write_object_now(?boolean_marker, Bin).

boolean_to_binary(true) -> write_u8(1);
boolean_to_binary(false) -> write_u8(0).

%% input Value is the list of objects
write_strict_array(Value) ->
    {ok, Bin} = strict_array_to_binary(Value),
    write_object_now(?strict_array_marker, Bin).

item_to_binary(Obj) ->
    {ok, Bin} = write_object(Obj),
    Bin.

strict_array_to_binary(Value) ->
    Len = length(Value),
    {ok, LenBin} = write_u32(Len),
    ObjBinArray = [ item_to_binary(X) || X <- Value ],
    {ok, list_to_binary([LenBin, ObjBinArray])}.

write_typed_object(Value) ->
    {bad, {"Not yet implemented", Value, ?MODULE, ?LINE}}.

write_reference(Value) ->
    {ok, Bin} = reference_to_binary(Value),
    write_object_now(?reference_marker, Bin).

reference_to_binary(Value) ->
    write_u16(Value).

write_ecma_array(Value) ->
    {ok, Bin} = ecma_array_to_binary(Value),
    write_object_now(?ecma_array_marker, Bin).

write_ecma_array_item({Name, Value}) ->
    {ok, NameBin} = string_to_binary(Name),
    {ok, ValueBin} = write_object(Value),
    [NameBin, ValueBin].

ecma_array_to_binary(#ecma_array{data = Value}) ->
    {ok, LenBin} = write_u32(0),
    ArrayBin = [write_ecma_array_item(X) || X <- Value],
    {ok, ObjectEndBin} = write_object_end(),
    {ok, list_to_binary([LenBin, ArrayBin, ObjectEndBin])}.
    
write_null() ->
    {ok, <<?null_marker:8>>}.

write_object_end() ->
    {ok, <<0, 0, ?object_end_marker>>}.
