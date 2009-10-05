%% 
%% Test cases for read/write operation for all AMF0 data types
%%
-module(amf0_test).
-author("trung@mdkt.org").

-include_lib("eunit/include/eunit.hrl").
-include ("../include/types.hrl").
-compile(export_all).

%%
%% Read/write an unsigned byte, 8-bit of data, an octet
%%
u8_test() ->
    ExpectedValue = 200,
    {ok, Bin} = amf0:write_u8(ExpectedValue),
    ?assert(Bin /= <<>>),
    {ok, ActualValue, _Rest} = amf0:read_u8(Bin),
    ?assertEqual(ExpectedValue, ActualValue).

%%
%% Read/write an unsigned 16-bit integer in big endian byte order
%%
u16_test() ->
    ExpectedValue = 32767,
    {ok, Bin} = amf0:write_u16(ExpectedValue),
    ?assert(Bin /= <<>>),
    {ok, ActualValue, _Rest} = amf0:read_u16(Bin),
    ?assertEqual(ExpectedValue, ActualValue).

%%
%% Read/write an unsigned 32-bit integer in big endian byte order
%%
u32_test() ->
    ExpectedValue = 3276712,
    {ok, Bin} = amf0:write_u32(ExpectedValue),
    ?assert(Bin /= <<>>),
    {ok, ActualValue, _Rest} = amf0:read_u32(Bin),
    ?assertEqual(ExpectedValue, ActualValue).

number_test() ->
    ExpectedValue = 12.3,
    {ok, Bin} = amf0:write_number(marker, ExpectedValue),
    ?assert(Bin /= <<>>),
    {ok, ActualValue, _Rest} = amf0:read_object(Bin),
    ?assertEqual(ExpectedValue, ActualValue).

string_test() ->
    ExpectedValue = #string{data = "nguyen Kien trung"},
    {ok, Bin} = amf0:write_string(marker, ExpectedValue),
    ?assert(Bin /= <<>>),
    {ok, ActualValue, _Rest} = amf0:read_object(Bin),
	?assert(is_record(ActualValue, string)),
    ?assertEqual(ExpectedValue#string.data, ActualValue#string.data).

long_string_test() ->
    ExpectedValue = #long_string{data = "nguyen Kien trung"},
    {ok, Bin} = amf0:write_long_string(marker, ExpectedValue),
    ?assert(Bin /= <<>>),
    {ok, ActualValue, _Rest} = amf0:read_object(Bin),
    ?assertEqual(ExpectedValue#long_string.data, ActualValue#long_string.data).

xml_test() ->
    ExpectedValue = #xml{data = "<xml><a></a></xml>"},
    {ok, Bin} = amf0:write_xml(marker, ExpectedValue),
    ?assert(Bin /= <<>>),
    {ok, ActualValue, _Rest} = amf0:read_object(Bin),
    ?assertEqual(ExpectedValue#xml.data, ActualValue#xml.data).

date_test() ->
    ExpectedValue = {{2009, 12, 12}, {12, 30, 40}},
    {ok, Bin} = amf0:write_date(marker, ExpectedValue),
    ?assert(Bin /= <<>>),
    {ok, ActualValue, _Rest} = amf0:read_object(Bin),
    ?assertEqual(ExpectedValue, ActualValue).

boolean_true_test() ->
    ExpectedValue = true,
    {ok, Bin} = amf0:write_boolean(marker, ExpectedValue),
    ?assert(Bin /= <<>>),
    {ok, ActualValue, _Rest} = amf0:read_object(Bin),
    ?assertEqual(ExpectedValue, ActualValue).

boolean_false_test() ->
    ExpectedValue = false,
    {ok, Bin} = amf0:write_boolean(marker, ExpectedValue),
    ?assert(Bin /= <<>>),
    {ok, ActualValue, _Rest} = amf0:read_object(Bin),
    ?assertEqual(ExpectedValue, ActualValue).

null_test() ->
    ExpectedValue = null,
    {ok, Bin} = amf0:write_null(),
    ?assert(Bin /= <<>>),
    {ok, ActualValue, _Rest} = amf0:read_object(Bin),
    ?assertEqual(ExpectedValue, ActualValue).

reference_test() ->
    ExpectedValue = "something",
    %% First, store an object in to object reference table
    amf0:reset(),
    amf0:write_object_reference(ExpectedValue),
    {ok, Bin} = amf0:write_reference(marker, 0),
    ?assert(Bin /= <<>>),
    {ok, ActualValue, _Rest} = amf0:read_object(Bin),
    ?assertEqual(ExpectedValue, ActualValue).
