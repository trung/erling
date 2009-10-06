%% 
%% Test cases for read/write operation for all AMF0 data types
%%
-module(amf0_test).
-author("trung@mdkt.org").

-include_lib("eunit/include/eunit.hrl").
-include("../include/types.hrl").
-include("../include/messages.hrl").
-compile(export_all).

%% Generic method to test write_object/read_object
verify(ExpectedValue) ->
    {ok, Bin} = amf0:write_object(ExpectedValue),
    ?assert(Bin /= <<>>),
    {ok, ActualValue, _Rest} = amf0:read_object(Bin),
    ?assertEqual(ExpectedValue, ActualValue).

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
    verify(ExpectedValue).

string_test() ->
    ExpectedValue = #string{data = "nguyen Kien trung"},
    verify(ExpectedValue).

long_string_test() ->
    ExpectedValue = #long_string{data = "nguyen Kien trung"},
    verify(ExpectedValue).

xml_test() ->
    ExpectedValue = #xml{data = "<xml><a></a></xml>"},
    verify(ExpectedValue).

date_test() ->
    ExpectedValue = {{2009, 12, 12}, {12, 30, 40}},
    verify(ExpectedValue).

boolean_true_test() ->
    ExpectedValue = true,
    verify(ExpectedValue).

boolean_false_test() ->
    ExpectedValue = false,
    verify(ExpectedValue).

null_test() ->
    ExpectedValue = null,
    verify(ExpectedValue).

reference_test() ->
    ExpectedValue = "something",
    %% First, store an object in to object reference table
    amf0:reset(),
    amf0:write_object_reference(ExpectedValue),
    {ok, Bin} = amf0:write_object(ref, 0),
    ?assert(Bin /= <<>>),
    {ok, ActualValue, _Rest} = amf0:read_object(Bin),
    ?assertEqual(ExpectedValue, ActualValue).

strict_array_test() ->
    ExpectedValue = [#string{data = "SomeString"}, #xml{data = "<b></b>"}, true, 13.4],
    verify(ExpectedValue).

registered_typed_object_test() ->
    ExpectedValue = #remoting_message{source = #string{data = "somesource"}},
    verify(ExpectedValue).
