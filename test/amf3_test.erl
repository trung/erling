-module(amf3_test).
-author("trung@mdkt.org").

-include_lib("eunit/include/eunit.hrl").
-include("../include/types.hrl").
-include("../include/messages.hrl").
-compile(export_all).

uint29_verify(ExpectedValue) ->
    {ok, Bin} = amf3:write_uint_29(ExpectedValue),
    ?assert(Bin /= <<>>),
    ?debugFmt("~p~n", [Bin]),
    {ok, ActualValue, _} = amf3:read_uint_29(Bin),
    ?assertEqual(ExpectedValue, ActualValue).

uint29_less_128_test() ->
    ExpectedValue = 123,
    uint29_verify(ExpectedValue).

uint29_less_16384_test() ->
    ExpectedValue = 15384,
    uint29_verify(ExpectedValue).

uint29_less_2097152_test() ->
    ExpectedValue = 1097152,
    uint29_verify(ExpectedValue).

uint29_less_536870911_test() ->
    ExpectedValue = 536870911,
    uint29_verify(ExpectedValue).

uint29_out_of_range_test() ->
    ExpectedValue = 536870912,
    {bad, _} = amf3:write_uint_29(ExpectedValue).
