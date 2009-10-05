-module(utils_test).
-author("trung@mdkt.org").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

milliseconds_to_date_test() ->
    Milli = 1254730491959,
    ExpectedDate = {{2009, 10, 05}, {16, 14, 51}},
    ActualDate = utils:milliseconds_to_date(Milli),
    ?assertEqual(ExpectedDate, ActualDate).

date_to_milliseconds_test() ->
    ExpectedMilli = (1254730491959 div 1000) * 1000,
    Date = {{2009, 10, 05}, {16, 14, 51}},
    ActualMilli = utils:date_to_milliseconds(Date),
    ?assertEqual(ExpectedMilli, ActualMilli).
