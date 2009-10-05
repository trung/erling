-module(utils).
-author("trung@mdkt.org").
-compile(export_all).

milliseconds_to_date(Milliseconds) ->
    BaseDate = calendar:datetime_to_gregorian_seconds({{1970,1,1},{8,0,0}}),
    Seconds = BaseDate + (trunc(Milliseconds) div 1000),
    calendar:gregorian_seconds_to_datetime(Seconds).

%% Convert date to milliseconds since 0:0:0 1/1/1970
%% Date = date() {{Date}, {Time}}
date_to_milliseconds(Date) ->
    BaseDate = calendar:datetime_to_gregorian_seconds({{1970,1,1},{8,0,0}}),
    Seconds = calendar:datetime_to_gregorian_seconds(Date),
    DiffSeconds = Seconds - BaseDate,
    DiffSeconds * 1000.
