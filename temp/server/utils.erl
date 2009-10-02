-module(utils).
-author("trung@mdkt.org").
-compile(export_all).

milliseconds_to_date(Milliseconds) ->
    BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Seconds       = BaseDate + (Milliseconds div 1000),
    calendar:gregorian_seconds_to_datetime(Seconds).
