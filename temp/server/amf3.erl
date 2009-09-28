-module(amf3).
-export([read_uint_29/1]).

%% The high bit of the first 3 bytes are used as flags to determine 
%% whether the next byte is part of the integer. 
%% With up to 3 bits of the 32 bits being used as flags, 
%% only 29 significant bits remain for encoding an integer. 
%% This means the largest unsigned integer value that can be represented is 2^29 - 1.
%%       (hex)             :      (binary)
%% 0x00000000 - 0x0000007F : 0xxxxxxx
%% 0x00000080 - 0x00003FFF : 1xxxxxxx 0xxxxxxx
%% 0x00004000 - 0x001FFFFF : 1xxxxxxx 1xxxxxxx 0xxxxxxx
%% 0x00200000 - 0x3FFFFFFF : 1xxxxxxx 1xxxxxxx 1xxxxxxx xxxxxxxx
%% 0x40000000 - 0xFFFFFFFF : throw range exception
%% return {ok, Value, RemainBin} or {bad, Reason}
read_uint_29(Bin, _LastByte, Acc, Count) when Count =:= 4 ->
    {ok, Acc, Bin};

read_uint_29(<<>>, LastByte, Acc, _Count) ->
    {ok, (Acc bsl 8) bor LastByte, <<>>};

read_uint_29(Bin, LastByte, Acc, _Count) when LastByte < 128 ->
    read_uint_29(Bin, 0, (Acc bsl 8) bor LastByte, 4);

read_uint_29(Bin, LastByte, Acc, Count) when LastByte >= 128 ->
    <<First:8, Rest/binary>> = Bin,
    io:fwrite("~p - ~p - ~p - ~p - ~p~n", [Count, First, Rest, LastByte, Acc]),
    read_uint_29(Rest, First, (Acc bsl 8) bor LastByte, Count + 1).

read_uint_29(Bin) ->
    case is_binary(Bin) of
	true ->
	    <<First:8, Rest/binary>> = Bin,
	    read_uint_29(Rest, First, 0, 0);
	false ->
	    {bad, "Input is not a binary"}
    end.
