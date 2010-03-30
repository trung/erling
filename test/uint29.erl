-module(uint29).
-compile(export_all).

encode_int29(I) when I >= -16#10000000, I < 0 ->
    encode_uint29(16#20000000 + I);
encode_int29(I) when I =< 16#0FFFFFFF ->
    encode_uint29(I);
encode_int29(_) ->
    throw(badrange).
 
encode_uint29(I) when I >= 16#00000000, I =< 16#0000007F ->
    <<I>>;
encode_uint29(I) when I >= 16#00000080, I =< 16#00003FFF ->
    X1 = 16#80 bor (I bsr 7),
    X2 = I band 16#7F,
    <<X1, X2>>;
encode_uint29(I) when I >= 16#00004000, I =< 16#001FFFFF ->
    X1 = 16#80 bor (I bsr 14),
    X2 = 16#80 bor (I bsr 7),
    X3 = I band 16#7F,
    <<X1, X2, X3>>;
encode_uint29(I) when I >= 16#00200000, I =< 16#1FFFFFFF ->
    X1 = 16#80 bor (I bsr 22),
    X2 = 16#80 bor (I bsr 15),
    X3 = 16#80 bor (I bsr 8),
    X4 = I band 16#FF,
    <<X1, X2, X3, X4>>;
encode_uint29(_) ->
    throw(badrange).

decode_uint29(Data) ->
    decode_uint29(Data, 0, 0).
 
decode_uint29(<<1:1, Num:7, Data/binary>>, Result, N) when N < 3 ->
    decode_uint29(Data, (Result bsl 7) bor Num, N + 1);
decode_uint29(<<0:1, Num:7, Data/binary>>, Result, N) when N < 3 ->
    {(Result bsl 7) bor Num, Data};
decode_uint29(<<Byte, Data/binary>>, Result, _N) ->
    {(Result bsl 8) bor Byte, Data}.
