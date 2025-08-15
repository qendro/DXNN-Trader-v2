%% IB Protocol Binary Helpers
%% Clean encoding/decoding utilities for IB TWS API

-module(ib_proto).
-export([z/1, i2b/1, read_cstring/1, read_cstring/2]).

%% Add null terminator to binary
z(Bin) when is_binary(Bin) -> 
    <<Bin/binary, 0>>;
z(List) when is_list(List) -> 
    List ++ [0].

%% Integer to binary
i2b(Int) when is_integer(Int) -> 
    integer_to_binary(Int).

%% Consume a C-string (null-terminated) from a binary buffer
read_cstring(<<>>) -> 
    {error, incomplete};
read_cstring(Bin) ->
    read_cstring(Bin, <<>>).

read_cstring(<<0, Rest/binary>>, Acc) ->
    {ok, Acc, Rest};
read_cstring(<<Byte, Rest/binary>>, Acc) ->
    read_cstring(Rest, <<Acc/binary, Byte>>);
read_cstring(<<>>, _Acc) ->
    {error, incomplete}.