-module(wapi_lib).
-export([eval/1]).
-export([read_body/1]).
-export([format_binary/1]).

eval(S) ->
    io:format("~p~n", [S]),
    eval(S, []).

eval(S, Bindings) ->
    {ok, Scanned,_} = erl_scan:string(S),
    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
    erl_eval:exprs(Parsed, Bindings).

read_body(Req) ->
    read_body(Req, <<>>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} ->
            {ok, <<Acc/binary, Data/binary>>};
        {more, Data, Req} ->
            read_body(Req, <<Acc/binary, Data/binary>>)
    end.

format_binary(R) ->
    RStr = lists:flatten(io_lib:format("~p", [R])),
    list_to_binary(RStr).
