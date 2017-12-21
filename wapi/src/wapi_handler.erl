-module(wapi_handler).
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([handle_post_method/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, []},
       handle_post_method}], Req, State}.

handle_post_method(Req, State) ->
    {ok, JsonReq} = wapi_lib:read_body(Req),
    MapReq = jsx:decode(JsonReq, [return_maps]),
    #{<<"cmd">> := BinErlCmd} = MapReq,
    ErlCmd = binary_to_list(BinErlCmd),
    {value, Result, _} = wapi_lib:eval(ErlCmd),
    BinRes = wapi_lib:format_binary(Result),
    MapRes = #{<<"ans">> => BinRes},
    JsonRes = jsx:encode(MapRes),
    NewReq = cowboy_req:set_resp_body(JsonRes, Req),
    io:format("LOG: ~p~n~p~n~n",
              [{?MODULE, ?LINE},
               [{request_body, JsonReq},
                {map_converted_request, MapReq},
                {erlang_command, ErlCmd},
                {result, Result},
                {map_converted_response, MapRes},
                {response_body, JsonRes}]]),
    {true, NewReq, State}.
