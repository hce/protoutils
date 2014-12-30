%%% @author Hans-Christian Esperer <hc@operations.hcesperer.org>
%%% @copyright (C) 2014, Hans-Christian Esperer
%%% @doc
%%%
%%% @end
%%% Created : 30 Dec 2014 by Hans-Christian Esperer <hc@operations.hcesperer.org>

-module(test_protojson).

-include_lib("eunit/include/eunit.hrl").

protojson_test() ->
    {ok, Json} = file:read_file("../test/test.json"),

    Decoded2 = mochijson2:encode(mochijson2:decode(Json)),
    
    Encoded = iolist_to_binary(protojson:rawjsontoproto(Json)),
    
    Decoded = protojson:rawprototojson(Encoded),

    ?assertEqual(Decoded2, Decoded),
    ok.
