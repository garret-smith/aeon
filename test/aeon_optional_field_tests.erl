
-module(aeon_optional_field_tests).

-compile({parse_transform, runtime_types}).
-compile({parse_transform, exprecs}).

-include_lib("eunit/include/eunit.hrl").

-record(json_opt, {
          id = null :: integer() | aeon:optional_field() | aeon:suppress(null) | aeon:suppress(suppress),
          user = <<"">> :: binary(),
          age :: float()
         }).

-export_records([json_opt]).

optional_json_field_to_record_test() ->
    JSON = <<"{\"user\": \"user1\", \"age\": 34.5}">>,
    R = aeon:to_record(jsx:decode(JSON), ?MODULE, json_opt),
    ?assertMatch(#json_opt{id = null, user = <<"user1">>, age = 34.5}, R).

suppress_null_record_field_to_json_test() ->
    R = #json_opt{id = null, user = <<"user1">>, age = 34.5},
    JSON = jsx:encode(aeon:record_to_jsx(R, ?MODULE)),
    ?assertEqual(<<"{\"age\":34.5,\"user\":\"user1\"}">>, JSON).

nosuppress_undef_record_field_to_json_test() ->
    R = #json_opt{id = undefined, user = <<"user1">>, age = 34.5},
    JSON = jsx:encode(aeon:record_to_jsx(R, ?MODULE)),
    ?assertEqual(<<"{\"age\":34.5,\"user\":\"user1\",\"id\":\"undefined\"}">>, JSON).

suppress_suppress_record_field_to_json_test() ->
    R = #json_opt{id = suppress, user = <<"user1">>, age = 34.5},
    JSON = jsx:encode(aeon:record_to_jsx(R, ?MODULE)),
    ?assertEqual(<<"{\"age\":34.5,\"user\":\"user1\"}">>, JSON).

