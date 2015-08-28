
-module(aeon_optional_field_tests).

-compile({parse_transform, runtime_types}).
-compile({parse_transform, exprecs}).

-include_lib("eunit/include/eunit.hrl").

-record(json_opt, {
          id = null :: integer() | aeon:optional_field(),
          user = <<"">> :: binary(),
          age :: float()
         }).

-export_records([json_opt]).

optional_json_field_to_record_test() ->
    JSON = <<"{\"user\": \"user1\", \"age\": 34.5}">>,
    R = aeon:to_record(jsx:decode(JSON), ?MODULE, json_opt),
    ?assertMatch(#json_opt{id = null, user = <<"user1">>, age = 34.5}, R).

optional_record_field_to_json_test() ->
    R = #json_opt{id = null, user = <<"user1">>, age = 34.5},
    JSON = jsx:encode(aeon:record_to_jsx(R, ?MODULE)),
    ?assertEqual(<<"{\"age\":34.5,\"user\":\"user1\"}">>, JSON).

