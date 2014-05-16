
-module(aeon_tests).

-compile({parse_transform, runtime_types}).
-compile({parse_transform, exprecs}).

-include_lib("eunit/include/eunit.hrl").

-type pairlist() :: [a_pair()].
-type a_pair() :: {atom(), atom()}.

-record(baby_boy, {
          birthday :: {Mega :: integer(), Secs :: integer(), Milli :: integer()}
         }).

-record(empty, {}).

-record(big_boy, {
          int_t = 0 :: integer(),
          float_t :: float(),
          bool_t = false :: boolean(),
          def_v = x :: atom(),
          pair_t :: a_pair(),
          rec_t_t :: little(),
          list_t :: [integer()],
          rec_t :: #baby_boy{},
          empty_t = #empty{} :: #empty{}
         }).

-record(little_boy, {
          name :: string(),
          id :: binary(),
          height
         }).
-type little() :: #little_boy{}.

-export_records([empty, baby_boy, big_boy, little_boy]).

monolithic_record_roundtrip_test() ->
    Record = #big_boy{
                int_t = 4,
                float_t = 1.22234,
                bool_t = true,
                pair_t = {foo, bar},
                rec_t_t = #little_boy{
                             name = "my name",
                             id = <<"boy xyz">>
                            },
                list_t = [1, 2, 3],
                rec_t = #baby_boy{
                           birthday = erlang:now()
                          }
               },

    JSON = jsx:encode(aeon:record_to_jsx(Record, ?MODULE)),
    RoundtripRecord = aeon:to_record(jsx:decode(JSON), ?MODULE, big_boy),
    ?assertMatch(Record, RoundtripRecord).


monolithic_type_roundtrip_test() ->
    T = [{a,b},{c,d},{e,f}],
    JSON = jsx:encode(aeon:type_to_jsx(T, ?MODULE, pairlist)),
    RoundtripT = aeon:to_type(jsx:decode(JSON), ?MODULE, pairlist),
    ?assertMatch(T, RoundtripT).

