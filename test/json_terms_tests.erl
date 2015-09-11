
-module(json_terms_tests).

-compile({parse_transform, runtime_types}).
-compile({parse_transform, exprecs}).

-include_lib("eunit/include/eunit.hrl").

-record(wrapper, {
          version :: integer(),
          method :: binary(),
          params :: [aeon:json_terms()]
         }).

-record(wrapped, {
          name :: string(),
          id :: integer(),
          height :: float()
         }).

-export_records([wrapper, wrapped]).

json_terms_encode_test() ->
    Wrapped = #wrapped{
                 name = "John",
                 id = 3,
                 height = 5.9
                },
    Wrapper = #wrapper{
                 version = 1,
                 method = <<"test">>,
                 params = [aeon:record_to_jsx(Wrapped, ?MODULE)]
                },

    WrapperJSON = jsx:encode(aeon:record_to_jsx(Wrapper, ?MODULE)),
    Wrapper2 = aeon:to_record(jsx:decode(WrapperJSON), ?MODULE, wrapper),
    [ParamJSON] = Wrapper2#wrapper.params,
    Wrapped2 = aeon:to_record(ParamJSON, ?MODULE, wrapped),
    ?assertMatch(Wrapped, Wrapped2).

