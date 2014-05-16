
-module(aeon_common).

-export([
	 field_types/2,
	 first_no_fail/2
	]).

field_types(Module, Type) ->
	{Type, FieldList} = lists:keyfind(Type, 1, Module:'#types'()),
	FieldList.

-spec first_no_fail(fun((any()) -> any()), [any()]) -> any().
first_no_fail(_, []) ->
	throw(all_failed);
first_no_fail(F, [A | Args]) ->
	try
		F(A)
	catch
		_:_ -> first_no_fail(F, Args)
	end.

