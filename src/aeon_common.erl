
-module(aeon_common).

-export([
	 field_types/2,
	 first_no_fail/2
	]).

field_types(_Module, {type, Intrinsic}) when
	  Intrinsic =:= integer;
	  Intrinsic =:= float;
	  Intrinsic =:= boolean;
	  Intrinsic =:= binary;
	  Intrinsic =:= atom;
	  Intrinsic =:= any;
	  Intrinsic =:= term
	  ->
	throw(try_again);
field_types(Module, Type) ->
	case catch lists:keyfind(Type, 1, Module:'#types'()) of
		{Type, FieldList} ->
			FieldList;
		false ->
			error({no_type, {Module, Type}});
		{error, undef} ->
			error({no_parse_transform, Module})
	end.

-spec first_no_fail(fun((any()) -> any()), [any()]) -> any().
first_no_fail(_, []) ->
	throw(all_failed);
first_no_fail(F, [A | Args]) ->
	try
		F(A)
	catch
		error:{no_type, T} -> error({no_type, T});
		throw:{no_conversion, _V, _T} -> first_no_fail(F, Args);
		throw:try_again -> first_no_fail(F, Args)
	end.

