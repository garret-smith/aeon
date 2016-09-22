
-module(aeon_common).

-export([
	 field_types/2,
	 first_no_fail/2,
	 is_optional_field/1,
	 is_excluded_field/1,
	 suppress_values/1
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
		throw:{conversion_error, _, _, _} -> first_no_fail(F, Args);
		throw:all_failed -> first_no_fail(F, Args); % first_no_fail can be nested
		throw:try_again -> first_no_fail(F, Args)
	end.

suppress_values({union, UTypes}) ->
	lists:flatten([suppress_values(T) || T <- UTypes]);
suppress_values({type, {aeon, suppress, V}}) ->
	[A || {atom, A} <- V];
suppress_values(_) ->
	[].

is_optional_field({union, UTypes}) ->
	lists:any(fun is_optional_field/1, UTypes);
is_optional_field({type, {aeon, optional_field}}) ->
	true;
is_optional_field(_) ->
	false.

is_excluded_field({union, UTypes}) ->
	lists:any(fun is_excluded_field/1, UTypes);
is_excluded_field({type, {aeon, excluded_field}}) ->
	true;
is_excluded_field(_) ->
	false.