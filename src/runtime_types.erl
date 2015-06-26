
-module(runtime_types).

-export([parse_transform/2, do_transform/2]).

-include_lib("parse_trans/include/codegen.hrl").

-define(FUN_NAME, '#types').

parse_transform(Forms, Options) ->
	parse_trans:top(fun do_transform/2, Forms, Options).

do_transform(Forms, Context) ->
	F = erl_syntax_lib:analyze_forms(Forms),
	K = attributes,
	case lists:keyfind(K, 1, F) of
		false -> Forms;
		{K, R} ->
			Attr = lists:flatten([transform_attribute(A) || A <- R]),
			Func = codegen:gen_function(?FUN_NAME, fun() -> {'$var', Attr} end),
			Forms2 = parse_trans:do_insert_forms(below, [Func], Forms, Context),
			Forms3 = parse_trans:export_function(?FUN_NAME, 0, Forms2),
			parse_trans:revert(Forms3)
	end.

transform_attribute({type, {{record, RName}, Fields, []}}) ->
	{{record, RName}, [field_info(F) || F <- Fields]};
transform_attribute({type, {TName, TDescr, []}}) ->
	{{type, TName}, typename(TDescr)};
transform_attribute(_) ->
	[].

% field has no type info
field_info({record_field, _, {atom, _, FieldName}}) ->
	{FieldName, {atom, undefined}};
% field has no type info, but it does have an initializer
field_info({record_field, _, {atom, _, FieldName}, Init}) ->
	{FieldName, typename(Init)};
field_info({typed_record_field, {record_field, _, {atom, _, FieldName}}, Type}) ->
	{FieldName, typename(Type)};
field_info({typed_record_field, {record_field, _, {atom, _, FieldName}, _}, Type}) ->
	{FieldName, typename(Type)}.

typename({var, _, Lit}) ->
	{literal, Lit};
typename({atom, _, Value}) ->
	{atom, Value};
typename({type, _, record, [{atom, _, RecType}]}) ->
	{record, RecType};
typename({type, _, list, [LType]}) ->
	{list, typename(LType)};
typename({type, _, Type, Subtypes}) when Type =:= union; Type =:= tuple ->
	{Type, [typename(T) || T <- Subtypes]};
typename({type, _, Type, Subtypes, []}) when Type =:= union; Type =:= tuple ->
	{Type, [typename(T) || T <- Subtypes]};
typename({type, _, TypeName, _}) ->
	{type, TypeName};
typename({user_type, _, TypeName, _}) ->
	{type, TypeName};
typename({remote_type, _, [{atom, _, Mod}, {atom, _, Type}, _]}) ->
	{type, {Mod, Type}};
typename({ann_type, _, [{var, _, _}, T]}) ->
	typename(T);
typename({A, _, _}) when is_atom(A) ->
	A.

