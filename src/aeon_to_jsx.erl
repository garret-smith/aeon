
-module(aeon_to_jsx).

-export([
	 record_to_jsx/2,
	 type_to_jsx/3
	]).

record_to_jsx(Record, Module) ->
	RecordType = element(1, Record),
	FieldInfo = aeon_common:field_types(Module, {record, RecordType}),
	Get = list_to_atom("#get-" ++ atom_to_list(element(1, Record))),
	[{atom_to_binary(FieldName, utf8), converted_value(Module:Get(FieldName, Record), FieldType, Module)}
	 || {FieldName, FieldType} <- FieldInfo].

type_to_jsx(Value, Module, ValType) ->
	TypeSpec = aeon_common:field_types(Module, {type, ValType}),
	converted_value(Value, TypeSpec, Module).

converted_value(Val, {type, Intrinsic}, _Module) when
	  Intrinsic =:= integer;
	  Intrinsic =:= float;
	  Intrinsic =:= boolean;
	  Intrinsic =:= binary;
	  Intrinsic =:= atom
	  ->
	Val;
converted_value(Val, {type, string}, _Module) when is_list(Val) ->
	unicode:characters_to_binary(Val, utf8, utf8);
converted_value(Val, {type, T}, Module) when is_atom(T) ->
	type_to_jsx(Val, Module, T);
converted_value(Val, {type, {TMod, T}}, _Module) when is_atom(TMod), is_atom(T) ->
	type_to_jsx(Val, TMod, T);
converted_value(Val, {tuple, TTypes}, Module) when is_tuple(Val), size(Val) =:= length(TTypes) ->
	[converted_value(V,T,Module) || {V,T} <- lists:zip(tuple_to_list(Val), TTypes)];
converted_value(Val, {list, Ltype}, Module) when is_list(Val) ->
	[converted_value(V,Ltype,Module) || V <- Val];
converted_value(Val, {union, Utypes}, Module) ->
	Convert = fun(T) -> converted_value(Val, T, Module) end,
	aeon_common:first_no_fail(Convert, Utypes);
converted_value(Val, {record, _Rtype}, Module) when is_tuple(Val) ->
	record_to_jsx(Val, Module);
converted_value(Val, {atom, A}, _Module) when Val =:= A ->
	Val;
converted_value(Val, nil, _Module) ->
	Val.

