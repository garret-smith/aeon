
-module(aeon_to_erl).

-export([
	 to_record/3,
	 to_type/3
	]).

to_record(Proplist, Module, RecordType) ->
	FieldTypes = aeon_common:field_types(Module, {record, RecordType}),
	NewRec = new_rec(Module, RecordType),
	rec_field_convert(NewRec, Proplist, Module, FieldTypes).

to_type(Jsx, Module, Type) ->
	TypeSpec = aeon_common:field_types(Module, {type, Type}),
	validated_field_value(Jsx, TypeSpec, Module).


rec_field_convert(BuildRec, _Proplist, _RecordModule, []) ->
	BuildRec;
rec_field_convert(BuildRec, Proplist, RecordModule, [{FieldName, FieldType} | FieldTypes]) ->
	BinName = atom_to_binary(FieldName, utf8),
	{value, {_, FieldValue}, NewProplist} = lists:keytake(BinName, 1, Proplist),
	Value = validated_field_value(FieldValue, FieldType, RecordModule),
	rec_field_convert(set_field(FieldName, Value, RecordModule, BuildRec), NewProplist, RecordModule, FieldTypes).

validated_field_value(Val, {type, integer}, _Mod) when is_integer(Val) ->
	Val;
validated_field_value(Val, {type, float}, _Mod) when is_float(Val) ->
	Val;
validated_field_value(Val, {type, boolean}, _Mod) when is_boolean(Val) ->
	Val;
validated_field_value(Val, {type, binary}, _Mod) when is_binary(Val) ->
	Val;
validated_field_value(Val, {type, string}, _Mod) when is_binary(Val) -> % jsx only returns binaries.  Autoconvert if requested
	unicode:characters_to_list(Val);
validated_field_value(Val, {type, atom}, _Mod) when is_atom(Val) -> % what will this really catch?  JSX only converts true, false, null to atoms
	Val;
validated_field_value(Val, {type, atom}, _Mod) when is_binary(Val) -> % jsx only converts true, false, null to Erlang atoms
	binary_to_existing_atom(Val, utf8);
validated_field_value(Val, {type, Type}, Mod) when is_list(Val), is_atom(Type) -> % Val is a proplist to be turned into a type
	to_type(Val, Mod, Type);
validated_field_value(Val, {type, {TMod, Type}}, _Mod) -> % Val is a proplist to be turned into a type
	to_type(Val, TMod, Type);
validated_field_value(Val, {atom, A}, _Mod) when is_binary(Val) ->
	AtomVal = binary_to_existing_atom(Val, utf8),
	if
		A =:= AtomVal -> A;
		true -> throw({bad_atom, Val})
	end;
validated_field_value(Val, nil, _Mod) when is_list(Val) -> % 'nil' is the typespec []
	Val;
validated_field_value(Val, Any, _Mod) when Any =:= any; Any =:= term ->
	Val;
validated_field_value(Val, {record, RecType}, Mod) when is_list(Val), is_atom(RecType) -> % Val is a proplist to be turned into a record
	to_record(Val, Mod, RecType);
validated_field_value(Val, {list, []}, _Mod) when is_list(Val) -> % untyped list
	Val;
validated_field_value(Val, {list, LType}, Mod) when is_list(Val) ->
	[validated_field_value(V, LType, Mod) || V <- Val];
validated_field_value(Val, {union, UTypes}, Mod) ->
	Validator = fun(T) -> validated_field_value(Val, T, Mod) end,
	aeon_common:first_no_fail(Validator, UTypes);
validated_field_value(Val, {tuple, TTypes}, Mod) when is_list(Val) -> % erlang tuple as json list and vice versa
	ValidatedList = [validated_field_value(V, T, Mod) || {V,T} <- lists:zip(Val, TTypes)],
	list_to_tuple(ValidatedList).

set_field(Field, Value, RecMod, Record) ->
	RecMod:'#set-'([{Field, Value}], Record).

new_rec(ModuleName, TypeName) ->
	Constructor = list_to_atom("#new-" ++ atom_to_list(TypeName)),
	ModuleName:Constructor().

