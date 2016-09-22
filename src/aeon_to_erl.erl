
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
	case aeon_common:is_excluded_field(FieldType) of
		true ->
			rec_field_convert(BuildRec, Proplist, RecordModule, FieldTypes);
		false ->
			BinName = atom_to_binary(FieldName, utf8),
			case lists:keytake(BinName, 1, Proplist) of
				false ->
					% not a match for this record type,
					% try the next one in the type spec
					% unless aeon:optional_field() is in the typespec
					case aeon_common:is_optional_field(FieldType) of
						false ->
							throw(try_again);
						true ->
							rec_field_convert(
								BuildRec,
								Proplist,
								RecordModule,
								FieldTypes)
					end;
				{value, {_, FieldValue}, NewProplist} ->
					Value = try
								validated_field_value(FieldValue, FieldType, RecordModule)
							catch
								throw:all_failed ->
									throw({conversion_error, FieldName, FieldType, FieldValue})
							end,
					rec_field_convert(
						set_field(FieldName, Value, RecordModule, BuildRec),
						NewProplist,
						RecordModule,
						FieldTypes)
			end
	end.

validated_field_value(Val, {type, integer}, _Mod) when is_integer(Val) ->
	Val;
validated_field_value(Val, {type, float}, _Mod) when is_float(Val) ->
	Val;
validated_field_value(Val, {type, float}, _Mod) when is_integer(Val) ->
	float(Val);
validated_field_value(Val, {type, boolean}, _Mod) when is_boolean(Val) ->
	Val;
validated_field_value(Val, {type, binary}, _Mod) when is_binary(Val) ->
	Val;
validated_field_value(Val, {type, string}, _Mod) when is_binary(Val) -> % jsx only returns binaries.  Autoconvert if requested
	unicode:characters_to_list(Val);
validated_field_value(Val, {type, atom}, _Mod) when is_atom(Val) -> % what will this really catch?  JSX only converts true, false, null to atoms
	Val;
validated_field_value(Val, T={type, atom}, _Mod) when is_binary(Val) -> % jsx only converts true, false, null to Erlang atoms
	case catch binary_to_existing_atom(Val, utf8) of
		A when is_atom(A) -> A;
		_ -> throw({no_conversion, Val, T})
	end;
validated_field_value(Val, {type, Type}, Mod) when is_atom(Type) ->
	to_type(Val, Mod, Type);
validated_field_value(Val, {type, {aeon, json_terms}}, _Mod) -> % return the terms as-is, expecting them to be converted later
	Val;
validated_field_value(Val, {type, {TMod, Type}}, _Mod) -> % Val is a proplist to be turned into a type
	to_type(Val, TMod, Type);
validated_field_value(null, {atom, null}, _Mod) -> % jsx converts JSON null to 'null' atom
	null;
validated_field_value(Val, {atom, A}, _Mod) when is_binary(Val) ->
	case catch binary_to_existing_atom(Val, utf8) of
		A -> A;
		_ -> throw({no_conversion, Val, {atom, A}})
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
	list_to_tuple(ValidatedList);
validated_field_value(Val, Type, Mod) ->
	throw({no_conversion, Val, {Mod, Type}}).

set_field(Field, Value, RecMod, Record) ->
	RecMod:'#set-'([{Field, Value}], Record).

new_rec(ModuleName, TypeName) ->
	Constructor = list_to_atom("#new-" ++ atom_to_list(TypeName)),
	ModuleName:Constructor().

