
-module(aeon).

% Another Erlang to Object Notation translator
% A validating / type-enforcing Erlang record <=> JSON
% converter built on top of JSX supporting deeply nested types

% Requires that the modules defining the types be compiled
% with the runtime_types and exprecs parse transforms

-export([
	 to_record/3,
	 to_type/3,
	 record_to_jsx/2,
	 type_to_jsx/3
	]).

% (JSON -> erlang) The field may not be supplied in the JSON.  This will cause conversion to fail unless this flag is supplied.
-opaque optional_field() :: optional_field.

% (erlang -> JSON) when converting a field to JSON, suppress it if the value matches the parameter.
-opaque suppress(T) :: {suppress, T}.

% (erlang <-> JSON) Ignore the field if supplied in the JSON, never add the record value to the JSON.
-opaque excluded_field() :: excluded_field.

% (erlang <-> JSON) (->erlang) Assume that the value contains JSX-ready erlang terms.  (->JSON) Leave JSON terms un-translated.
-opaque json_terms() :: json_terms.

-export_type([
	      optional_field/0,
	      excluded_field/0,
	      suppress/1,
	      json_terms/0
	     ]).

to_record(Jsx, Module, RecordType) ->
	aeon_to_erl:to_record(Jsx, Module, RecordType).

to_type(Jsx, Module, TypeName) ->
	aeon_to_erl:to_type(Jsx, Module, TypeName).

record_to_jsx(Record, Module) ->
	aeon_to_jsx:record_to_jsx(Record, Module).

type_to_jsx(Term, Module, TypeName) ->
	aeon_to_jsx:type_to_jsx(Term, Module, TypeName).

