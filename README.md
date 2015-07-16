
aeon
====

Another Erlang to Object Notation translator

## What it is ##

A validating / type-enforcing Erlang record <=> JSON
converter built on top of [JSX](https://github.com/talentdeficit/jsx) supporting
deeply nested types.

**aeon** bridges the gap between records and Erlang <=> JSON mapping libraries
like jsx. jsx can convert many Erlang terms to/from JSON, but cannot deal with
Erlang records or tuples. **aeon** can take an Erlang record and turn it into a
form that JSX can then turn into JSON. It can then do the reverse, taking the
JSX-generated terms and re-assemble them into a record.

**aeon** can do the same with arbitrary tuples as long as they follow a defined
`-type`.

Roundtripping is important.  Any valid Erlang term with an external
representation (meaning no funs) should be able to roundtrip to JSON and back.
Numbers, binaries, strings, atoms, records, tuples and lists in any combination
should have no problems. Caveats of unicode strings and numbers mentioned in the
[JSX docs](https://github.com/talentdeficit/jsx/tree/develop#json---erlang-mapping)
apply.

## How it works ##

**aeon** uses a parse transform to store the record and type information defined
in a module and retrieve it at runtime.  This type information is used to drive
the construction of the record(s) and validate field types along the way.
exprecs, part of `parse_trans` is used to create records and get/set fields.
**aeon** uses `parse_trans` to accomplish it's parse transformation.

## Dependencies ##

Modules defining the types to be translated must be compiled with the
`runtime_types` (part of **aeon**) and
`exprecs` (part of [parse_trans](https://github.com/uwiger/parse_trans)) parse transforms.

jsx, or any library using the same JSON <=> Erlang mapping, is a runtime
dependency.  **aeon** does not handle JSON directly.

## Examples ##

In a module "test.erl", define a 'user' record.

    -module(test).
    
    -compile({parse_transform, runtime_types}).
    -compile({parse_transform, exprecs}).
    
    -type privilege() :: login | create | delete | grant.

    -record(user, {
              name :: binary(),
              age :: integer(),
              height :: float(),
              birthday :: {Year :: integer(),
                           Month :: integer(),
                           Day :: integer()},
              privileges :: [privilege()]
             }).

    -export_type([privilege/0]).
    -export_records([user]).

Breaking this down, here's what's going on:

* line 3 tells the compiler to use a parse_transform that makes the type
  information available at runtime.  Usually it is discarded during
  compliation.

* line 4 tell the compiler to use a parse_transform that creates functions for
  introspecting and manipulating records, in this case the 'user' record

* line 6 defines a type, 'privilege' that will be used later.  In this case,
  privilege is an attribute of the user record.

* lines 8-16 define the user record and, importantly, the type of each field.
  aeon will use this type information to convert the JSON into the record, and
  ensure that the record matches the specified types.  If the JSON type can be
  converted into the specified type, it will be.  For example, JSON does not
  distinguish between integer and float.  JSON does't have atoms either, but
  aeon will try to convert JSON strings into atoms, as long as the atom already
  exists.  Since we've defined the privilege type, the atoms 'login', 'create',
  'delete' and 'grant' already exist.  If the JSON had a string like "update"
  in the "privileges" field, the conversion would fail since it doesn't match
  the specification.

* line 18 exports the defined privilege type.  aeon does not need the type
  defined in order to use it, but you may want to use this type elsewhere.

* line 19 exports the 'user' record.  This is required for aeon to encode and
  decode the 'user' record.

Now that everything is set up, here's how you could use this to actually do
something.  Here is how to turn a record into JSON, which could then be sent to
a browser.

    User = #user{
              name = <<"Garret Smith">>,
              age = 34,
              height = 6.0,
              birthday = {1982, 06, 29},
              privileges = [login, create, delete, grant]
             },
    Json = jsx:encode(aeon:record_to_jsx(User, test)),
    io:fwrite("~s~n", [Json]),

This would print the JSON string:

    {"name":"Garret Smith","age":34,"height":6.0,"birthday":[1982,6,29],"privileges":["login","create","delete","grant"]}

How about the other direction?  Validating input is extremely important.  Never
trust that the data you receive back from a browser.  If it isn't correct, it
could make your program behave strangley sometime down the road.  To turn some
JSON back into an Erlang record, asserting that the record matches the specified
type information:

    User1 = aeon:to_record(jsx:decode(Json), test, user),
    User = User1,

The record created from the JSON, User1, exactly matches the original User
record.

## Inspirations ##

* [json_rec](https://github.com/justinkirby/json_rec)
* [sherrif](https://github.com/extend/sheriff)
* [Destructuring JSON in Erlang Made Easy](http://www.progski.net/blog/2010/destructuring_json_in_erlang_made_easy.html)

