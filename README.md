
aeon
====

Another Erlang to Object Notation translator

## What it is ##

A validating / type-enforcing Erlang record <=> JSON
converter built on top of [JSX](jsx) supporting deeply nested types.

**aeon** bridges the gap between records and Erlang <=> JSON mapping libraries
like [JSX](jsx). [JSX](jsx) can convert many Erlang terms to/from JSON, but
cannot deal with Erlang records or tuples. **aeon** can take an Erlang record
and turn it into a form that JSX can then turn into JSON. It can then do the
reverse, taking the JSX-generated terms and re-assemble them into a record.

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
`runtime_types` (part of **aeon**) and [exprecs](exprecs) parse transforms.

[JSX](jsx), or any library using the same JSON <=> Erlang mapping, is a
runtime dependency.  **aeon** does not handle JSON directly.

## Examples ##

## Inspirations ##

* [json_rec](json_rec)
* [sherrif](sherrif)
* [Destructuring JSON in Erlang Made Easy](http://www.progski.net/blog/2010/destructuring_json_in_erlang_made_easy.html)

