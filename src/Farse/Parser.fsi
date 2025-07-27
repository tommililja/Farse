namespace Farse

// This seems to solve most type alias problems, but not all.
// For some reason it also increases memory allocations...

module Parser =

    val from: 'a -> Parser<'a>

    // Refuses to work in the implementation file.
    val bind: ('a -> Parser<'b>) -> Parser<'a> -> Parser<'b>

    val map: ('a -> 'b) -> Parser<'a> -> Parser<'b>

    val ignore: Parser<'a> -> Parser<unit>

    val validate: ('a -> Result<'b, string>) -> Parser<'a> -> Parser<'b>

    val parse: string -> Parser<'a> -> Result<'a, string>