namespace Farse.Operators

open Farse

[<AutoOpen>]
module Operators =

    /// <summary>Parses a required property with the given parser.</summary>
    /// <example><code>let! int = "prop.prop2" &amp;= Parse.int</code></example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let inline (&=) path parser =
        Prop.req path parser

    /// <summary>Parses an optional property with the given parser.</summary>
    /// <example><code>let! int = "prop.prop2" ?= Parse.int</code></example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let inline (?=) path parser =
        Prop.opt path parser