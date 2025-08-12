namespace Farse.Operators

open Farse

[<AutoOpen>]
module Operators =
    open Parse

    /// <summary>Parses a required property with the given parser.</summary>
    /// <example>
    /// <code>
    /// "prop.prop2" &amp;= Parse.int
    /// </code>
    /// </example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let inline (&=) path parser =
        req path parser

    /// <summary>Parses an optional property with the given parser.</summary>
    /// <example>
    /// <code>
    /// "prop.prop2" ?= Parse.int
    /// </code>
    /// </example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let inline (?=) path parser =
        opt path parser