namespace Farse

module Operators =

    /// <summary>Parses a required property.</summary>
    /// <example>let! int = "prop.prop2" &amp;= Parse.int</example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let inline (&=) path parser =
        Prop.req path parser

    /// <summary>Parses an optional property.</summary>
    /// <example>let! int = "prop.prop2" ?= Parse.int</example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let inline (?=) path parser =
        Prop.opt path parser