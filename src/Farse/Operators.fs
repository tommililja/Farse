namespace Farse

module Operators =

    /// <summary>Parses a required property.</summary>
    /// <example><code>let! int = "prop.prop2" &amp;= Parse.int</code></example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let inline (&=) path parser =
        Prop.req path parser

    /// <summary>Parses an optional property.</summary>
    /// <example><code>let! int = "prop.prop2" ?= Parse.int</code></example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let inline (?=) path parser =
        Prop.opt path parser

    /// <summary>Parses an optional property.</summary>
    /// <remarks>Distinguishes between a missing property and a null value.</remarks>
    /// <example><code>let! int = "prop.prop2" ??= Parse.int</code></example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let inline (??=) path parser =
        Prop.tryOpt path parser