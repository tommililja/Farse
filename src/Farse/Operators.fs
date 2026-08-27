namespace Farse

module Operators =

    /// <summary>Parses a required property.</summary>
    /// <remarks>
    /// Property names are matched with ordinal, case-sensitive comparison.
    /// The last occurrence is chosen when duplicate properties exist.
    /// </remarks>
    /// <example><code>let! int = "prop.prop2" &amp;= Parse.int</code></example>
    let inline (&=) path parser =
        Prop.get path parser

    /// <summary>Parses an optional property.</summary>
    /// <remarks>
    /// Property names are matched with ordinal, case-sensitive comparison.
    /// The last occurrence is chosen when duplicate properties exist.
    /// </remarks>
    /// <example><code>let! int = "prop.prop2" ?= Parse.int</code></example>
    let inline (?=) path parser =
        Prop.tryGet path parser

    /// <summary>Parses an optional property, distinguishing between a missing property and a null value.</summary>
    /// <remarks>
    /// Property names are matched with ordinal, case-sensitive comparison.
    /// The last occurrence is chosen when duplicate properties exist.
    /// </remarks>
    /// <example><code>let! int = "prop.prop2" ??= Parse.int</code></example>
    let inline (??=) path parser =
        Prop.tryGet2 path parser