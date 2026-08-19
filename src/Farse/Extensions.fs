namespace Farse

open System.Runtime.CompilerServices

module Extensions =

    type Parser<'r> with

        // Sequences

        /// <summary>Parses an array as <c>'r Microsoft.FSharp.Collections.seq</c>.</summary>
        /// <remarks>Ignores null and invalid values.</remarks>
        /// <example><code>let! seq = "prop" &amp;= Parse.int.Choose()</code></example>
        member this.Choose() =
            Parse.choose this

        /// <summary>Parses an array as <c>'a Microsoft.FSharp.Collections.list</c>.</summary>
        /// <example><code>let! list = "prop" &amp;= Parse.int.List()</code></example>
        member this.List() =
            Parse.list this

        /// <summary>Parses an array as <c>'a Microsoft.FSharp.Core.array</c>.</summary>
        /// <example><code>let! array = "prop" &amp;= Parse.int.Array()</code></example>
        member this.Array() =
            Parse.array this

        /// <summary>Parses an array as <c>System.Collections.Generic.HashSet&lt;'a&gt;</c>.</summary>
        /// <example><code>let! hashSet = "prop" &amp;= Parse.int.HashSet()</code></example>
        member this.HashSet() =
            Parse.hashSet this

        /// <summary>Parses an array as <c>'a Microsoft.FSharp.Collections.seq</c>.</summary>
        /// <example><code>let! seq = "prop" &amp;= Parse.int.Seq()</code></example>
        member this.Seq() =
            Parse.seq this

        /// <summary>Parses an array at a specific index.</summary>
        /// <example><code>let! int = "prop" &amp;= Parse.int.Index(0)</code></example>
        member this.Index(index:int) =
            Parse.index index this

        /// <summary>Parses the first element of an array.</summary>
        /// <example><code>let! int = "prop" &amp;= Parse.int.First()</code></example>
        member this.First() =
            Parse.first this

        /// <summary>Parses the last element of an array.</summary>
        /// <example><code>let! int = "prop" &amp;= Parse.int.Last()</code></example>
        member this.Last() =
            Parse.last this

        // Key/Value

        /// <summary>Parses an object's properties as <c>Microsoft.FSharp.Collections.Map&lt;string, 'a&gt;</c>.</summary>
        /// <remarks>Fails when duplicate keys are found.</remarks>
        /// <example><code>let! map = "prop" &amp;= Parse.int.Map()</code></example>
        member this.Map() =
            Parse.map this

        /// <summary>Parses an object's properties as <c>System.Collections.Generic.IDictionary&lt;string, 'a&gt;</c>.</summary>
        /// <remarks>Fails when duplicate keys are found.</remarks>
        /// <example><code>let! dict = "prop" &amp;= Parse.int.Dict()</code></example>
        member this.Dict() =
            Parse.dict this

        /// <summary>Parses an object's properties as <c>System.Collections.Generic.KeyValuePair&lt;string, 'a&gt;</c> <c>Microsoft.FSharp.Collections.seq</c>.</summary>
        /// <remarks>Fails when duplicate keys are found.</remarks>
        /// <example><code>let! keyValuePairs = "prop" &amp;= Parse.int.KeyValuePairs()</code></example>
        member this.KeyValuePairs() =
            Parse.keyValuePairs this

        /// <summary>Parses an object's properties as <c>string * 'a</c> <c>Microsoft.FSharp.Collections.seq</c>.</summary>
        /// <remarks>Fails when duplicate keys are found.</remarks>
        /// <example><code>let! tuples = "prop" &amp;= Parse.int.Tuples()</code></example>
        member this.Tuples() =
            Parse.tuples this

        // Misc

        /// <summary>Parses an optional value but returns a default value when null.</summary>
        /// <example><code>let! int = "prop" &amp;= Parse.int.Nil(1)</code></example>
        member this.Nil(x) =
            Parse.nil this x

        /// <summary>Parses an optional value.</summary>
        /// <example><code>let! int = "prop" &amp;= Parse.int.Option()</code></example>
        member this.Option() =
            Parse.option this

        /// <summary>Catches all errors.</summary>
        /// <example><code>let! int = "prop" &amp;= Parse.int.Catch()</code></example>
        member this.Catch() =
            Parse.catch this

        /// <summary>Refines a parsed value.</summary>
        /// <example><code>let! type' = "prop" &amp;= Parse.string.Refine(Type.fromString)</code></example>
        member this.Refine(fn) =
            Parse.refine this fn

        /// <summary>Verifies a parsed value.</summary>
        /// <example><code>let! int = "prop" &amp;= Parse.int.Verify(fun x -> x > 0, "message")</code></example>
        member this.Verify(fn, msg) =
            Parse.verify this fn msg

    // Workaround: These need extra constraints.

    type Parse =

        /// <summary>Parses an exact value and returns <c>FSharp.Core.Unit</c>.</summary>
        /// <example><code>do! "prop" &amp;= Parse.int.Exact(1)</code></example>
        [<Extension>]
        static member Exact(parser, value) =
            Parse.exact parser value

        /// <summary>Parses an array as <c>Microsoft.FSharp.Collections.Set&lt;'a&gt;</c>.</summary>
        /// <example><code>let! set = "prop" &amp;= Parse.int.Set()</code></example>
        [<Extension>]
        static member Set(parser) =
            Parse.set parser