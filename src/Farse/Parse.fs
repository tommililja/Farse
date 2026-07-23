namespace Farse

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.Net
open System.Net.Mail
open System.Numerics
open System.Text.Json
open System.Text.RegularExpressions

module Parse =

    let inline private message<'r> article =
        $"Expected %s{article} %s{Type.getName typeof<'r>}."

    let inline private customInternal fn expectedKind : Parser<'r> =
        Parser (fun element ->
            match expectedKind with
            | ExpectedKind.Any
            | Equals element.ValueKind ->
                try fn element
                with ex ->
                    element
                    |> ParseError.invalidEx ex typeof<'r>
                    |> Error.list
            | _ ->
                element
                |> ParseError.expectedKind expectedKind JsonPath.empty typeof<'r>
                |> Error.list
        )

    /// <summary>Creates a custom parser.</summary>
    /// <example><code>
    /// let parser =
    ///     Parse.custom (fun element ->
    ///         let string = element.GetString()
    ///         match InstantPattern.General.Parse(string) with
    ///         | result when result.Success -> Ok result.Value
    ///         | result -> Error result.Exception.Message
    ///     ) ExpectedKind.String
    /// </code></example>
    let custom fn expectedKind : Parser<'r> =
        customInternal (fun element ->
            match fn element with
            | Ok x -> Ok x
            | Error msg ->
                element
                |> ParseError.invalid msg typeof<'r>
                |> Error.list
        ) expectedKind

    // Basic types

    let inline private tryParse article fn : Result<'r, _> =
        match fn () with
        | true, x -> Ok x
        | _ -> Error <| message<'r> article

    /// <summary>Parses a number as System.Int32.</summary>
    /// <example><code>let! int = "prop" &amp;= Parse.int</code></example>
    let int = custom (_.TryGetInt32 >> tryParse An) ExpectedKind.Number

    /// <summary>Parses a number as System.Int16.</summary>
    /// <example><code>let! int16 = "prop" &amp;= Parse.int16</code></example>
    let int16 = custom (_.TryGetInt16 >> tryParse An) ExpectedKind.Number

    /// <summary>Parses a number as System.Int64.</summary>
    /// <example><code>let! int64 = "prop" &amp;= Parse.int64</code></example>
    let int64 = custom (_.TryGetInt64 >> tryParse An) ExpectedKind.Number

    /// <summary>Parses a number as System.UInt16.</summary>
    /// <example><code>let! uint64 = "prop" &amp;= Parse.uint16</code></example>
    let uint16 = custom (_.TryGetUInt16 >> tryParse A) ExpectedKind.Number

    /// <summary>Parses a number as System.UInt32.</summary>
    /// <example><code>let! uint32 = "prop" &amp;= Parse.uint32</code></example>
    let uint32 = custom (_.TryGetUInt32 >> tryParse A) ExpectedKind.Number

    /// <summary>Parses a number as System.UInt64.</summary>
    /// <example><code>let! uint64 = "prop" &amp;= Parse.uint64</code></example>
    let uint64 = custom (_.TryGetUInt64 >> tryParse A) ExpectedKind.Number

    /// <summary>Parses a number as System.Double.</summary>
    /// <example><code>let! float = "prop" &amp;= Parse.float</code></example>
    let float = custom (_.TryGetDouble >> tryParse A) ExpectedKind.Number

    /// <summary>Parses a number as System.Single.</summary>
    /// <example><code>let! float32 = "prop" &amp;= Parse.float32</code></example>
    let float32 = custom (_.TryGetSingle >> tryParse A) ExpectedKind.Number

    /// <summary>Parses a number as System.Decimal.</summary>
    /// <example><code>let! decimal = "prop" &amp;= Parse.decimal</code></example>
    let decimal = custom (_.TryGetDecimal >> tryParse A) ExpectedKind.Number

    /// <summary>Parses a number as System.Byte.</summary>
    /// <example><code>let! byte = "prop" &amp;= Parse.byte</code></example>
    let byte = custom (_.TryGetByte >> tryParse A) ExpectedKind.Number

    /// <summary>Parses a number as System.SByte.</summary>
    /// <example><code>let! sbyte = "prop" &amp;= Parse.sbyte</code></example>
    let sbyte = custom (_.TryGetSByte >> tryParse An) ExpectedKind.Number

    /// <summary>Parses a string as System.Char.</summary>
    /// <example><code>let! char = "prop" &amp;= Parse.char</code></example>
    let char =
        custom (fun element ->
            match element.GetString() with
            | string when string.Length = 1 -> Ok string[0]
            | _ -> Error "Expected a string length of 1."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.String.</summary>
    /// <example><code>let! string = "prop" &amp;= Parse.string</code></example>
    let string = custom (_.GetString() >> Ok) ExpectedKind.String

    /// <summary>Parses a non-empty, non-whitespace string as System.String.</summary>
    /// <example><code>let! string = "prop" &amp;= Parse.stringNonEmpty</code></example>
    let stringNonEmpty =
        custom (fun element ->
            match element.GetString() with
            | string when String.isNotEmpty string -> Ok string
            | _ -> Error "Expected a non-empty string."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.String that matches a regular expression.</summary>
    /// <example><code>let! string = "prop" &amp;= Parse.regex "^[0-9]+$"</code></example>
    let regex ([<StringSyntax("Regex")>] regex:string) =
        custom (fun element ->
            try
                let string = element.GetString()
                if Regex.IsMatch(string, regex) then Ok string
                else Error $"Expected the string to match '%s{regex}'."
            with :? ArgumentException -> Error $"Invalid regular expression '%s{regex}'."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.Numerics.INumber.</summary>
    /// <example><code>let! int = "prop" &amp;= Parse.number&lt;int&gt;</code></example>
    let number<'r when 'r :> INumber<'r>> (_:Parser<'r>) =
        custom (fun element ->
            let string = element.GetString()
            match 'r.TryParse(string, NumberStyles.Any, CultureInfo.InvariantCulture) with
            | true, number -> Ok number
            | _ -> Error "Expected a number string."
        ) ExpectedKind.String

    /// <summary>Parses a base64 string as System.String.</summary>
    /// <example><code>let! string = "prop" &amp;= Parse.base64String</code></example>
    let base64String =
        custom (fun element ->
            match element.TryGetBytesFromBase64() with
            | true, _ -> Ok <| element.GetString()
            | _ -> Error "Expected a base64 string."
        ) ExpectedKind.String

    /// <summary>Parses a base64 string as System.Byte array.</summary>
    /// <example><code>let! bytes = "prop" &amp;= Parse.base64Bytes</code></example>
    let base64Bytes =
        custom (fun element ->
            match element.TryGetBytesFromBase64() with
            | true, bytes -> Ok bytes
            | _ -> Error "Expected a base64 string."
        ) ExpectedKind.String

    /// <summary>Parses a number as System.Numerics.BigInteger.</summary>
    /// <example><code>let! bigint = "prop" &amp;= Parse.bigint</code></example>
    let bigint : Parser<bigint> =
        custom (fun element ->
            let string = element.GetRawText()
            match BigInteger.TryParse(string, NumberStyles.Any, CultureInfo.InvariantCulture) with
            | true, bigint -> Ok bigint
            | _ -> Error "Expected a bigint."
        ) ExpectedKind.Number

    /// <summary>Parses a bool as System.Boolean.</summary>
    /// <example><code>let! bool = "prop" &amp;= Parse.bool</code></example>
    let bool = custom (_.GetBoolean() >> Ok) ExpectedKind.Bool

    /// <summary>Parses a string as System.Guid.</summary>
    /// <example><code>let! guid = "prop" &amp;= Parse.guid</code></example>
    let guid =
        custom (fun element ->
            match element.TryGetGuid() with
            | true, guid -> Ok guid
            | _ -> Error "Expected a Guid string."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.Guid with a specific format.</summary>
    /// <example><code>let! guid = "prop" &amp;= Parse.guidExact "N"</code></example>
    let guidExact ([<StringSyntax("GuidFormat")>] format:string) =
        custom (fun element ->
            let string = element.GetString()
            match Guid.TryParseExact(string, format) with
            | true, guid -> Ok guid
            | _ -> Error $"Expected a Guid string (%s{format})."
        ) ExpectedKind.String

    /// <summary>Parses null as FSharp.Core.Unit.</summary>
    /// <example><code>do! "prop" &amp;= Parse.unit</code></example>
    let unit = custom (ignore >> Ok) ExpectedKind.Null

    /// <summary>Always succeeds and returns FSharp.Core.Unit.</summary>
    /// <example><code>do! "prop" &amp;= Parse.none</code></example>
    let none = Parser.from ()

    // Enums

    let inline private parseEnum<'r, 'e when 'r: enum<'e>> article fn =
        let enumType = typeof<'r>
        match fn () with
        | true, x when Enum.IsDefined(enumType, x) -> Ok <| LanguagePrimitives.EnumOfValue<'e, 'r> x
        | true, _ -> Error $"Expected a value of %s{enumType.Name}."
        | _ -> Error <| message<'e> article

    /// <summary>Parses a string as an enum type.</summary>
    /// <example><code>let! enum = "prop" &amp;= Parse.enum&lt;Enum&gt;</code></example>
    let enum<'r when 'r :> Enum and 'r : struct and 'r : (new: unit -> 'r)> =
        custom (fun element ->
            let string = element.GetString()
            let enumType = typeof<'r>
            match Enum.TryParse<'r>(string, true) with
            | true, enum when Enum.IsDefined(enumType, enum) -> Ok enum
            | _ -> Error $"Expected a value of %s{enumType.Name}."
        ) ExpectedKind.String

    /// <summary>Parses a number as a System.Int32 enum.</summary>
    /// <example><code>let! enum = "prop" &amp;= Parse.intEnum&lt;Enum&gt;</code></example>
    let intEnum<'r when 'r : enum<int>> =
        custom (_.TryGetInt32 >> parseEnum<'r, int> An) ExpectedKind.Number

    /// <summary>Parses a number as a System.Int16 enum.</summary>
    /// <example><code>let! enum = "prop" &amp;= Parse.int16Enum&lt;Enum&gt;</code></example>
    let int16Enum<'r when 'r : enum<int16>> =
        custom (_.TryGetInt16 >> parseEnum<'r, int16> An) ExpectedKind.Number

    /// <summary>Parses a number as a System.Int64 enum.</summary>
    /// <example><code>let! enum = "prop" &amp;= Parse.int64Enum&lt;Enum&gt;</code></example>
    let int64Enum<'r when 'r : enum<int64>> =
        custom (_.TryGetInt64 >> parseEnum<'r, int64> An) ExpectedKind.Number

    /// <summary>Parses a number as a System.UInt16 enum.</summary>
    /// <example><code>let! enum = "prop" &amp;= Parse.uint16Enum&lt;Enum&gt;</code></example>
    let uint16Enum<'r when 'r : enum<uint16>> =
        custom (_.TryGetUInt16 >> parseEnum<'r, uint16> A) ExpectedKind.Number

    /// <summary>Parses a number as a System.UInt32 enum.</summary>
    /// <example><code>let! enum = "prop" &amp;= Parse.uint32Enum&lt;Enum&gt;</code></example>
    let uint32Enum<'r when 'r : enum<uint32>> =
        custom (_.TryGetUInt32 >> parseEnum<'r, uint32> A) ExpectedKind.Number

    /// <summary>Parses a number as a System.UInt64 enum.</summary>
    /// <example><code>let! enum = "prop" &amp;= Parse.uint64Enum&lt;Enum&gt;</code></example>
    let uint64Enum<'r when 'r : enum<uint64>> =
        custom (_.TryGetUInt64 >> parseEnum<'r, uint64> A) ExpectedKind.Number

    /// <summary>Parses a number as a System.Byte enum.</summary>
    /// <example><code>let! enum = "prop" &amp;= Parse.byteEnum&lt;Enum&gt;</code></example>
    let byteEnum<'r when 'r : enum<byte>> =
        custom (_.TryGetByte >> parseEnum<'r, byte> A) ExpectedKind.Number

    /// <summary>Parses a number as a System.SByte enum.</summary>
    /// <example><code>let! enum = "prop" &amp;= Parse.sbyteEnum&lt;Enum&gt;</code></example>
    let sbyteEnum<'r when 'r : enum<sbyte>> =
        custom (_.TryGetSByte >> parseEnum<'r, sbyte> An) ExpectedKind.Number

    // Date and time

    /// <summary>Parses a string as System.TimeOnly.</summary>
    /// <example><code>let! timeOnly = "prop" &amp;= Parse.timeOnly</code></example>
    let timeOnly =
        custom (fun element ->
            let string = element.GetString()
            match TimeOnly.TryParse(string, CultureInfo.InvariantCulture) with
            | true, timeOnly -> Ok timeOnly
            | _ -> Error "Expected a TimeOnly string."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.TimeOnly with a specific format.</summary>
    /// <example><code>let! timeOnly = "prop" &amp;= Parse.timeOnlyExact "HH:mm:ss"</code></example>
    let timeOnlyExact ([<StringSyntax("TimeOnlyFormat")>] format:string) =
        custom (fun element ->
            let string = element.GetString()
            match TimeOnly.TryParseExact(string, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, timeOnly -> Ok timeOnly
            | _ -> Error $"Expected a TimeOnly string (%s{format})."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.TimeSpan.</summary>
    /// <example><code>let! timeSpan = "prop" &amp;= Parse.timeSpan</code></example>
    let timeSpan =
        custom (fun element ->
            let string = element.GetString()
            match TimeSpan.TryParse(string, CultureInfo.InvariantCulture) with
            | true, timeSpan -> Ok timeSpan
            | _ -> Error "Expected a TimeSpan string."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.TimeSpan with a specific format.</summary>
    /// <example><code>let! timeSpan = "prop" &amp;= Parse.timeSpanExact "c"</code></example>
    let timeSpanExact ([<StringSyntax("TimeSpanFormat")>] format:string) =
        custom (fun element ->
            let string = element.GetString()
            match TimeSpan.TryParseExact(string, format, CultureInfo.InvariantCulture) with
            | true, timeSpan -> Ok timeSpan
            | _ -> Error $"Expected a TimeSpan string (%s{format})."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.DateOnly (ISO 8601).</summary>
    /// <example><code>let! dateOnly = "prop" &amp;= Parse.dateOnly</code></example>
    let dateOnly =
        custom (fun element ->
            let string = element.GetString()
            match DateOnly.TryParse(string, CultureInfo.InvariantCulture) with
            | true, dateOnly -> Ok dateOnly
            | _ -> Error "Expected a DateOnly string."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.DateOnly with a specific format.</summary>
    /// <example><code>let! dateOnly = "prop" &amp;= Parse.dateOnlyExact "yyyy-MM-dd"</code></example>
    let dateOnlyExact ([<StringSyntax("DateOnlyFormat")>] format:string) =
        custom (fun element ->
            let string = element.GetString()
            match DateOnly.TryParseExact(string, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, dateOnly -> Ok dateOnly
            | _ -> Error $"Expected a DateOnly string (%s{format})."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.DateTime (ISO 8601).</summary>
    /// <example><code>let! dateTime = "prop" &amp;= Parse.dateTime</code></example>
    let dateTime =
        custom (fun element ->
            match element.TryGetDateTime() with
            | true, dateTime -> Ok dateTime
            | _ -> Error "Expected a DateTime string."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.DateTime (ISO 8601) and converts it to UTC.</summary>
    /// <example><code>let! dateTime = "prop" &amp;= Parse.dateTimeUtc</code></example>
    let dateTimeUtc =
        custom (fun element ->
            match element.TryGetDateTime() with
            | true, dateTime -> Ok <| dateTime.ToUniversalTime()
            | _ -> Error "Expected a DateTime string."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.DateTime with a specific format.</summary>
    /// <example><code>let! dateTime = "prop" &amp;= Parse.dateTimeExact "yyyy-MM-dd HH:mm:ss"</code></example>
    let dateTimeExact ([<StringSyntax("DateTimeFormat")>] format:string) =
        custom (fun element ->
            let string = element.GetString()
            match DateTime.TryParseExact(string, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, dateTime -> Ok dateTime
            | _ -> Error $"Expected a DateTime string (%s{format})."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.DateTimeOffset (ISO 8601).</summary>
    /// <example><code>let! dateTimeOffset = "prop" &amp;= Parse.dateTimeOffset</code></example>
    let dateTimeOffset =
        custom (fun element ->
            match element.TryGetDateTimeOffset() with
            | true, dateTimeOffset -> Ok dateTimeOffset
            | _ -> Error "Expected a DateTimeOffset string."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.DateTimeOffset (ISO 8601) and converts it to UTC.</summary>
    /// <example><code>let! dateTimeOffset = "prop" &amp;= Parse.dateTimeOffsetUtc</code></example>
    let dateTimeOffsetUtc =
        custom (fun element ->
            match element.TryGetDateTimeOffset() with
            | true, dateTimeOffset -> Ok <| dateTimeOffset.ToUniversalTime()
            | _ -> Error "Expected a DateTimeOffset string."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.DateTimeOffset with a specific format.</summary>
    /// <example><code>let! dateTimeOffset = "prop" &amp;= Parse.dateTimeOffsetExact "yyyy-MM-dd HH:mm:ss zzz"</code></example>
    let dateTimeOffsetExact ([<StringSyntax("DateTimeFormat")>] format:string) =
        custom (fun element ->
            let string = element.GetString()
            match DateTimeOffset.TryParseExact(string, format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, dateTimeOffset -> Ok dateTimeOffset
            | _ -> Error $"Expected a DateTimeOffset string (%s{format})."
        ) ExpectedKind.String

    /// <summary>Parses a number as System.DateTimeOffset from a Unix timestamp in seconds.</summary>
    /// <example><code>let! unixSeconds = "prop" &amp;= Parse.unixSeconds</code></example>
    let unixSeconds =
        custom (fun element ->
            match element.TryGetInt64() with
            | true, seconds -> Ok <| DateTimeOffset.FromUnixTimeSeconds(seconds)
            | _ -> Error "Expected an int64 Unix timestamp (seconds)."
        ) ExpectedKind.Number

    /// <summary>Parses a number as System.DateTimeOffset from a Unix timestamp in milliseconds.</summary>
    /// <example><code>let! unixMilliseconds = "prop" &amp;= Parse.unixMilliseconds</code></example>
    let unixMilliseconds =
        custom (fun element ->
            match element.TryGetInt64() with
            | true, milliseconds -> Ok <| DateTimeOffset.FromUnixTimeMilliseconds(milliseconds)
            | _ -> Error "Expected an int64 Unix timestamp (milliseconds)."
        ) ExpectedKind.Number

    // Other

    /// <summary>Parses a string as System.Uri with a specific kind.</summary>
    /// <example><code>let! uri = "prop" &amp;= Parse.uri UriKind.Absolute</code></example>
    let uri (kind:UriKind) =
        custom (fun element ->
            let string = element.GetString()
            match Uri.TryCreate(string, kind) with
            | true, uri -> Ok uri
            | _ -> Error $"Expected a Uri string (%O{kind})."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.Version.</summary>
    /// <example><code>let! version = "prop" &amp;= Parse.version</code></example>
    let version =
        custom (fun element ->
            let string = element.GetString()
            match Version.TryParse(string) with
            | true, version -> Ok version
            | _ -> Error "Expected a Version string."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.Net.IPAddress.</summary>
    /// <example><code>let! ip = "prop" &amp;= Parse.ipAddress</code></example>
    let ipAddress =
        custom (fun element ->
            let string = element.GetString()
            match IPAddress.TryParse(string) with
            | true, ip -> Ok ip
            | _ -> Error "Expected an IPAddress string."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.Net.Mail.MailAddress.</summary>
    /// <example><code>let! email = "prop" &amp;= Parse.mailAddress</code></example>
    let mailAddress =
        custom (fun element ->
            let string = element.GetString()
            match MailAddress.TryCreate(string) with
            | true, email -> Ok email
            | _ -> Error "Expected a MailAddress string."
        ) ExpectedKind.String

    /// <summary>Parses a string as System.Globalization.RegionInfo (ISO 3166).</summary>
    /// <example><code>let! region = "prop" &amp;= Parse.regionInfo</code></example>
    let regionInfo = custom (_.GetString() >> RegionInfo >> Ok) ExpectedKind.String

    // Sequences

    let inline private parseIndex n (Parser parse) (element:JsonElement) =
        match parse (element.Item n) with
        | Ok x -> Ok x
        | Error errors ->
            errors
            |> List.map (ParseError.withIndex n)
            |> Error

    let inline private arr convert (Parser parse) : Parser<'r> =
        customInternal (fun element ->
            let mutable error = false
            let mutable enumerator = element.EnumerateArray()
            let mutable i = 0

            let items =
                element.GetArrayLength()
                |> Array.zeroCreate

            while not error && enumerator.MoveNext() do
                match parse enumerator.Current with
                | Ok x -> items[i] <- x; i <- i + 1
                | Error _ -> error <- true

            if error then
                element.EnumerateArray()
                |> List.ofSeq
                |> List.indexed
                |> List.collect (fun (i, element) ->
                    match parse element with
                    | Ok _ -> List.empty
                    | Error list ->
                        list
                        |> List.map (fun error ->
                            error
                            |> ParseError.withIndex i
                        )
                )
                |> Error
            else Ok <| convert items
        ) ExpectedKind.Array

    /// <summary>Parses an array as Microsoft.FSharp.Collections.seq.</summary>
    /// <remarks>Ignores null and invalid values.</remarks>
    /// <example><code>let! seq = "prop" &amp;= Parse.choose Parse.int</code></example>
    let choose (Parser parse) : Parser<'r seq> =
        customInternal (fun element ->
            element.EnumerateArray()
            |> Seq.choose (fun element ->
                match parse element with
                | Ok x -> Some x
                | Error _ -> None
            )
            |> Seq.toArray :> seq<_>
            |> Ok
        ) ExpectedKind.Array

    /// <summary>Parses an array as Microsoft.FSharp.Collections.list.</summary>
    /// <example><code>let! list = "prop" &amp;= Parse.list Parse.int</code></example>
    let list parser = arr List.ofSeq parser

    /// <summary>Parses an array as Microsoft.FSharp.Core.array.</summary>
    /// <example><code>let! array = "prop" &amp;= Parse.array Parse.int</code></example>
    let array parser = arr id parser

    /// <summary>Parses an array as Microsoft.FSharp.Collections.Set.</summary>
    /// <example><code>let! set = "prop" &amp;= Parse.set Parse.int</code></example>
    let set parser = arr Set.ofSeq parser

    /// <summary>Parses an array as System.Collections.Generic.HashSet.</summary>
    /// <example><code>let! hashSet = "prop" &amp;= Parse.hashSet Parse.int</code></example>
    let hashSet parser = arr HashSet parser

    /// <summary>Parses an array as Microsoft.FSharp.Collections.seq.</summary>
    /// <example><code>let! seq = "prop" &amp;= Parse.seq Parse.int</code></example>
    let seq parser = arr Seq.ofSeq parser

    /// <summary>Parses an array at a specific index.</summary>
    /// <example><code>let! int = "prop" &amp;= Parse.index 0 Parse.int</code></example>
    let index n parser : Parser<'r> =
        customInternal (fun (element:JsonElement) ->
            match element.GetArrayLength() with
            | length when n >= 0 && length >= n + 1 -> parseIndex n parser element
            | _ ->
                element
                |> ParseError.invalidIndex n typeof<'r>
                |> Error.list
        ) ExpectedKind.Array

    /// <summary>Parses the first element of an array.</summary>
    /// <example><code>let! int = "prop" &amp;= Parse.first Parse.int</code></example>
    let first parser = index 0 parser

    /// <summary>Parses the last element of an array.</summary>
    /// <example><code>let! int = "prop" &amp;= Parse.last Parse.int</code></example>
    let last parser : Parser<'r> =
        customInternal (fun (element:JsonElement) ->
            match element.GetArrayLength() with
            | length when length > 0 -> parseIndex (length - 1) parser element
            | _ ->
                element
                |> ParseError.invalidIndex 0 typeof<'r>
                |> Error.list
        ) ExpectedKind.Array

    // Key/Value

    let inline private getDuplicateKeys (pairs:('k * 'v) seq) =
        let seen = HashSet<'k>()
        pairs
        |> Seq.choose (fun (k, _) ->
            if seen.Add(k) then None
            else Some k
        )
        |> Seq.distinct
        |> List.ofSeq

    let inline private keyValue ([<InlineIfLambda>] convert) (Parser parse) : Parser<'r> =
        customInternal (fun (element:JsonElement) ->
            let mutable error = false
            let mutable enumerator = element.EnumerateObject()
            let mutable i = 0

            let items =
                element.GetPropertyCount()
                |> Array.zeroCreate

            while not error && enumerator.MoveNext() do
                let current = enumerator.Current
                match parse current.Value with
                | Ok x -> items[i] <- current.Name, x; i <- i + 1
                | Error _ -> error <- true

            if error then
                element.EnumerateObject()
                |> List.ofSeq
                |> List.collect (fun prop ->
                    match parse prop.Value with
                    | Ok _ -> List.empty
                    | Error list ->
                        list
                        |> List.map (fun error ->
                            error
                            |> ParseError.withProp prop.Name
                        )
                )
                |> Error
            else
                match getDuplicateKeys items with
                | [] -> Ok <| convert items
                | keys ->
                    keys
                    |> List.map (fun key -> ParseError.duplicateKey key typeof<'r> element)
                    |> Error
        ) ExpectedKind.Object

    /// <summary>Parses an object's properties as Microsoft.FSharp.Collections.Map.</summary>
    /// <example><code>let! map = "prop" &amp;= Parse.map Parse.int</code></example>
    let map parser = keyValue Map.ofSeq parser

    /// <summary>Parses an object's properties as System.Collections.Generic.IDictionary.</summary>
    /// <example><code>let! dict = "prop" &amp;= Parse.dict Parse.int</code></example>
    let dict parser = keyValue dict parser

    /// <summary>Parses an object's properties as System.Collections.Generic.KeyValuePair seq.</summary>
    /// <example><code>let! keyValuePairs = "prop" &amp;= Parse.keyValuePairs Parse.int</code></example>
    let keyValuePairs parser = keyValue (Seq.map KeyValuePair.Create) parser

    /// <summary>Parses an object's properties as tuple Microsoft.FSharp.Collections.seq.</summary>
    /// <example><code>let! tuples = "prop" &amp;= Parse.tuples Parse.int</code></example>
    let tuples parser = keyValue Seq.ofSeq parser

    /// <summary>Parses an object's keys as System.String Microsoft.FSharp.Collections.seq.</summary>
    /// <example><code>let! keys = "prop" &amp;= Parse.keys</code></example>
    let keys = keyValue (Seq.map fst) none

    // Tuples

    let inline private tuple expected ([<InlineIfLambda>] fn) : Parser<'r> =
        customInternal (fun (element:JsonElement) ->
            match element.GetArrayLength() with
            | actual when actual = expected -> fn element
            | actual ->
                element
                |> ParseError.invalidTuple actual expected typeof<'r>
                |> Error.list
        ) ExpectedKind.Array

    /// <summary>Parses an array with two values as a tuple.</summary>
    /// <example><code>let! tuple = "prop" &amp;= Parse.tuple2 Parse.int Parse.string</code></example>
    let tuple2 a b =
        tuple 2 (fun e ->
            result {
                let! a = parseIndex 0 a e
                and! b = parseIndex 1 b e

                return a, b
            }
        )

    /// <summary>Parses an array with three values as a tuple of three.</summary>
    /// <example><code>let! tuple = "prop" &amp;= Parse.tuple3 Parse.int Parse.string Parse.bool</code></example>
    let tuple3 a b c =
        tuple 3 (fun e ->
            result {
                let! a = parseIndex 0 a e
                and! b = parseIndex 1 b e
                and! c = parseIndex 2 c e

                return a, b, c
            }
        )

    /// <summary>Parses an array with four values as a tuple of four.</summary>
    /// <example><code>let! tuple = "prop" &amp;= Parse.tuple4 Parse.int Parse.string Parse.bool Parse.float</code></example>
    let tuple4 a b c d =
        tuple 4 (fun e ->
            result {
                let! a = parseIndex 0 a e
                and! b = parseIndex 1 b e
                and! c = parseIndex 2 c e
                and! d = parseIndex 3 d e

                return a, b, c, d
            }
        )

    /// <summary>Parses an array with five values as a tuple of five.</summary>
    /// <example><code>let! tuple = "prop" &amp;= Parse.tuple5 Parse.int Parse.string Parse.bool Parse.float Parse.int</code></example>
    let tuple5 a b c d e =
        tuple 5 (fun el ->
            result {
                let! a = parseIndex 0 a el
                and! b = parseIndex 1 b el
                and! c = parseIndex 2 c el
                and! d = parseIndex 3 d el
                and! e = parseIndex 4 e el

                return a, b, c, d, e
            }
        )

    // One-of

    /// <summary>Parses an object based on a discriminator property.</summary>
    /// <example><code>let! x = Parse.oneOf "type" [ "a", a; "b", b ]</code></example>
    let oneOf name parsers : Parser<'r> =
        customInternal (fun element ->
            let (Parser parse) = Prop.get name string
            match parse element with
            | Ok disc ->
                let parser = List.tryFind (fun (key, _) -> key = disc) parsers
                match parser with
                | Some (_, Parser parse) -> parse element
                | None ->
                    element
                    |> ParseError.missingParser disc typeof<'r>
                    |> Error.list
            | Error e -> Error e
        ) ExpectedKind.Object

    /// <summary>Creates a parser that can reference itself.</summary>
    /// <example><code>let! x = "prop" &amp;= Parse.self (fun self -> Parse.oneOf "type" [ "leaf", a; "branch", b self ])</code></example>
    let self fn =
        let self = ref (Parser (fun _ -> failwith "Uninitialized recursive parser."))
        let parser = fn (Parser (fun element -> let (Parser parse) = self.Value in parse element))
        self.Value <- parser
        parser

    /// <summary>Creates two parsers that can reference each other.</summary>
    /// <example><code>
    /// let valueParser, fieldParser =
    ///     Parse.mutual (fun (valueParser, fieldParser) ->
    ///         parser {
    ///             let! id = "id" &amp;= Parse.string
    ///             and! fields = "fields" &amp;= Parse.array fieldParser
    ///             return { Id = id; Fields = fields }
    ///         },
    ///         parser {
    ///             let! name = "name" &amp;= Parse.string
    ///             and! values = "values" &amp;= Parse.array valueParser
    ///             return { Name = name; Values = values }
    ///         }
    ///     )
    /// </code></example>
    let mutual fn =
        let refA = ref (Parser (fun _ -> failwith "Uninitialized recursive parser."))
        let refB = ref (Parser (fun _ -> failwith "Uninitialized recursive parser."))
        let parserA = Parser (fun element -> let (Parser parse) = refA.Value in parse element)
        let parserB = Parser (fun element -> let (Parser parse) = refB.Value in parse element)
        let a, b = fn (parserA, parserB)
        refA.Value <- a
        refB.Value <- b
        a, b

    /// <summary>Parses an element by trying each parser in order.</summary>
    /// <remarks>Returns the first that succeeds.</remarks>
    /// <example><code>let! x = Parse.attempt [ a; b ]</code></example>
    let attempt parsers : Parser<'r> =
        Parser (fun element ->
            match parsers with
            | [] ->
                element
                |> ParseError.emptyParsers typeof<'r>
                |> Error.list
            | _ ->
                let rec loop errors = function
                    | [] ->
                        element
                        |> ParseError.attempt errors typeof<'r>
                        |> Error.list
                    | Parser parse :: rest ->
                        match parse element with
                        | Ok x -> Ok x
                        | Error _ -> loop (errors + 1) rest
                loop 0 parsers
        )

    // Combinators

    /// <summary>Parses an optional value but returns a default value when null.</summary>
    /// <example><code>let! int = "prop" &amp;= Parse.nil Parse.int 1</code></example>
    let nil (Parser parse) x =
        Parser (fun (element:JsonElement) ->
            match element.ValueKind with
            | Kind.Null -> Ok x
            | _ -> parse element
        )

    /// <summary>Parses an optional value.</summary>
    /// <example><code>let! int = "prop" &amp;= Parse.option Parse.int</code></example>
    let option (Parser parse) =
        Parser (fun (element:JsonElement) ->
            match element.ValueKind with
            | Kind.Null -> Ok None
            | _ ->
                match parse element with
                | Ok x -> Ok <| Some x
                | Error e -> Error e
        )

    /// <summary>Refines a parsed value.</summary>
    /// <example><code>let! type' = "prop" &amp;= Parse.refine Parse.string Type.fromString</code></example>
    let refine (Parser parse) fn : Parser<'r> =
        Parser (fun element ->
            match parse element with
            | Ok x ->
                match fn x with
                | Ok x -> Ok x
                | Error msg ->
                    element
                    |> ParseError.validation $"%A{x}" msg typeof<'r>
                    |> Error.list
            | Error e -> Error e
        )

    /// <summary>Verifies a parsed value.</summary>
    /// <example><code>let! int = "prop" &amp;= Parse.verify Parse.int (fun x -> x > 0) "message"</code></example>
    let verify (Parser parse) fn msg : Parser<'r> =
        Parser (fun element ->
            match parse element with
            | Ok x when fn x -> Ok x
            | Ok x ->
                element
                |> ParseError.validation $"%A{x}" msg typeof<'r>
                |> Error.list
            | Error e -> Error e
        )

    /// <summary>Parses an exact value and returns FSharp.Core.Unit.</summary>
    /// <example><code>do! "prop" &amp;= Parse.exact Parse.int 1</code></example>
    let exact (Parser parse) (expected:'a) =
        Parser (fun element ->
            match parse element with
            | Ok x when x = expected -> Ok ()
            | Ok x ->
                element
                |> ParseError.expectedValue x expected typeof<'a>
                |> Error.list
            | Error e -> Error e
        )

    // Json

    /// <summary>Parses an element's kind as System.Text.Json.JsonValueKind.</summary>
    /// <example><code>let! jsonValueKind = "prop" &amp;= Parse.kind</code></example>
    let kind = custom (_.ValueKind >> Ok) ExpectedKind.Any

    /// <summary>Parses an element as System.Text.Json.JsonElement.</summary>
    /// <example><code>let! jsonElement = "prop" &amp;= Parse.element</code></example>
    let element = custom (JsonElement.clone >> Ok) ExpectedKind.Any

    /// <summary>Parses an element's raw text as System.String.</summary>
    /// <example><code>let! string = "prop" &amp;= Parse.rawText</code></example>
    let rawText = custom (_.GetRawText() >> Ok) ExpectedKind.Any

    /// <summary>Parses an array's length as System.Int32.</summary>
    /// <example><code>let! int = "prop" &amp;= Parse.arrayLength</code></example>
    let arrayLength = custom (_.GetArrayLength() >> Ok) ExpectedKind.Array

    /// <summary>Parses an object's property count as System.Int32.</summary>
    /// <example><code>let! int = "prop" &amp;= Parse.propertyCount</code></example>
    let propertyCount = custom (_.GetPropertyCount() >> Ok) ExpectedKind.Object