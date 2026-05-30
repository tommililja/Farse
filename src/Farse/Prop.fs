namespace Farse

open System
open System.Text.Json

module Prop =

    let inline private createPath array =
        array
        |> Array.map (sprintf ".%s")
        |> String.concat String.Empty
        |> JsonPath

    let inline private jsonPath path count =
        path
        |> Array.take count
        |> createPath

    let inline private fold path element =
        path
        |> Array.fold (fun (element:JsonElement, count) (name:string) ->
            match element.ValueKind with
            | Kind.Object -> element.TryGetProperty(name) |> snd, count + 1
            | _ -> element, count
        ) (element, 0)

    let private parse (name:string) (Parser parse) : Parser<'r> =
        Parser (fun element ->
            match element.ValueKind with
            | Kind.Object ->
                match element.TryGetProperty(name) with
                | true, prop ->
                    match parse prop with
                    | Ok x -> Ok x
                    | Error errors ->
                        errors
                        |> List.map (ParseError.withProp name)
                        |> Error
                | _, prop ->
                    prop
                    |> ParseError.required (JsonPath.prop name) typeof<'r>
                    |> Error.list
            | _ ->
                element
                |> ParseError.expectedKind ExpectedKind.Object JsonPath.empty typeof<'r>
                |> Error.list
        )

    let private tryParse (name:string) (Parser parse) : Parser<'r option> =
        Parser (fun element ->
            match element.ValueKind with
            | Kind.Object ->
                match element.TryGetProperty(name) with
                | true, prop when prop.isNotNull ->
                    match parse prop with
                    | Ok x -> Ok <| Some x
                    | Error errors ->
                        errors
                        |> List.map (ParseError.withProp name)
                        |> Error
                | _ -> Ok None
            | _ ->
                element
                |> ParseError.expectedKind ExpectedKind.Object JsonPath.empty typeof<'r>
                |> Error.list
        )

    let private tryParseNull (name:string) (Parser parse) : Parser<'r option option> =
        Parser (fun element ->
            match element.ValueKind with
            | Kind.Object ->
                match element.TryGetProperty(name) with
                | true, prop when prop.isNull -> Ok <| Some None
                | true, prop ->
                    match parse prop with
                    | Ok x -> Ok <| Some (Some x)
                    | Error errors ->
                        errors
                        |> List.map (ParseError.withProp name)
                        |> Error
                | _ -> Ok None
            | _ ->
                element
                |> ParseError.expectedKind ExpectedKind.Object JsonPath.empty typeof<'r>
                |> Error.list
        )

    let private traverse (path:string array) (Parser parse) : Parser<'r> =
        Parser (fun element ->
            match fold path element with
            | element, count when count = path.Length && element.isUndefined ->
                element
                |> ParseError.required (jsonPath path count) typeof<'r>
                |> Error.list
            | element, count when count = path.Length ->
                match parse element with
                | Ok x -> Ok x
                | Error errors ->
                    errors
                    |> List.map (
                        createPath path
                        |> ParseError.withPath
                    )
                    |> Error
            | element, count ->
                element
                |> ParseError.expectedKind ExpectedKind.Object (jsonPath path count) typeof<'r>
                |> Error.list
        )

    let private tryTraverse (path:string array) (Parser parse) : Parser<'r option> =
        Parser (fun element ->
            match fold path element with
            | element, _ when element.isNullOrUndefined -> Ok None
            | element, count when count = path.Length ->
                match parse element with
                | Ok x -> Ok <| Some x
                | Error errors ->
                    errors
                    |> List.map (
                        createPath path
                        |> ParseError.withPath
                    )
                    |> Error
            | element, count ->
                element
                |> ParseError.expectedKind ExpectedKind.Object (jsonPath path count) typeof<'r>
                |> Error.list
        )

    let private tryTraverseNull (path:string array) (Parser parse) : Parser<'r option option> =
        Parser (fun element ->
            match fold path element with
            | element, count when element.isNull && count = path.Length -> Ok <| Some None
            | element, _ when element.isNullOrUndefined -> Ok None
            | element, count when count = path.Length ->
                match parse element with
                | Ok x -> Ok <| Some (Some x)
                | Error errors ->
                    errors
                    |> List.map (
                        createPath path
                        |> ParseError.withPath
                    )
                    |> Error
            | element, count ->
                element
                |> ParseError.expectedKind ExpectedKind.Object (jsonPath path count) typeof<'r>
                |> Error.list
        )

    /// <summary>Parses a required property.</summary>
    /// <example><code>let! int = Prop.req "prop.prop2" Parse.int</code></example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let req path parser =
        match path with
        | Prop name -> parse name parser
        | Path path -> traverse path parser

    /// <summary>Parses an optional property.</summary>
    /// <example><code>let! int = Prop.opt "prop.prop2" Parse.int</code></example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let opt path parser =
        match path with
        | Prop name -> tryParse name parser
        | Path path -> tryTraverse path parser

    /// <summary>Parses an optional property.</summary>
    /// <remarks>Distinguishes between a missing property and a null value.</remarks>
    /// <example><code>let! int = Prop.tryOpt "prop.prop2" Parse.int</code></example>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let tryOpt path parser =
        match path with
        | Prop name -> tryParseNull name parser
        | Path path -> tryTraverseNull path parser