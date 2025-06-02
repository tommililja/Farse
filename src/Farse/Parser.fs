namespace Farse

open System
open System.Text.Json

type Parser<'a> = JsonElement -> Result<'a, string>

module Parser =

    let from x : Parser<_> = fun _ -> Ok x

    let bind (fn:_ -> Parser<_>) (parser:Parser<_>) =
        fun element ->
            match parser element with
            | Ok x -> fn x <| element
            | Error e -> Error e

    let validate fn (parser:Parser<_>) : Parser<_> =
        fun element ->
            match parser element with
            | Ok x -> fn x
            | Error e -> Error e

    let map fn x : Parser<_> = bind (fn >> from) x

    /// Traverses the JSON document to an element.
    /// <param name="list">The path to traverse.</param>
    /// <param name="parser">The parser used to parse the element.</param>
    let traverse (path:string list) (parser:Parser<_>) : Parser<_> =
        fun (element:JsonElement) ->
            let mutable object = element
            let element =
                path
                |> List.fold (fun (state:Result<JsonElement,_>, _) name ->
                    match state with
                    | Ok state ->
                        if state.ValueKind = JsonValueKind.Object then
                            object <- state
                            match state.TryGetProperty(name) with
                            | true, prop when prop.ValueKind <> JsonValueKind.Null -> Ok prop, name
                            | _ -> Error.nullProperty name state, name
                        else Error.notObject name state, name
                    | Error e -> Error e, name
                ) (Ok element, String.Empty)

            match element with
            | Ok x, name ->
                try
                    match parser x with
                    | Ok x -> Ok x
                    | Error msg -> Error.parseError name msg object
                with ArrayException msg -> Error.parseError name msg object
            | Error e, _ -> Error e

    /// Tries to traverse the JSON document to an element.
    /// <param name="list">The path to traverse.</param>
    /// <param name="parser">The parser used to parse the element.</param>
    let tryTraverse (path:string list) (parser:Parser<_>) : Parser<_> =
        fun (element:JsonElement) ->
            let mutable object = element
            let element =
                path
                |> List.fold (fun (state:Result<JsonElement option,_>, _) name ->
                    match state with
                    | Ok None -> Ok None, name
                    | Ok (Some state) ->
                        if state.ValueKind = JsonValueKind.Object then
                            object <- state
                            match state.TryGetProperty(name) with
                            | true, prop when prop.ValueKind <> JsonValueKind.Null -> Ok <| Some prop, name
                            | _ -> Ok None, name
                        else Error.notObject name state, name
                    | Error e -> Error e, name
                ) (Ok <| Some element, String.Empty)

            match element with
            | Ok (Some x), name ->
                try
                    match parser x with
                    | Ok x -> Ok (Some x)
                    | Error msg -> Error.parseError name msg object
                with ArrayException msg -> Error.parseError name msg object
            | Ok None, _ -> Ok None
            | Error e, _ -> Error e

    /// <summary>Parses a JSON string using the supplied parser.</summary>
    /// <param name="json">The JSON string to parse.</param>
    /// <param name="parser">The parser used to parse the JSON string.</param>
    let parse (json:string) (parser:Parser<_>) =
        try
            match json with
            | NullOrEmpty -> Error.nullOrEmptyJson ()
            | String json ->
                use document = JsonDocument.Parse(json)
                parser document.RootElement
        with
            | :? JsonException as exn -> Error.invalidJson json exn