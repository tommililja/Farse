namespace Farse

open System
open System.Text.Json

type Parser<'a> = JsonElement -> Result<'a, string>

module Parser =

    let from x : Parser<_> =
        fun _ -> Ok x

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

    let internal traverse (path:string) (parser:Parser<_>) : Parser<_> =
        fun (element:JsonElement) ->
            let mutable object = element
            let element =
                path.Split(".", StringSplitOptions.RemoveEmptyEntries)
                |> Array.fold (fun (state:Result<JsonElement,_>, _, path) name ->
                    let newPath =
                        if path = String.Empty
                        then name
                        else $"%s{path}.%s{name}"
                    match state with
                    | Ok state ->
                        if state.ValueKind = JsonValueKind.Object then
                            object <- state
                            match state.TryGetProperty(name) with
                            | true, prop when prop.ValueKind <> JsonValueKind.Null -> Ok prop, name, newPath
                            | false, _ -> Error.missingProperty name (Some path) state, name, newPath
                            | _ -> Error.nullProperty name (Some newPath) state, name, newPath
                        else Error.notObject name (Some path) state, name, newPath
                    | Error e -> Error e, name, newPath
                ) (Ok element, String.Empty, String.Empty)

            match element with
            | Ok x, name, path ->
                try
                    match parser x with
                    | Ok x -> Ok x
                    | Error msg -> Error.parseError name (Some path) msg object
                with ArrayException msg -> Error.parseError name (Some path) msg object
            | Error e, _, _ -> Error e

    let internal tryTraverse (path:string) (parser:Parser<_>) : Parser<_> =
        fun (element:JsonElement) ->
            let mutable object = element
            let element =
                path.Split(".", StringSplitOptions.RemoveEmptyEntries)
                |> Array.fold (fun (state:Result<JsonElement option,_>, _, path) name ->
                    let newPath =
                        if path = String.Empty
                        then name
                        else $"%s{path}.%s{name}"
                    match state with
                    | Ok None -> Ok None, name, newPath
                    | Ok (Some state) ->
                        if state.ValueKind = JsonValueKind.Object then
                            object <- state
                            match state.TryGetProperty(name) with
                            | true, prop when prop.ValueKind <> JsonValueKind.Null -> Ok <| Some prop, name, newPath
                            | _ -> Ok None, name, newPath
                        else Error.notObject name (Some path) state, name, newPath
                    | Error e -> Error e, name, newPath
                ) (Ok <| Some element, String.Empty, String.Empty)

            match element with
            | Ok (Some x), name, path ->
                try
                    match parser x with
                    | Ok x -> Ok <| Some x
                    | Error msg -> Error.parseError name (Some path) msg object
                with ArrayException msg -> Error.parseError name (Some path) msg object
            | Ok None, _, _ -> Ok None
            | Error e, _, _ -> Error e

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