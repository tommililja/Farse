namespace Farse

open System
open System.Collections.Generic
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

    let internal traverseImpl okFn nullFn notFoundFn (path:string) (parser:Parser<_>) : Parser<_> =
        fun (element:JsonElement) ->
            let mutable lastElement = element
            let mutable lastName = String.Empty
            try
                let element =
                    path.Split(".", StringSplitOptions.RemoveEmptyEntries)
                    |> Array.fold (fun (state:JsonElement) name ->
                        match state.ValueKind with
                        | JsonValueKind.Null -> raise <| NullPropertyException (lastName, lastElement)
                        | JsonValueKind.Object ->
                            lastName <- name
                            lastElement <- state

                            let next = state.GetProperty(name)

                            if next.ValueKind = JsonValueKind.Null then
                                raise <| NullPropertyException (lastName, lastElement)

                            next
                        | _ -> raise <| NotObjectException (name, state)
                    ) element

                match parser element with
                | Ok x -> Ok <| okFn x
                | Error e -> Error.parseError lastName e lastElement
            with
                | NotObjectException (name, element) -> Error.notObject name element
                | NullPropertyException (name, element) -> nullFn name element
                | ArrayException msg -> Error.parseError lastName msg lastElement
                | :? KeyNotFoundException -> notFoundFn lastName lastElement
                | :? InvalidOperationException -> Error.notObject lastName lastElement

    let internal traverse (path:string) (parser:Parser<_>) : Parser<_> =
        traverseImpl
           id
           Error.nullProperty
           Error.missingProperty
           path
           parser

    let internal tryTraverse (path:string) (parser:Parser<_>) : Parser<_> =
        traverseImpl
           Some
           (fun _ _ -> Ok None)
           (fun _ _ -> Ok None)
           path
           parser

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