namespace Farse

open System.Text.Json

module Prop =

    /// <summary>Parses a required property with the given parser.</summary>
    /// <code>let! int = Prop.req "prop.prop2" Parse.int</code>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let req path (parser:Parser<_>) : Parser<_> =
        match path with
        | Flat name ->
            fun (element:JsonElement) ->
                match element.ValueKind with
                | Kind.Object ->
                    let prop = JsonElement.getProperty name element
                    match parser prop with
                    | Ok x -> Ok x
                    | Error error ->
                        error
                        |> ParserError.enrich name element
                        |> Error
                | _ ->
                   CouldNotRead (name, element)
                   |> Error
        | Nested path ->
            fun element ->
                let mutable prop = Ok element
                let mutable name = path[0]
                let mutable object = element

                for segment in path do
                    prop <- prop
                    |> Result.bind (fun element ->
                        match element.ValueKind with
                        | Kind.Object ->
                            name <- segment
                            object <- element

                            element
                            |> JsonElement.getProperty name
                            |> Ok
                        | _ -> Error element
                    )

                match prop with
                | Ok prop ->
                    match parser prop with
                    | Ok x -> Ok x
                    | Error error ->
                        error
                        |> ParserError.enrich name object
                        |> Error
                | Error element ->
                    NotObject (name, object, element)
                    |> Error

    /// <summary>Parses an optional property with the given parser.</summary>
    /// <code>let! int = Prop.opt "prop.prop2" Parse.int</code>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let opt path (parser:Parser<_>) : Parser<_> =
        match path with
        | Flat name ->
            fun (element:JsonElement) ->
                match element.ValueKind with
                | Kind.Object ->
                    let prop = JsonElement.tryGetProperty name element
                    match prop with
                    | Some prop ->
                        match parser prop with
                        | Ok x -> Ok <| Some x
                        | Error error ->
                            error
                            |> ParserError.enrich name element
                            |> Error
                    | None -> Ok None
                | _ ->
                    CouldNotRead (name, element)
                    |> Error
        | Nested path ->
            fun element ->
                let mutable prop = Ok <| Some element
                let mutable name = path[0]
                let mutable object = element

                for segment in path do
                    prop <- prop
                    |> ResultOption.bind (fun element ->
                        match element.ValueKind with
                        | Kind.Object ->
                            name <- segment
                            object <- element

                            element
                            |> JsonElement.tryGetProperty name
                            |> Ok
                        | _ -> Error element
                    )

                match prop with
                | Ok (Some prop) ->
                    match parser prop with
                    | Ok x -> Ok <| Some x
                    | Error error ->
                        error
                        |> ParserError.enrich name object
                        |> Error
                | Ok None -> Ok None
                | Error element ->
                    NotObject (name, object, element)
                    |> Error