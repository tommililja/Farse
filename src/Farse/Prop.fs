namespace Farse

open System.Text.Json

module Prop =

    /// <summary>Parses a required property with the given parser.</summary>
    /// <code>let! int = Prop.req "prop.prop2" Parse.int</code>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let req path (parser:Parser<_>) : Parser<_> =
        match path with
        | Prop name ->
            fun (element:JsonElement) ->
                match element.ValueKind with
                | Kind.Object ->
                    let prop = JsonElement.getProperty name element
                    parser prop
                    |> Result.bindError (fun error ->
                        let path = JsonPath.prop name
                        ParserError.enrich path error
                    )
                | _ -> InvalidKind.create ExpectedKind.Object element
        | Path arr ->
            fun element ->
                let prop, path =
                    arr
                    |> Seq.fold (fun (prop, path) name ->
                        match prop with
                        | Ok (element:JsonElement) ->
                            match element.ValueKind with
                            | Kind.Object ->
                                element
                                |> JsonElement.getProperty name
                                |> Ok, JsonPath.append path (JsonPath.prop name)
                            | _ -> Error element, path
                        | Error e -> Error e, path
                    ) (Ok element, JsonPath.empty)

                match prop with
                | Ok prop ->
                    parser prop
                    |> Result.bindError (ParserError.enrich path)
                | Error element ->
                    ExpectedKind.Object
                    |> CouldNotParse.invalidKind path element

    /// <summary>Parses an optional property with the given parser.</summary>
    /// <code>let! int = Prop.opt "prop.prop2" Parse.int</code>
    /// <param name="path">The path to the property.</param>
    /// <param name="parser">The parser used to parse the property value.</param>
    let opt path (parser:Parser<_>) : Parser<_> =
        match path with
        | Prop name ->
            fun (element:JsonElement) ->
                match element.ValueKind with
                | Kind.Object ->
                    let prop = JsonElement.tryGetProperty name element
                    match prop with
                    | Some prop ->
                        match parser prop with
                        | Ok x -> Ok <| Some x
                        | Error error ->
                            let path = JsonPath.prop name
                            ParserError.enrich path error
                    | None -> Ok None
                | _ -> InvalidKind.create ExpectedKind.Object element
        | Path arr ->
            fun element ->
                let prop, path =
                    arr
                    |> Seq.fold (fun (prop, path) name ->
                        match prop with
                        | Ok (Some (element:JsonElement)) ->
                            match element.ValueKind with
                            | Kind.Object ->
                                element
                                |> JsonElement.tryGetProperty name
                                |> Ok, JsonPath.append path (JsonPath.prop name)
                            | _ -> Error element, path
                        | _ -> prop, path
                    ) (Ok (Some element), JsonPath.empty)

                match prop with
                | Ok (Some prop) ->
                    match parser prop with
                    | Ok x -> Ok <| Some x
                    | Error error -> ParserError.enrich path error
                | Ok None -> Ok None
                | Error element ->
                    ExpectedKind.Object
                    |> CouldNotParse.invalidKind path element