namespace Farse.Tests

open System
open System.IO
open System.Text
open System.Threading.Tasks
open Expecto.Flip
open VerifyTests
open VerifyXunit
open Farse

module Expect =

    let private settings =
        let settings = VerifySettings()
        settings.UseDirectory("Verify")
        settings.DisableDiff()
        settings

    let string (actual:string) =
        Verifier
            .Verify(actual, settings)
            .ToTask()

    let wantErrorString result =
        result
        |> Result.mapError ParserError.asString
        |> Expect.wantError "Expected a parser error."
        |> string

module Msg =

    let none = String.Empty

module MemoryStream =

    let create (string:string) =
        let bytes = Encoding.UTF8.GetBytes(string)
        new MemoryStream(bytes)

module Task =

    let map fn x =
        task {
            let! x = x
            return fn x
        }

    let bind (fn:'a -> Task<'b>) x =
        task {
            let! x = x
            return! fn x
        }