namespace Farse.Tests

open System
open System.IO
open System.Text
open System.Threading.Tasks
open VerifyTests
open VerifyXunit
open Expecto
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

    let ok x = Expect.wantOk x String.Empty

    let error x = Expect.wantError x String.Empty

    let errorString x =
        Result.mapError ParserError.asString x
        |> error
        |> string

    let isTrue actual =
        Expect.isTrue actual String.Empty

    let isFalse actual =
        Expect.isFalse actual String.Empty

    let isError actual =
        Expect.isError actual String.Empty

    let equal actual expected =
        Expect.equal actual expected String.Empty

    let notEqual actual expected =
        Expect.notEqual actual expected String.Empty

    let equalSeq actual expected =
        Expect.sequenceEqual actual expected String.Empty

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