namespace Farse.Tests

open System
open Expecto
open VerifyTests
open VerifyXunit

module Verify =

    let private settings =
        let settings = VerifySettings()
        settings.UseDirectory("Verify")
        settings.DisableDiff()
        settings

    let json (actual:string) =
        Verifier.VerifyJson(actual, settings).ToTask()

    let string (actual:string) =
        Verifier.Verify(actual, settings).ToTask()

module Expect =

    let ok x = Expect.wantOk x String.Empty

    let none x = Expect.isNone x String.Empty

    let error x = Expect.wantError x String.Empty

    let errorString x = error x |> Verify.string

    let equal actual expected =
        Expect.equal actual expected String.Empty