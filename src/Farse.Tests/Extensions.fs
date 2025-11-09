namespace Farse.Tests

open System
open Expecto
open VerifyTests
open VerifyXunit

module Expect =

    let private settings =
        let settings = VerifySettings()
        settings.UseDirectory("Verify")
        settings.DisableDiff()
        settings

    let string (actual:string) =
        Verifier.Verify(actual, settings).ToTask()

    let ok x = Expect.wantOk x String.Empty

    let error x = Expect.wantError x String.Empty

    let errorString x = error x |> string

    let isTrue actual =
        Expect.isTrue actual String.Empty

    let equal actual expected =
        Expect.equal actual expected String.Empty

    let equalSeq actual expected =
        Expect.sequenceEqual actual expected String.Empty