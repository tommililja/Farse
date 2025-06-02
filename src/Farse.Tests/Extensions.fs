namespace Farse.Tests

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
        Verifier.VerifyJson(actual, settings = settings).ToTask()

    let string (actual:string) =
        Verifier.Verify(actual, settings).ToTask()

module Expect =

    let ok x = Expect.wantOk x "Expected Ok"

    let none x = Expect.isNone x "Expected None"

    let error x = Expect.wantError x "Expected Error"

    let equal actual expected =
        Expect.equal actual expected "Expected values to be equal"