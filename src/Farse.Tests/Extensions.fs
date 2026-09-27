namespace Farse.Tests

open System
open System.Buffers
open System.IO
open System.Text
open System.Threading.Tasks
open Expecto.Flip
open NodaTime.Text
open VerifyTests
open VerifyXunit
open Farse

module Json =

    let example =
        JObj [
            "id", JStr "c8eae96a-025d-4bc9-88f8-f204e95f2883"
            "name", JStr "Alice"
            "age", JNil
            "email", JStr "alice@domain.com"
            "profiles",
                JArr [
                    JStr "01458283-b6e3-4ae7-ae54-a68eb587cdc0"
                    JStr "927eb20f-cd62-470c-aafc-c3ce6b9248b0"
                    JStr "bf00d1e2-ee53-4969-9507-86bed7e96432"
                ]
            "subscription",
                JObj [
                    "plan", JStr "pro"
                    "isCanceled", JBit false
                    "renewsAt", JStr "2026-12-25T10:30:00Z"
                ]
            "tags",
                JArr [
                    JStr "beta"
                    JStr "verified"
                ]
        ]

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

    let parserError result =
        result
        |> Result.mapError ParserError.asString
        |> Expect.wantError "Expected a parser error."
        |> string

module Msg =

    let none = String.Empty

module MemoryStream =

    let ofString (string:string) =
        let bytes = Encoding.UTF8.GetBytes(string)
        new MemoryStream(bytes)

module String =

    let ofBytes (bytes:byte array) =
        Encoding.UTF8.GetString(bytes)

    let asBytes (string:string) =
        Encoding.UTF8.GetBytes(string)

module Instant =

    let asString = InstantPattern.General.Format

module ReadOnlyMemory =

    let ofString (json:string) =
        json
        |> String.asBytes
        |> ReadOnlyMemory<byte>

module ReadOnlySequence =

    let ofString (json:string) =
        json
        |> String.asBytes
        |> ReadOnlySequence<byte>

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