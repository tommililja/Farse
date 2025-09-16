namespace Farse.Tests

open Farse
open Farse.Operators
open NodaTime
open NodaTime.Text
open System
open System.IO
open System.Text.Json
open Xunit

type UserId = UserId of Guid

module UserId =

    let asString (UserId x) =
        string x

type Age = Age of byte

module Age =

    let fromByte = function
        | age when age >= 12uy -> Ok <| Age age
        | age -> Error $"%u{age} is not a valid age."

    let asByte (Age x) = x

type Email = Email of string

module Email =

    let fromString =
        // Some validation.
        Email >> Ok

    let asString (Email x) = x

type ProfileId = ProfileId of Guid

module ProfileId =

    let asString (ProfileId x) =
        string x

type Plan =
    | Pro
    | Standard
    | Free

module Plan =

    let fromString = function
        | "Pro" -> Ok Pro
        | "Standard" -> Ok Standard
        | "Free" -> Ok Free
        | str -> Error $"%s{str} is not a valid plan."

    let asString = function
        | Pro -> "Pro"
        | Standard -> "Standard"
        | Free -> "Free"

type Subscription = {
    Plan: Plan
    IsCanceled: bool
    RenewsAt: Instant option
}

type User = {
    Id: UserId
    Name: string
    Age: Age option
    Email: Email
    Profiles: ProfileId Set
    Subscription: Subscription
}

module Parse =

    // Custom parser example.
    let instant =
        Parse.custom (fun element ->
            let string = element.GetString()
            match InstantPattern.General.Parse(string) with
            | result when result.Success -> Ok result.Value
            | result -> Error result.Exception.Message // Added as additional information.
        ) JsonValueKind.String // Expected kind.

    // Custom parser example.
    let userId =
        Parse.custom (fun element ->
            match element.TryGetGuid() with
            | true, guid -> Ok <| UserId guid
            | _ -> Error String.Empty // No additional info.
        ) JsonValueKind.String

    // Combined parsers example.

    let profileId =
        Parse.guid
        |> Parser.map ProfileId

    let email =
        Parse.string
        |> Parser.validate Email.fromString

    let age =
        Parse.byte
        |> Parser.validate Age.fromByte

    let plan =
        Parse.string
        |> Parser.validate Plan.fromString

module User =
    open Parse

    let parser =
        parser {
            let! id = "id" &= userId // Custom parser example.
            let! name = "name" &= string
            let! age = "age" ?= age
            let! email = "email" &= email
            let! profiles = "profiles" &= set profileId

            // Inlined parser example.
            let! subscription = "subscription" &= parser {
                let! plan = "plan" &= plan
                let! isCanceled = "isCanceled" &= bool
                let! renewsAt = "renewsAt" ?= instant // Custom parser example.

                return {
                    Plan = plan
                    IsCanceled = isCanceled
                    RenewsAt = renewsAt
                }
            }

            // "Path" example, which can be very useful
            // when we just want to parse a (few) nested value(s).
            let! _isCanceled = "subscription.isCanceled" &= bool

            return {
                Id = id
                Name = name
                Age = age
                Email = email
                Profiles = profiles
                Subscription = subscription
            }
        }

    let asJson user =
        JObj [
            "id", JStr <| UserId.asString user.Id
            "name", JStr user.Name
            "age",
                user.Age
                |> Option.map (Age.asByte >> JNum)
                |> JNil
            "email", JStr <| Email.asString user.Email
            "profiles",
                user.Profiles
                |> Seq.map (ProfileId.asString >> JStr)
                |> JArr
            "subscription",
                JObj [
                    "plan", JStr <| Plan.asString user.Subscription.Plan
                    "isCanceled", JBit user.Subscription.IsCanceled
                    "renewsAt",
                        user.Subscription.RenewsAt
                        |> Option.map (_.ToString() >> JStr)
                        |> JNil
                ]
        ]

module Example =

    [<Fact>]
    let ``Should parse and create example JSON`` () =
        let expected = File.ReadAllText("Example.json")

        let user =
            User.parser
            |> Parser.parse expected
            |> Expect.ok

        let actual =
            user
            |> User.asJson
            |> Json.asString

        Expect.equal actual expected