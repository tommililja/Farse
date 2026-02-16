namespace Farse.Tests

open Farse
open Farse.Operators
open NodaTime
open NodaTime.Text
open System
open System.IO
open Xunit

type UserId = UserId of Guid

module UserId =

    let asString (UserId x) =
        string x

type Age = Age of byte

module Age =

    [<Literal>]
    let private MinAge = 12uy

    let fromByte = function
        | age when age >= MinAge -> Ok <| Age age
        | _ -> Error $"The minimum age is '%u{MinAge}'."

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
        | str -> Error $"Plan '%s{str}' not found."

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

    let profileId =
        Parse.custom (fun element ->
            match element.TryGetGuid() with
            | true, guid -> Ok <| ProfileId guid
            | _ -> Error <| Some "Invalid guid." // Added as details.
        ) ExpectedKind.String

    let instant =
        Parse.custom (fun element ->
            let string = element.GetString()
            match InstantPattern.General.Parse(string) with
            | result when result.Success -> Ok result.Value
            | result -> Error <| Some result.Exception.Message // Added as details.
        ) ExpectedKind.String

module User =
    open Parse

    let parser =
        parser {
            let! id = "id" &= guid |> Parser.map UserId
            and! name = "name" &= string
            and! age = "age" ?= valid byte Age.fromByte
            and! email = "email" &= valid string Email.fromString
            and! profiles = "profiles" &= set profileId // Custom parser example.

            // Inlined parser example.
            and! subscription = "subscription" &= parser {
                let! plan = "plan" &= valid string Plan.fromString
                and! isCanceled = "isCanceled" &= bool
                and! renewsAt = "renewsAt" ?= instant // Custom parser example.

                return {
                    Plan = plan
                    IsCanceled = isCanceled
                    RenewsAt = renewsAt
                }
            }

            // "Path" example, which can be very useful
            // when we just want to parse a (few) nested value(s).
            and! _isCanceled = "subscription.isCanceled" &= bool

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
            "age", JNum.nil Age.asByte user.Age
            "email", JStr <| Email.asString user.Email
            "profiles", JStr.arr ProfileId.asString user.Profiles
            "subscription", JObj [
                "plan", JStr <| Plan.asString user.Subscription.Plan
                "isCanceled", JBit user.Subscription.IsCanceled
                "renewsAt", JStr.nil _.ToString() user.Subscription.RenewsAt
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
            |> Json.asString Indented

        Expect.equal actual expected