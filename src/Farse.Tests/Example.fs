namespace Farse.Tests

open Farse
open NodaTime
open NodaTime.Text
open System
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
        | "pro" -> Ok Pro
        | "standard" -> Ok Standard
        | "free" -> Ok Free
        | str -> Error $"Plan '%s{str}' not found."

    let asString = function
        | Pro -> "pro"
        | Standard -> "standard"
        | Free -> "free"

type Subscription = {
    Plan: Plan
    IsCanceled: bool
    RenewsAt: Instant option
}

type Tag =
    | Beta
    | Verified

module Tag =

    let fromString = function
        | "beta" -> Ok Beta
        | "verified" -> Ok Verified
        | str -> Error $"Tag '%s{str}' not found."

    let asString = function
        | Beta -> "beta"
        | Verified -> "verified"

type User = {
    Id: UserId
    Name: string
    Age: Age option
    Email: Email
    Profiles: ProfileId Set
    Subscription: Subscription
    Tags: Tag list
}

module Parse =

    let profileId =
        Parse.custom (fun element ->
            match element.TryGetGuid() with
            | true, guid -> Ok <| ProfileId guid
            | _ -> Error "Invalid guid." // Added as details.
        ) ExpectedKind.String

    let instant =
        Parse.custom (fun element ->
            let string = element.GetString()
            match InstantPattern.General.Parse(string) with
            | result when result.Success -> Ok result.Value
            | result -> Error result.Exception.Message // Added as details.
        ) ExpectedKind.String

module User =
    open Operators
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

            and! tags = "tags" &= list (valid string Tag.fromString)

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
                Tags = tags
            }
        }

    let asJson user =
        JObj [
            "id", JStr (UserId.asString user.Id)
            "name", JStr user.Name
            "age", JNum.nil Age.asByte user.Age
            "email", JStr (Email.asString user.Email)
            "profiles", JStr.arr ProfileId.asString user.Profiles
            "subscription",
                JObj [
                    "plan", JStr (Plan.asString user.Subscription.Plan)
                    "isCanceled", JBit user.Subscription.IsCanceled
                    "renewsAt", JStr.nil _.ToString() user.Subscription.RenewsAt
                ]
            "tags", JStr.arr Tag.asString user.Tags
        ]

    let asJson2 user =
        JObj [
            "id", JStr (UserId.asString user.Id)
            "name", JStr user.Name
            "age",
                user.Age
                |> Option.map (Age.asByte >> JNum)
                |> Option.defaultValue JNil
            "email", JStr (Email.asString user.Email)
            "profiles",
                user.Profiles
                |> Set.toList
                |> List.map (ProfileId.asString >> JStr)
                |> JArr
            "subscription",
                JObj [
                    "plan", JStr (Plan.asString user.Subscription.Plan)
                    "isCanceled", JBit user.Subscription.IsCanceled
                    "renewsAt",
                        user.Subscription.RenewsAt
                        |> Option.map (_.ToString() >> JStr)
                        |> Option.defaultValue JNil
                ]
            "tags",
                user.Tags
                |> List.map (Tag.asString >> JStr)
                |> JArr
        ]

    let asJsonString =
        asJson >> Json.asString Indented

module Example =

    [<Fact>]
    let ``Should parse and create example JSON string`` () =
        let expected =
            JsonTests.example
            |> Json.asString Indented

        let user =
            User.parser
            |> Parser.parse expected
            |> Result.mapError ParserError.asString
            |> Expect.ok

        let actual = User.asJsonString user

        Expect.equal actual expected