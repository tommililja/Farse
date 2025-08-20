namespace Farse.Tests

open Farse
open Farse.Operators
open NodaTime
open NodaTime.Text
open System
open System.Text.Json

module Parse =

    let instant =
        Parse.custom (fun (element:JsonElement) ->
            let string = element.GetString()
            match InstantPattern.General.Parse(string) with
            | result when result.Success -> true, result.Value
            | _ -> false, Instant.MinValue
        ) JsonValueKind.String

type UserId = UserId of Guid

module UserId =

    let asString (UserId x) =
        string x

    let parser =
        Parse.guid
        |> Parser.map UserId

type Age = Age of byte

module Age =

    let asByte (Age x) = x

    let fromByte age =
        match age with
        | age when age >= 12uy -> Ok <| Age age
        | age -> Error $"Invalid age: %u{age}."

    let parser =
        Parse.byte
        |> Parser.validate fromByte

type Email = Email of string

module Email =

    let asString (Email x) = x

    let fromString str =
        // Some validation.
        Ok <| Email str

    let parser =
        Parse.string
        |> Parser.validate fromString

type ProfileId = ProfileId of Guid

module ProfileId =

    let asString (ProfileId x) =
        string x

    let parser =
        Parse.guid
        |> Parser.map ProfileId

type Plan =
    | Pro
    | Standard
    | Free

module Plan =

    let fromString = function
        | "Pro" -> Ok Pro
        | "Standard" -> Ok Standard
        | "Free" -> Ok Free
        | invalid -> Error $"Invalid plan: %s{invalid}."

    let asString = function
        | Pro -> "Pro"
        | Standard -> "Standard"
        | Free -> "Free"

    let parser =
        Parse.string
        |> Parser.validate fromString

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

module User =
    open Parse

    let parser =
        parser {
            let! id = "id" &= UserId.parser
            let! name = "name" &= string
            let! age = "age" ?= Age.parser
            let! email = "email" &= Email.parser
            let! profiles = "profiles" &= set ProfileId.parser

            // Inlined parser example.
            let! subscription = "subscription" &= parser {
                let! plan = "plan" &= Plan.parser
                let! isCanceled = "isCanceled" &= bool
                let! renewsAt = "renewsAt" ?= instant

                return {
                    Plan = plan
                    IsCanceled = isCanceled
                    RenewsAt = renewsAt
                }
            }

            // Simple "path" example, which can be very useful
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

    let json user =
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
            "subscription", JObj [
                "plan", JStr <| Plan.asString user.Subscription.Plan
                "isCanceled", JBit user.Subscription.IsCanceled
                "renewsAt",
                    user.Subscription.RenewsAt
                    |> Option.map (_.ToString() >> JStr)
                    |> JNil
            ]
        ]

module App =

    let private json = String.Empty

    let user =
        User.parser
        |> Parser.parse json
        |> Result.defaultWith failwith

    printf "%s" user.Name