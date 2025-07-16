namespace Farse.Operators

open Farse

[<AutoOpen>]
module Operators =
    open Parse

    /// Parses a required property.
    let (&=) path parser =
        req path parser

    /// Parses an optional property.
    let (?=) path parser =
        opt path parser