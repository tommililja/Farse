namespace Farse.Operators

open Farse

[<AutoOpen>]
module Operators =
    open Parse

    /// Parses a required property.
    let (&=) name parser =
        req name parser

    /// Parses an optional property.
    let (?=) name parser =
        opt name parser