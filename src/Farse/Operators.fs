namespace Farse.Operators

open Farse

[<AutoOpen>]
module Operators =
    open Parse

    /// <summary>
    /// Parses a required property.
    /// </summary>
    let (&=) = req

    /// <summary>
    /// Parses an optional property.
    /// </summary>
    let (?=) = opt
