namespace Farse.Operators

open Farse

[<AutoOpen>]
module Operators =
    open Parse

    /// <summary>
    /// Parses a required field.
    /// </summary>
    let (&=) = req

    /// <summary>
    /// Parses an optional field.
    /// </summary>
    let (?=) = opt
