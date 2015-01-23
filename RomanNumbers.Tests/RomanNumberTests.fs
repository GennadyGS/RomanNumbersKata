namespace RomanNumbers.Tests

open Xunit
open Xunit.Extensions
open FsUnit.Xunit
open RomanNumbers.RomanNumbers

type RomanNumbersTests() =
    
    [<Theory>]
    [<InlineData(1, "I")>]
    let ``Should return correct result``(number, romanNumber) =
        number |> toRomanNumber |> should equal romanNumber

