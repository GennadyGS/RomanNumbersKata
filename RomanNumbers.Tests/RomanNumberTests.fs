namespace RomanNumbers.Tests

open Xunit
open Xunit.Extensions
open FsUnit.Xunit
open RomanNumbers.RomanNumbers

type RomanNumbersTests() =
    
    [<Theory>]
    [<InlineData(-1)>]
    [<InlineData(0)>]
    [<InlineData(1001)>]
    let ``Should raise exception when number not within acceptable range``number =
        number |> toRomanNumber |> should throw typeof<System.ArgumentOutOfRangeException>

    [<Theory>]
    [<InlineData(1, "I")>]
    let ``Should return correct result``(number, romanNumber) =
        number |> toRomanNumber |> should equal romanNumber

