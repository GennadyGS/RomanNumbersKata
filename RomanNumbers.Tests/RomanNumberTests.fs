namespace RomanNumbers.Tests

open Xunit
open Xunit.Extensions
open FsUnit.Xunit
open RomanNumbers.RomanNumbers

type RomanNumbersTests() =
    
    [<Theory>]
    [<InlineData(-1)>]
    [<InlineData(0)>]
    [<InlineData(4000)>]
    let ``Should raise exception when number not within acceptable range``number =
        (fun () -> number |> ToRomanNumber |> ignore) |> should throw typeof<System.ArgumentOutOfRangeException>

    [<Theory>]
    [<InlineData(1, "I")>]
    [<InlineData(2, "II")>]
    [<InlineData(5, "V")>]
    [<InlineData(7, "VII")>]
    [<InlineData(4, "IV")>]
    [<InlineData(10, "X")>]
    [<InlineData(9, "IX")>]
    let ``Should return correct result``(number, romanNumber) =
        number |> ToRomanNumber |> should equal romanNumber

