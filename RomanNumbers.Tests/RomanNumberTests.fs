﻿namespace RomanNumbers.Tests

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
    [<InlineData(11, "XI")>]
    [<InlineData(14, "XIV")>]
    [<InlineData(18, "XVIII")>]
    [<InlineData(19, "XIX")>]
    [<InlineData(20, "XX")>]
    [<InlineData(29, "XXIX")>]
    [<InlineData(39, "XXXIX")>]
    [<InlineData(40, "XL")>]
    [<InlineData(59, "LIX")>]
    [<InlineData(99, "XCIX")>]
    [<InlineData(147, "CXLVII")>]
    [<InlineData(481, "CDLXXXI")>]
    [<InlineData(999, "CMXCIX")>]
    [<InlineData(3999, "MMMCMXCIX")>]
    let ``Should return correct result``(number, romanNumber) =
        (number |> ToRomanNumber) |> should equal romanNumber

