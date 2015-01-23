﻿namespace RomanNumbers

module RomanNumbers = 
    [<Literal>]
    let One = 1

    [<Literal>]
    let OneRomanDigit = 'I'

    [<Literal>]
    let Five = 5
    
    [<Literal>]
    let FiveRomanDigit = 'V'

    [<Literal>]
    let Ten = 10
    
    [<Literal>]
    let TenRomanDigit = 'X'

    let ToRomanNumber number = 

        let DuplicateOneDigits number = 
            new string(OneRomanDigit, number)

        if number <= 0 || number > 3999 then
            raise (System.ArgumentOutOfRangeException(number |> sprintf "Number %i cannot be represented as roman number"))
        
        match number with
        | 10 -> string TenRomanDigit
        | 9 -> string OneRomanDigit + string TenRomanDigit
        | 5 | 6 | 7 | 8 -> string FiveRomanDigit + DuplicateOneDigits (number - Five)
        | 4 -> string OneRomanDigit + string FiveRomanDigit
        | 0 | 1 | 2 | 3 -> DuplicateOneDigits number   
        | _ -> raise (System.ArgumentOutOfRangeException())
