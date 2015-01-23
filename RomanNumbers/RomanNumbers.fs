namespace RomanNumbers

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

        let ToRomanDigit digit = 
            match digit with
            | 9 -> string OneRomanDigit + string TenRomanDigit
            | 5 | 6 | 7 | 8 -> string FiveRomanDigit + DuplicateOneDigits (digit - Five)
            | 4 -> string OneRomanDigit + string FiveRomanDigit
            | 0 | 1 | 2 | 3 -> DuplicateOneDigits digit   
            | _ -> raise (System.ArgumentOutOfRangeException())

        if number <= 0 || number > 3999 then
            raise (System.ArgumentOutOfRangeException(number |> sprintf "Number %i cannot be represented as roman number"))
        
        if number >= 10 then
            string TenRomanDigit + ToRomanDigit (number % Ten)
        else
            ToRomanDigit number
