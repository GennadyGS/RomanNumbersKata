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

        if number <= 0 || number > 3999 then
            raise (System.ArgumentOutOfRangeException(number |> sprintf "Number %i cannot be represented as roman number"))
        
        if number = Ten then
            string TenRomanDigit
        else
            if number = Ten - One then
                string OneRomanDigit + string TenRomanDigit
            else
                if number >= Five then 
                    string FiveRomanDigit + DuplicateOneDigits (number - Five)
                else
                    if number = Five - One then
                        string OneRomanDigit + string FiveRomanDigit
                    else
                        DuplicateOneDigits number    
