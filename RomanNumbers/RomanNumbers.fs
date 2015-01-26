namespace RomanNumbers

module RomanNumbers = 
    [<Literal>]
    let Ten = 10
    
    [<Literal>]
    let Five = 5

    let RomanDigitsDefs = [ ('I', 'V'); ('X', 'L')]

    let ToRomanNumber number = 

        let ToRomanDigit digit (oneRomanDigit, fiveRomanDigit, someTenRomanDidit) = 

            let DuplicateOneDigits number romanDigit = 
                new string(romanDigit, number)

            match digit with
            | 9 -> 
                match someTenRomanDidit with
                | Some(tenRomanDigit) -> string oneRomanDigit + string tenRomanDigit    
                | None -> raise (System.ArgumentOutOfRangeException())
            | 5 | 6 | 7 | 8 -> string fiveRomanDigit + DuplicateOneDigits (digit - Five) oneRomanDigit
            | 4 -> string oneRomanDigit + string fiveRomanDigit
            | 0 | 1 | 2 | 3 -> DuplicateOneDigits digit oneRomanDigit   
            | _ -> raise (System.ArgumentOutOfRangeException())

        let rec ToRomanNumberByDigitDefs number romanDigitsDefs = 
            
            let ToRomanNumberByUnitsAndTensDigitDefs number unitsRomanDigitsDef tensRomanDigitsDef = 
                    let units = number % Ten
                    let tens = (number - units) / Ten
                    let unitsRomanDigits = ToRomanDigit units unitsRomanDigitsDef
                    let tensRomanDigits = ToRomanNumberByDigitDefs tens tensRomanDigitsDef
                    tensRomanDigits + unitsRomanDigits

            if number > 0 then
                match romanDigitsDefs with
                | (oneRomanDigit, fiveRomanDigit) :: (tenRomanDigit, fiftyRomanDigit) :: hundredsRomanDigits -> 
                    ToRomanNumberByUnitsAndTensDigitDefs number (oneRomanDigit, fiveRomanDigit, Some(tenRomanDigit)) ((tenRomanDigit, fiftyRomanDigit) :: hundredsRomanDigits)
                | (oneRomanDigit, fiveRomanDigit) :: tensRomanDigits -> 
                    ToRomanNumberByUnitsAndTensDigitDefs number (oneRomanDigit, fiveRomanDigit, None) tensRomanDigits
                | [] -> raise (System.ArgumentOutOfRangeException())
            else System.String.Empty
        
        if number <= 0 then
            raise (System.ArgumentOutOfRangeException(number |> sprintf "Number %i cannot be represented as roman number"))
        ToRomanNumberByDigitDefs number RomanDigitsDefs
