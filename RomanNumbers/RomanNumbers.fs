namespace RomanNumbers

module RomanNumbers = 
    [<Literal>]
    let Ten = 10
    
    [<Literal>]
    let Five = 5

    let RomanDigitsDefs = [ ('I', Some('V')); ('X', Some('L')); ('C', Some('D')); ('M', None)]

    let ToRomanNumber number = 

        let ToRomanDigit digit (oneRomanDigit, someFiveRomanDigit, someTenRomanDidit) = 

            let DuplicateDigit number digit = 
                new string(digit, number)

            match digit with
            | 9 -> 
                match someTenRomanDidit with
                | Some(tenRomanDigit) -> string oneRomanDigit + string tenRomanDigit    
                | None -> raise (System.ArgumentOutOfRangeException())
            | 5 | 6 | 7 | 8 -> 
                match someFiveRomanDigit with
                | Some(fiveRomanDigit) -> string fiveRomanDigit + DuplicateDigit (digit - Five) oneRomanDigit
                | None -> raise (System.ArgumentOutOfRangeException())
            | 4 -> 
                match someFiveRomanDigit with
                | Some(fiveRomanDigit) -> string oneRomanDigit + string fiveRomanDigit
                | None -> raise (System.ArgumentOutOfRangeException())
            | 0 | 1 | 2 | 3 -> DuplicateDigit digit oneRomanDigit   
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
                | (oneRomanDigit, someFiveRomanDigit) :: (tenRomanDigit, fiftyRomanDigit) :: hundredsRomanDigits -> 
                    ToRomanNumberByUnitsAndTensDigitDefs number (oneRomanDigit, someFiveRomanDigit, Some(tenRomanDigit)) ((tenRomanDigit, fiftyRomanDigit) :: hundredsRomanDigits)
                | (oneRomanDigit, someFiveRomanDigit) :: tensRomanDigits ->
                    ToRomanNumberByUnitsAndTensDigitDefs number (oneRomanDigit, someFiveRomanDigit, None) tensRomanDigits
                | [] -> raise (System.ArgumentOutOfRangeException())
            else System.String.Empty
        
        if number <= 0 then
            raise (System.ArgumentOutOfRangeException(number |> sprintf "Number %i cannot be represented as roman number"))
        ToRomanNumberByDigitDefs number RomanDigitsDefs
