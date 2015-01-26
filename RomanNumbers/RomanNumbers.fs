namespace RomanNumbers

module RomanNumbers = 
    [<Literal>]
    let Ten = 10
    
    [<Literal>]
    let Five = 5

    let RomanDigitsDefs = [ ('I', Some('V')); ('X', Some('L')); ('C', Some('D')); ('M', None)]

    let ToRomanNumber number = 

        let BindBinaryOp binaryOp someA someB = 
            match (someA, someB) with
            | (Some(a), Some(b)) -> Some(binaryOp a b)
            | _ -> None

        let AddReversed a b = b + a

        let ToRomanDigit digit (oneRomanDigit, someFiveRomanDigit, someTenRomanDigit) = 

            let DuplicateDigit number digit = 
                new string(digit, number)

            match digit with
            | 9 -> 
                someTenRomanDigit |> (Option.map string) |> Option.map ((+) (string oneRomanDigit))
            | 5 | 6 | 7 | 8 -> 
                someFiveRomanDigit |> (Option.map string) |> Option.map (AddReversed (DuplicateDigit (digit - Five) oneRomanDigit))
            | 4 -> 
                someFiveRomanDigit |> (Option.map string) |> Option.map ((+) (string oneRomanDigit))
            | 0 | 1 | 2 | 3 -> 
                Some(DuplicateDigit digit oneRomanDigit)
            | _ -> raise (System.ArgumentException())

        let rec ToRomanNumberByDigitDefs number romanDigitsDefs = 
            
            let ToRomanNumberByUnitsAndTensDigitsDefs number unitsRomanDigitsDef tensRomanDigitsDef = 
                let units = number % Ten
                let tens = (number - units) / Ten
                let someUnitsRomanDigits = ToRomanDigit units unitsRomanDigitsDef
                let someTensRomanDigits = ToRomanNumberByDigitDefs tens tensRomanDigitsDef
                BindBinaryOp (+) someTensRomanDigits someUnitsRomanDigits 

            if number > 0 then
                match romanDigitsDefs with
                | (oneRomanDigit, someFiveRomanDigit) :: (tenRomanDigit, fiftyRomanDigit) :: hundredsRomanDigits -> 
                    ToRomanNumberByUnitsAndTensDigitsDefs number (oneRomanDigit, someFiveRomanDigit, Some(tenRomanDigit)) ((tenRomanDigit, fiftyRomanDigit) :: hundredsRomanDigits)
                | (oneRomanDigit, someFiveRomanDigit) :: tensRomanDigits ->
                    ToRomanNumberByUnitsAndTensDigitsDefs number (oneRomanDigit, someFiveRomanDigit, None) tensRomanDigits
                | [] -> None
            else Some(System.String.Empty)
        
        if number <= 0 then
            raise (System.ArgumentOutOfRangeException("Nonpositive number cannot be represented as roman number"))

        let someRomanDigits = ToRomanNumberByDigitDefs number RomanDigitsDefs

        match someRomanDigits with 
        | Some(romanDigits) -> romanDigits
        | None -> raise (System.ArgumentOutOfRangeException(number |> sprintf "Number %i cannot be represented as roman number"))
