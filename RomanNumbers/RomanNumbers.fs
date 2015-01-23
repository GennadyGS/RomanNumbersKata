namespace RomanNumbers

module RomanNumbers = 
    let toRomanNumber number = 
        if number <= 0 || number > 3999 then
            raise (System.ArgumentOutOfRangeException(number |> sprintf "Number %i cannot be represented as roman number"))
        if number = 5 then 
            "V"
        else
            "I" |> String.replicate number
