namespace RomanNumbers

module Utils = 
    let BindBinaryOp binaryOp someA someB = 
        match (someA, someB) with
        | (Some(a), Some(b)) -> Some(binaryOp a b)
        | _ -> None

    let AddReversed (a: string) (b: string) = b + a


