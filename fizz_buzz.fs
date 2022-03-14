module FizzBuzz_Match = 
    let fizzBuzz_101 i =
        match i with
        | _ when i % 15 = 0 ->
            printf "FizzBuzz"
        | _ when i % 3 = 0 ->
            printf "Fizz"
        | _ when i % 5 = 0 ->
            printf "Buzz"
        | _ ->
            printf "%i" i

        printf "; "

    // do the fizzbuzz
    [1..100] |> List.iter fizzBuzz_101
    
    let fizzBuzz rules i  =
        let mutable printed = false

        for factor,label in rules do
            if i % factor = 0 then
                printed <- true
                printf "%s" label

        if not printed then
            printf "%i" i

        printf "; "

    // do the fizzbuzz
    let rules = [ (3,"Fizz"); (5,"Buzz") ]
    [1..100] |> List.iter (fizzBuzz rules)
    
module FizzBuzz_pipes =
    type Data = {i:int; label:string option}

    let carbonate factor label data =
        let {i=i; label=labelSoFar} = data
        if i % factor = 0 then
            // pass on a new data record
            let newLabel =
                match labelSoFar with
                | Some s -> s + label
                | None -> label
            {data with label=Some newLabel}
        else
            // pass on the unchanged data
            data
    let labelOrDefault data =
        let {i=i; label=labelSoFar} = data
        match labelSoFar with
        | Some s -> s
        | None -> sprintf "%i" i
        
    let fizzBuzz i =
        {i=i; label=None}
        |> carbonate 3 "Fizz"
        |> carbonate 5 "Buzz"
        |> labelOrDefault     // convert to string
        |> printf "%s; "      // print
        
    [1..100] |> List.iter fizzBuzz


module fizzBuzz_railway_oriented =
//    let (|Uncarbonated|Carbonated|) =
//        function
//        | Choice1Of2 u -> Uncarbonated u
//        | Choice2Of2 c -> Carbonated c
    
    type Carbonation<'a, 'b> =
        | Uncarbonated of 'a
        | Carbonated of 'b

    /// convert a single value into a two-track result
    let uncarbonated x = Uncarbonated x
    let carbonated x = Carbonated x

    // carbonate a value
    let carbonate factor label i =
        if i % factor = 0 then
            carbonated label
        else
            uncarbonated i

    let connect f =
        function
        | Uncarbonated i -> f i
        | Carbonated x -> carbonated x

    let either uncarbonatedFunc carbonatedFunc =
        function
        | Uncarbonated i -> uncarbonatedFunc i
        | Carbonated x -> carbonatedFunc x
        
    let fizzBuzz =
        carbonate 15 "FizzBuzz"
        >> connect (carbonate 3 "Fizz")
        >> connect (carbonate 5 "Buzz")
        >> either (printf "%i; ") (printf "%s; ")

    // test
    [1..100] |> List.iter fizzBuzz
    
    // concat two carbonation functions
    let (<+>) switch1 switch2 x =
        match (switch1 x),(switch2 x) with
        | Carbonated s1, Carbonated s2 -> carbonated (s1 + s2)
        | Uncarbonated _ ,Carbonated s2  -> carbonated s2
        | Carbonated s1, Uncarbonated _ -> carbonated s1
        | Uncarbonated f1, Uncarbonated _ -> uncarbonated f1
        
    let fizzBuzzV2 =
        let carbonateAll =
            carbonate 3 "Fizz" <+> carbonate 5 "Buzz"

        carbonateAll
        >> either (printf "%i; ") (printf "%s; ")
        
    [1..100] |> List.iter fizzBuzzV2

    
    let fizzBuzzPrimes rules =
        let carbonateAll  =
            rules
            |> List.map (fun (factor,label) -> carbonate factor label)
            |> List.reduce (<+>)

        carbonateAll
        >> either (printf "%i; ") (printf "%s; ")

    // test
    let rules = [ (3,"Fizz"); (5,"Buzz"); (7,"Baz") ]
    [1..105] |> List.iter (fizzBuzzPrimes rules)
    
    
    
module fizzbuzz_active_pattern_matching =
    // setup the active patterns
    let (|MultOf3|_|) i = if i % 3 = 0 then Some MultOf3 else None
    let (|MultOf5|_|) i = if i % 5 = 0 then Some MultOf5 else None

    // the main function
    let fizzBuzz i =
      match i with
      | MultOf3 & MultOf5 -> printf "FizzBuzz, "
      | MultOf3 -> printf "Fizz, "
      | MultOf5 -> printf "Buzz, "
      | _ -> printf "%i, " i

    // test
    [1..20] |> List.iter fizzBuzz