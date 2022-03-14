namespace fun_rop
// Railway Oriented Programming

module fun_rop = 
    type Result<'TEntity> =
        | Success of 'TEntity
        | Failure of string
        
    let bind func result =
        match result with
        | Success s -> func s
        | Failure f -> Failure f
     
    let bindv2 switchFunction =
        function
        | Success s -> switchFunction s
        | Failure f -> Failure f

     // infix operator for bind
    let (>>=) result func =
        bind func result
        
    let map func result =
        match result with
        | Success s -> Success (func s)
        | Failure f -> Failure f

//    let exmp a = a + " "
//    let test a =
//        let res = exmp a 
//        Success res
//    let test = exmp >> Success 
    let mapv2 func =
        bind (func >> Success)
//        bind <| func >> Success

        
    // composition without bind
    let (>=>) funResult1 funResult2 x =
        match funResult1 x with
        | Success s -> funResult2 s
        | Failure f -> Failure f
    // with bind
    let (>=>>) funResult1 funResult2 =
        funResult1 >> (bind funResult2)
        
    // fire an forget function
    let tee f x =
        f x |> ignore
        x
    
    let switch f x =
        f x |> Success
        
    let doubleMap successFunc failureFunc twoTrackInput =
        match twoTrackInput with
        | Success s -> Success (successFunc s)
        | Failure f -> Failure (failureFunc f)
      
    let mapV2 successFunc =
        doubleMap successFunc id
       
    let tryCatch f x =
        try
            f x |> Success
        with
        | ex -> Failure ex.Message
        
    let plus addSuccess addFailure switch1 switch2 x =
        match (switch1 x),(switch2 x) with
        | Success s1,Success s2 -> Success (addSuccess s1 s2)
        | Failure f1,Success _  -> Failure f1
        | Success _ ,Failure f2 -> Failure f2
        | Failure f1,Failure f2 -> Failure (addFailure f1 f2)
    
    // create a "plus" function for validation functions
    let (&&&) v1 v2 =
        let addSuccess r1 _ = r1 // return first
        let addFailure s1 s2 = s1 + "; " + s2  // concat
        plus addSuccess addFailure v1 v2
        
    let nameNotEmpty name =
        if System.String.IsNullOrWhiteSpace name then
            Failure "Name is empty"
        else Success name
        
    let nameLimitReached (name:string) =
        if name.Length > 30 then
            Failure "Name is too long"
        else Success name
      
    let toTrimLower (str: string) =
        str.Trim().ToLower()
        
        // a dead-end function
    let updateDatabase input =
       ()   // dummy dead-end function for now
       
    let log twoTrackInput =
        let success x = printfn "DEBUG. Success so far: %A" x; x 
        let failure x = printfn "ERROR. %A" x; x
        doubleMap success failure twoTrackInput

    let validateNameV1 =
        nameNotEmpty
        >> bind nameLimitReached
        >> map toTrimLower
        
    let validateNameV2 name =
        name
        >>= nameNotEmpty
        >>= nameLimitReached
        >>= switch toTrimLower 
        
    let validateNameV3 =
        nameNotEmpty
        >=> nameLimitReached
        >=> switch toTrimLower
//        >> map toTrimLower
    
    let validateNameWithDeadEndFunc = 
        nameNotEmpty
        >=> nameLimitReached
        >=> switch toTrimLower
        >=> tryCatch (tee updateDatabase)
    
    let validateWithLog =
        nameNotEmpty
        >=> nameLimitReached
        >=> switch toTrimLower
        >=> tryCatch (tee updateDatabase)
        >> log

    let combinedValidation =
        nameNotEmpty
        &&& nameLimitReached
        
    let fullCombinedValidation =
        combinedValidation
        >=> switch toTrimLower
        >=> tryCatch (tee updateDatabase)
        >> log
        
        
    // function injecting
    type Config = {debug:bool}

    let debugLogger twoTrackInput =
        let success x = printfn "DEBUG. Success so far: %A" x; x
        let failure = id // don't log here
        doubleMap success failure twoTrackInput

    let injectableLogger config =
        if config.debug then debugLogger else id

    let usecase config =
        combinedValidation
        >> map toTrimLower
        >> injectableLogger config
//    let input = "good"
//    let releaseConfig = {debug=false}
//    input
//    |> usecase releaseConfig
//    |> ignore
//
//    let debugConfig = {debug=true}
//    input
//    |> usecase debugConfig
//    |> ignore