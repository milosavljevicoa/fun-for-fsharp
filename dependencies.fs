namespace dependency

open System

module dependency_retention =
  // mixing impure and pure code
  // I/O is impure
  // Decision making (if) is pure
  let compareTwoStrings() =
    printfn "Enter the first value"
    let str1 = Console.ReadLine()
    printfn "Enter the second value"
    let str2 = Console.ReadLine()

    if str1 > str2 then
      printfn "The first value is bigger"
    else if str1 < str2 then
      printfn "The first value is smaller"
    else
      printfn "The values are equal"
// -------------------------------------------------------------- 
type ComparisonResult =
| Bigger
| Smaller
| Equal

module pure_fun =
  let compareTwoStrings str1 str2 =
    if str1 > str2 then
      Bigger
    else if str1 < str2 then
      Smaller
    else
      Equal
      

module dependency_rejection = 
  let program() =
    // ----------- impure section -----------
    printfn "Enter the first value"
    let str1 = Console.ReadLine()
    printfn "Enter the second value"
    let str2 = Console.ReadLine()

    // ----------- pure section -----------
    let result = pure_fun.compareTwoStrings str1 str2

    // ----------- impure section -----------
    match result with
    | Bigger ->
      printfn "The first value is bigger"
    | Smaller ->
      printfn "The first value is smaller"
    | Equal ->
      printfn "The values are equal"

// In general, we want our functional pipeline to look just like this: 
// Some I/O or other non-deterministic code, such as reading from a console/file/database/etc
// The pure business logic which makes decisions
// Some more I/O, such as saving the result to a file/database/etc

// What’s also nice about this approach is that the test boundaries become very clear.
// You unit test the pure code in the center, and you do integration tests across the whole pipeline.


module pure_fun_v2 =
  let compareTwoStrings (comparison:StringComparison) str1 str2 =
    // The StringComparison enum lets you pick culture and case-sensitivity options
    let result = String.Compare(str1,str2,comparison)
    if result > 0 then
      Bigger
    else if result < 0 then
      Smaller
    else
      Equal
      

module dependency_parameterization =
  let compareCaseSensitive = pure_fun_v2.compareTwoStrings StringComparison.CurrentCulture
  let compareCaseInsensitive = pure_fun_v2.compareTwoStrings StringComparison.CurrentCultureIgnoreCase
  
  type IConsole =
    abstract ReadLn : unit -> string
    abstract WriteLn : string -> unit
    
  let compareTwoStrings (console:IConsole)  =
      // ----------- impure section ----------- 
      console.WriteLn "Enter the first value"
      let str1 = console.ReadLn()
      console.WriteLn "Enter the second value"
      let str2 = console.ReadLn()

      // ----------- pure section ----------- 
      let result = compareCaseSensitive str1 str2

      // ----------- impure section ----------- 
      match result with
      | Bigger ->
          console.WriteLn "The first value is bigger"
      | Smaller ->
          console.WriteLn "The first value is smaller"
      | Equal ->
          console.WriteLn "The values are equal"

  // the final code with the "services" passed in
  let program() =
      let console = {
          new IConsole with
              member this.ReadLn() = Console.ReadLine()
              member this.WriteLn str = printfn "%s" str
          }
      // call the parameterized function
      compareTwoStrings console
      
      
type ILogger =
  abstract Debug : string -> unit
  abstract Info : string -> unit
  abstract Error : string -> unit
  
module dependency_injection =
  type Reader<'env,'a> = Reader of action:('env -> 'a)
  let compareTwoStrings str1 str2 :Reader<ILogger,ComparisonResult> =
    fun (logger:ILogger) ->
      logger.Debug "compareTwoStrings: Starting"

      let result = pure_fun.compareTwoStrings str1 str2 

      logger.Info (sprintf "compareTwoStrings: result=%A" result)
      logger.Debug "compareTwoStrings: Finished"
      result
  |> Reader
  
  // Why do this?
  // The reason is that the Reader type can be composed, transformed and chained in just the same way that the Option or Result or List or Async types can be. 