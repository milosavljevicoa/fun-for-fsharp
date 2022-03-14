namespace main_program

open System.Collections.Generic
open System.Runtime.CompilerServices

module identity_function =
    type Example = {
        Children: int list
    }
    let control = [{ Children = [1;2;3] }; { Children = [4;5;6 ] }]
    let allGrandChildren = control |> Seq.collect (fun c -> c.Children)
    let flattened = [ [1;2]; [3;4]; [5;6] ] |> Seq.collect (fun x -> x)
    // fun x -> x --- this function just returns the argument and is called identity function
    let flattened = [ [1;2]; [3;4]; [5;6] ] |> Seq.collect id
    // id --- identity function

module function_keyword =
    let isEven value = match value with
                        | x when (x % 2) = 0 -> true
                        | _ -> false

    // It’s used in pattern matching expressions when we want to match against one of the parameters passed into the function which contains the pattern match.
    let isEven2  = function
               | x when (x % 2) = 0 -> true
               | _ -> false

module partial_application =
    let add x y = x + y
    add (1+2) (3+4)
    (+) 1 2
    1+2 |> add <| 3+4        // pseudo infix
    
//    printf "%i" 1+2          // error
//    printf "%i" (+) 1 2          // error
    printf "%i" (1+2)        // using parens
    printf "%i" <| 1+2       // using reverse pipe


module compositions =
// (|>) // : ('a -> ('a -> 'b) -> 'b)
// (>>) // : (('a -> 'b) -> ('b -> 'c) -> 'a -> 'c)
    let add1 a = a + 1
    let times2 a = a * 2
    let subtract20 a = a - 20
    let a1 = [100;200;300] |> List.map (add1 >> times2 >> subtract20)
    let a2 = [100;200;300] |> List.map (fun x -> x |> add1 |> times2 |> subtract20)


module functions =
    // recursion
    let rec fib n =
        match n with
        | 0 | 1 -> n
        | n -> fib (n-1) + fib (n-2)

    // tail recursion - https://cs.stackexchange.com/questions/6230/what-is-tail-recursion
    let fib_tail_rec n =
        let rec loop acc1 acc2 n =
            match n with
            | 0 -> acc1
            | 1 -> acc2
            | _ ->
                loop acc2 (acc1 + acc2) (n - 1)
        loop 0 1 n
        
     // inline
    let inline printAsFloatingPoint number =
        printfn "%f" (float number)
    let printAsFloatingPoint_noInline number =
        printfn "%f" (float number)
            
    printAsFloatingPoint 3
    printAsFloatingPoint 3.4
    printAsFloatingPoint "3.4"

    printAsFloatingPoint_noInline 3
    printAsFloatingPoint_noInline 3.5
    printAsFloatingPoint_noInline "3.5"

module pattern_matching = 
    type A() = class end
    type B() = inherit A()
    type C() = inherit A()

    let m (a: A) =
        match a with
        | :? B -> printfn "It's a B"
        | :? C -> printfn "It's a C"
        | _ -> ()
        
    
        
    type Slice = Slice of int * int * string
    let GetSubstring1 (Slice(p0, p1, text: string)) =
        printfn "Data begins at %d and ends at %d in string %s" p0 p1 text
        text.[p0..p1]
    let substring = GetSubstring1 (Slice(0, 4, "Et tu, Brute?"))
    printfn "Substring: %s" substring
    
    type Point = { x : float; y : float }

    let (| Polar |) { x = x; y = y} =
        ( sqrt (x*x + y*y), System.Math.Atan (y/ x) ) 

    let radius (Polar(r, _)) = r
    let angle (Polar(_, _) as p) = p.y 
    
module casting_conversion =
    type Base1() =
        abstract member F : unit -> unit
        default u.F() =
         printfn "F Base1"

    type Derived1() =
        inherit Base1()
        override _.F() =
          printfn "F Derived1"

    let d1 : Derived1 = Derived1()

    // Upcast to Base1.
    let base1 = d1 :> Base1

    // This might throw an exception, unless
    // you are sure that base1 is really a Derived1 object, as
    // is the case here.
    let derived1 = base1 :?> Derived1

    // If you cannot be sure that b1 is a Derived1 object,
    // use a type test, as follows:
    let downcastBase1 (b1 : Base1) =
       match b1 with
       | :? Derived1 as derived1 -> derived1.F()
       | _ -> ()

    downcastBase1 base1
    
module generics =
    // implicit generics
    let function1 (x: 'a) (y: 'a) =
//    let function1 x y =
        printfn "%A %A" x y

    // explicit
    let function2<'T> (x: 'T) (y: 'T) =
        printfn "%A, %A" x y
    
    function1 10 20
    function1 10.0 20.0
    // Type arguments can be specified, but should only be specified
    // if the type parameters are declared explicitly. If specified,
    // they have an effect on type inference, so in this example,
    // a and b are inferred to have type int.
    let function3 a b =
        // The compiler reports a warning:
        function1<int> a b
        // No warning.
        function2<int> a b
        
     // To specify that a type argument should be inferred by the compiler,
     // you can use the underscore, or wildcard symbol "_"  
    let printSequence (sequence1: Collections.seq<_>) =
        Seq.iter (fun elem -> printf "%s " (elem.ToString())) sequence1
        
// statically resolved type - example type: ^a -> ^c -> ^d
// usually inline functions
// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/generics/statically-resolved-type-parameters

// flexible types - example type: #type
// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/flexible-types

// byref - low lever programming
// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/byrefs

module sequence =
    let seq = seq {
        for x in 1..10 do
            yield x
            yield! seq { for i in 1..x -> i}
    }
    
module records =
    // Create a Person type and use the Address type that is not defined
    type Person =
      { Name: string
        Age: int
        Address: Address }
    // Define the Address type which is used in the Person record
    and Address =
      { Line1: string
        Line2: string
        PostCode: string
        Occupant: Person }
      // If you were to define the previous example without the and keyword, then it would not compile.
      // The and keyword is required for mutually recursive definitions.
      
    type RecordTest = { X: int; Y: int }
    let record1 = { X = 1; Y = 2 }
    let record2 = { X = 1; Y = 2 }

    if (record1 = record2) then
        printfn "The records are equal."
    else
        printfn "The records are unequal."
        
    // anonymous records
    open System
    let getCircleStats radius =
        let d = radius * 2.0
        let a = Math.PI * (radius ** 2.0)
        let c = 2.0 * Math.PI * radius

        {| Diameter = d; Area = a; Circumference = c |}
        // record struct type
        // struct {| Diameter = d; Area = a; Circumference = c |}

    let r = 2.0
    let stats = getCircleStats r
    printfn "Circle with radius: %f has diameter %f, area %f, and circumference %f"
        r stats.Diameter stats.Area stats.Circumference
        
module discriminatedUnions =
    type Shape =
    | Rectangle of width : float * length : float
    | Circle of radius : float
    | Prism of width : float * float * height : float
    let rect = Rectangle(length = 1.3, width = 10.0)
    let circ = Circle (1.0)
    let prism = Prism(5., 2.0, height = 3.0)
    
module classes =
    // There is always a primary constructor whose arguments
    // are described in the parameter-list that follows the type name,
    // and whose body consists of the let (and let rec) bindings at the start of the class declaration
    // and the do bindings that follow.
   
   // The following example illustrates this concept. In the following code, MyClass has two constructors,
   // a primary constructor that takes two arguments and another constructor that takes no arguments. 
   type MyClass1(x: int, y: int) =
       do printfn "%d %d" x y
       new() = MyClass1(0, 0)
   //  The do expressions that follow are compiled into the primary constructor and execute initialization code
   // for every instance. Because any additional constructors always call the primary constructor,
   // the let bindings and do bindings always execute regardless of which constructor is called.
   
   // The self identifier that is declared with the as keyword is not initialized until after the base constructor.
   type MyClass2(dataIn) as self =
        let data = dataIn
        do
            self.PrintMessage()
        member this.PrintMessage() =
            printf "Creating MyClass2 with Data %d" data
            
   type MyGenericClass<'a> (x: 'a) =
      do printfn "%A" x
      
   // type is inferred
   let g1 = MyGenericClass( seq { for i in 1 .. 10 -> (i, i*i) } )
   
   // Mutually Recursive Types
   open System.IO

    type Folder(pathIn: string) =
      let path = pathIn
      let filenameArray : string array = Directory.GetFiles(path)
      member this.FileArray = Array.map (fun elem -> new File(elem, this)) filenameArray

    and File(filename: string, containingFolder: Folder) =
       member this.Name = filename
       member this.ContainingFolder = containingFolder

module interfaces =
    // Interface declarations resemble class declarations except that no members are implemented.
    // Instead, all the members are abstract
    type ISprintable =
        abstract member Print : format:string -> unit
        
        
     // these two compile as same for .NET world
    type INumericFSharp =
        abstract Add: x: int -> y: int -> int // calling Add 2 3
    type INumericDotNet =
        abstract Add: x: int * y: int -> int // calling Add (2 3) - this is tuple
        
        
    type IPrintable =
       abstract member Print : unit -> unit

    type SomeClass1(x: int, y: float) =
        interface IPrintable with
            member this.Print() = printfn "%d %f" x y   
    let x1 = new SomeClass1(1, 2.0)
//  no can do
//  x1.Print()
    (x1 :> IPrintable).Print()
    
    // this way we can do
    type SomeClass2(x: int, y: float) =
        member this.Print() = (this :> IPrintable).Print()
        interface IPrintable with
            member this.Print() = printfn "%d %f" x y
    let x2 = new SomeClass2(1, 2.0)
    x2.Print()
    
    
    // Implementing Interfaces by Using Object Expressions
    let makePrintable(x: int, y: float) =
        { new IPrintable with
                  member this.Print() = printfn "%d %f" x y }
    let x3 = makePrintable(1, 2.0)
    x3.Print()
    
    
    
    // Interface inheritance
    type Interface1 =
        abstract member Method1 : int -> int

    type Interface2 =
        abstract member Method2 : int -> int

    type Interface3 =
        inherit Interface1
        inherit Interface2
        abstract member Method3 : int -> int

    type MyClass() =
        interface Interface3 with
            member this.Method1(n) = 2 * n
            member this.Method2(n) = n + 100
            member this.Method3(n) = n / 10
            
            
    // Implementing the same interface at different generic instantiations
    type IA<'T> =
        abstract member Get : unit -> 'T

    type MyClass3() =
        interface IA<int> with
            member x.Get() = 1
        interface IA<string> with
            member x.Get() = "hello"

    let mc = MyClass3()
    let iaInt = mc :> IA<int>
    let iaString = mc :> IA<string>

    iaInt.Get() // 1
    iaString.Get() // "hello"
    
    
module constructors =
    // Executing side effects in the primary constructor and
    // additional constructors.
    type Person(nameIn : string, idIn : int) =
        let mutable name = nameIn
        let mutable id = idIn
        do printfn "Created a person object."
        member this.Name
            with get() = name
            and set(v) = name <- v
        member this.ID with get() = id and set(v) = id <- v
        new() =
            Person("Invalid Name", -1)
            then
                printfn "Created an invalid person object."

    let person1 = new Person("Humberto Acevedo", 123458734)
    let person2 = new Person()
    
    
    //A let binding creates a private field or function; to expose data or functions publicly,
    //declare a property or a member method.
    
    //Static let bindings are part of the static initializer for the class,
    //which is guaranteed to execute before the type is first used.
    
    //You can also use the val keyword to create a private field.
    //When using the val keyword, the field is not given a value when the object is created,
    //but instead is initialized with a default value
    type TestClass() =
//        let b: int - ne moze
        let a: int = 3
        [<DefaultValue>] val mutable b: int
        
module properties =
    type MyClass(property1) =
        static let mutable myStaticValue = 0
        let mutable myInternalValue = 0
        // A read-only property.
        member public this.MyReadOnlyProperty = myInternalValue
        // A write-only property.
        member private this.MyWriteOnlyProperty with set (value) = myInternalValue <- value
        // A read-write property.
        member this.MyReadWriteProperty
            with get () = myInternalValue
            and set (value) = myInternalValue <- value

        // Automatically implemented properties
        member val Property1 = property1
        member val Property2 = "" with get, set
        
        static member MyStaticProperty
            with get() = myStaticValue
            and set(value) = myStaticValue <- value
        
    // Note that the expression that initializes
    // an automatically implemented property is only evaluated upon initialization,    
    type MyClass1() =
        let random  = new System.Random()
        member val AutoProperty = random.Next() with get, set
        member this.ExplicitProperty = random.Next()

    let class1 = MyClass1()

    printfn $"class1.AutoProperty = %d{class1.AutoProperty}"
    printfn $"class1.AutoProperty = %d{class1.AutoProperty}"
    printfn $"class1.ExplicitProperty = %d{class1.ExplicitProperty}"
    printfn $"class1.ExplicitProperty = %d{class1.ExplicitProperty}"
    
module methods =
    // Methods usually use the tuple form of passing arguments.
    // This achieves a clearer result from the perspective of other .NET languages
    // because the tuple form matches the way arguments are passed in .NET methods.
    type SomeType(factor0: int) =
        let factor = factor0
        member this.SomeMethod(a, b, c) =
          (a + b + c) * factor

        member this.SomeOtherMethod(a, b, c) =
          this.SomeMethod(a, b, c) * factor
    let someType = SomeType(2)
    someType.SomeMethod(2, 3, 4) |> ignore
    
    type Ellipse(a0 : float, b0 : float, theta0 : float) =
        let mutable axis1 = a0
        let mutable axis2 = b0
        let mutable rotAngle = theta0
        abstract member Rotate: float -> unit
        default this.Rotate(delta : float) = rotAngle <- rotAngle + delta
        
    type Circle(radius : float) =
        inherit Ellipse(radius, radius, 0.0)
         // Circles are invariant to rotation, so do nothing.
        override this.Rotate(_) = ()
        
    open System.Runtime.InteropServices
    // optional arguments
    // A class with a method M, which takes in an optional integer argument.
    type C() =
        member _.M([<Optional; DefaultParameterValue(12)>] i) = i + 1
        member _.M(?i: int) =
            match i with
            | Some value -> value
            | None -> 0
            
            
module indexed_propertios =
    type NumberStrings() =
       let mutable ordinals = [| "one"; "two"; "three"; "four"; "five";
                                 "six"; "seven"; "eight"; "nine"; "ten" |]
       let mutable cardinals = [| "first"; "second"; "third"; "fourth";
                                  "fifth"; "sixth"; "seventh"; "eighth";
                                  "ninth"; "tenth" |]
       member this.Item
          with get(index) = ordinals.[index]
          and set index value = ordinals.[index] <- value
       member this.Ordinal
          with get(index) = ordinals.[index]
          and set index value = ordinals.[index] <- value
       member this.Cardinal
          with get(index) = cardinals.[index]
          and set index value = cardinals.[index] <- value

    let nstrs = NumberStrings()
    // calls member this.Item
    nstrs.[0] <- "ONE"
    for i in 0 .. 9 do
      printf "%s " nstrs.[i]
    printfn ""

    nstrs.Cardinal(5) <- "6th"

    for i in 0 .. 9 do
      printf "%s " (nstrs.Ordinal(i))
      printf "%s " (nstrs.Cardinal(i))
    printfn ""
    
    
    open System.Collections.Generic

    /// Basic implementation of a sparse matrix based on a dictionary
    type SparseMatrix() =
        let table = Dictionary<(int * int), float>()
        member _.Item
            // Because the key is comprised of two values, 'get' has two index values
            with get(key1, key2) = table.[(key1, key2)]

            // 'set' has two index values and a new value to place in the key's position
            and set (key1, key2) value = table.[(key1, key2)] <- value

    let sm = SparseMatrix()
    for i in 1..1000 do
        sm.[i, i] <- float i * float i
        
        
module operator_overloading =
// Operator overloads for unary operators, such as + and -, must use a tilde (~)
// in the operator-symbol to indicate that the operator is a unary operator and not a binary operator
    type Vector(x: float, y : float) =
       member this.x = x
       member this.y = y
       static member (~-) (v : Vector) =
         Vector(-1.0 * v.x, -1.0 * v.y)
       static member (*) (v : Vector, a) =
         Vector(a * v.x, a * v.y)
       static member (*) (a, v: Vector) =
         Vector(a * v.x, a * v.y)
       override this.ToString() =
         this.x.ToString() + " " + this.y.ToString()

    let v1 = Vector(1.0, 2.0) 
    let v2 = v1 * 2.0
    let v3 = 2.0 * v1 
    let v4 = - v2 // unary operator ~

    printfn "%s" (v1.ToString())
    printfn "%s" (v2.ToString())
    printfn "%s" (v3.ToString())
    printfn "%s" (v4.ToString())
    
// You can overload all the standard operators, but you can also create new operators out of sequences
// of certain characters. Allowed operator characters are !, $, %, &, *, +, -, ., /, <, =, >, ?, @, ^, |, and ~.

module object_expression =
    // This object expression specifies a System.Object but overrides the ToString method.
    let obj1 = { new System.Object() with member x.ToString() = "F#" }
    printfn $"{obj1}"

    // This object expression implements the IFormattable interface.
    let delimiter(delim1: string, delim2: string, value: string) =
        { new System.IFormattable with
            member x.ToString(format: string, provider: System.IFormatProvider) =
                if format = "D" then
                    delim1 + value + delim2
                else
                    value }

    let obj2 = delimiter("{","}", "Bananas!");

    printfn "%A" (System.String.Format("{0:D}", obj2))

    // Define two interfaces
    type IFirst =
      abstract F : unit -> unit
      abstract G : unit -> unit

    type ISecond =
      inherit IFirst
      abstract H : unit -> unit
      abstract J : unit -> unit

    // This object expression implements both interfaces.
    let implementer() =
        { new ISecond with
            member this.H() = ()
            member this.J() = ()
          interface IFirst with
            member this.F() = ()
            member this.G() = () }
        
    // You use object expressions when you want to avoid the extra code and overhead
    // that is required to create a new, named type.
    // If you use object expressions to minimize the number of types created in a program,
    // you can reduce the number of lines of code and prevent the unnecessary proliferation of types.
    
module type_extensions =
    type Variant =
    | Num of int
    | Str of string
  
    module Variant =
        let print v =
            match v with
            | Num n -> printf "Num %d" n
            | Str s -> printf "Str %s" s

    // Add a member to Variant as an extension
    type Variant with
        member x.Print() = Variant.print x
        
        

    type IEnumerable<'T> with
    /// Repeat each element of the sequence n times
    member xs.RepeatElements(n: int) =
        seq {
            for x in xs do
                for _ in 1 .. n -> x
        }
        
[<Extension>]
type IEnumerableExtensions =
    [<Extension>]
    static member inline Sum(xs: IEnumerable<'T>) = Seq.sum xs
    
    
module inheritance =
    type MyClassBase1() =
       let mutable z = 0
       abstract member function1 : int -> int
       default u.function1(a : int) = z <- z + a; z

    type MyClassDerived1() =
       inherit MyClassBase1()
       override u.function1(a: int) = a + 1
       
       
module structs =
    // In Point3D, three immutable values are defined.
    // x, y, and z will be initialized to 0.0.
    type Point3D =
        struct
            val x: float
            val y: float
            val z: float
        end
        
    [<Struct>]
    type S(count1: int, count2: int) =
        member x.Count1 = count1
        member x.Count2 = count2

        
    [<IsReadOnly; Struct>]
    type S_ReadOnly(count1: int, count2: int) =
        member x.Count1 = count1
        member x.Count2 = count2

module computation_expressions =
    // All computation expressions have the following form:
    // syntax: builder-expr { cexper }
    // In this form, builder-expr is the name of a builder type that defines the computation expression,
    // and cexper is the expression body of the computation expression.
    // For example, async computation expression code can look like this:
    let downloadData url = url
    let processData data = data
    let fetchAndDownload url =
        async {
            let! data = downloadData url

            let processedData = processData data

            return processedData
        }
        
    // There is a special, additional syntax available within a computation expression,
    // as shown in the previous example. The following expression forms are possible with computation expressions: 
    //    expr { let! ... }
    //    expr { do! ... }
    //    expr { yield ... }
    //    expr { yield! ... }
    //    expr { return ... }
    //    expr { return! ... }
    //    expr { match! ... }
    // match is syntactic sugar for the use of let! followed by a pattern match on the result.
    
    // let!
    // The let! keyword binds the result of a call to another computation expression to a name
    // If you bind the call to a computation expression with let,
    // you will not get the result of the computation expression.
    // Instead, you will have bound the value of the unrealized call to that computation expression.
    // Use let! to bind to the result.
    
    // do!
    // The do! keyword is for calling a computation expression that returns a unit-like type
    
    // yield
    // The yield keyword is for returning a value from the computation expression so that it can be consumed as an IEnumerable<T>
    let squares = seq { for i in 1..10 do yield i * i } 
    for sq in squares do printfn $"%d{sq}"
        
    // In most cases, it can be omitted by callers. The most common way to omit yield is with the -> operator: 
    let squares = seq { for i in 1..10 -> i * i }
    
    // The yield! keyword is for flattening a collection of values from a computation expression:
    let squares = seq { for i in 1..3 -> i * i } 
    let cubes = seq { for i in 1..3 -> i * i * i }

    let squaresAndCubes = seq {
            yield! squares
            yield! cubes
        } 
    printfn $"{squaresAndCubes}"  // Prints - 1; 4; 9; 1; 8; 27
    // When evaluated, the computation expression called by yield! will have its items yielded back one-by-one, flattening the result
    
    // return
    let fetch url = async { return url }
    let url = "test"
    let req =
        async {
            let! data = fetch url
            return data
        }
    let reqv2 = async { return! fetch url } 
    let result = Async.RunSynchronously req
    let result = Async.RunSynchronously reqv2
    
    // match! 
    let doThingsAsync url =
        async {
            match! fetch url with
            | Some data -> printf "data"
            | None -> printf "None"
        }
        
    // create new computation expression
    // https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions#creating-a-new-type-of-computation-expression
   
   
module async_and_task_expression =
    // sync expressions provide one way of performing computations asynchronously, that is, without blocking execution of other work.
    // Using task expressions is preferred when interoperating extensively with .NET libraries that create or consume .NET tasks. 
    // In general, you should use async { } programming in F# unless you frequently need to create or consume .NET tasks.
    // When you define a task {} expression, the code inside is executed immediately - the task object itself holds the mutable state of the computation.
    // You can think of this as a "hot" computation.
    
    // Conversely, the code contained within an async {} block represents a future computation that is generated on demand.
    // In other words, on creating the value no computation is immediately started
    // - you can pass the generator around as a simple value and execute it at some later point in time.
    
    
    // Because async {} is analogous to a unit -> Async<'a> function, each time you evaluate it you will generate and execute a new operation. 
    // Conversely, a task {} holds the result of the computation it represents.
    // This means that once the operation has returned a value, re-evaluating the task will just return the same result. It will not be executed again.
    
    
    // Because async {} is just a computation generator, its behaviour depends on how you start it. 
    // Async.StartImmediate will execute a child computation that begins on the current thread. 
    // Async.StartChild will start a child computation that shares a cancellation token with the parent. It is not tied to the current thread. 
    // Async.Start will start a child computation which returns unit. It does not share a cancellation token, and is not tied to the current thread.
    printf "hello"
    
    
module lazy_expression =
    // Lazy expressions are expressions that are not evaluated immediately, but are instead evaluated when the result is needed. This can help to improve the performance of your code.
    let x = 10
    let result = lazy (x + 10)
    printfn "%d" (result.Force())
    
    
module open_type =
    module M =
        type DU = A | B | C
        type Test = TEST | TEST_TWO

        let someOtherFunction x = x + 1

    // Open only the type inside the module
    open type M.DU
    open type M.Test

    printfn "%A" A
    printfn "%A" TEST_TWO
    
    // You can apply the AutoOpen attribute to an assembly if you want to automatically open a namespace or module when the assembly is referenced. 
    
    
module style_guidelines =
    // https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/
    let a = true