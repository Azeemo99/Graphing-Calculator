//*************
// Example 01
//*************
// tupled parameter list

let add(first,second) = first + second
add(12,-56)
(*
val add : first:int * second:int -> int
val it : int = -44
*)

// curried parameter list

let addc first second =
    first + second  // NOTE: indentation when continuing on next line
addc 23 45

(*
val addc : first:int -> second:int -> int
val it : int = 68
*)

// tuples are often demo-ed in pairs but can be any length

let addn(one, two, three, four) =
    one + two + three + four
addn(5,6,7,8)

(*
val addn : one:int * two:int * three:int * four:int -> int
val it : int = 26
*)

// and curried version

let addnn one two three four =
    one + two + three + four
addnn 3 5 7 9

(*
val addnn : one:int -> two:int -> three:int -> four:int -> int
val it : int = 24
*)

// Currying - further explained
let addfive = addc 5
let fifteen = addfive 10

//*************
// Example 02
//*************
let greetPerson name age =
    sprintf "Hello %s. You are %d years old" name age
greetPerson "John" 18

(*
val greetPerson : name:string -> age:int -> string
val it : string = "Hello John. You are 18 years old"
*)
let Joan = greetPerson "Joan" 43
(* val Joan : string = "Hello Joan. You are 43 years old" *)


let countWords  (text:string) = 
    text.Split([|' ';',';';'|])
countWords "This is a text,to be;split"

//*************
// Example 03
//*************
open System
let year = DateTime.Now.Year
let age = year - 1980
Console.WriteLine("Your are {0} years old", age)

let estimatedAge = 
    let age = 
        let year = DateTime.Now.Year
        let BYear = 1993
        year - BYear
    sprintf "You are about %d years old!" age
    //sprintf "Can I access %d and %d?" year BYear
estimatedAge

let generateRandomNumber max = 
    let randy = System.Random()
    let nextValue = randy.Next(1, max)
    nextValue
generateRandomNumber 100

//*************
// Example 04
//*************
let add3(a:int, b:int) : int =
    let answer:int = a + b
    answer

let add3b(a, b) =
    let answer = a + b
    answer

let add3c(a:int, b:float) : int =
    let answer:int = a + (int)b
    answer

let add3d a b =
    let answer = a + b
    answer

let add3e (a:int) (b:int) =
    let answer = a + b
    answer

//let getlength name = sprintf "Name has %d letters" name.Length 
let getlength (name:string) = sprintf "Name has %d letters" name.Length 
getlength("Rudy Lapeer")
//getlength "Rudy Lapeer" // Would work as well as there is only one parameter so here tupled and curried versions are equivalent

let addnumbers2 arguments = 
    let a, b, c, _ = arguments
    a + b
    // val addnumbers : int * int * 'a * 'b -> int   uses generics for "unknown types"
addnumbers2(3, 4, 5, 6)  // val it : int = 7

//*************
// Example 05
//*************
open System
open System.Collections.Generic

let numbers = List() //List<'a>() //List<_>()
numbers.Add(12)
//numbers.Add(3.14)
//numbers.Add("Rudy")
numbers.Add(23)
numbers.Add(15)
numbers

let numbers2 = List<Object>()
numbers2.Add("Rudy")
numbers2.Add(12)
numbers2.Add(3,1415)
numbers2.Add(3.1415)
numbers2

let CreateList(first, second) =
    let output = List()
    output.Add(first)
    output.Add(second)
    output
CreateList(12,23)           //val it : List<int> = seq [12; 23]
CreateList("Rudy","Lapeer") //val it : List<string> = seq ["Rudy"; "Lapeer"]
//CreateList(45,"Rudy")

// generic placeholders have not the same effects as in C++:
open System
open System.Collections.Generic
//let CreateList2<'a,'b>(first:'a, second:'b) =  // does not work (1)
let CreateList2(first,second) =  
    let output = List()  // Part of (1)
    let output = List<Object>() // List()
    output.Add(first)
    output.Add(second)
    output
CreateList2(45,18)
CreateList2("Rudy",45)
CreateList2(45,"Rudy")

//*************
// Example 06
//*************
let name = "Rudy"
name = "Erna"  // will give false

let name2 = "Rudy"
//name2 <- "Erna" // will tell you name2 is immutable

let mutable name3 = "Rudy"
name3 <- "Erna" // works!

let name4 = "Rudy"
//let name4 = "Erna" // duplicate definition though this will work in F# Interactive and in general - see later

//*************
// Example 07
//*************

let drive(petrol, distance) = 
    if distance = "far" then petrol*0.5 // petrol<-petrol*0.5
    elif distance = "medium" then petrol*0.7
    else petrol*0.9
let petrol = 100.0
let petrolF = drive(petrol, "far")
let petrolM = drive(petrol, "medium")
petrol, petrolF, petrolM 

open System
let describeAge age =
    let ageDescription =
        if age < 18 then "a child."
        elif age < 65 then "an adult."
        else "an OAP!"
    let greeting = "Hello"
    Console.WriteLine("{0}! You are {1}", greeting, ageDescription) // Let binding returns 'unit'
    //sprintf "%s! You are ..." greeting  // Let binding returns 'string'
describeAge 54

open System
let now = System.DateTime.UtcNow.TimeOfDay.TotalHours
if now < 12.0 then Console.WriteLine "It's morning"
elif now < 18.0 then Console.WriteLine "It's afternoon"
elif now < 24.0 then ignore("It's evening")
else ()

let drive2(petrol, distance) = 
    match distance with
    | "far" -> petrol*0.5
    | "medium" -> petrol*0.7
    | _ -> petrol*0.9
let petrol2 = 100.0
let petrol2F = drive2(petrol2, "far")
let petrol2M = drive2(petrol2, "medium")
petrol2F, petrol2M, petrol2

//*************
// Example 08
//*************
// Strings, Tuples, Records

let parseName(name:string) =
    let parts = name.Split(' ')
    let forename = parts.[0]
    let surname = parts.[1]
    forename, surname
    //val parseName : name:string -> string * string
let name5 = parseName("John Doe")
let forename, surname = name5
let fname, sname = parseName("Jane Doe")

// Nested tuples

open System
let nameAndAge = ("John", "Doe"), 27
let name6, age2 = nameAndAge
let (forename1, surname1), myAge = nameAndAge
let fullname = String.Concat(forename1, surname1)
let fllname = sprintf "%s %s" forename1 surname1

// Records

type Car = { make:string ; model:string; enginesize:int; colour:string }
let myCar:Car = 
    { make = "Audi"
      model = "A6"
      enginesize = 1800
      colour = "silver" }
let my2ndCar:Car = { make = "VW"; model = "Passat"; enginesize = 1600; colour = "blue" }
let updatedCar = { myCar with colour = "black" } // updated myCar with copy updatedCar => immutable record

type Car2 = { make:string ; model:string; enginesize:int; mutable colour:string}
let myCar2:Car2 = 
    { make = "Audi"
      model = "A6"
      enginesize = 1800
      colour = "silver" }
myCar2.colour <- "black"
//myCar2.make <- "Mercedes"  // Field is not mutable

let isSameSize = ( myCar.enginesize = my2ndCar.enginesize) // val isSameSize : bool = false

//*************
// Example 09
//*************
let add4 first second = first + second
let answer = 10 |> add4 5  // val answer : int = 15

// Same as drive in Example 08 
let drive3(petrol, distance) = 
    if distance = "far" then petrol*0.5
    elif distance = "medium" then petrol*0.7
    else petrol*0.9
let petrol3 = 100.0
let firststate = drive3(petrol3, "far")           // val firststate : float = 50.0
let secondstate = drive3(firststate, "medium")   // val secondstate : float = 35.0
let thirdstate = drive3(secondstate, "short")    // val thirdstate : float = 31.5

// improved version using pipes
let drive4 distance petrol = 
    if distance = "far" then petrol*0.5
    elif distance = "medium" then petrol*0.7
    else petrol*0.9
petrol3 |> drive4 "far" |> drive4 "medium" |> drive4 "short"  // val it : float = 31.5

