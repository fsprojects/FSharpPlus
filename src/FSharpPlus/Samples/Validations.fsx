#if INTERACTIVE
#r @"../bin/Release/net45/FSharpPlus.dll"
#else
module Samples.Validations
#endif
open System
open FSharpPlus
open FSharpPlus.Data

module Person=
    type Name = { unName : String } 
    with static member create s={unName=s}
    type Email = { unEmail : String } 
    with static member create s={unEmail=s}
    type Age = { unAge : int }
    with static member create i={unAge=i}

    type Person = { name : Name
                    email : Email
                    age : Age }
    with static member create name email age={name=name;email=email;age=age }


    type Error = 
        | NameBetween1And50
        | EmailMustContainAtChar
        | AgeBetween0and120

    // Smart constructors
    let mkName s = 
        let l = length s
        if (l >= 1 && l <= 50)
        then Success <| Name.create s
        else Failure  [ NameBetween1And50 ]

    let mkEmail s = 
        if String.contains '@' s
        then Success <| Email.create s
        else Failure [ EmailMustContainAtChar ]

    let mkAge a = 
        if (a >= 0 && a <= 120)
        then Success <| Age.create a
        else Failure [ AgeBetween0and120 ]

    let mkPerson pName pEmail pAge =
        Person.create
        <!> mkName pName
        <*> mkEmail pEmail
        <*> mkAge pAge

    // Examples

    let validPerson = mkPerson "Bob" "bob@gmail.com" 25
    // Success ({name = {unName = "Bob"}; email = {unEmail = "bob@gmail.com"}; age = {unAge = 25}})

    let badName = mkPerson "" "bob@gmail.com" 25
    // Failure [NameBetween1And50]

    let badEmail = mkPerson "Bob" "bademail" 25
    // Failure [EmailMustContainAtChar]

    let badAge = mkPerson "Bob" "bob@gmail.com" 150
    // Failure [AgeBetween0and120]

    let badEverything = mkPerson "" "bademail" 150
    // Failure [NameBetween1And50;EmailMustContainAtChar;AgeBetween0and120]

    open FSharpPlus.Lens
    let asMaybeGood = validPerson ^? Validation._Success
    // Some ({name = {unName = "Bob"}; email = {unEmail = "bob@gmail.com"}; age = {unAge = 25}})
    let asMaybeBad = badEverything ^? Validation._Success
    // None

    let asResultGood = validPerson ^. Validation.isoValidationResult
    // Ok ({name = {unName = "Bob"}; email = {unEmail = "bob@gmail.com"}; age = {unAge = 25}})

    let asResultBad = badEverything ^. Validation.isoValidationResult
    // Error [NameBetween1And50;EmailMustContainAtChar;AgeBetween0and120]


module Email =

    // ***** Types *****
    type AtString = AtString of string 
    type PeriodString = PeriodString of string 
    type NonEmptyString = NonEmptyString of string 

    type Email = Email of string 

    type VError = | MustNotBeEmpty
                  | MustContainAt
                  | MustContainPeriod

    // ***** Base smart constructors *****
    // String must contain an '@' character
    let atString (x:string) : Validation<VError list,AtString> =
        if String.contains '@' x then Success <| AtString x
        else Failure [MustContainAt]

    // String must contain an '.' character
    let periodString (x:string) : Validation<VError list,PeriodString> = 
        if String.contains '.' x
        then Success <| PeriodString x
        else Failure [MustContainPeriod]

    // String must not be empty
    let nonEmptyString (x:string) : Validation<VError list,NonEmptyString> = 
        if not <| String.IsNullOrEmpty x 
        then Success <| NonEmptyString x
        else Failure [MustNotBeEmpty]

    // ***** Combining smart constructors *****
    let email (x:string) : Validation<VError list, Email> = 
        result (Email x) <*
        nonEmptyString x <*
        atString       x <*
        periodString   x

    // ***** Example usage *****
    let success = email "bob@gmail.com"

    // Success (Email "bob@gmail.com")

    let failureAt = email "bobgmail.com"
    // Failure [MustContainAt]

    let failurePeriod = email "bob@gmailcom"
    // Failure [MustContainPeriod]


    let failureAll = email ""
    // Failure [MustNotBeEmpty;MustContainAt;MustContainPeriod]