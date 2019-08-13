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

module MovieValidations=
    type VError= | MustNotBeEmpty
                 | MustBeAtLessThanChars of int
                 | MustBeADate
                 | MustBeOlderThan of int
                 | MustBeWithingRange of int*int
    module String=
        let nonEmpty (x:string) : Validation<VError list,string> = 
            if not <| String.IsNullOrEmpty x 
            then Success <| x
            else Failure [MustNotBeEmpty]
        let mustBeLessThan (i:int) (x:string) : Validation<VError list,string> = 
            if not <| isNull x && x.Length<=i
            then Success <| x
            else Failure [MustBeAtLessThanChars i]
    module Number=
        let inline mustBeWithin (from,to') (x)=
            if from<= x && x <= to'
            then Success <| x
            else Failure [MustBeWithingRange ((int from),(int to'))]
    module DateTime=
        let classicMovie year (d:DateTime)=
            if d.Year < year
            then Success <| d
            else Failure [MustBeOlderThan year]
        let date (d:DateTime)=
            if d.Hour = 0 && d.Minute =0 && d.Second = 0
            then Success <| d
            else Failure [MustBeADate]
    type Genre=
        |Classic
        |PostClassic
        |Modern
        |PostModern
        |Contemporary
    type Movie = {
        Id: int
        Title: String
        ReleaseDate: DateTime
        Description: String
        Price: decimal
        Genre: Genre
    }
    with static member Mk(id,title,releaseDate,description,price,genre): Validation<VError list,Movie> =
            fun title releaseDate description price->{ Id=id;Title=title;ReleaseDate=releaseDate;Description=description;Price=price;Genre=genre }
            <!> (String.nonEmpty title) <* (String.mustBeLessThan 100 title)
            <*> (DateTime.classicMovie 1960 releaseDate) <* (DateTime.date releaseDate)
            <*> (String.nonEmpty description) <* (String.mustBeLessThan 1000 description)
            <*> (Number.mustBeWithin (0.0m, 999.99m) price)

    let newRelease = Movie.Mk(1,"Midsommar",DateTime(2019,6,24),"Midsommar is a 2019 folk horror film written and directed by Ari Aster and starring Florence Pugh, Jack Reynor, William Jackson Harper, Vilhelm Blomgren, and Will Poulter. It follows a group of friends who travel to Sweden for a festival that occurs once every ninety years and find themselves in the clutches of a pagan cult.",1m,Classic) //Failure [MustBeOlderThan 1960]
    let oldie = Movie.Mk(2,"Münchhausen",DateTime(1943,1,1),"Münchhausen is a 1943 fantasy comedy film directed by Josef von Báky. Science fiction author David Wingrove has commented that this work \"sidesteps immediate political issues whilst conjuring up marvellous visual images of an ageless pastoral Germany.",1m,Classic) // Success..
    let titleToLong = Movie.Mk(3, String.Concat (seq{  1..110 }), DateTime(1943,1,1),"11",1m,Classic) //Failure [MustBeAtLessThanChars 100]
