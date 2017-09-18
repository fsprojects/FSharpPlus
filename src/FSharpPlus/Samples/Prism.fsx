#nowarn "3186"
#r @"../bin/Release/FSharpPlus.dll"

open FSharpPlus
open FSharpPlus.Lens

type Team   = {name: string; victories: int}
let inline _name      f {name = a; victories = b} = map (fun a' -> {name = a'; victories = b }) (f a)
let inline _victories f {name = a; victories = b} = map (fun b' -> {name = a ; victories = b'}) (f b)

type Player = {team: Team; score: int}
let inline _team  f {team = a; score = b} = map (fun a' -> {team = a'; score = b }) (f a)
let inline _score f {team = a; score = b} = map (fun b' -> {team = a ; score = b'}) (f b)

type Result = {winner: Player option; started: bool}
let inline _winner   f {winner = a; started = b} = map (fun a' -> {winner = a'; started = b }) (f a)
let inline _started  f {winner = a; started = b} = map (fun b' -> {winner = a ; started = b'}) (f b)

type Match<'t>  = {players: 't; finished: bool}
let inline _players  f {players = a; finished = b} = map (fun a' -> {players = a'; finished = b }) (f a)
let inline _finished f {players = a; finished = b} = map (fun b' -> {players = a ; finished = b'}) (f b)

// Lens composed with Prism -> Prism
let inline _winner_team x = (_players << _winner << _Some << _team) x

// initial state
let match0 = {players = ({team = {name = "The A Team"; victories = 0}; score = 0}, {team = {name = "The B Team"; victories = 0}; score = 0}) ; finished = false}


// Team 1 scores
let match1 = over (_players << _1 << _score) ((+) 1) match0

// Team 2 scores
let match2 = over (_players << _2 << _score) ((+) 1) match1

// Produce Match<Result> from Match<Player * Player> 
// This is possible with these Lenses since they support polymorphic updates.
let matchResult0 = set _players {winner = None; started = true} match2

// See if there is a winner by using a prism
let _noWinner = preview _winner_team matchResult0

// Team 1 scores
let match3 = over (_players << _1 << _score) ((+) 1) match2

// End of the match
let match4 = set _finished true match3
let match5 = over (_players << _1 << _team << _victories) ((+) 1) match4
let matchResult1 = over _players (fun (x, _) -> {winner = Some x; started = true}) match5

// And the winner is ...
let winner = preview _winner_team matchResult1