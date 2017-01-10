-- shamelessly stolen from CS:GO itself


module BotNames exposing (..)

import List
import Random
import Random.List exposing (..)


botNames =
    [ "Cliffe"
    , "Minh"
    , "Crusher"
    , "Garret"
    , "Rock"
    , "Shark"
    , "Wolf"
    , "Gunner"
    , "Vitaliy"
    , "Ridgway"
    , "Steel"
    , "Stone"
    , "Arnold"
    , "Brett"
    , "Kurt"
    , "Kyle"
    , "Moe"
    , "Quade"
    , "Quintin"
    , "Ringo"
    , "Rip"
    , "Zach"
    , "Cory"
    , "Quinn"
    , "Seth"
    , "Vinny"
    , "Brian"
    , "Chad"
    , "Chet"
    , "Gabe"
    , "Hank"
    , "Ivan"
    , "Jim"
    , "Joe"
    , "John"
    , "Tony"
    , "Tyler"
    , "Victor"
    , "Vladimir"
    , "Zane"
    , "Zim"
    , "Adrian"
    , "Brad"
    , "Connor"
    , "Dave"
    , "Dan"
    , "Derek"
    , "Don"
    , "Eric"
    , "Erik"
    , "Finn"
    , "Jeff"
    , "Kevin"
    , "Reed"
    , "Rick"
    , "Ted"
    , "Troy"
    , "Wade"
    , "Wayne"
    , "Xander"
    , "Xavier"
    , "Adam"
    , "Andy"
    , "Chris"
    , "Colin"
    , "Dennis"
    , "Doug"
    , "Duffy"
    , "Gary"
    , "Grant"
    , "Greg"
    , "Ian"
    , "Jerry"
    , "Jon"
    , "Keith"
    , "Mark"
    , "Matt"
    , "Mike"
    , "Nate"
    , "Paul"
    , "Scott"
    , "Steve"
    , "Tom"
    , "Yahn"
    , "Alfred"
    , "Bill"
    , "Brandon"
    , "Calvin"
    , "Dean"
    , "Dustin"
    , "Ethan"
    , "Harold"
    , "Henry"
    , "Irving"
    , "Jason"
    , "Jenssen"
    , "Josh"
    , "Martin"
    , "Nick"
    , "Norm"
    , "Orin"
    , "Pat"
    , "Perry"
    , "Ron"
    , "Shawn"
    , "Tim"
    , "Will"
    , "Wyatt"
    , "Albert"
    , "Allen"
    , "Bert"
    , "Bob"
    , "Cecil"
    , "Clarence"
    , "Elliot"
    , "Elmer"
    , "Ernie"
    , "Eugene"
    , "Fergus"
    , "Ferris"
    , "Frank"
    , "Frasier"
    , "Fred"
    , "George"
    , "Graham"
    , "Harvey"
    , "Irwin"
    , "Larry"
    , "Lester"
    , "Marvin"
    , "Neil"
    , "Niles"
    , "Oliver"
    , "Opie"
    , "Ryan"
    , "Toby"
    , "Ulric"
    , "Ulysses"
    , "Uri"
    , "Waldo"
    , "Wally"
    , "Walt"
    , "Wesley"
    , "Yanni"
    , "Yogi"
    , "Yuri"
    ]


pickNames : Int -> Random.Generator (List String)
pickNames count =
    botNames
        |> shuffle
        |> Random.map (List.take count)