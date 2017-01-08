module Player exposing (..)

import Equipment exposing (..)


type alias Player =
    { money : Int
    , primary : Maybe Primary
    , secondary : Maybe Pistol
    , armor : Maybe Armor
    , grenades : List Grenade
    , misc : List Misc
    }
