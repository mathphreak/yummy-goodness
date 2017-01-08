-- based on all the work I already did in https://github.com/mathphreak/cs-eco-dash/blob/master/src/game/equipment.rs


module Equipment exposing (..)


type Pistol
    = Glock
    | P2000
    | USPS
    | P250
    | Deagle
    | Berettas
    | Tec9
    | FiveSeven
    | CZ75
    | R8


type Primary
    = Nova
    | XM1014
    | SawedOff
    | MAG7
    | M249
    | Negev
    | MAC10
    | MP9
    | MP7
    | UMP45
    | PPBizon
    | P90
    | GalilAR
    | FAMAS
    | AK47
    | M4A4
    | M4A1S
    | SSG08
    | SG553
    | AUG
    | AWP
    | G3GS1
    | SCAR20


type Armor
    = Vest
    | VestHelmet


type Misc
    = Zeus
    | Defuse


type Grenade
    = Molotov
    | Incendiary
    | Decoy
    | HENade
    | Flash
    | Smoke
