-- based on all the work I already did in https://github.com/mathphreak/cs-eco-dash/blob/master/src/game/equipment.rs


module Equipment exposing (..)


type Side
    = CT
    | T


type Submenu
    = Pistols
    | Heavy
    | SMGs
    | Rifles
    | GearMenu
    | Grenades


type Equipment
    = Glock
    | USPS
    | P250
    | Deagle
    | Berettas
    | Tec9
    | FiveSeven
    | Nova
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
    | SSG08
    | SG553
    | AUG
    | AWP
    | G3SG1
    | SCAR20
    | Vest
    | VestHelmet
    | Zeus
    | Defuse
    | Molotov
    | Incendiary
    | Decoy
    | HENade
    | Flash
    | Smoke


type Slot
    = Primary
    | Secondary
    | GearSlot
    | Grenade


lists =
    { pistols =
        [ Glock
        , USPS
        , Berettas
        , P250
        , Tec9
        , FiveSeven
        , Deagle
        ]
    , heavy =
        [ Nova
        , XM1014
        , SawedOff
        , MAG7
        , M249
        , Negev
        ]
    , smgs =
        [ MAC10
        , MP9
        , MP7
        , UMP45
        , P90
        , PPBizon
        ]
    , rifles =
        [ GalilAR
        , FAMAS
        , AK47
        , M4A4
        , SSG08
        , SG553
        , AUG
        , AWP
        , G3SG1
        , SCAR20
        ]
    , gear =
        [ Vest
        , VestHelmet
        , Zeus
        , Defuse
        ]
    , grenades =
        [ Molotov
        , Incendiary
        , Decoy
        , Flash
        , HENade
        , Smoke
        ]
    , submenus =
        [ Pistols
        , Heavy
        , SMGs
        , Rifles
        , GearMenu
        , Grenades
        ]
    }


defaultPistol : Side -> Equipment
defaultPistol s =
    case s of
        CT ->
            USPS

        T ->
            Glock


listFor : Submenu -> List Equipment
listFor m =
    case m of
        Pistols ->
            lists.pistols

        Heavy ->
            lists.heavy

        SMGs ->
            lists.smgs

        Rifles ->
            lists.rifles

        GearMenu ->
            lists.gear

        Grenades ->
            lists.grenades


submenuName : Submenu -> String
submenuName m =
    case m of
        Pistols ->
            "PISTOLS"

        Heavy ->
            "HEAVY"

        SMGs ->
            "SMGs"

        Rifles ->
            "RIFLES"

        GearMenu ->
            "GEAR"

        Grenades ->
            "GRENADES"


toString : Equipment -> String
toString e =
    case e of
        Glock ->
            "Glock"

        USPS ->
            "USP-S"

        P250 ->
            "P250"

        Deagle ->
            "Desert Eagle"

        Berettas ->
            "Dual Berettas"

        Tec9 ->
            "Tec-9"

        FiveSeven ->
            "Five-SeveN"

        Nova ->
            "Nova"

        XM1014 ->
            "XM1014"

        SawedOff ->
            "Sawed-Off"

        MAG7 ->
            "MAG-7"

        MAC10 ->
            "MAC-10"

        MP9 ->
            "MP9"

        MP7 ->
            "MP7"

        UMP45 ->
            "UMP-45"

        PPBizon ->
            "PP-Bizon"

        P90 ->
            "P90"

        GalilAR ->
            "Galil AR"

        FAMAS ->
            "FAMAS"

        AK47 ->
            "AK-47"

        M4A4 ->
            "M4A4"

        SSG08 ->
            "SSG 08"

        SG553 ->
            "SG 553"

        AUG ->
            "AUG"

        AWP ->
            "AWP"

        G3SG1 ->
            "G3SG1"

        SCAR20 ->
            "SCAR-20"

        M249 ->
            "M249"

        Negev ->
            "Negev"

        Vest ->
            "Kevlar"

        VestHelmet ->
            "Kevlar + Helmet"

        Zeus ->
            "Zeus x27"

        Defuse ->
            "Defuse Kit"

        Molotov ->
            "Molotov"

        Incendiary ->
            "Incendiary"

        Decoy ->
            "Decoy"

        HENade ->
            "HE grenade"

        Flash ->
            "Flashbang"

        Smoke ->
            "Smoke"


teamRestriction : Equipment -> Maybe Side
teamRestriction e =
    case e of
        Glock ->
            Just T

        USPS ->
            Just CT

        Tec9 ->
            Just T

        FiveSeven ->
            Just CT

        SawedOff ->
            Just T

        MAG7 ->
            Just CT

        MAC10 ->
            Just T

        MP9 ->
            Just CT

        GalilAR ->
            Just T

        FAMAS ->
            Just CT

        AK47 ->
            Just T

        M4A4 ->
            Just CT

        SG553 ->
            Just T

        AUG ->
            Just CT

        G3SG1 ->
            Just T

        SCAR20 ->
            Just CT

        Defuse ->
            Just CT

        Molotov ->
            Just T

        Incendiary ->
            Just CT

        _ ->
            Nothing


slot : Equipment -> Slot
slot e =
    case e of
        Glock ->
            Secondary

        USPS ->
            Secondary

        P250 ->
            Secondary

        Deagle ->
            Secondary

        Berettas ->
            Secondary

        Tec9 ->
            Secondary

        FiveSeven ->
            Secondary

        Vest ->
            GearSlot

        VestHelmet ->
            GearSlot

        Zeus ->
            GearSlot

        Defuse ->
            GearSlot

        Molotov ->
            Grenade

        Incendiary ->
            Grenade

        Decoy ->
            Grenade

        HENade ->
            Grenade

        Flash ->
            Grenade

        Smoke ->
            Grenade

        _ ->
            Primary



-- statistics from http://strike-counter.com/cs-go-stats/weapons-stats and http://counterstrike.wikia.com/


type alias WeaponStats =
    { cost : Int
    , killReward : Int
    , baseDamage : Int
    , ticksBetweenShots : Int
    , armorPenetration : Float
    }


emptyStats =
    WeaponStats 0 0 0 0 0


stats : Equipment -> WeaponStats
stats e =
    case e of
        Glock ->
            WeaponStats 200 300 28 10 47

        USPS ->
            WeaponStats 200 300 35 11 50.5

        P250 ->
            WeaponStats 300 300 35 10 77.65

        Deagle ->
            WeaponStats 700 300 63 14 93.2

        Berettas ->
            WeaponStats 500 300 38 8 52.5

        Tec9 ->
            WeaponStats 500 300 33 8 90.6

        FiveSeven ->
            WeaponStats 500 300 32 10 91.15

        Nova ->
            WeaponStats 1200 900 (26 * 2) 56 50

        XM1014 ->
            WeaponStats 2000 900 (20 * 2) 22 80

        SawedOff ->
            WeaponStats 1200 900 (32 * 2) 54 75

        MAG7 ->
            WeaponStats 1800 900 (30 * 2) 54 75

        MAC10 ->
            WeaponStats 1050 600 29 5 47.5

        MP9 ->
            WeaponStats 1250 600 26 4 50

        MP7 ->
            WeaponStats 1700 600 29 5 52.5

        UMP45 ->
            WeaponStats 1200 600 35 6 55

        PPBizon ->
            WeaponStats 1400 600 27 5 47.5

        P90 ->
            WeaponStats 2350 300 26 4 65

        GalilAR ->
            WeaponStats 2000 300 30 6 77.5

        FAMAS ->
            WeaponStats 2250 300 30 6 70

        AK47 ->
            WeaponStats 2700 300 36 6 77.5

        M4A4 ->
            WeaponStats 3100 300 33 6 70

        SSG08 ->
            WeaponStats 1700 300 88 80 85

        SG553 ->
            WeaponStats 3000 300 30 6 100

        AUG ->
            WeaponStats 3300 300 28 6 90

        AWP ->
            WeaponStats 4750 100 115 94 97.5

        G3SG1 ->
            WeaponStats 5000 300 80 16 82.5

        SCAR20 ->
            WeaponStats 5000 300 80 16 82.5

        M249 ->
            WeaponStats 5200 300 32 5 80

        Negev ->
            WeaponStats 5700 300 35 4 75

        Vest ->
            WeaponStats 650 0 0 0 0

        VestHelmet ->
            WeaponStats 1000 0 0 0 0

        Zeus ->
            WeaponStats 200 0 500 0 0

        Defuse ->
            WeaponStats 400 0 0 0 0

        -- shout out to https://www.youtube.com/watch?v=MbDkyndlQGk
        Molotov ->
            WeaponStats 400 300 32 0 0

        Incendiary ->
            WeaponStats 600 300 32 0 0

        Decoy ->
            WeaponStats 50 0 0 0 0

        HENade ->
            WeaponStats 300 300 (98 // 2) 0 0

        Flash ->
            WeaponStats 200 0 0 0 0

        Smoke ->
            WeaponStats 300 0 0 0 0


cost : Equipment -> Int
cost e =
    .cost (stats e)
