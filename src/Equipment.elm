-- based on all the work I already did in https://github.com/mathphreak/cs-eco-dash/blob/master/src/game/equipment.rs


module Equipment exposing (..)


type Team
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
        , PPBizon
        , P90
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
        , HENade
        , Flash
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
            "Vest"

        VestHelmet ->
            "Vest + Helmet"

        Zeus ->
            "Zeus x27"

        Defuse ->
            "Defuse kit"

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


cost : Equipment -> Int
cost e =
    case e of
        Glock ->
            200

        USPS ->
            200

        P250 ->
            300

        Deagle ->
            700

        Berettas ->
            500

        Tec9 ->
            500

        FiveSeven ->
            500

        Nova ->
            1200

        XM1014 ->
            2000

        SawedOff ->
            1200

        MAG7 ->
            1800

        MAC10 ->
            1050

        MP9 ->
            1250

        MP7 ->
            1700

        UMP45 ->
            1200

        PPBizon ->
            1400

        P90 ->
            2350

        GalilAR ->
            2000

        FAMAS ->
            2250

        AK47 ->
            2700

        M4A4 ->
            3100

        SSG08 ->
            1700

        SG553 ->
            3000

        AUG ->
            3300

        AWP ->
            4750

        G3SG1 ->
            5000

        SCAR20 ->
            5000

        M249 ->
            5200

        Negev ->
            5700

        Vest ->
            650

        VestHelmet ->
            1000

        Zeus ->
            200

        Defuse ->
            400

        Molotov ->
            400

        Incendiary ->
            600

        Decoy ->
            50

        HENade ->
            300

        Flash ->
            200

        Smoke ->
            300


teamRestriction : Equipment -> Maybe Team
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
