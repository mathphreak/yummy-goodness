module KillFeed exposing (KillFeed, KillFeedEntry, view)

import Player exposing (Player)
import Equipment exposing (Equipment)
import Html exposing (Html)
import Html.Attributes


-- MODEL


type alias KillFeedEntry =
    { winner : Player
    , loser : Player
    , weapon : Equipment
    }


type alias KillFeed =
    List KillFeedEntry



-- VIEW


spriteName : Equipment -> String
spriteName e =
    case e of
        Equipment.PPBizon ->
            "bizon"

        Equipment.Berettas ->
            "dualies"

        Equipment.Flash ->
            "flashbang"

        Equipment.HENade ->
            "hegrenade"

        Equipment.Incendiary ->
            "incgrenade"

        Equipment.M4A4 ->
            "m4a1"

        Equipment.SG553 ->
            "sg556"

        Equipment.Smoke ->
            "smokegrenade"

        Equipment.Zeus ->
            "taser"

        Equipment.USPS ->
            "usp_silencer"

        Equipment.Vest ->
            "knife_karambit"

        Equipment.VestHelmet ->
            "knife_karambit"

        Equipment.Defuse ->
            "knife_karambit"

        _ ->
            String.toLower (toString e)


showKill : KillFeedEntry -> Html a
showKill kill =
    Html.div []
        [ Html.span [ Html.Attributes.class ("playerName " ++ String.toLower (toString kill.winner.team)) ] [ Html.text kill.winner.name ]
        , Html.img
            [ Html.Attributes.src
                ("http://tools.dathost.net/killfeed-generator/sprites/" ++ (spriteName kill.weapon) ++ ".png")
            ]
            []
        , Html.span [ Html.Attributes.class ("playerName " ++ String.toLower (toString kill.loser.team)) ] [ Html.text kill.loser.name ]
        ]


view : KillFeed -> Html a
view kills =
    Html.div [ Html.Attributes.class "killFeed" ]
        (kills |> List.map showKill)
