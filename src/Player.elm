module Player
    exposing
        ( Player
        , newPlayer
        , dead
        , Msg
        , update
        , buyMenuFor
        , view
        )

import Equipment exposing (Equipment, Side, Submenu)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import String
import BuyMenu


-- MODEL


type alias Player =
    { money : Int
    , team : Side
    , secondary : Maybe Equipment
    , primary : Maybe Equipment
    , gear : List Equipment
    , grenades : List Equipment
    , submenu : Maybe Submenu
    , name : String
    }


newPlayer : Side -> String -> Player
newPlayer side name =
    let
        pistol =
            case side of
                Equipment.CT ->
                    Equipment.USPS

                Equipment.T ->
                    Equipment.Glock
    in
        Player 800 side (Just pistol) Nothing [] [] Nothing name


dead : Player -> Player
dead p =
    let
        pistol =
            case p.team of
                Equipment.CT ->
                    Equipment.USPS

                Equipment.T ->
                    Equipment.Glock
    in
        { p | secondary = Just pistol, primary = Nothing, gear = [], grenades = [] }



-- UPDATE


type Msg
    = Purchase Equipment
    | MenuSelect (Maybe Submenu)


update : Msg -> Player -> Player
update msg origPlayer =
    case msg of
        Purchase item ->
            let
                newMoney =
                    origPlayer.money - (Equipment.cost item)

                player =
                    { origPlayer | money = newMoney, submenu = Nothing }

                sortByName =
                    List.sortBy Equipment.toString
            in
                case (Equipment.slot item) of
                    Equipment.Primary ->
                        { player | primary = Just item }

                    Equipment.Secondary ->
                        { player | secondary = Just item }

                    Equipment.GearSlot ->
                        if item == Equipment.VestHelmet then
                            { player | gear = sortByName (item :: (List.filter ((/=) Equipment.Vest) player.gear)) }
                        else
                            { player | gear = sortByName (item :: player.gear) }

                    Equipment.Grenade ->
                        { player | grenades = sortByName (item :: player.grenades) }

        MenuSelect sm ->
            { origPlayer | submenu = sm }



-- VIEW


playerCanUseEquipment : Player -> Equipment -> Bool
playerCanUseEquipment p e =
    case (Equipment.teamRestriction e) of
        Just team ->
            p.team == team

        Nothing ->
            True


playerCanPurchaseEquipment : Player -> Equipment -> Bool
playerCanPurchaseEquipment p e =
    if (Equipment.cost e) > p.money then
        False
    else if (p.primary == Just e) then
        False
    else if (p.secondary == Just e) then
        False
    else if not (playerCanUseEquipment p e) then
        False
    else if (List.member e p.gear) then
        False
    else if (e == Equipment.Vest && List.member Equipment.VestHelmet p.gear) then
        False
    else if ((Equipment.slot e) == Equipment.Grenade) && List.length p.grenades > 3 then
        False
    else if (e /= Equipment.Flash && List.member e p.grenades) then
        False
    else if (e == Equipment.Flash && (List.length (List.filter ((==) Equipment.Flash) p.grenades)) > 1) then
        False
    else
        True


buyMenuFor : (Msg -> msg) -> Player -> Html msg
buyMenuFor msg player =
    let
        canPurchase =
            playerCanPurchaseEquipment player
    in
        case player.submenu of
            Nothing ->
                BuyMenu.viewMenu (\a -> (msg <| MenuSelect <| Just a)) player.team

            Just submenu ->
                BuyMenu.viewSubmenu
                    (msg <| MenuSelect Nothing)
                    (\a -> msg <| Purchase a)
                    canPurchase
                    player.team
                    ((Equipment.listFor submenu) |> List.filter (playerCanUseEquipment player))


view : Maybe msg -> Bool -> Player -> Html msg
view onClick selected player =
    let
        main =
            [ player.primary, player.secondary ] |> List.filterMap (Maybe.map identity)

        inventory =
            main ++ player.gear ++ player.grenades

        selectionAttr =
            if selected then
                [ Html.Attributes.class "player selected" ]
            else
                [ Html.Attributes.class "player" ]

        handleClick =
            case onClick of
                Just msg ->
                    [ Html.Events.onClick msg ]

                Nothing ->
                    []
    in
        Html.div (selectionAttr ++ handleClick)
            [ Html.h2 []
                [ Html.strong [] [ Html.text player.name ]
                , Html.text (", $" ++ toString player.money)
                ]
            , Html.p [ Html.Attributes.class "inventory" ]
                (inventory
                    |> List.map (\i -> Html.span [] [ Html.text (Equipment.toString i) ])
                    |> List.intersperse (Html.text ", ")
                )
            ]
