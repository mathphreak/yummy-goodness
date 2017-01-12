module BuyMenu exposing (viewMenu, viewSubmenu)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Equipment exposing (Equipment)


-- VIEW


lineAtAngle : Float -> Html msg
lineAtAngle angle =
    line
        [ x1 (toString (50 + 50 * cos angle))
        , y1 (toString (50 + 50 * sin angle))
        , x2 (toString (50 + 50 * cos (angle + pi)))
        , y2 (toString (50 + 50 * sin (angle + pi)))
        , stroke "#808080"
        ]
        []


wedge : Int -> Int -> String -> String -> Maybe msg -> Bool -> Bool -> Html msg
wedge count idx top bottom msg topEnabled bottomEnabled =
    let
        startAngle =
            (2 * pi * ((toFloat (idx * 2) + 1) / (toFloat count * 2))) + pi

        midAngle =
            (2 * pi * ((toFloat (idx * 2)) / (toFloat count * 2))) + pi

        endAngle =
            (2 * pi * ((toFloat (idx * 2) - 1) / (toFloat count * 2))) + pi

        startX =
            toString (50 + 50 * cos startAngle)

        startY =
            toString (50 + 50 * sin startAngle)

        midX =
            toString (50 + 30 * cos midAngle)

        midY1 =
            toString (47 + 30 * sin midAngle)

        midY2 =
            toString (53 + 30 * sin midAngle)

        endX =
            toString (50 + 50 * cos endAngle)

        endY =
            toString (50 + 50 * sin endAngle)

        linkAttributes =
            if msg /= Nothing && topEnabled then
                [ class "wedge link" ]
            else
                [ class "wedge" ]

        eventAttributes =
            case msg of
                Nothing ->
                    []

                Just x ->
                    if topEnabled then
                        [ onClick x ]
                    else
                        []

        topColor =
            if topEnabled then
                "lime"
            else
                "grey"

        bottomColor =
            if bottomEnabled then
                "white"
            else
                "red"
    in
        a (linkAttributes ++ eventAttributes)
            [ Svg.path [ d ("M 50 50 L " ++ startX ++ " " ++ startY ++ " A 50 50 0 0 0 " ++ endX ++ " " ++ endY ++ " L 50 50 z"), fill "black" ] []
            , text_ [ x midX, y midY1, fill topColor, textAnchor "middle", fontSize "5px", dominantBaseline "middle" ] [ text top ]
            , text_ [ x midX, y midY2, fill bottomColor, textAnchor "middle", fontSize "5px", dominantBaseline "middle" ] [ text bottom ]
            ]


center : Maybe msg -> Equipment.Side -> Html msg
center backMsg team =
    let
        teamColor =
            if team == Equipment.CT then
                "hsl(207, 30%, 30%)"
            else
                "hsl(45, 30%, 30%)"

        clickCenterAction =
            case backMsg of
                Nothing ->
                    []

                Just msg ->
                    [ class "link", onClick msg ]

        centerLabel =
            case backMsg of
                Nothing ->
                    []

                _ ->
                    [ text_ [ x "50", y "50", fill "white", textAnchor "middle", dominantBaseline "middle", fontSize "4px" ] [ text "BACK" ] ]
    in
        a clickCenterAction
            ([ circle [ cx "50", cy "50", r "10", fill "#FFFFFF" ] []
             , circle [ cx "50", cy "50", r "7.5", fill teamColor ] []
             ]
                ++ centerLabel
            )


viewSix : msg -> (Equipment -> msg) -> (Equipment -> Bool) -> Equipment.Side -> List Equipment -> Html msg
viewSix backMsg msg canPurchase team inv =
    let
        buildItem i e =
            { index = (i + 1)
            , cost = "$" ++ (toString (Equipment.cost e))
            , name = Equipment.toString e
            , action = Just (msg e)
            , enabled = canPurchase e
            }

        items =
            List.concat
                [ inv
                    |> List.indexedMap buildItem
                , List.repeat (6 - (List.length inv)) { index = 6, cost = "", name = "", action = Nothing, enabled = False }
                ]

        wedgeFor item =
            wedge 6 item.index item.cost item.name item.action item.enabled True
    in
        svg [ viewBox "0 0 100 100", width "300px" ]
            ((List.map wedgeFor items)
                ++ [ lineAtAngle (pi / 6)
                   , lineAtAngle (3 * pi / 6)
                   , lineAtAngle (5 * pi / 6)
                   , center (Just backMsg) team
                   ]
            )


viewFour : msg -> (Equipment -> msg) -> (Equipment -> Bool) -> Equipment.Side -> List Equipment -> Html msg
viewFour backMsg msg canPurchase team inv =
    let
        buildItem i e =
            { index = (i + 1)
            , cost = "$" ++ (toString (Equipment.cost e))
            , name = Equipment.toString e
            , action = Just (msg e)
            , enabled = canPurchase e
            }

        items =
            List.concat
                [ inv
                    |> List.indexedMap buildItem
                , List.repeat (4 - (List.length inv)) { index = 4, cost = "", name = "", action = Nothing, enabled = False }
                ]

        wedgeFor item =
            wedge 4 item.index item.cost item.name item.action item.enabled True
    in
        svg [ viewBox "0 0 100 100", width "300px" ]
            ((List.map wedgeFor items)
                ++ [ lineAtAngle (pi / 4)
                   , lineAtAngle (3 * pi / 4)
                   , center (Just backMsg) team
                   ]
            )


viewSubmenu : msg -> (Equipment -> msg) -> (Equipment -> Bool) -> Equipment.Side -> List Equipment -> Html msg
viewSubmenu backMsg msg canPurchase team inv =
    if (List.length inv) > 4 then
        (viewSix backMsg msg canPurchase team inv)
    else
        (viewFour backMsg msg canPurchase team inv)


viewMenu : (Equipment.Submenu -> msg) -> (Equipment -> Bool) -> Equipment.Side -> Html msg
viewMenu msg canPurchase team =
    let
        anyAffordable : Equipment.Submenu -> Bool
        anyAffordable m =
            List.any canPurchase (Equipment.listFor m)

        buildWedge : Int -> Equipment.Submenu -> Html msg
        buildWedge i m =
            wedge 6 (i + 1) "" (Equipment.submenuName m) (Just (msg m)) True (anyAffordable m)
    in
        svg [ viewBox "0 0 100 100", width "300px" ]
            ((List.indexedMap buildWedge (.submenus Equipment.lists))
                ++ [ lineAtAngle (pi / 6)
                   , lineAtAngle (3 * pi / 6)
                   , lineAtAngle (5 * pi / 6)
                   , center Nothing team
                   ]
            )
