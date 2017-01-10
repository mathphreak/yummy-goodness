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


wedge : Int -> Int -> String -> String -> Maybe msg -> Html msg
wedge count idx top bottom msg =
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

        eventAttributes =
            case msg of
                Just x ->
                    [ onClick x ]

                Nothing ->
                    []
    in
        a ([ class "wedge" ] ++ eventAttributes)
            [ Svg.path [ d ("M 50 50 L " ++ startX ++ " " ++ startY ++ " A 50 50 0 0 0 " ++ endX ++ " " ++ endY ++ " L 50 50 z"), fill "black" ] []
            , text_ [ x midX, y midY1, fill "white", textAnchor "middle", fontSize "5px", dominantBaseline "middle" ] [ text top ]
            , text_ [ x midX, y midY2, fill "white", textAnchor "middle", fontSize "5px", dominantBaseline "middle" ] [ text bottom ]
            ]


viewSix : (Equipment -> msg) -> Equipment.Team -> List Equipment -> Html msg
viewSix msg team inv =
    let
        buildItem i e =
            { index = (i + 1), cost = "$" ++ (toString (Equipment.cost e)), name = Equipment.toString e, action = Just (msg e) }

        items =
            List.concat
                [ inv
                    |> List.indexedMap buildItem
                , List.repeat (6 - (List.length inv)) { index = 6, cost = "", name = "", action = Nothing }
                ]

        wedgeFor item =
            wedge 6 item.index item.cost item.name item.action

        teamColor =
            if team == Equipment.CT then
                "#0000DD"
            else
                "#DD00AA"
    in
        svg [ viewBox "0 0 100 100", width "300px" ]
            ((List.map wedgeFor items)
                ++ [ lineAtAngle (pi / 6)
                   , lineAtAngle (3 * pi / 6)
                   , lineAtAngle (5 * pi / 6)
                   , circle [ cx "50", cy "50", r "10", fill "#FFFFFF" ] []
                   , circle [ cx "50", cy "50", r "7.5", fill teamColor ] []
                   ]
            )


viewFour : (Equipment -> msg) -> Equipment.Team -> List Equipment -> Html msg
viewFour msg team inv =
    let
        buildItem i e =
            { index = (i + 1), cost = "$" ++ (toString (Equipment.cost e)), name = Equipment.toString e, action = Just (msg e) }

        items =
            List.concat
                [ inv
                    |> List.indexedMap buildItem
                , List.repeat (4 - (List.length inv)) { index = 4, cost = "", name = "", action = Nothing }
                ]

        wedgeFor item =
            wedge 4 item.index item.cost item.name item.action

        teamColor =
            if team == Equipment.CT then
                "#0000DD"
            else
                "#DD00AA"
    in
        svg [ viewBox "0 0 100 100", width "300px" ]
            ((List.map wedgeFor items)
                ++ [ lineAtAngle (pi / 4)
                   , lineAtAngle (3 * pi / 4)
                   , circle [ cx "50", cy "50", r "10", fill "#FFFFFF" ] []
                   , circle [ cx "50", cy "50", r "7.5", fill teamColor ] []
                   ]
            )


viewSubmenu : (Equipment -> msg) -> Equipment.Team -> List Equipment -> Html msg
viewSubmenu msg team inv =
    if (List.length inv) > 4 then
        (viewSix msg team inv)
    else
        (viewFour msg team inv)


viewMenu : (Equipment.Submenu -> msg) -> Equipment.Team -> Html msg
viewMenu msg team =
    let
        teamColor =
            if team == Equipment.CT then
                "#0000DD"
            else
                "#DD00AA"
    in
        svg [ viewBox "0 0 100 100", width "300px" ]
            [ wedge 6 1 "" "PISTOLS" (Just (msg Equipment.Pistols))
            , wedge 6 2 "" "HEAVY" (Just (msg Equipment.Heavy))
            , wedge 6 3 "" "SMGs" (Just (msg Equipment.SMGs))
            , wedge 6 4 "" "RIFLES" (Just (msg Equipment.Rifles))
            , wedge 6 5 "" "GEAR" (Just (msg Equipment.GearMenu))
            , wedge 6 6 "" "GRENADES" (Just (msg Equipment.Grenades))
            , lineAtAngle (pi / 6)
            , lineAtAngle (3 * pi / 6)
            , lineAtAngle (5 * pi / 6)
            , circle [ cx "50", cy "50", r "10", fill "#FFFFFF" ] []
            , circle [ cx "50", cy "50", r "7.5", fill teamColor ] []
            ]
