module BuyMenu exposing (view)

import Html exposing (Html)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


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


wedge : Int -> Int -> String -> String -> msg -> Html msg
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
    in
        a [ class "wedge", onClick msg ]
            [ Svg.path [ d ("M 50 50 L " ++ startX ++ " " ++ startY ++ " A 50 50 0 0 0 " ++ endX ++ " " ++ endY ++ " L 50 50 z"), fill "black" ] []
            , text_ [ x midX, y midY1, fill "white", textAnchor "middle", fontSize "5px", dominantBaseline "middle" ] [ text top ]
            , text_ [ x midX, y midY2, fill "white", textAnchor "middle", fontSize "5px", dominantBaseline "middle" ] [ text bottom ]
            ]


viewSix : msg -> Html msg
viewSix msg =
    svg [ viewBox "0 0 100 100", width "300px" ]
        [ wedge 6 1 "" "PISTOLS" msg
        , wedge 6 2 "" "HEAVY" msg
        , wedge 6 3 "" "SMGs" msg
        , wedge 6 4 "" "RIFLES" msg
        , wedge 6 5 "" "GEAR" msg
        , wedge 6 6 "" "GRENADES" msg
        , lineAtAngle (pi / 6)
        , lineAtAngle (3 * pi / 6)
        , lineAtAngle (5 * pi / 6)
        , circle [ cx "50", cy "50", r "10", fill "#FFFFFF" ] []
        , circle [ cx "50", cy "50", r "7.5", fill "#0000DD" ] []
        ]


viewFour : msg -> Html msg
viewFour msg =
    svg [ viewBox "0 0 100 100", width "300px" ]
        [ wedge 4 1 "" "PISTOLS" msg
        , wedge 4 2 "" "HEAVY" msg
        , wedge 4 3 "" "SMGs" msg
        , wedge 4 4 "" "RIFLES" msg
        , lineAtAngle (pi / 4)
        , lineAtAngle (3 * pi / 4)
        , circle [ cx "50", cy "50", r "10", fill "#FFFFFF" ] []
        , circle [ cx "50", cy "50", r "7.5", fill "#0000DD" ] []
        ]


view : msg -> Int -> Html msg
view msg wheels =
    if wheels == 6 then
        (viewSix msg)
    else
        (viewFour msg)
