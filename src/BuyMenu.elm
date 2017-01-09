module BuyMenu exposing (view)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Events


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


viewSix : msg -> Html msg
viewSix msg =
    svg [ viewBox "0 0 100 100", width "150px" ]
        [ Svg.path [ d "M 50 50 L 6.7382812 25 A 50 50 0 0 0 0 50 A 50 50 0 0 0 6.7382812 75 L 50 50 z ", fill "#0A0A0A" ] []
        , Svg.path [ d "M 50 50 L 6.7382812 75 A 50 50 0 0 0 50 100 L 50 50 z ", fill "#0A0A0A" ] []
        , Svg.path [ d "M 50 50 L 50 100 A 50 50 0 0 0 93.261719 75 L 50 50 z ", fill "#0A0A0A" ] []
        , Svg.path [ d "M 93.261719 25 L 50 50 L 93.261719 75 A 50 50 0 0 0 100 50 A 50 50 0 0 0 93.261719 25 z ", fill "#0A0A0A" ] []
        , Svg.path [ d "M 50 0 A 50 50 0 0 0 6.7382812 25 L 50 50 L 50 0 z ", fill "#0A0A0A" ] []
        , Svg.path [ d "M 50 0 L 50 50 L 93.261719 25 A 50 50 0 0 0 50 0 z ", fill "#0A0A0A" ] []
        , circle [ cx "50", cy "50", r "10", fill "#FFFFFF" ] []
        , circle [ cx "50", cy "50", r "7.5", fill "#0000DD" ] []
        ]


viewFour : msg -> Html msg
viewFour msg =
    svg [ viewBox "0 0 100 100", width "100px" ] []


view : msg -> Int -> Html msg
view msg wheels =
    if wheels == 6 then
        (viewSix msg)
    else
        (viewFour msg)
