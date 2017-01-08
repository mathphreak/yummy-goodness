module Main exposing (..)

import Html exposing (Html)
import Html.Events
import List
import Array exposing (Array)


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { cashLeft : Int
    , equipment : List Equipment
    }


type Equipment
    = Armor
    | Helmet
    | AK47


allAvailableEquipment : Array Equipment
allAvailableEquipment =
    Array.fromList
        [ Armor, Helmet, AK47 ]


init : ( Model, Cmd Msg )
init =
    ( Model 0 [ Armor ], Cmd.none )



-- UPDATE


type Msg
    = Purchase Equipment


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Purchase item ->
            ( { model | equipment = model.equipment ++ [ item ] }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        enlist a =
            [ a ]
    in
        Html.div []
            [ Html.text (toString model.cashLeft)
            , Html.ul []
                (List.map
                    (Html.li [] << enlist << Html.text << toString)
                    model.equipment
                )
            , Html.ul []
                (Array.toList
                    (Array.map
                        (\e ->
                            Html.li []
                                [ Html.button [ Html.Events.onClick (Purchase e) ] [ Html.text ("Purchase " ++ toString e) ]
                                ]
                        )
                        allAvailableEquipment
                    )
                )
            ]
