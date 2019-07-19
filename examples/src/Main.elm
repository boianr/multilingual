module Main exposing (Model, Msg(..), main, update, view)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder, Error)
import Multilingual exposing (Code(..), Multilingual, decode, print)



-- MODEL


type alias Model =
    { lang : Code
    , multilingual : Result Error Multilingual
    }



-- UPDATE


type Msg
    = Noop
    | Change Code


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model

        Change lang ->
            { model | lang = lang }



-- view


view : Model -> Html.Html Msg
view model =
    case model.multilingual of
        Ok multilingual ->
            Html.div []
                [ Html.select
                    [ Events.on "change"
                        (Decode.at [ "target", "value" ] Decode.string
                            |> Decode.andThen
                                (\str ->
                                    case str of
                                        "En" ->
                                            Decode.succeed (Change En)

                                        "Fr" ->
                                            Decode.succeed (Change Fr)

                                        "Sr" ->
                                            Decode.succeed (Change Sr)

                                        _ ->
                                            Decode.fail "Alien language"
                                )
                        )
                    ]
                    [ Html.option [ Attributes.selected (model.lang == En) ] [ Html.text "En" ]
                    , Html.option [ Attributes.selected (model.lang == Fr) ] [ Html.text "Fr" ]
                    , Html.option [ Attributes.selected (model.lang == Sr) ] [ Html.text "Sr" ]
                    ]
                , Html.h1 [] [ print model.lang multilingual |> Html.map (always Noop) ]
                ]

        Err reason ->
            Html.pre [] [ Decode.errorToString reason |> Html.text ]


main : Program () Model Msg
main =
    Browser.sandbox
        { view = view
        , update = update
        , init =
            { lang = Fr
            , multilingual =
                --Decode.decodeString decode """
                --    {
                --        "copy":"Prevedi me",
                --        "translations": {
                --            "klingon":"alien"
                --        }
                --    }
                --"""
                Decode.decodeString decode """
                {
                    "copy":"Prevedi me",
                    "translations": {
                        "en":"Translate me",
                        "fr":"Traduis-moi"
                    }
                }
            """
            }
        }
