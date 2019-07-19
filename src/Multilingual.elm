module Multilingual exposing
    ( Multilingual, Code(..)
    , print
    , decode
    )

{-| Display text copy in multiple languages.


# Definition

@docs Multilingual, Code


# Output

@docs print


# Decode

@docs decode

-}

import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode exposing (Decoder, Error)


{-| Describe original text copy and available translations.

If translation is not found for given ISO `Code` output defaults to `copy`.

-}
type Multilingual
    = Multilingual { copy : String, translations : List ( Code, String ) }


{-| Language Codes according to ISO 639-1

This list is not complete.

-}
type Code
    = Bg -- Bulgarian
    | Cs -- Czech
    | Da -- Danish
    | De -- German
    | El -- Greek
    | En -- English
    | Es -- Spanish
    | Et -- Estonian
    | Fi -- Finish
    | Fr -- French
    | Ga -- Irish
    | He -- Hebrew
    | Hr -- Croatian
    | Hu -- Hungarian
    | Hy -- Armenian
    | It -- Italian
    | Ja -- Japanese
    | Ka -- Georgian
    | Ko -- Korean
    | Lt -- Lithuanian
    | Lv -- Latvian
    | Mt -- Maltese
    | Nl -- Dutch
    | No -- Norwegian
    | Pl -- Polish
    | Pt -- Portuguese
    | Ro -- Romanian
    | Ru -- Russian
    | Sk -- Slovak
    | Sl -- Slovenian
    | Sr -- Serbian
    | Sv -- Swedish
    | Tr -- Turkish
    | Uk -- Ukrainian
    | Zh -- Chinese
    | Zu -- Zulu


{-| Render Multilingual copy to given language code.

    Html.p []
        [ Multilingual.print Jp copy
            |> Html.map (always NoOp)
        ]

Above example renders Multilingual `copy` in Japanese.

-}
print : Code -> Multilingual -> Html Never
print lang (Multilingual { copy, translations }) =
    List.filter (Tuple.first >> (==) lang) translations
        |> List.head
        |> Maybe.map (Tuple.second >> Html.text)
        |> Maybe.withDefault (Html.span [ Attributes.class "multilingual-original-copy" ] [ Html.text copy ])


{-| Decode Json data to Multilingual type

    Decode.decodeString decode """
                {
                    "copy":"Prevedi me",
                    "translations": {
                        "en":"Translate me",
                        "fr":"Traduis-moi"
                    }
                }
            """

-}
decode : Decoder Multilingual
decode =
    Decode.map Multilingual <|
        Decode.map2 (\copy translations -> { copy = copy, translations = translations })
            (Decode.field "copy" Decode.string)
            (Decode.field "translations" decodeTranslations)


decodeTranslations : Decoder (List ( Code, String ))
decodeTranslations =
    Decode.keyValuePairs Decode.string
        |> Decode.andThen (List.foldr mapCode (Decode.succeed []))


mapCode : ( String, String ) -> Decoder (List ( Code, String )) -> Decoder (List ( Code, String ))
mapCode ( key, val ) list =
    case key of
        "bg" ->
            Decode.map ((::) ( Bg, val )) list

        "cs" ->
            Decode.map ((::) ( Cs, val )) list

        "da" ->
            Decode.map ((::) ( Da, val )) list

        "de" ->
            Decode.map ((::) ( De, val )) list

        "el" ->
            Decode.map ((::) ( El, val )) list

        "en" ->
            Decode.map ((::) ( En, val )) list

        "es" ->
            Decode.map ((::) ( Es, val )) list

        "et" ->
            Decode.map ((::) ( Et, val )) list

        "fi" ->
            Decode.map ((::) ( Fi, val )) list

        "fr" ->
            Decode.map ((::) ( Fr, val )) list

        "ga" ->
            Decode.map ((::) ( Ga, val )) list

        "he" ->
            Decode.map ((::) ( He, val )) list

        "hr" ->
            Decode.map ((::) ( Hr, val )) list

        "hu" ->
            Decode.map ((::) ( Hu, val )) list

        "hy" ->
            Decode.map ((::) ( Hy, val )) list

        "it" ->
            Decode.map ((::) ( It, val )) list

        "ja" ->
            Decode.map ((::) ( Ja, val )) list

        "ka" ->
            Decode.map ((::) ( Ka, val )) list

        "ko" ->
            Decode.map ((::) ( Ko, val )) list

        "lt" ->
            Decode.map ((::) ( Lt, val )) list

        "lv" ->
            Decode.map ((::) ( Lv, val )) list

        "mt" ->
            Decode.map ((::) ( Mt, val )) list

        "nl" ->
            Decode.map ((::) ( Nl, val )) list

        "no" ->
            Decode.map ((::) ( No, val )) list

        "pl" ->
            Decode.map ((::) ( Pl, val )) list

        "pt" ->
            Decode.map ((::) ( Pt, val )) list

        "ro" ->
            Decode.map ((::) ( Ro, val )) list

        "ru" ->
            Decode.map ((::) ( Ru, val )) list

        "sk" ->
            Decode.map ((::) ( Sk, val )) list

        "sl" ->
            Decode.map ((::) ( Sl, val )) list

        "sr" ->
            Decode.map ((::) ( Sr, val )) list

        "sv" ->
            Decode.map ((::) ( Sv, val )) list

        "tr" ->
            Decode.map ((::) ( Tr, val )) list

        "uk" ->
            Decode.map ((::) ( Uk, val )) list

        "zh" ->
            Decode.map ((::) ( Zh, val )) list

        "zu" ->
            Decode.map ((::) ( Zu, val )) list

        _ ->
            Decode.fail ("Alien language: " ++ key)
