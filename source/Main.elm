module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Dom as Dom
import Html exposing (Html, a, button, div, h1, h2, hr, img, li, pre, span, text, ul)
import Html.Attributes exposing (alt, classList, disabled, href, id, rel, src, target)
import Html.Events exposing (onClick)
import List.Extra exposing (groupsOf)
import Maybe exposing (map, withDefault)
import String exposing (concat, fromChar, fromInt, toInt, toList)
import Task


type alias Model =
    { rangeMax : Int -- to display an initial set of numbers in the sequence
    , limit : Int -- font colour beyond the six numbers in the sequence
    , inter : Bool -- intermediary values to display as well?
    , spoilerMode : Bool
    , spoilerVal : Int -- the spoiler gets enabled when rangeMax > spoilerVal
    , modeText : String -- button text for the spoiler
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { rangeMax = 21
      , limit = 18
      , inter = False
      , spoilerMode = True
      , spoilerVal = 60
      , modeText = "SPOILER ALERT!"
      }
    , focusIncrementButton
    )


toSquareString : Maybe Int -> Maybe String
toSquareString =
    map (\n -> n ^ 2) >> map fromInt


squareDigit : Int -> Int
squareDigit =
    fromInt
        -- make list of digital characters
        >> toList
        -- map each digit_char -> integer -> square string equivalent
        >> List.filterMap (fromChar >> toInt >> toSquareString)
        -- join all square strings
        >> concat
        -- convert back to number
        >> toInt
        >> withDefault 0


mapAdjacent : (a -> a -> b) -> List a -> List b
mapAdjacent f list =
    List.map2 f list (withDefault [] (List.tail list))


squareList : Int -> List ( Int, Int )
squareList rangeMax =
    let
        rangeList =
            List.range 1 rangeMax
                |> List.filter (\x -> modBy 3 (x - 1) == 0)
    in
    rangeList
        |> List.map squareDigit
        |> List.indexedMap Tuple.pair


calculateDiff : List Int -> List Int
calculateDiff =
    mapAdjacent (\x y -> y - x)


calculateDiff3 : List Int -> List Int
calculateDiff3 =
    calculateDiff >> groupsOf 3 >> List.map List.sum


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = IncrementRange
    | DecrementRange
    | ToggleShowInterim
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncrementRange ->
            ( { model
                | rangeMax = model.rangeMax + 3
                , spoilerMode =
                    if model.spoilerMode && model.rangeMax > model.spoilerVal then
                        False

                    else
                        model.spoilerMode
              }
            , Cmd.none
            )

        DecrementRange ->
            ( { model
                | rangeMax =
                    if model.rangeMax >= 3 then
                        model.rangeMax - 3

                    else
                        model.rangeMax
              }
            , Cmd.none
            )

        ToggleShowInterim ->
            let
                mtext =
                    if model.modeText == "hide hints" then
                        "show hints"

                    else
                        "hide hints"
            in
            ( { model
                | inter = not model.inter
                , modeText = mtext
              }
            , focusIncrementButton
            )

        NoOp ->
            ( model, Cmd.none )


focusIncrementButton : Cmd Msg
focusIncrementButton =
    Task.attempt (\_ -> NoOp) (Dom.focus "increment")


view : Model -> Html Msg
view model =
    -- simplest version
    -- div [] [ text (squareList 20 |> Debug.toString) ]
    {--}
    let
        numberList =
            List.range 1 model.rangeMax
                |> List.map squareDigit

        -- |> List.reverse
        differenceList =
            if model.inter then
                calculateDiff numberList

            else
                calculateDiff3 numberList
    in
    div []
        [ div [] [ h1 [] [ text "What's next and Why?" ] ]
        , footer
        , hr [] []
        , button [ onClick DecrementRange ] [ text "Decrease" ]

        -- , pre [] [ text <| String.fromInt model.rangeMax ]
        , button [ id "increment", onClick IncrementRange ] [ text "Increase" ]
        , h2 [] [ text "The sequence" ]
        , div [ classList [ ( "numbers", True ) ] ]
            [ renderNumbers numberList model.limit model.inter ]

        -- , hr [] []
        , button [ onClick ToggleShowInterim, disabled model.spoilerMode ]
            [ text model.modeText ]
        , h2 [] [ text "The difference" ]
        , div [ classList [ ( "numbers", True ) ] ]
            [ renderDiff differenceList model.inter ]
        ]
--}


renderNumbers : List Int -> Int -> Bool -> Html msg
renderNumbers lst limit inter =
    lst
        |> List.indexedMap
            (\index value ->
                span
                    [ classList
                        [ ( "highlightNum", index > limit )
                        , ( "interNumber", modBy 3 index /= 0 )
                        , ( "hideNumber", not inter && modBy 3 index /= 0 )
                        ]
                    ]
                    [ text (String.fromInt value ++ ", ") ]
            )
        |> ul []


renderDiff : List Int -> Bool -> Html msg
renderDiff lst inter =
    let
        offset =
            if inter then
                3

            else
                1

        highIndex =
            List.length lst - offset

        items =
            lst
                |> List.indexedMap
                    (\i v ->
                        span [ classList [ ( "highlightNum", i == highIndex ) ] ]
                            [ text (String.fromInt v ++ ", ") ]
                    )
    in
    ul [] items


gitRepo : String
gitRepo =
    "https://github.com/kgashok/numberseq"


footer : Html Msg
footer =
    div [ id "footer" ]
        [ a
            [ href (gitRepo ++ "/issues/new")
            , target "_blank"
            , rel "noopener noreferrer"
            ]
            [ text "Provide feedback?;" ]
        , a
            [ href (gitRepo ++ "/commits/glitch")
            , target "_blank"
            , rel "noopener noreferrer"
            ]
            [ text " last checkin" ]
        ]



-- Legacy Code
{--
calculateDiff0 : List ( Int, Int ) -> Bool -> List ( Int, Int )
calculateDiff0 tlst inter =
    let
        ( _, lst ) =
            List.unzip tlst

        ziplist =
            List.map2 Tuple.pair lst (withDefault [] (List.tail lst))
                |> List.map (\( x, y ) -> y - x)

        lst3 =
            ziplist |> List.Extra.groupsOf 3 |> List.map List.sum
    in
    (if inter then
        ziplist

     else
        lst3
    )
        |> List.indexedMap Tuple.pair



renderList : List Int -> Html msg
renderList lst =
    let
        updateClr plst =
            plst
    in
    lst
        |> List.map (\i -> String.fromInt i)
        |> String.join ", "
        |> text
        |> List.singleton
        -- |> List.map(pre [] << List.singleton)
        -- |> List.map (text)
        -- |> List.map (pre [] << List.singleton << text)
        -- |> List.map (pre [] << text)
        |> ul []


diffList : List Int -> List Int
diffList lst =
    let
        sub pair =
            List.foldl (-) 0 pair |> List.singleton

        diffTwo alst =
            case List.length alst of
                1 ->
                    [ 0 ]

                0 ->
                    [ 0 ]

                _ ->
                    List.take 2 alst |> sub
    in
    diffTwo lst


diffList2 : List ( Int, Int ) -> List Int
diffList2 tlst =
    let
        ( ilst, lst ) =
            List.unzip tlst

        ziplist =
            List.map2 Tuple.pair lst (withDefault [] (List.tail lst))
    in
    List.map (\( x, y ) -> y - x) ziplist

--}
