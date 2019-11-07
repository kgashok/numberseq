module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Dom as Dom
import Html exposing (Html, a, button, div, h1, h2, hr, img, li, pre, span, text, ul)
import Html.Attributes exposing (alt, classList, href, id, rel, src, target)
import Html.Events exposing (onClick)
import Maybe exposing (map, withDefault)
import String exposing (concat, fromChar, fromInt, toInt, toList)
import Task


type alias Model =
    { rangeMax : Int -- to display an initial set of numbers in the sequence
    , limit : Int -- font colour beyond the six numbers in the sequence
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { rangeMax = 21
      , limit = 6
      }
    , focusSearchBox
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


calculateDiff : List ( Int, Int ) -> List ( Int, Int )
calculateDiff tlst =
    let
        ( ilst, lst ) =
            List.unzip tlst

        ziplist =
            List.map2 Tuple.pair lst (withDefault [] (List.tail lst))
    in
    List.map (\( x, y ) -> y - x) ziplist
        |> List.indexedMap Tuple.pair


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = Increment
    | Decrement
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | rangeMax = model.rangeMax + 3 }, Cmd.none )

        Decrement ->
            ( { model
                | rangeMax =
                    if model.rangeMax >= 3 then
                        model.rangeMax - 3

                    else
                        model.rangeMax
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


focusSearchBox : Cmd Msg
focusSearchBox =
    Task.attempt (\_ -> NoOp) (Dom.focus "increment")


view : Model -> Html Msg
view model =
    let
        numberList =
            squareList model.rangeMax
    in
    div []
        [ div [] [ h1 [] [ text "What's next and Why?" ] ]
        , footer
        , hr [] []
        , button [ onClick Decrement ] [ text "Press to decrease" ]

        -- , div [] [ text (String.fromInt model.rangeMax ++ " " ++ String.fromInt model.limit) ]
        , button [ id "increment", onClick Increment ] [ text "Press to increase" ]

        -- div [] [ text (squareList 20)]
        , h2 [] [ text "The sequence" ]
        , div [ classList [ ( "numbers", True ) ] ] [ renderNumbers numberList model.limit ]
        , h2 [] [ text "The difference" ]
        , div [ classList [ ( "numbers", True ) ] ] [ renderDiff (calculateDiff numberList) ]
        ]


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


index t =
    Tuple.first t


value t =
    Tuple.second t


renderNumbers : List ( Int, Int ) -> Int -> Html msg
renderNumbers lst limit =
    let
        displayattr index_ limit_ =
            classList [ ( "numberRed", index_ > limit_ ) ]
    in
    lst
        |> List.map (\t -> span [ displayattr (index t) limit ] [ text (String.fromInt (value t) ++ ", ") ])
        |> ul []


renderDiff : List ( Int, Int ) -> Html msg
renderDiff lst =
    let
        lastIndex =
            List.length lst - 1

        displayattr index_ =
            classList [ ( "numberRed", index_ == lastIndex ) ]
    in
    lst
        |> List.map (\t -> span [ displayattr (index t) ] [ text (String.fromInt (value t) ++ ", ") ])
        |> ul []



-- Legacy Code


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
