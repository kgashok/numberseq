module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Dom as Dom
import Html exposing (Html, a, button, div, h1, h2, hr, img, li, pre, span, text, ul)
import Html.Attributes exposing (alt, classList, href, id, rel, src, target, disabled)
import Html.Events exposing (onClick)
import Maybe exposing (map, withDefault)
import String exposing (concat, fromChar, fromInt, toInt, toList)
import Task


type alias Model =
    { rangeMax : Int -- to display an initial set of numbers in the sequence
    , limit : Int -- font colour beyond the six numbers in the sequence
    , inter : Bool -- intermediary values to display as well?
    , spoilerMode : Bool 
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { rangeMax = 21
      , limit = 18
      , inter = False
      , spoilerMode = True
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
    | ToggleInter
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | rangeMax = model.rangeMax + 3
                      , spoilerMode = 
                        if model.spoilerMode && model.rangeMax > 60 
                        then False 
                        else model.spoilerMode
              }
            , Cmd.none 
            )

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

        ToggleInter ->
            if model.rangeMax == 0 then
                ( { model | rangeMax = 21, inter = not model.inter }, focusSearchBox )

            else
                ( { model | rangeMax = 21, inter = not model.inter }, focusSearchBox )

        NoOp ->
            ( model, Cmd.none )


focusSearchBox : Cmd Msg
focusSearchBox =
    Task.attempt (\_ -> NoOp) (Dom.focus "increment")


view : Model -> Html Msg
view model =
    -- div [] [ text (squareList 20)]
    let
        -- numberList =
        --    squareList model.rangeMax
        {--}
        numberList =
            List.range 1 model.rangeMax
                |> List.map squareDigit
                |> List.indexedMap Tuple.pair

        --}
    in
    div []
        [ div [] [ h1 [] [ text "What's next and Why?" ] ]
        , footer
        , hr [] []
        , button [ onClick Decrement ] [ text "Press to decrease" ]
        , button [ id "increment", onClick Increment ] [ text "Press to increase" ]
        , h2 [] [ text "The sequence" ]
        , div [ classList [ ( "numbers", True ) ] ] [ renderNumbers numberList model.limit model.inter ]
        , button [ onClick ToggleInter, disabled model.spoilerMode ] [ text "SPOILER ALERT!" ]
        , h2 [] [ text "The difference" ]
        , div [ classList [ ( "numbers", True ) ] ] [ renderDiff (calculateDiff numberList) model.inter ]
        ]


renderNumbers : List ( Int, Int ) -> Int -> Bool -> Html msg
renderNumbers lst limit inter =
    let
        numattr index_ limit_ =
            ( "numberRed", index_ > limit_ )

        interattr index_ =
            ( "interNumber", modBy 3 index_ /= 0 )
    in
    lst
        |> List.map (\t -> ( t, String.fromInt (value t) ++ ", " ))
        |> List.map
            (\( t, intAsText ) ->
                span
                    [ classList
                        [ numattr (index t) limit
                        , interattr (index t)
                        , ( "hideNumber", not inter && modBy 3 (index t) /= 0 )
                        ]
                    ]
                    [ text intAsText ]
            )
        |> ul []


renderDiff : List ( Int, Int ) -> Bool -> Html msg
renderDiff lst inter =
    let
        lastIndex =
            if inter then
                List.length lst - 3

            else
                List.length lst - 1

        displayattr index_ =
            ( "numberRed", index_ == lastIndex )
    in
    lst
        |> List.map
            (\t ->
                span
                    [ classList
                        [ displayattr (index t)
                        , ( "hideNumber", not inter && modBy 3 (index t) /= 0 )
                        ]
                    ]
                    [ text (String.fromInt (value t) ++ ", ") ]
            )
        |> ul []


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


strong : Int -> String
strong n =
    let
        digitList number =
            number
                |> String.fromInt
                |> String.toList
                |> List.filterMap (String.fromChar >> String.toInt)

        factorial f =
            if f <= 1 then
                1

            else
                f * factorial (f - 1)

        factsum lst =
            List.map factorial lst |> List.sum
    in
    if n == factsum (digitList n) then
        "STRONG!!!!"

    else
        "Not Strong !!"
