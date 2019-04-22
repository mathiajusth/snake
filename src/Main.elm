module Main exposing (main, view)

import Browser
import Browser.Dom exposing (focus)
import Canvas exposing (..)
import Color
import Html exposing (Attribute, Html, button, div, h1, img, input, text)
import Html.Attributes exposing (autofocus, hidden, id, src, style, tabindex)
import Html.Events exposing (keyCode, on, onClick)
import Json.Decode as Json
import Random
import Task
import Time



---- MODEL ----


type Model
    = Playing GameModel
    | GameOver


type alias GameModel =
    { head : Position
    , tail : List Position
    , facing : Direction
    , food : Position
    }


type Direction
    = Up
    | Down
    | Left
    | Right


reverse : Direction -> Direction
reverse direction =
    case direction of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


type alias Position =
    ( Int, Int )


type alias Coordinates =
    ( Float, Float )


buildTail : Position -> List Direction -> List Position
buildTail head dirs =
    case dirs of
        [] ->
            []

        d :: ds ->
            let
                pos =
                    move d head
            in
            pos :: buildTail pos ds


headInitPosition =
    ( worldSize // 2, worldSize // 2 )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Playing
        { head = headInitPosition
        , tail = buildTail headInitPosition [ Left, Left, Left ]
        , facing = Right
        , food = ( 5, 5 )
        }
    , Task.attempt (always Noop) (focus "canvasWrap")
    )



---- UPDATE ----


type Msg
    = Noop
    | Move Direction
    | PlaceFood Position
    | GenerateFood
    | PlayAgain
    | EndGame


isHeadOutsideWorld : Position -> Bool
isHeadOutsideWorld ( x, y ) =
    not (List.all (inRange 0 worldSize) [ x, y ])


didCrashInto : Position -> List Position -> Bool
didCrashInto head obstacles =
    List.member head obstacles


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Playing gameModel ->
            let
                { head, tail, facing, food } =
                    gameModel
            in
            case msg of
                Move direction ->
                    let
                        newHead =
                            move direction head

                        isEating =
                            newHead == food

                        newTail =
                            head
                                :: (if isEating then
                                        tail

                                    else
                                        dropLast tail
                                   )

                        isGameOver =
                            isHeadOutsideWorld newHead || didCrashInto head tail
                    in
                    if isGameOver then
                        ( GameOver, Cmd.none )

                    else
                        Playing
                            { gameModel
                                | head = newHead
                                , tail = newTail
                                , facing = direction
                            }
                            |> update
                                (if isEating then
                                    GenerateFood

                                 else
                                    Noop
                                )

                GenerateFood ->
                    ( model, Random.generate PlaceFood randomPosition )

                PlaceFood position ->
                    let
                        foodPos =
                            -- Random.int sometimes produces an int outside of the given rabge hence the modBy safeguard
                            mapBoth (modBy worldSize) position
                    in
                    if didCrashInto foodPos (head :: tail) then
                        model |> update GenerateFood

                    else
                        ( Playing
                            { gameModel
                                | food = foodPos
                            }
                        , Cmd.none
                        )

                EndGame ->
                    ( GameOver, Cmd.none )

                _ ->
                    ( Playing gameModel, Cmd.none )

        GameOver ->
            case msg of
                PlayAgain ->
                    init ()

                _ ->
                    ( model, Cmd.none )


randomPosition : Random.Generator ( Int, Int )
randomPosition =
    Random.pair (Random.int 0 worldSize) (Random.int 0 worldSize)


move : Direction -> Position -> Position
move direction ( x, y ) =
    case direction of
        Up ->
            ( x, y - 1 )

        Down ->
            ( x, y + 1 )

        Right ->
            ( x + 1, y )

        Left ->
            ( x - 1, y )



-- VIEW ----


canvasLength =
    400


worldSize =
    40


snakeBodypartLength =
    canvasLength / worldSize


positionToCoordinates : Position -> Coordinates
positionToCoordinates position =
    mapBoth (\x -> toFloat x / worldSize * canvasLength) position


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


decodeArrow : Int -> Direction
decodeArrow int =
    case int of
        37 ->
            Left

        38 ->
            Up

        39 ->
            Right

        40 ->
            Down

        _ ->
            Right


isArrow : Int -> Bool
isArrow key =
    case key of
        37 ->
            True

        38 ->
            True

        39 ->
            True

        40 ->
            True

        _ ->
            False


maybeMove currentlyFacing key =
    if isArrow key then
        let
            arrow =
                decodeArrow key
        in
        if arrow == reverse currentlyFacing then
            Noop

        else
            Move arrow

    else
        Noop


view : Model -> Html Msg
view model =
    case model of
        Playing gameModel ->
            let
                { head, tail, facing, food } =
                    gameModel
            in
            div [ id "canvasWrap", tabindex 0, onKeyDown (maybeMove facing), style "outline" "none", style "height" "100vh" ]
                [ Canvas.toHtml ( canvasLength, canvasLength )
                    [ style "border" "1px solid black" ]
                    (renderBackground
                        :: renderHead head
                        :: renderFood food
                        :: renderTail tail
                    )
                ]

        GameOver ->
            div []
                [ text "Game Over"
                , button
                    [ onClick PlayAgain ]
                    [ text "Try Again!" ]
                ]


renderSquare color length position =
    shapes
        [ fill color ]
        [ rect (positionToCoordinates position) length length ]


renderBackground =
    renderSquare Color.gray canvasLength ( 0, 0 )


renderHead =
    renderSquare Color.darkCharcoal snakeBodypartLength


renderTail =
    List.map (renderSquare Color.blue snakeBodypartLength)


renderFood food =
    renderSquare Color.red snakeBodypartLength food



---- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Playing gameModel ->
            Time.every 300 (\time -> Move gameModel.facing)

        GameOver ->
            Sub.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



---- UTILS ----


dropLast : List a -> List a
dropLast xs =
    List.take (List.length xs - 1) xs


mapBoth : (a -> b) -> ( a, a ) -> ( b, b )
mapBoth f ( x, y ) =
    ( f x, f y )


inRange : Int -> Int -> Int -> Bool
inRange min max n =
    min <= n && n <= max
