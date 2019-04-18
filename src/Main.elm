module Main exposing (main, view)

import Browser
import Browser.Dom exposing (focus)
import Canvas exposing (..)
import Color
import Html exposing (Attribute, Html, div, h1, img, input, text)
import Html.Attributes exposing (autofocus, hidden, id, src, style, tabindex)
import Html.Events exposing (keyCode, on)
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
    , tail : List Direction
    , facing : Direction
    , food : Position
    }


type FoodStatus
    = Eaten
    | NotEaten


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


init : () -> ( Model, Cmd Msg )
init _ =
    ( Playing
        { head = ( 0, 1 )
        , tail = [ Left, Left, Left ]
        , facing = Right
        , food = ( 5, 5 )
        }
    , Task.attempt (always Noop) (focus "123")
    )



---- UPDATE ----


type Msg
    = Noop
    | Move Direction
    | PlaceFood Position
    | GenerateFood
    | PlayAgain
    | EndGame


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
                        newHeadPos =
                            move direction head
                    in
                    case newHeadPos of
                        Nothing ->
                            ( GameOver, Cmd.none )

                        Just newHead ->
                            let
                                isEating =
                                    newHead == food
                            in
                            Playing
                                { gameModel
                                    | head = newHead
                                    , tail =
                                        reverse direction
                                            :: (if isEating then
                                                    tail

                                                else
                                                    List.take (List.length tail - 1) tail
                                               )
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
                    -- Random.int sometimes produces an int outside of the given rabge hence the modBy safeguard
                    ( Playing
                        { gameModel
                            | food = mapBoth (modBy worldSize) position
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


mapBoth : (a -> b) -> ( a, a ) -> ( b, b )
mapBoth f ( x, y ) =
    ( f x, f y )


move : Direction -> Position -> Maybe Position
move direction ( x, y ) =
    let
        ( newX, newY ) =
            case direction of
                Up ->
                    ( x, y - 1 )

                Down ->
                    ( x, y + 1 )

                Right ->
                    ( x + 1, y )

                Left ->
                    ( x - 1, y )
    in
    if List.all (inRange 0 worldSize) [ newX, newY ] then
        Just ( newX, newY )

    else
        Nothing


inRange : Int -> Int -> Int -> Bool
inRange min max n =
    min <= n && n <= max



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


maybeLook currentlyFacing key =
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
            div [ id "123", tabindex 0, onKeyDown (maybeLook facing), style "outline" "none", style "height" "100vh" ]
                [ Canvas.toHtml ( canvasLength, canvasLength )
                    [ style "border" "1px solid black" ]
                    (renderBackground
                        :: renderHead head
                        :: renderFood food
                        :: renderBodyparts head tail
                    )
                ]

        GameOver ->
            text "Game Over"


renderBodyparts : Position -> List Direction -> List Renderable
renderBodyparts previousPosition tail =
    case tail of
        direction :: directions ->
            let
                position =
                    move direction previousPosition
            in
            case position of
                Just pos ->
                    renderSquare Color.blue snakeBodypartLength pos :: renderBodyparts pos directions

                Nothing ->
                    []

        -- never happens
        [] ->
            []


renderSquare color length position =
    shapes
        [ fill color ]
        [ rect (positionToCoordinates position) length length ]


renderBackground =
    renderSquare Color.gray canvasLength ( 0, 0 )


renderHead =
    renderSquare Color.darkCharcoal snakeBodypartLength


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
