module Main exposing (main, view)

import Browser
import Browser.Dom exposing (focus)
import Canvas exposing (..)
import Color
import Html exposing (Attribute, Html, div, h1, img, input, text)
import Html.Attributes exposing (autofocus, hidden, id, src, style, tabindex)
import Html.Events exposing (keyCode, on)
import Json.Decode as Json
import Task
import Time



---- MODEL ----


type alias Model =
    { headPosition : Position
    , headDirection : Direction
    }


type Direction
    = Up
    | Down
    | Left
    | Right


opposite : Direction -> Direction
opposite direction =
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
    ( { headPosition = ( 0, 1 )
      , headDirection = Right
      }
    , Task.attempt (always Noop) (focus "123")
    )



---- UPDATE ----


type Msg
    = Noop
    | Look Direction
    | Move
    | KeyDown Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Look newDirection ->
            ( { model
                | headDirection =
                    let
                        currentDirection =
                            model.headDirection
                    in
                    if currentDirection == opposite newDirection then
                        currentDirection

                    else
                        newDirection
              }
            , Cmd.none
            )

        Move ->
            ( { model | headPosition = move model.headDirection model.headPosition }
            , Cmd.none
            )

        KeyDown int ->
            case int of
                37 ->
                    model |> update (Look Left)

                38 ->
                    model |> update (Look Up)

                39 ->
                    model |> update (Look Right)

                40 ->
                    model |> update (Look Down)

                _ ->
                    ( model, Cmd.none )


mapBoth : (a -> b) -> ( a, a ) -> ( b, b )
mapBoth f ( x, y ) =
    ( f x, f y )


move : Direction -> Position -> Position
move direction ( x, y ) =
    mapBoth (modBy worldSize)
        (case direction of
            Up ->
                ( x, y - 1 )

            Down ->
                ( x, y + 1 )

            Right ->
                ( x + 1, y )

            Left ->
                ( x - 1, y )
        )



-- VIEW ----


canvasLength =
    400


worldSize =
    30


snakeSize =
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


maybeLook key =
    if isArrow key then
        Look (decodeArrow key)

    else
        Noop


view : Model -> Html Msg
view model =
    div [ id "123", tabindex 0, onKeyDown maybeLook, style "outline" "none", style "height" "100vh" ]
        [ Canvas.toHtml ( canvasLength, canvasLength )
            [ style "border" "1px solid black" ]
            [ renderBackground
            , renderSnakeHead model.headPosition
            ]

        -- , input
        --     []
        --     []
        ]


renderBackground =
    shapes [ fill Color.gray ]
        [ rect ( 0, 0 ) canvasLength canvasLength ]


renderSnakeHead position =
    shapes [ fill Color.darkCharcoal ]
        [ rect (positionToCoordinates position) snakeSize snakeSize ]



---- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 300 (\time -> Move)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
