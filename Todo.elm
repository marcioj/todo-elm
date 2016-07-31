module App exposing (..)

import Html exposing (Html, Attribute, div, text, ul, li, h1, input, a, span)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Html.App


-- MODEL


type alias Todo =
    { name : String, completed : Bool }


type alias Model =
    List Todo


init : ( Model, Cmd Msg )
init =
    ( [ Todo "Sweep the floor" False, Todo "Pick up the trash" False ] , Cmd.none )



-- MESSAGES


type Msg
    = NoOp
    | Add String
    | Remove Todo
    | ToggleCompleted Todo



-- VIEW


todoStyle : Todo -> List ( String, String )
todoStyle todo =
    if todo.completed then
        [ ( "text-decoration", "line-through" ) ]
    else
        []


renderTodo : Todo -> Html Msg
renderTodo todo =
    li []
        [ input [ type' "checkbox", checked todo.completed, onClick (ToggleCompleted todo) ] []
        , span [ style (todoStyle todo) ] [ text todo.name ]
        , a
            [ style [ ( "margin-left", "20px" ) ]
            , href "#"
            , onClick (Remove todo)
            ]
            [ text "X" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Todos" ]
        , input
            [ placeholder "what needs to be done?"
            , autofocus True
            , onEnterInput Add
            ]
            []
        , ul [ style [ ( "list-style", "none" ) ] ] (List.map renderTodo model)
        , div [] [ remainingTodos model ]
        ]


remainingTodos : List Todo -> Html Msg
remainingTodos todos =
    let
        itemsLeft =
            List.filter (\todo -> not todo.completed) todos
                |> List.length
                |> toString
    in
        span []
            [ text (itemsLeft ++ " items left") ]

{-| A Json.Decoder that combines Html.Events.keyCode and Html.Events.targetValue in a tuple.
-}
keyCodeAndValue: Json.Decoder (Int, String)
keyCodeAndValue =
    Json.object2 (,) keyCode targetValue

{-| Triggers an event when the user presses down the ENTER key (keycode 13).

It grabs the **string** value at `event.target.value`, so it will not work if
need some other type of information.
Check Html.Events.keyCode and Html.Events.targetValue for more information.
-}
onEnterInput : (String -> Msg) -> Attribute Msg
onEnterInput msg =
    let
        handleKeyDown (code, value) =
            if code == 13 then
                msg value
            else
                NoOp
    in
        on "keydown" (Json.map handleKeyDown keyCodeAndValue )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Add newtodo ->
            ( (Todo newtodo False) :: model, Cmd.none )

        Remove todo ->
            ( List.filter (\item -> item /= todo) model, Cmd.none )

        ToggleCompleted current ->
            ( List.map (\todo -> toggle current todo) model, Cmd.none )


toggle : Todo -> Todo -> Todo
toggle current todo =
    if current == todo then
        { todo | completed = (not todo.completed) }
    else
        todo



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
