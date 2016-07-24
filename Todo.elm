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
    { field : String, todos : List Todo }


init : ( Model, Cmd Msg )
init =
    ( { field = "", todos = [ Todo "Hello" False, Todo "Dood" False ] }, Cmd.none )



-- MESSAGES


type Msg
    = NoOp
    | Add
    | UpdateField String
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
            , value model.field
            , onInput UpdateField
            , onEnter Add
            ]
            []
        , ul [ style [ ( "list-style", "none" ) ] ] (List.map renderTodo model.todos)
        , div [] [ remainingTodos model.todos ]
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


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        tagger code =
            if code == 13 then
                msg
            else
                NoOp
    in
        on "keydown" (Json.map tagger keyCode)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateField msg ->
            ( { model | field = msg }, Cmd.none )

        Add ->
            ( { model | todos = (Todo model.field False) :: model.todos, field = "" }, Cmd.none )

        Remove todo ->
            let
                newTodos =
                    List.filter (\item -> item /= todo) model.todos
            in
                ( { model | todos = newTodos }, Cmd.none )

        ToggleCompleted current ->
            let
                newTodos =
                    List.map (\todo -> toggle current todo) model.todos
            in
                ( { model | todos = newTodos }, Cmd.none )


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
