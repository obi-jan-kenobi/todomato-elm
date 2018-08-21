module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = initialModel, view = view, update = update }


type alias Model =
    { todos : List Todo
    , active : Maybe TodoState
    , todo : String
    }


initialModel : Model
initialModel =
    { todos = []
    , active = Nothing
    , todo = ""
    }


type Msg
    = Todo String
    | Add
    | Select Todo
    | Start
    | Stop


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "New Todo", onInput Todo, value model.todo ] []
        , button [ onClick Add ] [ text "Add Todo" ]
        , div [] <| List.map (todoItem) model.todos
        , div []
            [ text "active: "
            , text <|
                (\mt ->
                    case mt of
                        Just (Running t) ->
                            t.description

                        Just (Stopped t) ->
                            t.description

                        Nothing ->
                            "none"
                )
                    model.active
            ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Todo todo ->
            { model
                | todo = todo
            }

        Add ->
            { model
                | todos = { id = "1", duration = 0.0, cycles = 0, description = model.todo, completed = False } :: model.todos
                , todo = ""
            }

        Select todo ->
            { model
                | active = Just <| Stopped todo
            }

        Start ->
            case model.active of
                Just (Stopped todo) ->
                    { model | active = Just (Running todo) }

                Just (Running todo) ->
                    model

                Nothing ->
                    model

        Stop ->
            model


type alias Entity a =
    { a | id : String }


type alias Todo =
    Entity
        { description : String
        , duration : Time
        , cycles : Int
        , completed : Bool
        }


type TodoState
    = Running Todo
    | Stopped Todo


todoItem : Todo -> Html Msg
todoItem todo =
    div [ onClick <| Select todo ] [ text todo.description ]
