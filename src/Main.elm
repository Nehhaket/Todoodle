port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Json.Decode as D
import Json.Encode as E



-- MAIN


main : Program E.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Todo =
    { id : Int
    , done : Bool
    , content : String
    , tags : List String
    }


type alias Model =
    { todoInput : String
    , todoList : List Todo
    }


init : E.Value -> ( Model, Cmd msg )
init flags =
    ( case D.decodeValue decoder flags of
        Ok model ->
            model

        Err _ ->
            { todoList = [], todoInput = "" }
    , Cmd.none
    )



-- UPDATE


type Msg
    = AddTodo String
    | TodoInputChanged String
    | Toggle Int
    | Remove Int
    | AddTag Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTodo todoStr ->
            ( { model
                | todoInput = ""
                , todoList =
                    model.todoList
                        ++ [ { done = False
                             , content = todoStr
                             , id = List.length model.todoList
                             , tags = []
                             }
                           ]
              }
            , Cmd.none
            )

        TodoInputChanged todoStr ->
            ( { model | todoInput = todoStr }
            , Cmd.none
            )

        Toggle id ->
            ( { model | todoList = List.map (toggleTodo id) model.todoList }
            , Cmd.none
            )

        Remove id ->
            ( { model
                | todoList =
                    model.todoList
                        -- filter out the item that is to be deleted
                        |> List.filter (\e -> e.id /= id)
                        -- reapply id
                        |> List.indexedMap (\i -> \e -> { e | id = i })
              }
            , Cmd.none
            )

        AddTag id tagStr ->
            ( { model
                | todoList =
                    model.todoList
                        |> List.map
                            (\e ->
                                if e.id == id then
                                    { e | tags = e.tags ++ [ tagStr ] }

                                else
                                    e
                            )
              }
            , Cmd.none
            )


toggleTodo : Int -> Todo -> Todo
toggleTodo id todo =
    case todo.id == id of
        True ->
            { todo | done = not todo.done }

        False ->
            todo


addTag : Int -> String -> Todo -> Todo
addTag id tag todo =
    case todo.id == id of
        True ->
            { todo | tags = todo.tags ++ [ tag ] }

        False ->
            todo



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input
            [ type_ "text"
            , placeholder "Add new todo"
            , onInput TodoInputChanged
            , value model.todoInput
            ]
            []
        , button [ onClick (AddTodo model.todoInput) ] [ text "Add" ]
        , div [] []
        , ul [ style "list-style-type" "none" ] (List.map viewTodo model.todoList)
        ]


viewTodo : Todo -> Html Msg
viewTodo todo =
    li []
        [ button [ onClick (Remove todo.id) ] [ text "X" ]
        , p
            [ onClick (Toggle todo.id)
            , style "display" "inline"
            , style "margin-left" "8px"
            , todoDecoration todo.done
            ]
            [ text todo.content ]
        , p [ style "display" "inline" ]
            ([ text " | tags: " ] ++ List.map (\t -> text (t ++ " ")) todo.tags)
        ]


todoDecoration : Bool -> Html.Attribute Msg
todoDecoration done =
    case done of
        True ->
            style "text-decoration" "line-through"

        False ->
            style "" ""



-- PORTS


port setStorage : E.Value -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    ( newModel
    , Cmd.batch [ setStorage (encode newModel), cmds ]
    )



-- JSON


encode : Model -> E.Value
encode model =
    E.object
        [ ( "todo", E.string model.todoInput )
        , ( "todoList", E.list encodeTodo model.todoList )
        ]


encodeTodo : Todo -> E.Value
encodeTodo todo =
    E.object
        [ ( "id", E.int todo.id )
        , ( "done", E.bool todo.done )
        , ( "content", E.string todo.content )
        , ( "tags", E.list (\t -> E.string t) todo.tags )
        ]


decoder : D.Decoder Model
decoder =
    D.map2 Model
        (D.field "todo" D.string)
        (D.field "todoList" (D.list todoDecoder))


todoDecoder : D.Decoder Todo
todoDecoder =
    D.map4 Todo
        (D.field "id" D.int)
        (D.field "done" D.bool)
        (D.field "content" D.string)
        (D.field "tags" (D.list D.string))
