port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Json.Decode as D
import Json.Encode as E
import Regex



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
    , input : String
    , expanded : Bool
    }


type alias Model =
    { input : String
    , todoList : List Todo
    }


init : E.Value -> ( Model, Cmd msg )
init flags =
    ( case D.decodeValue decoder flags of
        Ok model ->
            model

        Err _ ->
            { todoList = [], input = "" }
    , Cmd.none
    )



-- UPDATE


type Msg
    = TodoInputChanged String
    | AddTodo String
    | Toggle Int
    | Remove Int
    | AddTag Int String
    | ToggleTagInput Int
    | RemoveTag Int Int
    | TagInputChanged Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TodoInputChanged todoStr ->
            ( { model | input = todoStr }
            , Cmd.none
            )

        Toggle id ->
            ( { model | todoList = List.map (toggleTodo id) model.todoList }
            , Cmd.none
            )

        TagInputChanged id str ->
            ( { model | todoList = List.map (changeTagInput id str) model.todoList }
            , Cmd.none
            )

        ToggleTagInput id ->
            ( { model | todoList = List.map (toggleTagInput id) model.todoList }
            , Cmd.none
            )

        AddTodo content ->
            ( { model
                | input = ""
                , todoList = appendTodo content model.todoList
              }
            , Cmd.none
            )

        AddTag id tag ->
            case String.isEmpty tag of
                False ->
                    update (ToggleTagInput id)
                        { model | todoList = List.map (addTag id tag) model.todoList }

                True ->
                    update (ToggleTagInput id) model

        Remove id ->
            ( { model
                | todoList =
                    model.todoList
                        -- filter out the item that is to be deleted
                        |> List.filter (\e -> e.id /= id)
                        -- reapply ids
                        |> List.indexedMap (\i -> \e -> { e | id = i })
              }
            , Cmd.none
            )

        RemoveTag id index ->
            ( { model | todoList = List.map (removeTag id index) model.todoList }
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
            { todo | tags = todo.tags ++ [ tag |> String.trim |> removeWhitespace ] }

        False ->
            todo


appendTodo : String -> List Todo -> List Todo
appendTodo content todoList =
    todoList ++ [ newTodo (List.length todoList) content ]


newTodo : Int -> String -> Todo
newTodo id content =
    { done = False
    , content = content
    , id = id
    , tags = []
    , input = ""
    , expanded = False
    }


changeTagInput : Int -> String -> Todo -> Todo
changeTagInput id str todo =
    case todo.id == id of
        True ->
            { todo | input = str }

        False ->
            todo


toggleTagInput : Int -> Todo -> Todo
toggleTagInput id todo =
    case todo.id == id of
        False ->
            todo

        True ->
            case todo.expanded of
                True ->
                    { todo | expanded = False, input = "" }

                False ->
                    { todo | expanded = True }


removeTag : Int -> Int -> Todo -> Todo
removeTag id index todo =
    case todo.id == id of
        False ->
            todo

        True ->
            { todo | tags = List.take index todo.tags ++ List.drop (index + 1) todo.tags }


removeWhitespace : String -> String
removeWhitespace str =
    case Regex.fromString "\\s+" of
        Nothing ->
            str

        Just regex ->
            Regex.replace regex (\_ -> "-") str



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "text-align" "center" ]
        [ viewTodoInput model.input
        , viewAddButton model.input
        , viewTodoList model.todoList
        ]


viewTodoInput : String -> Html Msg
viewTodoInput todoInput =
    input
        [ type_ "text"
        , placeholder "Add new todo"
        , onInput TodoInputChanged
        , value todoInput
        ]
        []


viewAddButton : String -> Html Msg
viewAddButton todoInput =
    button [ onClick (AddTodo todoInput) ] [ text "Add" ]


viewTodoList : List Todo -> Html Msg
viewTodoList todoList =
    ul
        [ style "list-style-type" "none"
        , style "padding" "0"
        ]
        (List.map viewTodo todoList)


viewTodo : Todo -> Html Msg
viewTodo todo =
    li []
        [ viewTodoTags todo.id todo.tags
        , p [ style "margin-top" "0px" ]
            [ button [ onClick (Remove todo.id) ] [ text "X" ]
            , viewTodoContent todo
            , button [ onClick (ToggleTagInput todo.id) ] [ text "Tags" ]
            , viewExpanded todo
            ]
        ]


viewTodoContent : Todo -> Html Msg
viewTodoContent todo =
    p
        [ onClick (Toggle todo.id)
        , style "display" "inline"
        , style "margin-left" "8px"
        , style "margin-right" "8px"
        , todoDecoration todo.done
        ]
        [ text todo.content ]


todoDecoration : Bool -> Html.Attribute Msg
todoDecoration done =
    case done of
        True ->
            style "text-decoration" "line-through"

        False ->
            style "" ""


viewTodoTags : Int -> List String -> Html Msg
viewTodoTags id tags =
    p
        [ style "display" "inline"
        , style "color" "#aaaaaa"
        , style "font-size" ".8rem"
        ]
        (List.indexedMap (viewTodoTag id) tags)


viewTodoTag : Int -> Int -> String -> Html Msg
viewTodoTag id index tag =
    p
        [ style "display" "inline"
        , style "margin-right" "3px"
        , style "margin-left" "3px"
        , onClick (RemoveTag id index)
        ]
        [ text tag ]


viewExpanded : Todo -> Html Msg
viewExpanded todo =
    case todo.expanded of
        True ->
            viewTagInput todo

        False ->
            text ""


viewTagInput : Todo -> Html Msg
viewTagInput todo =
    div []
        [ input
            [ type_ "text"
            , placeholder "Add new tag"
            , onInput (TagInputChanged todo.id)
            , value todo.input
            ]
            []
        , button [ onClick (AddTag todo.id todo.input) ] [ text "Add" ]
        ]



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
        [ ( "todo", E.string "" )
        , ( "todoList", E.list encodeTodo model.todoList )
        ]


encodeTodo : Todo -> E.Value
encodeTodo todo =
    E.object
        [ ( "id", E.int todo.id )
        , ( "done", E.bool todo.done )
        , ( "content", E.string todo.content )
        , ( "tags", E.list (\t -> E.string t) todo.tags )
        , ( "input", E.string "" )
        , ( "expanded", E.bool False )
        ]


decoder : D.Decoder Model
decoder =
    D.map2 Model
        (D.field "todo" D.string)
        (D.field "todoList" (D.list todoDecoder))


todoDecoder : D.Decoder Todo
todoDecoder =
    D.map6 Todo
        (D.field "id" D.int)
        (D.field "done" D.bool)
        (D.field "content" D.string)
        (D.field "tags" (D.list D.string))
        (D.field "input" D.string)
        (D.field "expanded" D.bool)
