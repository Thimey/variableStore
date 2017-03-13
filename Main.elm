module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, target, href, property, defaultValue, value, type_, placeholder)
import Html.Events exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }


type alias Model =
    { variables : List Variable
    , tags : List Tag
    , tagInput : String
    , variableNameInput : String
    , variableValueInput : String
    , lastId : Int
    }


type alias Variable =
    { id : Int
    , name : String
    , value : String
    , tags : List Int
    }


type alias Tag =
    { id : Int
    , label : String
    }


initialModel : Model
initialModel =
    { variables = []
    , tags = []
    , tagInput = ""
    , variableNameInput = ""
    , variableValueInput = ""
    , lastId = 1
    }


type Msg
    = SetTagInput String
    | SetVariableNameInput String
    | SetVariableValueInput String
    | AddTag Tag
    | AddVariable Variable
    | AssignTag Int Int
    | RemoveTag Int Int
    | RemoveVariable Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetTagInput tagLabel ->
            { model | tagInput = tagLabel }

        SetVariableNameInput name ->
            { model | variableNameInput = name }

        SetVariableValueInput value ->
            { model | variableValueInput = value }

        AddTag tag ->
            { model | tags = tag :: model.tags, lastId = model.lastId + 1 }

        AddVariable variable ->
            { model
                | variables = variable :: model.variables
                , variableNameInput = ""
                , variableValueInput = ""
                , lastId = model.lastId + 1
            }

        AssignTag varId tagId ->
            { model
                | variables =
                    List.map
                        (\var ->
                            if var.id == varId then
                                { var | tags = tagId :: var.tags }
                            else
                                var
                        )
                        model.variables
            }

        RemoveTag varId tagId ->
            { model
                | variables =
                    List.map
                        (\var ->
                            if var.id == varId then
                                { var | tags = List.filter (\id -> id /= tagId) var.tags }
                            else
                                var
                        )
                        model.variables
            }

        RemoveVariable varId ->
            { model | variables = List.filter (\var -> var.id /= varId) model.variables }


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ onInput SetVariableNameInput ] []
            , input [ onInput SetVariableValueInput ] []
            , button [ onClick (AddVariable (Variable model.lastId model.variableNameInput model.variableValueInput [])) ] [ text "add" ]
            ]
        , div []
            [ ul [] (List.map (\var -> li [] [ text (var.name ++ " = " ++ var.value) ]) model.variables) ]
        ]
