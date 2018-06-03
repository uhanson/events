module Client exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Name = String

-- MODEL

type alias Event =
  { id : Int
  , name : Name
  }

type alias Model = 
  { events : List Event
  , name : Name
  , nextId : Int
  }

model : Model
model = { events = [], name = "", nextId = 1 }

-- MESSAGES

type Msg
    = Change Name
    | Remove Int
    | Add Int

-- VIEW

view : Model -> Html Msg
view model =
    let eventList = List.map viewEvent model.events 
    in div []
        (List.append eventList
          [ viewAddEvent model ])

viewEvent : Event -> Html Msg
viewEvent event =
  div []
      [ text event.name 
      , button [ onClick (Remove event.id) ] [ text "-" ] ]

viewAddEvent : Model -> Html Msg
viewAddEvent model =
  div []
      [ input [ type_ "text", placeholder "Name", onInput Change ] []
      , button [ onClick (Add model.nextId) ] [ text "Add" ] ]

-- UPDATE

update : Msg -> Model -> Model
update msg model =
    case msg of
        Change name ->
            { model | name = name }

        Remove id ->
            { model | events = List.filter (\event -> event.id /= id) model.events}

        Add id -> 
            { model | events = 
                        let ev = Event model.nextId model.name
                        in ev :: model.events,
                      nextId = model.nextId + 1 }

-- MAIN

main : Program Never Model Msg
main =
    beginnerProgram 
        { model = model
        , view = view
        , update = update
        }