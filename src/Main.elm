

module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, input, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model = 
    { content: String
    , count: Int
    , username: String
    , password: String
    , password2: String}

init : Model
init = 
    { content = ""
    , count = 0
    , username = ""
    , password = ""
    , password2 = "" }

type Msg = 
    Increment | Decrement | Reset 
    | ChangeText String | ChangeUsername String | ChangePassword String | ChangePassword2 String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      {model | count = model.count + 1}

    Decrement ->
      {model | count = model.count - 1}

    Reset -> 
      {model | count = 0}
    
    ChangeText content -> 
      {model | content = content}

    ChangeUsername username -> 
      {model | username = username}
    ChangePassword password -> 
      {model | password = password}
    ChangePassword2 password2 -> 
      {model | password2 = password2}

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg = 
    input [type_ t, placeholder p, value v, onInput toMsg ] []

viewValidation : Model -> Html msg
viewValidation model = 
    if not (containsAnyOf "$!_-" model.password) then
        passwordNeedsWeirdChars
    else if String.length model.password < 8 then
        passwordTooShortWarning
    else if model.password == model.password2 then
        passwordsOkIndicator
    else
        mismatchWarning

view model =
  div []
    [ viewInput "text" "Username" model.username ChangeUsername
    , viewInput "password" "Password" model.password ChangePassword
    , viewInput "password" "Password Again" model.password2 ChangePassword2
    , div [] []
    , viewValidation model
    , div [] []
    , input [ placeholder "Text to reverse", value model.content, onInput ChangeText] []
    , div [] [text (String.reverse model.content)]
    , div [] [ text (String.fromInt (String.length model.content)) ]

    , button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model.count) ]
    , button [ onClick Increment ] [ text "+" ]
    , button [ onClick Reset ] [ text "reset" ]
    ]


passwordsOkIndicator = div [ style "color" "green" ] [ text "OK"]
mismatchWarning = div [ style "color" "red" ] [ text "Passwords do not match."]
passwordTooShortWarning = div [ style "color" "red" ] [ text "Password is too short"]
passwordNeedsWeirdChars = div [ style "color" "red" ] [ text "Password needs any of these: $!_-"]

containsChar : Char -> String -> Bool
containsChar c str =
    String.contains (String.fromChar c) str

containsAnyOf : String -> String -> Bool 
containsAnyOf chars str = 
    List.any (\c -> containsChar c str) (String.toList chars)
