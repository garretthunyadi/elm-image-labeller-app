

module Label exposing (..)

import Browser
import Html exposing (Html, input, button, div, text, img, ul, li)
import Html.Attributes exposing (width,height,style,placeholder,value, type_, src)
import Html.Events exposing (onClick, onInput)
import List exposing (map)

main =
  Browser.sandbox { init = init, update = update, view = view }

defaultHeight = 300


type alias Model = 
    { content: String
    , count: Int
    , username: String
    , password: String
    , password2: String
    , imageDims: Int 
    , domains: (List String)}

init : Model
init = 
    { content = ""
    , count = 0
    , username = ""
    , password = ""
    , password2 = ""
    , imageDims = 300 
    , domains = someDomains }

type Msg = 
    Increment | Decrement | Reset 
    | ChangeText String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      {model | count = model.imageDims + 10}

    Decrement ->
      {model | count = model.imageDims - 10}

    Reset -> 
      {model | imageDims = 300}
    
    ChangeText content -> 
      {model | content = content}

    -- ChangeImageWidth w -> 
    --   {model | imageWidth = w}
    -- ChangeImageHeight h -> 
    --   {model | imageHeight = h}

viewInput : String -> String -> (String -> msg) -> Html msg
viewInput p v toMsg = 
    input [type_ "text", placeholder p, value v, onInput toMsg ] []

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

view : Model -> Html Msg
view model =
  div []
    [ div [] []

    , button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model.count) ]
    , button [ onClick Increment ] [ text "+" ]
    , button [ onClick Reset ] [ text "reset" ]
    , div [] []
    , img [src (imageUrl "1107savoline.com"), width model.imageDims, height model.imageDims] []
    -- , viewImages model.imageDims model.imageDims model.domains
    , renderList ["aaa", "Bbb"]
    , div [] (viewImages model.imageDims model.imageDims model.domains)
    -- , renderImageList model.imageDims model.imageDims ["aaa", "Bbb"]
    ]

renderList : List String -> Html msg
renderList lst =
    ul []
        (List.map (\l -> li [] [ text l ]) lst)

-- renderImageList : Int -> Int -> List String -> Html msg
-- renderImageList lst =
--     div []
--         (List.map (\l -> li [] [ text l ]) lst)


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


imageUrl domain = "https://eiginsites.s3.amazonaws.com/sites/"++domain++"/screenshots_600x600.jpeg"

viewImage : Int -> Int -> String -> Html Msg
viewImage w h domain = 
    img [src (imageUrl domain), width w, height h] []

viewImages: Int -> Int -> List String -> List (Html Msg)
viewImages width height domains = map (viewImage width height) domains

someDomains: List String
someDomains = 
    ["deavervineyards.com", "douglashatch.com", "dk.com.pk"
    , "brainfart55.com", "bereaworldmission.org"
    , "anywherefest.com", "dadpowered.com", "beautifulworld.uk.com", "boatrax.com"
    , "denigris1889.com", "copperzapper.com", "bwavejewelry.com", "cassavasite.com"
    , "artofmagic.com", "bbblanc.com", "dynatecmigrate.info", "conceptfireplace.com"
    , "bronzehorsemanbooks.com", "cp-e.com", "artcellarhouston.com", "chefsgarden.net"
    , "desishops.com", "cheapjiujitsu.com", "crimsonlotusteas.com", "cumbrestoltec.org"
    , "daytopraise.com"]