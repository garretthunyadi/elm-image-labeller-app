

module Label exposing (..)

import Browser
import Html exposing (Html, input, button, div, text, img, ul, li)
import Html.Attributes exposing (width,height,style,placeholder,value, type_, src)
import Html.Events exposing (onClick, onInput)
import List exposing (map)
-- import List.Extra exposing (zip)

main =
  Browser.sandbox { init = init, update = update, view = view }

defaultHeight = 80


type alias Domain = String

type alias Label = 
    { name: String
    , index: Int }

type alias Image = 
    { domain: Domain
    , label: Maybe Label}

type alias LabelledImage = 
    { image: Image
    , label: Label}

type alias Model = 
    { flash: String
    , jsonLabels: String
    , labels: List Label
    , imageDims: Int
    , images: List Image}

init : Model
init = 
    { flash = ""
    , jsonLabels = ""
    , labels = makeLabels ["Category1", "Category2"]
    , imageDims = defaultHeight 
    , images = someImages }

type Msg = 
    Increment | Decrement | Reset | LabelChange Image

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      {model | imageDims = model.imageDims + 50}

    Decrement ->
      {model | imageDims = model.imageDims - 50}

    Reset -> 
      {model | imageDims = defaultHeight}
    
    LabelChange image ->
        let
            updateImage i =
                if i == image then
                    { i | label = (nextLabel model.labels image.label) }
                else
                    i

            updatedImages =
                List.map updateImage model.images
        in
            { model 
                | flash = image.domain ++ ": " ++ labelText image.label
                , images = updatedImages
                , jsonLabels = dumpJsonLabels model.images
                }

labelText : Maybe Label -> String
labelText label =
    case label of
        Just l -> l.name
        Nothing -> "--"

nextLabel : List Label -> Maybe Label -> Maybe Label
nextLabel ls ml = 
  case ml of
    Just l -> getWanted ls l
    Nothing -> List.head ls

viewInput : String -> String -> (String -> msg) -> Html msg
viewInput p v toMsg = 
    input [type_ "text", placeholder p, value v, onInput toMsg ] []

view : Model -> Html Msg
view model =
  div []
    [ div [] []
    , button [ onClick Decrement ] [ text "-" ]
    , button [ onClick Increment ] [ text "+" ]
    , button [ onClick Reset ] [ text "reset" ]
    , div [] []
    , text model.flash
    , div [] []
    -- , img [src (imageUrl "1107savoline.com"), width model.imageDims, height model.imageDims] []
    -- , viewImages model.imageDims model.imageDims model.domains
    -- , renderList ["aaa", "Bbb"]
    , div [] (viewImages model.imageDims model.imageDims model.images)
    -- , renderImageList model.imageDims model.imageDims ["aaa", "Bbb"]
    , div [] []
    , text model.jsonLabels

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


imageUrl domain = "https://eiginsites.s3.amazonaws.com/sites/"++domain++"/screenshots_600x600.jpeg"

viewImage : Int -> Int -> Image -> Html Msg
viewImage w h image = 
    img [
        src (imageUrl image.domain), width w, height h,
        onClick (LabelChange image)] []

viewImages: Int -> Int -> List Image -> List (Html Msg)
viewImages width height images = map (viewImage width height) images

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

someImages: List Image
someImages = map (\d -> makeImage d ) someDomains

makeImage: Domain -> Image
makeImage d = {domain = d, label = Nothing }

-- helper
getWanted  : List a -> a -> Maybe a
getWanted list currentElement = 
    let 
        findNextInList l = case l of
            [] -> Nothing
            x :: [] -> if x == currentElement
                          then List.head list
                          else Nothing
            x :: y :: rest -> if x == currentElement
                          then Just y
                          else findNextInList (y :: rest)
    in
        findNextInList list

dumpJsonLabels :List Image -> String
dumpJsonLabels images =
    -- filterMap : (a -> Maybe b) -> List a -> List b
    -- concat : List String -> String
    List.filterMap maybeJsonLabel images |> String.concat

maybeJsonLabel : Image -> Maybe String
maybeJsonLabel image =
    case image.label of
        Just l -> Just ("{\"source-ref\":\"s3://image/filename1.jpg\", \"class\":\"" ++ String.fromInt l.index ++"\"}\n")
        Nothing -> Nothing

    -- {"source-ref":"s3://image/filename1.jpg", "class":"0"} 
    -- {"source-ref":"s3://image/filename2.jpg", "class":"1", "class-metadata": {"class-name": "cat", "type" : "groundtruth/image-classification"}}

makeLabels : List String -> List Label
makeLabels strings = 
    List.map2 (\index name -> {index = index, name = name }) (List.range 0 (List.length strings)) strings