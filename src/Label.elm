module Label exposing (Domain, main)

{-| Notes and Items to do

[ ] Where do I get my input?

[ ] Configure Labels
[ ] Show/Hide Label Configuration section
[ ] Add a list for labels containing the text labels
[ ] Add logic to set Label.index on any update of the list
[-] OOS: Add a color selector

[] Need to determine if I am manipulating files directly (as orig thought) or just labels pointing to S3 images

  - manipulating files is more general, but requires syncing with S3 and manipulating files locally
  - maintaining labels is more lightweight and perferred if I can make the downstream workflow work

Approach 1: Manipulating labels, referencing only S3, generating JSON lines only
Approach 2: Manipulating labels, referencing only S3, generating .lst file
Approach 3: Manipulating labels, referencing only S3, generating a script to be run, or a command to a backend server

[x] Show/Hide JSON Lines output
[x] Bug: Need to cycle through having no label

-}

-- import Bool.Extra

import Browser
import Html exposing (Html, button, div, img, input, pre, span, text, textarea)
import Html.Attributes exposing (cols, height, rows, src, type_, value, width)
import Html.Events exposing (on, onClick, onInput)
import List exposing (map)
import List.Extra as LE exposing (zip)



-- import List.Extra as LE exposing ((!!))


defaultHeight : Int
defaultHeight =
    80


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- model


type alias Domain =
    String


type alias Label =
    { name : String
    , index : Int
    }


makeLabels : List String -> List Label
makeLabels strings =
    List.map2 (\index name -> { index = index, name = name }) (List.range 0 (List.length strings)) strings



-- labels : List Image -> List Label
-- labels images =
--     let
--         maybeLabel : Image -> Maybe Label
--         maybeLabel image =
--             Maybe.map
--                 (\l -> l)
--                 image.label
--     in
--     List.filterMap maybeLabel images


imagesWithLabels : List Image -> List ( Image, Label )
imagesWithLabels images =
    let
        maybeLabel : Image -> Maybe ( Image, Label )
        maybeLabel image =
            case image.label of
                Just l ->
                    Just ( image, l )

                Nothing ->
                    Nothing
    in
    List.filterMap maybeLabel images


jsonLineFor : Image -> Label -> String
jsonLineFor image label =
    "{\"source-ref\":\"" ++ imageUrl image.domain ++ "\", \"class\":\"" ++ String.fromInt label.index ++ "\", \"class-metadata\": {\"class-name\": \"" ++ label.name ++ "\"}}\n"



-- {"source-ref":"s3://image/filename1.jpg", "class":"0"}
-- {"source-ref":"s3://image/filename2.jpg", "class":"1", "class-metadata": {"class-name": "cat", "type" : "groundtruth/image-classification"}}


jsonLabels : List Image -> List String
jsonLabels images =
    map (\( i, l ) -> jsonLineFor i l) (imagesWithLabels images)



-- map (\i l -> "") (imagesWithLabels images)
-- [ "aaa", "bbb" ]
-- map jsonLineFor (imagesWithLabels images)
-- map (\( i, l ) -> "") (imagesWithLabels images)
-- let
--     maybeJsonLabel : Image -> Maybe String
--     maybeJsonLabel image =
--         Maybe.map
--             (\l -> "{\"source-ref\":\"" ++ imageUrl image.domain ++ "\", \"class\":\"" ++ String.fromInt l.index ++ "\"}\n")
--             image.label
-- in
-- List.filterMap maybeJsonLabel images
-- map (\l -> "{\"source-ref\":\"" ++ imageUrl image.domain ++ "\", \"class\":\"" ++ String.fromInt l.index ++ "\"}\n") (labels images)


type alias Image =
    { domain : Domain
    , label : Maybe Label
    }


imageUrl : Domain -> String
imageUrl domain =
    "https://eiginsites.s3.amazonaws.com/sites/" ++ domain ++ "/screenshots_600x600.jpeg"



-- s3Url : Domain -> String
-- s3Url domain =
--     -- TODO incorrect
--     "s3://sites/" ++ domain ++ "/screenshots_600x600.jpeg"


type alias Model =
    { flash : String

    -- , jsonLabels : List String
    , labels : List Label
    , imageDims : Int
    , images : List Image
    , showLabelConfig : Bool
    , showJsonLines : Bool
    , rawCategoryText : String
    }


init : Model
init =
    let
        categoryText =
            "Bad\nOk\nGood"
    in
    { flash = ""
    , labels = makeLabels (categoriesFrom categoryText)
    , imageDims = defaultHeight
    , images = someImages
    , showLabelConfig = True
    , showJsonLines = True
    , rawCategoryText = categoryText
    }


type Msg
    = Increment
    | Decrement
    | Reset
    | LabelChange Image
    | ToggleLabelConfigSection
    | ToggleJsonLinesSection
    | UpdateCategoriesFromCategoryTextArea String



-- controller


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | imageDims = model.imageDims + 50 }

        Decrement ->
            { model | imageDims = model.imageDims - 50 }

        Reset ->
            { model | imageDims = defaultHeight }

        LabelChange image ->
            let
                updateImage i =
                    if i == image then
                        { i | label = nextLabel model.labels image.label }

                    else
                        i

                updatedImages =
                    List.map updateImage model.images
            in
            { model
                | flash = image.domain ++ ": " ++ labelText image.label
                , images = updatedImages

                -- , jsonLabels = jsonLabels model.images
            }

        ToggleJsonLinesSection ->
            let
                newVal =
                    not model.showJsonLines
            in
            { model | showJsonLines = newVal }

        ToggleLabelConfigSection ->
            let
                newVal =
                    not model.showLabelConfig
            in
            { model | showLabelConfig = newVal }

        UpdateCategoriesFromCategoryTextArea categoryText ->
            let
                cats =
                    categoriesFrom categoryText
            in
            { model | flash = String.concat cats, labels = makeLabels cats, rawCategoryText = categoryText }



-- case categoriesFrom categoryText of
-- Just categories ->
--     { model | labels = makeLabels categories }
-- Nothing ->
--     { model | flash = "INVALID CATEGORY TEXT" }


categoriesFrom : String -> List String
categoriesFrom text =
    String.split "\n" text



-- view


labelText : Maybe Label -> String
labelText label =
    case label of
        Just l ->
            l.name

        Nothing ->
            "--"


nextLabel : List Label -> Maybe Label -> Maybe Label
nextLabel list maybeItem =
    let
        getAt2 : List a -> Int -> Maybe a
        getAt2 l i =
            LE.getAt i l

        nextItem l item =
            list
                |> LE.elemIndex item
                |> Maybe.map ((+) 1)
                |> Maybe.andThen (getAt2 l)
    in
    case ( list, maybeItem ) of
        ( l, Nothing ) ->
            LE.getAt 0 l

        ( l, Just a ) ->
            nextItem l a



-- cycleWithBlank ls ml


cycleWithBlank : List a -> a -> Maybe a
cycleWithBlank list a =
    let
        getAt2 : List a -> Int -> Maybe a
        getAt2 l i =
            LE.getAt i l

        -- getNext : a -> List a -> Maybe a
        getNext item l =
            list
                |> LE.elemIndex item
                |> Maybe.map ((+) 1)
                |> Maybe.andThen (getAt2 l)
    in
    case getNext a list of
        Just next ->
            Just next

        Nothing ->
            LE.getAt 0 list



-- case ml of
--     Just l ->
--         cycleWithBlank ls l
--     Nothing ->
--         List.head ls
-- viewInput : String -> String -> (String -> msg) -> Html msg
-- viewInput p v toMsg =
--     input [ type_ "text", placeholder p, value v, onInput toMsg ] []


view : Model -> Html Msg
view model =
    div []
        [ div [] []
        , button [ onClick Decrement ] [ text "-" ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Reset ] [ text "reset" ]

        -- , div [] []
        , text model.flash
        , div [] []
        , maybeShowLabelConfig model
        , Html.hr [] []

        -- , img [src (imageUrl "1107savoline.com"), width model.imageDims, height model.imageDims] []
        -- , viewImages model.imageDims model.imageDims model.domains
        -- , renderList ["aaa", "Bbb"]
        , div [] (viewImages model.imageDims model.imageDims model.images)

        -- , renderImageList model.imageDims model.imageDims ["aaa", "Bbb"]
        , div [] []
        , maybeShowJsonLabels model
        ]


maybeShowJsonLabels : Model -> Html Msg
maybeShowJsonLabels model =
    if model.showJsonLines then
        div []
            [ button
                [ onClick ToggleJsonLinesSection ]
                [ text "Hide Json" ]
            , dumpJsonLabelsAsPre model.images
            ]

    else
        div []
            [ button [ onClick ToggleJsonLinesSection ] [ text "Show Json" ]
            ]


maybeShowLabelConfig : Model -> Html Msg
maybeShowLabelConfig model =
    if model.showLabelConfig then
        div []
            [ button
                [ onClick ToggleLabelConfigSection ]
                [ text "Hide Label Configuration" ]
            , viewLabelConfig model
            ]

    else
        div []
            [ button [ onClick ToggleLabelConfigSection ] [ text "Label Configuration" ]
            ]


viewLabelConfig : Model -> Html Msg
viewLabelConfig model =
    -- div [] (map (\l -> viewLabelEditor l) model.labels)
    -- div [] (map (\l -> text l.name) ls)
    div []
        [ textarea [ cols 40, rows 6, value model.rawCategoryText, onInput UpdateCategoriesFromCategoryTextArea ] []
        ]



-- viewLabelEditor : Label -> Html Msg
-- viewLabelEditor label =
--     div []
--         [ -- viewInput label.name label.name (onInput (UpdateLabelsWithLabel label))
--         --   viewInput label.name UpdateLabelWithName
--             textarea (map name la
--         ]
-- viewInput : String -> (String -> msg) -> Html msg
-- viewInput v toMsg =
--     input [ type_ "text", value v, onInput toMsg ] []


viewImage : Int -> Int -> Image -> Html Msg
viewImage w h image =
    img
        [ src (imageUrl image.domain)
        , width w
        , height h
        , onClick (LabelChange image)
        ]
        []


viewImages : Int -> Int -> List Image -> List (Html Msg)
viewImages width height images =
    map (viewImage width height) images



-- data


someDomains : List String
someDomains =
    [ "deavervineyards.com"
    , "douglashatch.com"
    , "dk.com.pk"
    , "brainfart55.com"
    , "bereaworldmission.org"
    , "anywherefest.com"
    , "dadpowered.com"
    , "beautifulworld.uk.com"
    , "boatrax.com"
    , "denigris1889.com"
    , "copperzapper.com"
    , "bwavejewelry.com"
    , "cassavasite.com"
    , "artofmagic.com"
    , "bbblanc.com"
    , "dynatecmigrate.info"
    , "conceptfireplace.com"
    , "bronzehorsemanbooks.com"
    , "cp-e.com"
    , "artcellarhouston.com"
    , "chefsgarden.net"
    , "desishops.com"
    , "cheapjiujitsu.com"
    , "crimsonlotusteas.com"
    , "cumbrestoltec.org"
    , "daytopraise.com"
    ]


someImages : List Image
someImages =
    map (\d -> makeImage d) someDomains


makeImage : Domain -> Image
makeImage d =
    { domain = d, label = Nothing }



-- helper
-- getWanted : List a -> a -> Maybe a
-- getWanted list currentElement =
--     let
--         findNextInList l =
--             case l of
--                 [] ->
--                     Nothing
--                 x :: [] ->
--                     if x == currentElement then
--                         List.head list
--                     else
--                         Nothing
--                 x :: y :: rest ->
--                     if x == currentElement then
--                         Just y
--                     else
--                         findNextInList (y :: rest)
--     in
--     findNextInList list


dumpJsonLabelsAsPre : List Image -> Html Msg
dumpJsonLabelsAsPre images =
    pre []
        [ text (jsonLabels images |> String.concat) ]
