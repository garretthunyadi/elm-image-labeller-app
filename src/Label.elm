module Label exposing (main)

{-| Notes and Items to do

[ ] Where do I get my input?
work from local files and save label files

    file input supports previous labels:
    image_name, image_url, [class_name]

    file output
    image_name, image_url, class_name

[ ] Bug: Label names should always be generated dynamically, in case the schema changes.
Or should it?
[ ] Add logic to set Label.index on any update of the list

[] Need to determine if I am manipulating files directly (as orig thought) or just labels pointing to S3 images

  - manipulating files is more general, but requires syncing with S3 and manipulating files locally
  - maintaining labels is more lightweight and perferred if I can make the downstream workflow work

Approach 1: Manipulating labels, referencing only S3, generating JSON lines only
Approach 2: Manipulating labels, referencing only S3, generating .lst file
Approach 3: Manipulating labels, referencing only S3, generating a script to be run, or a command to a backend server

[-] Add a list for labels containing the text labels
[-] OOS: Add a color selector
[x] Show/Hide Label Configuration section
[/] Configure Labels
[x] Color coding
[x] Show/Hide JSON Lines output
[x] Bug: Need to cycle through having no label

-}

-- import Bool.Extra

import Browser
import Debug exposing (log)
import File exposing (File)
import File.Download
import File.Select
import Html exposing (Html, button, div, img, pre, span, text, textarea)
import Html.Attributes exposing (class, cols, height, rows, src, value, width)
import Html.Events exposing (onClick, onInput)
import List exposing (map)
import List.Extra as LE
import Task



-- import List.Extra as LE exposing ((!!))


defaultHeight : Int
defaultHeight =
    300


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    -- TODO impl time to label the saved files
    Sub.none



----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
-- model
----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------


type alias Domain =
    String


type alias Label =
    { name : String
    , index : Int
    }


makeLabels : List String -> List Label
makeLabels strings =
    List.map2 (\index name -> { index = index, name = name }) (List.range 0 (List.length strings)) strings


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



-- {"source-ref":"s3://image/filename1.jpg", "class":"0"}
-- {"source-ref":"s3://image/filename2.jpg", "class":"1", "class-metadata": {"class-name": "cat", "type" : "groundtruth/image-classification"}}


jsonLineFor : Image -> Label -> String
jsonLineFor image label =
    "{\"source-ref\":\"" ++ imageUrl image.domain ++ "\", \"class\":\"" ++ String.fromInt label.index ++ "\", \"class-metadata\": {\"class-name\": \"" ++ label.name ++ "\"}}\n"


jsonLabels : List Image -> List String
jsonLabels images =
    map (\( i, l ) -> jsonLineFor i l) (imagesWithLabels images)


type alias Image =
    { domain : Domain
    , label : Maybe Label
    }


imageUrl : Domain -> String
imageUrl domain =
    "https://eiginsites.s3.amazonaws.com/sites/" ++ domain ++ "/screenshots_600x600.jpeg"


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



-- init : Model
-- init =
--     let
--         categoryText =
--             "Bad\nOk\nGood"
--     in
--     { flash = ""
--     , labels = makeLabels (categoriesFrom categoryText)
--     , imageDims = defaultHeight
--     , images = someImages
--     , showLabelConfig = True
--     , showJsonLines = True
--     , rawCategoryText = categoryText
--     }


init : () -> ( Model, Cmd Msg )
init _ =
    ( let
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
    , Cmd.none
    )



----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
-- update
----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------


type Msg
    = Increment
    | Decrement
    | LabelChange Image
    | ToggleLabelConfigSection
    | ToggleJsonLinesSection
    | UpdateCategoriesFromCategoryTextArea String
    | SaveLabels
    | LoadLabels
    | LoadImageSet
    | LoadDomains
    | GotLabelFile File
    | GotImageFile File
    | GotDomainFile File
    | SaveImageSet
    | LabelFileLoaded String
    | ImageFileLoaded String
    | DomainFileLoaded String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | imageDims = model.imageDims + 50 }, Cmd.none )

        Decrement ->
            ( { model | imageDims = model.imageDims - 50 }, Cmd.none )

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
            ( { model
                | flash = image.domain ++ ": " ++ labelText image.label
                , images = updatedImages

                -- , jsonLabels = jsonLabels model.images
              }
            , Cmd.none
            )

        ToggleJsonLinesSection ->
            let
                newVal =
                    not model.showJsonLines
            in
            ( { model | showJsonLines = newVal }, Cmd.none )

        ToggleLabelConfigSection ->
            let
                newVal =
                    not model.showLabelConfig
            in
            ( { model | showLabelConfig = newVal }, Cmd.none )

        UpdateCategoriesFromCategoryTextArea categoryText ->
            let
                cats =
                    categoriesFrom categoryText
            in
            ( { model | flash = String.concat cats, labels = makeLabels cats, rawCategoryText = categoryText }, Cmd.none )

        LoadLabels ->
            ( model, loadLabels model )

        LoadImageSet ->
            ( model, loadImageSet model )

        LoadDomains ->
            ( model, loadDomains model )

        SaveLabels ->
            ( model, save model )

        GotLabelFile file ->
            ( model
            , Task.perform LabelFileLoaded (File.toString file)
            )

        GotDomainFile file ->
            ( model
            , Task.perform DomainFileLoaded (File.toString file)
            )

        GotImageFile file ->
            ( model
            , Task.perform ImageFileLoaded (File.toString file)
            )

        ImageFileLoaded text ->
            ( { model | images = imagesFromImageSetFileText text }
            , Cmd.none
            )

        LabelFileLoaded text ->
            let
                -- domainsAndMaybeLabels =
                ( images, labels ) =
                    imagesAndLabelsFromTextFile text
            in
            ( { model | labels = labels, images = images }
            , Cmd.none
            )

        DomainFileLoaded text ->
            ( { model | images = imagesFromDomainFileText text }
            , Cmd.none
            )

        SaveImageSet ->
            ( model, saveImageSet model )


imagesFromImageSetFileText : String -> List Image
imagesFromImageSetFileText text =
    -- TODO impl
    someImages


imagesFromDomainFileText : String -> List Image
imagesFromDomainFileText text =
    let
        domainFromString : String -> Maybe Domain
        domainFromString s =
            -- TODO validate string? Currently any line will be recognized as a valid domain
            -- should return Maybe String and then filter the results
            --
            -- if this is a csv, assume that the first row is the name/domain
            LE.getAt 0 (String.split "," s)

        domains =
            String.split "\n" text
                |> List.filterMap domainFromString
    in
    List.map (\d -> makeImage d) domains


imagesAndLabelsFromTextFile : String -> ( List Image, List Label )
imagesAndLabelsFromTextFile text =
    let
        images =
            List.map (\( d, ml ) -> makeImage d) domainsAndMaybeLabels

        labels =
            List.filterMap (\( d, ml ) -> ml) domainsAndMaybeLabels

        domainsAndMaybeLabels =
            domainsAndMaybeLabelsFromFileText

        domainsAndMaybeLabelsFromFileText : List ( Domain, Maybe Label )
        domainsAndMaybeLabelsFromFileText =
            String.split "\n" text |> List.filterMap mDomainAndMLabelFromString

        mDomainAndMLabelFromString : String -> Maybe ( Domain, Maybe Label )
        mDomainAndMLabelFromString line =
            let
                split =
                    String.split "," line
            in
            case LE.getAt 0 split of
                Just s ->
                    let
                        d =
                            String.trim s

                        ml =
                            case LE.getAt 1 split of
                                Just s2 ->
                                    -- TODO: setting zero index... remove indexes if possible
                                    -- { index = 0, name = s2 }
                                    Just (Label s2 0)

                                Nothing ->
                                    Nothing
                    in
                    Just ( d, ml )

                Nothing ->
                    Nothing

        -- let
        -- domainFromString : String -> Maybe Domain
        -- domainFromString s =
        --     -- TODO validate string? Currently any line will be recognized as a valid domain
        --     -- should return Maybe String and then filter the results
        --     --
        --     -- if this is a csv, assume that the first row is the name/domain
        --     LE.getAt 0 (String.split "," s)
        -- domains =
        --     String.split "\n" text
        --         |> List.filterMap domainFromString
        -- in
        -- List.map (\d -> makeImage d) domains
    in
    ( images, labels )



-- labelsFromLabelFileText : String -> List Label
-- labelsFromLabelFileText text =
--     let
--         -- String (lines) -> List Maybe (Domain, Maybe Label)
--         domainAndLabelTextFromString : String -> ( Maybe Domain, Maybe String )
--         domainAndLabelTextFromString s =
--             -- TODO validate string? Currently any line will be recognized as a valid domain
--             --
--             -- if this is a csv, assume that the first row is the name/domain
--             let
--                 parts =
--                     String.split "," s
--             in
--             ( LE.getAt 0 parts, LE.getAt 1 parts )
--         -- mDomainAndMLabelStringTuples : List Maybe ( Domain, Maybe String )
--         -- mDomainAndMLabelStringTuples =
--         --     String.split "\n" text
--         --         |> List.filterMap domainAndLabelTextFromString
--     in
--     List.map (\( md, ms ) -> maybeMakeLabel md ms) domainAndLabelTextFromString
-- maybeMakeLabel : Maybe Domain -> Maybe String -> Maybe Label
-- maybeMakeLabel mDomain mString =
--     --TODO: impl
--     Nothing
-- imagesFromImageFile2 : File -> List Image
-- imagesFromImageFile2 file =
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



----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
-- View
--
--  V                  V
--   V                V
--    V              V
--     v            v
--      v          v
--       v        v
--        v      v
--         v    v
--          v  v
--           vv
--
--
----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div []
        [ div [] []
        , button [ onClick Decrement ] [ text "-" ]
        , button [ onClick Increment ] [ text "+" ]
        , text "     |     "
        , button [ onClick SaveLabels ] [ text "Save Labels" ]
        , button [ onClick LoadLabels ] [ text "Load Labels" ]
        , text "     |     "
        , button [ onClick SaveImageSet ] [ text "Save Image Set" ]
        , button [ onClick LoadImageSet ] [ text "Load Image Set" ]
        , text "     |     "
        , button [ onClick LoadDomains ] [ text "Load Domains List" ]

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
    let
        labelLegendFor : Label -> Html Msg
        labelLegendFor label =
            span
                [ class ("label-" ++ String.fromInt label.index)
                ]
                [ text label.name ]

        showLabelLegend : Html Msg
        showLabelLegend =
            span [] (List.map labelLegendFor model.labels)

        --     List.map
        --     labelLegendFor
        --     model.labels
        -- [ span [ class "label-0" ] [ text "label 0" ]
        -- ]
        viewLabelConfig : Html Msg
        viewLabelConfig =
            div []
                [ textarea [ cols 30, rows 6, value model.rawCategoryText, onInput UpdateCategoriesFromCategoryTextArea ] []
                ]
    in
    if model.showLabelConfig then
        div []
            [ button
                [ onClick ToggleLabelConfigSection ]
                [ text "Hide Label Configuration" ]
            , showLabelLegend
            , viewLabelConfig
            ]

    else
        div []
            [ button [ onClick ToggleLabelConfigSection ] [ text "Label Configuration" ]
            , showLabelLegend
            ]


viewImage : Int -> Int -> Image -> Html Msg
viewImage w h image =
    let
        viewLabel : Maybe Label -> Html Msg
        viewLabel mLabel =
            case mLabel of
                Just l ->
                    span
                        [ class ("label-" ++ String.fromInt l.index)
                        ]
                        [ text l.name ]

                Nothing ->
                    span [] []
    in
    span
        [ width w
        , height h
        , class (cssClass image.label)
        ]
        [ img
            [ src (imageUrl image.domain)
            , width w
            , height h
            , class (cssClass image.label)
            , onClick (LabelChange image)
            ]
            []
        , viewLabel image.label
        ]


cssClass : Maybe Label -> String
cssClass maybeLabel =
    case maybeLabel of
        Just l ->
            "label-" ++ String.fromInt l.index

        _ ->
            "unlabelled"



-- adjust for now having css capabilities
-- cssColor : Maybe Label -> String
-- cssColor maybeLabel =
--     let
--         colors =
--             [ "red", "green", "blue", "yellow", "purple", "orange" ]
--         colorAtIndex i =
--             case LE.getAt i colors of
--                 Just c ->
--                     c
--                 Nothing ->
--                     "error_color_455023"
--     in
--     case maybeLabel of
--         Just l ->
--             "label-" ++ colorAtIndex l.index
--         _ ->
--             ""


viewImages : Int -> Int -> List Image -> List (Html Msg)
viewImages width height images =
    map (viewImage width height) images



----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
-- data
----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------


someDomains : List String
someDomains =
    []



-- [ "deavervineyards.com"
-- , "douglashatch.com"
-- , "dk.com.pk"
-- , "brainfart55.com"
-- , "bereaworldmission.org"
-- , "anywherefest.com"
-- , "dadpowered.com"
-- , "beautifulworld.uk.com"
-- , "boatrax.com"
-- , "denigris1889.com"
-- , "copperzapper.com"
-- , "bwavejewelry.com"
-- , "cassavasite.com"
-- , "artofmagic.com"
-- , "bbblanc.com"
-- , "dynatecmigrate.info"
-- , "conceptfireplace.com"
-- , "bronzehorsemanbooks.com"
-- , "cp-e.com"
-- , "artcellarhouston.com"
-- , "chefsgarden.net"
-- , "desishops.com"
-- , "cheapjiujitsu.com"
-- , "crimsonlotusteas.com"
-- , "cumbrestoltec.org"
-- , "daytopraise.com"
-- ]


someImages : List Image
someImages =
    map (\d -> makeImage d) someDomains


makeImage : Domain -> Image
makeImage d =
    { domain = d, label = Nothing }



-- helper


dumpJsonLabelsAsPre : List Image -> Html Msg
dumpJsonLabelsAsPre images =
    pre []
        [ text (jsonLabels images |> String.concat) ]


save : Model -> Cmd Msg
save model =
    jsonLabels model.images
        |> String.concat
        |> download


loadLabels : Model -> Cmd Msg
loadLabels model =
    File.Select.file [ "*/" ] GotLabelFile


loadImageSet : Model -> Cmd Msg
loadImageSet model =
    File.Select.file [ "*/images" ] GotImageFile


loadDomains : Model -> Cmd Msg
loadDomains model =
    File.Select.file [ "*/list" ] GotDomainFile


saveImageSet : Model -> Cmd Msg
saveImageSet model =
    let
        imageSetTextFromImages : List Image -> String
        imageSetTextFromImages images =
            map (\image -> image.domain ++ "," ++ imageUrl image.domain ++ "\n") images |> String.concat
    in
    File.Download.string "some.images" "text/csv" (imageSetTextFromImages model.images)


download : String -> Cmd msg
download text =
    File.Download.string "some_labels.csv" "text/csv" text
