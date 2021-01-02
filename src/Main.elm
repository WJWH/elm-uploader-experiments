module Main exposing (..)

import Browser exposing (..)
import Debug
import File exposing (File, name)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Url exposing (Url)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { files : List File, extraOptionsExpanded : Bool, hover : Bool }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { files = [], extraOptionsExpanded = False, hover = False }, Cmd.none )


type Msg
    = ToggleExpandedOptions
    | WantMoreFiles
    | FilesSelected File (List File)
    | DragEnter
    | DragLeave


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleExpandedOptions ->
            ( { model | extraOptionsExpanded = not model.extraOptionsExpanded }, Cmd.none )

        WantMoreFiles ->
            ( model, Select.files [] FilesSelected )

        FilesSelected f fs ->
            ( { model | files = model.files ++ (f :: fs), hover = False }, Cmd.none )

        DragEnter ->
            ( { model | hover = True }, Cmd.none )

        DragLeave ->
            ( { model | hover = False }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div
        [ style "border"
            (if model.hover then
                "6px dashed purple"

             else
                "6px dashed #ccc"
            )
        , style "border-radius" "20px"
        , style "width" "480px"
        , style "height" "200px"
        , style "margin" "100px auto"
        , style "padding" "20px"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "center"
        , style "align-items" "center"
        , hijackOn "dragenter" (D.succeed DragEnter)
        , hijackOn "dragover" (D.succeed DragEnter)
        , hijackOn "dragleave" (D.succeed DragLeave)
        , hijackOn "drop" dropDecoder
        ]
        [ h1 []
            [ text "JAJAJA Ik ben geen designer ik weet het" ]

        -- File list, if any
        , viewFileList model

        -- Uploader button
        , br [] []
        , button [ onClick WantMoreFiles ] [ text "Add more files" ]

        -- extra options
        , viewExpandedOptions model
        ]


viewFileList : Model -> Html Msg
viewFileList model =
    if model.files == [] then
        text "No files selected yet"

    else
        div [] (List.concatMap (\x -> [ text (File.name x), br [] [] ]) model.files)


viewExpandedOptions : Model -> Html Msg
viewExpandedOptions model =
    if model.extraOptionsExpanded then
        div [ style "background-color" "lightblue" ]
            [ button [ onClick ToggleExpandedOptions ] [ text "expand extra options" ]
            , br [] []
            , text "Ik wil een link transfer"
            , input [ type_ "checkbox" ] []
            , br [] []
            , text "Receiver pays for bandwidth"
            , input [ type_ "checkbox" ] []
            ]

    else
        div []
            [ button [ onClick ToggleExpandedOptions ] [ text "expand extra options" ] ]



-- utility stuff


dropDecoder : D.Decoder Msg
dropDecoder =
    D.at [ "dataTransfer", "files" ] (D.oneOrMore FilesSelected File.decoder)


hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (D.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )
