module Main exposing (main)

import Base
import Browser
import Browser.Navigation
import Html
import Html.Attributes
import Page.About
import Page.Blog0
import Page.Blog1
import Page.Blog2
import Page.Home
import Url
import Url.Builder
import Url.Parser exposing ((</>))



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type Page
    = Home
    | About
    | Blog Int
    | NotFound


type alias Model =
    { key : Browser.Navigation.Key
    , url : Url.Url
    , page : Page
    }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , url = url
      , page = toPage url
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Browser.Navigation.load href
                    )

        UrlChanged url ->
            ( { model | url = url, page = toPage url }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- ROUTE


route : Url.Parser.Parser (Page -> a) a
route =
    Url.Parser.oneOf
        [ Url.Parser.map Home Url.Parser.top
        , Url.Parser.map Home (Url.Parser.s Base.base)
        , Url.Parser.map About (Url.Parser.s Base.base </> Url.Parser.s "about")
        , Url.Parser.map Blog (Url.Parser.s Base.base </> Url.Parser.s "blog" </> Url.Parser.int)
        ]


toPage : Url.Url -> Page
toPage url =
    case Url.Parser.parse route url of
        Just answer ->
            answer

        Nothing ->
            NotFound



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "annaghi | elm-github-pages"
    , body =
        [ Html.text "The current URL is: "
        , Html.b [] [ Html.text (Url.toString model.url) ]
        , Html.ul []
            [ internalLinkView "/"
            , internalLinkView "/about"
            , internalLinkView "/blog/0"
            , internalLinkView "/blog/1"
            , internalLinkView "/blog/2"
            ]
        , Html.ul [] [ externalLinkView "https://github.com/annaghi/elm-github-pages" ]
        , Html.hr [] []
        , case model.page of
            Home ->
                Page.Home.view

            About ->
                Page.About.view

            Blog number ->
                blogView number

            NotFound ->
                notFoundView
        ]
    }


internalLinkView : String -> Html.Html msg
internalLinkView path =
    Html.li []
        [ Html.a
            [ Html.Attributes.href <|
                Url.Builder.absolute [ Base.base, String.dropLeft 1 path ] []
            ]
            [ Html.text path ]
        ]


externalLinkView : String -> Html.Html msg
externalLinkView href =
    Html.li []
        [ Html.a
            [ Html.Attributes.href href ]
            [ Html.text href ]
        ]


blogView : Int -> Html.Html msg
blogView number =
    case number of
        0 ->
            Page.Blog0.view

        1 ->
            Page.Blog1.view

        2 ->
            Page.Blog2.view

        _ ->
            notFoundView


notFoundView : Html.Html msg
notFoundView =
    Html.text "Not found"
