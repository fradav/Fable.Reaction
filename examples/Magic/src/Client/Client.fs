module Client

//open Fable.React
//open Fable.React.Props
open Fetch.Types

open Fable.React
open Fable.Reaction
open FSharp.Control
open Feliz
open Feliz.Bulma

open Thoth.Fetch

type AppModel = {
    InitialString: string
    Info : Info.Model
}

type Model =
    | Loading
    | Error of string
    | App of AppModel


type Msg =
    | InitialLetterStringLoaded of Result<string, exn>
    | MagicMsg of Magic.Msg
    | InfoMsg of Info.Msg

let initialModel =
    Loading

let update model (msg : Msg) =
    match model, msg with
    | Loading, InitialLetterStringLoaded (Ok letterString) ->
        App {
            InitialString = letterString
            Info = Info.init letterString
        }
    | Loading, InitialLetterStringLoaded (Result.Error exn) ->
        Error exn.Message
    | _ -> model

let safeComponents =
    let components =
        Html.span [
            Html.a [ prop.href "https://github.com/giraffe-fsharp/Giraffe"; prop.text "Giraffe" ]
            Html.text ", "
            Html.a [ prop.href "http://fable.io"; prop.text "Fable" ]
            Html.text ", "
            Html.a [ prop.href "https://mangelmaxime.github.io/Fulma"; prop.text "Fulma" ]
            Html.text ", "
            Html.a [ prop.href "https://github.com/dbrattli/Fable.Reaction"; prop.text "Reaction" ]
        ]

    Html.p [
        Html.strong "SAFE Template"
        Html.text " powered by: "
        components
    ]


let viewApp model dispatch =
    match model with
    | Loading ->
        Html.div "Initial Values not loaded"

    | Error error ->
        Html.div ("Something went wrong: " + error)

    | App model ->
        Html.div [
            Magic.magic model.InitialString ()
            Info.info model.InitialString ()
        ]

let view (model : Model) (dispatch : Msg -> unit) =
    Html.div [
        Bulma.navbar [
            color.isPrimary
            prop.children [
                Bulma.levelItem [
                    Bulma.title.h2 "Fable Reaction Playground"
                ]
            ]
        ]

        Bulma.container [ viewApp model dispatch ]

        Bulma.footer [
            Bulma.content [
                prop.style [ style.textAlign.center ]
                prop.children [
                    safeComponents
                ]
            ]
        ]
    ]


let stream model msgs =
    match model with
    | Loading ->
        AsyncRx.ofPromise (Fetch.get<_,string>("/api/init"))
        |> AsyncRx.map (Ok >> InitialLetterStringLoaded)
        |> AsyncRx.catch (Result.Error >> InitialLetterStringLoaded >> AsyncRx.single)
        |> AsyncRx.tag "loading"

    | Error exn -> msgs |> AsyncRx.tag "error"
    | App model -> msgs |> AsyncRx.tag "msgs"

let app = Reaction.StreamView initialModel view update stream
mountById "reaction-app" (ofFunction app () [])
