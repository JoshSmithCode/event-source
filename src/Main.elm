module Main exposing (main)

import Browser
import Model exposing (Model, initialModel)
import Msg exposing (Msg)
import Update exposing (update)
import View exposing (view)




main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
