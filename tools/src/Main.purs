module Main where
import Common
import Control.Monad.State as State
import Diagram as Diagram
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Utils as U
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

type MainState = { input :: String, link :: String, error :: String }

initialState :: MainState
initialState = { input: "", link: "", error: "" }

data Action
  = Initialize
  | SetInput String
  | UpdateLink

inputStorageKey :: String
inputStorageKey = "input"

mainUI :: forall m q. MonadEffect m => H.Component HH.HTML q Unit Void m
mainUI =
  H.mkComponent
  { initialState: \ _ -> initialState
  , render
  , eval: H.mkEval (H.defaultEval
                    { handleAction = handleAction
                    , initialize = Just Initialize })
  }
  where

  render :: MainState -> H.ComponentHTML Action () m
  render state =
    HH.main
    []
    [ HH.h1_
      [ HH.text "Diagram input tool" ]
    , HH.textarea
      [ HP.id_ "input"
      , HP.value state.input
      , HE.onBlur (\_ -> Just UpdateLink)
      , HE.onValueInput (\input -> Just (SetInput input)) ]
    , if state.error == ""
      then
        HH.a
        [ HP.href (".." <> state.link) ]
        [ HH.text "[Show diagram]" ]
      else
        HH.div
        [ HP.class_ (HH.ClassName "error") ]
        [ HH.text if state.error == ""
                  then "\xa0"
                  else state.error]
    ]

  handleAction :: Action -> H.HalogenM MainState Action () Void m Unit
  handleAction = case _ of
    Initialize -> do
      input <- liftEffect (window
                           >>= Window.localStorage
                           >>= Storage.getItem inputStorageKey)
      State.modify_ \state -> state { input = fromMaybe defaultInput input }
      handleAction UpdateLink
    SetInput input -> do
      State.modify_ \state -> state { input = input }
      liftEffect (window
                  >>= Window.localStorage
                  >>= Storage.setItem inputStorageKey input)
    UpdateLink -> do
      state <- State.get
      case Diagram.parseSubdiagrams state.input >>=
           Diagram.constructSubdiagrams of
        Left error -> do
          State.put state
            { error = if error == ""
                      then "unspecified error"
                      else error }
        Right diagram -> do
          State.put do
            case Diagram.snapshotHash (Diagram.newFrozenSnapshot diagram) of
              Just link -> state { error = "", link = link }
              Nothing -> state { error = "snapshotHash failed", link = "" }

  defaultInput =
    U.dropFirstChar """
# syntax:
#
#   w3j|wet <j> <j> <j>
#   rec|rel <j> <j>
#
# where <j> = <j-var> | <j> ± <j> | ±<j>
"""

main :: Effect Unit
main = HA.runHalogenAff (HA.awaitBody >>= runUI mainUI unit)
