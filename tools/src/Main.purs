module Main where
import Common
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.State as State
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window as Window
import DOM.WebStorage.Storage as Storage
import Diagram as Diagram
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type HE' eff = HA.HalogenEffects (console :: CONSOLE, dom :: DOM | eff)

type MainState = { input :: String, link :: String, error :: String }

initialState :: MainState
initialState = { input: "", link: "#", error: "" }

data Query a
  = UpdateLink a
  | ValueInput String a
  | Initialize a

inputStorageKey :: String
inputStorageKey = "input"

mainUI :: forall eff. H.Component HH.HTML Query Unit Void (Aff (HE' eff))
mainUI =
  H.lifecycleComponent
  { initialState: \ _ -> initialState
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Nothing
  , receiver: \ _ -> Nothing
  }
  where

  render :: MainState -> H.ComponentHTML Query
  render state =
    HH.main
    []
    [ HH.textarea
      [ HP.id_ "input"
      , HP.value state.input
      , HE.onValueInput (HE.input ValueInput) ]
    , HH.a
      [ HP.href ("https://rufflewind.com/jucys" <> state.link) ]
      [ HH.text state.link ]
    , HH.div
      [ HP.class_ (HH.ClassName "error") ]
      [ HH.text state.error ]
    ]

  eval :: Query ~> H.ComponentDSL MainState Query Void (Aff (HE' eff))
  eval = case _ of
    Initialize next -> do
      input <- liftEff (window
                        >>= Window.localStorage
                        >>= Storage.getItem inputStorageKey)
      State.modify \state -> state { input = fromMaybe defaultInput input }
      eval (UpdateLink next)
    ValueInput input next -> do
      State.modify \state -> state { input = input }
      liftEff (window
               >>= Window.localStorage
               >>= Storage.setItem inputStorageKey input)
      eval (UpdateLink next)
    UpdateLink next -> do
      state <- State.get
      result <- liftEff (Diagram.encodeDiagram state.input)
      case result of
        Left error -> State.put state { error = if error == ""
                                                then "unknown error"
                                                else error }
        Right link -> State.put state { link = link, error = "" }
      pure next

  defaultInput =
    """# syntax: w3j|wet|cgp|cgm <j1> <j2> <j3>\n"""

main :: Eff (HE' ()) Unit
main = HA.runHalogenAff (HA.awaitBody >>= runUI mainUI unit)

_main :: Unit
_main = unsafePerformEff main
