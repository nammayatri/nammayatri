module Screens.DriverApp.Handler where

import Prelude
import PrestoDOM
import Helpers.Commons
import Effect
import Presto.Core.Types.Language.Flow
import Types
import Data.Maybe
import Effect.Uncurried

renderDriverApp :: Flow OverlayData Unit
renderDriverApp = do
  void $ liftFlow $ initUIWithNameSpace "DriverApp" Nothing
  void $ fork $ runScreenWithNameSpace $ screen { renderDriverApp: false }

type State
  = { renderDriverApp :: Boolean
    }

screen :: State -> ScopedScreen Action State ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "DriverApp"
  , globalEvents: []
  , parent: Just "DriverApp"
  , eval
  }

view :: forall w. (Action -> Effect Unit) -> State -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    ( if state.renderDriverApp then
        [ mapp "in.yatri.provider"
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , useStartApp true
            , payload $ getDriverAppPayload "payload"
            ]
        ]
      else
        []
    )

data ScreenOutput
  = Back State

data Action
  = RenderDriverApp

instance showAction :: Show Action where
  show _ = "RenderDriverApp"

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

eval :: Action -> State -> Eval Action ScreenOutput State
eval RenderDriverApp state = continue state { renderDriverApp = true }
