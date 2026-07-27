
module Components.Loader.Types where


import PrestoDOM (Length(..), Margin(..))
import Data.Maybe(Maybe(..))
import PrestoDOM.Animation as PrestoAnim


data DotLoaderAnimation = Shrink | Blink | Wave | ShrinkAndBlink
type DotConfig =
  { width :: Length
  , height :: Length
  , background :: String
  , cornerRadius :: Number
  , margin :: Margin
  , count :: Int
  , animation :: Maybe DotLoaderAnimation
  , alpha :: Maybe Number
  }

type AnimationConfig =
  { duration :: Int
  , initScale :: Number
  , finalScale :: Number
  , repeatMode :: Maybe PrestoAnim.RepeatMode
  , repeatCount :: Maybe PrestoAnim.RepeatCount
  , interpolator :: Maybe PrestoAnim.Interpolator
  , stagger :: Maybe Int
  }

defaultDotAnimation =
  { duration: 390
  , initScale: 0.4
  , finalScale: 1.0
  , repeatMode: Nothing
  , repeatCount: Nothing
  , interpolator: Nothing
  , stagger: Nothing
  }

defaultDotLoaderConfig =
  { width: V 6
  , height: V 6
  , background: "#000000"
  , cornerRadius: 3.0
  , margin: Margin 1 5 1 0
  , count: 3
  , alpha : Nothing
  , animation: Nothing
  }

