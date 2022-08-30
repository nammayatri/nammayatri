module API.Types where

import Data.Text (Text)
import Servant

type API = Get '[JSON] Text
