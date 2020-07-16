module Main where

import App
import App.Routes
import Control.Lens
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Proxy
import Data.Swagger
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import Data.Typeable (Typeable)
import GHC.Generics
import Servant
import Servant.Swagger
import Prelude

main :: IO ()
main = runAppBackend
