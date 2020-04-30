module Main where

import           Beckn.App
import           Beckn.App.Routes
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Proxy
import           Data.Swagger
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime (..), fromGregorian)
import           Data.Typeable              (Typeable)
import           Prelude
import           GHC.Generics
import           Servant
import           Servant.Swagger

main :: IO ()
main =
  -- writeSwaggerJSON
  runBecknBackendApp

todoSwagger :: Swagger
todoSwagger = toSwagger epassAPIs
  & info.title   .~ "Epass API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API document for beckn epas"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")
  & host ?~ "http://localhost:8012"

writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "swagger.json" (encodePretty todoSwagger)
