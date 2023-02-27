{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Metro.Search.Gps (Gps (..)) where

import Control.Arrow ((>>>))
import Data.Aeson
import Data.Aeson.Types (parseFail)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import EulerHS.Prelude hiding (many, try, (<|>))
import Kernel.Utils.Error.Throwing (fromEitherM')
import Kernel.Utils.Example
import Kernel.Utils.GenericPretty (PrettyShow)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

-- Regular expression: ^[-+]?([1-8]?\d(\.\d+)?|90(\.0+)?),\s*[-+]?(180(\.0+)?|((1[0-7]\d)|([1-9]?\d))(\.\d+)?)$

data Gps = Gps
  { lat :: Double,
    lon :: Double
  }
  deriving (Generic, Show, ToSchema, PrettyShow)

instance Example Gps where
  example =
    Gps
      { lat = 20.5937,
        lon = 78.9629
      }

instance FromJSON Gps where
  parseJSON =
    withText "Gps" $
      T.unpack
        >>> parse parseGps ""
        >>> fromEitherM' (parseFail . show)

instance ToJSON Gps where
  toJSON (Gps lat lon) = String $ show lat <> ", " <> show lon

parseGps :: Parser Gps
parseGps =
  Gps
    <$> (double >>= validate ((<= 90.0) . abs))
    <* char ','
    <* spaces
    <*> (double >>= validate ((<= 180.0) . abs))
    <* eof

type Parser = Parsec String ()

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser emptyDef

double :: Parser Double
double = P.float lexer

validate :: Show a => (a -> Bool) -> a -> Parser a
validate p a = if p a then pure a else unexpected (show a)
