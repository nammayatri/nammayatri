module Beckn.Types.Core.Migration.Gps (Gps (..)) where

import Beckn.Utils.Example
import Data.Aeson
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import EulerHS.Prelude hiding (many, try, (<|>))
import Text.Parsec

-- Regular expression: ^[-+]?([1-8]?\d(\.\d+)?|90(\.0+)?),\s*[-+]?(180(\.0+)?|((1[0-7]\d)|([1-9]?\d))(\.\d+)?)$

data Gps = Gps
  { lat :: Double,
    lon :: Double
  }
  deriving (Generic, Show)

instance Example Gps where
  example =
    Gps
      { lat = 20.5937,
        lon = 78.9629
      }

instance FromJSON Gps where
  parseJSON = withText "Gps" \gps -> do
    let res = parse parseGps "" $ T.unpack gps
    (lat, lon) <- either (\err -> parseFail $ "Error occurred while parsing Gps: " <> show err) pure res
    latDouble <- maybe (parseFail "Couldn't parse latitude from Gps.") pure (readMaybe lat)
    lonDouble <- maybe (parseFail "Couldn't parse longitude from Gps.") pure (readMaybe lon)
    pure $ Gps latDouble lonDouble

instance ToJSON Gps where
  toJSON (Gps lat lon) = String $ show lat <> ", " <> show lon

parseGps :: Parsec String st (String, String)
parseGps =
  (,)
    <$> parseLatitude
    <* char ','
    <* many (space <|> endOfLine <|> tab)
    <*> parseLongitude
    <* eof

parseLatitude :: Parsec String st String
parseLatitude = do
  mbSign <- optionMaybe parseSign
  latitude <- try parseLessThanBound <|> parseBound
  pure $ ifJust mbSign (: latitude) latitude
  where
    parseLessThanBound = do
      firstDigit <- oneOf "12345678"
      secondDigit <- digit
      let integerPart = [firstDigit, secondDigit]
      mbRealPart <- optionMaybe parseRealPart
      pure $ ifJust mbRealPart (integerPart ++) integerPart
    parseBound = do
      integerPart <- string "90"
      mbRealPart <- optionMaybe parseDummyRealPart
      pure $ ifJust mbRealPart (integerPart ++) integerPart

parseLongitude :: Parsec String st String
parseLongitude = do
  mbSign <- optionMaybe parseSign
  longitude <- parseIntegerPart
  pure $ ifJust mbSign (: longitude) longitude
  where
    parseIntegerPart = try parseBound <|> try parseHundredAndMore <|> parseLessThanHundred
    parseBound = do
      integerPart <- string "180"
      mbRealPart <- optionMaybe parseDummyRealPart
      pure $ ifJust mbRealPart (integerPart ++) integerPart
    parseHundredAndMore = do
      firstDigit <- char '1'
      secondDigit <- oneOf "01234567"
      thirdDigit <- digit
      let integerPart = [firstDigit, secondDigit, thirdDigit]
      mbRealPart <- optionMaybe parseRealPart
      pure $ ifJust mbRealPart (integerPart ++) integerPart
    parseLessThanHundred = do
      mbTensDigit <- optionMaybe $ oneOf "123456789"
      unitsDigit <- digit
      let integerPart = ifJust mbTensDigit (: [unitsDigit]) [unitsDigit]
      mbRealPart <- optionMaybe parseRealPart
      pure $ ifJust mbRealPart (integerPart ++) integerPart

parseRealPart :: Parsec String st String
parseRealPart = do
  point <- char '.'
  realPart <- many1 digit
  pure $ point : realPart

parseDummyRealPart :: Parsec String st String
parseDummyRealPart = do
  point <- char '.'
  realPart <- many1 $ char '0'
  pure $ point : realPart

parseSign :: Parsec String st Char
parseSign = oneOf "+-"

ifJust :: Maybe a -> (a -> b) -> b -> b
ifJust Nothing _ val = val
ifJust (Just val) f _ = f val
