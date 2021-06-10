module Beckn.Types.Core.Migration.Gps (Gps (..)) where

import Data.Aeson
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import EulerHS.Prelude hiding (many, try, (<|>))
import Text.Parsec

-- Regular expression: ^[-+]?([1-8]?\d(\.\d+)?|90(\.0+)?),\s*[-+]?(180(\.0+)?|((1[0-7]\d)|([1-9]?\d))(\.\d+)?)$

data Gps = Gps
  { lat :: Text,
    lon :: Text
  }
  deriving (Generic, Show)

instance FromJSON Gps where
  parseJSON = withText "Gps" \gps -> do
    let res = parse parseGps "" $ T.unpack gps
    either (\err -> parseFail $ "Error occurred while parsing Gps: " <> show err) pure res

instance ToJSON Gps where
  toJSON (Gps lat lon) = String $ lat <> ", " <> lon

parseGps :: Parsec String st Gps
parseGps = do
  (lat, lon) <-
    (,)
      <$> parseLatitude
      <* char ','
      <* many (space <|> endOfLine <|> tab)
      <*> parseLongitude
      <* eof
  pure $ Gps (T.pack lat) (T.pack lon)

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
