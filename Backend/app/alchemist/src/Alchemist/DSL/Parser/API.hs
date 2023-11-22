module Alchemist.DSL.Parser.API where

import Alchemist.DSL.Syntax.API
import Kernel.Prelude
import Text.Parsec
import Text.Parsec.String (Parser)

-- Parses an HTTP method
httpMethodParser :: Parser HttpMethod
httpMethodParser =
  (string "GET" >> return GET)
    <|> (string "POST" >> return POST)

-- Add other HTTP methods as needed

pathParser :: Parser String
pathParser = many1 (noneOf " \n")

authParser :: Parser AuthType
authParser = do
  _ <- string "AUTH"
  spaces
  (string "TokenAuth" >> return TokenAuth)

headerParser :: Parser Header
headerParser = do
  _ <- string "H"
  spaces
  headerName <- many1 alphaNum
  spaces
  headerType <- many1 alphaNum
  return $ Header headerName (DataType headerType)

reqRespParser :: String -> Parser String
reqRespParser prefix = do
  _ <- string prefix
  spaces
  many1 alphaNum

apiEndpointParser :: Parser APIEndpoint
apiEndpointParser = do
  method <- httpMethodParser
  spaces
  pth <- pathParser
  spaces
  auth <- optionMaybe authParser
  spaces
  hdrs <- many headerParser
  spaces
  reqType <- optionMaybe (reqRespParser "REQ")
  spaces
  respType <- reqRespParser "RESP"
  return $ APIEndpoint method pth auth hdrs (DataType <$> reqType) (DataType respType)

apiParser :: Parser [APIEndpoint]
apiParser = apiEndpointParser `sepBy` string "---"
