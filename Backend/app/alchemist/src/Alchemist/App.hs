{-# LANGUAGE QuasiQuotes #-}

module Alchemist.App where

import Alchemist.DSL.Parser.API (apiParser)
import Alchemist.DSL.Parser.Storage
import Alchemist.Generator.Haskell.Servant (apisToText)
import Alchemist.Utils
import Kernel.Prelude
import Text.Parsec
import Text.RawString.QQ

dslStorageInput :: String
dslStorageInput =
  [r|Table "TicketService" "ticket_service"
  Field "id" "Maybe (Id Domain.Types.TicketService.TicketService)" PrimaryKey NotNull
  Field "placesId" "Text" PrimaryKey NotNull
  Field "service" "Text"  SecondaryKey NotNull
  Field "maxVerification" "Int" "INT" NotNull Default "1"
  Field "openTimings" "Maybe TimeOfDay" "UTCTime" CustomConstraint "time without time zone" Default "CURRENT TIMESTAMP"
  Field "closeTimings" "Maybe TimeOfDay"
  Field "validityTimings" "Maybe TimeOfDay"
|]

parseStorage :: IO ()
parseStorage = do
  case parse parseStorageDSL "" dslStorageInput of
    Left err -> putStrLn $ "Error: " ++ show err
    Right tableDef -> print tableDef

processAPIDSL :: String -> String
processAPIDSL dsl = case apiParser dsl of
  Left err -> "Parsing error: " ++ show err
  Right ast -> apisToText ast

dslInput :: String
dslInput =
  [r|
  GET /ticket/places/{placeId:Id DTB.TicketPlace}
  AUTH TokenAuth
  Header vt {DTB.Vehicle}
  REQJ {Id DTB.Driver}
  RESPJ {DTB.TicketPlace}
  ---
  GET /ticket/places1/{placeId:Id DTB.TicketPlace}
  AUTH TokenAuth
  RESPJ {DTB.TicketPlace}
|]

generateAPI :: IO ()
generateAPI = do
  let generatedCode = processAPIDSL dslInput
  writeToFile "GeneratedServantAPI.hs" generatedCode
