{-# LANGUAGE QuasiQuotes #-}

module Alchemist.App where

import Alchemist.DSL.Parser.API (apiParser)
import Alchemist.DSL.Parser.Storage
import Alchemist.Generator.Haskell.BeamQueries (generateTTypeInstances)
import Alchemist.Generator.Haskell.BeamTable (generateBeamTable)
import Alchemist.Generator.Haskell.DomainType (generateHaskellModule)
import Alchemist.Generator.Haskell.Servant (apisToText)
import Alchemist.Generator.SQL.Table (generateSQL)
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
  Field "openTimings" "Maybe TimeOfDay" "time without time zone" Default "CURRENT_TIMESTAMP"
  Field "closeTimings" "Maybe TimeOfDay" "time without time zone"
  Field "validityTimings" "Maybe TimeOfDay" "time without time zone"
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

generateStorage :: IO ()
generateStorage = do
  let tableDef = case parse parseStorageDSL "" dslStorageInput of
        Left err -> error (show err)
        Right tp -> tp
  writeToFile "./output/GeneratedBeamTable.hs" (generateBeamTable tableDef)
  writeToFile "./output/GeneratedBeamQueries.hs" (generateTTypeInstances tableDef)
  writeToFile "./output/GenerateHaskellModule.hs" (generateHaskellModule tableDef)
  writeToFile "./output/GenerateSQLTables.sql" (generateSQL tableDef)

generateAPI :: IO ()
generateAPI = do
  let generatedCode = processAPIDSL dslInput
  writeToFile "./output/GeneratedServantAPI.hs" generatedCode

generateAll :: IO ()
generateAll = do
  generateStorage
  generateAPI
