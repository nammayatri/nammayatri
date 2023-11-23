{-# LANGUAGE QuasiQuotes #-}

module Alchemist.App where

import Alchemist.DSL.Parser.API (apiParser)
import Alchemist.DSL.Parser.Storage
import Alchemist.Generator.Haskell.Servant (generateServantCode)
import Alchemist.Utils
import Kernel.Prelude
import Text.Parsec
import Text.RawString.QQ

processAPIDSL :: String -> String
processAPIDSL dsl = case parse apiParser "dsl-input" dsl of
  Left err -> "Parsing error: " ++ show err
  Right ast -> generateServantCode ast

dslInput :: String
dslInput =
  [r|POST /org/driver/services?*status:BookingStatus&limit:Int&offset:Int
  AUTH TokenAuth
  H mId Merchant
  REQ TicketBody
  RESP TicketServiceVerificationResp
|]

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

generateAPI :: IO ()
generateAPI = do
  let generatedCode = processAPIDSL dslInput
  writeToFile "GeneratedServantAPI.hs" generatedCode

parseStorage :: IO ()
parseStorage = do
  case parse parseStorageDSL "" dslStorageInput of
    Left err -> putStrLn $ "Error: " ++ show err
    Right tableDef -> print tableDef
