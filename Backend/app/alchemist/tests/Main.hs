{-# LANGUAGE QuasiQuotes #-}

module Main where

import Alchemist.App
import Kernel.Prelude
import System.Directory (createDirectoryIfMissing)
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

dslInput :: String
dslInput =
  [r|
  Module TicketService
  GET /ticket/places/{placeId:Id DTB.TicketPlace}/services?*status:Data.Types.BookingStatus&limit:Int&offset:Int
  AUTH TokenAuth
  Header vt {Domain.Action.UI.Tickets.Vehicle}
  REQJ {Id Domain.Action.UI.Tickets.Driver}
  RESPJ {Domain.Action.UI.Tickets.TicketPlace}
  ---
  GET /ticket/places1/{placeId:Id DTB.TicketPlace}
  AUTH TokenAuth
  RESPJ {Domain.Action.UI.Tickets.TicketPlace}
  ---
  GET /ticket/places2/{placeId:Id DTB.TicketPlace}
  AUTH TokenAuth
  RESPJ {Domain.Action.UI.Tickets.TicketPlace}
  ---
  GET /ticket/places3/{placeId:Id DTB.TicketPlace}
  AUTH TokenAuth
  RESPJ {Domain.Action.UI.Tickets.TicketPlace}
|]

generateAllExample :: IO ()
generateAllExample = do
  mapM_ (createDirectoryIfMissing True) ["./output/Beam", "./output/Queries", "./output/Domain/Type", "./output/SQL"]
  mkBeamTable "./output/Beam" dslStorageInput
  mkBeamQueries "./output/Queries" dslStorageInput
  mkDomainType "./output/Domain/Type" dslStorageInput
  mkSQLFile "./output/SQL" dslStorageInput
  mkServantAPI "./output" dslInput

main :: IO ()
main = generateAllExample
