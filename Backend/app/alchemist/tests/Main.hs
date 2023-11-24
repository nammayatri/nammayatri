{-# LANGUAGE QuasiQuotes #-}

module Main where

import Alchemist.DSL.Parser.API (apiParser)
import Alchemist.DSL.Parser.Storage
import Alchemist.DSL.Syntax.API
import Alchemist.DSL.Syntax.Storage
import Alchemist.Generator.Haskell
import Alchemist.Generator.SQL
import Alchemist.Utils
import qualified Data.Text as T
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
  mkBeamTable "./output" dslStorageInput
  mkBeamQueries "./output" dslStorageInput
  mkDomainType "./output" dslStorageInput
  mkSQLFile "./output" dslStorageInput
  mkServantAPI "./output" dslInput
