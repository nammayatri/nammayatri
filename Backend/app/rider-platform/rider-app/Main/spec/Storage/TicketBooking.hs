{-# LANGUAGE QuasiQuotes #-}

module Storage.TicketBooking where

import Alchemist.App
import Kernel.Prelude
import Text.RawString.QQ

dslStorageInput :: String
dslStorageInput =
  [r|Table "TicketService" "ticket_service"
  Field "id" "Id Domain.Types.Tickets.TicketService" PrimaryKey NotNull
  Field "placesId" "Text" PrimaryKey NotNull
  Field "service" "Text" SecondaryKey NotNull
  Field "maxVerification" "Int" "INT" NotNull Default "1"
  Field "openTimings" "Maybe TimeOfDay" "time without time zone" Default "CURRENT_TIMESTAMP"
  Field "closeTimings" "Maybe TimeOfDay" "time without time zone"
  Field "validityTimings" "Maybe TimeOfDay" "time without time zone"
|]

mkStorage :: String -> IO ()
mkStorage targetFolder = do
  mkBeamTable (targetFolder ++ "/Storage/Beam") dslStorageInput
  mkBeamQueries (targetFolder ++ "/Storage/Queries") dslStorageInput
  mkDomainType (targetFolder ++ "/Domain/Types") dslStorageInput
  mkSQLFile targetFolder dslStorageInput
