{-# LANGUAGE QuasiQuotes #-}

module Main where

import Alchemist.App
import Kernel.Prelude
import System.Directory (createDirectoryIfMissing)
import Text.RawString.QQ

storageYamlFilePath :: FilePath
storageYamlFilePath = "./src/alchemist/tests/storage.yaml"

dslInput :: String
dslInput =
  [r|
  Module Tickets
  GET /ticket/places
  AUTH TokenAuth
  RESPJ {[Domain.Types.Tickets.TicketPlace]}
  ---
  GET /ticket/places/{placeId:(Id Domain.Types.Tickets.TicketPlace)}/services
  AUTH TokenAuth
  RESPJ {[Domain.Types.Tickets.TicketPlace]}
  ---
  POST /ticket/places/book
  AUTH TokenAuth
  REQJ {Domain.Types.Tickets.TicketBookingReq}
  RESPJ {Kernel.External.Payment.Interface.CreateOrderResp}
  ---
  GET /ticket/places/bookings?*status:Domain.Types.Tickets.BookingStatus&limit:Int&offset:Int
  AUTH TokenAuth
  RESPJ {[Domain.Types.Tickets.TicketBookingAPIEntity]}
  ---
  GET /ticket/places/bookings/{personServiceId:(Id Domain.Types.Tickets.TicketService)}/{ticketServiceShortId:(Id Domain.Types.Tickets.TicketBooking)}/details
  AUTH TokenAuth
  RESPJ {Domain.Types.Tickets.TicketBookingDetails}
  ---
  POST /ticket/places/bookings/{personServiceId:(Id Domain.Types.Tickets.TicketService)}/{ticketServiceShortId:(Id Domain.Types.Tickets.TicketBooking)}/verify
  AUTH TokenAuth
  RESPJ {Domain.Types.Tickets.TicketServiceVerificationResp}
  ---
  GET /ticket/places/bookings/{ticketServiceShortId:(Id Domain.Types.Tickets.TicketBooking)}/status
  AUTH TokenAuth
  RESPJ {Domain.Types.Tickets.BookingStatus}
|]

generateAllExample :: IO ()
generateAllExample = do
  mapM_ (createDirectoryIfMissing True) ["./output/Beam", "./output/Queries", "./output/Domain/Type", "./output/SQL"]
  mkBeamTable "./output/Beam" storageYamlFilePath
  mkBeamQueries "./output/Queries" storageYamlFilePath
  mkDomainType "./output/Domain/Type" storageYamlFilePath
  mkSQLFile "./output/SQL" storageYamlFilePath
  mkServantAPI "./output" dslInput
  mkDomainHandler "./output/Domain" dslInput

main :: IO ()
main = pure ()
