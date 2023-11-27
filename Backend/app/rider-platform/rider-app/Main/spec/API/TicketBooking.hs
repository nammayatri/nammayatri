{-# LANGUAGE QuasiQuotes #-}

module API.TicketBooking where

import Alchemist.App
import Kernel.Prelude
import Text.RawString.QQ

dslAPIInput :: String
dslAPIInput =
  [r|
  Module TicketService
  GET /ticket/places/{placeId:Id Domain.Types.Tickets.TicketPlace}/services?*status:Domain.Types.Tickets.TicketBooking.BookingStatus&limit:Int&offset:Int
  AUTH TokenAuth
  REQJ {Domain.Types.Tickets.TicketPlace}
  RESPJ {Domain.Types.Tickets.TicketPlace}
  ---
  GET /ticket/places1/{placeId:Id Domain.Types.Tickets.TicketPlace}
  AUTH TokenAuth
  RESPJ {Domain.Types.Tickets.TicketPlace}
  ---
  GET /ticket/places2/{placeId:Id Domain.Types.Tickets.TicketPlace}
  AUTH TokenAuth
  RESPJ {Domain.Types.Tickets.TicketPlace}
  ---
  GET /ticket/places3/{placeId:Id Domain.Types.Tickets.TicketPlace}
  AUTH TokenAuth
  RESPJ {Domain.Types.Tickets.TicketPlace}
|]

mkAPI :: String -> IO ()
mkAPI targetFolder = do
  mkServantAPI (targetFolder ++ "/API/UI") dslAPIInput
  mkDomainHandler (targetFolder ++ "/Domain/Action") dslAPIInput
