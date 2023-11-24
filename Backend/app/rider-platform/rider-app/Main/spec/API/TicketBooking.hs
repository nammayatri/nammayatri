{-# LANGUAGE QuasiQuotes #-}

module API.TicketBooking where

import Alchemist.App
import Kernel.Prelude
import Text.RawString.QQ

dslAPIInput :: String
dslAPIInput =
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

mkAPI :: String -> IO ()
mkAPI targetFolder = do
  mkServantAPI (targetFolder ++ "/API/UI") dslAPIInput
