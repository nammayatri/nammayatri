module Alchemist.Generator.Purs.API where

-- data TicketBookingRequest = TicketBookingRequest String TicketBookingReq

-- newtype TicketBookingReq = TicketBookingReq
--   { visitDate :: String,
--     services :: Array TicketService
--   }

-- newtype CreateOrderRes = CreateOrderRes --TODO:: Move to common
--   {
--     sdk_payload :: PaymentPagePayload,
--     id :: String,
--     order_id :: String,
--     payment_links :: PaymentLinks
--   }

-- instance makeTicketBookingRequest :: RestEndpoint TicketBookingRequest CreateOrderRes where
--   makeRequest reqBody@(TicketBookingRequest placeId (TicketBookingReq rqBody)) headers = defaultMakeRequest POST (EP.ticketPlaceBook placeId) headers reqBody Nothing
--   decodeResponse = decodeJSON
--   encodeRequest req = standardEncode req

-- derive instance genericTicketBookingReq:: Generic TicketBookingReq _
-- derive instance newtypeTicketBookingReq :: Newtype TicketBookingReq _
-- instance standardEncodeTicketBookingReq :: StandardEncode TicketBookingReq where standardEncode (TicketBookingReq req) = standardEncode req
-- instance showTicketBookingReq:: Show TicketBookingReq where show = genericShow
-- instance decodeTicketBookingReq :: Decode TicketBookingReq where decode = defaultDecode
-- instance encodeSTicketBookingReq :: Encode TicketBookingReq where encode = defaultEncode

-- derive instance genericTicketBookingRequest :: Generic TicketBookingRequest _
-- instance standardEncodeTicketBookingRequest :: StandardEncode TicketBookingRequest where standardEncode (TicketBookingRequest id body) = standardEncode body
-- instance showTicketBookingRequest :: Show TicketBookingRequest where show = genericShow
-- instance decodeTicketBookingRequest :: Decode TicketBookingRequest where decode = defaultDecode
-- instance encodeTicketBookingRequest  :: Encode TicketBookingRequest where encode = defaultEncode
