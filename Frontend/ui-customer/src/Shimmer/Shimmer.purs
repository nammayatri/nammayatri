module Shimmer.Shimmer where

import Prelude


-- ticketBookingFlow = 
--     API.getPlaces 
--       >>= UI.showPlaceList 
--       >>= API.getPlaceServiceInfo
--       >>= UI.showPlacServiceeInfo
--       >>= UI.selectTickets
--       >>= API.bookTicket
--       >>= UI.showPaymentPage
--       >>= UI.showTicketQRCode <|> showPaymentFailed



-- showPlaceList :: PlaceInfoResponse -> Flow String Unit
-- showPlaceList resp = do
--   modifyState $ \s -> s { ticketState = state { placeInfo = Just resp } }
--   state <- getState
--   action <- runScreen $ UI.placeListScreen state
--   case action of
--     SelectPlace placeId -> Next placeId
--     s -> Exit s 

-- homeFlow = do
--   result <- UI.homeScreen
--   case result of
--     TicketSelection -> Redirect UI.showTicketSelection

-- combine :: FlowBT String Unit -> FlowBT String Unit -> FlowBT String Unit
-- combine action1 action2 = (/\) <$> action1 <*> action2

-- ticketBookingFlow :: Maybe String -> FlowBT String Unit
-- ticketBookingFlow mbPlaceId = do
--   placeId <- mbPlaceId ?<|> (API.getPlaces <||> UI.showPlaceList)
--   tickets <- API.getServiceInfo placeId <||> UI.showTicketSelection
--   void $ API.bookTickets tickets >>= callPaymentPage
--   combine API.getPaymentStatus API.getBookingDetails >>= UI.showStatus >>= UI.ticketQR

-- main = do

-- -- PlaceListScreen
-- -- PlaceDetailsScreen
-- -- TicketStatusScreen
-- -- TicketQRScreen


-- (<||>) :: FlowBT String Unit -> FlowBT String Unit -> FlowBT String Unit
-- (<||>) api ui = do
--   result <- race api (ui Nothing)
--   case result of
--     Left value -> ui $ Just value
--     Right value -> return value 

-- (?<|>) :: Maybe a -> FlowBT String Unit -> FlowBT String Unit
-- (?<|>) mb api = maybe api pure mb

-- ticketBookingFlow :: FlowBT String Unit
-- ticketBookingFlow = do
--   liftFlowBT $ hideLoader
--   modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen{data{dateOfVisit = (getNextDateV2 "")}})             
--   flow <- UI.ticketBookingScreen
--   case flow of
--     GO_TO_TICKET_PAYMENT state -> ticketPaymentFlow state.data
--     GO_TO_OPEN_GOOGLE_MAPS_FROM_ZOO_FLOW dstLat1 dstLon2  -> do
--       _ <- lift $ lift $ fork $ liftFlow $ openNavigation 0.0 0.0 dstLat1 dstLon2 "DRIVE"
--       ticketBookingFlow
--     GET_BOOKING_INFO_SCREEN state bookingStatus -> do
--       (TicketBookingDetails resp) <- Remote.getTicketBookingDetailsBT state.props.selectedBookingId--state.props.selectedBookingInfo.shortId (show state.props.selectedBookingInfo.status)
--       if bookingStatus == Pending
--         then do
--           modifyScreenState $ TicketBookingScreenStateType (\ticketBookingScreen -> ticketBookingScreen { props { currentStage = BookingConfirmationStage } })
--           setValueToLocalStore PAYMENT_STATUS_POOLING "true"
--           fillBookingDetails (TicketBookingDetails resp) state.props.selectedBookingId "Pending"
--           ticketBookingFlow
--         else do
--           let ticketBookingDetails = (ticketDetailsTransformer (TicketBookingDetails resp))
--           let dummyListItem = dummyServiceDetailsNON
--           modifyScreenState $ TicketInfoScreenStateType (\ticketInfoScreen ->  ticketInfoScreen{data{selectedBookingInfo = ticketBookingDetails}, props {activeListItem = fromMaybe dummyListItem (ticketBookingDetails.services !! 0), rightButtonDisable = (length ticketBookingDetails.services < 2)}})
--           zooTicketInfoFlow
--     GO_TO_HOME_SCREEN_FROM_TICKET_BOOKING state -> do
--       modifyScreenState $ TicketBookingScreenStateType (\_ ->  TicketBookingScreenData.initData)
--       if state.props.navigateToHome then homeScreenFlow else ticketingScreenFlow
--     RESET_SCREEN_STATE -> do
--       modifyScreenState $ TicketBookingScreenStateType (\_ ->  TicketBookingScreenData.initData)
--       ticketBookingFlow
--     REFRESH_PAYMENT_STATUS state -> do
--       (GetTicketStatusResp ticketStatus) <- Remote.getTicketStatusBT state.props.selectedBookingId
--       updatePaymentStatusData ticketStatus state.props.selectedBookingId
--       setValueToLocalStore PAYMENT_STATUS_POOLING "false"
--       ticketBookingFlow





