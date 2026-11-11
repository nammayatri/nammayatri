-- {"api":"GetFRFSTicketServiceCustomerFrfsConfig","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFRFSTicketServiceCustomerFrfsAutocomplete","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFRFSTicketServiceCustomerFrfsRoutes","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFRFSTicketServiceCustomerFrfsStations","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostFRFSTicketServiceCustomerFrfsStationsPossibleStops","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFRFSTicketServiceCustomerFrfsRoute","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostFRFSTicketServiceCustomerFrfsSearch","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFRFSTicketServiceCustomerFrfsSearchQuote","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostFRFSTicketServiceCustomerFrfsQuoteV2Confirm","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFRFSTicketServiceCustomerFrfsBookingStatus","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostFRFSTicketServiceCustomerFrfsBookingCancel","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'city-operations.frfs.execute' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFRFSTicketServiceCustomerFrfsBookingReschedule","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id) VALUES ( '37947162-3b5d-4ed6-bcac-08841be1534d', 'city-operations.frfs.execute' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFRFSTicketServiceCustomerFrfsBookingPaymentAttempts","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFRFSTicketServiceCustomerFrfsPaymentAttempts","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFRFSTicketServiceCustomerFrfsRouteSeatLayout","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"GetFRFSTicketServiceCustomerFrfsTripRouteSeats","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostFRFSTicketServiceCustomerFrfsRouteServiceability","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostFRFSTicketServiceCustomerFrfsFleetOperatorTripAction","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.

-- {"api":"PostFRFSTicketServiceCustomerFrfsFleetOperatorCurrentOperation","migration":"localAccessForRoleId","param":"37947162-3b5d-4ed6-bcac-08841be1534d","schema":"atlas_dashboard"}
-- no capability declared (endpoint predates the capability framework); nothing to grant locally.
