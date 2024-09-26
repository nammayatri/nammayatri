-- {"api":"PostTicketServiceCreateKaptureTicket","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/TICKET_SERVICE/POST_TICKET_SERVICE_CREATE_KAPTURE_TICKET'
  WHERE endpoint = 'TicketServiceAPI PostTicketServiceCreateKaptureTicketEndpoint';
