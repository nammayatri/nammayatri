-- {"api":"PostMessageUploadFile","migration":"endpoint","param":"MessageAPI UploadFileEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_UPLOAD_FILE'
  WHERE endpoint = 'MessageAPI UploadFileEndpoint';

-- {"api":"PostMessageUploadFile","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_UPLOAD_FILE'
  WHERE endpoint = 'MessageAPI PostMessageUploadFileEndpoint';

-- {"api":"PostMessageAddLink","migration":"endpoint","param":"MessageAPI AddLinkEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_ADD_LINK'
  WHERE endpoint = 'MessageAPI AddLinkEndpoint';

-- {"api":"PostMessageAddLink","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_ADD_LINK'
  WHERE endpoint = 'MessageAPI PostMessageAddLinkEndpoint';

-- {"api":"PostMessageAdd","migration":"endpoint","param":"MessageAPI AddMessageEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_ADD'
  WHERE endpoint = 'MessageAPI AddMessageEndpoint';

-- {"api":"PostMessageAdd","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_ADD'
  WHERE endpoint = 'MessageAPI PostMessageAddEndpoint';

-- {"api":"PostMessageSend","migration":"endpoint","param":"MessageAPI SendMessageEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_SEND'
  WHERE endpoint = 'MessageAPI SendMessageEndpoint';

-- {"api":"PostMessageSend","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_SEND'
  WHERE endpoint = 'MessageAPI PostMessageSendEndpoint';
