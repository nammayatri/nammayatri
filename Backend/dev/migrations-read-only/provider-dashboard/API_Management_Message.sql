-- {"api":"PostMessageUploadFile","migration":"endpoint","param":"MessageAPI UploadFileEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MessageAPI_PostMessageUploadFileEndpoint'
  WHERE endpoint = 'MessageAPI UploadFileEndpoint';

-- {"api":"PostMessageUploadFile","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MessageAPI_PostMessageUploadFileEndpoint'
  WHERE endpoint = 'MessageAPI PostMessageUploadFileEndpoint';

-- {"api":"PostMessageAddLink","migration":"endpoint","param":"MessageAPI AddLinkEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MessageAPI_PostMessageAddLinkEndpoint'
  WHERE endpoint = 'MessageAPI AddLinkEndpoint';

-- {"api":"PostMessageAddLink","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MessageAPI_PostMessageAddLinkEndpoint'
  WHERE endpoint = 'MessageAPI PostMessageAddLinkEndpoint';

-- {"api":"PostMessageAdd","migration":"endpoint","param":"MessageAPI AddMessageEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MessageAPI_PostMessageAddEndpoint'
  WHERE endpoint = 'MessageAPI AddMessageEndpoint';

-- {"api":"PostMessageAdd","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MessageAPI_PostMessageAddEndpoint'
  WHERE endpoint = 'MessageAPI PostMessageAddEndpoint';

-- {"api":"PostMessageSend","migration":"endpoint","param":"MessageAPI SendMessageEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MessageAPI_PostMessageSendEndpoint'
  WHERE endpoint = 'MessageAPI SendMessageEndpoint';

-- {"api":"PostMessageSend","migration":"endpointV2","param":null,"schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'ProviderManagementAPI_MessageAPI_PostMessageSendEndpoint'
  WHERE endpoint = 'MessageAPI PostMessageSendEndpoint';
