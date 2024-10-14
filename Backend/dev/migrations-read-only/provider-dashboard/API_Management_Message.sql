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


------- SQL updates -------

-- {"api":"PostMessageUploadFile","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MESSAGE UPLOAD_FILE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_UPLOAD_FILE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MESSAGE' AND T1.user_action_type = 'UPLOAD_FILE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMessageAddLink","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MESSAGE ADD_LINK","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_ADD_LINK' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MESSAGE' AND T1.user_action_type = 'ADD_LINK' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMessageAdd","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MESSAGE ADD_MESSAGE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_ADD' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MESSAGE' AND T1.user_action_type = 'ADD_MESSAGE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMessageSend","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MESSAGE SEND_MESSAGE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_SEND' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MESSAGE' AND T1.user_action_type = 'SEND_MESSAGE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetMessageList","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MESSAGE MESSAGE_LIST","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MESSAGE/GET_MESSAGE_LIST' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MESSAGE' AND T1.user_action_type = 'MESSAGE_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetMessageInfo","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MESSAGE MESSAGE_INFO","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MESSAGE/GET_MESSAGE_INFO' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MESSAGE' AND T1.user_action_type = 'MESSAGE_INFO' ) ON CONFLICT DO NOTHING;

-- {"api":"GetMessageDeliveryInfo","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MESSAGE MESSAGE_DELIVERY_INFO","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MESSAGE/GET_MESSAGE_DELIVERY_INFO' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MESSAGE' AND T1.user_action_type = 'MESSAGE_DELIVERY_INFO' ) ON CONFLICT DO NOTHING;

-- {"api":"GetMessageReceiverList","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP_MANAGEMENT MESSAGE MESSAGE_RECEIVER_LIST","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_MANAGEMENT/MESSAGE/GET_MESSAGE_RECEIVER_LIST' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'MESSAGE' AND T1.user_action_type = 'MESSAGE_RECEIVER_LIST' ) ON CONFLICT DO NOTHING;
