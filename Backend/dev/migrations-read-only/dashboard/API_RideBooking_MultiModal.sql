-- {"api":"PostMultiModalCustomerMultimodalOrderSoftCancel","migration":"capability","param":"city-operations.multimodal.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.multimodal.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/MULTI_MODAL/POST_MULTI_MODAL_CUSTOMER_MULTIMODAL_ORDER_SOFT_CANCEL' ) ON CONFLICT DO NOTHING;

-- {"api":"GetMultiModalCustomerMultimodalOrderCancelStatus","migration":"capability","param":"city-operations.multimodal.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.multimodal.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/MULTI_MODAL/GET_MULTI_MODAL_CUSTOMER_MULTIMODAL_ORDER_CANCEL_STATUS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMultiModalCustomerMultimodalOrderCancel","migration":"capability","param":"city-operations.multimodal.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.multimodal.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/MULTI_MODAL/POST_MULTI_MODAL_CUSTOMER_MULTIMODAL_ORDER_CANCEL' ) ON CONFLICT DO NOTHING;

-- {"api":"PostMultiModalCustomerMultimodalOrderReschedule","migration":"capability","param":"city-operations.multimodal.execute","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.multimodal.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/MULTI_MODAL/POST_MULTI_MODAL_CUSTOMER_MULTIMODAL_ORDER_RESCHEDULE' ) ON CONFLICT DO NOTHING;
