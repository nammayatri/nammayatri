INSERT INTO
  atlas_driver_offer_bpp.merchant_overlay
    (id,
    merchant_id,
    language,
    overlay_key,
    udf1,
    image_url,
    title,
    description,
    ok_button_text,
    cancel_button_text,
    req_body,
    end_point,
    method,
    delay,
    contact_support_number,
    toast_message,
    secondary_actions,
    social_media_links,
    show_push_notification,
    merchant_operating_city_id
    )
    (select
    atlas_driver_offer_bpp.uuid_generate_v4() as id,
    merchant_id,
    'ENGLISH' as language,
    'AC_USAGE_RESTRICTED' as overlay_key,
    null as udf1,
    'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_save_more' as image_url,
    'Downgraded to Non-AC' as title,
    'Downgraded to only Non-AC rides due to multiple customer complaints.' as description,
    'View Booking Preferences' as ok_button_text,
    'Maybe Later' as cancel_button_text,
    json_build_object() as req_body,
    null as end_point,
    null as method,
    0 as delay,
    null as contact_support_number,
    null as toast_message,
    null as secondary_actions,
    null as social_media_links,
    true as show_push_notification,
    id as merchant_operating_city_id
    from atlas_driver_offer_bpp.merchant_operating_city
    );

INSERT INTO
  atlas_driver_offer_bpp.merchant_overlay
    (id,
    merchant_id,
    language,
    overlay_key,
    udf1,
    image_url,
    title,
    description,
    ok_button_text,
    cancel_button_text,
    req_body,
    end_point,
    method,
    delay,
    contact_support_number,
    toast_message,
    secondary_actions,
    social_media_links,
    show_push_notification,
    merchant_operating_city_id
    )
    (select
    atlas_driver_offer_bpp.uuid_generate_v4() as id,
    merchant_id,
    'ENGLISH' as language,
    'AC_USAGE_RESTRICTION_LIFTED' as overlay_key,
    null as udf1,
    'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_save_more' as image_url,
    'Upgraded to AC' as title,
    'Upgraded to AC rides.' as description,
    'View Booking Preferences' as ok_button_text,
    'Maybe Later' as cancel_button_text,
    json_build_object() as req_body,
    null as end_point,
    null as method,
    0 as delay,
    null as contact_support_number,
    null as toast_message,
    null as secondary_actions,
    null as social_media_links,
    true as show_push_notification,
    id as merchant_operating_city_id
    from atlas_driver_offer_bpp.merchant_operating_city
    );

INSERT INTO
  atlas_driver_offer_bpp.merchant_overlay
    (id,
    merchant_id,
    language,
    overlay_key,
    udf1,
    image_url,
    title,
    description,
    ok_button_text,
    cancel_button_text,
    req_body,
    end_point,
    method,
    delay,
    contact_support_number,
    toast_message,
    secondary_actions,
    social_media_links,
    show_push_notification,
    merchant_operating_city_id
    )
    (select
    atlas_driver_offer_bpp.uuid_generate_v4() as id,
    merchant_id,
    'ENGLISH' as language,
    'AC_USAGE_WARNING' as overlay_key,
    null as udf1,
    'https://raw.githubusercontent.com/witcher-shailesh/github-asset-store/main/uploads/img-ny_ic_ac_warning-1760509546890.jpg' as image_url,
    'Multiple AC-related complaints' as title,
    'You may be downgraded to Non-AC rides if this continues' as description,
    'Okay' as ok_button_text,
    '' as cancel_button_text,
    json_build_object() as req_body,
    null as end_point,
    null as method,
    0 as delay,
    null as contact_support_number,
    null as toast_message,
    null as secondary_actions,
    null as social_media_links,
    true as show_push_notification,
    id as merchant_operating_city_id
    from atlas_driver_offer_bpp.merchant_operating_city
    );

insert into atlas_driver_offer_bpp.translations(created_at, id, language, message, message_key, updated_at) VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4() ,'ENGLISH' , 'You have been downgraded to only Non-AC rides due to multiple customer complaints', 'AC_RESTRICTION_MESSAGE', now());
insert into atlas_driver_offer_bpp.translations(created_at, id, language, message, message_key, updated_at) VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4() ,'ENGLISH' , 'For uninterrupted service, please ensure that the AC is turned ON on AC rides', 'AC_WORKING_MESSAGE', now());