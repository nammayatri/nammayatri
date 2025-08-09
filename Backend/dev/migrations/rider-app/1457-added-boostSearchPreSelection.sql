UPDATE atlas_app.rider_config
SET boost_search_pre_selection_service_tier_config = '[
    {
        "vehicle": "AUTO_RICKSHAW",
        "orderArray": ["AUTO_RICKSHAW","AMBULANCE_TAXI_OXY"]
    },
    {
        "vehicle": "TAXI",
        "orderArray": ["ECO", "TAXI"]
    }
]'::json;