UPDATE atlas_app.rider_config
SET user_service_tier_order_config = '[
    {
        "vehicle": "AUTO_RICKSHAW",
        "orderArray": ["AUTO_RICKSHAW", "ECO", "TAXI", "COMFY", "SUV", "SUV_PLUS"]
    },
    {
        "vehicle": "TAXI",
        "orderArray": ["ECO", "TAXI", "AUTO_RICKSHAW", "COMFY", "SUV", "SUV_PLUS"]
    }
]'::json;