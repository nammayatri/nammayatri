INSERT INTO atlas_driver_offer_bpp.llm_prompt (
    merchant_id,
    merchant_operating_city_id,
    prompt_key,
    prompt_template,
    service_name,
    use_case
) VALUES (
    'favorit0-0000-0000-0000-00000favorit',
    'favorit0-0000-0000-0000-00000000city',
    'DriverProfileGen_1',
    'I am a ride-hailing app. Create a maximum 2 sentence profile for a driver based on the following details. Ignore empty  or Nothing values. Make it relevant and formal to show to riders:\nName: {#driverName#}\nDriving Since: {#withNY#} months\nRating: {#rating#}\nVehicle Tags: {#vehicleTags#}\nApplication Name: {#merchant#}\nHomeTown: {#homeTown#}\nAspirations: {#aspiration#}\nPledges: {#pledge#}\nonPlatformSince: {#createdAt#}\nVehicle Tags: {#vehicleTags#}',
    'LLMChatCompletion_AzureOpenAI',
    'DriverProfileGen'
);