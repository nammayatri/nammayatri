INSERT INTO atlas_driver_offer_bpp.merchant_service_config (
    merchant_id,
    merchant_operating_city_id,
    config_json,
    service_name
) VALUES (
    'favorit0-0000-0000-0000-00000favorit',
    'favorit0-0000-0000-0000-00000000city',
    '{
        "azureOpenAIChatCompletionUrl": "https://gateway-integration-south-india.openai.azure.com",
        "azureApiKey": "xxxxxxxxxxxx",
        "azureApiVersion": "2024-08-01-preview"
    }',
    'LLMChatCompletion_AzureOpenAI'
);