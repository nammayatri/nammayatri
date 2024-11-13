INSERT INTO atlas_app.merchant_service_config (
    merchant_id,
    merchant_operating_city_id,
    config_json,
    service_name
) VALUES (
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    '{
        "azureOpenAIChatCompletionUrl": "https://gateway-integration-south-india.openai.azure.com",
        "azureApiKey": "0.1.0|2|0skOWnsD9ZX8CwnIUf+52vkZhWxXWsV9k5C7M6iz0gbjmwKsi89braSBuMG+1LcyiUxJwN9yKgBYSR2Rhmqe+an6ZdgSrlqH/KB4/nuljyK/rOA=",
        "azureApiVersion": "2023-07-01-preview"
    }',
    'LLMChatCompletion_AzureOpenAI'
);