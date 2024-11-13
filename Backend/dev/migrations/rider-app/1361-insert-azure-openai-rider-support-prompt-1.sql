INSERT INTO atlas_app.llm_prompt (
    merchant_id,
    merchant_operating_city_id,
    prompt_key,
    prompt_template,
    service_name,
    use_case
) VALUES (
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    'AzureOpenAI_RiderSupport_1',
    'Categorize the given issue, {#description#}, with the one given in the provided issue categories list: {#categories#}. Provide ONLY (category name,category ID) WITHOUT QUOTES as result.',
    'LLMChatCompletion_AzureOpenAI',
    'RiderSupport'
);