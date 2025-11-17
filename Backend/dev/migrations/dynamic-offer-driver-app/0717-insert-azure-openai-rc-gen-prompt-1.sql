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
    'AzureOpenAI_RCExtraction_1',
    'Extract the vehicle registration certificate number from the following document text. Return ONLY valid JSON in this exact format: {"vehicleRegistrationCertNumber": "RC123456"}\n\nDocument Text:\n{#documentInfoText#}',
    'LLMChatCompletion_AzureOpenAI',
    'RCExtraction'
);