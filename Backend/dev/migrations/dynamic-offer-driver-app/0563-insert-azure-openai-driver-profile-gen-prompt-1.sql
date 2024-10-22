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
    'AzureOpenAI_DriverProfileGen_1',
    'Write about me: \n\n-**Hometown**: I am from {#hometown#} , and growing up in this area has shaped my understanding of roads, safety, and community. \n- **Driving Experience**: I have been with Nammayatri for {#withNY#} months, learning valuable lessons along the way such as patience, focus, and safety. \n- **Driver Statistics**: {#driverStats#} \n- **Aspirations**: {#aspirations#} \n- **Pledge or Commitment**: My personal commitment as a driver is to always prioritize safety, respect other drivers, and maintain a calm and professional attitude on the road.',
    'LLMChatCompletion_AzureOpenAI',
    'DriverProfileGen'
);