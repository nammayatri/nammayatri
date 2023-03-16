CREATE TABLE atlas_driver_offer_bpp.issue_category (
  id character varying(255) NOT NULL,
  category character varying(255) NOT NULL,
  logo_url character varying(255) NOT NULL,
  CONSTRAINT issue_category_pkey PRIMARY KEY (id)
);

INSERT INTO atlas_driver_offer_bpp.issue_category VALUES ('02d3fbe2-d5ad-4057-94e1-e84fde1ef3f6','lost and found','');
INSERT INTO atlas_driver_offer_bpp.issue_category VALUES ('5ca814d9-66e2-4ccc-b236-40b73b705e88','fare related','');
INSERT INTO atlas_driver_offer_bpp.issue_category VALUES ('a8ff40fb-88a0-440b-9763-e6c58929e5036','ride related','');
INSERT INTO atlas_driver_offer_bpp.issue_category VALUES ('3c2970e3-f01a-4dc3-8a21-3b77f01497ea','app related','');

CREATE TABLE atlas_driver_offer_bpp.issue_option (
  id character varying(255) NOT NULL,
  issue_category_id character varying(255) NOT NULL,
  option character varying(255) NOT NULL,
  CONSTRAINT issue_option_pkey PRIMARY KEY (id)
);

INSERT INTO atlas_driver_offer_bpp.issue_option VALUES ('489fa56f-d58e-4c56-8d84-bbe9ecd8ec00','02d3fbe2-d5ad-4057-94e1-e84fde1ef3f6','call the customer');
INSERT INTO atlas_driver_offer_bpp.issue_option VALUES ('169ae361-ab6e-4c77-b5ff-c898114d6b3c','02d3fbe2-d5ad-4057-94e1-e84fde1ef3f6','report lost item to support');
INSERT INTO atlas_driver_offer_bpp.issue_option VALUES ('528f0d86-a1c6-4f5c-90b5-fa7514e940ce','02d3fbe2-d5ad-4057-94e1-e84fde1ef3f6','talk to the support team');
INSERT INTO atlas_driver_offer_bpp.issue_option VALUES ('d8eee179-c34a-4d70-ba2e-1222962091cb','5ca814d9-66e2-4ccc-b236-40b73b705e88','customer paid less');
INSERT INTO atlas_driver_offer_bpp.issue_option VALUES ('16bf2a0d-229e-4afc-be9d-a06fb1bd0581','5ca814d9-66e2-4ccc-b236-40b73b705e88','customer paid more');
INSERT INTO atlas_driver_offer_bpp.issue_option VALUES ('9e8785e1-77d1-42c2-945e-15d398ed8e38','5ca814d9-66e2-4ccc-b236-40b73b705e88','poor customer behaviour');
INSERT INTO atlas_driver_offer_bpp.issue_option VALUES ('3d68dbe3-bc3a-44a0-939a-7ad166c4818b','5ca814d9-66e2-4ccc-b236-40b73b705e88','fare calculation error');
INSERT INTO atlas_driver_offer_bpp.issue_option VALUES ('5bed68d9-265c-4fb3-bdcc-22a4ed45de37','5ca814d9-66e2-4ccc-b236-40b73b705e88','other issue');
INSERT INTO atlas_driver_offer_bpp.issue_option VALUES ('208e90d4-63c0-4176-a91f-2a94fadfb53c','a8ff40fb-88a0-440b-9763-e6c58929e5036','pickup related issue');
INSERT INTO atlas_driver_offer_bpp.issue_option VALUES ('2d1c22eb-f576-4c9b-9d92-985d29149253','a8ff40fb-88a0-440b-9763-e6c58929e5036','poor customer behaviour');
INSERT INTO atlas_driver_offer_bpp.issue_option VALUES ('9bcf1fbd-0d32-48ea-a267-0de4f0ae3114','a8ff40fb-88a0-440b-9763-e6c58929e5036','location related issue');
INSERT INTO atlas_driver_offer_bpp.issue_option VALUES ('e9403adc-baaf-454f-ae75-20bb76f7a823','a8ff40fb-88a0-440b-9763-e6c58929e5036','talk to the support team');
INSERT INTO atlas_driver_offer_bpp.issue_option VALUES ('a9af6580-da16-438e-af5a-caaf4bce0d05','a8ff40fb-88a0-440b-9763-e6c58929e5036','other issue');
INSERT INTO atlas_driver_offer_bpp.issue_option VALUES ('8e23eecf-55d8-4a15-9cd5-670246f2e442','3c2970e3-f01a-4dc3-8a21-3b77f01497ea','OTP issues');
INSERT INTO atlas_driver_offer_bpp.issue_option VALUES ('251a4cd0-efda-4d43-94b7-ebd6f7164c10','3c2970e3-f01a-4dc3-8a21-3b77f01497ea','app not working properly');
INSERT INTO atlas_driver_offer_bpp.issue_option VALUES ('0028272a-5019-4785-a153-ae463f0bd1a3','3c2970e3-f01a-4dc3-8a21-3b77f01497ea','location related issue');
INSERT INTO atlas_driver_offer_bpp.issue_option VALUES ('30052650-c9b6-4801-82a4-1fcb7c6bce75','3c2970e3-f01a-4dc3-8a21-3b77f01497ea','other issue');

CREATE TABLE atlas_driver_offer_bpp.issue_translation (
  id character varying(255) NOT NULL,
  sentence character varying(255) NOT NULL,
  translation character varying(255) NOT NULL,
  language character varying(255) NOT NULL,
  CONSTRAINT issue_translation_pkey PRIMARY KEY (id)
);

INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('cb1f174f-f69a-4298-95f6-27a39a83dc9a','lost and found','lost and found','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('749a0bba-b798-4f8f-8156-76314be66123','lost and found','ಕಳೆದುಹೋದ ವಸ್ತುಗಳು ಮತ್ತು ಕಂಡುಬಂದಿವೆ','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('f5617307-5c20-4c3d-93ac-e223ed782a8e','lost and found','தொலைந்து போனது','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('aaae0b41-2a33-458a-b346-954e9308b950','fare related','fare related','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('99bad7be-f261-42e7-bc55-f70a78d0040d','fare related','ಶುಲ್ಕ ಸಂಬಂಧಿಸಿದಂತೆ','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('75736812-ffb3-46d3-9016-675fc584b881','fare related','கட்டணம் தொடர்பானது','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('2b3343d0-4197-40f5-a7a9-80ce494f9c11','ride related','ride related','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('7074aa4c-8ce1-4af0-98e9-97fb98b2f205','ride related','ರೈಡ್ ಸಂಬಂಧಿಸಿದಂತೆ','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('18683bbb-5d7d-4d42-a1b3-3899c48df6f3','ride related','சவாரி தொடர்பானது','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('94718e87-cdad-4224-95c4-cc8093b174d5','app related','app related','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('a7b5e46d-9698-4b78-a277-dca064d377c3','app related','ಅಪ್ಲಿಕೇಶನ್ಗೆ ಸಂಬಂಧಿಸಿದಂತೆ','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('ad542665-1c7b-4033-824a-6d485a722536','app related','ஆப் தொடர்பானது','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('0a9ac759-9c64-4314-89f7-e80392d9859b','call the customer','call the customer','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('04fdd533-54c5-45fc-bd65-300680b1be89','call the customer','ಗ್ರಾಹಕರಿಗೆ ಕರೆ ಮಾಡಿ','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('c659e8ab-a7df-4302-b838-d98badc5fd94','call the customer','வாடிக்கையாளரை அழைக்கவும்','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('dfc7c0b3-2452-4a8a-b765-4a4df6660211','report lost item to support','report lost item to support','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('013679dd-9453-4f31-b18d-d53e23fd6b09','report lost item to support','ನಮ್ಮ ಯಾತ್ರಿ ಬೆಂಬಲಕ್ಕೆ ಕಳೆದುಹೋದವಸ್ತುಗಳನ್ನು ವರದಿ ಮಾಡಿ','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('763f55c3-f14e-4d35-b159-07b5a0b30faf','report lost item to support','இழந்த உருப்படியை ஆதரவிற்குப் புகாரளிக்கவும்','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('6d077c30-13dc-4035-806a-8d13be80e3ee','talk to the support team','talk to the support team','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('7924e837-807c-4e5f-a5da-e39b7464988c','talk to the support team','ನಮ್ಮ ಯಾತ್ರಿ ಬೆಂಬಲದ ತಂಡದೊಂದಿಗೆ ಮಾತನಾಡಿ','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('3c1c5d21-5008-4a81-a6ca-093f425aa662','talk to the support team','ஆதரவுக் குழுவுடன் பேசுங்கள்','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('082e4033-e3e4-42ef-bb5b-8675ae8b7569','customer paid less','customer paid less','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('07523f0f-6958-4938-a45b-2b521a9dc54f','customer paid less','ಗ್ರಾಹಕರು ಕಡಿಮೆ ಪಾವತಿಸಿದ್ದಾರೆ','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('77068ed3-c35a-44ef-bec1-4590b13777e7','customer paid less','வாடிக்கையாளர் குறைவாக செலுத்தினார்','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('33db7471-d040-452e-b1ab-536e9b699768','customer paid more','customer paid more','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('a5c893f5-ac4b-46f2-a7bd-3e6002014628','customer paid more','ಗ್ರಾಹಕರು ಹೆಚ್ಚು ಪಾವತಿಸಿದ್ದಾರೆ','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('c851e467-4b12-4df5-8464-7de7dc76473b','customer paid more','வாடிக்கையாளர் அதிக கட்டணம் செலுத்தினார்','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('aa313eef-7e52-4c32-87cb-0171ec445540','poor customer behaviour','poor customer behaviour','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('329ad810-84b5-44d6-9af4-c016561cfdad','poor customer behaviour','ಗ್ರಾಹಕರ ಅನಿಯಮಿತ ನಡವಳಿಕೆ','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('bb9dfece-e9d2-4f27-8f9a-f25668781fa7','poor customer behaviour','மோசமான வாடிக்கையாளர் நடத்தை','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('59b1b54d-75b8-47b4-a998-c9c3e26d00f6','fare calculation error','fare calculation error','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('31e1c2f4-484e-49e4-9991-5b74c2401de3','fare calculation error','ದರ ಲೆಕ್ಕಾಚಾರ ತಪ್ಪಾಗಿರುವುದು','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('26ac280e-c8f9-4d3e-b72f-a5631f7f4181','fare calculation error','கட்டண கணக்கீடு பிழை','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('b07a9119-c76f-42f2-8209-68fb00b875ee','other issue','other issue','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('6ef81a61-8114-4bb0-ad50-e0c2eed124f8','other issue','ಇತರೆ ಸಮಸ್ಯೆ','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('a70d7091-8069-42dc-93ef-63d46b32e370','other issue','மற்ற பிரச்சினை','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('d8d03fb2-afe8-4741-bd03-1476088cbb3d','pickup related issue','pickup related issue','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('1538b58d-bb9b-4a9d-b3e3-5313f32704ef','pickup related issue','ಪಿಕಪ್ ಸಂಬಂಧಿತ ಸಮಸ್ಯೆ','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('9c81944e-ca9c-4234-9bd1-705cec31fcd9','pickup related issue','பிக்கப் தொடர்பான பிரச்சனை','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('22f35de8-20c6-4646-858f-fdc3e192afaa','location related issue','location related issue','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('15b044c7-1050-4af2-8f02-e2eee5a36012','location related issue','ಸ್ಥಳ ಸಂಬಂಧಿತ ಸಮಸ್ಯೆ','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('5ccd51ca-0d9e-43c2-964e-d6b0f817df79','location related issue','இடம் தொடர்பான பிரச்சினை','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('97ca0755-3c00-4cea-b885-d8d920524d9f','OTP issues','OTP issues','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('30ae39e2-c167-4ee0-83a2-1000cd7e4cf8','OTP issues','OTP ಸಮಸ್ಯೆಗಳು','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('8d02944f-c208-411c-ab6a-e1c8a7f0491f','OTP issues','OTP பிரச்சினைகள்','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('6cbefba6-1dfc-4c47-b876-ffb961579be4','app not working properly','app not working properly','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('87016b8b-2fce-496e-b345-ad66b531965c','app not working properly','ಅಪ್ಲಿಕೇಶನ್ ಸರಿಯಾಗಿ ಕಾರ್ಯನಿರ್ವಹಿಸುತ್ತಿಲ್ಲ','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('f494b182-3293-4918-b65b-ec1e9487e645','app not working properly','ஆப் சரியாக வேலை செய்யவில்லை','TAMIL');

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.issue_report (
  id character varying(255) PRIMARY KEY NOT NULL,
  driver_id character varying(255) NOT NULL REFERENCES atlas_driver_offer_bpp.person (id),
  ride_id character varying(255) REFERENCES atlas_driver_offer_bpp.ride (id),
  description character varying(255) NOT NULL,
  assignee character varying(255),
  status character varying(255) NOT NULL,
  category character varying(255) NOT NULL,
  option character varying(255),
  deleted boolean,
  media_files text[][],
  created_at timestamp NOT NULL,
  updated_at timestamp NOT NULL
);

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.comment (
  id character varying(255) PRIMARY KEY NOT NULL,
  issue_report_id character varying(255) NOT NULL REFERENCES atlas_driver_offer_bpp.issue_report (id),
  author character varying(255) NOT NULL,
  comment character varying(255) NOT NULL,
  created_at timestamp NOT NULL
);