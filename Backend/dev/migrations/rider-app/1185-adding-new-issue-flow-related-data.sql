CREATE TABLE atlas_app.issue_category (
  id character varying(255) NOT NULL,
  category character varying(255) NOT NULL,
  logo_url character varying(255) NOT NULL,
  CONSTRAINT issue_category_pkey PRIMARY KEY (id)
);

CREATE TABLE atlas_app.issue_option (
  id character varying(255) NOT NULL,
  issue_category_id character varying(255),
  issue_message_id character varying(255) NOT NULL,
  option character varying(255) NOT NULL,
  label text,
  priority int NOT NULL,
  CONSTRAINT issue_option_pkey PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS atlas_app.issue_report (
  id character varying(255) PRIMARY KEY NOT NULL,
  person_id character varying(255) NOT NULL REFERENCES atlas_app.person (id),
  ride_id character varying(255) REFERENCES atlas_app.ride (id),
  description character varying(255) NOT NULL,
  assignee character varying(255),
  status character varying(255) NOT NULL,
  category_id character varying(255) NOT NULL REFERENCES atlas_app.issue_category(id),
  option_id character varying(255) REFERENCES atlas_app.issue_option(id),
  deleted boolean,
  media_files text[][],
  ticket_id character varying(255),
  chats text[],
  created_at timestamp NOT NULL,
  updated_at timestamp NOT NULL
);

CREATE TABLE IF NOT EXISTS atlas_app.comment (
  id character varying(255) PRIMARY KEY NOT NULL,
  issue_report_id character varying(255) NOT NULL REFERENCES atlas_app.issue_report (id),
  author_id character varying(255) NOT NULL,
  comment character varying(255) NOT NULL,
  created_at timestamp NOT NULL
);

CREATE TABLE atlas_app.issue_translation (
  id character varying(255) NOT NULL,
  sentence character varying(255) NOT NULL,
  translation character varying(255) NOT NULL,
  language character varying(255) NOT NULL,
  CONSTRAINT issue_translation_pkey PRIMARY KEY (id)
);

CREATE TABLE atlas_app.media_file (
  id character(36) NOT NULL,
  type character(36) NOT NULL,
  url text NOT NULL,
  created_at timestamp NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE atlas_app.issue_message (
  id character(36) NOT NULL PRIMARY KEY,
  option_id character(36) REFERENCES atlas_app.issue_option (id),
  category_id character(36) REFERENCES atlas_app.issue_category (id),
  message character varying(255) NOT NULL,
  label text,
  priority int NOT NULL
);

CREATE TABLE atlas_app.issue_config (
  id character(36) NOT NULL PRIMARY KEY,
  auto_mark_issue_resolve_duration double precision,
  on_auto_mark_issue_res_msgs text[],
  on_create_issue_msgs text[],
  on_issue_reopen_msgs text[],
  on_kapt_mark_issue_awt_msgs text[]
);

--MEDIAFILE CONFIGS TO MERCHANT TABLE
ALTER TABLE atlas_app.merchant ADD COLUMN media_file_url_pattern text DEFAULT 'http://localhost:8013/v2/<DOMAIN>/media?filePath=<FILE_PATH>' NOT NULL;
ALTER TABLE atlas_app.merchant ADD COLUMN media_file_size_upper_limit int DEFAULT 10000000 NOT NULL;

--ISSUE CONFIGS
INSERT INTO atlas_app.issue_config VALUES ('h56bjh3i-4n5b-34ut-bg8b3k3ggkq8', 2, ARRAY['qradsvna-c76f-42f2-8209-68fb00b875ef'],ARRAY['v31ghv31-1234-234f-fb2ds-34v2dfstf1j'], ARRAY['v31ghv31-1234-234f-fb2ds-34v2dfstf1j'], ARRAY['12m3n3ql-ch17-12cb-34hu-1h23ewdf112j', '123md312-ch17-u3tj-123d-febf223b12j3']);

--CATEGORY -> LOST AND FOUND
INSERT INTO atlas_app.issue_category VALUES ('02d3fbe2-d5ad-4057-94e1-e84fde1ef3f7','lost and found','ny_ic_lost_and_found,https://assets.juspay.in/nammayatri/images/common/ny_ic_lost_and_found.png');

INSERT INTO atlas_app.issue_message VALUES ('ab7a9119-c76f-42f2-8209-68fb00b875ef', null, '02d3fbe2-d5ad-4057-94e1-e84fde1ef3f7', 'Hey XYZ,We’re sorry to hear about your lost item', null,1);
INSERT INTO atlas_app.issue_message VALUES ('bc7a9119-c76f-42f2-8209-68fb00b875ef', null, '02d3fbe2-d5ad-4057-94e1-e84fde1ef3f7', 'How do you wish to resolve this issue?', null,2);

INSERT INTO atlas_app.issue_option VALUES ('489fa56f-d58e-4c56-8d84-bbe9ecd8ec01', '02d3fbe2-d5ad-4057-94e1-e84fde1ef3f7', 'bc7a9119-c76f-42f2-8209-68fb00b875ef', 'call the driver', 'CALL_DRIVER', 1);
INSERT INTO atlas_app.issue_option VALUES ('169ae361-ab6e-4c77-b5ff-c898114d6b3d', '02d3fbe2-d5ad-4057-94e1-e84fde1ef3f7', 'bc7a9119-c76f-42f2-8209-68fb00b875ef', 'report lost item to support', null, 2);

INSERT INTO atlas_app.issue_message VALUES ('asvdasgv-c76f-13bg-8209-q123vhve123v', '489fa56f-d58e-4c56-8d84-bbe9ecd8ec01', null, 'Please share more details on the lost item. You can also add images or voice notes for us to help you out better.', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('de7a9119-45jn-12v3-8209-68fb00b875ef', '169ae361-ab6e-4c77-b5ff-c898114d6b3d', null, 'Please share more details on the lost item. You can also add images or voice notes for us to help you out better.', null, 1);

--CATEGORY -> DRIVER RELATED
INSERT INTO atlas_app.issue_category VALUES ('5ca814d9-66e2-4ccc-b236-40b73b705e89','driver related','ny_ic_driver_related_issue,https://assets.juspay.in/nammayatri/images/common/ny_ic_driver_related_issue.png');

INSERT INTO atlas_app.issue_message VALUES ('kl7a9119-c76f-42f2-8209-68fb00b875ef', null, '5ca814d9-66e2-4ccc-b236-40b73b705e89', 'Hey XYZ, We’re really sorry to hear you have been facing driver related issues.', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('mn7a9119-c76f-42f2-8209-68fb00b875ef', null, '5ca814d9-66e2-4ccc-b236-40b73b705e89', 'Please select the type of issue you are facing so we can help you out better', null, 2);

INSERT INTO atlas_app.issue_option VALUES ('d8eee179-c34a-4d70-ba2e-1222962091ce', '5ca814d9-66e2-4ccc-b236-40b73b705e89', 'mn7a9119-c76f-42f2-8209-68fb00b875ef', 'poor driver behaviour', null, 1);
INSERT INTO atlas_app.issue_option VALUES ('16bf2a0d-229e-4afc-be9d-a06fb1bd0582', '5ca814d9-66e2-4ccc-b236-40b73b705e89', 'mn7a9119-c76f-42f2-8209-68fb00b875ef', 'safety concerns', null, 2);
INSERT INTO atlas_app.issue_option VALUES ('9e8785e1-77d1-42c2-945e-15d398ed8e39', '5ca814d9-66e2-4ccc-b236-40b73b705e89', 'mn7a9119-c76f-42f2-8209-68fb00b875ef', 'hygiene of the auto', null, 3);
INSERT INTO atlas_app.issue_option VALUES ('3d68dbe3-bc3a-44a0-939a-7ad166c4818c', '5ca814d9-66e2-4ccc-b236-40b73b705e89', 'mn7a9119-c76f-42f2-8209-68fb00b875ef', 'My issue is not listed here', null, 4);

INSERT INTO atlas_app.issue_message VALUES ('qr7a9119-c76f-42f2-8209-68fb00b875ef', 'd8eee179-c34a-4d70-ba2e-1222962091ce', null, 'Please select an option that best represents your issue', null, 1);

INSERT INTO atlas_app.issue_option VALUES ('8e23eecf-55d8-4a15-9cd5-670246f2e111', '5ca814d9-66e2-4ccc-b236-40b73b705e89', 'qr7a9119-c76f-42f2-8209-68fb00b875ef', 'Inappropriate Behaviour', null, 1);
INSERT INTO atlas_app.issue_option VALUES ('251a4cd0-efda-4d43-94b7-ebd6f7164112', '5ca814d9-66e2-4ccc-b236-40b73b705e89', 'qr7a9119-c76f-42f2-8209-68fb00b875ef', 'Abusive Language', null, 2);
INSERT INTO atlas_app.issue_option VALUES ('0028272a-5019-4785-a153-ae463f0bd113', '5ca814d9-66e2-4ccc-b236-40b73b705e89', 'qr7a9119-c76f-42f2-8209-68fb00b875ef', 'Threats or Violence', null, 3);
INSERT INTO atlas_app.issue_option VALUES ('30052650-c9b6-4801-82a4-1fcb7c6bc114', '5ca814d9-66e2-4ccc-b236-40b73b705e89', 'qr7a9119-c76f-42f2-8209-68fb00b875ef', 'My issue is not listed here', null, 4);

INSERT INTO atlas_app.issue_message VALUES ('op7a9119-24nj-42f2-8209-68fb00b875ef', '8e23eecf-55d8-4a15-9cd5-670246f2e111', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('op7a9119-c76f-236r-8209-asdv1jg3vrc2', '251a4cd0-efda-4d43-94b7-ebd6f7164112', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('op7a9119-534d-y4fe-8209-1231cv21yjg3', '0028272a-5019-4785-a153-ae463f0bd113', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('12g3jjqv-db2e-f42f-8209-12gy3cv1h3e5', '30052650-c9b6-4801-82a4-1fcb7c6bc114', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('21v3123d-12b3-324n-8209-234ncngnwsdf', '16bf2a0d-229e-4afc-be9d-a06fb1bd0582', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('12exv12e-56n2-6j5n-8209-45n6jngddfgn', '9e8785e1-77d1-42c2-945e-15d398ed8e39', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('op7a9119-c76f-42f2-8209-24rjs4nhjleg', '3d68dbe3-bc3a-44a0-939a-7ad166c4818c', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);

--CATEGORY -> RIDE RELATED
INSERT INTO atlas_app.issue_category VALUES ('a8ff40fb-88a0-440b-9763-e6c589295037','ride related','ny_ic_ride_related_issue,https://assets.juspay.in/nammayatri/images/common/ny_ic_ride_related_issue.png');

INSERT INTO atlas_app.issue_message VALUES ('ef7a9119-c76f-42f2-8209-68fb00b875ef', null, 'a8ff40fb-88a0-440b-9763-e6c589295037', 'Hey XYZ, We’re really sorry to hear you have been facing ride related issues.', null,1);
INSERT INTO atlas_app.issue_message VALUES ('gh7a9119-c76f-42f2-8209-68fb00b875ef', null, 'a8ff40fb-88a0-440b-9763-e6c589295037', 'Please select the type of issue you are facing so we can help you out better', null,2);

INSERT INTO atlas_app.issue_option VALUES ('208e90d4-63c0-4176-a91f-2a94fadfb53f','a8ff40fb-88a0-440b-9763-e6c589295037', 'gh7a9119-c76f-42f2-8209-68fb00b875ef', 'fare discrepancies', null, 1);
INSERT INTO atlas_app.issue_option VALUES ('2d1c22eb-f576-4c9b-9d92-985d29149254','a8ff40fb-88a0-440b-9763-e6c589295037', 'gh7a9119-c76f-42f2-8209-68fb00b875ef', 'longer route taken', null, 2);
INSERT INTO atlas_app.issue_option VALUES ('9bcf1fbd-0d32-48ea-a267-0de4f0ae3115','a8ff40fb-88a0-440b-9763-e6c589295037', 'gh7a9119-c76f-42f2-8209-68fb00b875ef', 'unable to contact the driver', null, 3);
INSERT INTO atlas_app.issue_option VALUES ('e9403adc-baaf-454f-ae75-20bb76f7a824','a8ff40fb-88a0-440b-9763-e6c589295037', 'gh7a9119-c76f-42f2-8209-68fb00b875ef', 'longer Pickup time', null, 4);
INSERT INTO atlas_app.issue_option VALUES ('a9af6580-da16-438e-af5a-caaf4bce0d06','a8ff40fb-88a0-440b-9763-e6c589295037', 'gh7a9119-c76f-42f2-8209-68fb00b875ef', 'My issue is not listed here', null, 5);

INSERT INTO atlas_app.issue_message VALUES ('ij7a9119-c76f-42f2-8209-68fb00b875ef', '208e90d4-63c0-4176-a91f-2a94fadfb53f', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('ij8a9119-c76f-42f2-8209-68fb00b875ef', '2d1c22eb-f576-4c9b-9d92-985d29149254', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('ij9a9119-c76f-42f2-8209-68fb00b875ef', '9bcf1fbd-0d32-48ea-a267-0de4f0ae3115', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('ij1a9119-c76f-42f2-8209-68fb00b875ef', 'e9403adc-baaf-454f-ae75-20bb76f7a824', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('ij3a9119-c76f-42f2-8209-68fb00b875ef', 'a9af6580-da16-438e-af5a-caaf4bce0d06', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);

--CATEGORY -> APP RELATED
INSERT INTO atlas_app.issue_category VALUES ('3c2970e3-f01a-4dc3-8a21-3b77f01497eb','app related','ny_ic_app_related_issue,https://assets.juspay.in/nammayatri/images/common/ny_ic_app_related_issue.png');

INSERT INTO atlas_app.issue_message VALUES ('kq7a9119-c76f-42f2-8209-68fb00b875ef', null, '3c2970e3-f01a-4dc3-8a21-3b77f01497eb', 'Hey XYZ, We’re really sorry to hear you have been facing app related issues.', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('ly7a9119-c76f-42f2-8209-68fb00b875ef', null, '3c2970e3-f01a-4dc3-8a21-3b77f01497eb', 'Please select the type of issue you are facing so we can help you out better', null, 2);

INSERT INTO atlas_app.issue_option VALUES ('8e23eecf-55d8-4a15-9cd5-670246f2e443','3c2970e3-f01a-4dc3-8a21-3b77f01497eb', 'ly7a9119-c76f-42f2-8209-68fb00b875ef', 'OTP Issues', null, 1);
INSERT INTO atlas_app.issue_option VALUES ('251a4cd0-efda-4d43-94b7-ebd6f7164c11','3c2970e3-f01a-4dc3-8a21-3b77f01497eb', 'ly7a9119-c76f-42f2-8209-68fb00b875ef', 'app not working properly', null, 2);
INSERT INTO atlas_app.issue_option VALUES ('0028272a-5019-4785-a153-ae463f0bd1a4','3c2970e3-f01a-4dc3-8a21-3b77f01497eb', 'ly7a9119-c76f-42f2-8209-68fb00b875ef', 'location related issue', null, 3);
INSERT INTO atlas_app.issue_option VALUES ('30052650-c9b6-4801-82a4-1fcb7c6bce76','3c2970e3-f01a-4dc3-8a21-3b77f01497eb', 'ly7a9119-c76f-42f2-8209-68fb00b875ef', 'other issue', null, 4);

INSERT INTO atlas_app.issue_message VALUES ('ze7a9119-c76f-42f2-8209-68fb00b875ef', '8e23eecf-55d8-4a15-9cd5-670246f2e443', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('ze8a9119-c76f-42f2-8209-68fb00b875ef', '251a4cd0-efda-4d43-94b7-ebd6f7164c11', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('ze9a9119-c76f-42f2-8209-68fb00b875ef', '0028272a-5019-4785-a153-ae463f0bd1a4', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('ze5a9119-c76f-42f2-8209-68fb00b875ef', '30052650-c9b6-4801-82a4-1fcb7c6bce76', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);

--TRANSLATIONS
INSERT INTO atlas_app.issue_translation VALUES ('cb1f174f-f69a-4298-95f6-27a39a83dc9a','lost and found','lost and found','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('749a0bba-b798-4f8f-8156-76314be66123','lost and found','ಕಳೆದುಹೋದ ವಸ್ತುಗಳು ಮತ್ತು ಕಂಡುಬಂದಿವೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('f5617307-5c20-4c3d-93ac-e223ed782a8e','lost and found','தொலைந்து போனது','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('x2i7ccru-dytu-s18e-wglh-a604devwdy8m','lost and found','खोया और पाया','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('aaae0b41-2a33-458a-b346-954e9308b950','driver related','Driver Related','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('99bad7be-f261-42e7-bc55-f70a78d0040d','driver related','ಶುಲ್ಕ ಸಂಬಂಧಿಸಿದಂತೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('75736812-ffb3-46d3-9016-675fc584b881','driver related','கட்டணம் தொடர்பானது','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('53ke8z4s-seak-jqw4-un4b-o8pmmpydfgqh','driver related','ड्राइवर संबंधी','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('2b3343d0-4197-40f5-a7a9-80ce494f9c11','ride related','ride related','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('7074aa4c-8ce1-4af0-98e9-97fb98b2f205','ride related','ರೈಡ್ ಸಂಬಂಧಿಸಿದಂತೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('18683bbb-5d7d-4d42-a1b3-3899c48df6f3','ride related','சவாரி தொடர்பானது','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('lf8gvnvu-9csb-ol0w-fv9s-g5wccbu6kv68','ride related','सवारी संबंधी','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('94718e87-cdad-4224-95c4-cc8093b174d5','app related','app related','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('a7b5e46d-9698-4b78-a277-dca064d377c3','app related','ಅಪ್ಲಿಕೇಶನ್ಗೆ ಸಂಬಂಧಿಸಿದಂತೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('ad542665-1c7b-4033-824a-6d485a722536','app related','ஆப் தொடர்பானது','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('p6wptksc-eq2v-qs1e-evtj-pq3x83ymumqk','app related','ऐप संबंधी','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('0a9ac759-9c64-4314-89f7-e80392d9859b','call the driver','call the driver','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('04fdd533-54c5-45fc-bd65-300680b1be89','call the driver','ಚಾಲಕನನ್ನು ಕರೆ ಮಾಡಿ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('c659e8ab-a7df-4302-b838-d98badc5fd94','call the driver','டிரைவரை அழைக்கவும்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('3b9ci99q-3d0f-wobw-v9dt-yl1j972lehls','call the driver','ड्राइवर को कॉल करें','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('dfc7c0b3-2452-4a8a-b765-4a4df6660211','report lost item to support','report lost item to support','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('013679dd-9453-4f31-b18d-d53e23fd6b09','report lost item to support','ನಮ್ಮ ಯಾತ್ರಿ ಬೆಂಬಲಕ್ಕೆ ಕಳೆದುಹೋದವಸ್ತುಗಳನ್ನು ವರದಿ ಮಾಡಿ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('763f55c3-f14e-4d35-b159-07b5a0b30faf','report lost item to support','இழந்த உருப்படியை ஆதரவிற்குப் புகாரளிக்கவும்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('4y9blu5q-lib8-p132-1y3q-0zuk0fz3mpuh','report lost item to support','समर्थन के लिए खोई हुई वस्तु की रिपोर्ट करें','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('082e4033-e3e4-42ef-bb5b-8675ae8b7569','poor driver behaviour','poor driver behaviour','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('07523f0f-6958-4938-a45b-2b521a9dc54f','poor driver behaviour','ಕಳಪೆ ಚಾಲಕ ವರ್ತನೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('77068ed3-c35a-44ef-bec1-4590b13777e7','poor driver behaviour','மோசமான ஓட்டுநர் நடத்தை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('gc77wc15-0lsv-ecmb-aw5g-jxn8g6bs17z6','poor driver behaviour','ड्राइवर का ख़राब व्यवहार','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('33db7471-d040-452e-b1ab-536e9b699768','safety concerns','safety concerns','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('a5c893f5-ac4b-46f2-a7bd-3e6002014628','safety concerns','ಸುರಕ್ಷತೆ ಕಾಳಜಿಗಳು','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('c851e467-4b12-4df5-8464-7de7dc76473b','safety concerns','பாதுகாப்பு கவலைகள்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('qq2fmixc-fivf-sk7y-iduy-y7xqlrmuny8r','safety concerns','सुरक्षा चिंताएं','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('aa313eef-7e52-4c32-87cb-0171ec445540','hygiene of the auto','hygiene of the auto','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('329ad810-84b5-44d6-9af4-c016561cfdad','hygiene of the auto','ಆಟೋ ನೈರ್ಮಲ್ಯ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('bb9dfece-e9d2-4f27-8f9a-f25668781fa7','hygiene of the auto','ஆட்டோவின் சுகாதாரம்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('2e2hhzxn-29yx-nbnr-gzz6-a8zzmrc0xema','hygiene of the auto','ऑटो की स्वच्छता','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('59b1b54d-75b8-47b4-a998-c9c3e26d00f6','fare discrepancies','fare discrepancies','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('31e1c2f4-484e-49e4-9991-5b74c2401de3','fare discrepancies','ದರ ವ್ಯತ್ಯಾಸಗಳು','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('26ac280e-c8f9-4d3e-b72f-a5631f7f4181','fare discrepancies','கட்டண முரண்பாடுகள்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('djk69e18-qnm4-hega-driu-gc2iil9vrdpr','fare discrepancies','किराये की विसंगतियाँ','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('b07a9119-c76f-42f2-8209-68fb00b875ee','My issue is not listed here','My issue is not listed here','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('6ef81a61-8114-4bb0-ad50-e0c2eed124f8','My issue is not listed here','ನನ್ನ ಸಮಸ್ಯೆಯನ್ನು ಇಲ್ಲಿ ಪಟ್ಟಿ ಮಾಡಲಾಗಿಲ್ಲ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('a70d7091-8069-42dc-93ef-63d46b32e370','My issue is not listed here','எனது பிரச்சினை இங்கே பட்டியலிடப்படவில்லை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('qvjky9dr-oc5e-mjx8-i1kz-eu029q9yx0bm','My issue is not listed here','मेरा मुद्दा यहां सूचीबद्ध नहीं है','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('d8d03fb2-afe8-4741-bd03-1476088cbb3d','longer route taken','longer route taken','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('1538b58d-bb9b-4a9d-b3e3-5313f32704ef','longer route taken','ದೀರ್ಘ ಮಾರ್ಗವನ್ನು ತೆಗೆದುಕೊಳ್ಳಲಾಗಿದೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('9c81944e-ca9c-4234-9bd1-705cec31fcd9','longer route taken','நீண்ட பாதை எடுக்கப்பட்டது','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('8ipedd78-3oux-ss26-0yfj-hs547m74xx24','longer route taken','लंबा रास्ता अपनाया गया','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('d8d03fb2-afe8-4741-bd03-1476088cbb3f','unable to contact the driver','unable to contact the driver','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('1538b58d-bb9b-4a9d-b3e3-5313f32704eh','unable to contact the driver','ಚಾಲಕನನ್ನು ಸಂಪರ್ಕಿಸಲು ಸಾಧ್ಯವಾಗಲಿಲ್ಲ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('9c81944e-ca9c-4234-9bd1-705cec31fcd1','unable to contact the driver','டிரைவரை தொடர்பு கொள்ள முடியவில்லை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('wvx5mvfi-qo60-6fjh-ls37-url76sggv08u','unable to contact the driver','ड्राइवर से संपर्क नहीं हो सका','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('d8d03fb2-afe8-4741-bd03-1476088cbb3e','longer Pickup time','longer Pickup time','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('1538b58d-bb9b-4a9d-b3e3-5313f32704eg','longer Pickup time','ದೀರ್ಘ ಪಿಕಪ್ ಸಮಯ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('9c81944e-ca9c-4234-9bd1-705cec31fcd0','longer Pickup time','அதிக பிக்அப் நேரம்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('px6ms36x-8qk6-yi8o-7jr5-sr9j1du7cjhk','longer Pickup time','अधिक पिकअप समय','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('97ca0755-3c00-4cea-b885-d8d920524d9f','OTP issues','OTP issues','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('30ae39e2-c167-4ee0-83a2-1000cd7e4cf8','OTP issues','OTP ಸಮಸ್ಯೆಗಳು','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('8d02944f-c208-411c-ab6a-e1c8a7f0491f','OTP issues','OTP பிரச்சினைகள்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('u0zlcwto-aic5-lm0u-sy6t-qtz7wq5u2oui','OTP issues','ओटीपी मुद्दे','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('6cbefba6-1dfc-4c47-b876-ffb961579be4','app not working properly','app not working properly','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('87016b8b-2fce-496e-b345-ad66b531965c','app not working properly','ಅಪ್ಲಿಕೇಶನ್ ಸರಿಯಾಗಿ ಕಾರ್ಯನಿರ್ವಹಿಸುತ್ತಿಲ್ಲ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('f494b182-3293-4918-b65b-ec1e9487e645','app not working properly','ஆப் சரியாக வேலை செய்யவில்லை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('9cyyenc3-2hej-yygm-1bne-1cd948b87ljv','app not working properly','ऐप ठीक से काम नहीं कर रहा','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('22f35de8-20c6-4646-858f-fdc3e192afaa','location related issue','location related issue','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('15b044c7-1050-4af2-8f02-e2eee5a36012','location related issue','ಸ್ಥಳ ಸಂಬಂಧಿತ ಸಮಸ್ಯೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('5ccd51ca-0d9e-43c2-964e-d6b0f817df79','location related issue','இடம் தொடர்பான பிரச்சினை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('ik4txddl-x1mn-xjvk-bar2-w1k8kyownmrm','location related issue','स्थान संबंधी मुद्दा','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('abcde119-c76f-42f2-8209-68fb00b875ef','Hey XYZ,We’re sorry to hear about your lost item','Hey XYZ,We’re sorry to hear about your lost item','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('fghija61-8114-4bb0-ad50-e0c2eed124f9','Hey XYZ,We’re sorry to hear about your lost item','ಹೇ XYZ, ನಿಮ್ಮ ಕಳೆದುಹೋದ ಐಟಂ ಬಗ್ಗೆ ಕೇಳಲು ನಾವು ವಿಷಾದಿಸುತ್ತೇವೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('klmno091-8069-42dc-93ef-63d46b32e371','Hey XYZ,We’re sorry to hear about your lost item','ஏய் XYZ, உங்கள் தொலைந்து போன பொருளைப் பற்றி அறிந்து வருந்துகிறோம்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('y6dsvl92-zbbv-43fj-uvad-52bl0wrfr6bc','Hey XYZ,We’re sorry to hear about your lost item','हे XYZ, हमें आपकी खोई हुई वस्तु के बारे में सुनकर दुख हुआ','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('a70d7091-8069-42dc-93ef-abcdeb32e371','How do you wish to resolve this issue?','How do you wish to resolve this issue?','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('a70d7091-8069-42dc-93ef-fghijb32e371','How do you wish to resolve this issue?','ಈ ಸಮಸ್ಯೆಯನ್ನು ಹೇಗೆ ಪರಿಹರಿಸಲು ನೀವು ಬಯಸುತ್ತೀರಿ?','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('a70d7091-8069-42dc-93ef-klmnob32e371','How do you wish to resolve this issue?','இந்த சிக்கலை எவ்வாறு தீர்க்க விரும்புகிறீர்கள்?','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('o03uex41-cr70-66ju-nigi-hgw39r34kd4w','How do you wish to resolve this issue?','आप इस मुद्दे को कैसे हल करना चाहते हैं?','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('a7012332-8069-42dc-93ef-63dabcdee371','Please share more details on the lost item. You can also add images or voice notes for us to help you out better.','Please share more details on the lost item. You can also add images or voice notes for us to help you out better.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('a70123bj-8069-42dc-93ef-63dfghije371','Please share more details on the lost item. You can also add images or voice notes for us to help you out better.','ಕಳೆದುಹೋದ ವಸ್ತುವಿನ ಕುರಿತು ಹೆಚ್ಚಿನ ವಿವರಗಳನ್ನು ಹಂಚಿಕೊಳ್ಳಿ. ನಿಮಗೆ ಉತ್ತಮವಾಗಿ ಸಹಾಯ ಮಾಡಲು ನೀವು ಚಿತ್ರಗಳನ್ನು ಅಥವಾ ಧ್ವನಿ ಟಿಪ್ಪಣಿಗಳನ್ನು ಸಹ ಸೇರಿಸಬಹುದು.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('a1b23cg2-8069-42dc-93ef-63dlmnope371','Please share more details on the lost item. You can also add images or voice notes for us to help you out better.','இழந்த பொருளைப் பற்றிய கூடுதல் விவரங்களைப் பகிரவும். நீங்கள் சிறப்பாகச் செயல்பட எங்களுக்காக படங்கள் அல்லது குரல் குறிப்புகளைச் சேர்க்கலாம்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('vni95aod-0j81-zlfz-pqck-37g9qdwgkns5','Please share more details on the lost item. You can also add images or voice notes for us to help you out better.','कृपया खोई हुई वस्तु के बारे में अधिक जानकारी साझा करें। बेहतर मदद के लिए आप हमारे लिए चित्र या वॉयस नोट्स भी जोड़ सकते हैं।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('a70d7091-131e-42dc-93ef-63d46b32e371','Hey XYZ, We’re really sorry to hear you have been facing app related issues.','Hey XYZ, We’re really sorry to hear you have been facing app related issues.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('a70d7091-1232-42dc-93ef-63d46b32e371','Hey XYZ, We’re really sorry to hear you have been facing app related issues.','ಹೇ XYZ, ನೀವು ಅಪ್ಲಿಕೇಶನ್ ಸಂಬಂಧಿತ ಸಮಸ್ಯೆಗಳನ್ನು ಎದುರಿಸುತ್ತಿರುವುದನ್ನು ಕೇಳಲು ನಾವು ನಿಜವಾಗಿಯೂ ವಿಷಾದಿಸುತ್ತೇವೆ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('a70d7091-8341-42dc-93ef-63d46b32e371','Hey XYZ, We’re really sorry to hear you have been facing app related issues.','ஹாய் XYZ, ஆப்ஸ் தொடர்பான சிக்கல்களை நீங்கள் எதிர்கொண்டிருப்பதைக் கேட்டு வருந்துகிறோம்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('lasobrny-f235-d23c-b1d0-lsysngg5drxv','Hey XYZ, We’re really sorry to hear you have been facing app related issues.','हे XYZ, हमें यह जानकर वास्तव में खेद है कि आपको ऐप से संबंधित समस्याओं का सामना करना पड़ रहा है।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('a70d7091-8069-42dc-93ef-613hj1v1e371','Please select the type of issue you are facing so we can help you out better','Please select the type of issue you are facing so we can help you out better','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('a70d7091-8069-42dc-93ef-1g23hj123h12','Please select the type of issue you are facing so we can help you out better','ದಯವಿಟ್ಟು ನೀವು ಎದುರಿಸುತ್ತಿರುವ ಸಮಸ್ಯೆಯ ಪ್ರಕಾರವನ್ನು ಆಯ್ಕೆಮಾಡಿ ಇದರಿಂದ ನಾವು ನಿಮಗೆ ಉತ್ತಮವಾಗಿ ಸಹಾಯ ಮಾಡಬಹುದು','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('a70d7091-8069-42dc-93ef-ir1t23f1fv31','Please select the type of issue you are facing so we can help you out better','நீங்கள் எதிர்கொள்ளும் சிக்கலின் வகையைத் தேர்ந்தெடுக்கவும், நாங்கள் உங்களுக்குச் சிறப்பாக உதவ முடியும்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('rdzorct4-ihnb-3242-oxv1-nbieoohev1dl','Please select the type of issue you are facing so we can help you out better','कृपया उस प्रकार की समस्या का चयन करें जिसका आप सामना कर रहे हैं ताकि हम आपकी बेहतर मदद कर सकें','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('asd123c3-8069-42dc-93ef-g13gv4ft1j11','Hey XYZ, We’re really sorry to hear you have been facing ride related issues.','Hey XYZ, We’re really sorry to hear you have been facing ride related issues.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('3231c123-8069-42dc-93ef-3jv162j3j1h3','Hey XYZ, We’re really sorry to hear you have been facing ride related issues.','ಹೇ XYZ, ನೀವು ಸವಾರಿ ಸಂಬಂಧಿತ ಸಮಸ್ಯೆಗಳನ್ನು ಎದುರಿಸುತ್ತಿರುವುದನ್ನು ಕೇಳಲು ನಾವು ನಿಜವಾಗಿಯೂ ವಿಷಾದಿಸುತ್ತೇವೆ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('eqygtv2k-1234-42dc-93ef-fgu3jy41g23v','Hey XYZ, We’re really sorry to hear you have been facing ride related issues.','ஹாய் XYZ, நீங்கள் சவாரி தொடர்பான சிக்கல்களை எதிர்கொண்டிருப்பதைக் கேட்டு வருந்துகிறோம்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('a9239tiv-ltvf-z2a3-inf1-9cba94pv50cb','Hey XYZ, We’re really sorry to hear you have been facing ride related issues.','हे XYZ, हमें यह जानकर वास्तव में खेद है कि आपको सवारी संबंधी समस्याओं का सामना करना पड़ रहा है।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('b5bh3b4m-5453-4545-93ef-234vjm2bdkng','Hey XYZ, We’re really sorry to hear you have been facing driver related issues.','Hey XYZ, We’re really sorry to hear you have been facing driver related issues.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('db1b2h34-8069-42dc-93ef-981jhvcg1ivf','Hey XYZ, We’re really sorry to hear you have been facing driver related issues.','ಹೇ XYZ, ನೀವು ಚಾಲಕ ಸಂಬಂಧಿತ ಸಮಸ್ಯೆಗಳನ್ನು ಎದುರಿಸುತ್ತಿರುವುದನ್ನು ಕೇಳಲು ನಾವು ನಿಜವಾಗಿಯೂ ವಿಷಾದಿಸುತ್ತೇವೆ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('123vgcdb-1235-42dc-1234-12vg3no1oj23','Hey XYZ, We’re really sorry to hear you have been facing driver related issues.','ஹாய் XYZ, நீங்கள் இயக்கி தொடர்பான சிக்கல்களை எதிர்கொள்வதைக் கேட்டு வருந்துகிறோம்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('67xpn8da-on68-zeal-jzal-zkyzf3pqodcr','Hey XYZ, We’re really sorry to hear you have been facing driver related issues.','हे XYZ, हमें यह जानकर सचमुच दुख हुआ कि आपको ड्राइवर संबंधी समस्याओं का सामना करना पड़ रहा है।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('123gciur-5453-5322-1235-234vjm2bdkng','We are glad to be of help. You can reach out to us in case of any other issues using help & support option in the side menu.','We are glad to be of help. You can reach out to us in case of any other issues using help & support option in the side menu.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('db1b2h34-8069-42dc-5846-1bce1kevfb2j','We are glad to be of help. You can reach out to us in case of any other issues using help & support option in the side menu.','ಸಹಾಯ ಮಾಡಲು ನಮಗೆ ಸಂತೋಷವಾಗಿದೆ. ಸೈಡ್ ಮೆನುವಿನಲ್ಲಿ ಸಹಾಯ ಮತ್ತು ಬೆಂಬಲ ಆಯ್ಕೆಯನ್ನು ಬಳಸಿಕೊಂಡು ಯಾವುದೇ ಇತರ ಸಮಸ್ಯೆಗಳ ಸಂದರ್ಭದಲ್ಲಿ ನೀವು ನಮ್ಮನ್ನು ಸಂಪರ್ಕಿಸಬಹುದು.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('123vgcdb-1235-42dc-9384-12vg3no1oj23','We are glad to be of help. You can reach out to us in case of any other issues using help & support option in the side menu.','உதவுவதில் நாங்கள் மகிழ்ச்சியடைகிறோம். பக்க மெனுவில் உள்ள உதவி & ஆதரவு விருப்பத்தைப் பயன்படுத்தி வேறு ஏதேனும் சிக்கல்கள் இருந்தால் எங்களைத் தொடர்புகொள்ளலாம்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('n7r70zao-7eh7-rlkx-ye7i-0pb1kczpq5jm','We are glad to be of help. You can reach out to us in case of any other issues using help & support option in the side menu.','हमें मदद करके खुशी हुई. किसी भी अन्य समस्या के मामले में आप साइड मेनू में सहायता एवं सहायता विकल्प का उपयोग करके हमसे संपर्क कर सकते हैं।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('f12j5l12-8069-42dc-93ef-63d46b32e371','Please select an option that best represents your issue','Please select an option that best represents your issue','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('u3234by4-2344-42dc-93ef-24hbk762k3g4','Please select an option that best represents your issue','ದಯವಿಟ್ಟು ನಿಮ್ಮ ಸಮಸ್ಯೆಯನ್ನು ಉತ್ತಮವಾಗಿ ಪ್ರತಿನಿಧಿಸುವ ಆಯ್ಕೆಯನ್ನು ಆಯ್ಕೆಮಾಡಿ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('1v2353yu-3532-42dc-3452-3b2hg4v3k243','Please select an option that best represents your issue','உங்கள் சிக்கலைக் குறிக்கும் விருப்பத்தைத் தேர்ந்தெடுக்கவும்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('2dr0zu7y-lirb-k4fw-xlbt-g6l9s74ge5ml','Please select an option that best represents your issue','कृपया वह विकल्प चुनें जो आपकी समस्या का सबसे अच्छा प्रतिनिधित्व करता हो','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('asnv12vc-8069-42dc-93ef-63d46b32e371','This Issue was mark as resolved automatically due to no response within 2 hours.','This Issue was mark as resolved automatically due to no response within 2 hours.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('vaskbk12-2344-1241-93ef-24hbk762k3g4','This Issue was mark as resolved automatically due to no response within 2 hours.','ದಯವಿಟ್ಟು ನಿಮ್ಮ ಸಮಸ್ಯೆಯನ್ನು ಉತ್ತಮವಾಗಿ ಪ್ರತಿನಿಧಿಸುವ ಆಯ್ಕೆಯನ್ನು ಆಯ್ಕೆಮಾಡಿ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('ab1k1231-3532-42dc-1234-3b2hg4v3k243','This Issue was mark as resolved automatically due to no response within 2 hours.','உங்கள் சிக்கலைக் குறிக்கும் விருப்பத்தைத் தேர்ந்தெடுக்கவும்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('lu5bbicy-mt84-mwn0-d0tc-gp76p450bqgr','This Issue was mark as resolved automatically due to no response within 2 hours.','2 घंटे के भीतर कोई प्रतिक्रिया नहीं मिलने के कारण इस समस्या को स्वचालित रूप से हल हो गया के रूप में चिह्नित किया गया था।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('asnv12vc-8069-42dc-93ef-kadbdg13213g','We have looked into the issue and have concluded to refund you the fare amount for the discrepancy in the fare.','We have looked into the issue and have concluded to refund you the fare amount for the discrepancy in the fare.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('vaskbk12-2344-1241-93ef-avsdjvacdjtd','We have looked into the issue and have concluded to refund you the fare amount for the discrepancy in the fare.','ನಾವು ಸಮಸ್ಯೆಯನ್ನು ಪರಿಶೀಲಿಸಿದ್ದೇವೆ ಮತ್ತು ದರದಲ್ಲಿನ ವ್ಯತ್ಯಾಸಕ್ಕಾಗಿ ಶುಲ್ಕದ ಮೊತ್ತವನ್ನು ನಿಮಗೆ ಮರುಪಾವತಿಸಲು ತೀರ್ಮಾನಿಸಿದ್ದೇವೆ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('ab1k1231-ad21-42dc-1234-1t23vcajsvdd','We have looked into the issue and have concluded to refund you the fare amount for the discrepancy in the fare.','சிக்கலைப் பார்த்துவிட்டு, கட்டணத்தில் உள்ள முரண்பாட்டிற்கான கட்டணத் தொகையை உங்களுக்குத் திருப்பித் தர முடிவு செய்துள்ளோம்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('yugehbg3-2ang-z41h-frar-voadcmr7gqo6','We have looked into the issue and have concluded to refund you the fare amount for the discrepancy in the fare.','हमने इस मुद्दे पर गौर किया है और किराये में विसंगति के लिए आपको किराया राशि वापस करने का निष्कर्ष निकाला है।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('asdvj1be-8723-42dc-93ef-hvsayd1b23u1','Was your issue resolved succesfully?','Was your issue resolved succesfully?','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('asdkngas-123c-1233-93ef-1evj1tvuhwad','Was your issue resolved succesfully?','ನಿಮ್ಮ ಸಮಸ್ಯೆಯನ್ನು ಯಶಸ್ವಿಯಾಗಿ ಪರಿಹರಿಸಲಾಗಿದೆಯೇ?','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('ab1k1231-2341-5457-1234-1hb7rcbgka12','Was your issue resolved succesfully?','உங்கள் பிரச்சினை வெற்றிகரமாக தீர்க்கப்பட்டதா?','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('i66n6fv8-xboz-rjty-kvg3-vw8vmsqnmftc','Was your issue resolved succesfully?','क्या आपकी समस्या सफलतापूर्वक हल हो गई?','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('j3rpftve-ffak-rtr0-p77n-dgvzy58mjy4v','Inappropriate Behaviour','Inappropriate Behaviour','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('5jc2pjdd-p3ii-cpp4-5ufb-jfracvvmevvp','Inappropriate Behaviour','ಅನುಚಿತ ವರ್ತನೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('wmaw2kqy-wrwr-76ii-45ua-jc6b5um86fxw','Inappropriate Behaviour','பொருத்தமற்ற நடத்தை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('4vnuwosd-omay-y4af-4c1s-0flge2yo1pbp','Inappropriate Behaviour','अनुपयुक्त व्यवहार','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('u7jwtu73-j1dm-mx37-3ea0-p91vhcjz64f7','Abusive Language','Abusive Language','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('1v4ejadm-667c-7ttr-7vwt-y86xxfanux6r','Abusive Language','ನಿಂದನೀಯ ಭಾಷೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('r4ti8z7v-czn0-a52x-wnhg-nnjb8pxjay5x','Abusive Language','தவறான மொழி','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('6fxqymns-r9xb-dc0w-rp8z-49f65xackzfe','Abusive Language','बदज़बानी','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('1ny4wypi-54fk-2e2b-j0up-40dv38t0iq9h','Threats or Violence','Threats or Violence','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('i3uvbfmk-xq6u-xrwc-25i4-zu6gn79fthq6','Threats or Violence','ಬೆದರಿಕೆಗಳು ಅಥವಾ ಹಿಂಸೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('6ggfz6fy-qdzv-mm9n-itcq-714j3hwrh680','Threats or Violence','அச்சுறுத்தல்கள் அல்லது வன்முறை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('70xksi00-jj3y-1918-0jms-99gqupi33qb3','Threats or Violence','धमकी या हिंसा','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('u1anyb2d-ipmt-y5x9-12eg-dpre4uqpqc1m','Please give some more details. You can also send images or voice notes to elaborate better','Please give some more details. You can also send images or voice notes to elaborate better','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('xmm2zki6-zaaw-8d2h-ywub-t1pkqijyjqey','Please give some more details. You can also send images or voice notes to elaborate better','ದಯವಿಟ್ಟು ಇನ್ನೂ ಕೆಲವು ವಿವರಗಳನ್ನು ನೀಡಿ. ಉತ್ತಮವಾಗಿ ವಿವರಿಸಲು ನೀವು ಚಿತ್ರಗಳನ್ನು ಅಥವಾ ಧ್ವನಿ ಟಿಪ್ಪಣಿಗಳನ್ನು ಸಹ ಕಳುಹಿಸಬಹುದು','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('z8e3ha22-quin-a7kf-zae6-qumw9ha13dwy','Please give some more details. You can also send images or voice notes to elaborate better','மேலும் சில விவரங்களைத் தரவும். நீங்கள் இன்னும் சிறப்பாக விவரிக்க படங்கள் அல்லது குரல் குறிப்புகளை அனுப்பலாம்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('4kwioi3o-p9ke-emta-mhkf-wi1v5iei19q8','Please give some more details. You can also send images or voice notes to elaborate better','कृपया कुछ और विवरण दें. आप बेहतर ढंग से विस्तार से बताने के लिए चित्र या वॉयस नोट्स भी भेज सकते हैं','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('n8hxga88-nx91-xk3q-2t7c-aanmdi51xxfa','other issue','other issue','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('n6t9ceaz-7wyb-r6qh-2yi6-ua3a5w8xxcjn','other issue','ಇತರ ಸಮಸ್ಯೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('cw1uu9pm-drz0-3ruv-0evn-1r4c8qecihpd','other issue','மற்ற பிரச்சினை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('9o7t3fw5-akk8-txpl-px5b-3fjg7vk3tz7o','other issue','अन्य मुद्दे','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('q98qcg49-gmvn-wgri-vi0j-3hu3n3cdc2pb','Details received! Our team will reach out to you within 24 hours to help you out with the issue.','Details received! Our team will reach out to you within 24 hours to help you out with the issue.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('4j1xk7zv-uegk-m0ur-yt1h-32mt0qzapbjp','Details received! Our team will reach out to you within 24 hours to help you out with the issue.','ವಿವರಗಳನ್ನು ಸ್ವೀಕರಿಸಲಾಗಿದೆ! ಸಮಸ್ಯೆಯ ಕುರಿತು ನಿಮಗೆ ಸಹಾಯ ಮಾಡಲು ನಮ್ಮ ತಂಡವು 24 ಗಂಟೆಗಳ ಒಳಗೆ ನಿಮ್ಮನ್ನು ಸಂಪರ್ಕಿಸುತ್ತದೆ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('w5z0th56-z0xi-89tu-8j5m-bnwmk21q0prz','Details received! Our team will reach out to you within 24 hours to help you out with the issue.','பெறப்பட்ட விவரங்கள்! சிக்கலில் உங்களுக்கு உதவ எங்கள் குழு 24 மணி நேரத்திற்குள் உங்களைத் தொடர்பு கொள்ளும்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('3qpary9y-ut2s-byyr-k7ty-nhtle6m7v6ti','Details received! Our team will reach out to you within 24 hours to help you out with the issue.','विवरण प्राप्त हुआ! समस्या से निपटने में आपकी सहायता के लिए हमारी टीम 24 घंटे के भीतर आपके पास पहुंचेगी।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('kx02wwq3-vpg9-b1z9-x42y-md2yb42z0gvh','Yes, mark this issue as resolved','Yes, mark this issue as resolved','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('xixrkpyn-gzt4-ewnv-4j99-a0pjcwrk6ma9','Yes, mark this issue as resolved','ಹೌದು, ಈ ಸಮಸ್ಯೆಯನ್ನು ಪರಿಹರಿಸಲಾಗಿದೆ ಎಂದು ಗುರುತಿಸಿ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('h07td3cx-9i6e-nbng-ptfp-zbp1czqdj3mz','Yes, mark this issue as resolved','ஆம், இந்தப் பிரச்சினை தீர்க்கப்பட்டதாகக் குறிக்கவும்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('v5wmstue-u93x-img4-0ps6-3d1karg7msis','Yes, mark this issue as resolved','हां, इस समस्या को हल हो गया के रूप में चिह्नित करें','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('jmwz1p49-xupv-2m4r-hxgm-vp7vfyq7p1q6','I need more help','I need more help','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('nf3f30xw-ncp1-2n3n-38cp-w0pfhyzw4e3i','I need more help','ನನಗೆ ಹೆಚ್ಚಿನ ಸಹಾಯ ಬೇಕು','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('r4m84nm2-117m-1pi1-weca-fdg7xp8eru05','I need more help','எனக்கு மேலும் உதவி தேவை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('a9021yoe-w89m-5xr1-xf0d-v4q7x1la5pkj','I need more help','मुझे और मदद की ज़रूरत है','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('taa8uzbv-2r1r-gk7a-b7iw-51bv40yirii4','Would you like to talk to our customer support to help you out further?','Would you like to talk to our customer support to help you out further?','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('aj0cf4r0-yuap-6rh1-p1mq-etepx2hrezpy','Would you like to talk to our customer support to help you out further?','ನಿಮಗೆ ಮತ್ತಷ್ಟು ಸಹಾಯ ಮಾಡಲು ನಮ್ಮ ಗ್ರಾಹಕ ಬೆಂಬಲದೊಂದಿಗೆ ಮಾತನಾಡಲು ನೀವು ಬಯಸುವಿರಾ?','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('3wjkzpg5-b1k2-7xpi-vu7k-4wqdu41k26ui','Would you like to talk to our customer support to help you out further?','மேலும் உங்களுக்கு உதவ எங்கள் வாடிக்கையாளர் ஆதரவுடன் பேச விரும்புகிறீர்களா?','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('l8qoryok-ryxq-t54h-tic2-385y3pcorjoz','Would you like to talk to our customer support to help you out further?','क्या आप आगे सहायता के लिए हमारी ग्राहक सहायता से बात करना चाहेंगे?','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('grvdc098-2pq9-zzzy-6kke-fgk40ft9c1t5','Talk to Customer Support','Talk to Customer Support','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('qmtnj50m-n3ac-afuu-jpjg-phcjc9u36ftm','Talk to Customer Support','ಗ್ರಾಹಕ ಬೆಂಬಲದೊಂದಿಗೆ ಮಾತನಾಡಿ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('zpvb69a9-fhbd-dj4m-ub1b-gpm6n6cjrptp','Talk to Customer Support','வாடிக்கையாளர் ஆதரவுடன் பேசுங்கள்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('z25xmql5-flco-2j16-n664-4cq5ay0zuwrn','Talk to Customer Support','ग्राहक सहायता से बात करें','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('3m5hw22w-hynn-mtka-vux7-z0jnznv94vp2','Mark this issue as resolved.','Mark this issue as resolved.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('xpynwe02-zc2e-dijr-bw52-30xd2eaaxker','Mark this issue as resolved.','ಈ ಸಮಸ್ಯೆಯನ್ನು ಪರಿಹರಿಸಲಾಗಿದೆ ಎಂದು ಗುರುತಿಸಿ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('w1ve0kw6-0hw4-7dkp-1f3m-4y2avpx8b88q','Mark this issue as resolved.','இந்தப் பிரச்சினை தீர்க்கப்பட்டதாகக் குறிக்கவும்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('fjs4dkl662yaa-5kpy-hobs-16prd226yxcs','Mark this issue as resolved.','इस समस्या को हल हो गया के रूप में चिह्नित करें.','HINDI');

--AFTER ISSUE REPORT IS CREATED
INSERT INTO atlas_app.issue_message VALUES ('v31ghv31-1234-234f-fb2ds-34v2dfstf1j', null, null, 'Details received! Our team will reach out to you within 24 hours to help you out with the issue.', null, 1);

INSERT INTO atlas_app.issue_message VALUES ('qradsvna-c76f-42f2-8209-68fb00b875ef', null, null, 'This Issue was mark as resolved automatically due to no response within 2 hours.', 'AUTO_MARKED_RESOLVED',1);

INSERT INTO atlas_app.issue_message VALUES ('12m3n3ql-ch17-12cb-34hu-1h23ewdf112j', null, null, 'We have looked into the issue and have concluded to refund you the fare amount for the discrepancy in the fare.', null, 1);

INSERT INTO atlas_app.issue_option VALUES ('adasasd1-d58e-234v-8i6g-3nbtdsfvsnfv', null, '123md312-ch17-u3tj-123d-febf223b12j3', 'Yes, mark this issue as resolved', 'MARK_RESOLVED', 1);
INSERT INTO atlas_app.issue_option VALUES ('adv1v23f-6j5u-y4km-bdwc-1g3fsdaft12f', null, '123md312-ch17-u3tj-123d-febf223b12j3', 'I need more help', null, 2);

INSERT INTO atlas_app.issue_message VALUES ('wefgwfye-thef-12gf-4ybh-bhsdfbvgyw32', 'adasasd1-d58e-234v-8i6g-3nbtdsfvsnfv', null, 'We are glad to be of help. You can reach out to us in case of any other issues using help & support option in the side menu', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('asdg12ev-546h-7j5j-4hrg-34jtnrgwfbhf', 'adv1v23f-6j5u-y4km-bdwc-1g3fsdaft12f', null, 'Would you like to talk to our customer support to help you out further?', null, 1);

INSERT INTO atlas_app.issue_option VALUES ('awdvgsdd-bhtf-234v-8i6g-3nbtdsfvsnfv', null, 'asdg12ev-546h-7j5j-4hrg-34jtnrgwfbhf', 'Talk to Customer Support', 'CALL_SUPPORT', 1);
INSERT INTO atlas_app.issue_option VALUES ('bu4tbrfg-u6jn-dnj2-j6ne-qwd7fbe123gs', null, 'asdg12ev-546h-7j5j-4hrg-34jtnrgwfbhf', 'Mark this issue as resolved.', 'MARK_RESOLVED', 2);

INSERT INTO atlas_app.issue_message VALUES ('fjadshj4-ch17-73hv-3jit-asdgv1j2g331', 'bu4tbrfg-u6jn-dnj2-j6ne-qwd7fbe123gs', null, 'We are glad to be of help. You can reach out to us in case of any other issues using help & support option in the side menu', null, 1);

INSERT INTO atlas_app.issue_message VALUES ('123md312-ch17-u3tj-123d-febf223b12j3', 'awdvgsdd-bhtf-234v-8i6g-3nbtdsfvsnfv', null, 'Was your issue resolved succesfully?', null, 2);
