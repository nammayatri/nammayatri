ALTER TABLE atlas_driver_offer_bpp.issue_report RENAME COLUMN driver_id TO person_id;

CREATE TABLE atlas_driver_offer_bpp.issue_message (
  id character(36) NOT NULL PRIMARY KEY,
  option_id character(36) REFERENCES atlas_driver_offer_bpp.issue_option (id),
  category_id character(36) REFERENCES atlas_driver_offer_bpp.issue_category (id),
  message character varying(255) NOT NULL,
  label text,
  priority int NOT NULL
);

CREATE TABLE atlas_driver_offer_bpp.issue_config (
  id character(36) NOT NULL PRIMARY KEY,
  auto_mark_issue_resolve_duration double precision,
  on_auto_mark_issue_res_msgs text[],
  on_create_issue_msgs text[],
  on_issue_reopen_msgs text[],
  on_kapt_mark_issue_awt_msgs text[]
);


ALTER TABLE atlas_driver_offer_bpp.issue_option ADD COLUMN priority int NOT NULL DEFAULT 1;

ALTER TABLE atlas_driver_offer_bpp.issue_option ADD COLUMN label text;

ALTER TABLE atlas_driver_offer_bpp.issue_report ADD COLUMN chats text[];

ALTER TABLE atlas_driver_offer_bpp.issue_option ADD COLUMN issue_message_id character varying(255);

INSERT INTO atlas_driver_offer_bpp.issue_config VALUES ('h56bjh3i-4n5b-34ut-bg8b3k3ggkq8', 2, ARRAY['qradsvna-c76f-42f2-8209-68fb00b875ef'],ARRAY['v31ghv31-1234-234f-fb2ds-34v2dfstf1j'], ARRAY['v31ghv31-1234-234f-fb2ds-34v2dfstf1j'], ARRAY['12m3n3ql-ch17-12cb-34hu-1h23ewdf112j', '123md312-ch17-u3tj-123d-febf223b12j3']);

--LOST AND FOUND
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('ab7a9119-c76f-42f2-8209-68fb00b875ef', null, '02d3fbe2-d5ad-4057-94e1-e84fde1ef3f6', 'Hey XYZ,We’re sorry to hear about your lost item.', null, 1);
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('bc7a9119-c76f-42f2-8209-68fb00b875ef', null, '02d3fbe2-d5ad-4057-94e1-e84fde1ef3f6', 'How do you wish to resolve this issue?', null, 2);

UPDATE atlas_driver_offer_bpp.issue_option SET issue_message_id = 'bc7a9119-c76f-42f2-8209-68fb00b875ef', priority = 1 WHERE id = '489fa56f-d58e-4c56-8d84-bbe9ecd8ec00';
UPDATE atlas_driver_offer_bpp.issue_option SET issue_message_id = 'bc7a9119-c76f-42f2-8209-68fb00b875ef', priority = 2 WHERE id = '169ae361-ab6e-4c77-b5ff-c898114d6b3c';
UPDATE atlas_driver_offer_bpp.issue_option SET issue_message_id = 'bc7a9119-c76f-42f2-8209-68fb00b875ef', priority = 3 WHERE id = '528f0d86-a1c6-4f5c-90b5-fa7514e940ce';

INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('op7a9119-24nj-42f2-8209-68fb00b875ef', '489fa56f-d58e-4c56-8d84-bbe9ecd8ec00', null, 'Please share more details on the lost item. You can also add images or voice notes for us to help you out better.', 'CALL_CUSTOMER', 1);
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('op7a9119-c76f-236r-8209-asdv1jg3vrc2', '169ae361-ab6e-4c77-b5ff-c898114d6b3c', null, 'Please share more details on the lost item. You can also add images or voice notes for us to help you out better.', null, 1);
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('op7a9119-534d-y4fe-8209-1231cv21yjg3', '528f0d86-a1c6-4f5c-90b5-fa7514e940ce', null, 'Please share more details on the lost item. You can also add images or voice notes for us to help you out better.', null, 1);

--FARE RELATED
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('ef7a9119-c76f-42f2-8209-68fb00b875ef', null, '5ca814d9-66e2-4ccc-b236-40b73b705e88', 'Hey XYZ, We’re really sorry to hear you have been facing fare related issues.', null, 1);
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('gh7a9119-c76f-42f2-8209-68fb00b875ef', null, '5ca814d9-66e2-4ccc-b236-40b73b705e88', 'Please select the type of issue you are facing so we can help you out better', null, 2);

UPDATE atlas_driver_offer_bpp.issue_option SET issue_message_id = 'gh7a9119-c76f-42f2-8209-68fb00b875ef', priority = 1 WHERE id = 'd8eee179-c34a-4d70-ba2e-1222962091cb';
UPDATE atlas_driver_offer_bpp.issue_option SET issue_message_id = 'gh7a9119-c76f-42f2-8209-68fb00b875ef', priority = 2 WHERE id = '16bf2a0d-229e-4afc-be9d-a06fb1bd0581';
UPDATE atlas_driver_offer_bpp.issue_option SET issue_message_id = 'gh7a9119-c76f-42f2-8209-68fb00b875ef', priority = 3 WHERE id = '9e8785e1-77d1-42c2-945e-15d398ed8e38';
UPDATE atlas_driver_offer_bpp.issue_option SET issue_message_id = 'gh7a9119-c76f-42f2-8209-68fb00b875ef', priority = 4 WHERE id = '3d68dbe3-bc3a-44a0-939a-7ad166c4818b';
UPDATE atlas_driver_offer_bpp.issue_option SET issue_message_id = 'gh7a9119-c76f-42f2-8209-68fb00b875ef', priority = 5 WHERE id = '5bed68d9-265c-4fb3-bdcc-22a4ed45de37';

INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('32jn4n2t-nwsd-j124-4n45-5k6oyh0hmg4f', 'd8eee179-c34a-4d70-ba2e-1222962091cb', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('8h9238yr-5yvy-nqi3-7g77-3nifg87483e2', '16bf2a0d-229e-4afc-be9d-a06fb1bd0581', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('42nsjh59-24pl-tdeu-4zra-1ubar1v49j6m', '9e8785e1-77d1-42c2-945e-15d398ed8e38', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('bsdfy12e-gy1d-7fgh-7y3f-yg2fbsdjfbdf', '3d68dbe3-bc3a-44a0-939a-7ad166c4818b', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('7hwfwfyy-sdfb-ysdf-bysd-fbysdfybsdff', '5bed68d9-265c-4fb3-bdcc-22a4ed45de37', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);

--RIDE RELATED
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('kl7a9119-c76f-42f2-8209-68fb00b875ef', null, 'a8ff40fb-88a0-440b-9763-e6c58929e503', 'Hey XYZ, We’re really sorry to hear you have been facing ride related issues.', null, 1);
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('mn7a9119-c76f-42f2-8209-68fb00b875ef', null, 'a8ff40fb-88a0-440b-9763-e6c58929e503', 'Please select the type of issue you are facing so we can help you out better', null, 2);

UPDATE atlas_driver_offer_bpp.issue_option SET issue_message_id = 'mn7a9119-c76f-42f2-8209-68fb00b875ef', priority = 1 WHERE id = '208e90d4-63c0-4176-a91f-2a94fadfb53c';
UPDATE atlas_driver_offer_bpp.issue_option SET issue_message_id = 'mn7a9119-c76f-42f2-8209-68fb00b875ef', priority = 2 WHERE id = '2d1c22eb-f576-4c9b-9d92-985d29149253';
UPDATE atlas_driver_offer_bpp.issue_option SET issue_message_id = 'mn7a9119-c76f-42f2-8209-68fb00b875ef', priority = 3 WHERE id = '9bcf1fbd-0d32-48ea-a267-0de4f0ae3114';
UPDATE atlas_driver_offer_bpp.issue_option SET issue_message_id = 'mn7a9119-c76f-42f2-8209-68fb00b875ef', priority = 4 WHERE id = 'e9403adc-baaf-454f-ae75-20bb76f7a823';
UPDATE atlas_driver_offer_bpp.issue_option SET issue_message_id = 'mn7a9119-c76f-42f2-8209-68fb00b875ef', priority = 5 WHERE id = 'a9af6580-da16-438e-af5a-caaf4bce0d05';

INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('ajtkztg1-cyru-lwwf-y9kl-mdqjfteznhlo', '208e90d4-63c0-4176-a91f-2a94fadfb53c', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('wc2p6nfg-p2br-cujz-l8yz-rlxwlb9fi0wu', '2d1c22eb-f576-4c9b-9d92-985d29149253', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('mfl3pvmg-16we-6bin-8umj-3eaoezhusngz', '9bcf1fbd-0d32-48ea-a267-0de4f0ae3114', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('cb1gemvq-nxrn-8y4r-dt2h-13c69m0zrs16', 'e9403adc-baaf-454f-ae75-20bb76f7a823', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('sjoo3n11-la54-l8zi-wklz-9g39tydkl2cs', 'a9af6580-da16-438e-af5a-caaf4bce0d05', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);

--APP RELATED
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('kq7a9119-c76f-42f2-8209-68fb00b875ef', null, '3c2970e3-f01a-4dc3-8a21-3b77f01497ea', 'Hey XYZ, We’re really sorry to hear you have been facing app related issues.', null, 1);
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('ly7a9119-c76f-42f2-8209-68fb00b875ef', null, '3c2970e3-f01a-4dc3-8a21-3b77f01497ea', 'Please select the type of issue you are facing so we can help you out better', null, 2);

UPDATE atlas_driver_offer_bpp.issue_option SET issue_message_id = 'ly7a9119-c76f-42f2-8209-68fb00b875ef', priority = 1 WHERE id = '8e23eecf-55d8-4a15-9cd5-670246f2e442';
UPDATE atlas_driver_offer_bpp.issue_option SET issue_message_id = 'ly7a9119-c76f-42f2-8209-68fb00b875ef', priority = 2 WHERE id = '251a4cd0-efda-4d43-94b7-ebd6f7164c10';
UPDATE atlas_driver_offer_bpp.issue_option SET issue_message_id = 'ly7a9119-c76f-42f2-8209-68fb00b875ef', priority = 3 WHERE id = '0028272a-5019-4785-a153-ae463f0bd1a3';
UPDATE atlas_driver_offer_bpp.issue_option SET issue_message_id = 'ly7a9119-c76f-42f2-8209-68fb00b875ef', priority = 4 WHERE id = '30052650-c9b6-4801-82a4-1fcb7c6bce75';

INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('z0l2yuvg-ea8b-zzzd-c0rm-qsiuusqi33yn', '8e23eecf-55d8-4a15-9cd5-670246f2e442', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('40dmsjym-8j4e-kmwt-k1qs-ic91mawc65hz', '251a4cd0-efda-4d43-94b7-ebd6f7164c10', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('svl7wo3h-63db-btac-0k1f-u6ze9rre9gz0', '0028272a-5019-4785-a153-ae463f0bd1a3', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('wq05prx4-cbbn-52nb-uxu7-cb38e15zrw8n', '30052650-c9b6-4801-82a4-1fcb7c6bce75', null, 'Please give some more details. You can also send images or voice notes to elaborate better', null, 1);

--AFTER ISSUE REPORT SUBMISSION
INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('v31ghv31-1234-234f-fb2ds-34v2dfstf1j', null, null, 'Details received! Our team will reach out to you within 24 hours to help you out with the issue.', null, 1);

INSERT INTO atlas_driver_offer_bpp.issue_message VALUES ('12m3n3ql-ch17-12cb-34hu-1h23ewdf112j', null, null, 'We have looked into the issue and have concluded to refund you the fare amount for the discrepancy in the fare.', null, 1);

--TRANSLATIONS
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('abcde119-c76f-42f2-8209-68fb00b875ef','Hey XYZ,We’re sorry to hear about your lost item','Hey XYZ,We’re sorry to hear about your lost item','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('fghija61-8114-4bb0-ad50-e0c2eed124f9','Hey XYZ,We’re sorry to hear about your lost item','ಹೇ XYZ, ನಿಮ್ಮ ಕಳೆದುಹೋದ ಐಟಂ ಬಗ್ಗೆ ಕೇಳಲು ನಾವು ವಿಷಾದಿಸುತ್ತೇವೆ','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('klmno091-8069-42dc-93ef-63d46b32e371','Hey XYZ,We’re sorry to hear about your lost item','ஏய் XYZ, உங்கள் தொலைந்து போன பொருளைப் பற்றி அறிந்து வருந்துகிறோம்','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('y6dsvl92-zbbv-43fj-uvad-52bl0wrfr6bc','Hey XYZ,We’re sorry to hear about your lost item','हे XYZ, हमें आपकी खोई हुई वस्तु के बारे में सुनकर दुख हुआ','HINDI');

INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('a70d7091-8069-42dc-93ef-abcdeb32e371','How do you wish to resolve this issue?','How do you wish to resolve this issue?','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('a70d7091-8069-42dc-93ef-fghijb32e371','How do you wish to resolve this issue?','ಈ ಸಮಸ್ಯೆಯನ್ನು ಹೇಗೆ ಪರಿಹರಿಸಲು ನೀವು ಬಯಸುತ್ತೀರಿ?','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('a70d7091-8069-42dc-93ef-klmnob32e371','How do you wish to resolve this issue?','இந்த சிக்கலை எவ்வாறு தீர்க்க விரும்புகிறீர்கள்?','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('o03uex41-cr70-66ju-nigi-hgw39r34kd4w','How do you wish to resolve this issue?','आप इस मुद्दे को कैसे हल करना चाहते हैं?','HINDI');

INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('asd123c3-8069-42dc-93ef-g13gv4ft1j11','Hey XYZ, We’re really sorry to hear you have been facing ride related issues.','Hey XYZ, We’re really sorry to hear you have been facing ride related issues.','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('3231c123-8069-42dc-93ef-3jv162j3j1h3','Hey XYZ, We’re really sorry to hear you have been facing ride related issues.','ಹೇ XYZ, ನೀವು ಸವಾರಿ ಸಂಬಂಧಿತ ಸಮಸ್ಯೆಗಳನ್ನು ಎದುರಿಸುತ್ತಿರುವುದನ್ನು ಕೇಳಲು ನಾವು ನಿಜವಾಗಿಯೂ ವಿಷಾದಿಸುತ್ತೇವೆ.','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('eqygtv2k-1234-42dc-93ef-fgu3jy41g23v','Hey XYZ, We’re really sorry to hear you have been facing ride related issues.','ஹாய் XYZ, நீங்கள் சவாரி தொடர்பான சிக்கல்களை எதிர்கொண்டிருப்பதைக் கேட்டு வருந்துகிறோம்.','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('a9239tiv-ltvf-z2a3-inf1-9cba94pv50cb','Hey XYZ, We’re really sorry to hear you have been facing ride related issues.','हे XYZ, हमें यह जानकर वास्तव में खेद है कि आपको सवारी संबंधी समस्याओं का सामना करना पड़ रहा है।','HINDI');

INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('a70d7091-8069-42dc-93ef-613hj1v1e371','Please select the type of issue you are facing so we can help you out better','Please select the type of issue you are facing so we can help you out better','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('a70d7091-8069-42dc-93ef-1g23hj123h12','Please select the type of issue you are facing so we can help you out better','ದಯವಿಟ್ಟು ನೀವು ಎದುರಿಸುತ್ತಿರುವ ಸಮಸ್ಯೆಯ ಪ್ರಕಾರವನ್ನು ಆಯ್ಕೆಮಾಡಿ ಇದರಿಂದ ನಾವು ನಿಮಗೆ ಉತ್ತಮವಾಗಿ ಸಹಾಯ ಮಾಡಬಹುದು','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('a70d7091-8069-42dc-93ef-ir1t23f1fv31','Please select the type of issue you are facing so we can help you out better','நீங்கள் எதிர்கொள்ளும் சிக்கலின் வகையைத் தேர்ந்தெடுக்கவும், நாங்கள் உங்களுக்குச் சிறப்பாக உதவ முடியும்','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('rdzorct4-ihnb-3242-oxv1-nbieoohev1dl','Please select the type of issue you are facing so we can help you out better','कृपया उस प्रकार की समस्या का चयन करें जिसका आप सामना कर रहे हैं ताकि हम आपकी बेहतर मदद कर सकें','HINDI');

INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('a70d7091-131e-42dc-93ef-63d46b32e371','Hey XYZ, We’re really sorry to hear you have been facing app related issues.','Hey XYZ, We’re really sorry to hear you have been facing app related issues.','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('a70d7091-1232-42dc-93ef-63d46b32e371','Hey XYZ, We’re really sorry to hear you have been facing app related issues.','ಹೇ XYZ, ನೀವು ಅಪ್ಲಿಕೇಶನ್ ಸಂಬಂಧಿತ ಸಮಸ್ಯೆಗಳನ್ನು ಎದುರಿಸುತ್ತಿರುವುದನ್ನು ಕೇಳಲು ನಾವು ನಿಜವಾಗಿಯೂ ವಿಷಾದಿಸುತ್ತೇವೆ.','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('a70d7091-8341-42dc-93ef-63d46b32e371','Hey XYZ, We’re really sorry to hear you have been facing app related issues.','ஹாய் XYZ, ஆப்ஸ் தொடர்பான சிக்கல்களை நீங்கள் எதிர்கொண்டிருப்பதைக் கேட்டு வருந்துகிறோம்.','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('lasobrny-f235-d23c-b1d0-lsysngg5drxv','Hey XYZ, We’re really sorry to hear you have been facing app related issues.','हे XYZ, हमें यह जानकर वास्तव में खेद है कि आपको ऐप से संबंधित समस्याओं का सामना करना पड़ रहा है।','HINDI');

INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('a7012332-8069-42dc-93ef-63dabcdee371','Please share more details on the lost item. You can also add images or voice notes for us to help you out better.','Please share more details on the lost item. You can also add images or voice notes for us to help you out better.','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('a70123bj-8069-42dc-93ef-63dfghije371','Please share more details on the lost item. You can also add images or voice notes for us to help you out better.','ಕಳೆದುಹೋದ ವಸ್ತುವಿನ ಕುರಿತು ಹೆಚ್ಚಿನ ವಿವರಗಳನ್ನು ಹಂಚಿಕೊಳ್ಳಿ. ನಿಮಗೆ ಉತ್ತಮವಾಗಿ ಸಹಾಯ ಮಾಡಲು ನೀವು ಚಿತ್ರಗಳನ್ನು ಅಥವಾ ಧ್ವನಿ ಟಿಪ್ಪಣಿಗಳನ್ನು ಸಹ ಸೇರಿಸಬಹುದು.','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('a1b23cg2-8069-42dc-93ef-63dlmnope371','Please share more details on the lost item. You can also add images or voice notes for us to help you out better.','இழந்த பொருளைப் பற்றிய கூடுதல் விவரங்களைப் பகிரவும். நீங்கள் சிறப்பாகச் செயல்பட எங்களுக்காக படங்கள் அல்லது குரல் குறிப்புகளைச் சேர்க்கலாம்.','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('vni95aod-0j81-zlfz-pqck-37g9qdwgkns5','Please share more details on the lost item. You can also add images or voice notes for us to help you out better.','कृपया खोई हुई वस्तु के बारे में अधिक जानकारी साझा करें। बेहतर मदद के लिए आप हमारे लिए चित्र या वॉयस नोट्स भी जोड़ सकते हैं।','HINDI');

INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('u1anyb2d-ipmt-y5x9-12eg-dpre4uqpqc1m','Please give some more details. You can also send images or voice notes to elaborate better','Please give some more details. You can also send images or voice notes to elaborate better','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('xmm2zki6-zaaw-8d2h-ywub-t1pkqijyjqey','Please give some more details. You can also send images or voice notes to elaborate better','ದಯವಿಟ್ಟು ಇನ್ನೂ ಕೆಲವು ವಿವರಗಳನ್ನು ನೀಡಿ. ಉತ್ತಮವಾಗಿ ವಿವರಿಸಲು ನೀವು ಚಿತ್ರಗಳನ್ನು ಅಥವಾ ಧ್ವನಿ ಟಿಪ್ಪಣಿಗಳನ್ನು ಸಹ ಕಳುಹಿಸಬಹುದು','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('z8e3ha22-quin-a7kf-zae6-qumw9ha13dwy','Please give some more details. You can also send images or voice notes to elaborate better','மேலும் சில விவரங்களைத் தரவும். நீங்கள் இன்னும் சிறப்பாக விவரிக்க படங்கள் அல்லது குரல் குறிப்புகளை அனுப்பலாம்','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('4kwioi3o-p9ke-emta-mhkf-wi1v5iei19q8','Please give some more details. You can also send images or voice notes to elaborate better','कृपया कुछ और विवरण दें. आप बेहतर ढंग से विस्तार से बताने के लिए चित्र या वॉयस नोट्स भी भेज सकते हैं','HINDI');

INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('q98qcg49-gmvn-wgri-vi0j-3hu3n3cdc2pb','Details received! Our team will reach out to you within 24 hours to help you out with the issue.','Details received! Our team will reach out to you within 24 hours to help you out with the issue.','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('4j1xk7zv-uegk-m0ur-yt1h-32mt0qzapbjp','Details received! Our team will reach out to you within 24 hours to help you out with the issue.','ವಿವರಗಳನ್ನು ಸ್ವೀಕರಿಸಲಾಗಿದೆ! ಸಮಸ್ಯೆಯ ಕುರಿತು ನಿಮಗೆ ಸಹಾಯ ಮಾಡಲು ನಮ್ಮ ತಂಡವು 24 ಗಂಟೆಗಳ ಒಳಗೆ ನಿಮ್ಮನ್ನು ಸಂಪರ್ಕಿಸುತ್ತದೆ.','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('w5z0th56-z0xi-89tu-8j5m-bnwmk21q0prz','Details received! Our team will reach out to you within 24 hours to help you out with the issue.','பெறப்பட்ட விவரங்கள்! சிக்கலில் உங்களுக்கு உதவ எங்கள் குழு 24 மணி நேரத்திற்குள் உங்களைத் தொடர்பு கொள்ளும்.','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('3qpary9y-ut2s-byyr-k7ty-nhtle6m7v6ti','Details received! Our team will reach out to you within 24 hours to help you out with the issue.','विवरण प्राप्त हुआ! समस्या से निपटने में आपकी सहायता के लिए हमारी टीम 24 घंटे के भीतर आपके पास पहुंचेगी।','HINDI');

INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('asnv12vc-8069-42dc-93ef-kadbdg13213g','We have looked into the issue and have concluded to refund you the fare amount for the discrepancy in the fare.','We have looked into the issue and have concluded to refund you the fare amount for the discrepancy in the fare.','ENGLISH');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('vaskbk12-2344-1241-93ef-avsdjvacdjtd','We have looked into the issue and have concluded to refund you the fare amount for the discrepancy in the fare.','ನಾವು ಸಮಸ್ಯೆಯನ್ನು ಪರಿಶೀಲಿಸಿದ್ದೇವೆ ಮತ್ತು ದರದಲ್ಲಿನ ವ್ಯತ್ಯಾಸಕ್ಕಾಗಿ ಶುಲ್ಕದ ಮೊತ್ತವನ್ನು ನಿಮಗೆ ಮರುಪಾವತಿಸಲು ತೀರ್ಮಾನಿಸಿದ್ದೇವೆ.','KANNADA');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('ab1k1231-ad21-42dc-1234-1t23vcajsvdd','We have looked into the issue and have concluded to refund you the fare amount for the discrepancy in the fare.','சிக்கலைப் பார்த்துவிட்டு, கட்டணத்தில் உள்ள முரண்பாட்டிற்கான கட்டணத் தொகையை உங்களுக்குத் திருப்பித் தர முடிவு செய்துள்ளோம்.','TAMIL');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('yugehbg3-2ang-z41h-frar-voadcmr7gqo6','We have looked into the issue and have concluded to refund you the fare amount for the discrepancy in the fare.','हमने इस मुद्दे पर गौर किया है और किराये में विसंगति के लिए आपको किराया राशि वापस करने का निष्कर्ष निकाला है।','HINDI');