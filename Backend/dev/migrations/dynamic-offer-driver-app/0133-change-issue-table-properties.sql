-- comments table
DELETE FROM atlas_driver_offer_bpp.comment;
ALTER TABLE atlas_driver_offer_bpp.comment DROP COLUMN author;
ALTER TABLE atlas_driver_offer_bpp.comment ADD COLUMN author_id character(36) NOT NULL;

-- issue report table
DELETE FROM atlas_driver_offer_bpp.issue_report;
ALTER TABLE atlas_driver_offer_bpp.issue_report DROP COLUMN category;
ALTER TABLE atlas_driver_offer_bpp.issue_report ADD COLUMN category_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.issue_category(id);

ALTER TABLE atlas_driver_offer_bpp.issue_report DROP COLUMN option;
ALTER TABLE atlas_driver_offer_bpp.issue_report ADD COLUMN option_id character(36) REFERENCES atlas_driver_offer_bpp.issue_option(id);

-- add hindi translation for issue_translation
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('69439940-c709-11ed-afa1-0242ac120002','lost and found','खोया और पाया','HINDI');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('69439c06-c709-11ed-afa1-0242ac120002','fare related','किराया संबंधित','HINDI');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('69439f26-c709-11ed-afa1-0242ac120002','ride related','सवारी संबंधित','HINDI');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('6943a07a-c709-11ed-afa1-0242ac120002','app related','ऐप संबंधित','HINDI');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('6943a19c-c709-11ed-afa1-0242ac120002','call the customer','ग्राहक को कॉल करें','HINDI');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('6943b4b6-c709-11ed-afa1-0242ac120002','report lost item to support','सहायता के लिए खोई हुई वस्तु की सूचना दें','HINDI');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('6943b9de-c709-11ed-afa1-0242ac120002','talk to the support team','सपोर्ट टीम से बात करें','HINDI');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('6943bbc8-c709-11ed-afa1-0242ac120002','customer paid less','ग्राहक ने कम भुगतान किया','HINDI');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('6943bdb2-c709-11ed-afa1-0242ac120002','customer paid more','ग्राहक ने अधिक भुगतान किया','HINDI');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('6943bf74-c709-11ed-afa1-0242ac120002','poor customer behaviour','ग्राहक का खराब व्यवहार','HINDI');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('6943c140-c709-11ed-afa1-0242ac120002','fare calculation error','किराया गिनती में गड़बड़ी','HINDI');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('6943c2f8-c709-11ed-afa1-0242ac120002','other issue','अन्य मुद्दे','HINDI');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('6943c4a6-c709-11ed-afa1-0242ac120002','pickup related issue','पिकअप संबंधित मामला','HINDI');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('6943ca8c-c709-11ed-afa1-0242ac120002','location related issue','स्थान से संबंधित समस्या','HINDI');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('6943cc3a-c709-11ed-afa1-0242ac120002','OTP issues','OTP मुद्दा','HINDI');
INSERT INTO atlas_driver_offer_bpp.issue_translation VALUES ('6943cdb6-c709-11ed-afa1-0242ac120002','app not working properly','ऐप ठीक से काम नहीं कर रहा है','HINDI');

-- add logo urls for issue_category
UPDATE atlas_driver_offer_bpp.issue_category SET logo_url = 'https://assets.juspay.in/hyper/nammayatri/images/common/ic_lost_and_found.png' WHERE id = '02d3fbe2-d5ad-4057-94e1-e84fde1ef3f6';
UPDATE atlas_driver_offer_bpp.issue_category SET logo_url = 'https://assets.juspay.in/hyper/nammayatri/images/common/ic_fare_related.png' WHERE id = '5ca814d9-66e2-4ccc-b236-40b73b705e88';
UPDATE atlas_driver_offer_bpp.issue_category SET logo_url = 'https://assets.juspay.in/hyper/nammayatri/images/common/ic_ride_related.png' WHERE id = 'a8ff40fb-88a0-440b-9763-e6c58929e5036';
UPDATE atlas_driver_offer_bpp.issue_category SET logo_url = 'https://assets.juspay.in/hyper/nammayatri/images/common/ic_app_related.png' WHERE id = '3c2970e3-f01a-4dc3-8a21-3b77f01497ea';

-- make primary key size as 36 character
UPDATE atlas_driver_offer_bpp.issue_category SET id = 'a8ff40fb-88a0-440b-9763-e6c58929e503' WHERE id = 'a8ff40fb-88a0-440b-9763-e6c58929e5036';
UPDATE atlas_driver_offer_bpp.issue_option SET issue_category_id = 'a8ff40fb-88a0-440b-9763-e6c58929e503' WHERE issue_category_id = 'a8ff40fb-88a0-440b-9763-e6c58929e5036';

ALTER TABLE atlas_driver_offer_bpp.issue_translation ALTER COLUMN id TYPE character (36);
ALTER TABLE atlas_driver_offer_bpp.issue_category ALTER COLUMN id TYPE character (36);
ALTER TABLE atlas_driver_offer_bpp.issue_option ALTER COLUMN id TYPE character (36);
ALTER TABLE atlas_driver_offer_bpp.issue_option ALTER COLUMN issue_category_id TYPE character (36);