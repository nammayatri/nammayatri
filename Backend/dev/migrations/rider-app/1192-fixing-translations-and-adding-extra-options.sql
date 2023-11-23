UPDATE atlas_app.issue_message
SET message = 'Hey, We’re sorry to hear about your lost item.'
WHERE message = 'Hey, We’re sorry to hear about your lost item';

UPDATE atlas_app.issue_option
SET option = 'Raise an issue.', label = 'REOPEN_TICKET'
WHERE id = 'awdvgsdd-bhtf-234v-8i6g-3nbtdsfvsnfv';

UPDATE atlas_app.issue_message
SET option_id = NULL
WHERE id = '123md312-ch17-u3tj-123d-febf223b12j3';

UPDATE atlas_app.issue_message
SET message = 'How should we help you?'
WHERE id = 'asdg12ev-546h-7j5j-4hrg-34jtnrgwfbhf';

UPDATE atlas_app.issue_translation
SET translation = 'ತಪ್ಪು'
where id = 'k20nff96-dv9s-6bzp-qvch-xvwc08ys801m';

INSERT INTO atlas_app.issue_message VALUES ('6w0syxuj-h9y0-qify-la08-xjia59v02lkd', 'xpjd28e5-vzie-oflc-8dqo-42xc8931yoth', null, 'Still need help?', null, 1);

INSERT INTO atlas_app.issue_option VALUES ('2imdeei3-ax77-9m0o-wo9y-51i0qpyzsdqh', 'nkm5pqj4-56hq-prdt-3s2y-9yuc1zgdy79w', '6w0syxuj-h9y0-qify-la08-xjia59v02lkd', 'Yes', null, 1);
INSERT INTO atlas_app.issue_option VALUES ('uyf9ycb1-kejn-qwlw-daci-40fj114l73jx', 'nkm5pqj4-56hq-prdt-3s2y-9yuc1zgdy79w', '6w0syxuj-h9y0-qify-la08-xjia59v02lkd', 'No', null, 2);

INSERT INTO atlas_app.issue_message VALUES ('km6vx13m-1ey9-nomi-f6eo-c6ell053rt6z', 'uyf9ycb1-kejn-qwlw-daci-40fj114l73jx', null, 'Please share more details on the lost item. You can also add images or voice notes for us to help you out better.', 'CREATE_TICKET', 1);
INSERT INTO atlas_app.issue_message VALUES ('r07j57jp-j4k8-ol4c-ga12-89lhbjxpbyel', '2imdeei3-ax77-9m0o-wo9y-51i0qpyzsdqh', null, 'We are glad to be of help. You can reach out to us in case of any other issues using help & support option in the side menu', 'END_FLOW', 1);

INSERT INTO atlas_app.issue_translation VALUES ('tcp853zx-imwt-nd7d-2lrj-7a64vwiu309s','Raise an issue.','Raise an Issue.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('jv7a9j4j-j7tz-tbwl-k98y-gsob2w2v5m02','Raise an issue.','ಸಮಸ್ಯೆ ಹೆಚ್ಚಿಸಿ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('n5x067bq-dzb6-3ppy-ummj-smydmwsx3kxp','Raise an issue.','பிரச்சினை உருவாக்கு','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('h11x4ml4-9s3t-l1sj-91zd-2jlwwi8op5o4','Raise an issue.','मुद्दा उठाएं','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('50stjs2d-7b8y-tpmu-yprk-kuwzuf67abi1','How should we help you?','How should we help you?','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('ytq5crwo-ym0a-qgre-3ca7-7lehgdncpjmu','How should we help you?','ನಾವು ನಿಮಗೆ ಹೇಗೆ ಸಹಾಯ ಮಾಡಬೇಕು?','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('s48tg5ba-hlq0-7l8u-oly9-5clmipcktr4y','How should we help you?','நாங்கள் உங்களுக்கு எப்படி உதவ வேண்டும்?','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('jqh9kbsh-n3xe-r7o8-8lrv-hpwli8jmebds','How should we help you?','हमें आपकी कैसे मदद करनी चाहिए?','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('m9xq1p5d-c13m-imlt-i9t3-bkimrxpvgfmu','Still need help?','Still need help?','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('d3u8c0ye-crfl-kjhx-ctuh-wc5j9q29uzkz','Still need help?','ಇನ್ನೂ ಸಹಾಯ ಬೇಕೇ?','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('m3yjewlj-v7jt-abas-pcev-y4ye9h9mj94e','Still need help?','இன்னும் உதவி தேவையா?','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('7qifi5gx-as3b-cfrt-g0v8-la8fiop7d1wf','Still need help?','अभी भी सहायता चाहिए?','HINDI');