-- Note : Please remove cached data from redis during deployment. Already existing issues might also need to be deleted in master (atlas_app only).
DELETE FROM atlas_app.issue_message;
DELETE FROM atlas_app.issue_option;
DELETE FROM atlas_app.issue_category;

ALTER TABLE atlas_app.issue_message
ALTER COLUMN message TYPE character varying(1000);

ALTER TABLE atlas_app.issue_translation
ALTER COLUMN sentence TYPE character varying(1000) USING sentence::character varying(1000),
ALTER COLUMN translation TYPE character varying(1000) USING translation::character varying(1000);

ALTER TABLE atlas_app.issue_category
ADD COLUMN priority int NOT NULL DEFAULT 1;

-- CATGORY -> SOS
INSERT INTO atlas_app.issue_category VALUES ('f01lail9-0hrg-elpj-skkm-2omgyhk3c2h0','SOS','ny_ic_sos_related,https://assets.juspay.in/nammayatri/images/common/ny_ic_sos_related.png', 5);

INSERT INTO atlas_app.issue_message VALUES ('go1kr3kr-4h41-sznv-2ndy-dfce7j2qzrlx', null, 'f01lail9-0hrg-elpj-skkm-2omgyhk3c2h0', 'Hey, We’re really sorry to hear you have been facing issues.', null,1);
INSERT INTO atlas_app.issue_message VALUES ('hkpdpoe5-5lny-3xkh-1oi0-7d7hkwrxabsx', null, 'f01lail9-0hrg-elpj-skkm-2omgyhk3c2h0', 'Please select the type of issue you are facing so we can help you out better', null,2);

INSERT INTO atlas_app.issue_option VALUES ('9dzrbtwg-zu2p-zmyj-584m-uk4a1vems75b', 'f01lail9-0hrg-elpj-skkm-2omgyhk3c2h0', 'hkpdpoe5-5lny-3xkh-1oi0-7d7hkwrxabsx', 'Driver misbehaved with me', null, 1);
INSERT INTO atlas_app.issue_option VALUES ('aqqdreml-zk3a-ovyg-l6ts-zbw65549e5p3', 'f01lail9-0hrg-elpj-skkm-2omgyhk3c2h0', 'hkpdpoe5-5lny-3xkh-1oi0-7d7hkwrxabsx', 'Rash driving', null, 2);
INSERT INTO atlas_app.issue_option VALUES ('qvn768jx-1j01-3d6k-dgzc-y28kdou85i74', 'f01lail9-0hrg-elpj-skkm-2omgyhk3c2h0', 'hkpdpoe5-5lny-3xkh-1oi0-7d7hkwrxabsx', 'Driver was rude', null, 3);

INSERT INTO atlas_app.issue_message VALUES ('3yug42y2-wle3-mizy-jksj-nzp60dsswhp4', '9dzrbtwg-zu2p-zmyj-584m-uk4a1vems75b', null, 'We apologize for the inconvenience. this is not the kind of experience we want our customers to have and this isn’t encouraged in Namma. Please provide additional information so that we can assist you better', 'CREATE_TICKET', 1);
INSERT INTO atlas_app.issue_message VALUES ('646r8c2t-48lu-3cvh-79sj-uk8k4ef67xwt', 'aqqdreml-zk3a-ovyg-l6ts-zbw65549e5p3', null, 'We apologize for the inconvenience. this is not the kind of experience we want our customers to have and this isn’t encouraged in Namma. Please provide additional information so that we can assist you better', 'CREATE_TICKET', 1);
INSERT INTO atlas_app.issue_message VALUES ('zi6tl2m7-vo4p-mba7-6dl7-tgpifvk2csgj', 'qvn768jx-1j01-3d6k-dgzc-y28kdou85i74', null, 'Sorry to hear that. Please provide additional information so that we can assist you better', 'CREATE_TICKET', 1);

-- CATEGORY -> PAYMENT RELATED
INSERT INTO atlas_app.issue_category VALUES ('abcdefgh-1234-5678-90ab-1234567890ab','payment related','ny_ic_payment_related,https://assets.juspay.in/nammayatri/images/common/ny_ic_payment_related.png', 4);

INSERT INTO atlas_app.issue_message VALUES ('qwertyui-5678-abcd-efgh-9876543210zy', null, 'abcdefgh-1234-5678-90ab-1234567890ab', 'Hey, We’re really sorry to hear you have been facing payment related issues.', null,1);
INSERT INTO atlas_app.issue_message VALUES ('12345678-abcd-efgh-ijkl-mnopqrstuvwx', null, 'abcdefgh-1234-5678-90ab-1234567890ab', 'Please select the type of issue you are facing so we can help you out better', null,2);

INSERT INTO atlas_app.issue_option VALUES ('zxcvbnmp-qrst-uwer-tyui-4567890abcde', 'abcdefgh-1234-5678-90ab-1234567890ab', '12345678-abcd-efgh-ijkl-mnopqrstuvwx', 'Invoice request', null, 1);
INSERT INTO atlas_app.issue_option VALUES ('asdfghjk-2345-6789-0poi-1q2w3e4r5t6y', 'abcdefgh-1234-5678-90ab-1234567890ab', '12345678-abcd-efgh-ijkl-mnopqrstuvwx', 'Multiple debits', null, 2);

INSERT INTO atlas_app.issue_message VALUES ('asvdasgv-c76f-13bg-8209-q123vhve123v', 'zxcvbnmp-qrst-uwer-tyui-4567890abcde', null, 'Go to menu \n My rides  \n Click on the particular ride  \n View invoice  \n Downlaod PDF. \n Please note : if the notification is turned off, you wont be able to download the Invoice copy', 'END_FLOW', 1);
INSERT INTO atlas_app.issue_message VALUES ('de7a9119-45jn-12v3-8209-68fb00b875ef', 'asdfghjk-2345-6789-0poi-1q2w3e4r5t6y', null, 'You can reach the driver directly through the app by selecting the specific ride under "My rides." Please note that you can contact the driver within 24 hours of the trip.', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('85zfqnuv-b92v-lycg-25i5-5kinvp9yqvvy', 'asdfghjk-2345-6789-0poi-1q2w3e4r5t6y', null, 'Was this helpful?', null, 1);

INSERT INTO atlas_app.issue_option VALUES ('1a2b3c4d-5e6f-7g8h-9i0j-abcdefgh1234', 'abcdefgh-1234-5678-90ab-1234567890ab', '85zfqnuv-b92v-lycg-25i5-5kinvp9yqvvy', 'Yes', null, 2);
INSERT INTO atlas_app.issue_option VALUES ('qwertyui-1234-5678-90ab-cdefghijklmn', 'abcdefgh-1234-5678-90ab-1234567890ab', '85zfqnuv-b92v-lycg-25i5-5kinvp9yqvvy', 'No', null, 2);

INSERT INTO atlas_app.issue_message VALUES ('mtrj567k-3vpo-5led-5va4-9i0zryxum4kq', 'qwertyui-1234-5678-90ab-cdefghijklmn', null, 'Thank you for providing the details', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('4e9iopk6-s4cc-wtbv-9d1p-kdsg9himhn1i', 'qwertyui-1234-5678-90ab-cdefghijklmn', null, 'Please provide additional information so that we can assist you better.', 'CREATE_TICKET', 2);
INSERT INTO atlas_app.issue_message VALUES ('66hcgr8h-3xzp-arlw-rkeq-3uzagva33zon', '1a2b3c4d-5e6f-7g8h-9i0j-abcdefgh1234', null, 'We''re happy to have helped you with your inquiries. Don''t hesitate to use the help feature on the app for any future questions or concerns.', 'END_FLOW', 1);

-- CATEGORY -> ACCOUNT RELATED
INSERT INTO atlas_app.issue_category VALUES ('1w4xjumg-2bvq-x6ez-voqj-ufc2m8ip2sxa','account related','ny_ic_account_related,https://assets.juspay.in/nammayatri/images/common/ny_ic_account_related.png', 3);

INSERT INTO atlas_app.issue_message VALUES ('kfoh7nau-uvst-tujx-vg0f-xx4mjiwubzcp', null, '1w4xjumg-2bvq-x6ez-voqj-ufc2m8ip2sxa', 'Hey, We’re really sorry to hear you have been facing account related issues.', null,1);
INSERT INTO atlas_app.issue_message VALUES ('orfz2jfv-ua8d-sbrm-ee1t-ecu0bnj74lhl', null, '1w4xjumg-2bvq-x6ez-voqj-ufc2m8ip2sxa', 'Please select the type of issue you are facing so we can help you out better', null,2);

INSERT INTO atlas_app.issue_option VALUES ('p7dxwprk-qr42-r9k9-jqvx-wk28r23wgr2v', '1w4xjumg-2bvq-x6ez-voqj-ufc2m8ip2sxa', 'orfz2jfv-ua8d-sbrm-ee1t-ecu0bnj74lhl', 'Phone number change', null, 1);
INSERT INTO atlas_app.issue_option VALUES ('4rzdf0q4-yc8q-3xco-d2uo-v8h5kofx8t1n', '1w4xjumg-2bvq-x6ez-voqj-ufc2m8ip2sxa', 'orfz2jfv-ua8d-sbrm-ee1t-ecu0bnj74lhl', 'How to update my Work/Home or favourite locations?', null, 2);
INSERT INTO atlas_app.issue_option VALUES ('icu519t8-h9vj-t7qw-bggr-qjlz2cr9s534', '1w4xjumg-2bvq-x6ez-voqj-ufc2m8ip2sxa', 'orfz2jfv-ua8d-sbrm-ee1t-ecu0bnj74lhl', 'How can I update the language on my app?', null, 3);

INSERT INTO atlas_app.issue_message VALUES ('zvy2njp1-op2v-7pki-latw-ff8shjb4qj3l', 'p7dxwprk-qr42-r9k9-jqvx-wk28r23wgr2v', null, 'Sorry! Currently we do not have the option, we suggest you to logout from this number and login with a new number', 'END_FLOW', 1);
INSERT INTO atlas_app.issue_message VALUES ('uibaibmz-44ob-25x2-mkro-9y7j3m8q81ps', '4rzdf0q4-yc8q-3xco-d2uo-v8h5kofx8t1n', null, 'You can update your work/home or favourite locations by navigating to three-line menu.  \n Click on favourites.', 'END_FLOW', 1);
INSERT INTO atlas_app.issue_message VALUES ('lbchsvu4-xbhn-jhjx-lkrk-owzfbr1vd7ud', 'icu519t8-h9vj-t7qw-bggr-qjlz2cr9s534', null, 'You can change the language by navigating to three-line menu and clicking on language option on the app', 'END_FLOW', 1);

-- CATEGORY -> APP RELATED
INSERT INTO atlas_app.issue_category VALUES ('xu5jlqrn-ogn5-l8vx-d6f2-fi1fr1q3u8zh','app related','ny_ic_app_related_issue,https://assets.juspay.in/nammayatri/images/common/ny_ic_app_related_issue.png', 1);

INSERT INTO atlas_app.issue_message VALUES ('kq7a9119-c76f-42f2-8209-68fb00b875ef', null, 'xu5jlqrn-ogn5-l8vx-d6f2-fi1fr1q3u8zh', 'Hey, We’re really sorry to hear you have been facing app related issues.', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('ly7a9119-c76f-42f2-8209-68fb00b875ef', null, 'xu5jlqrn-ogn5-l8vx-d6f2-fi1fr1q3u8zh', 'Please select the type of issue you are facing so we can help you out better', null, 2);

INSERT INTO atlas_app.issue_option VALUES ('8e23eecf-55d8-4a15-9cd5-670246f2e443','xu5jlqrn-ogn5-l8vx-d6f2-fi1fr1q3u8zh', 'ly7a9119-c76f-42f2-8209-68fb00b875ef', 'Driver location is not being updated', null, 1);
INSERT INTO atlas_app.issue_option VALUES ('251a4cd0-efda-4d43-94b7-ebd6f7164c11','xu5jlqrn-ogn5-l8vx-d6f2-fi1fr1q3u8zh', 'ly7a9119-c76f-42f2-8209-68fb00b875ef', 'I am not receiving OTP to login', null, 2);
INSERT INTO atlas_app.issue_option VALUES ('0028272a-5019-4785-a153-ae463f0bd1a4','xu5jlqrn-ogn5-l8vx-d6f2-fi1fr1q3u8zh', 'ly7a9119-c76f-42f2-8209-68fb00b875ef', 'App is stuck on previous ride', null, 3);
INSERT INTO atlas_app.issue_option VALUES ('30052650-c9b6-4801-82a4-1fcb7c6bce76','xu5jlqrn-ogn5-l8vx-d6f2-fi1fr1q3u8zh', 'ly7a9119-c76f-42f2-8209-68fb00b875ef', 'Unable to book a ride', null, 4);
INSERT INTO atlas_app.issue_option VALUES ('o0yjurs9-zl8i-k4gq-1roa-6winxfmnfxf7','xu5jlqrn-ogn5-l8vx-d6f2-fi1fr1q3u8zh', 'ly7a9119-c76f-42f2-8209-68fb00b875ef', 'My app is not responding', null, 5);

INSERT INTO atlas_app.issue_message VALUES ('g7zfveqq-a0dp-oref-p513-0maptw9jw7cp', '8e23eecf-55d8-4a15-9cd5-670246f2e443', null, 'Kindly try restarting the app to access the updated location. \n If the problem persists, contact the driver directly for the estimated time of arrival (ETA).', 'END_FLOW', 1);
INSERT INTO atlas_app.issue_message VALUES ('8pd28q10-o5lf-f7g3-r86d-ex3655x5li8d', '251a4cd0-efda-4d43-94b7-ebd6f7164c11', null, 'Your account may be blocked, Create a ticket  \n Reasons leading to account block  \n * If you attempt several booking cancellations over a period of time, your Namma Yatri account will be temporarily blocked.  \n * Account can also be blocked permanently if the system detects any unusual behavior/activities against Namma Yatri’s terms and conditions.', 'END_FLOW', 1);
INSERT INTO atlas_app.issue_message VALUES ('78kpl9ps-kug9-u2wl-nzng-j5lfukjgwzqd', '0028272a-5019-4785-a153-ae463f0bd1a4', null, 'Sorry to hear that. Please share you contact details so that we can assist you better', 'CREATE_TICKET', 1);
INSERT INTO atlas_app.issue_message VALUES ('u6p04zp4-oxtw-ij58-1kp2-ctpqb95a5uga', '30052650-c9b6-4801-82a4-1fcb7c6bce76', null, 'Try booking a ride after 5mins.', 'END_FLOW', 1);
INSERT INTO atlas_app.issue_message VALUES ('qmplr56k-x9qm-oqrx-u1a2-nfbii6jckwm3', 'o0yjurs9-zl8i-k4gq-1roa-6winxfmnfxf7', null, 'We''re sorry that you are facing this issue. Kindly follow the following steps: \n - Please make sure that you are connected to the internet with a strong signal \n - Update the app \n - Check the GPS settings and ensure that it is turned on for Namma Yatri', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('nr1ufdqu-w3di-0jc3-xa3x-992a6sxuo1l2', 'o0yjurs9-zl8i-k4gq-1roa-6winxfmnfxf7', null, 'Was this helpful?', null, 1);

INSERT INTO atlas_app.issue_option VALUES ('bysr7d11-8q3w-i3d3-0ib8-0pflanzrk43u', 'xu5jlqrn-ogn5-l8vx-d6f2-fi1fr1q3u8zh', 'nr1ufdqu-w3di-0jc3-xa3x-992a6sxuo1l2', 'Yes', null, 2);
INSERT INTO atlas_app.issue_option VALUES ('jw82axx2-vj8v-bk01-yqh3-03ktrnefrmnt', 'xu5jlqrn-ogn5-l8vx-d6f2-fi1fr1q3u8zh', 'nr1ufdqu-w3di-0jc3-xa3x-992a6sxuo1l2', 'No', null, 2);

INSERT INTO atlas_app.issue_message VALUES ('yxg6dyj1-3d51-qk1r-u4g7-trj2u26hqx1k', 'jw82axx2-vj8v-bk01-yqh3-03ktrnefrmnt', null, 'Thank you for providing the details', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('z6wcgeit-z2m3-wctw-1m0f-z6ovt8fu5g4l', 'jw82axx2-vj8v-bk01-yqh3-03ktrnefrmnt', null, 'Please provide additional information so that we can assist you better.', 'CREATE_TICKET', 2);
INSERT INTO atlas_app.issue_message VALUES ('1gn3gbc8-wsc2-09qs-ajsl-pym80rqtj80d', 'bysr7d11-8q3w-i3d3-0ib8-0pflanzrk43u', null, 'We''re happy to have helped you with your inquiries. Don''t hesitate to use the help feature on the app for any future questions or concerns.', 'END_FLOW', 1);

-- CATEGORY -> RIDE RELATED
INSERT INTO atlas_app.issue_category VALUES ('ziig3kxh-v0xc-kh0t-q6p1-f1v2n8ucs0kj','ride related','ny_ic_ride_related_issue,https://assets.juspay.in/nammayatri/images/common/ny_ic_ride_related_issue.png', 2);

INSERT INTO atlas_app.issue_message VALUES ('ylaru5lf-d2o8-8tqg-u92q-gjscry8q0pps', null, 'ziig3kxh-v0xc-kh0t-q6p1-f1v2n8ucs0kj', 'Hey, We’re really sorry to hear you have been facing ride related issues.', null,1);
INSERT INTO atlas_app.issue_message VALUES ('3iz3cfp2-jwdv-ikuq-23r5-tg6sugm3pwp2', null, 'ziig3kxh-v0xc-kh0t-q6p1-f1v2n8ucs0kj', 'Please select the type of issue you are facing so we can help you out better', null,2);

INSERT INTO atlas_app.issue_option VALUES ('hhm3wka4-xd0r-cmn1-4mbp-zn3ul50pzhkl','ziig3kxh-v0xc-kh0t-q6p1-f1v2n8ucs0kj', '3iz3cfp2-jwdv-ikuq-23r5-tg6sugm3pwp2', 'Change of destination', null, 1);
INSERT INTO atlas_app.issue_option VALUES ('ajmxpiyi-dyij-jxa6-vqnn-8y138j8sl8wn','ziig3kxh-v0xc-kh0t-q6p1-f1v2n8ucs0kj', '3iz3cfp2-jwdv-ikuq-23r5-tg6sugm3pwp2', 'Unable to cancel the ride', null, 2);
INSERT INTO atlas_app.issue_option VALUES ('9bcf1fbd-0d32-48ea-a267-0de4f0ae3115','ziig3kxh-v0xc-kh0t-q6p1-f1v2n8ucs0kj', '3iz3cfp2-jwdv-ikuq-23r5-tg6sugm3pwp2', 'Unable to check ride status', null, 3);
INSERT INTO atlas_app.issue_option VALUES ('gs52d30s-g918-6pwi-brrs-y5b05b8lya7b','ziig3kxh-v0xc-kh0t-q6p1-f1v2n8ucs0kj', '3iz3cfp2-jwdv-ikuq-23r5-tg6sugm3pwp2', 'unable to contact the driver', null, 4);
INSERT INTO atlas_app.issue_option VALUES ('eog4fbfl-je8r-hln2-0ll5-u6fj462gltht','ziig3kxh-v0xc-kh0t-q6p1-f1v2n8ucs0kj', '3iz3cfp2-jwdv-ikuq-23r5-tg6sugm3pwp2', 'Unable to get rides', null, 5);
INSERT INTO atlas_app.issue_option VALUES ('2yciywl6-rbjh-vsjo-d3z1-r1ytevv0h4oj','ziig3kxh-v0xc-kh0t-q6p1-f1v2n8ucs0kj', '3iz3cfp2-jwdv-ikuq-23r5-tg6sugm3pwp2', 'Driver has not arrived for pickup', null, 6);
INSERT INTO atlas_app.issue_option VALUES ('b9m8ztgh-x9p1-eepm-p4pu-8tada97ngo1x','ziig3kxh-v0xc-kh0t-q6p1-f1v2n8ucs0kj', '3iz3cfp2-jwdv-ikuq-23r5-tg6sugm3pwp2', 'Excess amount paid to the driver', null, 7);
INSERT INTO atlas_app.issue_option VALUES ('lpab2oxv-14td-jubb-i5df-83rq3sh55iyu','ziig3kxh-v0xc-kh0t-q6p1-f1v2n8ucs0kj', '3iz3cfp2-jwdv-ikuq-23r5-tg6sugm3pwp2', 'I was charged more than the fare shown', 'DOWNLOAD_INVOICE', 8);

INSERT INTO atlas_app.issue_message VALUES ('5zy6sg2k-4yfr-ysgp-nsmv-f9fwduneqn9q', 'hhm3wka4-xd0r-cmn1-4mbp-zn3ul50pzhkl', null, 'Currently, this option is unavailable, but we are working on implementing it soon', 'END_FLOW', 1);
INSERT INTO atlas_app.issue_message VALUES ('29co7ikr-b7kj-5lfs-sc91-9b8hh0ujlzur', 'ajmxpiyi-dyij-jxa6-vqnn-8y138j8sl8wn', null, 'Force close the app and restart the app \n On the bottom of the page you will be able to see cancel ride option \n Click on cancel ride and you will get an option to call the driver / cancel ride  \n Click on cancl ride and select the reason for cancellation and click cancel ride.', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('lladwv59-vb6d-lgbf-8l9j-ljtmqd7pko99', 'ajmxpiyi-dyij-jxa6-vqnn-8y138j8sl8wn', null, 'Was this helpful?', null, 2);
INSERT INTO atlas_app.issue_message VALUES ('maq7ycfx-bixq-1qzc-guml-u4kmyc7ood6p', '9bcf1fbd-0d32-48ea-a267-0de4f0ae3115', null, 'Force close the app and check once again.  \n If the issue is still not fixed uninstall the app and reinstall the app / try to update the app if not updated.', 'END_FLOW', 1);
INSERT INTO atlas_app.issue_message VALUES ('wvmx3lj2-h6vz-gpo4-i8n3-vntvsfek6gxb', 'gs52d30s-g918-6pwi-brrs-y5b05b8lya7b', null, 'Force close the app and restart the app \n You will be able to contact the driver via the call option available on the ride page.', 'END_FLOW', 1);
INSERT INTO atlas_app.issue_message VALUES ('9dr5mulj-kp65-oqy1-5ya4-6zak6he919qe', 'eog4fbfl-je8r-hln2-0ll5-u6fj462gltht', null, 'Because of the high demand, it is possible that you may not get a ride. We recommend trying to book a ride again in 5 minutes', 'END_FLOW', 1);
INSERT INTO atlas_app.issue_message VALUES ('karxfqyp-o52y-vpbs-0ztx-kup7ou4tg3bn', '2yciywl6-rbjh-vsjo-d3z1-r1ytevv0h4oj', null, 'Kindly call the driver. If the driver does not respond, you may cancel the ride and book a new one', 'END_FLOW', 1);
INSERT INTO atlas_app.issue_message VALUES ('d9n6y03b-q1ta-v3lv-gnsf-s3y9ydp574rp', 'b9m8ztgh-x9p1-eepm-p4pu-8tada97ngo1x', null, 'Sorry to hear that. Please provide additional information so that we can assist you better', 'CREATE_TICKET', 1);
INSERT INTO atlas_app.issue_message VALUES ('bimt3gzi-9n2t-ox6o-c88g-kam2nn3fjue5', 'lpab2oxv-14td-jubb-i5df-83rq3sh55iyu', null, 'We will certainly help you in this matter. This is the attached breakup of the fare. \n Please Note: To receive additional assistance, keep the app open. Check the invoice copy by minimizing the screen to avoid recreating a ticket.', 'DOWNLOAD_INVOICE', 1);
INSERT INTO atlas_app.issue_message VALUES ('uzjkl5l8-n6k6-hwj2-e32p-3xe4ggek6s6u', 'lpab2oxv-14td-jubb-i5df-83rq3sh55iyu', null, 'Was this helpful?', null, 2);

INSERT INTO atlas_app.issue_option VALUES ('91llr9b4-2h3p-vw26-w6a2-iwbadb0m06iy', 'ziig3kxh-v0xc-kh0t-q6p1-f1v2n8ucs0kj', 'lladwv59-vb6d-lgbf-8l9j-ljtmqd7pko99', 'Yes', null, 2);
INSERT INTO atlas_app.issue_option VALUES ('f3hm302d-i11z-ggek-jfnq-r8rffioiyd6y', 'ziig3kxh-v0xc-kh0t-q6p1-f1v2n8ucs0kj', 'lladwv59-vb6d-lgbf-8l9j-ljtmqd7pko99', 'No', null, 2);
INSERT INTO atlas_app.issue_option VALUES ('yu1js794-dvij-q4rx-mv7t-qp4fri4cbu5y', 'ziig3kxh-v0xc-kh0t-q6p1-f1v2n8ucs0kj', 'uzjkl5l8-n6k6-hwj2-e32p-3xe4ggek6s6u', 'Yes', null, 1);
INSERT INTO atlas_app.issue_option VALUES ('xeliu6n8-gg4z-d2z5-d94f-a7knlr8nk156', 'ziig3kxh-v0xc-kh0t-q6p1-f1v2n8ucs0kj', 'uzjkl5l8-n6k6-hwj2-e32p-3xe4ggek6s6u', 'No', null, 2);

INSERT INTO atlas_app.issue_message VALUES ('yaoa9ny8-gtl0-dfkl-afay-qlmhfv8vphwl', 'f3hm302d-i11z-ggek-jfnq-r8rffioiyd6y', null, 'Thank you for providing the details', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('y2ennqwo-r2b1-dd19-tjfz-jofazkaalze3', 'f3hm302d-i11z-ggek-jfnq-r8rffioiyd6y', null, 'Please provide additional information so that we can assist you better.', 'CREATE_TICKET', 2);
INSERT INTO atlas_app.issue_message VALUES ('o9ril9m2-16t2-78fy-h1pt-fu6ltljtmpyi', '91llr9b4-2h3p-vw26-w6a2-iwbadb0m06iy', null, 'We''re happy to have helped you with your inquiries. Don''t hesitate to use the help feature on the app for any future questions or concerns.', 'END_FLOW', 1);
INSERT INTO atlas_app.issue_message VALUES ('zb6ywral-rvkm-6tk6-5wot-et1vcb3syoif', 'xeliu6n8-gg4z-d2z5-d94f-a7knlr8nk156', null, 'Thank you for providing the details', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('7fhaassd-8hne-bop0-8k6s-eowdbqpffoag', 'xeliu6n8-gg4z-d2z5-d94f-a7knlr8nk156', null, 'Please provide additional information so that we can assist you better.', 'CREATE_TICKET', 2);
INSERT INTO atlas_app.issue_message VALUES ('2ee2f5iz-m16b-gxhh-xsos-5tuft3sccspb', 'yu1js794-dvij-q4rx-mv7t-qp4fri4cbu5y', null, 'We''re happy to have helped you with your inquiries. Don''t hesitate to use the help feature on the app for any future questions or concerns.', 'END_FLOW', 1);

-- CATEGORY -> LOST AND FOUND
INSERT INTO atlas_app.issue_category VALUES ('nkm5pqj4-56hq-prdt-3s2y-9yuc1zgdy79w','lost and found','ny_ic_lost_and_found,https://assets.juspay.in/nammayatri/images/common/ny_ic_lost_and_found.png', 6);

INSERT INTO atlas_app.issue_message VALUES ('nbp2xa17-aijc-3s0q-xp0f-3anugh8ds8rp', null, 'nkm5pqj4-56hq-prdt-3s2y-9yuc1zgdy79w', 'Hey, We’re sorry to hear about your lost item', null,1);
INSERT INTO atlas_app.issue_message VALUES ('hhoeba3o-v9v2-2m5l-uvk6-nfrzjjn64pcw', null, 'nkm5pqj4-56hq-prdt-3s2y-9yuc1zgdy79w', 'How do you wish to resolve this issue?', null,2);

INSERT INTO atlas_app.issue_option VALUES ('xpjd28e5-vzie-oflc-8dqo-42xc8931yoth', 'nkm5pqj4-56hq-prdt-3s2y-9yuc1zgdy79w', 'hhoeba3o-v9v2-2m5l-uvk6-nfrzjjn64pcw', 'call the driver', 'CALL_DRIVER', 1);
INSERT INTO atlas_app.issue_option VALUES ('pxz7sc8s-2uaq-s00y-1ii8-4luatg4bz58g', 'nkm5pqj4-56hq-prdt-3s2y-9yuc1zgdy79w', 'hhoeba3o-v9v2-2m5l-uvk6-nfrzjjn64pcw', 'report lost item to support', null, 2);

INSERT INTO atlas_app.issue_message VALUES ('x9c3ox41-8gr0-xili-8nk8-0zpkrk1spteg', 'pxz7sc8s-2uaq-s00y-1ii8-4luatg4bz58g', null, 'Please share more details on the lost item. You can also add images or voice notes for us to help you out better.', 'CREATE_TICKET', 1);

-- CATEGORY - OTHERS
INSERT INTO atlas_app.issue_category VALUES ('s5fkl23p-gr2l-z5qy-gjn5-gjpjvk6lp4u2','other','ny_ic_other_issues,https://assets.juspay.in/nammayatri/images/common/ny_ic_other_issues.png', 7);

INSERT INTO atlas_app.issue_message VALUES ('w52qwf6l-xabm-6w0c-lcw1-a58gfw19rqny', null, 's5fkl23p-gr2l-z5qy-gjn5-gjpjvk6lp4u2', 'Please select the type of issue you are facing so we can help you out better', null,1);

INSERT INTO atlas_app.issue_option VALUES ('jr4hxq6i-r0o9-xbng-q5zf-uur3q10exmtt','s5fkl23p-gr2l-z5qy-gjn5-gjpjvk6lp4u2', 'w52qwf6l-xabm-6w0c-lcw1-a58gfw19rqny', 'Existing follow up', null, 1);
INSERT INTO atlas_app.issue_option VALUES ('fehzz25n-ucoh-dj4s-r7o4-w7rgdm00mhnr','s5fkl23p-gr2l-z5qy-gjn5-gjpjvk6lp4u2', 'w52qwf6l-xabm-6w0c-lcw1-a58gfw19rqny', 'Feedback', null, 2);
INSERT INTO atlas_app.issue_option VALUES ('wpjyrwl0-6fra-7rvj-jx47-gi5dou7z7km2','s5fkl23p-gr2l-z5qy-gjn5-gjpjvk6lp4u2', 'w52qwf6l-xabm-6w0c-lcw1-a58gfw19rqny', 'Promotion', null, 3);
INSERT INTO atlas_app.issue_option VALUES ('1nnf0eog-x0yw-nxc8-5hxs-em7j388qcobn','s5fkl23p-gr2l-z5qy-gjn5-gjpjvk6lp4u2', 'w52qwf6l-xabm-6w0c-lcw1-a58gfw19rqny', 'Suggestions', null, 4);
INSERT INTO atlas_app.issue_option VALUES ('ul3d5l7s-9eug-30h4-ex1u-7e6ray7cfvfv','s5fkl23p-gr2l-z5qy-gjn5-gjpjvk6lp4u2', 'w52qwf6l-xabm-6w0c-lcw1-a58gfw19rqny', 'About namma yatri', null, 5);

INSERT INTO atlas_app.issue_message VALUES ('la5k66sv-0k1e-n2yx-y9iv-obqj8fwaxcdo', 'jr4hxq6i-r0o9-xbng-q5zf-uur3q10exmtt', null, 'Please write to us at nammayatri.support@juspay.in with the contact details / Ride ID', 'END_FLOW', 1);
INSERT INTO atlas_app.issue_message VALUES ('1orchx7b-e12l-czks-y7ka-a4dy03p1y5ei', 'fehzz25n-ucoh-dj4s-r7o4-w7rgdm00mhnr', null, 'Please write to us at nammayatri.support@juspay.in', 'END_FLOW', 1);
INSERT INTO atlas_app.issue_message VALUES ('zk2w0vxn-ggt1-rweq-g06n-hz3t8y4r74w6', 'wpjyrwl0-6fra-7rvj-jx47-gi5dou7z7km2', null, 'Please write to us at nammayatri.support@juspay.in', 'END_FLOW', 1);
INSERT INTO atlas_app.issue_message VALUES ('9c3iiumm-ds0c-ingy-6nk8-ozn18mrf4plo', '1nnf0eog-x0yw-nxc8-5hxs-em7j388qcobn', null, 'Please write to us at nammayatri.support@juspay.in', 'END_FLOW', 1);
INSERT INTO atlas_app.issue_message VALUES ('xmx7coto-mv7h-3onk-qz14-gm6l16eq5jbs', 'ul3d5l7s-9eug-30h4-ex1u-7e6ray7cfvfv', null, 'Namma Yatri is a Direct-to-Driver app. There is no commission or middle-men. What you pay goes 100% to the Driver and his family!', 'END_FLOW', 1);

--TRANSLATIONS
INSERT INTO atlas_app.issue_translation VALUES ('v9embwl6-t5tn-qud1-xc6e-e0czozhkqied','SOS','SOS','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('uo9zj8b5-1xce-wv8b-nxcr-fx9sqcab19g4','SOS','SOS','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('juquwopx-0jrf-496y-n0jj-l7udb8gtpt1j','SOS','SOS','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('d33ttcl0-jzvn-uos3-h8k8-vyhek2c8i1rh','SOS','SOS','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('9runvwvd-ct2s-5l3h-x722-eoqfzb98y5sy','Hey, We’re really sorry to hear you have been facing issues.','Hey, We’re really sorry to hear you have been facing issues.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('13ukahgg-kcdi-ymih-d8fz-bigkq7l0443f','Hey, We’re really sorry to hear you have been facing issues.','ಹೇ, ನೀವು ಸಮಸ್ಯೆಗಳನ್ನು ಎದುರಿಸುತ್ತಿರುವುದನ್ನು ಕೇಳಲು ನಾವು ನಿಜವಾಗಿಯೂ ವಿಷಾದಿಸುತ್ತೇವೆ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('vy5umlqm-j836-xgd0-436o-9oar0d78ehq4','Hey, We’re really sorry to hear you have been facing issues.','ஹாய், நீங்கள் பிரச்சனைகளை எதிர்கொண்டுள்ளதைக் கேட்டு வருந்துகிறோம்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('p8nc2cqz-0si0-1odc-imde-ipkkgktgpxhf','Hey, We’re really sorry to hear you have been facing issues.','हे, हमें यह जानकर सचमुच दुख हुआ कि आपको समस्याओं का सामना करना पड़ रहा है।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('65eho694-q7qu-b254-c101-detio8x4liou','Driver misbehaved with me','Driver misbehaved with me','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('zv1m53bn-fq86-8x9g-d6lw-fyaut1t70o8s','Driver misbehaved with me','ಚಾಲಕ ನನ್ನೊಂದಿಗೆ ಅನುಚಿತವಾಗಿ ವರ್ತಿಸಿದ್ದಾನೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('2o0opy4n-bw8f-qqnh-ciqh-r4u5ilrf52mg','Driver misbehaved with me','டிரைவர் என்னிடம் தவறாக நடந்து கொண்டார்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('a7gmmy8c-f5wg-8wfb-0c31-c8cy1gwe9msh','Driver misbehaved with me','ड्राइवर ने मेरे साथ दुर्व्यवहार किया','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('onl6cnl0-xymx-91qs-4rvu-kub18w0do8ot','Rash driving','Rash driving','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('ycehieih-s1a3-64xd-5h36-6qn1gu3o4jnu','Rash driving','ದುಡುಕಿನ ಚಾಲನೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('w56kyt0k-xvvw-55da-rxh0-mj9eql91jkkx','Rash driving','அவசரமாக ஓட்டுதல்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('iqvxss4p-i1a4-tpwx-cfst-whyovcrs1rmo','Rash driving','लापरवाही से गाड़ी चलाना','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('shr9hckr-j3i7-qs90-h9ux-pwb4ad39fva0','Driver was rude','Driver was rude','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('vcdxrdui-gy0c-q8yl-4duc-fy6204o962cv','Driver was rude','ಚಾಲಕ ಅಸಭ್ಯವಾಗಿ ವರ್ತಿಸಿದ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('0gs4sljc-68cg-lsw6-pu09-bev8k6jn3qwg','Driver was rude','டிரைவர் முரட்டுத்தனமாக நடந்து கொண்டார்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('dqkrlpj3-s0vw-tx80-a1n9-3j42wy8tw9pl','Driver was rude','ड्राइवर असभ्य था','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('pyxb66m7-vvb3-ygcr-o6vx-trdshc60ia0u','We apologize for the inconvenience. this is not the kind of experience we want our customers to have and this isn’t encouraged in Namma. Please provide additional information so that we can assist you better','We apologize for the inconvenience. this is not the kind of experience we want our customers to have and this isn’t encouraged in Namma. Please provide additional information so that we can assist you better','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('ps89lsdc-066b-8s08-348w-x5fvlr8uumfc','We apologize for the inconvenience. this is not the kind of experience we want our customers to have and this isn’t encouraged in Namma. Please provide additional information so that we can assist you better','ಅನಾನುಕೂಲತೆಗಾಗಿ ನಾವು ಕ್ಷಮೆಯಾಚಿಸುತ್ತೇವೆ. ಇದು ನಮ್ಮ ಗ್ರಾಹಕರು ಹೊಂದಬೇಕೆಂದು ನಾವು ಬಯಸುವ ರೀತಿಯ ಅನುಭವವಲ್ಲ ಮತ್ತು ಇದನ್ನು ನಮ್ಮಲ್ಲಿ ಪ್ರೋತ್ಸಾಹಿಸಲಾಗಿಲ್ಲ. ದಯವಿಟ್ಟು ಹೆಚ್ಚುವರಿ ಮಾಹಿತಿಯನ್ನು ಒದಗಿಸಿ ಇದರಿಂದ ನಾವು ನಿಮಗೆ ಉತ್ತಮವಾಗಿ ಸಹಾಯ ಮಾಡಬಹುದು','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('kax9evp1-9a1q-z90u-j9t7-q4kvsoe1gwps','We apologize for the inconvenience. this is not the kind of experience we want our customers to have and this isn’t encouraged in Namma. Please provide additional information so that we can assist you better','சிரமத்திற்கு வருந்துகிறோம். இது எங்கள் வாடிக்கையாளர்களுக்கு இருக்க வேண்டும் என்று நாங்கள் விரும்பும் அனுபவமல்ல, இது நம்மில் ஊக்குவிக்கப்படவில்லை. தயவு செய்து கூடுதல் தகவலை வழங்கவும், நாங்கள் உங்களுக்கு சிறப்பாக உதவ முடியும்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('zika60ah-8wi9-xbrh-cp3l-5u1djpxbj14r','We apologize for the inconvenience. this is not the kind of experience we want our customers to have and this isn’t encouraged in Namma. Please provide additional information so that we can assist you better','असुविधा के लिए हम खेद व्यक्त करते हैं। यह उस तरह का अनुभव नहीं है जैसा हम चाहते हैं कि हमारे ग्राहकों को मिले और नम्मा में इसे प्रोत्साहित नहीं किया जाता है। कृपया अतिरिक्त जानकारी प्रदान करें ताकि हम आपकी बेहतर सहायता कर सकें','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('4izdm9se-pb29-svgn-j27h-ugectgx42zaj','Sorry to hear that. Please provide additional information so that we can assist you better','Sorry to hear that. Please provide additional information so that we can assist you better','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('7d76xxx4-vqr8-8dof-zzeo-6oe3wniwepb6','Sorry to hear that. Please provide additional information so that we can assist you better','ಅದನ್ನು ಕೇಳಲು ಕ್ಷಮಿಸಿ. ದಯವಿಟ್ಟು ಹೆಚ್ಚುವರಿ ಮಾಹಿತಿಯನ್ನು ಒದಗಿಸಿ ಇದರಿಂದ ನಾವು ನಿಮಗೆ ಉತ್ತಮವಾಗಿ ಸಹಾಯ ಮಾಡಬಹುದು','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('8uy54t3r-c9ds-6x3u-nofm-3m4mp7x0k9um','Sorry to hear that. Please provide additional information so that we can assist you better','மன்னிக்கவும். தயவு செய்து கூடுதல் தகவலை வழங்கவும், நாங்கள் உங்களுக்கு சிறப்பாக உதவ முடியும்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('pqh041jc-9lyk-opcs-djax-n8g8mfot62r1','Sorry to hear that. Please provide additional information so that we can assist you better','ऐसा सुनने के लिए क्षमा करें। कृपया अतिरिक्त जानकारी प्रदान करें ताकि हम आपकी बेहतर सहायता कर सकें','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('rniwflie-v3mf-xzk3-0ep5-3o8s0txhmx4n','payment related','payment related','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('a0y26kgq-f06c-77ix-m3k1-71m0ayop981y','payment related','ಪಾವತಿಗೆ ಸಂಬಂಧಿಸಿದೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('pkdpkn0i-k1o3-s6x6-r8v8-ay63vtjqoami','payment related','கட்டணம் தொடர்பான','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('xmztq0js-lwrp-crca-2mjx-8aoz6xvvttyv','payment related','भुगतान संबंधी','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('3e68956w-llho-65zo-z6mr-xcgxqbtmwpih','Hey, We’re really sorry to hear you have been facing payment related issues.','Hey, We’re really sorry to hear you have been facing payment related issues.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('0y2303by-kznn-8lax-h1b8-d0ga3wsl7tmt','Hey, We’re really sorry to hear you have been facing payment related issues.','ಹೇ, ನೀವು ಪಾವತಿ ಸಂಬಂಧಿತ ಸಮಸ್ಯೆಗಳನ್ನು ಎದುರಿಸುತ್ತಿರುವುದನ್ನು ಕೇಳಲು ನಾವು ನಿಜವಾಗಿಯೂ ವಿಷಾದಿಸುತ್ತೇವೆ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('3531bffh-qrag-ju47-nvxz-okruagn1i34o','Hey, We’re really sorry to hear you have been facing payment related issues.','ஹாய், பணம் செலுத்துதல் தொடர்பான சிக்கல்களை நீங்கள் எதிர்கொண்டுள்ளதைக் கேட்டு வருந்துகிறோம்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('gccmeatb-4xkz-04kh-ykw8-psn16gk6avgd','Hey, We’re really sorry to hear you have been facing payment related issues.','हे, हमें यह जानकर सचमुच दुख हुआ कि आपको भुगतान संबंधी समस्याओं का सामना करना पड़ रहा है।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('1gtwjb89-srqv-9thi-9byn-4fzqkzpu3wu0','Invoice request','Invoice request','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('nmekpcrg-98gb-8pxj-ytew-9ceo4q5vemnc','Invoice request','ಸರಕುಪಟ್ಟಿ ವಿನಂತಿ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('7o88ge1l-30ed-i0wf-lo8x-4edin5ilzoy0','Invoice request','விலைப்பட்டியல் கோரிக்கை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('8yg1fo0d-4cpb-xjzu-4edv-hx1diktans35','Invoice request','बिल के लिए अनुरोध','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('ylcxpxyg-7ckg-s8vc-lnzv-3skguhde75do','Multiple debits','Multiple debits','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('px2pkut1-83yv-feos-hhxx-eujb9lcgbxls','Multiple debits','ಬಹು ಡೆಬಿಟ್‌ಗಳು','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('yvm0h6m1-this-76j1-g2jz-zllxumo1ts4r','Multiple debits','பல பற்றுகள்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('iwqjzx7j-1ici-k96f-5p6y-eadv4xa1cqf8','Multiple debits','एकाधिक डेबिट','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('mvjsib5m-b5p0-5769-bxze-q64xldsq4cqt','Go to menu \n My rides  \n Click on the particular ride  \n View invoice  \n Downlaod PDF. \n Please note : if the notification is turned off, you wont be able to download the Invoice copy','Go to menu \n My rides  \n Click on the particular ride  \n View invoice  \n Downlaod PDF. \n Please note : if the notification is turned off, you wont be able to download the Invoice copy','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('tpmp94vd-tbtz-yfxt-px19-3ev8j7jhx7wj','Go to menu \n My rides  \n Click on the particular ride  \n View invoice  \n Downlaod PDF. \n Please note : if the notification is turned off, you wont be able to download the Invoice copy','ಮೆನುಗೆ ಹೋಗಿ \n ನನ್ನ ಸವಾರಿಗಳು  \n ನಿರ್ದಿಷ್ಟ ರೈಡ್ ಅನ್ನು ಕ್ಲಿಕ್ ಮಾಡಿ  \n ಇನ್‌ವಾಯ್ಸ್ ವೀಕ್ಷಿಸಿ  \n PDF ಅನ್ನು ಡೌನ್‌ಲೋಡ್ ಮಾಡಿ. \n ದಯವಿಟ್ಟು ಗಮನಿಸಿ : ಅಧಿಸೂಚನೆಯನ್ನು ಆಫ್ ಮಾಡಿದ್ದರೆ, ನೀವು ಇನ್‌ವಾಯ್ಸ್ ನಕಲನ್ನು ಡೌನ್‌ಲೋಡ್ ಮಾಡಲು ಸಾಧ್ಯವಾಗುವುದಿಲ್ಲ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('wai2g8ai-6xz9-ko8o-xvo9-9qfxqsrxi64k','Go to menu \n My rides  \n Click on the particular ride  \n View invoice  \n Downlaod PDF. \n Please note : if the notification is turned off, you wont be able to download the Invoice copy','மெனுவிற்கு செல்க \n எனது சவாரிகள்  \n குறிப்பிட்ட சவாரியைக் கிளிக் செய்யவும்  \n விலைப்பட்டியலைப் பார்க்கவும் {IN}PDF ஐப் பதிவிறக்கவும்.{IN}கவனிக்கவும்: அறிவிப்பு முடக்கப்பட்டிருந்தால், நீங்கள் விலைப்பட்டியல் நகலைப் பதிவிறக்க முடியாது','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('808kcokt-ecgz-brkg-4d2r-d1ropuckne2p','Go to menu \n My rides  \n Click on the particular ride  \n View invoice  \n Downlaod PDF. \n Please note : if the notification is turned off, you wont be able to download the Invoice copy','मेनू पर जाएँ  \n मेरी सवारी  \n विशेष सवारी पर क्लिक करें  \n चालान देखें  \n पीडीएफ डाउनलोड करें। \n कृपया ध्यान दें: यदि अधिसूचना बंद है, तो आप चालान की प्रति डाउनलोड नहीं कर पाएंगे','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('hi8iij5t-c2z2-qdea-eubj-hskjfztlmmcd','You can reach the driver directly through the app by selecting the specific ride under "My rides." Please note that you can contact the driver within 24 hours of the trip.','You can reach the driver directly through the app by selecting the specific ride under "My rides." Please note that you can contact the driver within 24 hours of the trip.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('njn10p71-bubx-7wjm-qbb0-8r4cgq4yes5f','You can reach the driver directly through the app by selecting the specific ride under "My rides." Please note that you can contact the driver within 24 hours of the trip.','"ನನ್ನ ಸವಾರಿಗಳು" ಅಡಿಯಲ್ಲಿ ನಿರ್ದಿಷ್ಟ ರೈಡ್ ಅನ್ನು ಆಯ್ಕೆ ಮಾಡುವ ಮೂಲಕ ನೀವು ನೇರವಾಗಿ ಅಪ್ಲಿಕೇಶನ್ ಮೂಲಕ ಚಾಲಕರನ್ನು ತಲುಪಬಹುದು. ಪ್ರಯಾಣದ 24 ಗಂಟೆಗಳ ಒಳಗೆ ನೀವು ಚಾಲಕನನ್ನು ಸಂಪರ್ಕಿಸಬಹುದು ಎಂಬುದನ್ನು ದಯವಿಟ್ಟು ಗಮನಿಸಿ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('usu0lfvy-1iks-ntfe-z8od-w4sfsp1u011o','You can reach the driver directly through the app by selecting the specific ride under "My rides." Please note that you can contact the driver within 24 hours of the trip.','"எனது சவாரிகள்" என்பதன் கீழ் குறிப்பிட்ட பயணத்தைத் தேர்ந்தெடுப்பதன் மூலம் பயன்பாட்டின் மூலம் நேரடியாக டிரைவரை அணுகலாம். பயணத்தின் 24 மணி நேரத்திற்குள் நீங்கள் டிரைவரை தொடர்பு கொள்ளலாம் என்பதை நினைவில் கொள்ளவும்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('owpg6yg5-8vsm-d16l-csz7-i0kyqlylyg57','You can reach the driver directly through the app by selecting the specific ride under "My rides." Please note that you can contact the driver within 24 hours of the trip.','आप "मेरी सवारी" के अंतर्गत विशिष्ट सवारी का चयन करके ऐप के माध्यम से सीधे ड्राइवर तक पहुंच सकते हैं। कृपया ध्यान दें कि आप यात्रा के 24 घंटों के भीतर ड्राइवर से संपर्क कर सकते हैं।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('v4x401ug-33mr-wsph-s76f-857n02s6p1fw','Was this helpful?','Was this helpful?','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('le30oiyp-cqoy-wg5f-z94x-0pjp3al410fq','Was this helpful?','ಇದು ಸಹಾಯಕವಾಗಿತ್ತೇ?','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('twsjt902-4ulw-nulm-z67p-x0ru3ybhnj8n','Was this helpful?','இது உதவிகரமாக இருந்ததா?','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('jr935qrf-s8oh-za0c-8uei-9kl6ttay0lqt','Was this helpful?','क्या ये सहायक था?','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('4x58ldzi-v7ob-yf26-qcz6-dqdvx19q4fvw','Yes','Yes','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('cih3qp1p-9lgp-v84w-d8z3-ipdms4qiybcy','Yes','ಹೌದು','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('8jstuccg-zjzs-pc05-1ypi-44wo35lwuyn4','Yes','ஆம்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('crifqaak-xt9t-zhmz-qqfk-no1waox1drc0','Yes','हाँ','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('r7m8nojv-4iv1-2iqp-t60n-gbcrqtebdyq4','No','No','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('k20nff96-dv9s-6bzp-qvch-xvwc08ys801m','No','ಸಂ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('yzipp2zq-chwp-zi0z-z71c-5x37wkfd2xg5','No','இல்லை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('iqlv1itw-68r7-kbg1-1dom-09dvwcu65lps','No','नहीं','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('eo6rcil4-8sxx-hb7w-6d5q-j7ld820s3di6','account related','account related','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('nbma3yqa-jhtf-lcny-tngs-38h5dp9u95mj','account related','ಖಾತೆಗೆ ಸಂಬಂಧಿಸಿದ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('9nayojri-uinj-2z84-qdn8-78s7vfhvc852','account related','கணக்கு தொடர்பான','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('ivslggdi-bv2f-q785-yuxb-jisxsbl5rl29','account related','खाता संबंधी','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('uf0puk23-2z94-1oqp-cf14-lj1xnm6dezei','Hey, We’re really sorry to hear you have been facing account related issues.','Hey, We’re really sorry to hear you have been facing account related issues.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('47wenn0b-9twe-41ao-zq3n-6aycj4yuj24b','Hey, We’re really sorry to hear you have been facing account related issues.','ಹೇ, ನೀವು ಖಾತೆಗೆ ಸಂಬಂಧಿಸಿದ ಸಮಸ್ಯೆಗಳನ್ನು ಎದುರಿಸುತ್ತಿರುವುದನ್ನು ಕೇಳಲು ನಾವು ನಿಜವಾಗಿಯೂ ವಿಷಾದಿಸುತ್ತೇವೆ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('6glhwq9u-0x4f-mxzw-fgas-lx6b86gbq5nm','Hey, We’re really sorry to hear you have been facing account related issues.','ஹாய், கணக்கு தொடர்பான சிக்கல்களை நீங்கள் எதிர்கொள்வதைக் கேட்டு வருந்துகிறோம்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('tazssuv0-g4ne-3z9o-9xmp-0tyfsn48lzu4','Hey, We’re really sorry to hear you have been facing account related issues.','हे, हमें यह जानकर सचमुच दुख हुआ कि आपको खाता संबंधी समस्याओं का सामना करना पड़ रहा है।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('78jvpp3x-7wx8-uyo3-exor-u91txt4d5i8u','Phone number change','Phone number change','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('tqowg5wm-1gvo-9dhl-n0z5-z32zdetcws0u','Phone number change','ಫೋನ್ ಸಂಖ್ಯೆ ಬದಲಾವಣೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('n86jt1t5-eid2-jzsh-06yw-dtsyf7i7y3a0','Phone number change','தொலைபேசி எண் மாற்றம்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('m69wb8lg-5w1j-52dd-wkfr-83jzlamj3rqs','Phone number change','फ़ोन नंबर बदला','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('oqzcvxw1-q9fa-xvck-dnyd-du595rv29ysy','How to update my Work/Home or favourite locations?','How to update my Work/Home or favourite locations?','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('bad6e5b8-8dsz-436a-3q6b-hzxbwf071jj5','How to update my Work/Home or favourite locations?','ನನ್ನ ಕೆಲಸ/ಮನೆ ಅಥವಾ ಮೆಚ್ಚಿನ ಸ್ಥಳಗಳನ್ನು ನವೀಕರಿಸುವುದು ಹೇಗೆ?','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('rd8spyq9-g1k5-wclr-12qt-4uoak0e7rhod','How to update my Work/Home or favourite locations?','எனது பணி/வீடு அல்லது பிடித்த இடங்களை எவ்வாறு புதுப்பிப்பது?','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('bqmdjn5h-puld-frwz-opfj-h1fjygen96fn','How to update my Work/Home or favourite locations?','मैं अपने कार्यस्थल/घर या पसंदीदा स्थानों को कैसे अपडेट करूं?','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('0mrelipr-ayzl-qslk-kwo2-x3hwvm26ku44','How can I update the language on my app?','How can I update the language on my app?','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('kt0pd592-n3r1-3nao-kh0x-g4degw8ugq5n','How can I update the language on my app?','ನನ್ನ ಅಪ್ಲಿಕೇಶನ್‌ನಲ್ಲಿ ನಾನು ಭಾಷೆಯನ್ನು ಹೇಗೆ ನವೀಕರಿಸಬಹುದು?','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('3pwf0rjr-py0k-hqfm-rzlp-e5st7pv4trpq','How can I update the language on my app?','எனது பயன்பாட்டில் மொழியை எவ்வாறு மேம்படுத்துவது?','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('omu4zs93-bhv5-2rs0-b8r3-o2ysv4f02wh9','How can I update the language on my app?','मैं अपने ऐप पर भाषा कैसे अपडेट कर सकता हूं?','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('u4rwd3o1-66te-rfgh-jpwc-lha1ssy2bw13','Sorry! Currently we do not have the option, we suggest you to logout from this number and login with a new number','Sorry! Currently we do not have the option, we suggest you to logout from this number and login with a new number','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('st5ayr3r-f5a2-45u7-hhm1-rqo4aey4mrfn','Sorry! Currently we do not have the option, we suggest you to logout from this number and login with a new number','ಕ್ಷಮಿಸಿ! ಪ್ರಸ್ತುತ ನಾವು ಆಯ್ಕೆಯನ್ನು ಹೊಂದಿಲ್ಲ, ಈ ಸಂಖ್ಯೆಯಿಂದ ಲಾಗ್ ಔಟ್ ಮಾಡಲು ಮತ್ತು ಹೊಸ ಸಂಖ್ಯೆಯೊಂದಿಗೆ ಲಾಗಿನ್ ಮಾಡಲು ನಾವು ನಿಮಗೆ ಸಲಹೆ ನೀಡುತ್ತೇವೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('i3ll39v3-tguw-mpst-3th9-01sc4n7qcx0f','Sorry! Currently we do not have the option, we suggest you to logout from this number and login with a new number','மன்னிக்கவும்! தற்போது எங்களிடம் விருப்பம் இல்லை, இந்த எண்ணிலிருந்து வெளியேறி புதிய எண்ணைப் பயன்படுத்தி உள்நுழையுமாறு பரிந்துரைக்கிறோம்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('qhf4h0lz-fnsr-yjuz-ocuy-znnxt1c40jb7','Sorry! Currently we do not have the option, we suggest you to logout from this number and login with a new number','क्षमा मांगना! वर्तमान में हमारे पास विकल्प नहीं है, हम आपको इस नंबर से लॉगआउट करने और नए नंबर से लॉगिन करने का सुझाव देते हैं','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('4bsadv1r-g68b-jf09-b69y-3ayz6bk7biz1','You can update your work/home or favourite locations by navigating to three-line menu.  \n Click on favourites.','You can update your work/home or favourite locations by navigating to three-line menu.  \n Click on favourites.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('clw42duv-mnuv-8vj6-yxcb-f3vixflz57h1','You can update your work/home or favourite locations by navigating to three-line menu.  \n Click on favourites.','ಮೂರು-ಸಾಲಿನ ಮೆನುಗೆ ನ್ಯಾವಿಗೇಟ್ ಮಾಡುವ ಮೂಲಕ ನಿಮ್ಮ ಕೆಲಸ/ಮನೆ ಅಥವಾ ನೆಚ್ಚಿನ ಸ್ಥಳಗಳನ್ನು ನೀವು ನವೀಕರಿಸಬಹುದು.  \n ಮೆಚ್ಚಿನವುಗಳ ಮೇಲೆ ಕ್ಲಿಕ್ ಮಾಡಿ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('fdeilso2-vebf-ydsu-2idd-rqufa73y8h73','You can update your work/home or favourite locations by navigating to three-line menu.  \n Click on favourites.','மூன்று வரி மெனுவிற்குச் செல்வதன் மூலம் உங்கள் பணி/வீடு அல்லது விருப்பமான இடங்களைப் புதுப்பிக்கலாம்.  \n பிடித்தவற்றைக் கிளிக் செய்யவும்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('8tao5mc6-3gfc-uh5x-o0x2-y8v93ydve9bl','You can update your work/home or favourite locations by navigating to three-line menu.  \n Click on favourites.','आप तीन-पंक्ति मेनू पर जाकर अपने कार्यस्थल/घर या पसंदीदा स्थानों को अपडेट कर सकते हैं।  \n पसंदीदा पर क्लिक करें।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('33euiy5c-cdqp-d80c-4yf9-8ddsq5zr1nkz','You can change the language by navigating to three-line menu and clicking on language option on the app','You can change the language by navigating to three-line menu and clicking on language option on the app','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('uotpa8gq-dox1-hx3u-z66g-gsou9buxiuas','You can change the language by navigating to three-line menu and clicking on language option on the app','ಮೂರು-ಸಾಲಿನ ಮೆನುಗೆ ನ್ಯಾವಿಗೇಟ್ ಮಾಡುವ ಮೂಲಕ ಮತ್ತು ಅಪ್ಲಿಕೇಶನ್‌ನಲ್ಲಿ ಭಾಷಾ ಆಯ್ಕೆಯನ್ನು ಕ್ಲಿಕ್ ಮಾಡುವ ಮೂಲಕ ನೀವು ಭಾಷೆಯನ್ನು ಬದಲಾಯಿಸಬಹುದು','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('jpq43klo-vxg7-fbsh-uj6h-fp3ov5ftfizs','You can change the language by navigating to three-line menu and clicking on language option on the app','மூன்று வரி மெனுவிற்குச் சென்று, பயன்பாட்டில் உள்ள மொழி விருப்பத்தைக் கிளிக் செய்வதன் மூலம் மொழியை மாற்றலாம்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('nfv0m1e4-un3n-3czi-n4n4-mybrujjxr8e9','You can change the language by navigating to three-line menu and clicking on language option on the app','आप तीन-पंक्ति मेनू पर जाकर और ऐप पर भाषा विकल्प पर क्लिक करके भाषा बदल सकते हैं','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('v385tdi7-hvs3-iyck-5uty-ftguya7hygvw','Driver location is not being updated','Driver location is not being updated','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('v35sexjy-9vd4-fu09-4ca7-oguv4xv3lt4s','Driver location is not being updated','ಚಾಲಕ ಸ್ಥಳವನ್ನು ನವೀಕರಿಸಲಾಗುತ್ತಿಲ್ಲ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('43yj1d3w-6qo8-s3po-1jtm-hvw93h5aky6z','Driver location is not being updated','டிரைவர் இருப்பிடம் புதுப்பிக்கப்படவில்லை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('vrfwdhkp-dhw2-x80n-d0u2-eenj4otjena1','Driver location is not being updated','ड्राइवर का स्थान अपडेट नहीं किया जा रहा है','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('vtjroi70-xal7-gz0j-rrun-o5w3qstdt9fj','I am not receiving OTP to login','I am not receiving OTP to login','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('gyk8ez9s-4vla-7ogk-fsop-srwyaux5yvce','I am not receiving OTP to login','ನಾನು ಲಾಗಿನ್ ಮಾಡಲು OTP ಸ್ವೀಕರಿಸುತ್ತಿಲ್ಲ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('bwzgduea-znn6-znx2-z1os-exh7llkihz31','I am not receiving OTP to login','உள்நுழைவதற்கான OTP ஐ நான் பெறவில்லை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('mezta1ty-2s3h-g8ba-wbh4-2fxll55myt97','I am not receiving OTP to login','मुझे लॉगिन करने के लिए ओटीपी प्राप्त नहीं हो रहा है','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('tbsktd83-3w17-67me-zcic-x3582mymtlz3','App is stuck on previous ride','App is stuck on previous ride','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('f3j889rr-8jtm-d995-k65z-dsclhbq3fi6q','App is stuck on previous ride','ಹಿಂದಿನ ರೈಡ್‌ನಲ್ಲಿ ಅಪ್ಲಿಕೇಶನ್ ಅಂಟಿಕೊಂಡಿದೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('6ml2g4cj-os2i-aan0-fapn-d1dwsthdm8w8','App is stuck on previous ride','முந்தைய பயணத்தில் ஆப் சிக்கியுள்ளது','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('ckpojjml-1vyl-m23l-xqu7-auglkdx86gmd','App is stuck on previous ride','ऐप पिछली राइड पर अटका हुआ है','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('j0ko7n9k-03vt-tidx-rguz-8c3sppbmo2jc','Unable to book a ride','Unable to book a ride','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('k7yo7g56-zz3f-nvjy-ooz8-mjhixmu0lf1b','Unable to book a ride','ರೈಡ್ ಬುಕ್ ಮಾಡಲು ಸಾಧ್ಯವಾಗುತ್ತಿಲ್ಲ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('5s9dmtdc-b1ue-muhe-02r6-pk5njb5ji65r','Unable to book a ride','சவாரிக்கு முன்பதிவு செய்ய முடியவில்லை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('jbrs83fy-7xli-jw6e-4gr0-l8tnaf7r6uvj','Unable to book a ride','सवारी बुक करने में असमर्थ','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('12rcz8xo-gdkq-5ady-dfrz-1sdyairppkzr','My app is not responding','My app is not responding','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('a988d3vn-vbmp-hqa6-t2zt-k3jgdoddkydw','My app is not responding','ನನ್ನ ಅಪ್ಲಿಕೇಶನ್ ಪ್ರತಿಕ್ರಿಯಿಸುತ್ತಿಲ್ಲ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('yk6gjacb-mpya-x4w6-2gdv-qzzt40eebury','My app is not responding','எனது பயன்பாடு பதிலளிக்கவில்லை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('sq3ujs98-gjrt-8p1r-kgf9-1lw67vdtlrf7','My app is not responding','मेरा ऐप जवाब नहीं दे रहा है','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('9tq7nql3-pej1-lnf1-1b7y-7k29grgvr5tc','Kindly try restarting the app to access the updated location. \n If the problem persists, contact the driver directly for the estimated time of arrival (ETA).','Kindly try restarting the app to access the updated location. \n If the problem persists, contact the driver directly for the estimated time of arrival (ETA).','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('x4i1y6ai-wbo5-qtxx-f1t8-emx3r4btbbst','Kindly try restarting the app to access the updated location. \n If the problem persists, contact the driver directly for the estimated time of arrival (ETA).','ನವೀಕರಿಸಿದ ಸ್ಥಳವನ್ನು ಪ್ರವೇಶಿಸಲು ಅಪ್ಲಿಕೇಶನ್ ಅನ್ನು ಮರುಪ್ರಾರಂಭಿಸಲು ದಯವಿಟ್ಟು ಪ್ರಯತ್ನಿಸಿ. \n ಸಮಸ್ಯೆಯು ಮುಂದುವರಿದರೆ, ಆಗಮನದ ಅಂದಾಜು ಸಮಯಕ್ಕೆ (ETA) ನೇರವಾಗಿ ಚಾಲಕನನ್ನು ಸಂಪರ್ಕಿಸಿ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('5ngqd6al-d17q-oras-x0bw-3l3g85674ea0','Kindly try restarting the app to access the updated location. \n If the problem persists, contact the driver directly for the estimated time of arrival (ETA).','புதுப்பிக்கப்பட்ட இருப்பிடத்தை அணுக, பயன்பாட்டை மறுதொடக்கம் செய்ய முயற்சிக்கவும். \n சிக்கல் தொடர்ந்தால், வரவிருக்கும் மதிப்பிடப்பட்ட நேரத்திற்கு (ETA) நேரடியாக இயக்கியைத் தொடர்புகொள்ளவும்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('801dv20d-mpnq-iszy-wrjw-0uej8zygecv1','Kindly try restarting the app to access the updated location. \n If the problem persists, contact the driver directly for the estimated time of arrival (ETA).','नकृपया अद्यतन स्थान तक पहुंचने के लिए ऐप को पुनः आरंभ करने का प्रयास करें।  \n यदि समस्या बनी रहती है, तो आगमन के अनुमानित समय (ईटीए) के लिए सीधे ड्राइवर से संपर्क करें।हीं','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('rfpsmuan-dm9h-h5wa-nnkt-fn1700dtmy2z','Your account may be blocked, Create a ticket  \n Reasons leading to account block  \n * If you attempt several booking cancellations over a period of time, your Namma Yatri account will be temporarily blocked.  \n * Account can also be blocked permanently if the system detects any unusual behavior/activities against Namma Yatri’s terms and conditions.','Your account may be blocked, Create a ticket  \n Reasons leading to account block  \n * If you attempt several booking cancellations over a period of time, your Namma Yatri account will be temporarily blocked.  \n * Account can also be blocked permanently if the system detects any unusual behavior/activities against Namma Yatri’s terms and conditions.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('oa1tf62l-4cdb-4e9e-8brv-4fmxe19de0vb','Your account may be blocked, Create a ticket  \n Reasons leading to account block  \n * If you attempt several booking cancellations over a period of time, your Namma Yatri account will be temporarily blocked.  \n * Account can also be blocked permanently if the system detects any unusual behavior/activities against Namma Yatri’s terms and conditions.','ನಿಮ್ಮ ಖಾತೆಯನ್ನು ನಿರ್ಬಂಧಿಸಬಹುದು, ಟಿಕೆಟ್ ರಚಿಸಿ  \n ಖಾತೆ ನಿರ್ಬಂಧಿಸಲು ಕಾರಣಗಳು  \n * ನೀವು ಒಂದು ಅವಧಿಯಲ್ಲಿ ಹಲವಾರು ಬುಕಿಂಗ್ ರದ್ದುಗಳನ್ನು ಪ್ರಯತ್ನಿಸಿದರೆ, ನಿಮ್ಮ Namma Yatri ಖಾತೆಯನ್ನು ತಾತ್ಕಾಲಿಕವಾಗಿ ನಿರ್ಬಂಧಿಸಲಾಗುತ್ತದೆ.  \n * ಸಿಸ್ಟಮ್ ನಮ್ಮ ಯಾತ್ರಿಯ ನಿಯಮಗಳು ಮತ್ತು ಷರತ್ತುಗಳಿಗೆ ವಿರುದ್ಧವಾಗಿ ಯಾವುದೇ ಅಸಾಮಾನ್ಯ ನಡವಳಿಕೆ/ಚಟುವಟಿಕೆಗಳನ್ನು ಪತ್ತೆಮಾಡಿದರೆ ಖಾತೆಯನ್ನು ಶಾಶ್ವತವಾಗಿ ನಿರ್ಬಂಧಿಸಬಹುದು.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('3456abcd-ef78-ghij-klmn-opqrstu0vwxy','Your account may be blocked, Create a ticket  \n Reasons leading to account block  \n * If you attempt several booking cancellations over a period of time, your Namma Yatri account will be temporarily blocked.  \n * Account can also be blocked permanently if the system detects any unusual behavior/activities against Namma Yatri’s terms and conditions.','உங்கள் கணக்கு தடைசெய்யப்படலாம், டிக்கெட்டை உருவாக்கவும்  \n கணக்கு பிளாக் செய்யப்படுவதற்கான காரணங்கள்  \n * நீங்கள் ஒரு குறிப்பிட்ட காலத்தில் பல முன்பதிவுகளை ரத்து செய்ய முயற்சித்தால், உங்கள் Namma Yatri கணக்கு தற்காலிகமாக தடுக்கப்படும்.  \n * நம் யாத்ரியின் விதிமுறைகள் மற்றும் நிபந்தனைகளுக்கு எதிராக ஏதேனும் அசாதாரண நடத்தை/செயல்பாடுகளைக் கணினி கண்டறிந்தால், கணக்கையும் நிரந்தரமாகத் தடுக்கலாம்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('wtwee7ph-bw7r-t76l-txh9-pe47za7qddih','Your account may be blocked, Create a ticket  \n Reasons leading to account block  \n * If you attempt several booking cancellations over a period of time, your Namma Yatri account will be temporarily blocked.  \n * Account can also be blocked permanently if the system detects any unusual behavior/activities against Namma Yatri’s terms and conditions.','आपका खाता ब्लॉक किया जा सकता है, एक टिकट बनाएं  \n खाता ब्लॉक होने के कारण  \n * यदि आप एक समयावधि में कई बुकिंग रद्द करने का प्रयास करते हैं, तो आपका नम्मा यात्री खाता अस्थायी रूप से ब्लॉक कर दिया जाएगा।  \n * यदि सिस्टम नम्मा यात्री के नियमों और शर्तों के खिलाफ किसी असामान्य व्यवहार/गतिविधि का पता लगाता है तो खाते को स्थायी रूप से ब्लॉक भी किया जा सकता है।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('rc62xe9b-cnv1-nc40-ocay-n2zqzd5anfdj','Sorry to hear that. Please share you contact details so that we can assist you better','Sorry to hear that. Please share you contact details so that we can assist you better','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('twtea8dw-2dc2-50vg-zbx0-qjfikkm4cvjm','Sorry to hear that. Please share you contact details so that we can assist you better','ಅದನ್ನು ಕೇಳಲು ಕ್ಷಮಿಸಿ. ದಯವಿಟ್ಟು ನಿಮ್ಮ ಸಂಪರ್ಕ ವಿವರಗಳನ್ನು ಹಂಚಿಕೊಳ್ಳಿ ಇದರಿಂದ ನಾವು ನಿಮಗೆ ಉತ್ತಮವಾಗಿ ಸಹಾಯ ಮಾಡಬಹುದು','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('9yvqn0zg-htch-xms3-qigm-ctkjl5q5nlyw','Sorry to hear that. Please share you contact details so that we can assist you better','வருந்துகிறேன். உங்கள் தொடர்பு விவரங்களைப் பகிரவும், இதனால் நாங்கள் உங்களுக்கு சிறப்பாக உதவ முடியும்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('a80oonku-k0jo-tz08-egx9-9v4mhp4nbko1','Sorry to hear that. Please share you contact details so that we can assist you better','ऐसा सुनने के लिए क्षमा करें। कृपया अपना संपर्क विवरण साझा करें ताकि हम आपकी बेहतर सहायता कर सकें','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('we2o9cr1-23kz-foti-0gw2-bekq0twxya8y','Try booking a ride after 5mins.','Try booking a ride after 5mins.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('1ip098nc-76hf-3kp6-p2qm-yqe7wewz352a','Try booking a ride after 5mins.','5 ನಿಮಿಷಗಳ ನಂತರ ರೈಡ್ ಬುಕ್ ಮಾಡಲು ಪ್ರಯತ್ನಿಸಿ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('1z2dwrs7-88sh-3ij9-6di1-ldi9eoptlded','Try booking a ride after 5mins.','5 நிமிடங்களுக்குப் பிறகு சவாரிக்கு முன்பதிவு செய்ய முயற்சிக்கவும்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('cdbyouf9-zj6l-4blo-n1di-6ek2vk2af9v4','Try booking a ride after 5mins.','5 मिनट के बाद सवारी बुक करने का प्रयास करें।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('3b0cdk2q-lglc-uiy6-vhmg-an3hj1zliiu0','We''re sorry that you are facing this issue. Kindly follow the following steps: \n - Please make sure that you are connected to the internet with a strong signal \n - Update the app \n - Check the GPS settings and ensure that it is turned on for Namma Yatri','We''re sorry that you are facing this issue. Kindly follow the following steps: \n - Please make sure that you are connected to the internet with a strong signal \n - Update the app \n - Check the GPS settings and ensure that it is turned on for Namma Yatri','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('pvy9zy18-9yd6-g0jf-gzxf-x6zz6alt1ftz','We''re sorry that you are facing this issue. Kindly follow the following steps: \n - Please make sure that you are connected to the internet with a strong signal \n - Update the app \n - Check the GPS settings and ensure that it is turned on for Namma Yatri','ನೀವು ಈ ಸಮಸ್ಯೆಯನ್ನು ಎದುರಿಸುತ್ತಿರುವುದಕ್ಕೆ ನಮಗೆ ವಿಷಾದವಿದೆ. ದಯವಿಟ್ಟು ಈ ಕೆಳಗಿನ ಹಂತಗಳನ್ನು ಅನುಸರಿಸಿ: \n - ನೀವು ಬಲವಾದ ಸಿಗ್ನಲ್‌ನೊಂದಿಗೆ ಇಂಟರ್ನೆಟ್‌ಗೆ ಸಂಪರ್ಕ ಹೊಂದಿರುವಿರಾ ಎಂದು ಖಚಿತಪಡಿಸಿಕೊಳ್ಳಿ \n - ಅಪ್ಲಿಕೇಶನ್ ಅನ್ನು ನವೀಕರಿಸಿ \n - GPS ಸೆಟ್ಟಿಂಗ್‌ಗಳನ್ನು ಪರಿಶೀಲಿಸಿ ಮತ್ತು ಅದನ್ನು ನಮ್ಮ ಯಾತ್ರಿಗಾಗಿ ಆನ್ ಮಾಡಲಾಗಿದೆಯೇ ಎಂದು ಖಚಿತಪಡಿಸಿಕೊಳ್ಳಿ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('2kg285h7-h0qx-bw7t-n0bm-s24p7gr094u8','We''re sorry that you are facing this issue. Kindly follow the following steps: \n - Please make sure that you are connected to the internet with a strong signal \n - Update the app \n - Check the GPS settings and ensure that it is turned on for Namma Yatri','நீங்கள் இந்த சிக்கலை எதிர்கொள்வதற்காக நாங்கள் வருந்துகிறோம். தயவுசெய்து பின்வரும் படிகளைப் பின்பற்றவும்: \n - வலுவான சிக்னலுடன் இணையத்துடன் இணைக்கப்பட்டுள்ளதை உறுதிசெய்யவும் \n - பயன்பாட்டைப் புதுப்பிக்கவும் \n - GPS அமைப்புகளைச் சரிபார்த்து, அது நம்ம யாத்ரிக்கு இயக்கப்பட்டுள்ளதா என்பதை உறுதிப்படுத்தவும்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('ng078yuo-uez9-rhng-c8zg-63zsicsd2yqc','We''re sorry that you are facing this issue. Kindly follow the following steps: \n - Please make sure that you are connected to the internet with a strong signal \n - Update the app \n - Check the GPS settings and ensure that it is turned on for Namma Yatri','हमें खेद है कि आपको इस समस्या का सामना करना पड़ रहा है। कृपया निम्नलिखित चरणों का पालन करें:  \n  - कृपया सुनिश्चित करें कि आप मजबूत सिग्नल के साथ इंटरनेट से जुड़े हैं  \n  - ऐप अपडेट करें  \n  - जीपीएस सेटिंग्स की जांच करें और सुनिश्चित करें कि यह नम्मा यात्री के लिए चालू है','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('xrm1ykvb-5w4v-riln-r57n-aw368h2yz1q0','fare related','fare related','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('oiejjy30-hnsw-69zx-8joj-cbhupeoxlanl','fare related','ಶುಲ್ಕ ಸಂಬಂಧಿಸಿದಂತೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('lwtty14i-ljou-pkar-q3hr-vjfo9qscpptx','fare related','கட்டணம் தொடர்பானது','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('dfobeamc-xu33-iwq4-co35-wtb0zwcx3qzf','fare related','किराया संबंधित','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('b5m4qr1l-o9r5-oe5k-o11e-2i2vhn4ogki4','Hey, We’re really sorry to hear you have been facing fare related issues.','Hey, We’re really sorry to hear you have been facing fare related issues.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('dwjok2sn-xtr6-lz1n-xpgx-4ri2dftlyqkc','Hey, We’re really sorry to hear you have been facing fare related issues.','ಹೇ, ನೀವು ಶುಲ್ಕ ಸಂಬಂಧಿತ ಸಮಸ್ಯೆಗಳನ್ನು ಎದುರಿಸುತ್ತಿರುವುದನ್ನು ಕೇಳಲು ನಾವು ನಿಜವಾಗಿಯೂ ವಿಷಾದಿಸುತ್ತೇವೆ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('hehv03wn-x9ow-z2tn-9glt-ponfruot5991','Hey, We’re really sorry to hear you have been facing fare related issues.','ஹாய், கட்டணம் தொடர்பான சிக்கல்களை நீங்கள் எதிர்கொண்டுள்ளதைக் கேட்டு வருந்துகிறோம்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('6ltjgom6-nxb8-fhgl-8fb2-fkb5lsk2vo7d','Hey, We’re really sorry to hear you have been facing fare related issues.','हे, हमें यह जानकर सचमुच दुख हुआ कि आपको किराया संबंधी समस्याओं का सामना करना पड़ रहा है।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('e7uqttg0-04u3-gx6l-j8yd-qr3mvmca4042','Driver has not arrived for pickup','Driver has not arrived for pickup','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('z8wrbnut-0p2a-iww1-hajl-ig4ky8ua2bcf','Driver has not arrived for pickup','ಚಾಲಕ ಪಿಕಪ್‌ಗೆ ಬಂದಿಲ್ಲ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('pz9xmfax-fm9s-1rid-nnsk-qnhnjg90lh6s','Driver has not arrived for pickup','டிரைவர் பிக்அப் செய்ய வரவில்லை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('ouvvtqol-c36h-6zib-za1r-wlnwsojs34ry','Driver has not arrived for pickup','ड्राइवर पिकअप के लिए नहीं आया है','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('x9e11p8m-tvyz-92wk-waax-csrtzw5orej2','Excess amount paid to the driver','Excess amount paid to the driver','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('vatbylfo-npkz-q9i2-282o-q4simsvchi9x','Excess amount paid to the driver','ಚಾಲಕನಿಗೆ ಹೆಚ್ಚುವರಿ ಮೊತ್ತವನ್ನು ಪಾವತಿಸಲಾಗಿದೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('siof7ij1-s1te-43sl-w2zx-w03ykv6nl92s','Excess amount paid to the driver','ஓட்டுநருக்கு அதிக தொகை செலுத்தப்பட்டது','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('787uybz6-zlj4-y3aw-sajh-tegnyxt17pgb','Excess amount paid to the driver','चालक को अधिक राशि का भुगतान किया गया','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('rvri1an0-istw-c8jy-8lq2-escnh64btw6m','I was charged more than the fare shown','I was charged more than the fare shown','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('yvezeycg-1yoc-cfyf-77ld-4whfdg2wr41a','I was charged more than the fare shown','ನನಗೆ ತೋರಿಸಿದ ದರಕ್ಕಿಂತ ಹೆಚ್ಚಿನ ಶುಲ್ಕ ವಿಧಿಸಲಾಗಿದೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('f2nu6dc6-u9xi-a6gg-yo1l-xfqcm24yh8z9','I was charged more than the fare shown','என்னிடம் காட்டப்பட்ட கட்டணத்தை விட அதிகமாக வசூலிக்கப்பட்டது','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('cua78x2w-vpoa-gg56-40sv-9s6iperl6fn0','I was charged more than the fare shown','मुझसे दिखाए गए किराये से अधिक किराया वसूला गया','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('wbj1k94v-2g9l-mt0s-0xvr-yjpez5b8rxst','Kindly call the driver. If the driver does not respond, you may cancel the ride and book a new one','Kindly call the driver. If the driver does not respond, you may cancel the ride and book a new one','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('if29r5te-28e0-0t4f-2quq-alj73r0rvg9c','Kindly call the driver. If the driver does not respond, you may cancel the ride and book a new one','ದಯವಿಟ್ಟು ಚಾಲಕನನ್ನು ಕರೆ ಮಾಡಿ. ಚಾಲಕ ಪ್ರತಿಕ್ರಿಯಿಸದಿದ್ದರೆ, ನೀವು ರೈಡ್ ಅನ್ನು ರದ್ದುಗೊಳಿಸಬಹುದು ಮತ್ತು ಹೊಸದನ್ನು ಬುಕ್ ಮಾಡಬಹುದು','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('p21o0nau-rp2c-ip0a-lewb-2aidr01bh99e','Kindly call the driver. If the driver does not respond, you may cancel the ride and book a new one','தயவுசெய்து டிரைவரை அழைக்கவும். ஓட்டுநர் பதிலளிக்கவில்லை என்றால், நீங்கள் சவாரியை ரத்து செய்துவிட்டு புதிய ஒன்றை முன்பதிவு செய்யலாம்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('05e5ibag-i4oy-622a-ncxi-2n7a6f1j0zw2','Kindly call the driver. If the driver does not respond, you may cancel the ride and book a new one','कृपया ड्राइवर को बुलाएँ। यदि ड्राइवर जवाब नहीं देता है, तो आप यात्रा रद्द कर सकते हैं और नई यात्रा बुक कर सकते हैं','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('9w9ere66-r3bf-xcc8-m3e1-j9k9d9gkvmxl','We will certainly help you in this matter. This is the attached breakup of the fare. \n Please Note: To receive additional assistance, keep the app open. Check the invoice copy by minimizing the screen to avoid recreating a ticket.','We will certainly help you in this matter. This is the attached breakup of the fare. \n Please Note: To receive additional assistance, keep the app open. Check the invoice copy by minimizing the screen to avoid recreating a ticket.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('2j85dz48-ogmc-yvax-w7gi-mqitglhgealh','We will certainly help you in this matter. This is the attached breakup of the fare. \n Please Note: To receive additional assistance, keep the app open. Check the invoice copy by minimizing the screen to avoid recreating a ticket.','ಈ ವಿಷಯದಲ್ಲಿ ನಾವು ಖಂಡಿತವಾಗಿಯೂ ನಿಮಗೆ ಸಹಾಯ ಮಾಡುತ್ತೇವೆ. ಇದು ದರದ ಲಗತ್ತಿಸಲಾದ ವಿಘಟನೆಯಾಗಿದೆ. \n ದಯವಿಟ್ಟು ಗಮನಿಸಿ: ಹೆಚ್ಚುವರಿ ಸಹಾಯವನ್ನು ಪಡೆಯಲು, ಅಪ್ಲಿಕೇಶನ್ ಅನ್ನು ತೆರೆಯಿರಿ. ಟಿಕೆಟ್ ಅನ್ನು ಮರುಸೃಷ್ಟಿಸುವುದನ್ನು ತಪ್ಪಿಸಲು ಪರದೆಯನ್ನು ಕಡಿಮೆ ಮಾಡುವ ಮೂಲಕ ಸರಕುಪಟ್ಟಿ ಪ್ರತಿಯನ್ನು ಪರಿಶೀಲಿಸಿ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('u6e55avo-d7fh-h6b6-7wqr-t32uwpb8mdrn','We will certainly help you in this matter. This is the attached breakup of the fare. \n Please Note: To receive additional assistance, keep the app open. Check the invoice copy by minimizing the screen to avoid recreating a ticket.','இந்த விஷயத்தில் நாங்கள் உங்களுக்கு நிச்சயமாக உதவுவோம். இது கட்டணத்தின் இணைக்கப்பட்ட பிரிவாகும். \n கவனிக்கவும்: கூடுதல் உதவியைப் பெற, பயன்பாட்டைத் திறந்து வைக்கவும். டிக்கெட்டை மீண்டும் உருவாக்குவதைத் தவிர்க்க திரையைக் குறைத்து விலைப்பட்டியல் நகலைச் சரிபார்க்கவும்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('ft8iekt5-qbsz-igrg-cuxt-j8h7fjoksopn','We will certainly help you in this matter. This is the attached breakup of the fare. \n Please Note: To receive additional assistance, keep the app open. Check the invoice copy by minimizing the screen to avoid recreating a ticket.','इस मामले में हम आपकी मदद जरूर करेंगे. यह किराए का संलग्न विवरण है। {आईएनएस}कृपया ध्यान दें: अतिरिक्त सहायता प्राप्त करने के लिए, ऐप खुला रखें। टिकट दोबारा बनाने से बचने के लिए स्क्रीन को छोटा करके चालान कॉपी की जांच करें।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('uk3vv7f7-cfic-xx57-fq2v-sgky177m3dwl','Change of destination','Change of destination','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('vyu8svvo-t7n9-napd-j01i-gn5xxme2h03v','Change of destination','ಗಮ್ಯಸ್ಥಾನದ ಬದಲಾವಣೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('0nxovets-loyn-9bdw-p2l5-sxuji0dq8ret','Change of destination','இலக்கு மாற்றம்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('73xoaj06-d0pt-veh6-93wy-5kwa20r9t1ke','Change of destination','गंतव्य का परिवर्तन','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('zgzz9qti-7qf0-feeu-g1z6-bbhjj7azo7vu','Unable to cancel the ride','Unable to cancel the ride','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('syaj06t5-1itm-ruh1-5v3t-33j4fstu937n','Unable to cancel the ride','ಸವಾರಿಯನ್ನು ರದ್ದುಗೊಳಿಸಲು ಸಾಧ್ಯವಾಗಲಿಲ್ಲ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('bj9j3m4p-r443-v1zs-prw1-g70u6iijpb2l','Unable to cancel the ride','பயணத்தை ரத்து செய்ய முடியவில்லை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('6mr11ptv-ik6c-7iae-fvlo-x30na8w4j7go','Unable to cancel the ride','सवारी रद्द करने में असमर्थ','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('xkboxhmf-e3uw-5yug-o85i-gyj8syz3hyem','Unable to check ride status','Unable to check ride status','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('8juc3ww5-mvg0-mwkj-hd7b-50un45mmwfbr','Unable to check ride status','ಸವಾರಿಯ ಸ್ಥಿತಿಯನ್ನು ಪರಿಶೀಲಿಸಲು ಸಾಧ್ಯವಾಗುತ್ತಿಲ್ಲ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('eh0yc40z-gkke-9729-qzrr-06d75zna74ha','Unable to check ride status','சவாரி நிலையைச் சரிபார்க்க முடியவில்லை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('1pypxar9-yxsg-275j-rtf6-saakecn4io1p','Unable to check ride status','सवारी की स्थिति जांचने में असमर्थ','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('eoh6hxz1-rgd3-h0nr-ic5o-ynmfu1ogz80k','Unable to get rides','Unable to get rides','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('khx7jlcr-6f1t-k72o-vw4t-mv7i1oi3rfgv','Unable to get rides','ಸವಾರಿಗಳನ್ನು ಪಡೆಯಲು ಸಾಧ್ಯವಾಗುತ್ತಿಲ್ಲ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('p3q048bm-chln-rlob-xvfz-xoqf686xaipv','Unable to get rides','சவாரி செய்ய முடியவில்லை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('xsx6rhh7-qgwh-1jw2-u02i-tajo0o7y8xex','Unable to get rides','सवारी नहीं मिल पा रही','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('kplwtugn-lvyc-5trs-1ebo-dzybsy24t7ag','Currently, this option is unavailable, but we are working on implementing it soon','Currently, this option is unavailable, but we are working on implementing it soon','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('1faakkzn-ne84-hknz-ipiy-kv2y0eujfb0d','Currently, this option is unavailable, but we are working on implementing it soon','ಪ್ರಸ್ತುತ, ಈ ಆಯ್ಕೆಯು ಲಭ್ಯವಿಲ್ಲ, ಆದರೆ ನಾವು ಅದನ್ನು ಶೀಘ್ರದಲ್ಲೇ ಕಾರ್ಯಗತಗೊಳಿಸಲು ಕೆಲಸ ಮಾಡುತ್ತಿದ್ದೇವೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('ie52rf51-dcp8-t9xe-429f-nibw2hstznys','Currently, this option is unavailable, but we are working on implementing it soon','தற்போது, இந்த விருப்பம் இல்லை, ஆனால் விரைவில் அதை செயல்படுத்துவதில் நாங்கள் பணியாற்றி வருகிறோம்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('oa5uviu8-oyz0-q8bb-2eq7-s0pvmofh1cxq','Currently, this option is unavailable, but we are working on implementing it soon','फिलहाल यह विकल्प उपलब्ध नहीं है, लेकिन हम इसे जल्द ही लागू करने पर काम कर रहे हैं','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('wew9qkip-ql1e-zjl4-dulh-ub1ig68h32rq','Force close the app and restart the app \n On the bottom of the page you will be able to see cancel ride option \n Click on cancel ride and you will get an option to call the driver / cancel ride  \n Click on cancl ride and select the reason for cancellation and click cancel ride.','Force close the app and restart the app \n On the bottom of the page you will be able to see cancel ride option \n Click on cancel ride and you will get an option to call the driver / cancel ride  \n Click on cancl ride and select the reason for cancellation and click cancel ride.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('rj6dg0bq-407r-8wdg-d7oe-bzgfzszr6zhr','Force close the app and restart the app \n On the bottom of the page you will be able to see cancel ride option \n Click on cancel ride and you will get an option to call the driver / cancel ride  \n Click on cancl ride and select the reason for cancellation and click cancel ride.','ಆ್ಯಪ್ ನೆಮ್ಮದಿಯಾಗಿ ಮುಚ್ಚಿ, ಆ್ಯಪ್ ಮರಳಿ ಪ್ರಾರಂಭಿಸಿ \n ಪುಟದ ಕೆಳಗೆ ರೈಡ್ ರದ್ದು ಆಯ್ಕೆ ಮಾಡಬಹುದು \n ರೈಡ್ ರದ್ದು ಆಯ್ಕೆ ಮಾಡಿ, ಡ್ರೈವರನನ್ನು ಕರೆಯಲು ಅಥವೇ ರೈಡ್ ರದ್ದು ಮಾಡಲು ಆಯ್ಕೆ ದೊರಕುತ್ತದೆ \n ರೈಡ್ ರದ್ದು ಆಯ್ಕೆ ಮಾಡಿ, ರದ್ದು ಮಾಡುವ ಕಾರಣವನ್ನು ಆಯ್ಕೆ ಮಾಡಿ ಮತ್ತು ರೈಡ್ ರದ್ದು ಮಾಡಿ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('cp8m1dnt-h93y-6sz8-cosa-72edps5l6874','Force close the app and restart the app \n On the bottom of the page you will be able to see cancel ride option \n Click on cancel ride and you will get an option to call the driver / cancel ride  \n Click on cancl ride and select the reason for cancellation and click cancel ride.','ஆப்பை முடுக்கி புதிய ஆப்பை திரும்ப திரும்ப திரும்ப ஆரம்பிக்கவும் \n பக்கத்தின் கீழே ரைட் ரத்தத்தை ரத்தப்படுத்த முடியும் \n ரைட் ரத்தத்திற்கு கிளிக் செய்து நீங்கள் ஒரு வாக்குபோக்கி அல்லது ரைட் ரத்தம் செய்ய விரும்புகிறீர்கள் \n ரைட் ரத்தம் செய்து, ரத்தத்தின் காரணத்தை தேர்ந்தெடுக்கவும் ரத்தப்படுத்த கிளிக் செய்யுங்கள்।','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('qjz5wrff-mtel-fr33-1fm2-r95g1h43tpxd','Force close the app and restart the app \n On the bottom of the page you will be able to see cancel ride option \n Click on cancel ride and you will get an option to call the driver / cancel ride  \n Click on cancl ride and select the reason for cancellation and click cancel ride.','ऐप्लिकेशन को बंद करें और फिर से शुरू करें \n पृष्ठ के नीचे आप कैंसल राइड विकल्प देख सकेंगे \n कैंसल राइड पर क्लिक करें और आपको ड्राइवर को कॉल करने या राइड कैंसल करने का विकल्प मिलेगा \n कैंसल राइड पर क्लिक करें और रद्दीकरण का कारण चुनें और कैंसल राइड पर क्लिक करें।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('hce33q54-e3a5-gpvt-dtv3-4ytzedn7yfzt','Force close the app and check once again.  \n If the issue is still not fixed uninstall the app and reinstall the app / try to update the app if not updated.','Force close the app and check once again.  \n If the issue is still not fixed uninstall the app and reinstall the app / try to update the app if not updated.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('wfd47qjc-k7wa-pif6-uz9k-556w8jzyfkf9','Force close the app and check once again.  \n If the issue is still not fixed uninstall the app and reinstall the app / try to update the app if not updated.','ಅಪ್ಲಿಕೇಶನ್ ಅನ್ನು ಬಲವಂತವಾಗಿ ಮುಚ್ಚಿ ಮತ್ತು ಮತ್ತೊಮ್ಮೆ ಪರಿಶೀಲಿಸಿ.  \n ಸಮಸ್ಯೆಯನ್ನು ಇನ್ನೂ ಸರಿಪಡಿಸಲಾಗದಿದ್ದರೆ ಅಪ್ಲಿಕೇಶನ್ ಅನ್ನು ಅನ್‌ಇನ್‌ಸ್ಟಾಲ್ ಮಾಡಿ ಮತ್ತು ಅಪ್ಲಿಕೇಶನ್ ಅನ್ನು ಮರುಸ್ಥಾಪಿಸಿ / ಅಪ್‌ಡೇಟ್ ಮಾಡದಿದ್ದರೆ ಅಪ್ಲಿಕೇಶನ್ ಅನ್ನು ನವೀಕರಿಸಲು ಪ್ರಯತ್ನಿಸಿ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('6rdzjsyr-vz2y-yd7d-u54f-7kv1omy3bnsz','Force close the app and check once again.  \n If the issue is still not fixed uninstall the app and reinstall the app / try to update the app if not updated.','பயன்பாட்டை மூடிவிட்டு மீண்டும் ஒருமுறை சரிபார்க்கவும்.  \n சிக்கல் இன்னும் தீர்க்கப்படவில்லை என்றால், பயன்பாட்டை நிறுவல் நீக்கி, பயன்பாட்டை மீண்டும் நிறுவவும் / புதுப்பிக்கப்படவில்லை என்றால், பயன்பாட்டைப் புதுப்பிக்க முயற்சிக்கவும்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('tw41zac0-xqpe-cskr-ka9a-cb9ll614tire','Force close the app and check once again.  \n If the issue is still not fixed uninstall the app and reinstall the app / try to update the app if not updated.','ऐप को फोर्स बंद करें और एक बार फिर से जांचें।  \n यदि समस्या अभी भी ठीक नहीं हुई है तो ऐप को अनइंस्टॉल करें और ऐप को फिर से इंस्टॉल करें / यदि ऐप अपडेट नहीं हुआ है तो उसे अपडेट करने का प्रयास करें।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('thyq8hhm-k9jr-9zor-2azd-qbn04ms2ame8','Force close the app and restart the app \n You will be able to contact the driver via the call option available on the ride page.','Force close the app and restart the app \n You will be able to contact the driver via the call option available on the ride page.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('agcbehjr-idti-glnv-whxm-xngmwbc17ewa','Force close the app and restart the app \n You will be able to contact the driver via the call option available on the ride page.','ಅಪ್ಲಿಕೇಶನ್ ಅನ್ನು ಬಲವಂತವಾಗಿ ಮುಚ್ಚಿ ಮತ್ತು ಅಪ್ಲಿಕೇಶನ್ ಅನ್ನು ಮರುಪ್ರಾರಂಭಿಸಿ \n ರೈಡ್ ಪುಟದಲ್ಲಿ ಲಭ್ಯವಿರುವ ಕರೆ ಆಯ್ಕೆಯ ಮೂಲಕ ನೀವು ಚಾಲಕನನ್ನು ಸಂಪರ್ಕಿಸಲು ಸಾಧ್ಯವಾಗುತ್ತದೆ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('9okwj5e4-zqme-emxi-s0tb-wfvhigpm4dd3','Force close the app and restart the app \n You will be able to contact the driver via the call option available on the ride page.','பயன்பாட்டை மூடிவிட்டு, பயன்பாட்டை மறுதொடக்கம் செய்யுங்கள் \n சவாரி பக்கத்தில் உள்ள அழைப்பு விருப்பத்தின் மூலம் நீங்கள் டிரைவரைத் தொடர்புகொள்ள முடியும்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('79wdulbj-rpm0-c1ju-axs9-soyp82lvt6ip','Force close the app and restart the app \n You will be able to contact the driver via the call option available on the ride page.','ऐप को बलपूर्वक बंद करें और ऐप को पुनरारंभ करें।  \n आप राइड पेज पर उपलब्ध कॉल विकल्प के माध्यम से ड्राइवर से संपर्क कर पाएंगे।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('1pn7mctl-pg8q-9w4x-xkzc-1123wat8ozmw','Because of the high demand, it is possible that you may not get a ride. We recommend trying to book a ride again in 5 minutes','Because of the high demand, it is possible that you may not get a ride. We recommend trying to book a ride again in 5 minutes','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('gog59v8k-qhf4-h56w-6pb9-h628iammb9ob','Because of the high demand, it is possible that you may not get a ride. We recommend trying to book a ride again in 5 minutes','ಹೆಚ್ಚಿನ ಬೇಡಿಕೆಯ ಕಾರಣ, ನೀವು ಸವಾರಿ ಮಾಡದಿರುವ ಸಾಧ್ಯತೆಯಿದೆ. 5 ನಿಮಿಷಗಳಲ್ಲಿ ಮತ್ತೆ ರೈಡ್ ಬುಕ್ ಮಾಡಲು ಪ್ರಯತ್ನಿಸುವುದನ್ನು ನಾವು ಶಿಫಾರಸು ಮಾಡುತ್ತೇವೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('66ee0h80-t6es-5lq5-l4sn-7lley6oce3gp','Because of the high demand, it is possible that you may not get a ride. We recommend trying to book a ride again in 5 minutes','அதிக தேவை இருப்பதால், நீங்கள் சவாரி செய்யாமல் போகலாம். 5 நிமிடங்களில் மீண்டும் சவாரிக்கு முன்பதிவு செய்ய பரிந்துரைக்கிறோம்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('loy21kx2-1891-9uqj-z8z9-do3ztq5wzniz','Because of the high demand, it is possible that you may not get a ride. We recommend trying to book a ride again in 5 minutes','ज्यादा डिमांड की वजह से हो सकता है कि आपको सवारी न मिले. हमारा सुझाव है कि 5 मिनट में दोबारा यात्रा बुक करने का प्रयास करें','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('ck5rvy6e-qkdy-9m6j-40jk-6uto5egmx246','other','other','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('3gkfm97x-we3n-az82-gvcs-4en1sbvqgepq','other','ಇತರೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('efddrsif-ugcq-4t35-xg8d-rwuwclman0s4','other','மற்றவை','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('cp2a3n6x-gh5q-mepr-nv2y-rf3871z1lu7a','other','अन्य','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('eg5ccqnb-1qwl-u1h6-p4a0-g4ndai473lit','Existing follow up','Existing follow up','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('0wl2ks6i-pq4h-ugao-ixkp-xc2yd3xgbpaq','Existing follow up','ಅಸ್ತಿತ್ವದಲ್ಲಿರುವ ಅನುಸರಣೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('o5duvp7h-4o3x-jsvb-g1rv-j3lz4utf5h5r','Existing follow up','தற்போதுள்ள பின்தொடர்தல்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('g8hftrto-0k8r-5ghp-gmli-ws1j0utv3l2a','Existing follow up','मौजूदा अनुवर्ती','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('rhz0iug2-neea-50tu-zd49-lkk7i1wkq7ek','Feedback','Feedback','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('5yovy2pc-3dqv-gkob-38bj-g2hpejxb4aix','Feedback','ಪ್ರತಿಕ್ರಿಯೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('zz5szem6-ixwv-ksi3-6o88-lqbmmyw51g08','Feedback','பின்னூட்டம்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('78bzrmnw-2flz-huye-1q7q-kh04le05a4zh','Feedback','प्रतिक्रिया','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('7p0ku4oc-fyro-54dm-pe6f-lowkqos0nxxg','Promotion','Promotion','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('ydxske85-19f4-6lxy-vm0y-bl9zztnwi27s','Promotion','ಪ್ರಚಾರ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('eylpl9z4-ykly-efhg-1w6v-egwlc3r3urh4','Promotion','பதவி உயர்வு','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('xz6kt4sb-twul-xdsf-ie5q-k9mpb46yefpk','Promotion','पदोन्नति','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('iov9luym-x4dh-ywix-wkfq-fjbnfoa88fiw','Suggestions','Suggestions','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('64xlmixb-cyrn-68h9-xykr-v0d5oda8cq5t','Suggestions','ಸಲಹೆಗಳು','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('qaj9tvd3-iw7g-suwr-hrka-n54jtur552v3','Suggestions','பரிந்துரைகள்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('2js59q34-cqqx-ip6t-q29h-2tdn3v7ns27l','Suggestions','सुझाव','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('byg0g17x-s6ym-ndo4-c4xn-heyaj81na6s7','About namma yatri','About namma yatri','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('4wuewqup-c20w-x4a9-gzx2-ihac6ok43v4a','About namma yatri','ನಮ್ಮ ಯಾತ್ರಿ ಬಗ್ಗೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('z67evvf7-0evb-4yj5-euhq-h507qy481nis','About namma yatri','நம்ம யாத்ரி பற்றி','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('3gxjxfv3-5tun-9nlh-r51z-ilqeu8coq9ja','About namma yatri','नम्मा यात्री के बारे में','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('993g5r6z-x0un-kasl-kych-bgmdtyb53izh','Namma Yatri is a Direct-to-Driver app. There is no commission or middle-men. What you pay goes 100% to the Driver and his family!','Namma Yatri is a Direct-to-Driver app. There is no commission or middle-men. What you pay goes 100% to the Driver and his family!','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('3blif5jz-n4qi-y91w-dycl-yfudgo40hgjd','Namma Yatri is a Direct-to-Driver app. There is no commission or middle-men. What you pay goes 100% to the Driver and his family!','ನಮ್ಮ ಯಾತ್ರಿ ಎಂಬುದು ಡೈರೆಕ್ಟ್-ಟು-ಡ್ರೈವರ್ ಅಪ್ಲಿಕೇಶನ್ ಆಗಿದೆ. ಯಾವುದೇ ಕಮಿಷನ್ ಅಥವಾ ಮಧ್ಯವರ್ತಿಗಳಿಲ್ಲ. ನೀವು ಪಾವತಿಸುವ ಮೊತ್ತವು 100% ಚಾಲಕ ಮತ್ತು ಅವನ ಕುಟುಂಬಕ್ಕೆ ಹೋಗುತ್ತದೆ!','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('64q4elfe-dur6-y448-30z0-gtk88i7ou29m','Namma Yatri is a Direct-to-Driver app. There is no commission or middle-men. What you pay goes 100% to the Driver and his family!','நம்ம யாத்ரி என்பது நேரடியாக இயக்கி இயக்கும் பயன்பாடாகும். கமிஷனோ, இடைத்தரகர்களோ கிடையாது. நீங்கள் செலுத்தும் தொகை 100% ஓட்டுநருக்கும் அவரது குடும்பத்தினருக்கும் செல்கிறது!','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('jn62h701-wxln-h7ne-ak42-f3snhqibmawh','Namma Yatri is a Direct-to-Driver app. There is no commission or middle-men. What you pay goes 100% to the Driver and his family!','नम्मा यात्री एक डायरेक्ट-टू-ड्राइवर ऐप है। यहां कोई कमीशन या बिचौलिया नहीं है. आप जो भुगतान करते हैं उसका 100% ड्राइवर और उसके परिवार को जाता है!','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('fntpxxda-4kmy-e7yz-dcw0-8wfjxm27k89f','Hey there, We''ve addressed the issue as discussed. For any queries feel free to write to us at nammayatri.support@juspay.in','Hey there, We''ve addressed the issue as discussed. For any queries feel free to write to us at nammayatri.support@juspay.in','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('1f9qht74-hp2o-clh6-tyvm-qqg0vj4tclao','Hey there, We''ve addressed the issue as discussed. For any queries feel free to write to us at nammayatri.support@juspay.in','ಹೇ, ನಾವು ಚರ್ಚಿಸಿದಂತೆ ಸಮಸ್ಯೆಯನ್ನು ಪರಿಹರಿಸಿದ್ದೇವೆ. ಯಾವುದೇ ಪ್ರಶ್ನೆಗಳಿಗೆ nammayatri.support@juspay.in ನಲ್ಲಿ ನಮಗೆ ಬರೆಯಲು ಮುಕ್ತವಾಗಿರಿ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('8ylathtx-2p8g-6fog-tx53-rv4pzn7hq5nu','Hey there, We''ve addressed the issue as discussed. For any queries feel free to write to us at nammayatri.support@juspay.in','வணக்கம், நாங்கள் விவாதித்தபடி சிக்கலைத் தீர்த்தோம். ஏதேனும் கேள்விகளுக்கு, தயங்காமல் எங்களுக்கு எழுதவும், nammayatri.support@juspay.in','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('u5tygogb-ew69-obf9-64qr-akgf64z4ptrj','Hey there, We''ve addressed the issue as discussed. For any queries feel free to write to us at nammayatri.support@juspay.in','नमस्ते, हमने चर्चा के अनुसार मुद्दे का समाधान कर लिया है। किसी भी प्रश्न के लिए बेझिझक हमें nammayatri.support@juspay.in पर लिखें','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('33bhc7yl-vnu1-r76n-6qj7-r97kkhr0oj0v','We''re happy to have helped you with your inquiries. Don''t hesitate to use the help feature on the app for any future questions or concerns.','We''re happy to have helped you with your inquiries. Don''t hesitate to use the help feature on the app for any future questions or concerns.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('f8nrc4ah-8rss-kxit-mb35-x5satx6bfzez','We''re happy to have helped you with your inquiries. Don''t hesitate to use the help feature on the app for any future questions or concerns.','ನಿಮ್ಮ ವಿಚಾರಣೆಯಲ್ಲಿ ನಿಮಗೆ ಸಹಾಯ ಮಾಡಿದ್ದಕ್ಕಾಗಿ ನಾವು ಸಂತೋಷಪಡುತ್ತೇವೆ. ಭವಿಷ್ಯದ ಯಾವುದೇ ಪ್ರಶ್ನೆಗಳು ಅಥವಾ ಕಾಳಜಿಗಳಿಗಾಗಿ ಅಪ್ಲಿಕೇಶನ್‌ನಲ್ಲಿ ಸಹಾಯ ವೈಶಿಷ್ಟ್ಯವನ್ನು ಬಳಸಲು ಹಿಂಜರಿಯಬೇಡಿ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('5r6dk0gv-czo8-ib1i-v53h-4gubdki3fbyw','We''re happy to have helped you with your inquiries. Don''t hesitate to use the help feature on the app for any future questions or concerns.','உங்கள் விசாரணைகளில் உங்களுக்கு உதவியதில் நாங்கள் மகிழ்ச்சியடைகிறோம். எதிர்காலத்தில் ஏதேனும் கேள்விகள் அல்லது கவலைகளுக்கு பயன்பாட்டில் உள்ள உதவி அம்சத்தைப் பயன்படுத்த தயங்க வேண்டாம்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('2iv1cqt7-tg8y-ti15-4ai2-0s0jrzc0r9cw','We''re happy to have helped you with your inquiries. Don''t hesitate to use the help feature on the app for any future questions or concerns.','हमें आपकी पूछताछ में मदद करके खुशी हुई है। भविष्य के किसी भी प्रश्न या चिंता के लिए ऐप पर सहायता सुविधा का उपयोग करने में संकोच न करें।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('6wwq8zaa-nygz-awpg-xbur-ezmdbhbcmshk','Thank you for providing the details','Thank you for providing the details','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('naj1sbv5-ujsm-k5at-a5np-rux9k8lxy1sy','Thank you for providing the details','ವಿವರಗಳನ್ನು ಒದಗಿಸಿದ್ದಕ್ಕಾಗಿ ಧನ್ಯವಾದಗಳು','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('2u5ijg3v-kss4-pl1b-lsnw-ifb7ojdxqzrb','Thank you for providing the details','விவரங்களை வழங்கியதற்கு நன்றி','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('sqhqk4ss-4k2l-er4e-wjgb-dlapr10u3blx','Thank you for providing the details','विवरण उपलब्ध कराने के लिए धन्यवाद','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('b491lwb2-upt0-xoyq-l3zp-1awg7x9wvtvl','Please provide additional information so that we can assist you better.','Please provide additional information so that we can assist you better.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('igrde54y-cq30-a7e6-1s1i-rw1qs7xuaw52','Please provide additional information so that we can assist you better.','ದಯವಿಟ್ಟು ಹೆಚ್ಚುವರಿ ಮಾಹಿತಿಯನ್ನು ಒದಗಿಸಿ ಇದರಿಂದ ನಾವು ನಿಮಗೆ ಉತ್ತಮವಾಗಿ ಸಹಾಯ ಮಾಡಬಹುದು.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('iwk74s9x-tqsm-kz4p-gl9o-aonxiidbevox','Please provide additional information so that we can assist you better.','தயவு செய்து கூடுதல் தகவலை வழங்கவும், நாங்கள் உங்களுக்கு சிறப்பாக உதவ முடியும்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('l0j29hga-wud7-b8xq-da7z-wt2fo6smepze','Please provide additional information so that we can assist you better.','कृपया अतिरिक्त जानकारी प्रदान करें ताकि हम आपकी बेहतर सहायता कर सकें।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('3fiwy286-xwvs-p3sc-sstl-tin2z3oceuyj','Please write to us at nammayatri.support@juspay.in','Please write to us at nammayatri.support@juspay.in','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('n1guprmh-g92a-wlms-xg05-nsuls9mzx0ra','Please write to us at nammayatri.support@juspay.in','ದಯವಿಟ್ಟು ನಮಗೆ nammayatri.support@juspay.in ನಲ್ಲಿ ಬರೆಯಿರಿ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('acm0yeg0-3ser-08yv-bvx3-dcfmpciltdi5','Please write to us at nammayatri.support@juspay.in','nammayatri.support@juspay.in இல் எங்களுக்கு எழுதவும்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('xrpikgfn-1a0m-4k8k-og5o-pzgumxvpct6f','Please write to us at nammayatri.support@juspay.in','कृपया हमें nammayatri.support@juspay.in पर लिखें','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('m5otb28h-iwrt-bxy8-aqv9-toppisffinqu','Please write to us at nammayatri.support@juspay.in with the contact details / Ride ID','Please write to us at nammayatri.support@juspay.in with the contact details / Ride ID','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('jonwbgw1-f3qq-rpqm-cdhq-t1hu17wdeznh','Please write to us at nammayatri.support@juspay.in with the contact details / Ride ID','ದಯವಿಟ್ಟು ಸಂಪರ್ಕ ವಿವರಗಳು / ರೈಡ್ ಐಡಿಯೊಂದಿಗೆ nammayatri.support@juspay.in ನಲ್ಲಿ ನಮಗೆ ಬರೆಯಿರಿ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('m63i74i1-s4n5-cien-66dr-73tyhgy11b8s','Please write to us at nammayatri.support@juspay.in with the contact details / Ride ID','தொடர்பு விவரங்கள் / சவாரி ஐடியுடன் nammayatri.support@juspay.in இல் எங்களுக்கு எழுதவும்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('bluwoipf-13mm-y154-2w3e-zwtcl1xuhdhi','Please write to us at nammayatri.support@juspay.in with the contact details / Ride ID','कृपया हमें संपर्क विवरण / राइड आईडी के साथ nammayatri.support@juspay.in पर लिखें','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('z108ogsp-iwse-dr25-hxw1-vsgnluxsepla','Hey, We’re really sorry to hear you have been facing ride related issues.','Hey, We’re really sorry to hear you have been facing ride related issues.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('238yetyo-0g5c-nhft-7vkf-id7kszd6cmv8','Hey, We’re really sorry to hear you have been facing ride related issues.','ಹೇ, ನೀವು ಸವಾರಿ ಸಂಬಂಧಿತ ಸಮಸ್ಯೆಗಳನ್ನು ಎದುರಿಸುತ್ತಿರುವುದನ್ನು ಕೇಳಲು ನಾವು ನಿಜವಾಗಿಯೂ ವಿಷಾದಿಸುತ್ತೇವೆ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('env56z95-b9co-1p63-j8lt-hxne9xolntrc','Hey, We’re really sorry to hear you have been facing ride related issues.','ஹாய், நீங்கள் சவாரி தொடர்பான சிக்கல்களை எதிர்கொண்டிருப்பதைக் கேட்டு வருந்துகிறோம்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('6mc6v73d-jezj-bx13-29y4-hx5zhzfhkaq4','Hey, We’re really sorry to hear you have been facing ride related issues.','हे, हमें यह जानकर वास्तव में खेद है कि आपको सवारी संबंधी समस्याओं का सामना करना पड़ रहा है।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('lq3wx2d7-70bv-yhc0-vwv3-f897dh0gzr5g','Hey, We’re really sorry to hear you have been facing app related issues.','Hey, We’re really sorry to hear you have been facing app related issues.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('hf3uqr6o-ow86-gdgf-lav6-rq532pdvnhyc','Hey, We’re really sorry to hear you have been facing app related issues.','ಹೇ, ನೀವು ಅಪ್ಲಿಕೇಶನ್ ಸಂಬಂಧಿತ ಸಮಸ್ಯೆಗಳನ್ನು ಎದುರಿಸುತ್ತಿರುವುದನ್ನು ಕೇಳಲು ನಾವು ನಿಜವಾಗಿಯೂ ವಿಷಾದಿಸುತ್ತೇವೆ.','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('jt57h2w3-muj0-f4wq-c8xi-d6b2evsg0hjx','Hey, We’re really sorry to hear you have been facing app related issues.','ஹாய், ஆப்ஸ் தொடர்பான சிக்கல்களை நீங்கள் எதிர்கொண்டிருப்பதைக் கேட்டு வருந்துகிறோம்.','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('6wwj0q2m-q4b7-jhjo-474i-8658hzovv54o','Hey, We’re really sorry to hear you have been facing app related issues.','हे, हमें यह जानकर वास्तव में खेद है कि आपको ऐप से संबंधित समस्याओं का सामना करना पड़ रहा है।','HINDI');

INSERT INTO atlas_app.issue_translation VALUES ('1il347yc-oxpe-ltin-9vs7-l5btv1g2a30u','Hey, We’re sorry to hear about your lost item.','Hey, We’re sorry to hear about your lost item.','ENGLISH');
INSERT INTO atlas_app.issue_translation VALUES ('f3fthhr5-b3f8-qfrf-gg30-xrn9xsu3lacu','Hey, We’re sorry to hear about your lost item.','ಹೇ, ನಿಮ್ಮ ಕಳೆದುಹೋದ ಐಟಂ ಬಗ್ಗೆ ಕೇಳಲು ನಾವು ವಿಷಾದಿಸುತ್ತೇವೆ','KANNADA');
INSERT INTO atlas_app.issue_translation VALUES ('ths17z81-piyz-07wo-m2pu-cj6jslbyxtr8','Hey, We’re sorry to hear about your lost item.','ஏய், உங்கள் தொலைந்து போன பொருளைப் பற்றி அறிந்து வருந்துகிறோம்','TAMIL');
INSERT INTO atlas_app.issue_translation VALUES ('na4wrxwm-6cx4-zvmu-hq8g-qu58zkixta77','Hey, We’re sorry to hear about your lost item.','हे, हमें आपकी खोई हुई वस्तु के बारे में सुनकर दुख हुआ','HINDI');

-- AFTER ISSUE IS CREATED
INSERT INTO atlas_app.issue_message VALUES ('v31ghv31-1234-234f-fb2ds-34v2dfstf1j', null, null, 'Details received! Our team will reach out to you within 24 hours to help you out with the issue.', null, 1);

INSERT INTO atlas_app.issue_message VALUES ('qradsvna-c76f-42f2-8209-68fb00b875ef', null, null, 'This Issue was mark as resolved automatically due to no response within 2 hours.', 'AUTO_MARKED_RESOLVED',1);

INSERT INTO atlas_app.issue_message VALUES ('12m3n3ql-ch17-12cb-34hu-1h23ewdf112j', null, null, 'Hey there, We''ve addressed the issue as discussed. For any queries feel free to write to us at nammayatri.support@juspay.in', null, 1);

INSERT INTO atlas_app.issue_option VALUES ('adasasd1-d58e-234v-8i6g-3nbtdsfvsnfv', null, '123md312-ch17-u3tj-123d-febf223b12j3', 'Yes, mark this issue as resolved', 'MARK_RESOLVED', 1);
INSERT INTO atlas_app.issue_option VALUES ('adv1v23f-6j5u-y4km-bdwc-1g3fsdaft12f', null, '123md312-ch17-u3tj-123d-febf223b12j3', 'I need more help', null, 2);

INSERT INTO atlas_app.issue_message VALUES ('wefgwfye-thef-12gf-4ybh-bhsdfbvgyw32', 'adasasd1-d58e-234v-8i6g-3nbtdsfvsnfv', null, 'We are glad to be of help. You can reach out to us in case of any other issues using help & support option in the side menu', null, 1);
INSERT INTO atlas_app.issue_message VALUES ('asdg12ev-546h-7j5j-4hrg-34jtnrgwfbhf', 'adv1v23f-6j5u-y4km-bdwc-1g3fsdaft12f', null, 'Would you like to talk to our customer support to help you out further?', null, 1);

INSERT INTO atlas_app.issue_option VALUES ('awdvgsdd-bhtf-234v-8i6g-3nbtdsfvsnfv', null, 'asdg12ev-546h-7j5j-4hrg-34jtnrgwfbhf', 'Talk to Customer Support', 'CALL_SUPPORT', 1);
INSERT INTO atlas_app.issue_option VALUES ('bu4tbrfg-u6jn-dnj2-j6ne-qwd7fbe123gs', null, 'asdg12ev-546h-7j5j-4hrg-34jtnrgwfbhf', 'Mark this issue as resolved.', 'MARK_RESOLVED', 2);

INSERT INTO atlas_app.issue_message VALUES ('fjadshj4-ch17-73hv-3jit-asdgv1j2g331', 'bu4tbrfg-u6jn-dnj2-j6ne-qwd7fbe123gs', null, 'We are glad to be of help. You can reach out to us in case of any other issues using help & support option in the side menu', null, 1);

INSERT INTO atlas_app.issue_message VALUES ('123md312-ch17-u3tj-123d-febf223b12j3', 'awdvgsdd-bhtf-234v-8i6g-3nbtdsfvsnfv', null, 'Was your issue resolved succesfully?', null, 2);
