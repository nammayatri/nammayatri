CREATE TABLE IF NOT EXISTS atlas_app.disability (
	id character varying(36) NOT NULL,
	tag character varying(255) NOT NULL,
	description character varying(255) NOT NULL
);
ALTER TABLE atlas_app.disability OWNER TO atlas_app_user;


INSERT INTO atlas_app.disability VALUES
	(atlas_app.uuid_generate_v4(), 'BLIND_LOW_VISION', 'Blind/Low Vision'),
	(atlas_app.uuid_generate_v4(), 'HEAR_IMPAIRMENT', 'Hearing Impairment (Deaf/Mute)'),
	(atlas_app.uuid_generate_v4(), 'LOCOMOTOR_DISABILITY', 'Locomotor Disability'),
	(atlas_app.uuid_generate_v4(), 'OTHER', 'Other');


CREATE TABLE IF NOT EXISTS atlas_app.disability_translation (
	disability_id character varying(36) NOT NULL,
	disability_tag character varying(255) NOT NULL,
	translation character varying(255) NOT NULL,
	language character varying(255) NOT NULL
);

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'Blind/Low Vision',
		'ENGLISH'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'BLIND_LOW_VISION'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'ಕುರುಡು/ಕಡಿಮೆ ದೃಷ್ಟಿ',
		'KANNADA'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'BLIND_LOW_VISION'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'குருட்டு/குறைந்த பார்வை',
		'TAMIL'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'BLIND_LOW_VISION'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'अंध/कम दृष्टि',
		'HINDI'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'BLIND_LOW_VISION'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'ബ്ലൈൻഡ്/ലോ വിഷൻ',
		'MALAYALAM'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'BLIND_LOW_VISION'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'Aveugle/basse vision',
		'FRENCH'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'BLIND_LOW_VISION'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'অন্ধ/নিম্ন দৃষ্টি',
		'BENGALI'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'BLIND_LOW_VISION'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'Hearing Impairment (Deaf/Mute)',
		'ENGLISH'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'HEAR_IMPAIRMENT'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'ಶ್ರವಣ ದೋಷ (ಕಿವುಡ/ಮೂಕ)',
		'KANNADA'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'HEAR_IMPAIRMENT'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'செவித்திறன் குறைபாடு (செவிடு/ஊமை)',
		'TAMIL'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'HEAR_IMPAIRMENT'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'श्रवण बाधित (बधिर/मूक)',
		'HINDI'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'HEAR_IMPAIRMENT'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'ശ്രവണ വൈകല്യം (ബധിരൻ/മൂകൻ)',
		'MALAYALAM'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'HEAR_IMPAIRMENT'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'Déficience auditive (sourd/muet)',
		'FRENCH'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'HEAR_IMPAIRMENT'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'শ্রবণ প্রতিবন্ধকতা (বধির/নিঃশব্দ)',
		'BENGALI'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'HEAR_IMPAIRMENT'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'Locomotor Disability',
		'ENGLISH'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'LOCOMOTOR_DISABILITY'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'ಲೊಕೊಮೊಟರ್ ಅಸಾಮರ್ಥ್ಯ',
		'KANNADA'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'LOCOMOTOR_DISABILITY'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'லோகோமோட்டர் இயலாமை',
		'TAMIL'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'LOCOMOTOR_DISABILITY'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'लोकोमोटर विकलांगता',
		'HINDI'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'LOCOMOTOR_DISABILITY'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'ലോക്കോമോട്ടർ വൈകല്യം',
		'MALAYALAM'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'LOCOMOTOR_DISABILITY'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'Handicap locomoteur',
		'FRENCH'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'LOCOMOTOR_DISABILITY'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'লোকোমোটর অক্ষমতা',
		'BENGALI'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'LOCOMOTOR_DISABILITY'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'Other',
		'ENGLISH'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'OTHER'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'ಇತರೆ',
		'KANNADA'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'OTHER'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'மற்றவை',
		'TAMIL'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'OTHER'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'अन्य',
		'HINDI'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'OTHER'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'മറ്റുള്ളവ',
		'MALAYALAM'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'OTHER'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'Autre',
		'FRENCH'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'OTHER'
	ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.disability_translation (disability_id, disability_tag, translation, language)
	SELECT
		T1.id,
		T1.tag,
		'অন্যান্য',
		'BENGALI'
	FROM atlas_app.disability AS T1
	WHERE T1.tag = 'OTHER'
	ON CONFLICT DO NOTHING;