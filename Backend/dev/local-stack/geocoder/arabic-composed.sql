-- Streets and stops composed entirely from names we already hold.
--
-- Every token in these came from one of two places: geo.place itself, where
-- 3,080 rows already carry an Arabic name, or a hand-written table of the
-- elements Mauritanian street names are built from -- `Ould` appears 72 times
-- in 247 rows, `Cheikh` 23, `Sidi` 14. Nothing here was transliterated.
--
-- A phonetic fallback for the remaining surnames was written and then thrown
-- away: it produced هابا for Haiba where the real name is هيبة, and لي for
-- Ely. Plausible-looking wrong spellings are worse than an empty field,
-- because they invite acceptance. Those rows went to a human instead.

begin;
update geo.place set name_ar = 'كوكي - دييما' where id = 3332;  -- Gogui - Diéma
update geo.place set name_ar = 'موريتانيا - كوكي - نيورو الساحل - دييما - باماكو' where id = 3390;  -- Mauritanie - Gogui - Nioro du Sahel - Diéma - Bamako
update geo.place set name_ar = 'موريتانيا - كوكي - نيورو الساحل - دييما - باماكو' where id = 3388;  -- Mauritanie - Gogui - Nioro du Sahel - Diéma - Bamako
update geo.place set name_ar = 'موريتانيا - كوكي - نيورو الساحل - دييما - باماكو' where id = 3387;  -- Mauritanie - Gogui - Nioro du Sahel - Diéma - Bamako
update geo.place set name_ar = 'موريتانيا - كوكي - نيورو الساحل - دييما - باماكو' where id = 3389;  -- Mauritanie - Gogui - Nioro du Sahel - Diéma - Bamako
update geo.place set name_ar = 'موريتانيا - كوكي - نيورو الساحل - دييما - باماكو' where id = 3391;  -- Mauritanie - Gogui - Nioro du Sahel - Diéma - Bamako
update geo.place set name_ar = 'N1' where id = 3443;  -- N1
update geo.place set name_ar = 'طريق روصو - بوغي' where id = 3620;  -- Route Rosso - Boghé
update geo.place set name_ar = 'يليماني - كيراني - نيورو - بالي - ديلي - غومبو' where id = 4035;  -- Yélimané – Kirané – Nioro – Ballé – Dilli – Goumbou.
update geo.place set name_ar = 'يليماني - كيراني - نيورو - بالي - ديلي - غومبو' where id = 4034;  -- Yélimané – Kirané – Nioro – Ballé – Dilli – Goumbou.
update geo.place set name_ar = 'شارع الحاج عمر تال' where id = 3175;  -- Avenue El Hadji Oumar Tall
update geo.place set name_ar = 'بابه ولد الشيخ سيديا' where id = 3191;  -- Babe Ould Cheikh Sidya
update geo.place set name_ar = 'المفترق 24' where id = 3240;  -- Carrefour 24
update geo.place set name_ar = 'الشيخ سيد أحمد الكنتي' where id = 3267;  -- Cheikh Sid'Ahmed El Kounty
update geo.place set name_ar = 'محمد ولد مولاي' where id = 3429;  -- Mohamed Ould Moulaye
update geo.place set name_ar = 'الحاج محمود با' where id = 3305;  -- El Hadj Mahmoud Ba
update geo.place set name_ar = 'ماسينا محمد لامين' where id = 3378;  -- Macina Mamadou Lamine
update geo.place set name_ar = 'مبرا - أغور' where id = 3395;  -- Mbéra-Aghor
update geo.place set name_ar = 'عبد العزيز كان' where id = 3106;  -- Abdel Aziz Kane
update geo.place set name_ar = 'عبد الله فال' where id = 3109;  -- Abdellahi Fall
update geo.place set name_ar = 'عبد الوهاب الشيقر' where id = 3110;  -- Abdel Wahab Cheiguer
update geo.place set name_ar = 'اعبيدي ولد الغرابي' where id = 3114;  -- Abeidy Ould El Gharraby
update geo.place set name_ar = 'أحمد ولد العاقل' where id = 3124;  -- Ahmed Ould El Aghel
update geo.place set name_ar = 'أحمد ولد كركوب' where id = 3126;  -- Ahmed Ould Kerkoub
update geo.place set name_ar = 'أحمد سالم' where id = 3128;  -- Ahmed Salem
update geo.place set name_ar = 'أحمد سالم ولد هيبة' where id = 3129;  -- Ahmed Salem Ould Haiba
update geo.place set name_ar = 'أحمد سالم ولد مولاي' where id = 3130;  -- Ahmed Salem Ould Moulaye
update geo.place set name_ar = 'أمادو المختار ساكو' where id = 3140;  -- Amadou Moktar Sakho
update geo.place set name_ar = 'باكاو لوبل' where id = 3195;  -- Bakaw/ Lopel
update geo.place set name_ar = 'با محمود' where id = 3197;  -- Ba Mahmoud
update geo.place set name_ar = 'إبراهيم اخليل ولد بابته' where id = 3229;  -- Brahim Khill Ould Babetta
update geo.place set name_ar = 'إبراهيم اخليل ولد بابته' where id = 3228;  -- Brahim Khill Ould Babetta
update geo.place set name_ar = 'الحسن ولد الشيخ ولد أحمد سالم' where id = 3343;  -- Hassan ould Cheikh ould Ahmed Salem
update geo.place set name_ar = 'إسماعيل عبد سي' where id = 3354;  -- Ismail Abdoul Sy
update geo.place set name_ar = 'محمد عبد الله' where id = 3402;  -- Mohamed Abdellahi
update geo.place set name_ar = 'محمد خطري' where id = 3412;  -- Mohamed Khatry
update geo.place set name_ar = 'اركيز - بوتلميت' where id = 3484;  -- R'Kiz - Boutilimit
update geo.place set name_ar = 'طريق بوغي إلى باكاو' where id = 3496;  -- Route boghe vers Bakaw
update geo.place set name_ar = 'طريق بوتلميت - روصو' where id = 3499;  -- Route Boutilimit - Rosso
update geo.place set name_ar = 'سيدي ولد مولاي زين' where id = 3999;  -- Sidi Ould Moulaye Zeine
update geo.place set name_ar = 'مسار إلى أطار' where id = 4019;  -- track to atar
update geo.place set name_ar = 'مسار إلى أطار' where id = 4020;  -- track to atar
update geo.place set name_ar = 'مسار إلى أطار' where id = 4017;  -- track to atar
update geo.place set name_ar = 'مسار إلى أطار' where id = 4018;  -- track to atar
update geo.place set name_ar = 'شاطئ مسار' where id = 3212;  -- Beach Trail
update geo.place set name_ar = 'شاطئ مسار' where id = 3214;  -- Beach Trail
update geo.place set name_ar = 'شاطئ مسار' where id = 3213;  -- Beach Trail
update geo.place set name_ar = 'شاطئ مسار' where id = 3211;  -- Beach Trail
update geo.place set name_ar = 'قصر الكونغرس' where id = 3470;  -- Palais des congrés
update geo.place set name_ar = 'شاطئ مسار' where id = 3209;  -- Beach Trail
update geo.place set name_ar = 'شاطئ مسار' where id = 3210;  -- Beach Trail
update geo.place set name_ar = 'إلى مدرسة مدينة' where id = 4025;  -- Vers école MEDINA
update geo.place set name_ar = '04 22' where id = 4212;  -- 04/22
update geo.place set name_ar = 'المفترق 24' where id = 6739;  -- Carrefour 24
update geo.place set name_ar = 'المفترق أدرر' where id = 6740;  -- Carrefour Adrar
update geo.place set name_ar = 'مركز الإرسال' where id = 6860;  -- Centre émetteur
update geo.place set name_ar = 'المنزل دار نايم' where id = 7235;  -- dar Naim

select kind, count(*) filter (where name_ar is not null) as named, count(*) as rows
  from geo.place group by kind order by kind;
commit;
