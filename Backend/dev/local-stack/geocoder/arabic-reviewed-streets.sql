-- The 247 street and transport names, after review, 2026-09-10.
--
-- Composed rather than translated. Every token came from one of three
-- places, in order: geo.place itself (Rosso is روصو because our own index
-- says so, which stops a town being spelled one way as a locality and
-- another inside a road name), a table of the elements Mauritanian street
-- names are built from, and a hand-written list of the 257 words neither
-- covered -- written as NAMES, not as letter sequences: Melainine is ماء
-- العينين, which no transliteration reaches.
--
-- Reviewed on a page that arrived pre-filled, so an untouched row is an
-- accepted row. Five were corrected -- تعشوت for the composed تاشوط among them.
--
-- THREE ARE DELIBERATELY NOT HERE, and stay that way: Bassikonou-Aghor twice
-- and Carrefour 3 Poteaux. Their edits read like a deletion that was never
-- finished -- half a two-town road name, with the leading space still on it --
-- and asked about it the reviewer said to leave the French. So they show what
-- the other 6,681 unnamed places show, which is readable and true. A half-typed
-- name would have been neither.

begin;
update geo.place set name_ar = 'كوكي - دييما' where id = 3332;  -- Gogui - Diéma
update geo.place set name_ar = 'موريتانيا - كوكي - نيورو الساحل - دييما - باماكو' where id = 3390;  -- Mauritanie - Gogui - Nioro du Sahel - Diéma - Bamako
update geo.place set name_ar = 'موريتانيا - كوكي - نيورو الساحل - دييما - باماكو' where id = 3388;  -- Mauritanie - Gogui - Nioro du Sahel - Diéma - Bamako
update geo.place set name_ar = 'موريتانيا - كوكي - نيورو الساحل - دييما - باماكو' where id = 3387;  -- Mauritanie - Gogui - Nioro du Sahel - Diéma - Bamako
update geo.place set name_ar = 'موريتانيا - كوكي - نيورو الساحل - دييما - باماكو' where id = 3389;  -- Mauritanie - Gogui - Nioro du Sahel - Diéma - Bamako
update geo.place set name_ar = 'موريتانيا - كوكي - نيورو الساحل - دييما - باماكو' where id = 3391;  -- Mauritanie - Gogui - Nioro du Sahel - Diéma - Bamako
update geo.place set name_ar = 'الطريق الوطني 1' where id = 3443;  -- N1
update geo.place set name_ar = 'طريق روصو - بوغي' where id = 3620;  -- Route Rosso - Boghé
update geo.place set name_ar = 'تعشوت' where id = 4006;  -- Tachoot
update geo.place set name_ar = 'يليماني - كيراني - نيورو - بالي - ديلي - غومبو' where id = 4035;  -- Yélimané – Kirané – Nioro – Ballé – Dilli – Goumbou.
update geo.place set name_ar = 'يليماني - كيراني - نيورو - بالي - ديلي - غومبو' where id = 4034;  -- Yélimané – Kirané – Nioro – Ballé – Dilli – Goumbou.
update geo.place set name_ar = 'شارع الحاج عمر تال' where id = 3175;  -- Avenue El Hadji Oumar Tall
update geo.place set name_ar = 'بابه ولد الشيخ سيديا' where id = 3191;  -- Babe Ould Cheikh Sidya
update geo.place set name_ar = 'بداه ولد بوسيري' where id = 3224;  -- Bouddah Ould Bousseiry
update geo.place set name_ar = 'المفترق 24' where id = 3240;  -- Carrefour 24
update geo.place set name_ar = 'الشيخ سيد أحمد الكنتي' where id = 3267;  -- Cheikh Sid'Ahmed El Kounty
update geo.place set name_ar = 'محمد ولد مولاي' where id = 3429;  -- Mohamed Ould Moulaye
update geo.place set name_ar = 'تكنت ركيز' where id = 4013;  -- Tiguent - Rkiz
update geo.place set name_ar = 'المفترق بي إم دي' where id = 3244;  -- Carrefour BMD
update geo.place set name_ar = 'المفترق بداها' where id = 3245;  -- Carrefour Boudaha
update geo.place set name_ar = 'طريق إلى بدينكي و سيثيان' where id = 3277;  -- chemin vers Bédinki et Sithiane
update geo.place set name_ar = 'الحاج محمود با' where id = 3305;  -- El Hadj Mahmoud Ba
update geo.place set name_ar = 'الحسن بن علي ابن أبي طالب' where id = 3336;  -- Hacen Ben Ali Ibn Abi Taleb
update geo.place set name_ar = 'ماسينا محمد لامين' where id = 3378;  -- Macina Mamadou Lamine
update geo.place set name_ar = 'مبرا - أغور' where id = 3395;  -- Mbéra-Aghor
update geo.place set name_ar = 'مسار جوج' where id = 3473;  -- Piste de Djoudj
update geo.place set name_ar = 'دوار كوخ' where id = 3486;  -- Rond Point cabanon
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
update geo.place set name_ar = 'طريق اوللول' where id = 3151;  -- Aouelloul Road
update geo.place set name_ar = 'شارع ديانيل' where id = 3168;  -- Avenue Dianel
update geo.place set name_ar = 'باكاو لوبل' where id = 3195;  -- Bakaw/ Lopel
update geo.place set name_ar = 'باكاو إلى غوريل فوندي' where id = 3196;  -- Bakaw vers Gourel Fonde
update geo.place set name_ar = 'با محمود' where id = 3197;  -- Ba Mahmoud
update geo.place set name_ar = 'ببها ولد العاقل' where id = 3215;  -- Bebbaha Ould El Aghel
update geo.place set name_ar = 'البشير ولد بوزيد' where id = 3216;  -- Bechir Ould Bezeid
update geo.place set name_ar = 'إبراهيم اخليل ولد بابته' where id = 3229;  -- Brahim Khill Ould Babetta
update geo.place set name_ar = 'إبراهيم اخليل ولد بابته' where id = 3228;  -- Brahim Khill Ould Babetta
update geo.place set name_ar = 'إبراهيم ولد موكيه' where id = 3230;  -- Brahim Ould Mogueya
update geo.place set name_ar = 'طريق' where id = 3231;  -- camino
update geo.place set name_ar = 'طريق صغير 2' where id = 3232;  -- Camino Menor 2
update geo.place set name_ar = 'الشيخ المهدي' where id = 3254;  -- Cheikh El Mehdi
update geo.place set name_ar = 'الشيخ حماه الله' where id = 3255;  -- Cheikh Hamahoullah
update geo.place set name_ar = 'الشيخ حموني' where id = 3256;  -- Cheikh Hammonni
update geo.place set name_ar = 'الشيخ ماء العينين' where id = 3257;  -- Cheikh Melainine
update geo.place set name_ar = 'الشيخ ماء العينين روبير' where id = 3258;  -- Cheikh Mélanine Robert
update geo.place set name_ar = 'الشيخ محمد مامي' where id = 3259;  -- Cheikh Mohamed el Mamy
update geo.place set name_ar = 'الشيخ محمد فاضل' where id = 3261;  -- Cheikh Mohamed Vadhel
update geo.place set name_ar = 'شيخنا ولد محمد لغظف' where id = 3262;  -- Cheikhna Ould Mohamed Laghdaf
update geo.place set name_ar = 'الشيخ ولد النويصري' where id = 3264;  -- Cheikh Ould Noueisry
update geo.place set name_ar = 'الشيخ سعد بوه' where id = 3266;  -- Cheikh Saad Bouh
update geo.place set name_ar = 'الشيخ سيد أحمد لعروسي' where id = 3268;  -- Cheikh Sid'Ahmed Learoussi
update geo.place set name_ar = 'الشيخ سيد أحمد اقيبي' where id = 3269;  -- Cheikh Sid' Ahmed R Gueiby
update geo.place set name_ar = 'الشيخ سيداتي ولد الشيخ طالب بويه' where id = 3270;  -- Cheikh Sidaty Ould Cheikh Taleb Bouya
update geo.place set name_ar = 'الشيخ سيد ال المختار التندغي' where id = 3271;  -- Cheikh Sid'El Moktar Tendghy
update geo.place set name_ar = 'الشيخ سيدي محمد بن أومواس' where id = 3272;  -- Cheikh Sidi Mohamed Ben Omois
update geo.place set name_ar = 'الشيخ طوراد ولد عباس' where id = 3273;  -- Cheikh Tourad O/Abass
update geo.place set name_ar = 'الشريف صبار' where id = 3278;  -- Cherif Sabar
update geo.place set name_ar = 'العقيد الشيخ ولد البيضاء' where id = 3281;  -- Colonel Cheikh Ould Beida
update geo.place set name_ar = 'كوريا الشمالي' where id = 3288;  -- Corée du Nord
update geo.place set name_ar = 'كوريا الجنوبي' where id = 3289;  -- Corée du Sud
update geo.place set name_ar = 'داه ولد سيدي هيبة' where id = 3290;  -- Dah Ould Sidi Haiba
update geo.place set name_ar = 'دبينوخو كيلي' where id = 3293;  -- Débinokho kilé
update geo.place set name_ar = 'دياغانا عثمان ديالو' where id = 3295;  -- Diagana Ousmane Diallo
update geo.place set name_ar = 'الدكتور عبد الله ولد باه' where id = 3299;  -- Docteur Abdoullahi Ould Bah
update geo.place set name_ar = 'دي ولد زين' where id = 3301;  -- Dy Ould Zeine
update geo.place set name_ar = 'العربي ولد زركان' where id = 3302;  -- El Arby Ould Zergane
update geo.place set name_ar = 'الحاج عبد انغايدي' where id = 3303;  -- El Hadj Abdoul N'Gaydé
update geo.place set name_ar = 'الحاج كويمل' where id = 3304;  -- El Hadj Koueymel
update geo.place set name_ar = 'الحضرمي ولد خطري' where id = 3309;  -- El Hadrami Ould Khatry
update geo.place set name_ar = 'طريق امريتي الشمالي' where id = 3311;  -- El Mreiti Road North
update geo.place set name_ar = 'طريق امريتي الجنوبي' where id = 3312;  -- El Mreiti Road South
update geo.place set name_ar = 'إعلي ولد بيدد' where id = 3315;  -- Ely Ould Beydedde
update geo.place set name_ar = 'فال عبد الرحمن' where id = 3321;  -- Fall Abdarahmane
update geo.place set name_ar = 'شارع فاطمة اوفلي' where id = 3322;  -- Fatimetou Aoufly street
update geo.place set name_ar = 'فيليكس هوفويت بوانيي' where id = 3324;  -- Felix Houphouet Boigny
update geo.place set name_ar = 'فودي هادية سيسي' where id = 3325;  -- Fodie Hadietou Cisse
update geo.place set name_ar = 'فودي سيدي كويتا' where id = 3326;  -- Fodie Sidi Koita
update geo.place set name_ar = 'الغيثي ولد عبد الهايه' where id = 3330;  -- Ghaithy ould Abdel Haye
update geo.place set name_ar = 'غالي ولد عبد حميد' where id = 3331;  -- Ghali Ould Abdel Hamid
update geo.place set name_ar = 'اقيق ولد آله' where id = 3333;  -- Guig Ould Ale
update geo.place set name_ar = 'هيبة ولد اظمين' where id = 3339;  -- Haiba Ould Dhmine
update geo.place set name_ar = 'حامد ولد نجيم' where id = 3341;  -- Hamed Ould Nagim
update geo.place set name_ar = 'الحسن ولد الشيخ ولد أحمد سالم' where id = 3343;  -- Hassan ould Cheikh ould Ahmed Salem
update geo.place set name_ar = 'الحبيب ولد الحريطاني' where id = 3346;  -- H'Bib Ould H'Reitani
update geo.place set name_ar = 'همين ولد الرويسي' where id = 3347;  -- Hemeyenne Ould Eroissi
update geo.place set name_ar = 'حمودي ولد محمود' where id = 3348;  -- Hemody ould Mahmoud
update geo.place set name_ar = 'إسماعيل عبد سي' where id = 3354;  -- Ismail Abdoul Sy
update geo.place set name_ar = 'جدو ولد اخليفه' where id = 3356;  -- Jiddou Ould Kh'Live
update geo.place set name_ar = 'لام ألفا بوكار' where id = 3370;  -- Lam Alpha Bocar
update geo.place set name_ar = 'لمرابط سيدي محمود' where id = 3374;  -- Lembrabott Sidi Mahmoud
update geo.place set name_ar = 'لمرابط ولد برو' where id = 3375;  -- Lemrabott Ould Berrou
update geo.place set name_ar = 'الطريق الرئيسي' where id = 3381;  -- Main Road
update geo.place set name_ar = 'مأموني ولد المختار مبارك' where id = 3385;  -- Mamouny Ould Moktar M'bareck
update geo.place set name_ar = 'مبارك الشلحي' where id = 3393;  -- M'bareck Chelhi
update geo.place set name_ar = 'امباي واد' where id = 3394;  -- Mbaye Wade
update geo.place set name_ar = 'محمد الأمين ولد الشيخ' where id = 3397;  -- Med Lemine Ould Cheikh T
update geo.place set name_ar = 'محمد ولد بلال جولي' where id = 3398;  -- Med L. O. Bilal Djouly
update geo.place set name_ar = 'ميمونة ولد بلال' where id = 3399;  -- Meymoune Ould Bilal
update geo.place set name_ar = 'محمد عبد الله' where id = 3402;  -- Mohamed Abdellahi
update geo.place set name_ar = 'محمد عبد الله ولد مبارك' where id = 3403;  -- Mohamed Abdellahi Ould El Moubareck
update geo.place set name_ar = 'محمد الشيخ ولد ديدي السملالي' where id = 3406;  -- Mohamed Cheikh Ould Dide Semlali
update geo.place set name_ar = 'محمد المختار ولد بلعامش' where id = 3409;  -- Mohamed El Moktar Ould Bellaamech
update geo.place set name_ar = 'محمد خطري' where id = 3412;  -- Mohamed Khatry
update geo.place set name_ar = 'محمد الأمين ولد عبد الهايه' where id = 3415;  -- Mohamed Lemine Ould Abdel Haye
update geo.place set name_ar = 'محمد الأمين ولد التلاميذ' where id = 3417;  -- Mohamed Lemine Ould Tlamid
update geo.place set name_ar = 'محمد محمود ولد التلاميذ تركزي' where id = 3419;  -- Mohamed Mahmoud ould Tlamid Terkzi
update geo.place set name_ar = 'محمد ولد عبد ودود' where id = 3421;  -- Mohamed Ould Abdel Wedoud
update geo.place set name_ar = 'محمد ولد غال' where id = 3423;  -- Mohamed Ould Ghall
update geo.place set name_ar = 'محمد ولد غناه الله' where id = 3424;  -- Mohamed Ould Ghnahallah
update geo.place set name_ar = 'محمد ولد هيماسو' where id = 3425;  -- Mohamed Ould Haimassou
update geo.place set name_ar = 'محمد ولد خيار' where id = 3426;  -- Mohamed Ould Khayar
update geo.place set name_ar = 'محمد ولد اخليل' where id = 3427;  -- Mohamed Ould Kh'lil
update geo.place set name_ar = 'محمد ولد محمد سالم مدلشي' where id = 3428;  -- Mohamed Ould Mohamed Salem El Medelchi
update geo.place set name_ar = 'محمد ولد رمضان' where id = 3430;  -- Mohamed Ould Ramdhane
update geo.place set name_ar = 'محمد صالح' where id = 3431;  -- Mohamed Salah
update geo.place set name_ar = 'محمد تكدي' where id = 3432;  -- Mohamed Teguedi
update geo.place set name_ar = 'المختار ولد حدار' where id = 3436;  -- Moktar Ould Heddar
update geo.place set name_ar = 'محمدن ولد سيدي إبراهيم' where id = 3438;  -- Mouhameden Ould Sidi Brahim
update geo.place set name_ar = 'مولاي أحمد ولد الغرابي' where id = 3439;  -- Moulaye Ahmed Ould El Gharrabi
update geo.place set name_ar = 'مولاي ولد هاشم ولد مادي' where id = 3440;  -- Moulaye Ould Hacheme Ould Madi
update geo.place set name_ar = 'المتعالي' where id = 3441;  -- Moutaly
update geo.place set name_ar = 'امربيه ولد العابدين' where id = 3442;  -- M'Rabih Ould Abidine
update geo.place set name_ar = 'ولد بوبوت' where id = 3463;  -- Ould Boubout
update geo.place set name_ar = 'ممر أموجار' where id = 3471;  -- Passe d'Amogjar
update geo.place set name_ar = 'باتريس لومومبا' where id = 3472;  -- Patrice Lumumba
update geo.place set name_ar = 'مسار بدون اسم' where id = 3474;  -- piste non nommée
update geo.place set name_ar = 'اركيز - بوتلميت' where id = 3484;  -- R'Kiz - Boutilimit
update geo.place set name_ar = 'طريق بوغي إلى باكاو' where id = 3496;  -- Route boghe vers Bakaw
update geo.place set name_ar = 'طريق بوتلميت - روصو' where id = 3499;  -- Route Boutilimit - Rosso
update geo.place set name_ar = 'طريق مبرا لكارة' where id = 3590;  -- Route de Mbéra à Legara
update geo.place set name_ar = 'طريق قرية غوثيوب' where id = 3609;  -- route de village de gouthioube
update geo.place set name_ar = 'طريق نقوش صخرية' where id = 3616;  -- Route peintures rupestres
update geo.place set name_ar = 'طريق سكني' where id = 3618;  -- Route Residentiel
update geo.place set name_ar = 'طريق شاطئ سموكي' where id = 3635;  -- Route smoky beach
update geo.place set name_ar = 'شارع القصبة' where id = 3741;  -- Rue de la Casbah
update geo.place set name_ar = 'شارع الحداثة' where id = 3744;  -- Rue de la Modernité
update geo.place set name_ar = 'شارع الكثبان الذهبية' where id = 3768;  -- Rue des Dunes d'Or
update geo.place set name_ar = 'شارع المنحدرات' where id = 3772;  -- Rue des Falaises
update geo.place set name_ar = 'شارع الصيادين' where id = 3774;  -- Rue des Pêcheurs
update geo.place set name_ar = 'شارع الشعراء' where id = 3775;  -- Rue des Poètes
update geo.place set name_ar = 'شارع الصحراء' where id = 3800;  -- Rue du Sahara
update geo.place set name_ar = 'شارع محمد السالك ولد بنيجارة' where id = 3895;  -- Rue Mohamed Saleck Ould B'Neijara
update geo.place set name_ar = 'شارع الرئيسي' where id = 3926;  -- Rue Principale
update geo.place set name_ar = 'شارع سكنية' where id = 3928;  -- Rue residentielle
update geo.place set name_ar = 'شارع سيدي مبارك ولد أحمد بابا' where id = 3943;  -- Rue Sidi M'Bareck Ould Ahmed Baba
update geo.place set name_ar = 'الصديقي بوكار سيري' where id = 3972;  -- Sadigui Bocar Ciré
update geo.place set name_ar = 'ساموري ولد برهوم' where id = 3978;  -- Samory Ould Barhoum
update geo.place set name_ar = 'الثانوي' where id = 3979;  -- secondaire
update geo.place set name_ar = 'سيد أحمد عود عايدة' where id = 3986;  -- Sid'Ahmed Oud Aida
update geo.place set name_ar = 'سيد أحمد ولد حمدينو' where id = 3987;  -- Sid'Ahmed Ould Hamdinou
update geo.place set name_ar = 'سيد أحمد ولد خيار' where id = 3988;  -- Sid'Ahmed Ould Khyar
update geo.place set name_ar = 'سيد أحمد ولد لاب' where id = 3989;  -- Sid'Ahmed Ould Lab
update geo.place set name_ar = 'سيد أحمد ولد نوبي' where id = 3991;  -- Sid'Ahmed ould Noby
update geo.place set name_ar = 'سيد إعلي ولد عساس' where id = 3992;  -- Sid Ely Ould Assas
update geo.place set name_ar = 'سيدي عبد الله ولد الحاج إبراهيم' where id = 3993;  -- Sidi Abdoullah Ould El Hadj Brahim
update geo.place set name_ar = 'سيدي بونا ولد سيدي' where id = 3994;  -- Sidi Bouna Ould Sidi
update geo.place set name_ar = 'سيدي بونا ولد سيدي' where id = 3995;  -- Sidi Bouna Ould Sidi
update geo.place set name_ar = 'سيدينا ولد الذيب' where id = 3997;  -- Sidina Ould Dhib
update geo.place set name_ar = 'سيدي ولد حنن' where id = 3998;  -- Sidi Ould Hanenne
update geo.place set name_ar = 'سيدي ولد مولاي زين' where id = 3999;  -- Sidi Ould Moulaye Zeine
update geo.place set name_ar = 'سيدي يحيى الكبير' where id = 4000;  -- Sidi Yahya El Kebir
update geo.place set name_ar = 'سيدنا ولد الشيخ طالب بويه' where id = 4001;  -- Sidna Ould Cheikh Taleb Bouya
update geo.place set name_ar = 'مخرج إلى بوغل' where id = 4003;  -- sortie vers boguel
update geo.place set name_ar = 'سويدات ولد ودادة' where id = 4004;  -- Soueidatt Ould Waddade
update geo.place set name_ar = 'تييرنو انجاي با' where id = 4008;  -- Thierno N'diaye Ba
update geo.place set name_ar = 'تييرنو عثمان با' where id = 4009;  -- Thierno Ousmane Ba
update geo.place set name_ar = 'مسار إلى أطار' where id = 4019;  -- track to atar
update geo.place set name_ar = 'مسار إلى أطار' where id = 4020;  -- track to atar
update geo.place set name_ar = 'مسار إلى أطار' where id = 4017;  -- track to atar
update geo.place set name_ar = 'مسار إلى أطار' where id = 4018;  -- track to atar
update geo.place set name_ar = 'مسار إلى أم القرى' where id = 4021;  -- track to omelqoura
update geo.place set name_ar = 'إلى جزيرة تركه' where id = 4026;  -- Vers gazret terke
update geo.place set name_ar = 'إلى جزيرة تركه 2' where id = 4027;  -- Vers gazret terke 2
update geo.place set name_ar = 'فتن' where id = 4028;  -- Veten
update geo.place set name_ar = 'يحيى ولد بوعماتو' where id = 4031;  -- Yahya Ould Bouamatou
update geo.place set name_ar = 'زين ولد أبغري' where id = 4039;  -- Zein Ould Abghary
update geo.place set name_ar = 'طريق المدخل إلى موقف السيارات' where id = 3120;  -- Access road to Car Park
update geo.place set name_ar = 'مسار الشاطئ' where id = 3212;  -- Beach Trail
update geo.place set name_ar = 'مسار الشاطئ' where id = 3214;  -- Beach Trail
update geo.place set name_ar = 'مسار الشاطئ' where id = 3213;  -- Beach Trail
update geo.place set name_ar = 'مسار الشاطئ' where id = 3211;  -- Beach Trail
update geo.place set name_ar = 'محمد المولود ولد اعبيد' where id = 3420;  -- Mohamed Maouloud ould Abeid
update geo.place set name_ar = 'قصر الكونغرس' where id = 3470;  -- Palais des congrés
update geo.place set name_ar = 'باسكنو' where id = 3202;  -- Bassiknou
update geo.place set name_ar = 'مسار الشاطئ' where id = 3209;  -- Beach Trail
update geo.place set name_ar = 'مسار الشاطئ' where id = 3210;  -- Beach Trail
update geo.place set name_ar = 'طريق النباتات' where id = 3275;  -- Chemin de Plantes
update geo.place set name_ar = 'طريق صاعد' where id = 3276;  -- Chemin Montant
update geo.place set name_ar = 'الابتدائي' where id = 3479;  -- primaire
update geo.place set name_ar = 'حي مدينة' where id = 3482;  -- Quartier MEDINA
update geo.place set name_ar = 'سال أمادو كليدور' where id = 3975;  -- Sall Amadou Clédor
update geo.place set name_ar = 'إلى مدرسة مدينة' where id = 4025;  -- Vers école MEDINA
update geo.place set name_ar = 'مطار بودور' where id = 6097;  -- Aéroport de Podor
update geo.place set name_ar = 'مطار بئر مغرين' where id = 6501;  -- Bir Mogrein Airport
update geo.place set name_ar = 'مطار الشقة' where id = 6889;  -- Chegga Airport
update geo.place set name_ar = 'الطريق 04/22' where id = 4212;  -- 04/22
update geo.place set name_ar = 'ساحة الشحن' where id = 6269;  -- Apron Fret
update geo.place set name_ar = 'ساحة الطائرات ج' where id = 6270;  -- Apron G
update geo.place set name_ar = 'ساحة الطائرات م' where id = 6271;  -- Apron M
update geo.place set name_ar = 'عرفات بوتو 6' where id = 6274;  -- Arafatt poto 6
update geo.place set name_ar = 'موقف توجنين' where id = 6279;  -- Arret Toujounine
update geo.place set name_ar = 'الوصول البصرة' where id = 6281;  -- Arrivage Basra
update geo.place set name_ar = 'بانا الأبيض' where id = 6391;  -- Bana Blanc
update geo.place set name_ar = 'العاصمة' where id = 6732;  -- Capital
update geo.place set name_ar = 'المفترق 24' where id = 6739;  -- Carrefour 24
update geo.place set name_ar = 'المفترق أدرر' where id = 6740;  -- Carrefour Adrar
update geo.place set name_ar = 'المفترق بي إم دي' where id = 6743;  -- Carrefour BMD
update geo.place set name_ar = 'المفترق لمسيد النور' where id = 6750;  -- Carrefour Msid Nour
update geo.place set name_ar = 'المفترق ولد بادو' where id = 6752;  -- Carrefour Ould Badou
update geo.place set name_ar = 'المفترق الصباح' where id = 6755;  -- Carrefour Sabah
update geo.place set name_ar = 'المفترق تين اسويلم' where id = 6756;  -- Carrefour Tin Sweilim
update geo.place set name_ar = 'مركز الإرسال' where id = 6860;  -- Centre émetteur
update geo.place set name_ar = 'مدينة الشاطئ' where id = 7037;  -- Cité Plage
update geo.place set name_ar = 'الساحة التجارية' where id = 7109;  -- Commercial Apron
update geo.place set name_ar = 'دار النعيم' where id = 7235;  -- dar Naim
update geo.place set name_ar = 'الأمانة' where id = 7532;  -- El Amana
update geo.place set name_ar = 'السماحة' where id = 7667;  -- Essamaha
update geo.place set name_ar = 'محطة الركاب افديرك' where id = 7803;  -- Gare de passagers Fdérick
update geo.place set name_ar = 'منطقة معزولة' where id = 8083;  -- İsolated Area
update geo.place set name_ar = 'لغريكه' where id = 8211;  -- Leghreyga
update geo.place set name_ar = 'لمسيد الأحمر' where id = 8224;  -- Lemsid Lehmar
update geo.place set name_ar = 'السوق السادس' where id = 8443;  -- Marché 6ème
update geo.place set name_ar = 'الملاح' where id = 8633;  -- Mellah
update geo.place set name_ar = 'مستشفى العيون' where id = 8966;  -- Ophthalmological Hospital
update geo.place set name_ar = 'شاطئ الصيادين' where id = 9219;  -- Plage des pecheurs
update geo.place set name_ar = 'المحطة الطرقية روصو السنغال' where id = 9502;  -- Rosso Sénégal, Gare routiére
update geo.place set name_ar = 'سالدي' where id = 9534;  -- Saldé
update geo.place set name_ar = 'الشركة الوطنية للماء' where id = 9619;  -- SNDE
update geo.place set name_ar = 'الترحيل' where id = 9769;  -- Tarhil
update geo.place set name_ar = 'الطواف' where id = 9776;  -- Tawaf
update geo.place set name_ar = 'طيبة' where id = 9778;  -- Tayba
update geo.place set name_ar = 'تفرغ زينة' where id = 9835;  -- Tevragh Zeyna
update geo.place set name_ar = 'النقل الأمين' where id = 9898;  -- Transport Emine
update geo.place set name_ar = 'ساحة كبار الشخصيات' where id = 9952;  -- VIP Apron
update geo.place set name_ar = 'وقفة توجنين' where id = 9963;  -- Waghfet Toujounine

select kind,
       count(*) filter (where name_ar is not null) as named,
       count(*) as rows
  from geo.place group by kind order by kind;
commit;
