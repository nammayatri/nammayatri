-- The 93 names a human chose, 2026-09-10.
--
-- Written from the review page rather than by hand. Each row is matched by
-- geo.place.id, and the loader that produced this file asserted every one of
-- the 93 rows against the original export before writing a line: the answers
-- are keyed by position, and two rows really are called "Ambassade du Maroc,
-- E-Nord", so a row that had moved would have put an answer on the wrong place.

begin;
update geo.place set name_ar = 'عيون العتروس' where id = 184;  -- place | Aïoun El Atrouss
update geo.place set name_ar = 'بوغي' where id = 496;  -- place | Boghé
update geo.place set name_ar = 'نواديبو' where id = 2217;  -- place | Nouadhibou
update geo.place set name_ar = 'ولاته' where id = 2258;  -- place | Oualata
update geo.place set name_ar = 'روصو' where id = 2421;  -- place | Rosso
update geo.place set name_ar = 'أمورج' where id = 252;  -- place | Amourj
update geo.place set name_ar = 'بئر مغرين' where id = 482;  -- place | Bir Moghreïn
update geo.place set name_ar = 'بولنوار' where id = 569;  -- place | Boulenouar
update geo.place set name_ar = 'افديرك' where id = 1205;  -- place | F'Derick
update geo.place set name_ar = 'كرمسين' where id = 1689;  -- place | Keur Macène
update geo.place set name_ar = 'لكويرة' where id = 1788;  -- place | Lagouira
update geo.place set name_ar = 'مال' where id = 1923;  -- place | Mal
update geo.place set name_ar = 'اندياكو' where id = 2159;  -- place | N'Diago
update geo.place set name_ar = 'تفرغ زينة' where id = 2798;  -- place | Tevragh Zeina
update geo.place set name_ar = 'بيريت' where id = 479;  -- place | Birette, N'Diago
update geo.place set name_ar = 'بوتاندا' where id = 596;  -- place | Boutanda, Gouraye
update geo.place set name_ar = 'ديس 2' where id = 849;  -- place | Dios 2, N'Diago
update geo.place set name_ar = 'الحويطات' where id = 943;  -- place | El Ahouétat
update geo.place set name_ar = 'جدر المحكن' where id = 1631;  -- place | Jedrel Mohguen
update geo.place set name_ar = 'كوبانو' where id = 1718;  -- place | Kobanu
update geo.place set name_ar = 'مبويو' where id = 1984;  -- place | Mboyo, N'Diago
update geo.place set name_ar = 'مبويو 2' where id = 1986;  -- place | Mboyo 2, N'Diago
update geo.place set name_ar = 'رشيد' where id = 2392;  -- place | Rachid
update geo.place set name_ar = 'تانيت' where id = 2675;  -- place | Tânît
update geo.place set name_ar = 'تيونغ' where id = 2803;  -- place | Thiong, N'Diago
update geo.place set name_ar = 'والي' where id = 3032;  -- place | Wali, Waoundé
update geo.place set name_ar = 'الحلـــــه' where id = 1;  -- place | الحلـــــه
update geo.place set name_ar = 'بيريت بول' where id = 480;  -- place | Birette Peule, N'Diago
update geo.place set name_ar = 'لاباتري' where id = 1781;  -- place | La Batterie, Cansado
update geo.place set name_ar = 'سياسة' where id = 2550;  -- place | Siyassa
update geo.place set name_ar = 'رأس الأبيض' where id = 621;  -- place | Cap Blanc, Lagouira
update geo.place set name_ar = 'تكوين الريشات' where id = 2412;  -- place | Richat, l’œil de l’Afrique
update geo.place set name_ar = 'مستشفى لعيون' where id = 7956;  -- poi | Hôpital d’Aïoun, Aïoun El Atrouss
update geo.place set name_ar = 'مركز صحي بلنوار' where id = 9266;  -- poi | Poste de Santé Boulenouar, Sedoumiya
update geo.place set name_ar = 'جامع العتيق كانصادو' where id = 8714;  -- poi | Mosquée Al-Atîq de Cansado, Cansado
update geo.place set name_ar = 'مسجد الشيخ محمد فاضل' where id = 8761;  -- poi | Mosquée Cheikh Mohamed Fadel, Gouraye
update geo.place set name_ar = 'مسجد اندياكو' where id = 8791;  -- poi | Mosquée de N'Diago, N'Diago
update geo.place set name_ar = 'مسجد حايمه الجامع' where id = 8829;  -- poi | Mosquée Haima, Gogui
update geo.place set name_ar = 'البنك الوطني الموريتاني' where id = 8911;  -- poi | National Bank of Mauritania, Nouakchott
update geo.place set name_ar = 'ستار' where id = 9706;  -- poi | Star, Nouadhibou
update geo.place set name_ar = 'سفارة الصين' where id = 6222;  -- poi | Ambassade de Chine, Nouakchott
update geo.place set name_ar = 'سفارة فرنسا' where id = 6223;  -- poi | Ambassade de France, Nouakchott
update geo.place set name_ar = 'سفارة ليبيا' where id = 6224;  -- poi | Ambassade de Libye, E-Nord
update geo.place set name_ar = 'سفارة إيران' where id = 6225;  -- poi | Ambassade de l'Iran, Las Palmas
update geo.place set name_ar = 'سفارة الإمارات العربية المتحدة' where id = 6227;  -- poi | Ambassade des Émirats arabes unis, E-Nord
update geo.place set name_ar = 'سفارة مالي' where id = 6234;  -- poi | Ambassade du Mali, E-Nord
update geo.place set name_ar = 'سفارة المملكة المغربية' where id = 6235;  -- poi | Ambassade du Maroc, E-Nord
update geo.place set name_ar = 'سفارة المغرب' where id = 6236;  -- poi | Ambassade du Maroc, E-Nord
update geo.place set name_ar = 'بايو' where id = 6441;  -- poi | Bayo, N'Diago
update geo.place set name_ar = 'بير النار' where id = 6502;  -- poi | Bir Nar, Ouissiat
update geo.place set name_ar = 'ثكنة عسكرية' where id = 6793;  -- poi | Caserne militaire, Bir Moghreïn
update geo.place set name_ar = 'كوندور' where id = 7157;  -- poi | Condor, E-Nord
update geo.place set name_ar = 'قنصلية بلجيكا' where id = 7160;  -- poi | Consulat de Belgique, Nouakchott
update geo.place set name_ar = 'قنصلية كوت ديفوار' where id = 7161;  -- poi | Consulat de Côte d'Ivoire, Nouakchott
update geo.place set name_ar = 'قنصلية هولندا' where id = 7162;  -- poi | Consulat de Pays-Bas, Nouakchott
update geo.place set name_ar = 'قنصلية السويد' where id = 7163;  -- poi | Consulat de Suède, Nouakchott
update geo.place set name_ar = 'القنصلية الفخرية لغينيا بيساو' where id = 7165;  -- poi | Consulat honoraire de Guinée Bissau, Nouadhibou
update geo.place set name_ar = 'قنصلية جمهورية بنين' where id = 7166;  -- poi | Consulat honoraire du Bénin, Nouakchott
update geo.place set name_ar = 'جدرالمحكن' where id = 8104;  -- poi | Jedrel Mouhguen, Jidr El Mohguen
update geo.place set name_ar = 'ميشلان' where id = 8610;  -- poi | MCR sarl Michelin, Nouadhibou
update geo.place set name_ar = 'منطقة الكويرة المغربية' where id = 9372;  -- poi | Région de Lagouira au Maroc, Lagouira
update geo.place set name_ar = 'جي 5 الساحل' where id = 9573;  -- poi | Secrétariat permanent du G5 Sahel, E-Nord
update geo.place set name_ar = 'تيونغ' where id = 9855;  -- poi | Tiong, N'Diago
update geo.place set name_ar = 'انواشيد' where id = 4062;  -- poi | انواشيد, Nouachid
update geo.place set name_ar = 'جالة اهل عبد الرحمن ولد لمام' where id = 4092;  -- poi | جالة اهل عبد الرحمن ولد لمام, Lembeïdî‘a
update geo.place set name_ar = 'لبن الإبل' where id = 4066;  -- poi | لبن الإبل, Tafolli
update geo.place set name_ar = 'طريق روصو السريع' where id = 3155;  -- street | Autoroute Rosso, Araffat
update geo.place set name_ar = 'طريق روصو السريع' where id = 3157;  -- street | Autoroute Rosso, El Mina
update geo.place set name_ar = 'طريق روصو السريع' where id = 3158;  -- street | Autoroute Rosso, Riyadh
update geo.place set name_ar = 'طريق روصو السريع' where id = 3156;  -- street | Autoroute Rosso, Carrefour
update geo.place set name_ar = 'كونديوُرلا - فصالة' where id = 3365;  -- street | Koundiourla - Fassala, Fassale
update geo.place set name_ar = 'طريق شوم' where id = 3501;  -- street | Route Choum, Atar
update geo.place set name_ar = 'طريق اوجفت' where id = 3514;  -- street | Route d'Aoujeft, Terjit
update geo.place set name_ar = 'طريق الأمل' where id = 3563;  -- street | Route de l’Espoir, Etweymiret
update geo.place set name_ar = 'طريق نواكشوط' where id = 3601;  -- street | Route de Nouakchott, Ḍâyet Ould Zeïdâne
update geo.place set name_ar = 'طريق روصو - بوغي' where id = 3633;  -- street | Route Rosso - Boghé, Rosso
update geo.place set name_ar = 'شارع غاري' where id = 3819;  -- street | Rue Ghary, Ksar
update geo.place set name_ar = 'الطريق الوطني القديم 1' where id = 3146;  -- street | Ancienne Route Nationale 1, Choûm
update geo.place set name_ar = 'شارع القدس' where id = 3163;  -- street | Avenue Al Quds, Ksar
update geo.place set name_ar = 'شارع الملك فيصل' where id = 3174;  -- street | Avenue du Roi Fayçal, Nouakchott
update geo.place set name_ar = 'شارع الملك فيصل' where id = 3173;  -- street | Avenue du Roi Fayçal, Ksar
update geo.place set name_ar = 'شارع أبو بكر الصديق' where id = 3652;  -- street | Rue Aboubekr Seddigh, Las Palmas
update geo.place set name_ar = 'شارع دبي' where id = 3737;  -- street | Rue de Dubaï, Nouadhibou
update geo.place set name_ar = 'شارع المدرسة الثانوية' where id = 3793;  -- street | Rue du Lycée, Atar
update geo.place set name_ar = 'طريق عدل بكرو - نارا' where id = 3099;  -- street | طريق عدل بكرو - نارا, Adel Bagrou
update geo.place set name_ar = 'عبد العزيز سي' where id = 3107;  -- street | Abdel Aziz Sy, Nouakchott
update geo.place set name_ar = 'بكار ولد سويد أحمد' where id = 3193;  -- street | Bakar Ould Soueid Ahmad, Nouakchott
update geo.place set name_ar = 'بئر مغرين' where id = 3220;  -- street | Bir Moghrein, Ksar
update geo.place set name_ar = 'داودا كيرابا دياوارا' where id = 3292;  -- street | Daouda Keiraba Diawara, Nouakchott
update geo.place set name_ar = 'جدو ولد سالك' where id = 3357;  -- street | Jiddou Ould Saleck, Nouakchott
update geo.place set name_ar = 'كمال ولد بهاء' where id = 3362;  -- street | Kemal Ould Baha, Nouakchott
update geo.place set name_ar = 'شارع محطة الحافلات' where id = 3742;  -- street | Rue de la Gare Routière, Atar
update geo.place set name_ar = 'مطار عيون العتروس' where id = 6090;  -- transport | Aéroport d'Aioun el Atrouss, Aïoun El Atrouss

select count(*) filter (where name_ar is not null) as filled,
       count(*) filter (where name_ar is not null and kind = 'street') as streets
  from geo.place;
commit;
