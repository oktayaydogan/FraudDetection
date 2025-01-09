:- use_module('data/islem_verileri'). % Veriler
:- use_module('rules/islem_sikligi'). % İşlem sıklığı kuralları
:- use_module('rules/islem_miktari'). % İşlem miktarı kuralları
:- use_module('rules/islem_konumu'). % İşlem konumu kuralları
:- use_module('rules/farkli_konum'). % Farklı konum kuralları
:- use_module('rules/yeni_cihaz'). % Yeni cihaz kuralları
:- use_module('rules/davranis_analizi'). % Kullanıcı davranış analizi
:- use_module('rules/ortak_ip_kullanimi'). % Aynı IP adresinden farklı kullanıcı kontrolü
:- use_module('rules/odeme_suresi'). % Ödeme süresi analizi
:- use_module('rules/yeni_odeme_yontemi'). % Yeni ödeme yöntemi kontrolü
:- use_module('rules/odeme_yontemi_riski'). % Ödeme yöntemi riski kontrolü
:- use_module('rules/tekrarli_bilgi_kontrolu'). % Tekrarlı bilgi kontrolü
:- use_module('rules/para_iade_kontrolu'). % Para iade kontrolü
:- use_module('rules/oturum_bilgisi_degisim_kontrolu'). % Oturum bilgisi değişikliği kontrolü

% İşlem risk skoru hesaplama
risk_skoru_islem(IslemId, Risk) :-
    islem(IslemId, Kullanici, Miktar, _, _, _, _, _, _, _, _),
    (islem_miktari:anormal_islem(Kullanici, Miktar) -> P1 = 6; P1 = 0),
    (islem_konumu:konum_uyusmazligi(Kullanici) -> P2 = 3; P2 = 0),
    (yeni_cihaz:yeni_cihaz_tespiti(Kullanici) -> P3 = 4; P3 = 0),
    (islem_sikligi:supheli_islem(Kullanici, 0, 24) -> P4 = 5; P4 = 0),
    (yeni_odeme_yontemi:yeni_odeme_yontemi(Kullanici) -> P5 = 2; P5 = 0),
    (para_iade_kontrolu:para_iade_riski(Kullanici) -> P6 = 3; P6 = 0),
    Risk is P1 + P2 + P3 + P4 + P5 + P6.

% Kullanıcı risk skoru hesaplama
risk_skoru_kullanici(Kullanici, ToplamRisk) :-
    findall(IslemId, islem(IslemId, Kullanici, _, _, _, _, _, _, _, _, _), IslemList),
    findall(Risk, (member(IslemId, IslemList), risk_skoru_islem(IslemId, Risk)), RiskList),
    sum_list(RiskList, ToplamRisk).

% Kullanıcı sorgulama
sorgula(Kullanici) :-
    format('~nKullanıcı: ~w~n', [Kullanici]),
    risk_skoru_kullanici(Kullanici, ToplamRisk),
    format('Toplam Risk Skoru: ~w~n', [ToplamRisk]),
    (ToplamRisk > 50 -> writeln('-> Yüksek Risk: İşlem incelemeye alınmalı.');
    writeln('-> Düşük Risk: İşlem normal.')),
    writeln('-----------------------------------').

% Tüm kullanıcıları sorgula
tüm_kullanicilari_sorgula :-
    findall(Kullanici, islem(_, Kullanici, _, _, _, _, _, _, _, _, _), KullaniciListesi),
    list_to_set(KullaniciListesi, UnikKullanicilar),
    forall(member(Kullanici, UnikKullanicilar), sorgula(Kullanici)).

% İşlem ID'ye göre sorgulama
islem_id_sorgula(IslemId) :-
    islem(IslemId, Kullanici, Miktar, Zaman, Konum, Cihaz, DavranisSure, IslemTuru, IP, OdemeYontemi, Ekstra),
    format('~nİşlem ID: ~w~n', [IslemId]),
    format('Kullanıcı: ~w~n', [Kullanici]),
    format('Miktar: ~w, Zaman: ~w, Konum: ~w, Cihaz: ~w~n', [Miktar, Zaman, Konum, Cihaz]),
    format('Davranış Süresi: ~w, İşlem Türü: ~w, IP: ~w, Ödeme Yöntemi: ~w, Ekstra: ~w~n', 
           [DavranisSure, IslemTuru, IP, OdemeYontemi, Ekstra]),
    writeln('-----------------------------------'),
    risk_skoru_islem(IslemId, Risk),
    format('İşlem Risk Skoru: ~w~n', [Risk]),
    writeln('-----------------------------------').

% Test sorguları
% sorgula(kullanici1).
% sorgula(kullanici2).
% islem_id_sorgula(1).
% islem_id_sorgula(6).
% tüm_kullanicilari_sorgula.
