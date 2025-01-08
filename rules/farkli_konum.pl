:- module(farkli_konum, [farkli_konum_risk/2]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor

% Kullanıcının işlemlerini listeleme: Miktar, Zaman, Konum, Cihaz
kullanici_islemleri(Kullanici, Islemler) :-
    findall((Miktar, Zaman, Konum, Cihaz), islem(Kullanici, Miktar, Zaman, Konum, Cihaz, _), Islemler).

% Risk skorlama için farklı konum kontrolü
farkli_konum_risk(Kullanici, ToplamRisk) :-
    kullanici_islemleri(Kullanici, Islemler),
    skor_hesapla(Islemler, 0, ToplamRisk).

% İşlemler arasında farklı konum ve kısa süre için risk puanı hesaplama
skor_hesapla([(_, Zaman1, Konum1, _), (_, Zaman2, Konum2, _) | Kalan], MevcutRisk, ToplamRisk) :-
    Konum1 \= Konum2,                          % Konumlar farklı mı?
    ZamanFarki is abs(Zaman2 - Zaman1),
    ZamanFarki =< 10,                          % Zaman farkı 10 birimden az mı?
    RiskArtis is 5,                            % Bu durum için risk puanı
    YeniRisk is MevcutRisk + RiskArtis,        % Mevcut riske ekle
    skor_hesapla([(_, Zaman2, Konum2, _) | Kalan], YeniRisk, ToplamRisk).
skor_hesapla([(_, _, _, _), (_, _, _, _) | Kalan], MevcutRisk, ToplamRisk) :-
    skor_hesapla(Kalan, MevcutRisk, ToplamRisk).
skor_hesapla([], ToplamRisk, ToplamRisk).
skor_hesapla([_], ToplamRisk, ToplamRisk).
