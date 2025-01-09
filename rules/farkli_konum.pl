:- module(farkli_konum, [farkli_konum_risk/2]).
:- use_module('../data/islem_verileri').

% Kullanıcının işlemlerini listeleme: ID, Zaman, Konum, Cihaz
kullanici_islemleri(Kullanici, Islemler) :-
    findall((ID, Zaman, Konum, Cihaz), islem(ID, Kullanici, _, Zaman, Konum, Cihaz, _, _, _, _, _), Islemler),
    writeln(['[DEBUG] Kullanıcı işlemleri listelendi:', Islemler]).

% Risk skorlama için farklı konum kontrolü
farkli_konum_risk(Kullanici, ToplamRisk) :-
    kullanici_islemleri(Kullanici, Islemler),
    writeln(['[DEBUG] Farklı konum riski hesaplanıyor:', Kullanici]),
    skor_hesapla(Islemler, 0, ToplamRisk),
    writeln(['[DEBUG] Toplam risk skoru:', ToplamRisk]).

% İşlemler arasında farklı konum ve kısa süre için risk puanı hesaplama
skor_hesapla([(_, Zaman1, Konum1, _), (_, Zaman2, Konum2, _) | Kalan], MevcutRisk, ToplamRisk) :-
    (Konum1 \= Konum2, abs(Zaman2 - Zaman1) =< 10 -> % Konumlar farklı ve zaman farkı küçük mü?
        RiskArtis is 5,
        YeniRisk is MevcutRisk + RiskArtis,
        writeln(['[DEBUG] Konum farkı tespit edildi:', Konum1, '=>', Konum2, 'Yeni risk:', YeniRisk])
    ;
        YeniRisk is MevcutRisk
    ),
    skor_hesapla([(_, Zaman2, Konum2, _) | Kalan], YeniRisk, ToplamRisk). % Kalan işlemleri kontrol et
skor_hesapla([_], ToplamRisk, ToplamRisk). % Tek işlem kaldıysa risk hesaplamayı bitir
skor_hesapla([], ToplamRisk, ToplamRisk). % İşlem listesi boşsa risk hesaplamayı bitir

% Test sorgusu
% farkli_konum:farkli_konum_risk(kullanici1, Risk).
% farkli_konum:farkli_konum_risk(kullanici2, Risk).
