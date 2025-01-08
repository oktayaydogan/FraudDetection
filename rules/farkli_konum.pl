:- module(farkli_konum, [farkli_konum_risk/2]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor

% Kullanıcının işlemlerini listeleme: Miktar, Zaman, Konum, Cihaz
kullanici_islemleri(Kullanici, Islemler) :-
    findall((Miktar, Zaman, Konum, Cihaz), islem(Kullanici, Miktar, Zaman, Konum, Cihaz, _, _), Islemler),
    writeln(['Kullanıcı işlemleri listelendi:', Islemler]). % Debugging mesajı

% Risk skorlama için farklı konum kontrolü
farkli_konum_risk(Kullanici, ToplamRisk) :-
    kullanici_islemleri(Kullanici, Islemler),
    writeln(['Farklı konum riski hesaplanıyor:', Kullanici]),
    skor_hesapla(Islemler, 0, ToplamRisk),
    writeln(['Toplam risk skoru:', ToplamRisk]). % Debugging mesajı

% İşlemler arasında farklı konum ve kısa süre için risk puanı hesaplama
skor_hesapla([(_, Zaman1, Konum1, _), (_, Zaman2, Konum2, _) | Kalan], MevcutRisk, ToplamRisk) :-
    (Konum1 \= Konum2, abs(Zaman2 - Zaman1) =< 10 -> % Konumlar farklı ve zaman farkı küçük mü?
        RiskArtis is 5,
        YeniRisk is MevcutRisk + RiskArtis,
        writeln(['Konum farkı tespit edildi:', Konum1, '=>', Konum2, 'Yeni risk:', YeniRisk])
    ;
        YeniRisk is MevcutRisk
    ),
    skor_hesapla([(_, Zaman2, Konum2, _) | Kalan], YeniRisk, ToplamRisk). % Kalan işlemleri kontrol et
skor_hesapla([_], ToplamRisk, ToplamRisk). % Tek işlem kaldıysa risk hesaplamayı bitir
skor_hesapla([], ToplamRisk, ToplamRisk). % İşlem listesi boşsa risk hesaplamayı bitir
