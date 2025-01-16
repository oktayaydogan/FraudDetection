:- module(farkli_konum, [farkli_konum_risk/2, test_farkli_konum/0]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug'). % Debug mesajları
:- use_module('../utils/alert'). % Uyarı mesajları

% Kullanıcının işlemlerini listeleme: ID, Zaman, Konum, Cihaz
kullanici_islemleri(Kullanici, Islemler) :-
    findall((ID, Zaman, Konum, Cihaz), islem(ID, Kullanici, _, Zaman, Konum, Cihaz, _, _, _, _, _), Islemler),
    debug_message('Kullanıcı işlemleri listelendi: ~w', [Islemler]).

% Risk skorlama için farklı konum kontrolü
farkli_konum_risk(Kullanici, ToplamRisk) :-
    kullanici_islemleri(Kullanici, Islemler),
    debug_message('Farklı konum riski hesaplanıyor: ~w', [Kullanici]),
    skor_hesapla(Islemler, 0, ToplamRisk),
    debug_message('Toplam risk skoru: ~w', [ToplamRisk]).

% İşlemler arasında farklı konum ve kısa süre için risk puanı hesaplama
skor_hesapla([(_, Zaman1, Konum1, _), (_, Zaman2, Konum2, _) | Kalan], MevcutRisk, ToplamRisk) :-
    (Konum1 \= Konum2, abs(Zaman2 - Zaman1) =< 10 -> % Konumlar farklı ve zaman farkı küçük mü?
        RiskArtis is 5,
        YeniRisk is MevcutRisk + RiskArtis,
        debug_message('Konum farkı tespit edildi: ~w => ~w, Yeni risk: ~w', [Konum1, Konum2, YeniRisk])
    ;
        YeniRisk is MevcutRisk
    ),
    skor_hesapla([(_, Zaman2, Konum2, _) | Kalan], YeniRisk, ToplamRisk). % Kalan işlemleri kontrol et
skor_hesapla([_], ToplamRisk, ToplamRisk). % Tek işlem kaldıysa risk hesaplamayı bitir
skor_hesapla([], ToplamRisk, ToplamRisk). % İşlem listesi boşsa risk hesaplamayı bitir

% Test farklı konum riski
test_farkli_konum :-
    writeln('Test: farkli_konum_risk kontrolü başlıyor...'),
    set_debug(true),
    forall(member(Kullanici, [kullanici1, kullanici2, kullanici3, kullanici4]),
           (writeln('----------------------------------'),
            (farkli_konum_risk(Kullanici, Risk) ->
                format('Kullanıcı: ~w, Risk: ~w~n', [Kullanici, Risk]);
                format('Kullanıcı: ~w, Risk bulunamadı.~n', [Kullanici])))),
    set_debug(false),
    writeln('----------------------------------'),
    writeln('Test tamamlandı.').