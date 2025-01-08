:- module(farkli_konum, [farkli_konum_kontrol/1]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor

% Kullanıcının işlemlerini listeleme: Miktar, Zaman ve Konum
kullanici_islemleri(Kullanici, Islemler) :-
    findall((Miktar, Zaman, Konum), islem(Kullanici, Miktar, Zaman, Konum), Islemler).

% Farklı konumda kısa süreli işlem kontrolü
farkli_konum_kontrol(Kullanici) :-
    kullanici_islemleri(Kullanici, Islemler),
    kontrol_farkli_konum(Islemler).

% İşlemler arasında farklı konum ve kısa süre kontrolü
kontrol_farkli_konum([(_, Zaman1, Konum1), (_, Zaman2, Konum2) | Kalan]) :-
    Konum1 \= Konum2,       % Konumlar farklı mı?
    ZamanFarki is abs(Zaman2 - Zaman1),
    ZamanFarki =< 10,       % Zaman farkı 10 birimden az mı?
    !;                      % Şüpheli bir durum bulunduysa dur
    kontrol_farkli_konum([(_, Zaman2, Konum2) | Kalan]).
kontrol_farkli_konum([]). % Liste boşsa işlem yapılmaz
kontrol_farkli_konum([_]). % Tek işlem varsa kontrol yapılmaz
