:- module(islem_miktari, [ortalama/2, anormal_islem/2]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor

% Kullanıcının işlem ortalamasını hesaplama
ortalama(Kullanici, Ortalama) :-
    findall(Miktar, islem(Kullanici, Miktar, _, _, _), Islemler),
    toplam(Islemler, Toplam),
    length(Islemler, Say),
    Say > 0,
    Ortalama is Toplam / Say.

% Toplam işlemleri hesaplama
toplam([], 0).
toplam([H|T], Toplam) :-
    toplam(T, AltToplam),
    Toplam is H + AltToplam.

% Anormal işlem tespiti
anormal_islem(Kullanici, Miktar) :-
    ortalama(Kullanici, Ortalama),
    Katsayi is 3,
    Limit is Ortalama * Katsayi,
    Miktar > Limit.
