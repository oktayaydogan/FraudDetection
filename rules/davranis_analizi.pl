:- module(davranis_analizi, [davranis_sapmasi/1]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor

% Kullanıcının davranış sürelerinin ortalamasını hesaplama
davranis_ortalama(Kullanici, Ortalama) :-
    findall(Sure, islem(Kullanici, _, _, _, _, Sure), Sureler),
    toplam(Sureler, Toplam),
    length(Sureler, Say),
    Say > 0,
    Ortalama is Toplam / Say,
    writeln(['Ortalama süre:', Ortalama]). % Debugging

% Kullanıcının davranış sürelerinin standart sapmasını hesaplama
davranis_standart_sapma(Kullanici, Sapma) :-
    findall(Sure, islem(Kullanici, _, _, _, _, Sure), Sureler),
    davranis_ortalama(Kullanici, Ortalama),
    findall(SqDiff, (member(Sure, Sureler), SqDiff is (Sure - Ortalama) ** 2), KareFarklar),
    toplam(KareFarklar, KareToplam),
    length(Sureler, Say),
    Say > 0,
    Variance is KareToplam / Say,
    Sapma is sqrt(Variance),
    writeln(['Standart sapma:', Sapma]). % Debugging

% Toplam hesaplama
toplam([], 0). % Boş liste için toplam 0
toplam([H|T], Toplam) :-
    toplam(T, AltToplam),
    Toplam is H + AltToplam.

% Davranış süresi normalden sapıyor mu?
davranis_sapmasi(Kullanici) :-
    davranis_ortalama(Kullanici, Ortalama),
    davranis_standart_sapma(Kullanici, Sapma),
    findall((Sure, Zaman), islem(Kullanici, _, Zaman, _, _, Sure), TumIslemler),
    sort(2, @>=, TumIslemler, [(SonSure, _)|_]), % Son işlem süresini bul
    Limit is Sapma * 1,                          % Sapma katsayısını azaltıyoruz
    writeln(['Sapma limiti:', Limit]),           % Debugging
    (SonSure > Ortalama + Limit ; SonSure < Ortalama - Limit),
    writeln('Davranış süresi sapıyor!').
