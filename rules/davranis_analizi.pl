:- module(davranis_analizi, [davranis_sapmasi/1]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor

% Kullanıcının işlem sürelerinin ortalamasını hesaplama
davranis_ortalama(Kullanici, Ortalama) :-
    findall(DavranisSure, islem(_, Kullanici, _, _, _, _, DavranisSure, _, _, _, _), Sureler),
    toplam(Sureler, Toplam),
    length(Sureler, Say),
    Say > 0,
    Ortalama is Toplam / Say,
    writeln(['[DEBUG] Davranış sürelerinin ortalaması:', Ortalama]). % Debugging

% Kullanıcının işlem sürelerinin standart sapmasını hesaplama
davranis_standart_sapma(Kullanici, Sapma) :-
    findall(DavranisSure, islem(_, Kullanici, _, _, _, _, DavranisSure, _, _, _, _), Sureler),
    davranis_ortalama(Kullanici, Ortalama),
    findall(SqDiff, (member(DavranisSure, Sureler), SqDiff is (DavranisSure - Ortalama) ** 2), KareFarklar),
    toplam(KareFarklar, KareToplam),
    length(Sureler, Say),
    Say > 0,
    Variance is KareToplam / Say,
    Sapma is sqrt(Variance),
    writeln(['[DEBUG] Davranış sürelerinin standart sapması:', Sapma]). % Debugging

% Toplam hesaplama
toplam([], 0). % Boş liste için toplam 0
toplam([H|T], Toplam) :-
    toplam(T, AltToplam),
    Toplam is H + AltToplam.

% Davranış süresi normalden sapıyor mu?
davranis_sapmasi(Kullanici) :-
    davranis_ortalama(Kullanici, Ortalama),
    davranis_standart_sapma(Kullanici, Sapma),
    findall((DavranisSure, Zaman), islem(_, Kullanici, _, Zaman, _, _, DavranisSure, _, _, _, _), TumIslemler),
    sort(2, @>=, TumIslemler, [(SonSure, _)|_]), % Son işlem süresini bul
    writeln(['[DEBUG] Son işlem süresi:', SonSure]),      % Debugging
    Limit is Sapma * 1,                          % Sapma katsayısını azaltıyoruz
    writeln(['[DEBUG] Sapma limiti:', Limit]),           % Debugging
    (SonSure > Ortalama + Limit ; SonSure < Ortalama - Limit ->
        writeln('[ALERT] Davranış süresi sapıyor!');
        writeln('[INFO] Davranış süresi normal.')).

% Test sorgusu
% davranis_analizi:davranis_sapmasi(kullanici1).
% davranis_analizi:davranis_sapmasi(kullanici2).