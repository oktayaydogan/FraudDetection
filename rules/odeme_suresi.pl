:- module(odeme_suresi, [odeme_suresi_sapmasi/1]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor

% Kullanıcının ödeme sürelerinin ortalamasını hesaplama
ortalama_odeme_suresi(Kullanici, Ortalama) :-
    findall(DavranisSure, islem(_, Kullanici, _, _, _, _, DavranisSure, _, _, _, _), Sureler),
    toplam(Sureler, Toplam),
    length(Sureler, Say),
    Say > 0,
    Ortalama is Toplam / Say,
    writeln(['[DEBUG] Ortalama ödeme süresi:', Kullanici, '=>', Ortalama]).

% Kullanıcının son ödeme süresinin normalden sapıp sapmadığını kontrol et
odeme_suresi_sapmasi(Kullanici) :-
    findall(DavranisSure, islem(_, Kullanici, _, _, _, _, DavranisSure, _, _, _, _), Sureler),
    writeln(['[DEBUG] Kullanıcının ödeme süreleri:', Sureler]),
    ortalama_odeme_suresi(Kullanici, Ortalama),
    son_odeme_suresi(Sureler, SonSure),
    writeln(['[DEBUG] Son ödeme süresi:', SonSure]),
    Limit is Ortalama * 1.5, % %50 sapma limiti
    writeln(['[DEBUG] Sapma limiti:', Limit]),
    (SonSure > Ortalama + Limit ; SonSure < Ortalama - Limit ->
        writeln('[ALERT] Ödeme süresi sapması tespit edildi!');
        writeln('[INFO] Ödeme süresi normal.')).

% Son ödeme süresini bulma
son_odeme_suresi(Sureler, SonSure) :-
    last(Sureler, SonSure).

% Toplam hesaplama
toplam([], 0).
toplam([H|T], Toplam) :-
    toplam(T, AltToplam),
    Toplam is H + AltToplam.

% Test sorgusu
% odeme_suresi:ortalama_odeme_suresi(kullanici1, Ortalama).
% odeme_suresi:ortalama_odeme_suresi(kullanici2, Ortalama).
% odeme_suresi:odeme_suresi_sapmasi(kullanici1).
% odeme_suresi:odeme_suresi_sapmasi(kullanici5)%