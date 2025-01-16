:- module(odeme_suresi, [odeme_suresi_sapmasi/1]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug'). % Debug mesajları
:- use_module('../utils/alert'). % Alert mesajları

% Kullanıcının ödeme sürelerinin ortalamasını hesaplama
ortalama_odeme_suresi(Kullanici, Ortalama) :-
    findall(DavranisSure, islem(_, Kullanici, _, _, _, _, DavranisSure, _, _, _, _), Sureler),
    toplam(Sureler, Toplam),
    length(Sureler, Say),
    Say > 0,
    Ortalama is Toplam / Say,
    debug_message('Ortalama ödeme süresi: ~w => ~w', [Kullanici, Ortalama]).

% Kullanıcının son ödeme süresinin normalden sapıp sapmadığını kontrol et
odeme_suresi_sapmasi(Kullanici) :-
    findall(DavranisSure, islem(_, Kullanici, _, _, _, _, DavranisSure, _, _, _, _), Sureler),
    debug_message('Kullanıcının ödeme süreleri: ~w', [Sureler]),
    ortalama_odeme_suresi(Kullanici, Ortalama),
    son_odeme_suresi(Sureler, SonSure),
    debug_message('Son ödeme süresi: ~w', [SonSure]),
    Limit is Ortalama * 1.5, % %50 sapma limiti
    debug_message('Sapma limiti: ~w', [Limit]),
    (SonSure > Ortalama + Limit ; SonSure < Ortalama - Limit ->
        alert_message('Ödeme süresi sapması tespit edildi: Kullanıcı: ~w, Son Süre: ~w, Limit: ~w', [Kullanici, SonSure, Limit]);
        debug_message('Ödeme süresi normal: Kullanıcı: ~w, Son Süre: ~w', [Kullanici, SonSure])
    ).

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
% odeme_suresi:odeme_suresi_sapmasi(kullanici5).
