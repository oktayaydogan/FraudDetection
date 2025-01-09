:- module(islem_miktari, [ortalama/2, anormal_islem/2]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor

% Kullanıcının işlem miktarlarının ortalamasını hesaplama
ortalama(Kullanici, Ortalama) :-
    findall(Miktar, islem(_, Kullanici, Miktar, _, _, _, _, _, _, _, _), Islemler),
    toplam(Islemler, Toplam),
    length(Islemler, Say),
    Say > 0,
    Ortalama is Toplam / Say,
    writeln(['[DEBUG] İşlem ortalaması hesaplandı:', Kullanici, '=>', Ortalama]). % Debugging

% Toplam işlemleri hesaplama
toplam([], 0).
toplam([H|T], Toplam) :-
    toplam(T, AltToplam),
    Toplam is H + AltToplam.

% Anormal işlem tespiti
anormal_islem(Kullanici, Miktar) :-
    ortalama(Kullanici, Ortalama),
    Katsayi is 3, % Ortalamanın kaç katına kadar izin verileceğini belirler
    Limit is Ortalama * Katsayi,
    writeln(['[DEBUG] Anormal işlem limiti:', Limit, 'Girilen miktar:', Miktar]), % Debugging
    Miktar > Limit,
    writeln('[ALERT] Anormal işlem tespit edildi!').

% Test sorgusu
% islem_miktari:ortalama(kullanici1, Ortalama).
% islem_miktari:ortalama(kullanici2, Ortalama).
% islem_miktari:anormal_islem(kullanici1, 1200). % Anormal mi?
% islem_miktari:anormal_islem(kullanici2, 8000). % Anormal mi?
