:- module(davranis_analizi, [davranis_sapmasi/1, test_davranis_analizi/0]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug'). % Debug mesajları
:- use_module('../utils/alert'). % Uyarı mesajları

% Kullanıcının işlem sürelerinin ortalamasını hesaplama
davranis_ortalama(Kullanici, Ortalama) :-
    findall(DavranisSure, islem(_, Kullanici, _, _, _, _, DavranisSure, _, _, _, _), Sureler),
    toplam(Sureler, Toplam),
    length(Sureler, Say),
    (Say > 0 ->
        Ortalama is Toplam / Say,
        debug_message('Davranış sürelerinin ortalaması: ~w', [Ortalama]);
        debug_message('Kullanıcının işlem süreleri bulunamadı: ~w', [Kullanici]),
        fail).

% Kullanıcının işlem sürelerinin standart sapmasını hesaplama
davranis_standart_sapma(Kullanici, Sapma) :-
    findall(DavranisSure, islem(_, Kullanici, _, _, _, _, DavranisSure, _, _, _, _), Sureler),
    davranis_ortalama(Kullanici, Ortalama),
    findall(SqDiff, (member(DavranisSure, Sureler), SqDiff is (DavranisSure - Ortalama) ** 2), KareFarklar),
    toplam(KareFarklar, KareToplam),
    length(Sureler, Say),
    (Say > 0 ->
        Variance is KareToplam / Say,
        Sapma is sqrt(Variance),
        debug_message('Davranış sürelerinin standart sapması: ~w', [Sapma]);
        debug_message('Kullanıcının işlem süreleri standart sapması hesaplanamadı: ~w', [Kullanici]),
        fail).

% Toplam hesaplama
toplam([], 0). % Boş liste için toplam 0
toplam([H|T], Toplam) :-
    toplam(T, AltToplam),
    Toplam is H + AltToplam.

% Davranış süresi normalden sapıyor mu?
davranis_sapmasi(Kullanici) :-
    (var(Kullanici) -> alert_message('Hata: Kullanıcı belirtilmemiş.'); true),
    davranis_ortalama(Kullanici, Ortalama),
    davranis_standart_sapma(Kullanici, Sapma),
    findall((DavranisSure, Zaman), islem(_, Kullanici, _, Zaman, _, _, DavranisSure, _, _, _, _), TumIslemler),
    (TumIslemler \= [] ->
        sort(2, @>=, TumIslemler, [(SonSure, _)|_]), % Son işlem süresini bul
        debug_message('Son işlem süresi: ~w', [SonSure]),
        Katsayi = 1.5, % Dinamik olarak sapma katsayısını ayarlayın
        Limit is Sapma * Katsayi,
        debug_message('Sapma limiti: ~w', [Limit]),
        (SonSure > Ortalama + Limit ; SonSure < Ortalama - Limit ->
            alert_message('Davranış süresi sapıyor!'),
            true;
            debug_message('Davranış süresi normal.'),
            fail);
        debug_message('Kullanıcının işlem süreleri bulunamadı: ~w', [Kullanici]),
        fail).

% Test davranış sapması
test_davranis_analizi :-
    writeln('Test: davranis_sapmasi kontrolü başlıyor...'),
    set_debug(true),
    forall(member(Kullanici, [kullanici1, kullanici2, kullanici3, kullanici4]),
           (writeln('----------------------------------'),
            (davranis_sapmasi(Kullanici) ->
                format('Kullanıcı: ~w, Sonuç: Davranış süresi sapıyor.~n', [Kullanici]);
                format('Kullanıcı: ~w, Sonuç: Davranış süresi normal.~n', [Kullanici])))),
    set_debug(false),
    writeln('----------------------------------'),
    writeln('Test tamamlandı.').
