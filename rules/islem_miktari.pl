:- module(islem_miktari, [ortalama/2, anormal_islem/2, test_islem_miktari/0]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug'). % Debug mesajları
:- use_module('../utils/alert'). % Alert mesajları

% Kullanıcının işlem miktarlarının ortalamasını hesaplama
ortalama(Kullanici, Ortalama) :-
    findall(Miktar, islem(_, Kullanici, Miktar, _, _, _, _, _, _, _, _), Islemler),
    toplam(Islemler, Toplam),
    length(Islemler, Say),
    (Say > 0 ->
        Ortalama is Toplam / Say,
        debug_message('İşlem ortalaması hesaplandı: ~w => ~w', [Kullanici, Ortalama]);
        debug_message('Kullanıcının işlemleri bulunamadı: ~w', [Kullanici]),
        fail).

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
    debug_message('Anormal işlem limiti: ~w, Girilen miktar: ~w', [Limit, Miktar]),
    (Miktar > Limit ->
        alert_message('Anormal işlem tespit edildi: Kullanıcı: ~w, Miktar: ~w', [Kullanici, Miktar]);
        debug_message('İşlem normal: Kullanıcı: ~w, Miktar: ~w', [Kullanici, Miktar])
    ).

% Test işlemleri
test_islem_miktari :-
    writeln('Test: islem_miktari kontrolü başlıyor...'),
    set_debug(true),
    forall(
        member((Kullanici, TestMiktar), [
            (kullanici1, 1200),
            (kullanici2, 8000),
            (kullanici3, 400),
            (kullanici4,1500)
        ]),
        (
            writeln('----------------------------------'),
            (ortalama(Kullanici, Ortalama) ->
                format('Kullanıcı: ~w, Ortalama: ~w~n', [Kullanici, Ortalama]);
                format('Kullanıcı: ~w için işlem bulunamadı.~n', [Kullanici])),
            (anormal_islem(Kullanici, TestMiktar) ->
                format('Kullanıcı: ~w, Miktar: ~w => Anormal işlem tespit edildi.~n', [Kullanici, TestMiktar]);
                format('Kullanıcı: ~w, Miktar: ~w => İşlem normal.~n', [Kullanici, TestMiktar]))
        )
    ),
    set_debug(false),
    writeln('----------------------------------'),
    writeln('Test tamamlandı.').
