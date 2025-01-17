:- module(odeme_suresi, [odeme_suresi_sapmasi/1, ortalama_odeme_suresi/2, test_odeme_suresi/0]).

:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug').         % Debug mesajları
:- use_module('../utils/alert').         % Alert mesajları

% ----------------------------------------------------------------------
% 1) Kullanıcının ortalama ödeme süresini hesaplama
% ----------------------------------------------------------------------
ortalama_odeme_suresi(Kullanici, Ortalama) :-
    findall(DavranisSure,
            islem(_, Kullanici, _, _, _, _, DavranisSure, _, _, _, _),
            Sureler),
    toplam(Sureler, Toplam),
    length(Sureler, Say),
    Say > 0,  % Eğer hiç işlem yoksa false döner, ortalama hesaplanamaz
    Ortalama is Toplam / Say,
    debug_message('Ortalama ödeme süresi: ~w => ~2f', [Kullanici, Ortalama]).

% ----------------------------------------------------------------------
% 2) Kullanıcının son ödeme süresinin ortalamadan sapıp sapmadığını kontrol et
% ----------------------------------------------------------------------
odeme_suresi_sapmasi(Kullanici) :-
    findall(DavranisSure,
            islem(_, Kullanici, _, _, _, _, DavranisSure, _, _, _, _),
            Sureler),
    % Eğer hiç işlem yoksa sapma kontrolü yapamayız; başarısız olur.
    Sureler \= [],
    debug_message('Kullanıcının ödeme süreleri: ~w', [Sureler]),

    ortalama_odeme_suresi(Kullanici, Ortalama),
    son_odeme_suresi(Sureler, SonSure),

    debug_message('Son ödeme süresi: ~w', [SonSure]),
    Limit is Ortalama * 1.5,  % %50 sapma limiti
    debug_message('Sapma limiti: ~2f (Ortalama ± ~2f)', [Limit, Limit]),

    (   SonSure > Ortalama + Limit
     ;  SonSure < Ortalama - Limit
    ->  alert_message(
            'Ödeme süresi SAPMASI: Kullanıcı: ~w, Son Süre: ~w, Limit: ~2f (Ortalama ~2f)',
            [Kullanici, SonSure, Limit, Ortalama]
        )
    ;   debug_message(
            'Ödeme süresi normal: Kullanıcı: ~w, Son Süre: ~w (Ortalama ~2f)',
            [Kullanici, SonSure, Ortalama]
        ),
        fail  % Bu satır, "sapma yoksa" kuralı başarısız olsun ki "odeme_suresi_sapmasi/1" false dönsün
    ).

% ----------------------------------------------------------------------
% Son ödeme süresini (listede en son eklenen işlem süresi) bulma
% ----------------------------------------------------------------------
son_odeme_suresi(Sureler, SonSure) :-
    last(Sureler, SonSure).

% ----------------------------------------------------------------------
% Toplam hesaplama
% ----------------------------------------------------------------------
toplam([], 0).
toplam([H|T], Toplam) :-
    toplam(T, AltToplam),
    Toplam is H + AltToplam.

% ----------------------------------------------------------------------
% 3) Test Sorgusu
%    Bu sorgu, belirli kullanıcılar üzerinde ortalama ödeme süresi
%    ve sapma kontrolünü otomatik şekilde yapar.
% ----------------------------------------------------------------------
test_odeme_suresi :-
    writeln('--- [TEST] Odeme Süresi Kontrolü Başlıyor... ---'),
    set_debug(true),

    % Burada test etmek istediğiniz kullanıcıları belirtiyoruz.
    % İsterseniz kullanıcı10, kullanıcıX vs. ekleyebilirsiniz.
    forall(
        member(Kullanici,
               [kullanici1, kullanici2, kullanici3,
                kullanici4, kullanici5, kullanici6,
                kullanici7, kullanici8, kullanici9
               ]),
        (
            writeln('----------------------------------'),
            format('Kullanıcı: ~w~n', [Kullanici]),
            (   ortalama_odeme_suresi(Kullanici, OrtalamaHesaplandi)
            ->  format(' - Hesaplanan ortalama ödeme süresi: ~2f~n', [OrtalamaHesaplandi])
            ;   format(' - Ortalama ödeme süresi hesaplanamadı (veri yok).~n', [])
            ),
            % Sapma kontrolü: başarılı ise "sapma var" demektir.
            % Başarısız (false) ise "sapma yok" demektir.
            (   odeme_suresi_sapmasi(Kullanici)
            ->  format(' - Son ödeme süresi ORTALAMADAN SAPTI!~n', [])
            ;   format(' - Son ödeme süresi normal sınırlar içinde.~n', [])
            )
        )
    ),

    set_debug(false),
    writeln('----------------------------------'),
    writeln('--- [TEST] Tamamlandı. ---').
