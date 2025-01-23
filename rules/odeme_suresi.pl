% odeme_suresi.pl
%
% Açıklama:
%   Bu modül, kullanıcıların ödeme sürelerini analiz ederek ortalama ödeme süresini
%   hesaplar (ortalama_odeme_suresi/2) ve son ödeme süresinin ortalamadan sapıp
%   sapmadığını kontrol eder (odeme_suresi_sapmasi/1). Bu tür sapmalar, kullanıcı
%   hesaplarının güvenliği açısından potansiyel riskler olarak değerlendirilebilir.
%
% Kullanım:
%   1) Prolog ortamında bu dosyayı yükleyin:
%      ?- [odeme_suresi].
%
%   2) Predikatları aşağıdaki gibi test edebilirsiniz:
%      ?- ortalama_odeme_suresi(kullanici1, Ortalama).
%      ?- odeme_suresi_sapmasi(kullanici1).
%      ?- test_odeme_suresi.
%
% Gereksinimler:
%   - '../data/islem_verileri.pl' içinde islem/11 tanımı olması.
%     Örnek islem/11 yapısı:
%     islem(ID, Kullanici, Miktar, Zaman, Konum, Cihaz, DavranisSure, _, _, _, _).
%   - '../utils/debug.pl' ve '../utils/alert.pl' dosyalarında debug_message/2,
%     set_debug/1, alert_message/2 vb. tanımlı olması.
%
% Sınırlamalar:
%   - Bu modül, sadece 'DavranisSure' alanını kullanarak ödeme sürelerini analiz eder.
%   - Sapma limiti sabit olarak tanımlanmıştır (ortalama ±%50).
%
% Gelecek Geliştirmeler:
%   - Sapma limitini dinamik olarak değiştirebilme özelliği eklenebilir.
%   - Zaman içinde ödeme sürelerinin değişimini analiz eden dinamik bir model eklenebilir.
%
% Modül Tanımı ve İhracı:
:- module(odeme_suresi, [
    odeme_suresi_sapmasi/1,
    ortalama_odeme_suresi/2,
    test_odeme_suresi/0
]).

% Gerekli modüllerin dahil edilmesi
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug').         % Debug mesajları
:- use_module('../utils/alert').         % Alert mesajları

% ----------------------------------------------------------------------
% ortalama_odeme_suresi/2
%
% Açıklama:
%   Bir kullanıcının geçmiş tüm işlemlerine ait ödeme sürelerinin ortalamasını
%   hesaplar. Eğer kullanıcıya ait işlem bulunamazsa hata (fail) döndürür.
%
% Parametreler:
%   - Kullanici:  Ortalama ödeme süresi hesaplanacak kullanıcı kimliği.
%   - Ortalama:   Hesaplanan ortalama ödeme süresi (çıktı).
%
% Örnek Kullanım:
%   ?- ortalama_odeme_suresi(kullanici1, Ortalama).
%   Ortalama = 12.5.
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
% odeme_suresi_sapmasi/1
%
% Açıklama:
%   Bir kullanıcının son ödeme süresinin ortalamadan sapıp sapmadığını kontrol eder.
%   Eğer sapma varsa, alert_message/2 ile uyarı verir. Aksi halde debug_message/2 ile
%   "Ödeme süresi normal" mesajı yazdırır.
%
% Parametreler:
%   - Kullanici:  Ödeme süresi sapması kontrol edilecek kullanıcı kimliği.
%
% Örnek Kullanım:
%   ?- odeme_suresi_sapmasi(kullanici1).
%   true.  % Eğer sapma varsa
%   false. % Eğer sapma yoksa
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
    Limit is Ortalama * 1.5,  % ±%50 sapma limiti
    debug_message('Sapma limiti: ~2f (Ortalama ± ~2f)', [Limit, Limit]),

    (   SonSure > Ortalama + Limit
     ;  SonSure < Ortalama - Limit
    ->  % Kural 7 gereği: normalden çok farklı bir davranış süresi
        alert_message(
            'Kullanıcı ~w: DAVRANIŞ SÜRESİ SAPMASI! Son Süre: ~w, Limit: ~2f (Ortalama ~2f)',
            [Kullanici, SonSure, Limit, Ortalama]
        )
    ;   debug_message(
            'Ödeme süresi normal: Kullanıcı: ~w, Son Süre: ~w (Ortalama ~2f)',
            [Kullanici, SonSure, Ortalama]
        ),
        fail  % "sapma yok" => kural başarısız (false) döner
    ).

% ----------------------------------------------------------------------
% son_odeme_suresi/2
%
% Açıklama:
%   Bir liste içindeki son ödeme süresini (en son eklenen işlem süresi) bulur.
%
% Parametreler:
%   - Sureler:   Ödeme sürelerinin listesi.
%   - SonSure:   Listenin sonundaki ödeme süresi (çıktı).
%
% Örnek Kullanım:
%   ?- son_odeme_suresi([10, 20, 30], SonSure).
%   SonSure = 30.
% ----------------------------------------------------------------------
son_odeme_suresi(Sureler, SonSure) :-
    last(Sureler, SonSure).

% ----------------------------------------------------------------------
% toplam/2
%
% Açıklama:
%   Bir liste içindeki sayıların toplamını döndürür.
%
% Parametreler:
%   - Liste:   Toplamı alınacak sayı listesi.
%   - Toplam:  Sonuç olarak elde edilen toplam (çıktı).
%
% Örnek Kullanım:
%   ?- toplam([10, 20, 30], Sonuc).
%   Sonuc = 60.
% ----------------------------------------------------------------------
toplam([], 0).
toplam([H|T], Toplam) :-
    toplam(T, AltToplam),
    Toplam is H + AltToplam.

% ----------------------------------------------------------------------
% test_odeme_suresi/0
%
% Açıklama:
%   Belirli kullanıcılar üzerinde ortalama ödeme süresi ve sapma kontrolünü
%   otomatik şekilde yapar (Kural 7 testi). Debug modunu etkinleştirir ve
%   sonuçları ekrana yazdırır.
%
% Örnek Kullanım:
%   ?- test_odeme_suresi.
%
% Örnek Çıktı:
%   --- [TEST] Odeme Süresi Kontrolü Başlıyor... ---
%   ----------------------------------
%   Kullanıcı: kullanici1
%    - Hesaplanan ortalama ödeme süresi: 12.50
%    - Son ödeme süresi ORTALAMADAN SAPTI!
%   ----------------------------------
%   Kullanıcı: kullanici2
%    - Hesaplanan ortalama ödeme süresi: 15.00
%    - Son ödeme süresi normal sınırlar içinde.
%   ----------------------------------
%   --- [TEST] Tamamlandı. ---
% ----------------------------------------------------------------------
test_odeme_suresi :-
    writeln('--- [TEST] Odeme Süresi Kontrolü Başlıyor... ---'),    
    writeln('--- [TEST] Kural 7 Ödeme Süresi Kontrolü Başlıyor... ---'),

    set_debug(true),

    % Kendi veri kümenizde var olan kullanıcıları ekleyebilirsiniz.
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
            (   odeme_suresi_sapmasi(Kullanici)
            ->  format(' - Son ödeme süresi ORTALAMADAN SAPTI!~n', [])
            ;   format(' - Son ödeme süresi normal sınırlar içinde.~n', [])
            )
        )
    ),

    set_debug(false),
    writeln('----------------------------------'),
    writeln('--- [TEST] Tamamlandı. ---').