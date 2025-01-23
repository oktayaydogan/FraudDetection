% islem_miktari.pl
%
% Açıklama:
%   Bu modül, kullanıcının işlem miktarlarını analiz ederek ortalama hesaplar
%   (ortalama/2) ve anormal miktarları (ortalamanın belirli bir katsayısı
%   üzerindeki değerler) tespit eder (anormal_islem/2). Anormal işlemler,
%   kullanıcı hesaplarının güvenliği açısından potansiyel riskler olarak
%   değerlendirilebilir.
%
% Kullanım:
%   1) Prolog ortamında bu dosyayı yükleyin:
%      ?- [islem_miktari].
%
%   2) Predikatları aşağıdaki gibi çağırabilirsiniz:
%      ?- ortalama(kullanici1, Ortalama).
%      ?- anormal_islem(kullanici2, 5000).
%      ?- test_islem_miktari.
%
% Gereksinimler:
%   - '../data/islem_verileri.pl' içinde islem/11 tanımı olması.
%     Örnek islem/11 yapısı:
%     islem(ID, Kullanici, Miktar, Zaman, Konum, Cihaz, _, _, _, _, _).
%   - '../utils/debug.pl' ve '../utils/alert.pl' dosyalarında debug_message/2,
%     set_debug/1, alert_message/2 vb. tanımlı olması.
%
% Sınırlamalar:
%   - Bu modül, sadece 'Miktar' alanını kullanarak işlem miktarlarını analiz eder.
%   - Anormal işlem tespiti için sabit bir katsayı (örn. 3) kullanılır.
%
% Gelecek Geliştirmeler:
%   - Katsayıyı dinamik olarak değiştirebilme özelliği eklenebilir.
%   - Zaman içinde işlem miktarlarının değişimini analiz eden dinamik bir model eklenebilir.
%
% Modül Tanımı ve İhracı:
:- module(islem_miktari, [ortalama/2, anormal_islem/2, test_islem_miktari/0]).

% Gerekli modüllerin dahil edilmesi
:- use_module('../data/islem_verileri').  % İşlem verileri
:- use_module('../utils/debug').          % Debug mesajları
:- use_module('../utils/alert').          % Alert/uyarı mesajları

%-----------------------------------------------------------------------------
% ortalama/2
%
% Açıklama:
%   Bir kullanıcının geçmiş tüm işlemlerine ait miktarların ortalamasını 
%   hesaplar. Eğer kullanıcıya ait işlem bulunamazsa hata (fail) döndürür.
%
% Parametreler:
%   - Kullanici: Kullanıcının kimliği/ID'si.
%   - Ortalama:  Bulunan ortalama miktar (çıktı).
%
% Örnek Kullanım:
%   ?- ortalama(kullanici1, Ortalama).
%   Ortalama = 1200.5.
%-----------------------------------------------------------------------------
ortalama(Kullanici, Ortalama) :-
    findall(Miktar,
            islem(_, Kullanici, Miktar, _, _, _, _, _, _, _, _),
            Islemler),
    toplam(Islemler, Toplam),
    length(Islemler, Say),
    ( Say > 0 ->
        Ortalama is Toplam / Say,
        debug_message('İşlem ortalaması hesaplandı: ~w => ~w', [Kullanici, Ortalama])
    ;   debug_message('Kullanıcının işlemleri bulunamadı: ~w', [Kullanici]),
        fail
    ).

%-----------------------------------------------------------------------------
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
%   ?- toplam([10,20,30], Sonuc).
%   Sonuc = 60.
%-----------------------------------------------------------------------------
toplam([], 0).
toplam([H|T], Toplam) :-
    toplam(T, AltToplam),
    Toplam is H + AltToplam.

%-----------------------------------------------------------------------------
% anormal_islem/2
%
% Açıklama:
%   Bir kullanıcının (Kullanici) yaptığı işlemin (Miktar) ortalamanın belirli 
%   bir katsayısını (örn. 3) aşıp aşmadığını kontrol eder. Aşıyorsa 
%   alert_message/2 ile "Anormal işlem" uyarısı yayınlar, değilse "İşlem normal" 
%   şeklinde debug mesajı yazdırır.
%
% Parametreler:
%   - Kullanici: Anormalliği kontrol edilecek kullanıcı kimliği.
%   - Miktar:    İncelenecek işlemin miktarı.
%
% Örnek Kullanım:
%   ?- anormal_islem(kullanici1, 5000).
%   true.  % Eğer anormal işlem varsa
%   false. % Eğer anormal işlem yoksa
%-----------------------------------------------------------------------------
anormal_islem(Kullanici, Miktar) :-
    ortalama(Kullanici, Ortalama),
    Katsayi is 3,                    % Ortalamanın bu katsayı kadar üstü "anormal"
    Limit is Ortalama * Katsayi,
    debug_message('Anormal işlem limiti: ~w, Girilen miktar: ~w', [Limit, Miktar]),
    ( Miktar > Limit ->
        alert_message('Anormal işlem tespit edildi: Kullanıcı: ~w, Miktar: ~w', [Kullanici, Miktar])
    ;   debug_message('İşlem normal: Kullanıcı: ~w, Miktar: ~w', [Kullanici, Miktar])
    ).

%-----------------------------------------------------------------------------
% test_islem_miktari/0
%
% Açıklama:
%   Farklı kullanıcılar ve örnek miktar değerleri üzerinde ortalama ve 
%   anormal_islem kontrolleri yapar. Debug modunu açıp kapatarak detaylı 
%   mesaj çıktıları verir.
%
% Örnek Kullanım:
%   ?- test_islem_miktari.
%
% Örnek Çıktı:
%   --- [TEST] Kural 2 (İşlem Miktarı) Kontrolü Başlıyor... ---
%   ----------------------------------
%   Kullanıcı: kullanici1, Ortalama: 1200.5
%   Kullanıcı: kullanici1, Miktar: 1200 => İşlem normal.
%   ----------------------------------
%   Kullanıcı: kullanici2, Ortalama: 800.0
%   Kullanıcı: kullanici2, Miktar: 8000 => Anormal işlem tespit edildi.
%   ----------------------------------
%   --- [TEST] Tamamlandı. ---
%-----------------------------------------------------------------------------
test_islem_miktari :-    
    writeln('--- [TEST] Kural 2 (İşlem Miktarı) Kontrolü Başlıyor... ---'),
    writeln('Test: islem_miktari kontrolü başlıyor...'),
    set_debug(true),
    forall(
        member((Kullanici, TestMiktar),
               [(kullanici1, 1200),
                (kullanici2, 8000),
                (kullanici3, 400),
                (kullanici4, 1500)]),
        (
            writeln('----------------------------------'),
            ( ortalama(Kullanici, Ort) ->
                format('Kullanıcı: ~w, Ortalama: ~w~n', [Kullanici, Ort])
            ;   format('Kullanıcı: ~w için işlem bulunamadı.~n', [Kullanici])
            ),
            ( anormal_islem(Kullanici, TestMiktar) ->
                format('Kullanıcı: ~w, Miktar: ~w => Anormal işlem tespit edildi.~n',
                       [Kullanici, TestMiktar])
            ;   format('Kullanıcı: ~w, Miktar: ~w => İşlem normal.~n',
                       [Kullanici, TestMiktar])
            )
        )
    ),
    set_debug(false),
    writeln('----------------------------------'),
    writeln('--- [TEST] Tamamlandı. ---').