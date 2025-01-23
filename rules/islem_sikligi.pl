% islem_sikligi.pl
%
% Açıklama:
%   Bu modül, belirli bir zaman aralığında kullanıcının yaptığı işlem sayısını 
%   hesaplar (islem_sayisi/4). Bu sayı bir eşik değeri (esik_degeri/1) 
%   aştığında şüpheli işlem olarak işaretlenir (supheli_islem/3). Bu tür 
%   durumlar, kullanıcı hesaplarının güvenliği açısından potansiyel riskler 
%   olarak değerlendirilebilir.
%
% Kullanım:
%   1) Prolog ortamında bu dosyayı yükleyin:
%      ?- [islem_sikligi].
%
%   2) Predikatları aşağıdaki gibi test edebilirsiniz:
%      ?- islem_sayisi(kullanici1, 0, 24, Sayi).
%      ?- supheli_islem(kullanici1, 0, 24).
%      ?- test_islem_sikligi.
%
% Gereksinimler:
%   - '../data/islem_verileri.pl' içinde islem/11 tanımı olması.
%     Örnek islem/11 yapısı:
%     islem(ID, Kullanici, Miktar, Zaman, Konum, Cihaz, _, _, _, _, _).
%   - '../utils/debug.pl' ve '../utils/alert.pl' dosyalarında debug_message/2, 
%     set_debug/1, alert_message/2 vb. tanımlı olması.
%
% Sınırlamalar:
%   - Bu modül, sadece belirli bir zaman aralığındaki işlem sayısını analiz eder.
%   - Eşik değeri sabit olarak tanımlanmıştır (esik_degeri/1).
%
% Gelecek Geliştirmeler:
%   - Eşik değerini dinamik olarak değiştirebilme özelliği eklenebilir.
%   - Zaman aralıklarını daha esnek hale getirebilme özelliği eklenebilir.
%
% Modül Tanımı ve İhracı:
:- module(islem_sikligi, [islem_sayisi/4, supheli_islem/3, test_islem_sikligi/0]).

% Gerekli modüllerin dahil edilmesi
:- use_module('../data/islem_verileri').  % İşlem verileri (islem/11)
:- use_module('../utils/debug').          % Debug mesajları (debug_message/2, set_debug/1)
:- use_module('../utils/alert').          % Alert mesajları (alert_message/2)

%-----------------------------------------------------------------------------
% islem_sayisi/4
%
% Açıklama:
%   Belirli bir kullanıcının (Kullanici) belirlenen zaman aralığında
%   (Baslangic-Bitis) yaptığı işlem sayısını (Sayi) döndürür.
%
% Parametreler:
%   - Kullanici:  İşlem sayısı hesaplanacak kullanıcı kimliği.
%   - Baslangic:  Zaman aralığının başlangıç değeri.
%   - Bitis:      Zaman aralığının bitiş değeri.
%   - Sayi:       Hesaplanan işlem sayısı (çıktı).
%
% Örnek Kullanım:
%   ?- islem_sayisi(kullanici1, 0, 24, Sayi).
%   Sayi = 5.
%-----------------------------------------------------------------------------
islem_sayisi(Kullanici, Baslangic, Bitis, Sayi) :-
    findall(1,
            (
                islem(_, Kullanici, _, Zaman, _, _, _, _, _, _, _),
                Zaman >= Baslangic,
                Zaman =< Bitis
            ),
            Islemler),
    length(Islemler, Sayi),
    debug_message('İşlem sayısı hesaplandı: ~w => ~w', [Kullanici, Sayi]).

%-----------------------------------------------------------------------------
% supheli_islem/3
%
% Açıklama:
%   islem_sayisi/4 ile elde edilen işlem sayısının, esik_degeri/1'de tanımlı
%   eşik değeri aşıp aşmadığını kontrol eder. Aşıyorsa alert_message/2 ile 
%   “Şüpheli işlem sayısı” uyarısı yayınlar, aksi halde debug_message/2 ile
%   “İşlem sayısı normal” der.
%
% Parametreler:
%   - Kullanici:  Şüpheli işlem kontrolü yapılacak kullanıcı kimliği.
%   - Baslangic:  Zaman aralığının başlangıç değeri.
%   - Bitis:      Zaman aralığının bitiş değeri.
%
% Örnek Kullanım:
%   ?- supheli_islem(kullanici1, 0, 24).
%   true.  % Eğer şüpheli işlem varsa
%   false. % Eğer şüpheli işlem yoksa
%-----------------------------------------------------------------------------
supheli_islem(Kullanici, Baslangic, Bitis) :-
    esik_degeri(Esik),
    islem_sayisi(Kullanici, Baslangic, Bitis, Sayi),
    debug_message('Eşik değeri: ~w, İşlem sayısı: ~w', [Esik, Sayi]),
    ( Sayi > Esik ->
        alert_message(
            'Kural 1: Şüpheli işlem sayısı (>=~w) tespit edildi: Kullanıcı: ~w, İşlem Sayısı: ~w',
            [Esik, Kullanici, Sayi]
        )
    ;   debug_message('Kullanıcı: ~w, İşlem sayısı normal: ~w', [Kullanici, Sayi])
    ).

%-----------------------------------------------------------------------------
% esik_degeri/1
%
% Açıklama:
%   Şüpheli işlem olarak kabul edilebilecek üst sınırda kaç işlem 
%   olduğuna dair sabit değeri tutar. İhtiyaç halinde bu değeri değiştirebilirsiniz.
%
% Örnek Kullanım:
%   ?- esik_degeri(Esik).
%   Esik = 3.
%-----------------------------------------------------------------------------
esik_degeri(3).
% Örneğin kuralda "10 işlem" deniyorsa, esik_degeri(10). yapabilirsiniz.

%-----------------------------------------------------------------------------
% test_islem_sikligi/0
%
% Açıklama:
%   Örnek kullanıcılar ve zaman aralıkları için supheli_islem/3 predikatını
%   test eder. Debug modunu etkinleştirir ve sonuçları ekrana yazdırır.
%
% Örnek Kullanım:
%   ?- test_islem_sikligi.
%
% Örnek Çıktı:
%   --- [TEST] Kural 1 (İşlem Sıklığı) Kontrolü Başlıyor... ---
%   ----------------------------------
%   Kullanıcı: kullanici1, Şüpheli işlem sayısı tespit edildi.
%   ----------------------------------
%   Kullanıcı: kullanici2, Şüpheli işlem sayısı tespit edilemedi.
%   ----------------------------------
%   --- [TEST] Tamamlandı. ---
%-----------------------------------------------------------------------------
test_islem_sikligi :-
    writeln('--- [TEST] Kural 1 (İşlem Sıklığı) Kontrolü Başlıyor... ---'),
    set_debug(true),
    forall(
        member((Kullanici, Baslangic, Bitis),
               [
                 (kullanici1, 0, 24),
                 (kullanici2, 10, 30),
                 (kullanici3, 5, 20)
               ]),
        (
            writeln('----------------------------------'),
            ( supheli_islem(Kullanici, Baslangic, Bitis) ->
                format('Kullanıcı: ~w, Şüpheli işlem sayısı tespit edildi.~n', [Kullanici])
            ;   format('Kullanıcı: ~w, Şüpheli işlem sayısı tespit edilemedi.~n', [Kullanici])
            )
        )
    ),
    set_debug(false),
    writeln('----------------------------------'),
    writeln('--- [TEST] Tamamlandı. ---').