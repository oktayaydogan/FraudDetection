% davranis_analizi.pl
%
% Açıklama:
%   Bu modül, kullanıcıların işlem sürelerini analiz ederek ortalama, standart sapma ve
%   son işlem süresindeki olağan dışı sapmaları tespit eder. Davranışsal anormallikler
%   (örneğin çok kısa ya da çok uzun işlem süreleri) tespit edilirse uyarı mesajı verir.
%   Bu tür anormallikler, kullanıcı hesaplarının güvenliği açısından potansiyel riskler
%   olarak değerlendirilebilir.
%
% Kullanım:
%   1) Bu modülü Prolog ortamında yükleyin:
%      ?- [davranis_analizi].
%
%   2) Predikatları aşağıdaki gibi test edebilirsiniz:
%      ?- davranis_sapmasi(kullanici1).
%      ?- test_davranis_analizi.
%
% Gereksinimler:
%   - '../data/islem_verileri.pl' dosyasında 'islem/11' yapısının tanımlanmış olması.
%     Örnek islem/11 yapısı:
%     islem(ID, Kullanici, Tip, Zaman, Konum, Cihaz, DavranisSure, ...).
%   - '../utils/debug.pl' dosyasında debug_message/2 ve set_debug/1 gibi yardımcı predikatların tanımlı olması.
%   - '../utils/alert.pl' dosyasında alert_message/1 gibi fonksiyonların tanımlı olması (gerekirse).
%
% Sınırlamalar:
%   - Bu modül, sadece 'DavranisSure' alanını kullanarak işlem sürelerini analiz eder.
%   - Standart sapma hesaplaması, sadece mevcut işlem sürelerine dayanır. Geçmiş verilerle karşılaştırma yapmaz.
%
% Gelecek Geliştirmeler:
%   - İşlem sürelerinin zaman içindeki değişimini analiz eden dinamik bir model eklenebilir.
%   - Farklı kullanıcı grupları için özelleştirilmiş sapma katsayıları kullanılabilir.
%
% Modül Tanımı ve İhracı:
:- module(davranis_analizi, [davranis_sapmasi/1, test_davranis_analizi/0]).

% İlgili modüllerin yüklenmesi:
:- use_module('../data/islem_verileri').  % Veriler
:- use_module('../utils/debug').          % Debug mesajları
:- use_module('../utils/alert').          % Uyarı mesajları

%-----------------------------------------------------------------------------
%   KURAL 8:
%   "Kullanıcı oturum açtıktan sonra ödeme işlemlerine geçiş süresi normalden daha hızlı
%    veya daha yavaşsa, bu durum dolandırıcılık şüphesi oluşturabilir."
%  
%   Örnek: Kullanıcı normalde oturum açtıktan sonra 5-10 dakika geçtikten sonra ödeme
%          yaparken, bu sefer birkaç saniye içinde ödeme yapıyorsa bu durum şüpheli
%          olarak değerlendirilir.
%
%-----------------------------------------------------------------------------
% davranis_ortalama/2
%
% Açıklama:
%   Belirli bir kullanıcının gerçekleştirdiği tüm işlemlerin 'DavranisSure'
%   alanlarını bularak ortalamayı hesaplar. Bulunamazsa hata mesajı yazar.
%
% Parametreler:
%   - Kullanici:  Ortalama davranış süresi hesaplanacak kullanıcı.
%   - Ortalama:   Hesaplanan ortalama (çıktı).
%
% Örnek Kullanım:
%   ?- davranis_ortalama(kullanici1, Ortalama).
%   Ortalama = 15.7.
%-----------------------------------------------------------------------------
davranis_ortalama(Kullanici, Ortalama) :-
    findall(DavranisSure,
            islem(_, Kullanici, _, _, _, _, DavranisSure, _, _, _, _),
            Sureler),
    toplam(Sureler, Toplam),
    length(Sureler, Say),
    ( Say > 0 ->
        Ortalama is Toplam / Say,
        debug_message('Davranış sürelerinin ortalaması: ~w', [Ortalama])
    ;   debug_message('Kullanıcının işlem süreleri bulunamadı: ~w', [Kullanici]),
        fail
    ).

%-----------------------------------------------------------------------------
% davranis_standart_sapma/2
%
% Açıklama:
%   Belirli bir kullanıcının işlem sürelerinden (DavranisSure) hareketle
%   standart sapmayı hesaplar. Bunun için önce ortalama bulunur, sonra 
%   fark kareleri alınarak varyans üzerinden sapma bulunur.
%
% Parametreler:
%   - Kullanici:  Standart sapması hesaplanacak kullanıcı.
%   - Sapma:      Hesaplanan standart sapma (çıktı).
%
% Örnek Kullanım:
%   ?- davranis_standart_sapma(kullanici1, Sapma).
%   Sapma = 3.2.
%-----------------------------------------------------------------------------
davranis_standart_sapma(Kullanici, Sapma) :-
    findall(DavranisSure,
            islem(_, Kullanici, _, _, _, _, DavranisSure, _, _, _, _),
            Sureler),
    davranis_ortalama(Kullanici, Ortalama),
    findall(SqDiff,
            ( member(DavranisSure, Sureler),
              SqDiff is (DavranisSure - Ortalama) ** 2
            ),
            KareFarklar),
    toplam(KareFarklar, KareToplam),
    length(Sureler, Say),
    ( Say > 0 ->
        Variance is KareToplam / Say,
        Sapma is sqrt(Variance),
        debug_message('Davranış sürelerinin standart sapması: ~w', [Sapma])
    ;   debug_message('Kullanıcının işlem süreleri standart sapması hesaplanamadı: ~w', [Kullanici]),
        fail
    ).

%-----------------------------------------------------------------------------
% toplam/2
%
% Açıklama:
%   Bir sayı listesinin toplamını döndürür.
%
% Parametreler:
%   - Liste:  Toplamı alınacak liste.
%   - Toplam: Listenin toplam değeri (çıktı).
%
% Örnek Kullanım:
%   ?- toplam([10, 20, 30], Sonuc).
%   Sonuc = 60.
%-----------------------------------------------------------------------------
toplam([], 0).
toplam([H|T], Toplam) :-
    toplam(T, AltToplam),
    Toplam is H + AltToplam.

%-----------------------------------------------------------------------------
% davranis_sapmasi/1
%
% Açıklama:
%   Bir kullanıcının son işlem süresinin (DavranisSure) ortalama±sapma 
%   aralığından önemli ölçüde sapıp sapmadığını kontrol eder. Eğer 
%   beklenen aralığın dışında ise 'alert_message/1' ile uyarı verir.
%   Değilse normal kabul edilir.
%
% Parametreler:
%   - Kullanici:  Davranış süresi kontrolü yapılacak kullanıcı.
%
% Örnek Kullanım:
%   ?- davranis_sapmasi(kullanici1).
%   true.  % Eğer sapma varsa
%   false. % Eğer sapma yoksa
%-----------------------------------------------------------------------------
davranis_sapmasi(Kullanici) :-
    ( var(Kullanici) ->
        alert_message('Hata: Kullanıcı belirtilmemiş.')
    ;   true
    ),
    davranis_ortalama(Kullanici, Ortalama),
    davranis_standart_sapma(Kullanici, Sapma),
    findall((DavranisSure, Zaman),
            islem(_, Kullanici, _, Zaman, _, _, DavranisSure, _, _, _, _),
            TumIslemler),
    ( TumIslemler \= [] ->
        % Son işlem süresi bulmak için Zaman'a göre sıralıyoruz
        sort(2, @>=, TumIslemler, [(SonSure, _)|_]),
        debug_message('Son işlem süresi: ~w', [SonSure]),
        Katsayi = 1.5,       % Bu katsayıyı isteğe bağlı değiştirebilirsiniz
        Limit is Sapma * Katsayi,
        debug_message('Sapma limiti: ~w', [Limit]),
        ( SonSure > Ortalama + Limit ; SonSure < Ortalama - Limit ->
            alert_message('Davranış süresi sapıyor!'),
            true
        ;   debug_message('Davranış süresi normal.'),
            fail
        )
    ;   debug_message('Kullanıcının işlem süreleri bulunamadı: ~w', [Kullanici]),
        fail
    ).

%-----------------------------------------------------------------------------
% test_davranis_analizi/0
%
% Açıklama:
%   Belirli kullanıcılar (kullanici1, kullanici2, vb.) üzerinde 
%   davranis_sapmasi/1 predikatını test eder. Sonuçlar konsola yazdırılır.
%
% Örnek Kullanım:
%   ?- test_davranis_analizi.
%
% Örnek Çıktı:
%   --- [TEST] Kural 8 (İşlem Sıklığı) Kontrolü Başlıyor... ---
%   ----------------------------------
%   Kullanıcı: kullanici1, Sonuç: Davranış süresi sapıyor.
%   ----------------------------------
%   Kullanıcı: kullanici2, Sonuç: Davranış süresi normal.
%   ----------------------------------
%   --- [TEST] Tamamlandı. ---
%-----------------------------------------------------------------------------
test_davranis_analizi :-
    writeln('--- [TEST] Kural 8 (İşlem Sıklığı) Kontrolü Başlıyor... ---'),
    set_debug(true),  % Debug modunu aç
    forall(
        member(Kullanici, [kullanici1, kullanici2, kullanici3, kullanici4]),
        (
            writeln('----------------------------------'),
            ( davranis_sapmasi(Kullanici) ->
                format('Kullanıcı: ~w, Sonuç: Davranış süresi sapıyor.~n', [Kullanici])
            ;   format('Kullanıcı: ~w, Sonuç: Davranış süresi normal.~n', [Kullanici])
            )
        )
    ),
    set_debug(false), % Debug modunu kapat
    writeln('----------------------------------'),
    writeln('--- [TEST] Tamamlandı. ---').