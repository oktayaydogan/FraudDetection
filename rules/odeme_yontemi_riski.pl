% odeme_yontemi_riski.pl
%
% Açıklama:
%   Bu modül, aynı ödeme yönteminin (örneğin aynı kredi kartı) kısa süre içinde
%   farklı hesaplarda kullanılıp kullanılmadığını kontrol eder. Bu tür durumlar,
%   dolandırıcılık şüphesi olarak değerlendirilebilir ve uyarı (alert_message/2)
%   verilir.
%
% Kullanım:
%   1) Prolog ortamında bu dosyayı yükleyin:
%      ?- [odeme_yontemi_riski].
%
%   2) Predikatları aşağıdaki gibi test edebilirsiniz:
%      ?- odeme_yontemi_kontrol('Kredi Kartı').
%      ?- test_odeme_yontemi_riski.
%
% Gereksinimler:
%   - '../data/islem_verileri.pl' içinde islem/11 tanımı olması.
%     Örnek islem/11 yapısı:
%     islem(ID, Kullanici, Miktar, Zaman, Konum, Cihaz, _, _, _, OdemeYontemi, _).
%   - '../utils/debug.pl' ve '../utils/alert.pl' dosyalarında debug_message/2,
%     set_debug/1, alert_message/2 vb. tanımlı olması.
%
% Sınırlamalar:
%   - Bu modül, sadece 'OdemeYontemi' alanını kullanarak ödeme yöntemlerini analiz eder.
%   - Zaman farkı kontrolü için sabit bir eşik değeri (10 birim) kullanılır.
%
% Gelecek Geliştirmeler:
%   - Zaman farkı eşik değerini dinamik olarak değiştirebilme özelliği eklenebilir.
%   - Farklı ödeme yöntemleri için özelleştirilmiş eşik değerleri kullanılabilir.
%
% Modül Tanımı ve İhracı:
:- module(odeme_yontemi_riski, [
    odeme_yontemi_kontrol/1,
    test_odeme_yontemi_riski/0
]).

% Gerekli modüllerin dahil edilmesi
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug').         % Debug mesajları
:- use_module('../utils/alert').         % Alert mesajları

/*
 * KURAL 10:
 * "Aynı ödeme yöntemi (örneğin aynı kredi kartı) kısa süre içinde farklı 
 *  hesaplarda kullanılıyorsa, dolandırıcılık şüphesi oluşur."
 */

% ----------------------------------------------------------------------
% odeme_yontemi_kontrol/1
%
% Açıklama:
%   Belirli bir ödeme yöntemiyle kısa süre içinde işlem yapan kullanıcıları
%   kontrol eder. Eğer aynı ödeme yöntemi kısa sürede farklı hesaplarda
%   kullanılmışsa, uyarı verir.
%
% Parametreler:
%   - OdemeYontemi: Kontrol edilecek ödeme yöntemi (örneğin 'Kredi Kartı').
%
% Örnek Kullanım:
%   ?- odeme_yontemi_kontrol('Kredi Kartı').
%   true.  % Eğer şüpheli işlem varsa
%   false. % Eğer şüpheli işlem yoksa
% ----------------------------------------------------------------------
odeme_yontemi_kontrol(OdemeYontemi) :-
    % Bu ödeme yöntemiyle yapılan tüm işlemleri (Kullanici, Zaman) çiftleri olarak topla
    findall((Kullanici, Zaman),
        islem(_, Kullanici, _, Zaman, _, _, _, _, _, OdemeYontemi, _),
        Islemler),
    % İsterseniz Islemler'i kronolojik sıraya koymak için: 
    % sort(2, @=<, Islemler, IslemlerSirali), ve kontrol_et(IslemlerSirali) kullanabilirsiniz.
    debug_message('Ödeme yöntemiyle ilgili işlemler: ~w => ~w', [OdemeYontemi, Islemler]),
    kontrol_et(Islemler).

% ----------------------------------------------------------------------
% kontrol_et/1
%
% Açıklama:
%   İşlemler arasında kısa süreli tekrar kontrolü yapar. Arka arkaya gelen
%   iki işlemde kullanıcı farkı varsa ve zaman farkı 10 birimden az/eşitse
%   uyarı verir.
%
% Parametreler:
%   - Islemler: (Kullanici, Zaman) çiftlerinden oluşan işlem listesi.
% ----------------------------------------------------------------------
kontrol_et([(Kullanici1, Zaman1), (Kullanici2, Zaman2) | Kalan]) :-
    Kullanici1 \= Kullanici2,
    abs(Zaman2 - Zaman1) =< 10,  % 10 birimlik zaman farkı
    alert_message(
        'Kısa sürede farklı hesaplarda aynı ödeme yöntemi kullanıldı: ~w, ~w, Zamanlar: ~w, ~w',
        [Kullanici1, Kullanici2, Zaman1, Zaman2]
    ),
    kontrol_et([(Kullanici2, Zaman2) | Kalan]).
kontrol_et([(Kullanici1, Zaman1), (Kullanici2, Zaman2) | Kalan]) :-
    % Eğer ya kullanıcı aynı veya zaman farkı > 10 ise sadece debug mesajı verip devam ediyoruz
    debug_message('İşlem kontrol ediliyor -> (~w, ~w) -> (~w, ~w)',
                  [Kullanici1, Zaman1, Kullanici2, Zaman2]),
    kontrol_et([(Kullanici2, Zaman2) | Kalan]).
kontrol_et([_]) :-  % Tek işlem kaldığında kontrol sona erer
    debug_message('Tek işlem kaldı, kontrol sona erdi.').
kontrol_et([]) :-   % İşlem listesi boşsa kontrol sona erer
    debug_message('İşlem listesi boş, kontrol sona erdi.').

% ----------------------------------------------------------------------
% test_odeme_yontemi_riski/0
%
% Açıklama:
%   Belirli ödeme yöntemleri üzerinde otomatik kontrol yapar. Debug modunu
%   etkinleştirir ve sonuçları ekrana yazdırır.
%
% Örnek Kullanım:
%   ?- test_odeme_yontemi_riski.
%
% Örnek Çıktı:
%   --- [TEST] Kural 10: Aynı ödeme yöntemi kısa sürede farklı hesaplarda kullanımı kontrolü başlıyor... ---
%   ----------------------------------
%   Ödeme yöntemi: Kredi Kartı
%   Kısa sürede farklı hesaplarda aynı ödeme yöntemi kullanıldı: kullanici1, kullanici2, Zamanlar: 100, 105
%   ----------------------------------
%   Ödeme yöntemi: Banka Kartı
%   İşlem kontrol ediliyor -> (kullanici3, 200) -> (kullanici4, 220)
%   ----------------------------------
%   --- [TEST] Tamamlandı. ---
% ----------------------------------------------------------------------
test_odeme_yontemi_riski :-
    writeln('--- [TEST] Kural 10: Aynı ödeme yöntemi kısa sürede farklı hesaplarda kullanımı kontrolü başlıyor... ---'),
    set_debug(true),

    % Buradaki listede, proje veritabanınızda (islem_verileri) yer alan yöntemleri sıralayabilirsiniz
    forall(
        member(OW, ['Kredi Kartı', 'Banka Kartı', 'Havale', 'E-Cüzdan']),
        (
            writeln('----------------------------------'),
            format('Ödeme yöntemi: ~w~n', [OW]),
            odeme_yontemi_kontrol(OW)
        )
    ),

    set_debug(false),
    writeln('----------------------------------'),
    writeln('--- [TEST] Tamamlandı. ---').