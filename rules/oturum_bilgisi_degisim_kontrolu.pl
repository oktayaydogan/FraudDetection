% oturum_bilgisi_degisim_kontrolu.pl
%
% Açıklama:
%   Bu modül, kullanıcının bilgi değişikliği (örneğin, şifre veya iletişim bilgisi değişikliği)
%   yaptıktan sonra çok kısa süre içinde bir işlem (ödeme vb.) yapıp yapmadığını kontrol eder.
%   Bu tür durumlar, dolandırıcılık şüphesi olarak değerlendirilebilir ve uyarı (alert_message/2)
%   verilir.
%
% Kullanım:
%   1) Prolog ortamında bu dosyayı yükleyin:
%      ?- [oturum_bilgisi_degisim_kontrolu].
%
%   2) Predikatları aşağıdaki gibi test edebilirsiniz:
%      ?- bilgi_degisim_riski(kullanici1).
%      ?- test_bilgi_degisim_riski.
%
% Gereksinimler:
%   - '../data/islem_verileri.pl' içinde islem/11 tanımı olması.
%     Örnek islem/11 yapısı:
%     islem(ID, Kullanici, Miktar, Zaman, Konum, Cihaz, _, Tip, _, _, _).
%   - '../utils/debug.pl' ve '../utils/alert.pl' dosyalarında debug_message/2,
%     set_debug/1, alert_message/2 vb. tanımlı olması.
%
% Sınırlamalar:
%   - Bu modül, sadece 'degisim' ve 'islem' tipindeki işlemleri analiz eder.
%   - Zaman farkı kontrolü için sabit bir eşik değeri (5 birim) kullanılır.
%
% Gelecek Geliştirmeler:
%   - Zaman farkı eşik değerini dinamik olarak değiştirebilme özelliği eklenebilir.
%   - Farklı bilgi değişikliği türleri için özelleştirilmiş eşik değerleri kullanılabilir.
%
% Modül Tanımı ve İhracı:
:- module(oturum_bilgisi_degisim_kontrolu, [
    bilgi_degisim_riski/1,
    test_bilgi_degisim_riski/0
]).

% Gerekli modüllerin dahil edilmesi
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug').         % Debug mesajları
:- use_module('../utils/alert').         % Alert mesajları

% ----------------------------------------------------------------------
%   KURAL 14:
%   "Kullanıcının hesap bilgilerinde yapılan olağandışı değişiklikler (örneğin, e-posta veya şifre değişikliği)
%    sonrası hemen büyük bir işlem yapılması, dolandırıcılık olarak değerlendirilebilir."
%
%   Örnek: Kullanıcı şifresini değiştirdikten hemen sonra büyük miktarda bir ödeme yapmak istiyorsa
%          bu durum riskli olabilir.
%
% ----------------------------------------------------------------------
% bilgi_degisim_riski/1
%
% Açıklama:
%   Kullanıcının bilgi değişikliği ve işlem kayıtlarını toplar ve kontrol eder.
%   Eğer bilgi değişikliğinden kısa süre sonra bir işlem yapılmışsa, uyarı verir.
%
% Parametreler:
%   - Kullanici: Kontrol edilecek kullanıcı kimliği.
%
% Örnek Kullanım:
%   ?- bilgi_degisim_riski(kullanici1).
%   true.  % Eğer şüpheli işlem varsa
%   false. % Eğer şüpheli işlem yoksa
% ----------------------------------------------------------------------
bilgi_degisim_riski(Kullanici) :-
    findall((DegisimZamani, DegisimID),
            islem(DegisimID, Kullanici, _, DegisimZamani, _, _, _, 'degisim', _, _, _),
            Degisimler),
    findall((IslemZamani, IslemID),
            islem(IslemID, Kullanici, _, IslemZamani, _, _, _, 'islem', _, _, _),
            Islemler),
    debug_message('Bilgi değişiklikleri: ~w', [Degisimler]),
    debug_message('İşlemler: ~w', [Islemler]),
    % Eğer degisim ya da islem listesi boşsa, yine de kontrol predikatına gidecek
    bilgi_degisim_kontrol(Degisimler, Islemler).

% ----------------------------------------------------------------------
% bilgi_degisim_kontrol/2
%
% Açıklama:
%   Her bilgi değişikliği kaydı için, tüm işlem kayıtlarına bakar.
%   Eğer zaman farkı ≤5 ise uyarı verir.
%
% Parametreler:
%   - Degisimler: (Zaman, ID) çiftlerinden oluşan bilgi değişikliği listesi.
%   - Islemler:   (Zaman, ID) çiftlerinden oluşan işlem listesi.
% ----------------------------------------------------------------------
bilgi_degisim_kontrol([], _) :-
    debug_message('Bilgi değişikliği sonrası işlem kontrolü tamamlandı. Tüm değişiklikler incelendi.'),
    !.

bilgi_degisim_kontrol([(DegisimZamani, DegisimID)|KalanDegisimler], Islemler) :-
    % Bu degisim kaydı için, islem'lerle karşılaştırma yapalım
    degisim_islemlerini_karsilastir(DegisimZamani, DegisimID, Islemler),
    % Sonra kalan degisim kayıtlarına geçelim
    bilgi_degisim_kontrol(KalanDegisimler, Islemler).

% ----------------------------------------------------------------------
% degisim_islemlerini_karsilastir/3
%
% Açıklama:
%   Tek bir bilgi değişikliği kaydını, tüm işlem kayıtlarıyla karşılaştırır.
%   Eğer zaman farkı ≤5 ise uyarı verir.
%
% Parametreler:
%   - DegisimZamani: Bilgi değişikliğinin zamanı.
%   - DegisimID:     Bilgi değişikliğinin ID'si.
%   - Islemler:      (Zaman, ID) çiftlerinden oluşan işlem listesi.
% ----------------------------------------------------------------------
degisim_islemlerini_karsilastir(_, _, []) :-
    debug_message('Bu degisim için yakın zamanlı işlem bulunamadı.'),
    !.

degisim_islemlerini_karsilastir(DegisimZamani, DegisimID, [(IslemZamani, IslemID)|KalanIslemler]) :-
    abs(IslemZamani - DegisimZamani) =< 5,
    alert_message(
        'Şüpheli işlem tespit edildi: Bilgi Değişikliği ID: ~w => İşlem ID: ~w (D:~w, I:~w)',
        [DegisimID, IslemID, DegisimZamani, IslemZamani]
    ),
    % Sonraki islem kaydına da bakmaya devam ederiz (farklı ID, belki daha çok olay)
    degisim_islemlerini_karsilastir(DegisimZamani, DegisimID, KalanIslemler).

degisim_islemlerini_karsilastir(DegisimZamani, DegisimID, [_|KalanIslemler]) :-
    % Burada abs(fark) > 5 demek
    degisim_islemlerini_karsilastir(DegisimZamani, DegisimID, KalanIslemler).

% ----------------------------------------------------------------------
% test_bilgi_degisim_riski/0
%
% Açıklama:
%   Belirli kullanıcılar üzerinde toplu test yapar. Her kullanıcıda "degisim -> islem"
%   şüpheli durum var mı diye kontrol eder.
%
% Örnek Kullanım:
%   ?- test_bilgi_degisim_riski.
%
% Örnek Çıktı:
%   --- [TEST] Kural 14: Bilgi Değişikliği Sonrası İşlem Kontrolü Başlıyor... ---
%   ----------------------------------
%   Kullanıcı: kullanici1
%    - Kontrol tamamlandı, olası şüpheli işlemler üstte listelenir.
%   ----------------------------------
%   Kullanıcı: kullanici4
%    - Kontrol başarısız veya veri yok.
%   ----------------------------------
%   --- [TEST] Tamamlandı. ---
% ----------------------------------------------------------------------
test_bilgi_degisim_riski :-
    writeln('--- [TEST] Kural 14: Bilgi Değişikliği Sonrası İşlem Kontrolü Başlıyor... ---'),
    set_debug(true),
    forall(
        member(Kullanici, [kullanici1, kullanici4, kullanici5, kullanici6]),
        (
            writeln('----------------------------------'),
            format('Kullanıcı: ~w~n', [Kullanici]),
            (   bilgi_degisim_riski(Kullanici)
            ->  format(' - Kontrol tamamlandı, olası şüpheli işlemler üstte listelenir.~n', [])
            ;   format(' - Kontrol başarısız veya veri yok.~n', [])
            )
        )
    ),
    set_debug(false),
    writeln('----------------------------------'),
    writeln('--- [TEST] Tamamlandı. ---').