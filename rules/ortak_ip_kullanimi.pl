% ortak_ip_kullanimi.pl
%
% Açıklama:
%   Bu modül, aynı IP adresi üzerinden kısa sürede birden fazla kullanıcı hesabı ile
%   ödeme yapılıp yapılmadığını kontrol eder. Bu tür durumlar, dolandırıcılık şüphesi
%   olarak değerlendirilebilir ve uyarı (alert_message/2) verilir.
%
% Kullanım:
%   1) Prolog ortamında bu dosyayı yükleyin:
%      ?- [ortak_ip_kullanimi].
%
%   2) Predikatları aşağıdaki gibi test edebilirsiniz:
%      ?- ayni_ip_kontrol('192.168.1.1', Sonuc).
%      ?- ip_adresinden_islemler('192.168.1.1', Islemler).
%      ?- test_ortak_ip.
%
% Gereksinimler:
%   - '../data/islem_verileri.pl' içinde islem/11 tanımı olması.
%     Örnek islem/11 yapısı:
%     islem(ID, Kullanici, Miktar, Zaman, Konum, Cihaz, _, _, IP, OdemeYontemi, _).
%   - '../utils/debug.pl' ve '../utils/alert.pl' dosyalarında debug_message/2,
%     set_debug/1, alert_message/2 vb. tanımlı olması.
%
% Sınırlamalar:
%   - Bu modül, sadece 'IP' alanını kullanarak işlemleri analiz eder.
%   - Zaman farkı kontrolü için sabit bir eşik değeri (5 birim) kullanılır.
%
% Gelecek Geliştirmeler:
%   - Zaman farkı eşik değerini dinamik olarak değiştirebilme özelliği eklenebilir.
%   - Farklı IP adresleri için özelleştirilmiş eşik değerleri kullanılabilir.
%
% Modül Tanımı ve İhracı:
:- module(ortak_ip_kullanimi, [
    ayni_ip_kontrol/2,
    ip_adresinden_islemler/2,
    test_ortak_ip/0
]).

% Gerekli modüllerin dahil edilmesi
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug').         % Debug mesajları
:- use_module('../utils/alert').         % Alert mesajları

/*
 * KURAL 3:
 * "Aynı IP adresi üzerinden kısa sürede birden fazla kullanıcı hesabı ile
 *  ödeme yapılmaya çalışılıyorsa, bu durum dolandırıcılık şüphesi doğurabilir."
 *
 * Örnek: Farklı kullanıcılar aynı IP adresinden işlem yapıyorsa,
 *        hele bir de bu işlemler kısa zaman aralıklarında gerçekleşiyorsa
 *        şüpheli olarak değerlendirilir.
 */

% ----------------------------------------------------------------------
% ip_adresinden_kullanicilar/2
%
% Açıklama:
%   Belirli bir IP adresinden işlem yapan kullanıcıları listeler.
%
% Parametreler:
%   - IP:               Kontrol edilecek IP adresi.
%   - KullaniciListesi: Bu IP adresinden işlem yapan kullanıcıların listesi (çıktı).
%
% Örnek Kullanım:
%   ?- ip_adresinden_kullanicilar('192.168.1.1', KullaniciListesi).
%   KullaniciListesi = [kullanici1, kullanici2].
% ----------------------------------------------------------------------
ip_adresinden_kullanicilar(IP, KullaniciListesi) :-
    findall(Kullanici,
            islem(_, Kullanici, _, _, _, _, _, _, IP, _, _),
            TumKullanicilar),
    list_to_set(TumKullanicilar, KullaniciListesi),
    debug_message('IP adresinden işlem yapan kullanıcılar: ~w => ~w', [IP, KullaniciListesi]).

% ----------------------------------------------------------------------
% ayni_ip_kontrol/2
%
% Açıklama:
%   Aynı IP adresinden birden fazla kullanıcı işlem yapmış mı ve bu işlemler
%   kısa süre içinde gerçekleşmiş mi kontrol eder. Eğer şüpheli bir durum varsa,
%   uyarı verir.
%
% Parametreler:
%   - IP:     Kontrol edilecek IP adresi.
%   - Sonuc:  Kontrol sonucu ('Şüpheli' veya 'Normal').
%
% Örnek Kullanım:
%   ?- ayni_ip_kontrol('192.168.1.1', Sonuc).
%   Sonuc = 'Şüpheli'.
% ----------------------------------------------------------------------
ayni_ip_kontrol(IP, Sonuc) :-
    % Tüm işlemleri çekip zaman sırasına göre sıralayalım:
    findall((Zaman, Kullanici),
            islem(_, Kullanici, _, Zaman, _, _, _, _, IP, _, _),
            Islemler),
    sort(0, @=<, Islemler, IslemlerSirali),
    debug_message('Aynı IP üzerinden işlemler (Zaman,Kullanıcı): ~w => ~w', [IP, IslemlerSirali]),

    % Kullanıcı sayısı:
    ip_adresinden_kullanicilar(IP, KullaniciListesi),
    length(KullaniciListesi, KullaniciSayisi),
    debug_message('Kullanıcı sayısı: ~w', [KullaniciSayisi]),

    (   KullaniciSayisi =:= 0
    ->  debug_message('Bu IP ile hiç işlem yok.'),
        Sonuc = 'Normal'
    ;   KullaniciSayisi =:= 1
    ->  debug_message('Aynı IP üzerinden yalnızca bir kullanıcı işlem yaptı.'),
        Sonuc = 'Normal'
    ;   % Birden fazla kullanıcı var. Şimdi KISA SÜRE kriterini inceleyelim:
        (   farkli_kullanicilar_kisa_sure(IslemlerSirali, 5)
        ->  alert_message('Kural 3: Şüpheli! Aynı IP üzerinden kısa sürede birden fazla hesap kullanıldı. IP: ~w, Kullanıcılar: ~w',
                          [IP, KullaniciListesi]),
            Sonuc = 'Şüpheli'
        ;   debug_message('Aynı IP üzerinden birden fazla kullanıcı var ama kısa süreli geçiş yok.'),
            Sonuc = 'Normal'
        )
    ).

% ----------------------------------------------------------------------
% farkli_kullanicilar_kisa_sure/2
%
% Açıklama:
%   Liste halindeki (Zaman,Kullanici) çiftlerine bakar. Ardışık iki işlemde
%   kullanıcı farklı VE zaman farkı <= Limit ise true döner.
%
% Parametreler:
%   - Islemler: (Zaman, Kullanici) çiftlerinden oluşan işlem listesi.
%   - Limit:    Zaman farkı için eşik değeri.
%
% Örnek Kullanım:
%   ?- farkli_kullanicilar_kisa_sure([(100, kullanici1), (105, kullanici2)], 5).
%   true.
% ----------------------------------------------------------------------
farkli_kullanicilar_kisa_sure([(Z1, U1), (Z2, U2) | _Kalan], Limit) :-
    U1 \== U2,             % Farklı kullanıcı
    Z2 - Z1 =< Limit,      % Kısa zaman aralığı
    !.                     % Tek bir örnek bulmak yeterli => true
farkli_kullanicilar_kisa_sure([(_, _U1), (Z2, U2) | Kalan], Limit) :-
    % Şimdilik bulamadık, devam edelim
    farkli_kullanicilar_kisa_sure([(Z2, U2) | Kalan], Limit).
farkli_kullanicilar_kisa_sure(_Liste, _Limit) :-
    % Liste 0 veya 1 elemanlı kaldıysa ya da taradıkça bulamadıysak
    false.

% ----------------------------------------------------------------------
% ip_adresinden_islemler/2
%
% Açıklama:
%   Belirtilen IP adresinden yapılan tüm işlemleri listeler.
%
% Parametreler:
%   - IP:       Kontrol edilecek IP adresi.
%   - Islemler: Bu IP adresinden yapılan işlemlerin listesi (çıktı).
%
% Örnek Kullanım:
%   ?- ip_adresinden_islemler('192.168.1.1', Islemler).
%   Islemler = [(1, kullanici1, 100, 'Cihaz1', 'Kredi Kartı'), ...].
% ----------------------------------------------------------------------
ip_adresinden_islemler(IP, Islemler) :-
    findall((ID, Kullanici, Zaman, Cihaz, OdemeYontemi),
            islem(ID, Kullanici, _, Zaman, _, Cihaz, _, _, IP, OdemeYontemi, _),
            Islemler),
    debug_message('IP adresinden yapılan işlemler: ~w => ~w', [IP, Islemler]).

% ----------------------------------------------------------------------
% test_ortak_ip/0
%
% Açıklama:
%   Belirli IP adresleri üzerinde otomatik kontrol yapar. Debug modunu
%   etkinleştirir ve sonuçları ekrana yazdırır.
%
% Örnek Kullanım:
%   ?- test_ortak_ip.
%
% Örnek Çıktı:
%   --- [TEST] Kural 3: Aynı IP üzerinden kısa sürede çoklu kullanıcı kontrolü başlıyor... ---
%   ----------------------------------
%   Kontrol edilen IP: 192.168.1.1
%    - Sonuç: Şüpheli
%   ----------------------------------
%   Kontrol edilen IP: 10.0.0.2
%    - Sonuç: Normal
%   ----------------------------------
%   --- [TEST] Tamamlandı. ---
% ----------------------------------------------------------------------
test_ortak_ip :-
    writeln('--- [TEST] Kural 3: Aynı IP üzerinden kısa sürede çoklu kullanıcı kontrolü başlıyor... ---'),
    set_debug(true),

    % IP adresleri listesi: (Örnek)
    forall(
        member(IP, [
            '192.168.1.1',
            '10.0.0.2',
            '10.0.0.3',
            '192.168.1.5'
        ]),
        (
            writeln('----------------------------------'),
            format('Kontrol edilen IP: ~w~n', [IP]),
            (   ayni_ip_kontrol(IP, Sonuc)
            ->  format(' - Sonuç: ~w~n', [Sonuc])
            ;   format(' - Kontrol başarısız veya hiç işlem yok.~n', [])
            )
        )
    ),

    set_debug(false),
    writeln('----------------------------------'),
    writeln('--- [TEST] Tamamlandı. ---').