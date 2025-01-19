:- module(ortak_ip_kullanimi, [
    ayni_ip_kontrol/2,
    ip_adresinden_islemler/2,
    test_ortak_ip/0
]).

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

% 1) Belirli bir IP adresinden işlem yapan kullanıcıları listeleme
ip_adresinden_kullanicilar(IP, KullaniciListesi) :-
    findall(Kullanici,
            islem(_, Kullanici, _, _, _, _, _, _, IP, _, _),
            TumKullanicilar),
    list_to_set(TumKullanicilar, KullaniciListesi),
    debug_message('IP adresinden işlem yapan kullanıcılar: ~w => ~w', [IP, KullaniciListesi]).

% 2) Aynı IP adresinden birden fazla kullanıcı işlem yapmış mı VE kısa süre faktörü
%    - Hem kullanıcı sayısına hem de zaman aralığına bakıyoruz.
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

/*
 * 3) Yardımcı kural: farkli_kullanicilar_kisa_sure/2
 *    => Liste halindeki (Zaman,Kullanici) çiftlerine bakar,
 *       ardışık iki işlemde Kullanıcı farklı VE zaman farkı <= Limit ise true döner.
 *
 * Örn: Limit = 5 => "5 birimden kısa zamanda kullanıcı değişimi oldu mu?"
 */
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

% 4) Belirtilen IP adresinden yapılan tüm işlemleri listeleme
ip_adresinden_islemler(IP, Islemler) :-
    findall((ID, Kullanici, Zaman, Cihaz, OdemeYontemi),
            islem(ID, Kullanici, _, Zaman, _, Cihaz, _, _, IP, OdemeYontemi, _),
            Islemler),
    debug_message('IP adresinden yapılan işlemler: ~w => ~w', [IP, Islemler]).

% 5) Basit test sorgusu
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
