:- module(yeni_cihaz, [
    yeni_cihaz_tespiti/1,
    test_yeni_cihaz/0
]).

:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug').         % Debug mesajları
:- use_module('../utils/alert').         % Alert mesajları

/* 
 * Kural 6: 
 * "Kullanıcı daha önce hiç kullanmadığı bir cihaz veya tarayıcı ile
 *  işlem yapıyorsa, bu işlem daha yüksek risk kategorisinde değerlendirilebilir."
 *
 * Örnek: Kullanıcı normalde Chrome tarayıcısını kullanıyorsa 
 *        ve bu sefer ilk defa Safari kullanarak işlem yapıyorsa, şüpheli olabilir.
 */

/* 
 * 1) Kullanıcının önceki cihaz(lar)ını bulma
 *    - "Zaman" alanına göre sıralayıp en büyük Zaman'a sahip işlem = son işlem olarak alıyoruz.
 *    - Geri kalan işlemlerin cihazlarını/ tarayıcılarını "OncekiCihazlar" listesine atıyoruz.
 */
kullanici_cihazlari(Kullanici, OncekiCihazlar, SonCihaz) :-
    findall((Zaman, Cihaz),
            islem(_, Kullanici, _, Zaman, _, Cihaz, _, _, _, _, _),
            TumIslemler),
    % Zaman’a göre büyükten küçüğe sırala (en büyük Zaman = son işlem):
    sort(1, @>=, TumIslemler, [(_, SonCihaz) | KalanIslemler]),
    % Kalanların cihazlarını toplayıp eşsiz yapalım:
    findall(C, member((_, C), KalanIslemler), CihazListesi),
    list_to_set(CihazListesi, OncekiCihazlar),

    debug_message('Kullanıcının cihaz listesi (eski): ~w => ~w', [Kullanici, OncekiCihazlar]),
    debug_message('Kullanıcının en son cihaz/tarayıcısı: ~w => ~w', [Kullanici, SonCihaz]).

/*
 * 2) Son işlemin cihazının (tarayıcısının) yeni olup olmadığını kontrol et
 *    - Eğer OncekiCihazlar = [] ise bu, kullanıcının ilk işlemi olabilir.
 *    - Eğer SonCihaz önceki listede yoksa "yeni cihaz/tarayıcı" => yüksek risk / şüpheli.
 *    - Diğer durumda "cihaz zaten kullanılmış" => normal.
 */
yeni_cihaz_tespiti(Kullanici) :-
    kullanici_cihazlari(Kullanici, OncekiCihazlar, SonCihaz),
    (   OncekiCihazlar = []
    ->  alert_message('Kural 6: Kullanıcının ilk kez cihaz/tarayıcı kullandığı tespit edildi (~w). Yüksek risk!', [SonCihaz])
    ;   (\+ member(SonCihaz, OncekiCihazlar)
        ->  alert_message('Kural 6: Kullanıcı yeni bir cihaz/tarayıcı kullandı (~w). Yüksek risk!', [SonCihaz])
        ;   debug_message('Kural 6: Son cihaz/tarayıcı zaten kullanılmış => ~w (normal)', [SonCihaz])
        )
    ).

/* 
 * 3) Test Predikatı
 *    - Belirli kullanıcılar için otomatik test yapıp, 
 *      "yeni cihaz" durumunun şüpheli olarak işaretlendiğini (veya normal) görürüz.
 */
test_yeni_cihaz :-
    writeln('--- [TEST] Kural 6: Yeni Cihaz/Tarayıcı Kontrolü Başlıyor... ---'),
    set_debug(true),

    /* 
     * Burada test etmek istediğiniz kullanıcıların listesini belirtebilirsiniz.
     * islem_verileri.pl dosyanızdaki kullanıcıları ekleyebilirsiniz.
     */
    forall(
        member(Kullanici, [
            kullanici1, kullanici2, kullanici3,
            kullanici4, kullanici5, kullanici6,
            kullanici7, kullanici8, kullanici9
        ]),
        (
            writeln('----------------------------------'),
            format('Kullanıcı: ~w~n', [Kullanici]),
            (   yeni_cihaz_tespiti(Kullanici)
            ->  format(' - Kural 6 kontrolü tamamlandı (yukarıdaki mesaja bakın).~n', [])
            ;   format(' - Kullanıcının işlemi yok veya kontrol başarısız oldu.~n', [])
            )
        )
    ),

    set_debug(false),
    writeln('----------------------------------'),
    writeln('--- [TEST] Tamamlandı. ---').
