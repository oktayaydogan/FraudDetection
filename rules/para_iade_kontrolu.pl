:- module(para_iade_kontrolu, [
    para_iade_riski/1,
    test_para_iade_kontrolu/0
]).

:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug').         % Debug mesajları
:- use_module('../utils/alert').         % Alert mesajları

/* 
 * KURAL 13:
 * "Kullanıcı tarafından yapılan bir ödeme işlemine dair kısa süre içinde 
 *  şikayet veya para iade talebi gelirse, dolandırıcılık şüphesi doğurabilir."
 *
 * Örnek: Kullanıcı bir işlem yaptıktan kısa süre sonra 
 *        bu işlemi reddedip para iadesi talep ediyorsa şüpheli olabilir.
 */

/* 
 * 1) Kullanıcının işlem listesini çekip Zaman + Tip çiftlerini alalım.
 *    Ardından artan sırada sıralayalım ki kronolojik olarak inceleyebilelim.
 */
para_iade_riski(Kullanici) :-
    findall((Zaman, Tip),
        islem(_, Kullanici, _, Zaman, _, _, _, Tip, _, _, _),
        IslemlerBulunmus
    ),
    sort(0, @=<, IslemlerBulunmus, IslemlerSirali),
    debug_message('Kullanıcı işlemleri (sıralı): ~w', [IslemlerSirali]),
    kontrol_et(IslemlerSirali).

/*
 * 2) kontrol_et/1
 *    Ardışık iki işlem arasında: 
 *    - İlk işlem herhangi bir şey (islem, degisim vs.), 
 *    - İkinci işlem 'iade'
 *    => Zaman farkı <= 5 ise "Kısa sürede iade talebi" uyarısı verilir.
 */
kontrol_et([(Zaman1, Tip1), (Zaman2, 'iade') | Kalan]) :-
    % Sadece "Zaman2 - Zaman1 <= 5" ise şüpheli sayıyoruz (kısa sürede iade)
    ZamanFarki is Zaman2 - Zaman1,
    ZamanFarki =< 5,
    alert_message(
        'Kural 13: Kısa sürede para iade talebi tespit edildi => (Önce: ~w ~w, Sonra: ~w iade, Fark: ~w)',
        [Zaman1, Tip1, Zaman2, ZamanFarki]
    ),
    kontrol_et([(Zaman2, 'iade') | Kalan]).
kontrol_et([(Zaman1, Tip1), (Zaman2, Tip2) | Kalan]) :-
    % Herhangi iki işlem (örneğin "iade" yok veya zaman farkı > 5 vb.)
    debug_message(
        'İşlem kontrol ediliyor => (~w, ~w) -> (~w, ~w)',
        [Zaman1, Tip1, Zaman2, Tip2]
    ),
    kontrol_et([(Zaman2, Tip2) | Kalan]).
kontrol_et([_]) :-
    debug_message('Tek işlem kaldı, kontrol sona erdi.').
kontrol_et([]) :-
    debug_message('İşlem listesi boş, kontrol sona erdi.').

/*
 * 3) Test predikatı: Belirli kullanıcıların Kural 13'e göre 
 *    iade riskini kontrol edelim.
 */
test_para_iade_kontrolu :-
    writeln('--- [TEST] Kural 13: Kısa Sürede Para İade Kontrolü Başlıyor... ---'),
    set_debug(true),

    /* Burada test etmek istediğiniz kullanıcıları belirtebilirsiniz.
       islem_verileri.pl dosyanızda hangi kullanıcılar varsa ekleyin. */
    forall(
        member(Kullanici, [
            kullanici1, kullanici2, kullanici3,
            kullanici5, kullanici8
        ]),
        (
            writeln('----------------------------------'),
            format('Kullanıcı: ~w~n', [Kullanici]),
            (   para_iade_riski(Kullanici)
            ->  format(' - Kural 13 kontrolü tamamlandı (ayrıntılar yukarıda).~n', [])
            ;   format(' - Kullanıcının işlemi yok veya kontrol başarısız oldu.~n', [])
            )
        )
    ),

    set_debug(false),
    writeln('----------------------------------'),
    writeln('--- [TEST] Tamamlandı. ---').
