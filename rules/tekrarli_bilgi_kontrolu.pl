:- module(tekrarli_bilgi_kontrolu, [
    tekrarli_bilgi/1,
    test_tekrarli_bilgi_kontrolu/0
]).

:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug').         % Debug mesajları
:- use_module('../utils/alert').         % Alert mesajları

/* 
 * KURAL 12:
 * "Aynı telefon numarası veya e-posta adresiyle birden fazla kullanıcı 
 *  hesabı oluşturulmuşsa ve bu hesaplardan ödeme yapılmaya çalışılıyorsa, 
 *  dolandırıcılık riski vardır."
 *
 * Örnek: Aynı telefon numarasıyla birçok hesap varsa ve bu hesaplardan 
 *        farklı işlemler yapılmaya çalışılıyorsa, bu durum şüpheli olabilir.
 */

/*
 * tekrarli_bilgi(Alan):
 *   - Alan, telefon numarası veya e-posta olabilir. 
 *   - Aynı Alan değerine sahip birden çok kullanıcı varsa alarm verilir.
 */
tekrarli_bilgi(Alan) :-
    (   var(Alan)
    ->  alert_message('Hata: Alan değeri belirtilmemiş.')
    ;   % Alan belirtilmişse devam edelim
        findall(
            Kullanici,
            % islem(_, Kullanici, ..., Alan) => 11. argüman e-posta/tel
            islem(_, Kullanici, _, _, _, _, _, _, _, _, Alan),
            KullaniciListesi
        ),
        list_to_set(KullaniciListesi, UnikKullanicilar),
        length(UnikKullanicilar, Say),
        (   Say > 1
        ->  % Birden fazla kullanıcı bu Alan'ı kullanıyorsa "dolandırıcılık riski"
            alert_message(
                'Kural 12: Aynı bilgi (Telefon/E-posta) birden fazla hesapta kullanılıyor, dolandırıcılık riski: ~w -> ~w',
                [Alan, UnikKullanicilar]
            )
        ;   debug_message(
                'Kural 12: Bilgi tekrarı tespit edilmedi (Alan: ~w, Kullanıcı sayısı: ~w)',
                [Alan, Say]
            )
        )
    ).

/*
 * test_tekrarli_bilgi_kontrolu/0:
 *   - Bazı örnek Alan değerleri ile tekrarli_bilgi/1 kuralını çağırıyoruz.
 *   - islem_verileri.pl kayıtlarınıza göre uyarı veya debug mesajı basılacaktır.
 */
test_tekrarli_bilgi_kontrolu :-
    writeln('--- [TEST] Kural 12: Tekrarlı Bilgi (E-posta/Tel) Kontrolü Başlıyor... ---'),
    set_debug(true),

    % Örnek testler - elinizdeki verilerle örtüşen e-posta / telefon alanlarını yazabilirsiniz.
    tekrarli_bilgi('user1@example.com'),
    tekrarli_bilgi('user2@example.com'),
    tekrarli_bilgi('user3@example.com'),
    tekrarli_bilgi('user4@example.com'),
    tekrarli_bilgi('+9055XXXXXX'),        % Örnek telefon numarası
    tekrarli_bilgi('invalid@example.com'), % Hiç kimsede olmayan bilgi
    tekrarli_bilgi(_),                    % Var(Alan) durumu (hata test)
    
    set_debug(false),
    writeln('--- [TEST] Tamamlandı. ---').
