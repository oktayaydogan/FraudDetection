:- module(tekrarli_bilgi_kontrolu, [tekrarli_bilgi/1, test/0]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug'). % Debug mesajları
:- use_module('../utils/alert'). % Alert mesajları

% Aynı bilgiyle kayıtlı kullanıcıları kontrol et
tekrarli_bilgi(Alan) :-
    (var(Alan) ->
        alert_message('Hata: Alan değeri belirtilmemiş.');
        findall(Kullanici, islem(_, Kullanici, _, _, _, _, _, _, _, _, Alan), KullaniciListesi),
        list_to_set(KullaniciListesi, UnikKullanicilar), % Eşsiz kullanıcıları belirle
        length(UnikKullanicilar, Say),
        (Say > 1 ->
            alert_message('Aynı bilgi birden fazla hesapta kullanılıyor: ~w -> ~w', [Alan, UnikKullanicilar]);
            debug_message('Bilgi tekrarı tespit edilmedi: ~w', [Alan]))).

% Test kolaylığı için wrapper
test :-
    writeln('Test: tekrarli_bilgi kontrolü başlıyor...'),
    set_debug(true),
    tekrarli_bilgi('user1@example.com'),
    tekrarli_bilgi('user2@example.com'),
    tekrarli_bilgi('user3@example.com'),
    tekrarli_bilgi('user4@example.com'),
    tekrarli_bilgi('invalid@example.com'), % Geçersiz veya eşleşmeyen alan
    set_debug(false),
    writeln('Test tamamlandı.').
