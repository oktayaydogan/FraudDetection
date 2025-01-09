:- module(tekrarli_bilgi_kontrolu, [tekrarli_bilgi/1]).
:- use_module('../data/islem_verileri').

% Aynı bilgiyle kayıtlı kullanıcıları kontrol et
tekrarli_bilgi(Alan) :-
    findall(Kullanici, islem(_, Kullanici, _, _, _, _, _, _, _, _, Alan), KullaniciListesi),
    list_to_set(KullaniciListesi, UnikKullanicilar),
    length(UnikKullanicilar, Say),
    (Say > 1 ->
        writeln(['[ALERT] Aynı bilgi birden fazla hesapta kullanılıyor:', Alan, '->', UnikKullanicilar]);
        writeln(['[DEBUG] Bilgi tekrarı tespit edilmedi:', Alan])).

% Test sorgusu:
% tekrarli_bilgi_kontrolu:tekrarli_bilgi('user1@example.com').
% tekrarli_bilgi_kontrolu:tekrarli_bilgi('user2@example.com').
% tekrarli_bilgi_kontrolu:tekrarli_bilgi('user3@example.com').
