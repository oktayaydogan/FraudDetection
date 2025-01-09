:- module(yeni_odeme_yontemi, [yeni_odeme_yontemi/1]).
:- use_module('../data/islem_verileri').

% Kullanıcının önceki ödeme yöntemlerini listeleme
kullanici_odeme_yontemleri(Kullanici, Yontemler) :-
    findall(OdemeYontemi, islem(_, Kullanici, _, _, _, _, _, _, _, OdemeYontemi, _), TumYontemler),
    list_to_set(TumYontemler, Yontemler),
    writeln(['[DEBUG] Kullanıcının ödeme yöntemleri listesi:', Kullanici, '=>', Yontemler]). % Debugging

% Son işlemin ödeme yönteminin yeni olup olmadığını kontrol et
yeni_odeme_yontemi(Kullanici) :-
    kullanici_odeme_yontemleri(Kullanici, OncekiYontemler),
    findall((Zaman, OdemeYontemi), islem(_, Kullanici, _, Zaman, _, _, _, _, _, OdemeYontemi, _), TumOdemeYontemleri),
    sort(1, @>=, TumOdemeYontemleri, [(_, SonOdemeYontemi) | _]), % En son ödeme yöntemini bul
    (\+ member(SonOdemeYontemi, OncekiYontemler) ->
        writeln(['[ALERT] Yeni ödeme yöntemi tespit edildi:', SonOdemeYontemi]);
        writeln(['[DEBUG] Ödeme yöntemi daha önce kullanılmış:', SonOdemeYontemi])
    ).
