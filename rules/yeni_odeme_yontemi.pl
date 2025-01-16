:- module(yeni_odeme_yontemi, [yeni_odeme_yontemi/1]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug'). % Debug mesajları
:- use_module('../utils/alert'). % Alert mesajları

% Kullanıcının önceki ödeme yöntemlerini listeleme (son yöntem hariç)
kullanici_odeme_yontemleri(Kullanici, OncekiYontemler, SonOdemeYontemi) :-
    findall((Zaman, OdemeYontemi), islem(_, Kullanici, _, Zaman, _, _, _, _, _, OdemeYontemi, _), TumOdemeYontemleri),
    sort(1, @>=, TumOdemeYontemleri, [(_, SonOdemeYontemi) | KalanOdemeYontemleri]), % En son ödeme yöntemini ayır
    findall(OdemeYontemi, member((_, OdemeYontemi), KalanOdemeYontemleri), TumYontemler),
    list_to_set(TumYontemler, OncekiYontemler), % Önceki ödeme yöntemlerini eşsiz hale getir
    debug_message('Kullanıcının ödeme yöntemleri listesi: ~w => ~w', [Kullanici, OncekiYontemler]).

% Son işlemin ödeme yönteminin yeni olup olmadığını kontrol et
yeni_odeme_yontemi(Kullanici) :-
    kullanici_odeme_yontemleri(Kullanici, OncekiYontemler, SonOdemeYontemi),
    (OncekiYontemler = [] ->
        alert_message('İlk kez ödeme yöntemi kullanılıyor: ~w', [SonOdemeYontemi]);
        (\+ member(SonOdemeYontemi, OncekiYontemler) ->
            alert_message('Yeni ödeme yöntemi tespit edildi: ~w', [SonOdemeYontemi]);
            debug_message('Ödeme yöntemi daha önce kullanılmış: ~w', [SonOdemeYontemi]))
    ).

% Test sorgusu:
% yeni_odeme_yontemi:yeni_odeme_yontemi(kullanici1).
% yeni_odeme_yontemi:yeni_odeme_yontemi(kullanici2).
% yeni_odeme_yontemi:yeni_odeme_yontemi(kullanici3).
