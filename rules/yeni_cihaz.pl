:- module(yeni_cihaz, [yeni_cihaz_tespiti/1]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug'). % Debug mesajları
:- use_module('../utils/alert'). % Alert mesajları

% Kullanıcının önceki cihazlarını listeleme (son cihaz hariç)
kullanici_cihazlari(Kullanici, OncekiCihazlar, SonCihaz) :-
    findall((Zaman, Cihaz), islem(_, Kullanici, _, Zaman, _, Cihaz, _, _, _, _, _), TumIslemler),
    sort(1, @>=, TumIslemler, [(_, SonCihaz) | KalanIslemler]), % En son cihazı bul
    findall(Cihaz, member((_, Cihaz), KalanIslemler), TumCihazlar),
    list_to_set(TumCihazlar, OncekiCihazlar), % Önceki cihazları eşsiz hale getir
    debug_message('Kullanıcının cihaz listesi: ~w => ~w', [Kullanici, OncekiCihazlar]).

% Son işlemin cihazının yeni olup olmadığını kontrol eden kural
yeni_cihaz_tespiti(Kullanici) :-
    kullanici_cihazlari(Kullanici, OncekiCihazlar, SonCihaz),
    (OncekiCihazlar = [] ->
        alert_message('İlk kez cihaz tespit ediliyor: ~w', [SonCihaz]);
        (\+ member(SonCihaz, OncekiCihazlar) ->
            alert_message('Yeni cihaz tespit edildi: ~w', [SonCihaz]);
            debug_message('Cihaz zaten kullanılmış: ~w', [SonCihaz]))
    ).

% Test sorgusu:
% yeni_cihaz:yeni_cihaz_tespiti(kullanici1).
% yeni_cihaz:yeni_cihaz_tespiti(kullanici2).
% yeni_cihaz:yeni_cihaz_tespiti(kullanici3).
