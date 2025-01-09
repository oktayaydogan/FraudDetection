:- module(yeni_cihaz, [yeni_cihaz_tespiti/1]).
:- use_module('../data/islem_verileri').

% Kullanıcının önceki cihazlarını listeleme (son cihaz hariç)
kullanici_cihazlari(Kullanici, Cihazlar) :-
    findall(Cihaz, islem(_, Kullanici, _, _, _, Cihaz, _, _, _, _, _), TumCihazlar),
    list_to_set(TumCihazlar, Cihazlar),
    writeln(['[DEBUG] Kullanıcının cihaz listesi:', Kullanici, '=>', Cihazlar]). % Debugging

% Son işlemin cihazının yeni olup olmadığını kontrol eden kural
yeni_cihaz_tespiti(Kullanici) :-
    kullanici_cihazlari(Kullanici, OncekiCihazlar),
    findall((Zaman, Cihaz), islem(_, Kullanici, _, Zaman, _, Cihaz, _, _, _, _, _), TumIslemler),
    sort(1, @>=, TumIslemler, [(_, SonCihaz) | _]), % En son cihazı bul
    (\+ member(SonCihaz, OncekiCihazlar) ->
        writeln(['[ALERT] Yeni cihaz tespit edildi:', SonCihaz]);
        writeln(['[DEBUG] Cihaz zaten kullanılmış:', SonCihaz])
    ).

% Test sorgusu:
% yeni_cihaz:yeni_cihaz_tespiti(kullanici1).
% yeni_cihaz:yeni_cihaz_tespiti(kullanici2).
% yeni_cihaz:yeni_cihaz_tespiti(kullanici3).