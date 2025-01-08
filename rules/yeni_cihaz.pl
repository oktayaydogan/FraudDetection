:- module(yeni_cihaz, [yeni_cihaz_tespiti/1]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor

% Kullanıcının önceki cihazlarını listeleme (son cihaz hariç)
kullanici_cihazlari(Kullanici, Cihazlar) :-
    findall((Cihaz, Zaman), islem(Kullanici, _, Zaman, _, Cihaz, _, _), TumIslemler),
    sort(1, @<, TumIslemler, SiraliIslemler), % İşlemleri zamana göre sırala
    findall((Cihaz, Zaman), member((Cihaz, Zaman), SiraliIslemler), TumCihazlar),
    list_to_set(TumCihazlar, Cihazlar),
    writeln(['Cihazlar ve ilk kullanım zamanları:', TumCihazlar]).

% Son işlemin cihazının yeni olup olmadığını kontrol eden kural
yeni_cihaz_tespiti(Kullanici) :-
    kullanici_cihazlari(Kullanici, OncekiCihazlar),
    findall((Cihaz, Zaman), islem(Kullanici, _, Zaman, _, Cihaz, _, _), TumIslemler),
    sort(2, @>=, TumIslemler, [(SonCihaz, _)|_]), % Son işlem cihazını bul
    (\+ member(SonCihaz, OncekiCihazlar) ->
        writeln(['Yeni cihaz tespit edildi:', SonCihaz]),
        true
    ;
        writeln(['Cihaz zaten kullanılmış:', SonCihaz]),
        fail
    ).
