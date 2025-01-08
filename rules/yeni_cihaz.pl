:- module(yeni_cihaz, [yeni_cihaz_tespiti/1]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor

% Kullanıcının önceki cihazlarını listeleme
kullanici_cihazlari(Kullanici, Cihazlar) :-
    findall(Cihaz, islem(Kullanici, _, _, _, Cihaz, _), TumCihazlar),
    list_to_set(TumCihazlar, Cihazlar). % Eşsiz cihazlar

% Son işlemin cihazının yeni olup olmadığını kontrol eden kural
yeni_cihaz_tespiti(Kullanici) :-
    kullanici_cihazlari(Kullanici, Cihazlar),
    % Kullanıcının son işlem cihazını al
    findall((Cihaz, Zaman), islem(Kullanici, _, Zaman, _, Cihaz, _), TumIslemler),
    sort(2, @>=, TumIslemler, [(SonCihaz, _)|_]), % Son işlem cihazını bul
    \+ member(SonCihaz, Cihazlar). % Cihaz daha önce kullanılmış mı kontrol et
