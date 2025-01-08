:- module(islem_konumu, [konum_uyusmazligi/1]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor

% Kullanıcının önceki işlemlerindeki konumları listeleme
kullanici_konumlari(Kullanici, Konumlar) :-
    findall(Konum, islem(Kullanici, _, _, Konum, _), Konumlar).

% Konum uyuşmazlığını kontrol eden kural
konum_uyusmazligi(Kullanici) :-
    kullanici_konumlari(Kullanici, Konumlar),
    list_to_set(Konumlar, UnikKonumlar), % Konumların eşsiz listesini oluştur
    length(UnikKonumlar, Say),
    Say > 1. % Birden fazla farklı konum varsa uyuşmazlık vardır
