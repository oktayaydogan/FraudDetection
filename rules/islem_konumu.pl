:- module(islem_konumu, [konum_uyusmazligi/1]).
:- use_module('../data/islem_verileri').

% Kullanıcının önceki işlemlerindeki konumları listeleme
kullanici_konumlari(Kullanici, Konumlar) :-
    findall(Konum, islem(Kullanici, _, _, Konum, _, _), Konumlar),
    writeln(['Kullanıcı konumları:', Kullanici, '=>', Konumlar]).

% Konum uyuşmazlığını kontrol eden kural
konum_uyusmazligi(Kullanici) :-
    kullanici_konumlari(Kullanici, Konumlar),
    list_to_set(Konumlar, UnikKonumlar),
    length(UnikKonumlar, Say),
    writeln(['Farklı konum sayısı:', Kullanici, '=>', Say]),
    Say > 1.
