:- module(islem_konumu, [konum_uyusmazligi/1]).
:- use_module('../data/islem_verileri').

% Kullanıcının önceki işlemlerindeki konumları listeleme
kullanici_konumlari(Kullanici, Konumlar) :-
    findall(Konum, islem(_, Kullanici, _, _, Konum, _, _, _, _, _, _), Konumlar),
    writeln(['[DEBUG] Kullanıcı konumları:', Kullanici, '=>', Konumlar]).

% Konum uyuşmazlığını kontrol eden kural
konum_uyusmazligi(Kullanici) :-
    kullanici_konumlari(Kullanici, Konumlar),
    list_to_set(Konumlar, UnikKonumlar), % Konumların benzersiz listesini oluştur
    length(UnikKonumlar, Say),
    writeln(['[DEBUG] Farklı konum sayısı:', Kullanici, '=>', Say]),
    Say > 1, % Birden fazla farklı konum varsa uyuşmazlık var
    writeln('[ALERT] Konum uyuşmazlığı tespit edildi!').

% Test sorgusu
% islem_konumu:konum_uyusmazligi(kullanici1).
% islem_konumu:konum_uyusmazligi(kullanici2).