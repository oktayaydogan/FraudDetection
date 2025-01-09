:- module(oturum_bilgisi_degisim_kontrolu, [bilgi_degisim_riski/1]).
:- use_module('../data/islem_verileri').

% Kullanıcı bilgisi değişikliği sonrası işlemleri kontrol eden kural
bilgi_degisim_riski(Kullanici) :-
    findall((DegisimZamani, ID), islem(ID, Kullanici, _, DegisimZamani, _, _, _, 'degisim', _, _, _), Degisimler),
    findall((IslemZamani, ID), islem(ID, Kullanici, _, IslemZamani, _, _, _, 'islem', _, _, _), Islemler),
    bilgi_degisim_kontrol(Degisimler, Islemler).

% Bilgi değişikliği sonrası işlem analizleri
bilgi_degisim_kontrol([(DegisimZamani, DegisimID) | KalanDegisimler], Islemler) :-
    member((IslemZamani, IslemID), Islemler),
    abs(IslemZamani - DegisimZamani) =< 5, % 5 birimlik zaman farkı
    writeln(['[DEBUG] Şüpheli işlem tespit edildi:', 'Bilgi Değişikliği ID:', DegisimID, '=> İşlem ID:', IslemID, 
             'Değişiklik Zamanı:', DegisimZamani, 'İşlem Zamanı:', IslemZamani]),
    bilgi_degisim_kontrol(KalanDegisimler, Islemler).
bilgi_degisim_kontrol([], _). % Değişiklikler tükendiğinde sonlandır

% Bilgi değişikliği sonrası işlem olup olmadığını kontrol etme
bilgi_degisim_kontrol(_, []) :-
    writeln('[DEBUG] Bilgi değişikliği sonrası işlem bulunamadı.').

% Test sorgusu
% oturum_bilgisi_degisim_kontrolu:bilgi_degisim_riski(kullanici1).
% oturum_bilgisi_degisim_kontrolu:bilgi_degisim_riski(kullanici4).
% oturum_bilgisi_degisim_kontrolu:bilgi_degisim_riski(kullanici5).
