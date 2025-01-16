:- module(oturum_bilgisi_degisim_kontrolu, [bilgi_degisim_riski/1]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug'). % Debug mesajları
:- use_module('../utils/alert'). % Alert mesajları

% Kullanıcı bilgisi değişikliği sonrası işlemleri kontrol eden kural
bilgi_degisim_riski(Kullanici) :-
    findall((DegisimZamani, DegisimID), 
        islem(DegisimID, Kullanici, _, DegisimZamani, _, _, _, 'degisim', _, _, _), Degisimler),
    findall((IslemZamani, IslemID), 
        islem(IslemID, Kullanici, _, IslemZamani, _, _, _, 'islem', _, _, _), Islemler),
    debug_message('Bilgi değişiklikleri: ~w', [Degisimler]),
    debug_message('İşlemler: ~w', [Islemler]),
    bilgi_degisim_kontrol(Degisimler, Islemler).

% Bilgi değişikliği sonrası işlem analizleri
bilgi_degisim_kontrol([(DegisimZamani, DegisimID) | KalanDegisimler], Islemler) :-
    member((IslemZamani, IslemID), Islemler),
    abs(IslemZamani - DegisimZamani) =< 5, % 5 birimlik zaman farkı
    alert_message('Şüpheli işlem tespit edildi: Bilgi Değişikliği ID: ~w => İşlem ID: ~w, Değişiklik Zamanı: ~w, İşlem Zamanı: ~w', 
                  [DegisimID, IslemID, DegisimZamani, IslemZamani]),
    bilgi_degisim_kontrol(KalanDegisimler, Islemler).
bilgi_degisim_kontrol([], _) :- 
    debug_message('Bilgi değişikliği sonrası işlem kontrolü tamamlandı. Tüm değişiklikler incelendi.').

% Bilgi değişikliği sonrası işlem olup olmadığını kontrol etme
bilgi_degisim_kontrol(_, []) :-
    debug_message('Bilgi değişikliği sonrası işlem bulunamadı.').

% Test sorgusu
% oturum_bilgisi_degisim_kontrolu:bilgi_degisim_riski(kullanici1).
% oturum_bilgisi_degisim_kontrolu:bilgi_degisim_riski(kullanici4).
% oturum_bilgisi_degisim_kontrolu:bilgi_degisim_riski(kullanici5).
