:- module(islem_sikligi, [islem_sayisi/4, supheli_islem/3]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug'). % Debug mesajları
:- use_module('../utils/alert'). % Alert mesajları

% Belirli bir zaman aralığında aynı kullanıcının işlem sayısını sayan kural
islem_sayisi(Kullanici, Baslangic, Bitis, Sayi) :-
    findall(1, 
        (islem(_, Kullanici, _, Zaman, _, _, _, _, _, _, _), 
         Zaman >= Baslangic, Zaman =< Bitis), 
        Islemler),
    length(Islemler, Sayi),
    debug_message('İşlem sayısı hesaplandı: ~w => ~w', [Kullanici, Sayi]).

% İşlem sayısı eşik değeri aşıyor mu? (Şüpheli durum)
supheli_islem(Kullanici, Baslangic, Bitis) :-
    esik_degeri(Esik),
    islem_sayisi(Kullanici, Baslangic, Bitis, Sayi),
    debug_message('Eşik değeri: ~w, İşlem sayısı: ~w', [Esik, Sayi]),
    (Sayi > Esik ->
        alert_message('Şüpheli işlem sayısı tespit edildi: Kullanıcı: ~w, İşlem Sayısı: ~w', [Kullanici, Sayi]);
        debug_message('İşlem sayısı normal: Kullanıcı: ~w, İşlem Sayısı: ~w', [Kullanici, Sayi])
    ).

% Eşik değeri
esik_degeri(3). % Maksimum izin verilen işlem sayısı

% Test sorgusu
% islem_sikligi:islem_sayisi(kullanici1, 0, 24, Sayi).
% islem_sikligi:islem_sayisi(kullanici2, 10, 30, Sayi).
% islem_sikligi:supheli_islem(kullanici1, 0, 24).
% islem_sikligi:supheli_islem(kullanici3, 5, 20).
