:- module(islem_sikligi, [islem_sayisi/4, supheli_islem/3]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor

% Belirli bir zaman aralığında aynı kullanıcının işlem sayısını sayan kural
islem_sayisi(Kullanici, Baslangic, Bitis, Sayi) :-
    findall(1, (islem(Kullanici, _, Zaman, _), Zaman >= Baslangic, Zaman =< Bitis), Islemler),
    length(Islemler, Sayi).

% İşlem sayısı eşik değeri aşıyor mu? (Şüpheli durum)
supheli_islem(Kullanici, Baslangic, Bitis) :-
    esik_degeri(Eşik),
    islem_sayisi(Kullanici, Baslangic, Bitis, Sayi),
    Sayi > Eşik.

% Eşik değeri
esik_degeri(3).
