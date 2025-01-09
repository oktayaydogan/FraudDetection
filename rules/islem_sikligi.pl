:- module(islem_sikligi, [islem_sayisi/4, supheli_islem/3]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor

% Belirli bir zaman aralığında aynı kullanıcının işlem sayısını sayan kural
islem_sayisi(Kullanici, Baslangic, Bitis, Sayi) :-
    findall(1, 
        (islem(_, Kullanici, _, Zaman, _, _, _, _, _, _, _), 
         Zaman >= Baslangic, Zaman =< Bitis), 
        Islemler),
    length(Islemler, Sayi),
    writeln(['[DEBUG] İşlem sayısı hesaplandı:', Kullanici, '=>', Sayi]). % Debugging

% İşlem sayısı eşik değeri aşıyor mu? (Şüpheli durum)
supheli_islem(Kullanici, Baslangic, Bitis) :-
    esik_degeri(Eşik),
    islem_sayisi(Kullanici, Baslangic, Bitis, Sayi),
    writeln(['[DEBUG] Eşik değeri:', Eşik, 'İşlem sayısı:', Sayi]), % Debugging
    Sayi > Eşik,
    writeln('[ALERT] Şüpheli işlem sayısı tespit edildi!').

% Eşik değeri
esik_degeri(3). % Maksimum izin verilen işlem sayısı

% Test sorgusu
% islem_sikligi:islem_sayisi(kullanici1, 0, 24, Sayi).
% islem_sikligi:islem_sayisi(kullanici2, 10, 30, Sayi).
% islem_sikligi:supheli_islem(kullanici1, 0, 24).
% islem_sikligi:supheli_islem(kullanici3, 5, 20).
