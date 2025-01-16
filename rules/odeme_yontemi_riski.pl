:- module(odeme_yontemi_riski, [odeme_yontemi_kontrol/1]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug'). % Debug mesajları
:- use_module('../utils/alert'). % Alert mesajları

% Belirli bir ödeme yöntemiyle kısa süre içinde işlem yapan kullanıcıları kontrol et
odeme_yontemi_kontrol(OdemeYontemi) :-
    findall((Kullanici, Zaman), 
        islem(_, Kullanici, _, Zaman, _, _, _, _, _, OdemeYontemi, _), 
        Islemler),
    debug_message('Ödeme yöntemiyle ilgili işlemler: ~w => ~w', [OdemeYontemi, Islemler]),
    kontrol_et(Islemler).

% İşlemler arasında kısa süreli tekrar kontrolü
kontrol_et([(Kullanici1, Zaman1), (Kullanici2, Zaman2) | Kalan]) :-
    Kullanici1 \= Kullanici2,
    abs(Zaman2 - Zaman1) =< 10, % 10 birimlik zaman farkı
    alert_message('Kısa sürede farklı hesaplarda aynı ödeme yöntemi kullanıldı: ~w, ~w, Zamanlar: ~w, ~w', 
                  [Kullanici1, Kullanici2, Zaman1, Zaman2]),
    kontrol_et([(Kullanici2, Zaman2) | Kalan]).
kontrol_et([_]). % Tek işlem kaldığında kontrol sona erer
kontrol_et([]).  % İşlem listesi boşsa kontrol sona erer

% Test sorgusu
% odeme_yontemi_riski:odeme_yontemi_kontrol('Kredi Kartı').
% odeme_yontemi_riski:odeme_yontemi_kontrol('Banka Kartı').
% odeme_yontemi_riski:odeme_yontemi_kontrol('Havale').
% odeme_yontemi_riski:odeme_yontemi_kontrol('E-Cüzdan').
