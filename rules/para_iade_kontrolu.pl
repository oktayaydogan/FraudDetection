:- module(para_iade_kontrolu, [para_iade_riski/1]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug'). % Debug mesajları
:- use_module('../utils/alert'). % Alert mesajları

% İşlemler arasında kısa sürede para iadesi kontrolü
para_iade_riski(Kullanici) :-
    % Kullanıcıya ait işlem zamanları ve tiplerini al
    findall((Zaman, Tip), islem(_, Kullanici, _, Zaman, _, _, _, Tip, _, _, _), Islemler),
    debug_message('Kullanıcı işlemleri listesi: ~w', [Islemler]),
    kontrol_et(Islemler).

% Para iade talebi analizi
kontrol_et([(Zaman1, 'iade'), (Zaman2, Tip2) | Kalan]) :-
    abs(Zaman2 - Zaman1) =< 5, % 5 birimlik zaman farkı
    alert_message('Kısa sürede para iade talebi tespit edildi: Zaman1: ~w, Zaman2: ~w', [Zaman1, Zaman2]),
    kontrol_et([(Zaman2, Tip2) | Kalan]).
kontrol_et([(Zaman1, Tip1), (Zaman2, Tip2) | Kalan]) :-
    debug_message('İşlem kontrol ediliyor: Zaman1: ~w, Tip1: ~w -> Zaman2: ~w, Tip2: ~w', [Zaman1, Tip1, Zaman2, Tip2]),
    kontrol_et([(Zaman2, Tip2) | Kalan]).
kontrol_et([_]) :-
    debug_message('Tek işlem kaldı, kontrol sona erdi.').
kontrol_et([]) :-
    debug_message('İşlem listesi boş, kontrol sona erdi.').

% Test sorgusu:
% para_iade_kontrolu:para_iade_riski(kullanici3).
% para_iade_kontrolu:para_iade_riski(kullanici5).
% para_iade_kontrolu:para_iade_riski(kullanici1).
