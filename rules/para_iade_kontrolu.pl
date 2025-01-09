:- module(para_iade_kontrolu, [para_iade_riski/1]).
:- use_module('../data/islem_verileri').

% İşlemler arasında kısa sürede para iadesi kontrolü
para_iade_riski(Kullanici) :-
    % Kullanıcıya ait işlem zamanları ve tiplerini al
    findall((Zaman, Tip), islem(_, Kullanici, _, Zaman, _, _, _, Tip, _, _, _), Islemler),
    writeln(['[DEBUG] Kullanıcı işlemleri listesi:', Islemler]), % Debugging
    kontrol_et(Islemler).

% Para iade talebi analizi
kontrol_et([(Zaman1, 'iade'), (Zaman2, Tip2) | Kalan]) :-
    abs(Zaman2 - Zaman1) =< 5, % 5 birimlik zaman farkı
    writeln(['[ALERT] Kısa sürede para iade talebi tespit edildi:', Zaman1, Zaman2]), % Debugging
    kontrol_et([(Zaman2, Tip2) | Kalan]).
kontrol_et([(Zaman1, Tip1), (Zaman2, Tip2) | Kalan]) :-
    writeln(['[DEBUG] İşlem kontrol ediliyor:', Zaman1, Tip1, '->', Zaman2, Tip2]), % Debugging
    kontrol_et([(Zaman2, Tip2) | Kalan]).
kontrol_et([_]) :-
    writeln('[DEBUG] Tek işlem kaldı, kontrol sona erdi.'). % Debugging
kontrol_et([]) :-
    writeln('[DEBUG] İşlem listesi boş, kontrol sona erdi.'). % Debugging

% Test sorgusu:
% para_iade_kontrolu:para_iade_riski(kullanici3).
% para_iade_kontrolu:para_iade_riski(kullanici5).
% para_iade_kontrolu:para_iade_riski(kullanici1).
