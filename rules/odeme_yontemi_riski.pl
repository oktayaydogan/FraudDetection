:- module(odeme_yontemi_riski, [
    odeme_yontemi_kontrol/1,
    test_odeme_yontemi_riski/0
]).

:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug').         % Debug mesajları
:- use_module('../utils/alert').         % Alert mesajları

/*
 * KURAL 10:
 * "Aynı ödeme yöntemi (örneğin aynı kredi kartı) kısa süre içinde farklı 
 *  hesaplarda kullanılıyorsa, dolandırıcılık şüphesi oluşur."
 */

% ----------------------------------------------------------------------
% 1) Belirli bir ödeme yöntemiyle kısa süre içinde işlem yapan kullanıcıları kontrol et
% ----------------------------------------------------------------------
odeme_yontemi_kontrol(OdemeYontemi) :-
    % Bu ödeme yöntemiyle yapılan tüm işlemleri (Kullanici, Zaman) çiftleri olarak topla
    findall((Kullanici, Zaman),
        islem(_, Kullanici, _, Zaman, _, _, _, _, _, OdemeYontemi, _),
        Islemler),
    % İsterseniz Islemler'i kronolojik sıraya koymak için: 
    % sort(2, @=<, Islemler, IslemlerSirali), ve kontrol_et(IslemlerSirali) kullanabilirsiniz.
    debug_message('Ödeme yöntemiyle ilgili işlemler: ~w => ~w', [OdemeYontemi, Islemler]),
    kontrol_et(Islemler).

% ----------------------------------------------------------------------
% 2) İşlemler arasında kısa süreli tekrar kontrolü
%    - Arka arkaya gelen iki işlemde kullanıcı farkı varsa
%      ve zaman farkı 10 birimden az/eşitse alert veriyoruz.
% ----------------------------------------------------------------------
kontrol_et([(Kullanici1, Zaman1), (Kullanici2, Zaman2) | Kalan]) :-
    Kullanici1 \= Kullanici2,
    abs(Zaman2 - Zaman1) =< 10,  % 10 birimlik zaman farkı
    alert_message(
        'Kısa sürede farklı hesaplarda aynı ödeme yöntemi kullanıldı: ~w, ~w, Zamanlar: ~w, ~w',
        [Kullanici1, Kullanici2, Zaman1, Zaman2]
    ),
    kontrol_et([(Kullanici2, Zaman2) | Kalan]).
kontrol_et([(Kullanici1, Zaman1), (Kullanici2, Zaman2) | Kalan]) :-
    % Eğer ya kullanıcı aynı veya zaman farkı > 10 ise sadece debug mesajı verip devam ediyoruz
    debug_message('İşlem kontrol ediliyor -> (~w, ~w) -> (~w, ~w)',
                  [Kullanici1, Zaman1, Kullanici2, Zaman2]),
    kontrol_et([(Kullanici2, Zaman2) | Kalan]).
kontrol_et([_]) :-  % Tek işlem kaldığında kontrol sona erer
    debug_message('Tek işlem kaldı, kontrol sona erdi.').
kontrol_et([]) :-   % İşlem listesi boşsa kontrol sona erer
    debug_message('İşlem listesi boş, kontrol sona erdi.').

% ----------------------------------------------------------------------
% 3) Toplu test predikatı
%    - Bu örnek test, belirli ödeme yöntemleri üzerinde otomatik kontrol yapar.
% ----------------------------------------------------------------------
test_odeme_yontemi_riski :-
    writeln('--- [TEST] Kural 10: Aynı ödeme yöntemi kısa sürede farklı hesaplarda kullanımı kontrolü başlıyor... ---'),
    set_debug(true),

    % Buradaki listede, proje veritabanınızda (islem_verileri) yer alan yöntemleri sıralayabilirsiniz
    forall(
        member(OW, ['Kredi Kartı', 'Banka Kartı', 'Havale', 'E-Cüzdan']),
        (
            writeln('----------------------------------'),
            format('Ödeme yöntemi: ~w~n', [OW]),
            odeme_yontemi_kontrol(OW)
        )
    ),

    set_debug(false),
    writeln('----------------------------------'),
    writeln('--- [TEST] Tamamlandı. ---').
