% para_iade_kontrolu.pl
%
% Açıklama:
%   Bu modül, kullanıcıların yaptığı ödeme işlemlerine dair kısa süre içinde
%   şikayet veya para iade talebi gelip gelmediğini kontrol eder. Bu tür durumlar,
%   dolandırıcılık şüphesi olarak değerlendirilebilir ve uyarı (alert_message/2)
%   verilir.
%
% Kullanım:
%   1) Prolog ortamında bu dosyayı yükleyin:
%      ?- [para_iade_kontrolu].
%
%   2) Predikatları aşağıdaki gibi test edebilirsiniz:
%      ?- para_iade_riski(kullanici1).
%      ?- test_para_iade_kontrolu.
%
% Gereksinimler:
%   - '../data/islem_verileri.pl' içinde islem/11 tanımı olması.
%     Örnek islem/11 yapısı:
%     islem(ID, Kullanici, Miktar, Zaman, Konum, Cihaz, _, Tip, _, _, _).
%   - '../utils/debug.pl' ve '../utils/alert.pl' dosyalarında debug_message/2,
%     set_debug/1, alert_message/2 vb. tanımlı olması.
%
% Sınırlamalar:
%   - Bu modül, sadece 'iade' tipindeki işlemleri analiz eder.
%   - Zaman farkı kontrolü için sabit bir eşik değeri (5 birim) kullanılır.
%
% Gelecek Geliştirmeler:
%   - Zaman farkı eşik değerini dinamik olarak değiştirebilme özelliği eklenebilir.
%   - Farklı işlem türleri için özelleştirilmiş eşik değerleri kullanılabilir.
%
% Modül Tanımı ve İhracı:
:- module(para_iade_kontrolu, [
    para_iade_riski/1,
    test_para_iade_kontrolu/0
]).

% Gerekli modüllerin dahil edilmesi
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug').         % Debug mesajları
:- use_module('../utils/alert').         % Alert mesajları

/* 
 * KURAL 13:
 * "Kullanıcı tarafından yapılan bir ödeme işlemine dair kısa süre içinde 
 *  şikayet veya para iade talebi gelirse, dolandırıcılık şüphesi doğurabilir."
 *
 * Örnek: Kullanıcı bir işlem yaptıktan kısa süre sonra 
 *        bu işlemi reddedip para iadesi talep ediyorsa şüpheli olabilir.
 */

% ----------------------------------------------------------------------
% para_iade_riski/1
%
% Açıklama:
%   Kullanıcının işlem listesini çekip Zaman + Tip çiftlerini alır ve
%   kronolojik olarak sıralar. Ardından, kısa sürede para iade talebi
%   olup olmadığını kontrol eder.
%
% Parametreler:
%   - Kullanici: Kontrol edilecek kullanıcı kimliği.
%
% Örnek Kullanım:
%   ?- para_iade_riski(kullanici1).
%   true.  % Eğer şüpheli işlem varsa
%   false. % Eğer şüpheli işlem yoksa
% ----------------------------------------------------------------------
para_iade_riski(Kullanici) :-
    findall((Zaman, Tip),
        islem(_, Kullanici, _, Zaman, _, _, _, Tip, _, _, _),
        IslemlerBulunmus
    ),
    sort(0, @=<, IslemlerBulunmus, IslemlerSirali),
    debug_message('Kullanıcı işlemleri (sıralı): ~w', [IslemlerSirali]),
    kontrol_et(IslemlerSirali).

% ----------------------------------------------------------------------
% kontrol_et/1
%
% Açıklama:
%   Ardışık iki işlem arasında, ilk işlem herhangi bir şey (islem, degisim vs.),
%   ikinci işlem 'iade' ise ve zaman farkı ≤5 ise uyarı verir.
%
% Parametreler:
%   - Islemler: (Zaman, Tip) çiftlerinden oluşan işlem listesi.
% ----------------------------------------------------------------------
kontrol_et([(Zaman1, Tip1), (Zaman2, 'iade') | Kalan]) :-
    % Sadece "Zaman2 - Zaman1 <= 5" ise şüpheli sayıyoruz (kısa sürede iade)
    ZamanFarki is Zaman2 - Zaman1,
    ZamanFarki =< 5,
    alert_message(
        'Kural 13: Kısa sürede para iade talebi tespit edildi => (Önce: ~w ~w, Sonra: ~w iade, Fark: ~w)',
        [Zaman1, Tip1, Zaman2, ZamanFarki]
    ),
    kontrol_et([(Zaman2, 'iade') | Kalan]).
kontrol_et([(Zaman1, Tip1), (Zaman2, Tip2) | Kalan]) :-
    % Herhangi iki işlem (örneğin "iade" yok veya zaman farkı > 5 vb.)
    debug_message(
        'İşlem kontrol ediliyor => (~w, ~w) -> (~w, ~w)',
        [Zaman1, Tip1, Zaman2, Tip2]
    ),
    kontrol_et([(Zaman2, Tip2) | Kalan]).
kontrol_et([_]) :-
    debug_message('Tek işlem kaldı, kontrol sona erdi.').
kontrol_et([]) :-
    debug_message('İşlem listesi boş, kontrol sona erdi.').

% ----------------------------------------------------------------------
% test_para_iade_kontrolu/0
%
% Açıklama:
%   Belirli kullanıcılar üzerinde toplu test yapar. Her kullanıcıda "iade"
%   şüpheli durum var mı diye kontrol eder.
%
% Örnek Kullanım:
%   ?- test_para_iade_kontrolu.
%
% Örnek Çıktı:
%   --- [TEST] Kural 13: Kısa Sürede Para İade Kontrolü Başlıyor... ---
%   ----------------------------------
%   Kullanıcı: kullanici1
%    - Kural 13 kontrolü tamamlandı (ayrıntılar yukarıda).
%   ----------------------------------
%   Kullanıcı: kullanici2
%    - Kullanıcının işlemi yok veya kontrol başarısız oldu.
%   ----------------------------------
%   --- [TEST] Tamamlandı. ---
% ----------------------------------------------------------------------
test_para_iade_kontrolu :-
    writeln('--- [TEST] Kural 13: Kısa Sürede Para İade Kontrolü Başlıyor... ---'),
    set_debug(true),

    % Burada test etmek istediğiniz kullanıcıları belirtebilirsiniz.
    forall(
        member(Kullanici, [
            kullanici1, kullanici2, kullanici3,
            kullanici5, kullanici8
        ]),
        (
            writeln('----------------------------------'),
            format('Kullanıcı: ~w~n', [Kullanici]),
            (   para_iade_riski(Kullanici)
            ->  format(' - Kural 13 kontrolü tamamlandı (ayrıntılar yukarıda).~n', [])
            ;   format(' - Kullanıcının işlemi yok veya kontrol başarısız oldu.~n', [])
            )
        )
    ),

    set_debug(false),
    writeln('----------------------------------'),
    writeln('--- [TEST] Tamamlandı. ---').