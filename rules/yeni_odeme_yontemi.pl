% yeni_odeme_yontemi.pl
%
% Açıklama:
%   Bu modül, kullanıcının daha önce hiç kullanmadığı bir ödeme yöntemiyle
%   işlem yapıp yapmadığını kontrol eder. Bu tür durumlar, dolandırıcılık şüphesi
%   olarak değerlendirilebilir ve uyarı (alert_message/2) verilir.
%
% Kullanım:
%   1) Prolog ortamında bu dosyayı yükleyin:
%      ?- [yeni_odeme_yontemi].
%
%   2) Predikatları aşağıdaki gibi test edebilirsiniz:
%      ?- yeni_odeme_yontemi(kullanici1).
%      ?- test_yeni_odeme_yontemi.
%
% Gereksinimler:
%   - '../data/islem_verileri.pl' içinde islem/11 tanımı olması.
%     Örnek islem/11 yapısı:
%     islem(ID, Kullanici, Miktar, Zaman, Konum, Cihaz, _, _, _, OdemeYontemi, _).
%   - '../utils/debug.pl' ve '../utils/alert.pl' dosyalarında debug_message/2,
%     set_debug/1, alert_message/2 vb. tanımlı olması.
%
% Sınırlamalar:
%   - Bu modül, sadece 'OdemeYontemi' alanını kullanarak ödeme yöntemlerini analiz eder.
%   - Kullanıcının ilk işlemi için özel bir durum tanımlanmıştır.
%
% Gelecek Geliştirmeler:
%   - Ödeme yöntemlerinin geçerliliğini kontrol eden bir doğrulama mekanizması eklenebilir.
%   - Farklı ödeme yöntemleri için özelleştirilmiş risk puanları kullanılabilir.
%
% Modül Tanımı ve İhracı:
:- module(yeni_odeme_yontemi, [
    yeni_odeme_yontemi/1,
    test_yeni_odeme_yontemi/0
]).

% Gerekli modüllerin dahil edilmesi
:- use_module('../data/islem_verileri').  % Veriler dahil ediliyor
:- use_module('../utils/debug').          % Debug mesajları
:- use_module('../utils/alert').          % Alert (şüpheli) mesajları

/* 
 * KURAL 9: 
 * "Kullanıcı daha önce hiç kullanmadığı bir ödeme yöntemiyle
 *  işlem yapıyorsa, bu durum şüpheli olarak işaretlenebilir."
 *
 * Mantık: "Son işlemdeki ödeme yöntemi, önceki yöntemler listesinde yoksa" 
 * => "Şüpheli" (alert_message)
 */

% ----------------------------------------------------------------------
% yeni_odeme_yontemi/1
%
% Açıklama:
%   Kullanıcının son işleminde kullandığı ödeme yönteminin yeni olup
%   olmadığını kontrol eder. Eğer yeni bir ödeme yöntemi kullanılmışsa,
%   uyarı verir.
%
% Parametreler:
%   - Kullanici: Kontrol edilecek kullanıcı kimliği.
%
% Örnek Kullanım:
%   ?- yeni_odeme_yontemi(kullanici1).
%   true.  % Eğer şüpheli işlem varsa
%   false. % Eğer şüpheli işlem yoksa
% ----------------------------------------------------------------------
yeni_odeme_yontemi(Kullanici) :-
    kullanici_odeme_yontemleri(Kullanici, OncekiYontemler, SonOdemeYontemi),
    (   OncekiYontemler = []
    ->  % Hiç önceki yöntemi yok => Kullanıcının ilk işlemi olabilir
        debug_message('Kural 9: Kullanıcının ilk ödeme yöntemi ~w, şüpheli durum olarak değerlendirilebilir.', [SonOdemeYontemi])
    ;   % Daha önce yöntemler var. Son yöntem o listede yoksa => şüpheli
        (\+ member(SonOdemeYontemi, OncekiYontemler)
        ->  alert_message('Kural 9: Şüpheli durum - yeni ödeme yöntemi => ~w (Öncekiler: ~w)', [SonOdemeYontemi, OncekiYontemler])
        ;   debug_message('Kural 9: Son ödeme yöntemi (~w) daha önce kullanılmış, normal.', [SonOdemeYontemi])
        )
    ).

% ----------------------------------------------------------------------
% kullanici_odeme_yontemleri/3
%
% Açıklama:
%   Kullanıcının tüm işlemlerini bulur, Zaman alanına göre sıralar ve
%   en son işlemin ödeme yöntemini ile önceki yöntemleri döndürür.
%
% Parametreler:
%   - Kullanici:        Kontrol edilecek kullanıcı kimliği.
%   - OncekiYontemler:  Kullanıcının daha önce kullandığı ödeme yöntemlerinin listesi (çıktı).
%   - SonOdemeYontemi:  Kullanıcının en son kullandığı ödeme yöntemi (çıktı).
%
% Örnek Kullanım:
%   ?- kullanici_odeme_yontemleri(kullanici1, OncekiYontemler, SonOdemeYontemi).
%   OncekiYontemler = ['Kredi Kartı', 'Banka Kartı'], SonOdemeYontemi = 'E-Cüzdan'.
% ----------------------------------------------------------------------
kullanici_odeme_yontemleri(Kullanici, OncekiYontemler, SonOdemeYontemi) :-
    % Tüm (Zaman, OdemeYontemi) çiftlerini toplayalım
    findall((Zaman, OdemeYontemi),
            islem(_, Kullanici, _, Zaman, _, _, _, _, _, OdemeYontemi, _),
            TumYontemler),

    % Zaman'a göre büyükten küçüğe sıralayalım: en büyük Zaman = son işlem
    sort(1, @>=, TumYontemler, [(_, SonOdemeYontemi)|Kalan]),

    % Kalan işlemlerin ödeme yöntemlerini toplayıp 'set'e dönüştürelim
    findall(W, member((_, W), Kalan), OncekiYontemList),
    list_to_set(OncekiYontemList, OncekiYontemler),

    debug_message('Kural 9: ~w kullanıcısının son yöntemi => ~w, önceki yöntemler => ~w',
                  [Kullanici, SonOdemeYontemi, OncekiYontemler]).

% ----------------------------------------------------------------------
% test_yeni_odeme_yontemi/0
%
% Açıklama:
%   Belirli kullanıcılar üzerinde otomatik test yapar. Her kullanıcıda "yeni ödeme yöntemi"
%   durumunun şüpheli olarak işaretlenip işaretlenmediğini kontrol eder.
%
% Örnek Kullanım:
%   ?- test_yeni_odeme_yontemi.
%
% Örnek Çıktı:
%   --- [TEST] Kural 9 (Yeni Ödeme Yöntemi) Kontrolü Başlıyor... ---
%   ----------------------------------
%   Kullanıcı: kullanici1
%    - Kural 9 kontrolü tamamlandı (yukarıdaki mesajlara bakın).
%   ----------------------------------
%   Kullanıcı: kullanici2
%    - Kullanıcının hiç işlemi yok veya kontrol başarısız oldu.
%   ----------------------------------
%   --- [TEST] Tamamlandı. ---
% ----------------------------------------------------------------------
test_yeni_odeme_yontemi :-
    writeln('--- [TEST] Kural 9 (Yeni Ödeme Yöntemi) Kontrolü Başlıyor... ---'),
    set_debug(true),

    % Burada test etmek istediğiniz kullanıcı listesini yazabilirsiniz.
    forall(
        member(Kullanici, [
            kullanici1, kullanici2, kullanici3,
            kullanici4, kullanici5, kullanici6,
            kullanici7, kullanici8, kullanici9
        ]),
        (
            writeln('----------------------------------'),
            format('Kullanıcı: ~w~n', [Kullanici]),
            (   yeni_odeme_yontemi(Kullanici)
            ->  format(' - Kural 9 kontrolü tamamlandı (yukarıdaki mesajlara bakın).~n', [])
            ;   format(' - Kullanıcının hiç işlemi yok veya kontrol başarısız oldu.~n', [])
            )
        )
    ),

    set_debug(false),
    writeln('----------------------------------'),
    writeln('--- [TEST] Tamamlandı. ---').