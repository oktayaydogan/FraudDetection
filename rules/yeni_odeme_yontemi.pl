:- module(yeni_odeme_yontemi, [
    yeni_odeme_yontemi/1,
    test_yeni_odeme_yontemi/0
]).

:- use_module('../data/islem_verileri').  % Veriler dahil ediliyor
:- use_module('../utils/debug').          % Debug mesajları
:- use_module('../utils/alert').          % Alert (şüpheli) mesajları

% ----------------------------------------------------------------------
% 1) Kural 9: "Kullanıcı daha önce hiç kullanmadığı bir ödeme yöntemiyle
%    işlem yapıyorsa, bu durum şüpheli olarak işaretlenebilir."
%
%    Mantık: "Son işlemdeki ödeme yöntemi, önceki yöntemler listesinde yoksa" 
%    => "Şüpheli" (alert_message)
% ----------------------------------------------------------------------

% Kullanıcının (en) son işleminin ödeme yöntemini bulmak için,
% önceki yöntemleri de bir sete dönüştürüyoruz.
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
% 2) "kullanici_odeme_yontemleri/3"
%    => Kullanıcının tüm işlemlerini bul, Zaman (veya ID) alanına göre
%       en son işlemi ayıkla ve geriye kalanları "OncekiYontemler" olarak dön.
% ----------------------------------------------------------------------
kullanici_odeme_yontemleri(Kullanici, OncekiYontemler, SonOdemeYontemi) :-
    % Tüm (Zaman, OdemeYontemi) çiftlerini toplayalım
    findall((Zaman, OdemeYontemi),
            islem(_, Kullanici, _, Zaman, _, _, _, _, _, OdemeYontemi, _),
            TumYontemler),

    % Zaman'a göre büyükten küçüğe sıralayalım: en büyük Zaman = son işlem
    % (Zaman alanının gerçekten "yeni işlem > eski işlem" diye büyüdüğü varsayımı)
    sort(1, @>=, TumYontemler, [(_, SonOdemeYontemi)|Kalan]),

    % Kalan işlemlerin ödeme yöntemlerini toplayıp 'set'e dönüştürelim
    findall(W, member((_, W), Kalan), OncekiYontemList),
    list_to_set(OncekiYontemList, OncekiYontemler),

    debug_message('Kural 9: ~w kullanıcısının son yöntemi => ~w, önceki yöntemler => ~w',
                  [Kullanici, SonOdemeYontemi, OncekiYontemler]).

% ----------------------------------------------------------------------
% 3) Test Predikatı: test_yeni_odeme_yontemi/0
%    => Belirli kullanıcıları tek tek kontrol eder, Kural 9'a göre
%       "yeni yöntem" varsa "şüpheli" uyarısını gösterir.
% ----------------------------------------------------------------------
test_yeni_odeme_yontemi :-
    writeln('--- [TEST] Kural 9 (Yeni Ödeme Yöntemi) Kontrolü Başlıyor... ---'),
    set_debug(true),

    % Burada test etmek istediğiniz kullanıcı listesini yazabilirsiniz.
    % islem_verileri.pl içerisinde hangi kullanıcılar varsa onlardan seçin.
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
