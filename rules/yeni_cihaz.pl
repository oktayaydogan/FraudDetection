% yeni_cihaz.pl
%
% Açıklama:
%   Bu modül, kullanıcının daha önce hiç kullanmadığı bir cihaz veya tarayıcı ile
%   işlem yapıp yapmadığını kontrol eder. Bu tür durumlar, dolandırıcılık şüphesi
%   olarak değerlendirilebilir ve uyarı (alert_message/2) verilir.
%
% Kullanım:
%   1) Prolog ortamında bu dosyayı yükleyin:
%      ?- [yeni_cihaz].
%
%   2) Predikatları aşağıdaki gibi test edebilirsiniz:
%      ?- yeni_cihaz_tespiti(kullanici1).
%      ?- test_yeni_cihaz.
%
% Gereksinimler:
%   - '../data/islem_verileri.pl' içinde islem/11 tanımı olması.
%     Örnek islem/11 yapısı:
%     islem(ID, Kullanici, Miktar, Zaman, Konum, Cihaz, _, _, _, _, _).
%   - '../utils/debug.pl' ve '../utils/alert.pl' dosyalarında debug_message/2,
%     set_debug/1, alert_message/2 vb. tanımlı olması.
%
% Sınırlamalar:
%   - Bu modül, sadece 'Cihaz' alanını kullanarak cihaz/tarayıcı bilgilerini analiz eder.
%   - Kullanıcının ilk işlemi için özel bir durum tanımlanmıştır.
%
% Gelecek Geliştirmeler:
%   - Cihaz/tarayıcı bilgilerinin geçerliliğini kontrol eden bir doğrulama mekanizması eklenebilir.
%   - Farklı cihaz türleri için özelleştirilmiş risk puanları kullanılabilir.
%
% Modül Tanımı ve İhracı:
:- module(yeni_cihaz, [
    yeni_cihaz_tespiti/1,
    test_yeni_cihaz/0
]).

% Gerekli modüllerin dahil edilmesi
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug').         % Debug mesajları
:- use_module('../utils/alert').         % Alert mesajları

/* 
 * KURAL 6: 
 * "Kullanıcı daha önce hiç kullanmadığı bir cihaz veya tarayıcı ile
 *  işlem yapıyorsa, bu işlem daha yüksek risk kategorisinde değerlendirilebilir."
 *
 * Örnek: Kullanıcı normalde Chrome tarayıcısını kullanıyorsa 
 *        ve bu sefer ilk defa Safari kullanarak işlem yapıyorsa, şüpheli olabilir.
 */

% ----------------------------------------------------------------------
% kullanici_cihazlari/3
%
% Açıklama:
%   Kullanıcının önceki cihaz(lar)ını ve son cihazını bulur.
%
% Parametreler:
%   - Kullanici:        Kontrol edilecek kullanıcı kimliği.
%   - OncekiCihazlar:   Kullanıcının daha önce kullandığı cihazların listesi (çıktı).
%   - SonCihaz:         Kullanıcının en son kullandığı cihaz (çıktı).
%
% Örnek Kullanım:
%   ?- kullanici_cihazlari(kullanici1, OncekiCihazlar, SonCihaz).
%   OncekiCihazlar = ['Chrome', 'Firefox'], SonCihaz = 'Safari'.
% ----------------------------------------------------------------------
kullanici_cihazlari(Kullanici, OncekiCihazlar, SonCihaz) :-
    findall((Zaman, Cihaz),
            islem(_, Kullanici, _, Zaman, _, Cihaz, _, _, _, _, _),
            TumIslemler),
    % Zaman’a göre büyükten küçüğe sırala (en büyük Zaman = son işlem):
    sort(1, @>=, TumIslemler, [(_, SonCihaz) | KalanIslemler]),
    % Kalanların cihazlarını toplayıp eşsiz yapalım:
    findall(C, member((_, C), KalanIslemler), CihazListesi),
    list_to_set(CihazListesi, OncekiCihazlar),

    debug_message('Kullanıcının cihaz listesi (eski): ~w => ~w', [Kullanici, OncekiCihazlar]),
    debug_message('Kullanıcının en son cihaz/tarayıcısı: ~w => ~w', [Kullanici, SonCihaz]).

% ----------------------------------------------------------------------
% yeni_cihaz_tespiti/1
%
% Açıklama:
%   Kullanıcının son işleminde kullandığı cihazın/tarayıcının yeni olup
%   olmadığını kontrol eder. Eğer yeni bir cihaz/tarayıcı kullanılmışsa,
%   uyarı verir.
%
% Parametreler:
%   - Kullanici: Kontrol edilecek kullanıcı kimliği.
%
% Örnek Kullanım:
%   ?- yeni_cihaz_tespiti(kullanici1).
%   true.  % Eğer şüpheli işlem varsa
%   false. % Eğer şüpheli işlem yoksa
% ----------------------------------------------------------------------
yeni_cihaz_tespiti(Kullanici) :-
    kullanici_cihazlari(Kullanici, OncekiCihazlar, SonCihaz),
    (   OncekiCihazlar = []
    ->  alert_message('Kural 6: Kullanıcının ilk kez cihaz/tarayıcı kullandığı tespit edildi (~w). Yüksek risk!', [SonCihaz])
    ;   (\+ member(SonCihaz, OncekiCihazlar)
        ->  alert_message('Kural 6: Kullanıcı yeni bir cihaz/tarayıcı kullandı (~w). Yüksek risk!', [SonCihaz])
        ;   debug_message('Kural 6: Son cihaz/tarayıcı zaten kullanılmış => ~w (normal)', [SonCihaz])
        )
    ).

% ----------------------------------------------------------------------
% test_yeni_cihaz/0
%
% Açıklama:
%   Belirli kullanıcılar için otomatik test yapar. Her kullanıcıda "yeni cihaz"
%   durumunun şüpheli olarak işaretlenip işaretlenmediğini kontrol eder.
%
% Örnek Kullanım:
%   ?- test_yeni_cihaz.
%
% Örnek Çıktı:
%   --- [TEST] Kural 6: Yeni Cihaz/Tarayıcı Kontrolü Başlıyor... ---
%   ----------------------------------
%   Kullanıcı: kullanici1
%    - Kural 6 kontrolü tamamlandı (yukarıdaki mesaja bakın).
%   ----------------------------------
%   Kullanıcı: kullanici2
%    - Kullanıcının işlemi yok veya kontrol başarısız oldu.
%   ----------------------------------
%   --- [TEST] Tamamlandı. ---
% ----------------------------------------------------------------------
test_yeni_cihaz :-
    writeln('--- [TEST] Kural 6: Yeni Cihaz/Tarayıcı Kontrolü Başlıyor... ---'),
    set_debug(true),

    % Burada test etmek istediğiniz kullanıcıların listesini belirtebilirsiniz.
    forall(
        member(Kullanici, [
            kullanici1, kullanici2, kullanici3,
            kullanici4, kullanici5, kullanici6,
            kullanici7, kullanici8, kullanici9
        ]),
        (
            writeln('----------------------------------'),
            format('Kullanıcı: ~w~n', [Kullanici]),
            (   yeni_cihaz_tespiti(Kullanici)
            ->  format(' - Kural 6 kontrolü tamamlandı (yukarıdaki mesaja bakın).~n', [])
            ;   format(' - Kullanıcının işlemi yok veya kontrol başarısız oldu.~n', [])
            )
        )
    ),

    set_debug(false),
    writeln('----------------------------------'),
    writeln('--- [TEST] Tamamlandı. ---').