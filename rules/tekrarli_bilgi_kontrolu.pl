% tekrarli_bilgi_kontrolu.pl
%
% Açıklama:
%   Bu modül, aynı telefon numarası veya e-posta adresiyle birden fazla kullanıcı
%   hesabı oluşturulup oluşturulmadığını kontrol eder. Bu tür durumlar, dolandırıcılık
%   şüphesi olarak değerlendirilebilir ve uyarı (alert_message/2) verilir.
%
% Kullanım:
%   1) Prolog ortamında bu dosyayı yükleyin:
%      ?- [tekrarli_bilgi_kontrolu].
%
%   2) Predikatları aşağıdaki gibi test edebilirsiniz:
%      ?- tekrarli_bilgi('user1@example.com').
%      ?- test_tekrarli_bilgi_kontrolu.
%
% Gereksinimler:
%   - '../data/islem_verileri.pl' içinde islem/11 tanımı olması.
%     Örnek islem/11 yapısı:
%     islem(ID, Kullanici, Miktar, Zaman, Konum, Cihaz, _, _, _, _, Alan).
%   - '../utils/debug.pl' ve '../utils/alert.pl' dosyalarında debug_message/2,
%     set_debug/1, alert_message/2 vb. tanımlı olması.
%
% Sınırlamalar:
%   - Bu modül, sadece 'Alan' (telefon numarası veya e-posta) alanını kullanarak
%     tekrarlı bilgileri analiz eder.
%   - Alan değerinin benzersizliği, sadece aynı değere sahip kullanıcı sayısına göre
%     belirlenir.
%
% Gelecek Geliştirmeler:
%   - Alan değerlerinin geçerliliğini kontrol eden bir doğrulama mekanizması eklenebilir.
%   - Farklı bilgi türleri (örneğin, TC kimlik numarası) için özelleştirilmiş kontroller
%     eklenebilir.
%
% Modül Tanımı ve İhracı:
:- module(tekrarli_bilgi_kontrolu, [
    tekrarli_bilgi/1,
    test_tekrarli_bilgi_kontrolu/0
]).

% Gerekli modüllerin dahil edilmesi
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug').         % Debug mesajları
:- use_module('../utils/alert').         % Alert mesajları

% ----------------------------------------------------------------------
%   KURAL 12:
%   "Aynı telefon numarası veya e-posta adresiyle birden fazla kullanıcı 
%    hesabı oluşturulmuşsa ve bu hesaplardan ödeme yapılmaya çalışılıyorsa, 
%    dolandırıcılık riski vardır."
%
%   Örnek: Aynı telefon numarasıyla birçok hesap varsa ve bu hesaplardan 
%          farklı işlemler yapılmaya çalışılıyorsa, bu durum şüpheli olabilir.
%
% ----------------------------------------------------------------------
% tekrarli_bilgi/1
%
% Açıklama:
%   Belirli bir Alan (telefon numarası veya e-posta) değerinin birden fazla
%   kullanıcı tarafından kullanılıp kullanılmadığını kontrol eder. Eğer aynı
%   Alan değeri birden fazla kullanıcı tarafından kullanılıyorsa, uyarı verir.
%
% Parametreler:
%   - Alan: Kontrol edilecek telefon numarası veya e-posta adresi.
%
% Örnek Kullanım:
%   ?- tekrarli_bilgi('user1@example.com').
%   true.  % Eğer şüpheli işlem varsa
%   false. % Eğer şüpheli işlem yoksa
% ----------------------------------------------------------------------
tekrarli_bilgi(Alan) :-
    (   var(Alan)
    ->  alert_message('Hata: Alan değeri belirtilmemiş.')
    ;   % Alan belirtilmişse devam edelim
        findall(
            Kullanici,
            % islem(_, Kullanici, ..., Alan) => 11. argüman e-posta/tel
            islem(_, Kullanici, _, _, _, _, _, _, _, _, Alan),
            KullaniciListesi
        ),
        list_to_set(KullaniciListesi, UnikKullanicilar),
        length(UnikKullanicilar, Say),
        (   Say > 1
        ->  % Birden fazla kullanıcı bu Alan'ı kullanıyorsa "dolandırıcılık riski"
            alert_message(
                'Kural 12: Aynı bilgi (Telefon/E-posta) birden fazla hesapta kullanılıyor, dolandırıcılık riski: ~w -> ~w',
                [Alan, UnikKullanicilar]
            )
        ;   debug_message(
                'Kural 12: Bilgi tekrarı tespit edilmedi (Alan: ~w, Kullanıcı sayısı: ~w)',
                [Alan, Say]
            )
        )
    ).

% ----------------------------------------------------------------------
% test_tekrarli_bilgi_kontrolu/0
%
% Açıklama:
%   Belirli örnek Alan değerleri ile tekrarli_bilgi/1 predikatını test eder.
%   Debug modunu etkinleştirir ve sonuçları ekrana yazdırır.
%
% Örnek Kullanım:
%   ?- test_tekrarli_bilgi_kontrolu.
%
% Örnek Çıktı:
%   --- [TEST] Kural 12: Tekrarlı Bilgi (E-posta/Tel) Kontrolü Başlıyor... ---
%   Kural 12: Aynı bilgi (Telefon/E-posta) birden fazla hesapta kullanılıyor, dolandırıcılık riski: user1@example.com -> [kullanici1, kullanici2]
%   Kural 12: Bilgi tekrarı tespit edilmedi (Alan: user2@example.com, Kullanıcı sayısı: 1)
%   --- [TEST] Tamamlandı. ---
% ----------------------------------------------------------------------
test_tekrarli_bilgi_kontrolu :-
    writeln('--- [TEST] Kural 12: Tekrarlı Bilgi (E-posta/Tel) Kontrolü Başlıyor... ---'),
    set_debug(true),

    % Örnek testler - elinizdeki verilerle örtüşen e-posta / telefon alanlarını yazabilirsiniz.
    tekrarli_bilgi('user1@example.com'),
    tekrarli_bilgi('user2@example.com'),
    tekrarli_bilgi('user3@example.com'),
    tekrarli_bilgi('user4@example.com'),
    tekrarli_bilgi('+9055XXXXXX'),        % Örnek telefon numarası
    tekrarli_bilgi('invalid@example.com'), % Hiç kimsede olmayan bilgi
    tekrarli_bilgi(_),                    % Var(Alan) durumu (hata test)
    
    set_debug(false),
    writeln('--- [TEST] Tamamlandı. ---').