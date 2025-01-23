% islem_konumu.pl
%
% Açıklama:
%   Bu modül, belirli bir kullanıcının farklı konumlardan (şehir, ülke vb.) işlem
%   yapıp yapmadığını kontrol eder. Eğer bir kullanıcının önceki işlemleri
%   birden çok farklı konumdan gelmişse, 'konum_uyusmazligi/1' predikatı
%   uyarı (alert_message/1) verebilir. Bu tür durumlar, kullanıcı hesaplarının
%   güvenliği açısından potansiyel riskler olarak değerlendirilebilir.
%
% Kullanım:
%   1) Bu modülü Prolog ortamına yükleyin:
%      ?- [islem_konumu].
%
%   2) Predikatları aşağıdaki gibi test edebilirsiniz:
%      ?- konum_uyusmazligi(kullanici1).
%      ?- test_konum_uyusmazligi.
%
% Gereksinimler:
%   - '../data/islem_verileri.pl' dosyasında islem/11 tanımı olması.
%     Örnek islem/11 yapısı:
%     islem(ID, Kullanici, Tip, Zaman, Konum, Cihaz, _, _, _, _, _).
%   - '../utils/debug.pl' içinde debug_message/2, set_debug/1 gibi yardımcı predikatların tanımlı olması.
%   - '../utils/alert.pl' içinde alert_message/1 gibi fonksiyonların tanımlı olması (gerekirse).
%
% Sınırlamalar:
%   - Bu modül, sadece 'Konum' alanını kullanarak işlem konumlarını analiz eder.
%   - Konum bilgisi, işlem verilerinde doğru ve tutarlı bir şekilde sağlanmalıdır.
%
% Gelecek Geliştirmeler:
%   - Konumlar arasındaki mesafe hesaplamaları eklenebilir.
%   - Zaman içinde konum değişikliklerini analiz eden dinamik bir model eklenebilir.
%
% Modül Tanımı ve İhracı:
:- module(islem_konumu, [konum_uyusmazligi/1, test_konum_uyusmazligi/0]).

% Bağlı modüllerin yüklenmesi
:- use_module('../data/islem_verileri').  % Veri modülü (islem/11 tanımı)
:- use_module('../utils/debug').          % Debug mesajlarını yönetir
:- use_module('../utils/alert').          % Uyarı mesajlarını yönetir

%-----------------------------------------------------------------------------
% kullanici_konumlari/2
%
% Açıklama:
%   Verilen bir kullanıcının (Kullanici) yaptığı işlemlerdeki konum bilgilerini
%   toplayarak bir liste (Konumlar) halinde döndürür. Ardından debug_message/2
%   ile listeyi loglar.
%
% Parametreler:
%   - Kullanici:  Konumları listelenecek kullanıcı kimliği.
%   - Konumlar:   Kullanıcının işlemlerinde kullanılan konumların listesi (çıktı).
%
% Örnek Kullanım:
%   ?- kullanici_konumlari(kullanici1, Konumlar).
%   Konumlar = ['İstanbul', 'Ankara'].
%-----------------------------------------------------------------------------
kullanici_konumlari(Kullanici, Konumlar) :-
    findall(Konum,
            islem(_, Kullanici, _, _, Konum, _, _, _, _, _, _),
            Konumlar),
    debug_message('Kullanıcı konumları: ~w => ~w', [Kullanici, Konumlar]).

%-----------------------------------------------------------------------------
% konum_uyusmazligi/1
%
% Açıklama:
%   Bir kullanıcının işlemlerinin kaç farklı konumdan yapıldığını kontrol eder.
%   Eğer birden fazla farklı konum varsa, alert_message/1 ile konum uyuşmazlığı
%   uyarısı verir. 'Say > 1' koşulu ile belirlenir.
%
%   (Kural 4: İşlemin yapıldığı konum, önceki işlemlerindeki konumla
%    uyuşmuyorsa şüpheli olabilir.)
%
% Parametreler:
%   - Kullanici:  Konum uyuşmazlığı kontrolü yapılacak kullanıcı.
%
% Örnek Kullanım:
%   ?- konum_uyusmazligi(kullanici1).
%   true.  % Eğer konum uyuşmazlığı varsa
%   false. % Eğer konum uyuşmazlığı yoksa
%-----------------------------------------------------------------------------
konum_uyusmazligi(Kullanici) :-
    kullanici_konumlari(Kullanici, Konumlar),
    list_to_set(Konumlar, UnikKonumlar),
    length(UnikKonumlar, Say),
    debug_message('Farklı konum sayısı: ~w => ~w', [Kullanici, Say]),
    % Eğer kullanıcının farklı konum sayısı 1'den büyükse, uyuşmazlık var
    Say > 1,
    alert_message('Konum uyuşmazlığı tespit edildi!').

%-----------------------------------------------------------------------------
% test_konum_uyusmazligi/0
%
% Açıklama:
%   Bazı örnek kullanıcılar üzerinde konum uyuşmazlığı testini çalıştırır.
%   Debug modunu etkinleştirerek (set_debug(true)), ekrana ayrıntılı bilgi
%   yazdırır. Test bittiğinde debug kapatılır.
%
% Örnek Kullanım:
%   ?- test_konum_uyusmazligi.
%
% Örnek Çıktı:
%   --- [TEST] Kural 4 (İşlem Konumu) Kontrolü Başlıyor... ---
%   ----------------------------------
%   Kullanıcı: kullanici1, Konum uyuşmazlığı tespit edildi.
%   ----------------------------------
%   Kullanıcı: kullanici2, Konum uyuşmazlığı tespit edilemedi.
%   ----------------------------------
%   --- [TEST] Tamamlandı. ---
%-----------------------------------------------------------------------------
test_konum_uyusmazligi :-
    writeln('--- [TEST] Kural 4 (İşlem Konumu) Kontrolü Başlıyor... ---'),
    set_debug(true),
    forall(
        member(Kullanici, [kullanici1, kullanici2, kullanici3, kullanici4]),
        (
            writeln('----------------------------------'),
            ( konum_uyusmazligi(Kullanici) ->
                format('Kullanıcı: ~w, Konum uyuşmazlığı tespit edildi.~n', [Kullanici])
            ;   format('Kullanıcı: ~w, Konum uyuşmazlığı tespit edilemedi.~n', [Kullanici])
            )
        )
    ),
    set_debug(false),
    writeln('----------------------------------'),
    writeln('--- [TEST] Tamamlandı. ---').