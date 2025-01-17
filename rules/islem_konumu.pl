% islem_konumu.pl
%
% Açıklama:
%   Belirli bir kullanıcının farklı konumlardan (şehir, ülke vb.) işlem
%   yapıp yapmadığını kontrol eder. Eğer bir kullanıcının önceki işlemleri
%   birden çok farklı konumdan gelmişse, 'konum_uyusmazligi/1' predikatı
%   uyarı (alert_message/1) verebilir.
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
%   - '../utils/debug.pl' içinde debug_message/2, set_debug/1 vb.
%   - '../utils/alert.pl' içinde alert_message/1 vb.
%
% Modül Tanımı ve İhracı:
:- module(islem_konumu, [konum_uyusmazligi/1, test_konum_uyusmazligi/0]).

% Bağlı modüllerin yüklenmesi
:- use_module('../data/islem_verileri').  % Veri modülü
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
% Kullanım:
%   ?- kullanici_konumlari(kullanici1, Konumlar).
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
% Parametreler:
%   - Kullanici:  Konum uyuşmazlığı kontrolü yapılacak kullanıcı.
%
% Kullanım:
%   ?- konum_uyusmazligi(kullanici1).
%-----------------------------------------------------------------------------
konum_uyusmazligi(Kullanici) :-
    kullanici_konumlari(Kullanici, Konumlar),
    list_to_set(Konumlar, UnikKonumlar),
    length(UnikKonumlar, Say),
    debug_message('Farklı konum sayısı: ~w => ~w', [Kullanici, Say]),
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
% Kullanım:
%   ?- test_konum_uyusmazligi.
%-----------------------------------------------------------------------------
test_konum_uyusmazligi :-
    writeln('Test: konum_uyusmazligi kontrolü başlıyor...'),
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
    writeln('Test tamamlandı.').
