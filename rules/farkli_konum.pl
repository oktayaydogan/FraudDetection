% farkli_konum.pl
%
% Açıklama:
%   Bu modül, kullanıcıların kısa zaman aralıklarında farklı konumlardan yaptıkları işlemleri tespit eder ve bu durumlara 
%   belirli bir risk puanı ekler. Örneğin, 10 dakika içinde iki farklı ülkede yapılan işlemler potansiyel şüpheli durumlar 
%   olarak işaretlenir. Bu tür durumlar, kullanıcı hesaplarının güvenliği açısından önemli bir risk faktörü olabilir.
%
% Kullanım:
%   1) Bu modülü Prolog ortamına yükleyin:
%      ?- [farkli_konum].
%
%   2) Predikatları aşağıdaki gibi çağırabilirsiniz:
%      ?- farkli_konum_risk(kullanici1, Risk).
%      ?- test_farkli_konum.
%
% Örnek Çıktı:
%   ?- test_farkli_konum.
%   --- [TEST] Kural 5 (Farklı Konumu) Kontrolü Başlıyor... ---
%   ----------------------------------
%   Kullanıcı: kullanici1, Risk: 10
%   ----------------------------------
%   Kullanıcı: kullanici2, Risk: 0
%   ----------------------------------
%   --- [TEST] Tamamlandı. ---
%
% Gereksinimler:
%   - '../data/islem_verileri.pl' dosyasında 'islem/11' tanımının olması gerekmektedir. Bu tanım, işlem verilerini içerir.
%     Örnek islem/11 yapısı:
%     islem(ID, Kullanici, Tip, Zaman, Konum, Cihaz, ...).
%   - '../utils/debug.pl' dosyasında debug_message/2 ve set_debug/1 gibi yardımcı predikatların tanımlanmış olması gerekmektedir.
%   - '../utils/alert.pl' dosyasında alert_message/1 gibi fonksiyonların tanımlanmış olması (gerekirse).
%
% Sınırlamalar:
%   - Bu modül, sadece 10 dakika içinde yapılan işlemleri kontrol eder. Daha uzun zaman aralıkları için başka bir modül kullanılmalıdır.
%   - Konum bilgisi, işlem verilerinde doğru ve tutarlı bir şekilde sağlanmalıdır.
%
% Gelecek Geliştirmeler:
%   - Konum farkı kontrolü için daha esnek zaman aralıkları eklenebilir.
%   - Risk skoru hesaplama algoritması daha karmaşık hale getirilebilir.
%
% Modül Tanımı ve İhracı:
:- module(farkli_konum, [farkli_konum_risk/2, test_farkli_konum/0]).

% İlgili modüllerin yüklenmesi:
:- use_module('../data/islem_verileri').  % Veri kümesi (islem/11 tanımı)
:- use_module('../utils/debug').          % Debug mesajları (debug_message/2, set_debug/1)
:- use_module('../utils/alert').          % Uyarı mesajları (alert_message/1)

%-----------------------------------------------------------------------------
% kullanici_islemleri/2
%
% Açıklama:
%   Belirli bir kullanıcının yaptığı tüm işlemleri (ID, Zaman, Konum, Cihaz) şeklinde listeler. Bu işlemler, islem/11 
%   predikatından alınır ve kullanıcıya ait işlemlerin tam listesini döndürür.
%
% Parametreler:
%   - Kullanici:  İşlemleri listelenecek kullanıcı kimliği.
%   - Islemler:   Bulunan işlemlerin (ID, Zaman, Konum, Cihaz) şeklindeki listesi.
%
% Örnek Kullanım:
%   ?- kullanici_islemleri(kullanici1, Islemler).
%   Islemler = [(1, 100, 'Turkiye', 'Telefon'), (2, 110, 'Almanya', 'Bilgisayar')].
%-----------------------------------------------------------------------------
kullanici_islemleri(Kullanici, Islemler) :-
    findall((ID, Zaman, Konum, Cihaz),
            islem(ID, Kullanici, _, Zaman, Konum, Cihaz, _, _, _, _, _),
            Islemler),
    debug_message('Kullanıcı işlemleri listelendi: ~w', [Islemler]).

%-----------------------------------------------------------------------------
% farkli_konum_risk/2
%
% Açıklama:
%   Kısa sürede farklı konumlardan işlem yapan kullanıcıların risk skorunu hesaplar. Bu predikat, kullanıcının işlemlerini 
%   analiz eder ve ardışık işlemler arasında konum farkı ve zaman farkı kontrolü yapar. Eğer iki işlem arasındaki zaman 
%   farkı 10 dakikadan az ve konumlar farklı ise, risk skoruna 5 puan eklenir.
%
% Parametreler:
%   - Kullanici:   Riski hesaplanacak kullanıcı.
%   - ToplamRisk:  Kullanıcıya ait farklı konum risk skorunun toplam değeri.
%
% Örnek Kullanım:
%   ?- farkli_konum_risk(kullanici1, Risk).
%   Risk = 10.
%-----------------------------------------------------------------------------
farkli_konum_risk(Kullanici, ToplamRisk) :-
    kullanici_islemleri(Kullanici, Islemler),
    debug_message('Farklı konum riski hesaplanıyor: ~w', [Kullanici]),
    skor_hesapla(Islemler, 0, ToplamRisk),
    debug_message('Toplam risk skoru: ~w', [ToplamRisk]).

%-----------------------------------------------------------------------------
% skor_hesapla/3
%
% Açıklama:
%   İşlem listesindeki ardışık çiftleri inceleyerek, konumlar farklı ve iki işlem arasındaki zaman farkı (abs(Zaman2 - Zaman1)) 
%   10 dakikadan az ise risk skorunu artırır. Bu predikat, rekürsif olarak çalışır ve tüm işlem çiftlerini kontrol eder.
%
% Parametreler:
%   - Islemler:    İşlem listesi (ID, Zaman, Konum, Cihaz) şeklinde.
%   - MevcutRisk:  Şu ana kadar hesaplanan risk skoru.
%   - ToplamRisk:  Tüm işlemler kontrol edildikten sonra elde edilen toplam risk skoru.
%
% Örnek Kullanım:
%   ?- skor_hesapla([(1, 100, 'Turkiye', 'Telefon'), (2, 110, 'Almanya', 'Bilgisayar')], 0, Risk).
%   Risk = 5.
%-----------------------------------------------------------------------------
skor_hesapla([(_, Zaman1, Konum1, _), (_, Zaman2, Konum2, _) | Kalan], MevcutRisk, ToplamRisk) :-
    ( Konum1 \= Konum2,
      abs(Zaman2 - Zaman1) =< 10
    ->  RiskArtis is 5,
        YeniRisk is MevcutRisk + RiskArtis,
        debug_message('Konum farkı tespit edildi: ~w => ~w, Yeni risk: ~w',
                      [Konum1, Konum2, YeniRisk])
    ;   YeniRisk is MevcutRisk
    ),
    skor_hesapla([(_, Zaman2, Konum2, _) | Kalan], YeniRisk, ToplamRisk).

% Liste tek işlem kalırsa, risk hesaplamayı bitir:
skor_hesapla([_], ToplamRisk, ToplamRisk).
% Liste tamamen boşsa da risk hesaplamayı bitir:
skor_hesapla([], ToplamRisk, ToplamRisk).

%-----------------------------------------------------------------------------
% test_farkli_konum/0
%
% Açıklama:
%   Bu predikat, belirli kullanıcılar üzerinde farkli_konum_risk/2 predikatını test eder ve sonuçları ekrana yazdırır. 
%   Test sırasında debug modu aktif edilir ve her kullanıcı için risk skoru hesaplanır.
%
% Kullanım:
%   ?- test_farkli_konum.
%
% Örnek Çıktı:
%   --- [TEST] Kural 5 (Farklı Konumu) Kontrolü Başlıyor... ---
%   ----------------------------------
%   Kullanıcı: kullanici1, Risk: 10
%   ----------------------------------
%   Kullanıcı: kullanici2, Risk: 0
%   ----------------------------------
%   --- [TEST] Tamamlandı. ---
%-----------------------------------------------------------------------------
test_farkli_konum :-
    writeln('--- [TEST] Kural 5 (Farklı Konumu) Kontrolü Başlıyor... ---'),
    set_debug(true),
    forall(
        member(Kullanici, [kullanici1, kullanici2, kullanici3, kullanici4]),
        (
            writeln('----------------------------------'),
            ( farkli_konum_risk(Kullanici, Risk) ->
                format('Kullanıcı: ~w, Risk: ~w~n', [Kullanici, Risk])
            ;   format('Kullanıcı: ~w, Risk bulunamadı.~n', [Kullanici])
            )
        )
    ),
    set_debug(false),
    writeln('----------------------------------'),
    writeln('--- [TEST] Tamamlandı. ---').