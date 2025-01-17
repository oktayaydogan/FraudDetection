% farkli_konum.pl
%
% Açıklama:
%   Kullanıcının işlemlerini inceleyerek, kısa zaman aralıklarında 
%   farklı konumdan yapılan işlemleri tespit eder ve belirli bir 
%   risk puanı ekler. Böylece potansiyel şüpheli durumlar (örneğin, 
%   10 dakika içinde iki farklı ülkede işlem) saptanabilir.
%
% Kullanım:
%   1) Bu modülü Prolog ortamına yükleyin:
%      ?- [farkli_konum].
%
%   2) Predikatları aşağıdaki gibi çağırabilirsiniz:
%      ?- farkli_konum_risk(kullanici1, Risk).
%      ?- test_farkli_konum.
%
% Gereksinimler:
%   - '../data/islem_verileri.pl' içinde 'islem/11' tanımının olması.
%   - '../utils/debug.pl' içinde debug_message/2 ve set_debug/1 gibi 
%     yardımcı predikatların olması.
%   - '../utils/alert.pl' içinde alert_message/1 gibi fonksiyonların 
%     tanımlanmış olması (gerekirse).
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
%   Belirli bir kullanıcının yaptığı tüm işlemleri (ID, Zaman, Konum, Cihaz) 
%   çiftleri halinde listeleyip Islemler değişkenine aktarır.
%
% Parametreler:
%   - Kullanici:  İşlemleri listelenecek kullanıcı kimliği.
%   - Islemler:   Bulunan işlemlerin (ID, Zaman, Konum, Cihaz) şeklindeki listesi.
%
% Kullanım:
%   ?- kullanici_islemleri(kullanici1, Islemler).
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
%   Kısa sürede farklı konumlardan işlem yapan kullanıcıların risk 
%   skorunu hesaplar. kullanici_islemleri/2 ile elde edilen işlemleri 
%   skor_hesapla/3 yardımıyla analiz eder ve ToplamRisk değeri döndürür.
%
% Parametreler:
%   - Kullanici:   Riski hesaplanacak kullanıcı.
%   - ToplamRisk:  Kullanıcıya ait farklı konum risk skorunun toplam değeri.
%
% Kullanım:
%   ?- farkli_konum_risk(kullanici1, Risk).
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
%   İşlem listesindeki ardışık çiftleri inceleyerek, konumlar farklı 
%   ve iki işlem arasındaki zaman farkı (abs(Zaman2 - Zaman1)) 10 
%   dakikadan az ise risk skorunu artırır. 
%
% Parametreler:
%   - Islemler:    (ID, Zaman, Konum, Cihaz) şeklindeki işlem listesi.
%   - MevcutRisk:  Hesaplanmaya başlanan mevcut risk skoru.
%   - ToplamRisk:  Son hesaplanan toplam risk skoru (çıktı).
%
% Kullanım:
%   ?- skor_hesapla([(...),(...)], 0, SonRisk).
%-----------------------------------------------------------------------------
skor_hesapla([(_, Zaman1, Konum1, _), (_, Zaman2, Konum2, _) | Kalan], MevcutRisk, ToplamRisk) :-
    ( Konum1 \= Konum2, abs(Zaman2 - Zaman1) =< 10 ->
        RiskArtis is 5,
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
%   Bazı kullanıcılar üzerinde (kullanici1, kullanici2, vb.) farkli_konum_risk/2 
%   predikatını dener ve sonucu ekrana yazdırır. Debug modunu geçici olarak açıp
%   kapatarak ayrıntılı bilgi alınabilir.
%
% Kullanım:
%   ?- test_farkli_konum.
%-----------------------------------------------------------------------------
test_farkli_konum :-
    writeln('Test: farkli_konum_risk kontrolü başlıyor...'),
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
    writeln('Test tamamlandı.').
