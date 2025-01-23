% ----------------------------------------------------------------------
% fraud_detection.pl
%
% Açıklama:
%   Bu modül, kullanıcılar ve işlemler üzerinde dolandırıcılık risk analizi yapar.
%   Farklı kurallar (örneğin işlem sıklığı, işlem miktarı, konum uyuşmazlığı vb.)
%   kullanılarak her bir işlem ve kullanıcı için risk skorları hesaplanır.
%   Bu skorlar, dolandırıcılık şüphesi olan işlemleri tespit etmek için kullanılır.
%
% Kullanım:
%   1) Prolog ortamında bu dosyayı yükleyin:
%      ?- [fraud_detection].
%
%   2) Aşağıdaki predikatları kullanarak sorgular yapın:
%      - Kullanıcı sorgulama: fraud_kullanici_sorgula(KullaniciID, Risk).
%      - İşlem sorgulama: fraud_islem_sorgula(IslemID, Risk).
%      - Tüm kullanıcıları sorgulama: tüm_kullanicilari_sorgula.
%
% Gereksinimler:
%   - 'data/islem_verileri.pl' dosyasında 'islem/11' verisinin tanımlı olması.
%   - 'rules/*.pl' dosyalarında ilgili analiz kurallarının tanımlanmış olması.
%
% Sınırlamalar:
%   - İşlem verilerinin ve kuralların doğru formatta olması gerekmektedir.
%   - Risk skorları, kurallara bağlı olarak hesaplanır; kuralların güncelliği önemlidir.
%
% Gelecek Geliştirmeler:
%   - Daha fazla kural eklenerek risk analizi detaylandırılabilir.
%   - Raporlama ve loglama özellikleri eklenebilir.
%   - Kullanıcı dostu bir arayüz (GUI) entegre edilebilir.
%
% Modül Tanımı ve İhracı:
:- module(fraud_detection, [fraud_kullanici_sorgula/2, fraud_islem_sorgula/2, tüm_kullanicilari_sorgula/0]).

% Gerekli modüllerin yüklenmesi
:- use_module('data/islem_verileri').      % Tüm işlem verilerini yükler
:- use_module('rules/islem_sikligi').      % İşlem sıklığı kuralları
:- use_module('rules/islem_miktari').      % İşlem miktarı kuralları
:- use_module('rules/islem_konumu').       % İşlem konumu kuralları
:- use_module('rules/farkli_konum').       % Farklı konum kuralları
:- use_module('rules/yeni_cihaz').         % Yeni cihaz tespiti kuralları
:- use_module('rules/davranis_analizi').   % Kullanıcı davranış analizi kuralları
:- use_module('rules/ortak_ip_kullanimi'). % Aynı IP adresinden farklı kullanıcı kontrolü
:- use_module('rules/odeme_suresi').       % Ödeme süresi analizi kuralları
:- use_module('rules/yeni_odeme_yontemi'). % Yeni ödeme yöntemi kontrolü kuralları
:- use_module('rules/odeme_yontemi_riski').% Ödeme yöntemi riski analizi kuralları
:- use_module('rules/tekrarli_bilgi_kontrolu').   % Tekrarlı bilgi kontrolü
:- use_module('rules/para_iade_kontrolu').        % Para iade kontrolü
:- use_module('rules/oturum_bilgisi_degisim_kontrolu'). % Oturum bilgisi değişikliği kontrolü

%-----------------------------------------------------------------------------
% risk_skoru_islem/2
%
% Açıklama:
%   Belirli bir işlem (IslemId) için risk skoru hesaplar. Bu hesaplama
%   sırasında işlem miktarı, konum uyuşmazlığı, yeni cihaz, işlem sıklığı
%   gibi faktörler incelenir. Her anormal durum belirli bir puan (P1, P2,
%   vb.) ekler. Sonuçta toplam (Risk) değeri elde edilir.
%
% Parametreler:
%   - IslemId:  İncelenecek işlemin benzersiz kimliği.
%   - Risk:     Hesaplanan risk skoru (çıktı).
%
% Kullanım:
%   ?- risk_skoru_islem(1, Risk).
%-----------------------------------------------------------------------------
risk_skoru_islem(IslemId, Risk) :-
    % 'islem/11' yapısı: (IslemId, Kullanici, Miktar, Zaman, Konum, Cihaz,
    %                    DavranisSure, IslemTuru, IP, OdemeYontemi, Ekstra)
    islem(IslemId, Kullanici, Miktar, _, _, _, _, _, _, _, _),

    % islem_miktari:anormal_islem/2 -> P1
    ( islem_miktari:anormal_islem(Kullanici, Miktar)
    -> P1 = 6
    ;  P1 = 0
    ),

    % islem_konumu:konum_uyusmazligi/1 -> P2
    ( islem_konumu:konum_uyusmazligi(Kullanici)
    -> P2 = 3
    ;  P2 = 0
    ),

    % yeni_cihaz:yeni_cihaz_tespiti/1 -> P3
    ( yeni_cihaz:yeni_cihaz_tespiti(Kullanici)
    -> P3 = 4
    ;  P3 = 0
    ),

    % islem_sikligi:supheli_islem/3 -> P4 (örn. son 24 saatteki işlem sıklığı)
    ( islem_sikligi:supheli_islem(Kullanici, 0, 24)
    -> P4 = 5
    ;  P4 = 0
    ),

    % yeni_odeme_yontemi:yeni_odeme_yontemi/1 -> P5
    ( yeni_odeme_yontemi:yeni_odeme_yontemi(Kullanici)
    -> P5 = 2
    ;  P5 = 0
    ),

    % para_iade_kontrolu:para_iade_riski/1 -> P6
    ( para_iade_kontrolu:para_iade_riski(Kullanici)
    -> P6 = 3
    ;  P6 = 0
    ),

    % Toplam risk skorunu hesaplıyoruz
    Risk is P1 + P2 + P3 + P4 + P5 + P6.

%-----------------------------------------------------------------------------
% risk_skoru_kullanici/2
%
% Açıklama:
%   Belirli bir kullanıcının yaptığı tüm işlemleri (IslemList) bulur ve
%   her bir işlem için hesaplanan risk skorlarını toplar (RiskList).
%   Sonuçta ilgili kullanıcının toplam risk skoru (ToplamRisk) elde edilir.
%
% Parametreler:
%   - Kullanici:   İncelenecek kullanıcının kimliği/ID'si.
%   - ToplamRisk:  Kullanıcının tüm işlemlerinden kaynaklanan
%                  toplam risk puanı (çıktı).
%
% Kullanım:
%   ?- risk_skoru_kullanici(kullanici1, ToplamRisk).
%-----------------------------------------------------------------------------
risk_skoru_kullanici(KullaniciX, OrtalamaRisk) :-
    % Kullanıcı adını atom'a çevir (eğer string olarak geliyorsa)
    atom_string(Kullanici, KullaniciX),

    % Kullanıcıya ait tüm işlemlerin IDsini toplayalım
    findall(IslemId, islem(IslemId, Kullanici, _, _, _, _, _, _, _, _, _), IslemList),

    % Her işlem ID için risk_skoru_islem/2 ile risk skoru hesaplayıp listeye alalım
    findall(Risk,
            (member(IslemId, IslemList),
             risk_skoru_islem(IslemId, Risk)),
            RiskList),

    % RiskList boş mu kontrol edelim
    (   RiskList = []
    ->  OrtalamaRisk = 0  % Eğer hiç işlem yoksa ortalama risk 0 olarak kabul edilir
    ;   % Tüm risk skorlarını toplayarak kullanıcı için toplam risk skorunu elde edelim
        sum_list(RiskList, ToplamRisk),
        length(RiskList, RiskListLength),
        OrtalamaRisk is ToplamRisk / RiskListLength
    ).

%-----------------------------------------------------------------------------
% fraud_kullanici_sorgula/2
%
% Açıklama:
%   Tek bir kullanıcıyı (Kullanici) sorgular. Kullanıcının toplam
%   risk skorunu hesaplar ve ekrana yazdırır. Belirli bir eşik değere
%   göre (örneğin 50) işlem yüksek riskli olarak etiketlenebilir.
%
% Parametreler:
%   - Kullanici:  Risk skoru sorgulanacak kullanıcı kimliği.
%   - ToplamRisk: Hesaplanan risk skoru (çıktı).
%
% Kullanım:
%   ?- fraud_kullanici_sorgula(kullanici1, Risk).
%-----------------------------------------------------------------------------
fraud_kullanici_sorgula(Kullanici, ToplamRisk) :-
  risk_skoru_kullanici(Kullanici, ToplamRisk).

%-----------------------------------------------------------------------------
% tüm_kullanicilari_sorgula/0
%
% Açıklama:
%   Sistemde var olan tüm kullanıcıları (işlem tablosunda geçen)
%   bulur, her biri için fraud_kullanici_sorgula/2 predikatını çağırır.
%
% Kullanım:
%   ?- tüm_kullanicilari_sorgula.
%-----------------------------------------------------------------------------
tüm_kullanicilari_sorgula :-
    % islem/11 yapısından Kullanici alanını toplayalım
    findall(Kullanici, islem(_, Kullanici, _, _, _, _, _, _, _, _, _), KullaniciListesi),

    % Yinelenen kullanıcı adlarını set haline getirelim
    list_to_set(KullaniciListesi, UnikKullanicilar),

    % Her kullanıcı için fraud_kullanici_sorgula/2 predikatını çalıştıralım
    forall(member(Kullanici, UnikKullanicilar),
           fraud_kullanici_sorgula(Kullanici, _Response)).

%-----------------------------------------------------------------------------
% fraud_islem_sorgula/2
%
% Açıklama:
%   Belirli bir işlem ID'si hakkındaki temel bilgileri (Miktar, Zaman,
%   Konum, Cihaz, vb.) ekrana yazdırır ve ayrıca risk_skoru_islem/2
%   predikatı ile işlem risk skorunu hesaplar.
%
% Parametreler:
%   - IslemId:  İncelenecek işlemin kimliği/ID'si.
%   - Risk:     Hesaplanan risk skoru (çıktı).
%
% Kullanım:
%   ?- fraud_islem_sorgula(1, Risk).
%-----------------------------------------------------------------------------
fraud_islem_sorgula(IslemIdString, Risk) :-   
  atom_number(IslemId, IslemIdString),
  risk_skoru_islem(IslemId, Risk).

%-----------------------------------------------------------------------------
% Örnek Sorgular:
%
% ?- fraud_kullanici_sorgula(kullanici1, Risk).
% ?- fraud_kullanici_sorgula(kullanici2, Risk).
%
% ?- fraud_islem_sorgula(1, Risk).
% ?- fraud_islem_sorgula(6, Risk).
%
% ?- tüm_kullanicilari_sorgula.
%-----------------------------------------------------------------------------