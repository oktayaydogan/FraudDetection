% ----------------------------------------------------------------------
% islem_verileri.pl
%
% Açıklama:
%   Bu modül, JSON formatında saklanan işlem verilerini yükler ve bu verileri
%   Prolog veritabanına ekler. İşlem verileri, dolandırıcılık analizi gibi
%   görevler için kullanılabilir.
%
% Kullanım:
%   1) Prolog ortamında bu dosyayı yükleyin:
%      ?- [islem_verileri].
%
%   2) İşlem verilerini yüklemek için:
%      ?- initialize_islem_data.
%
%   3) İşlem verilerini test etmek için:
%      ?- test_islem_verileri.
%
% Gereksinimler:
%   - 'islem_verileri.json' dosyasının mevcut olması ve doğru formatta olması.
%   - SWI-Prolog kütüphaneleri: http/json.
%
% Sınırlamalar:
%   - JSON dosyasının doğru formatta olması gerekmektedir.
%   - İşlem verilerinin dinamik olarak yüklenmesi için `initialize_islem_data/0`
%     predikatının çağrılması gerekmektedir.
%
% Gelecek Geliştirmeler:
%   - Hata yönetimi ve loglama özellikleri eklenebilir.
%   - Farklı veri kaynaklarından (örneğin veritabanı) veri yükleme desteği eklenebilir.
%
% Modül Tanımı ve İhracı:
:- module(islem_verileri, [initialize_islem_data/0, test_islem_verileri/0, islem/11]).

% Dinamik veritabanı girdileri
:- dynamic islem/11.

% Gerekli kütüphanelerin yüklenmesi
:- use_module(library(http/json)).

%-----------------------------------------------------------------------------
% initialize_islem_data/0
%
% Açıklama:
%   'islem_verileri.json' dosyasından işlem verilerini yükler ve bu verileri
%   Prolog veritabanına ekler. Önceki veriler temizlenir ve yeni veriler yüklenir.
%
% Kullanım:
%   ?- initialize_islem_data.
%-----------------------------------------------------------------------------
initialize_islem_data :-
    % Önceki verileri temizle
    retractall(islem(_,_,_,_,_,_,_,_,_,_,_)),
    
    % JSON dosyasının yolunu al
    absolute_file_name('islem_verileri.json', JSONFile, [access(read), file_errors(fail)]),
    
    % JSON dosyasını oku
    open(JSONFile, read, Stream),
    json_read_dict(Stream, Dict),
    close(Stream),

    % JSON'daki "islemler" alanını kontrol et ve yükle
    (   _{ islemler:IslemList } :< Dict
    ->  load_islemler_list(IslemList)
    ;   format('Uyarı: Dosyada "islemler" alanı bulunamadı.~n', [])
    ).

%-----------------------------------------------------------------------------
% load_islemler_list/1
%
% Açıklama:
%   JSON'dan alınan işlem listesini işler ve her bir işlemi Prolog veritabanına ekler.
%
% Parametreler:
%   - IslemList: JSON'dan alınan işlem listesi.
%
% Kullanım:
%   ?- load_islemler_list([...]).
%-----------------------------------------------------------------------------
load_islemler_list([]).
load_islemler_list([Obj|Rest]) :-
    % JSON objesinden alanları çıkar
    get_dict(id,           Obj, IDString),
    get_dict(kullanici,    Obj, KullaniciString),
    get_dict(miktar,       Obj, Miktar),
    get_dict(zaman,        Obj, Zaman),
    get_dict(konum,        Obj, Konum),
    get_dict(cihaz,        Obj, Cihaz),
    get_dict(davranisSure, Obj, DavranisSure),
    get_dict(islemTuru,    Obj, IslemTuru),
    get_dict(ipAdresi,     Obj, IPAdresi),
    get_dict(odemeYontemi, Obj, OdemeYontemi),
    get_dict(alan,         Obj, Alan),

    % String ve atom dönüşümleri
    atom_number(ID,           IDString),
    atom_string(Kullanici,    KullaniciString),

    % İşlemi veritabanına ekle
    assertz(islem(
        ID,
        Kullanici,
        Miktar,
        Zaman,
        Konum,
        Cihaz,
        DavranisSure,
        IslemTuru,
        IPAdresi,
        OdemeYontemi,
        Alan
    )),
    % Kalan işlemleri yükle
    load_islemler_list(Rest).

% Modül yüklendiğinde otomatik olarak verileri yükle
:- initialization(initialize_islem_data, now).

%-----------------------------------------------------------------------------
% test_islem_verileri/0
%
% Açıklama:
%   Yüklenen işlem verilerini test eder ve tüm işlemleri ekrana yazdırır.
%
% Kullanım:
%   ?- test_islem_verileri.
%-----------------------------------------------------------------------------
test_islem_verileri :-
    writeln('--- [TEST] İşlem Verileri Kontrolü Başlıyor... ---'),
    setof((ID, Kullanici, Miktar, Zaman, Konum, Cihaz, DavranisSure, IslemTuru, IPAdresi, OdemeYontemi, Alan),
          islem(ID, Kullanici, Miktar, Zaman, Konum, Cihaz, DavranisSure, IslemTuru, IPAdresi, OdemeYontemi, Alan),
          Islemler),
    maplist(writeln, Islemler),
    writeln('--- [TEST] İşlem Verileri Kontrolü Bitti. ---').