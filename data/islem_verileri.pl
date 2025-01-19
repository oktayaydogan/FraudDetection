:- module(islem_verileri, [initialize_islem_data/0, test_islem_verileri/0, islem/11]).

:- dynamic islem/11.

:- use_module(library(http/json)).

/* 
 * 1) initialize_islem_data/0
 */
initialize_islem_data :-
    retractall(islem(_,_,_,_,_,_,_,_,_,_,_)),
    absolute_file_name('islem_verileri.json', JSONFile, [access(read), file_errors(fail)]),
    format('[islem_verileri] JSON dosyası: ~w~n', [JSONFile]),

    open(JSONFile, read, Stream),
    json_read_dict(Stream, Dict),
    close(Stream),

    (   _{ islemler:IslemList } :< Dict
    ->  load_islemler_list(IslemList)
    ;   format('Uyarı: Dosyada "islemler" alanı bulunamadı.~n', [])
    ).

load_islemler_list([]).
load_islemler_list([Obj|Rest]) :-
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

    atom_string(ID,           IDString),
    atom_string(Kullanici,    KullaniciString),

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
    load_islemler_list(Rest).

:- initialization(initialize_islem_data, now).

test_islem_verileri :-
    writeln('--- [TEST] İşlem Verileri Kontrolü Başlıyor... ---'),
    setof((ID, Kullanici, Miktar, Zaman, Konum, Cihaz, DavranisSure, IslemTuru, IPAdresi, OdemeYontemi, Alan),
          islem(ID, Kullanici, Miktar, Zaman, Konum, Cihaz, DavranisSure, IslemTuru, IPAdresi, OdemeYontemi, Alan),
          Islemler),
    maplist(writeln, Islemler),
    writeln('--- [TEST] İşlem Verileri Kontrolü Bitti. ---').
