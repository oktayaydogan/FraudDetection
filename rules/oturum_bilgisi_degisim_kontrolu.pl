:- module(oturum_bilgisi_degisim_kontrolu, [
    bilgi_degisim_riski/1,
    test_bilgi_degisim_riski/0
]).

:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug').         % Debug mesajları
:- use_module('../utils/alert').         % Alert mesajları

/* 
 * Kural: Kullanıcı bir "degisim" (bilgi değişikliği) yaptıysa
 *        ve bu değişiklikten çok kısa süre içinde (≤5 birim) 
 *        bir "islem" (ödeme vs.) yapılıyorsa şüpheli olabilir.
 */

/* 
 * 1) bilgi_degisim_riski/1
 *    => Kullanıcının degisim ve islem kayıtlarını toplar,
 *       kontrol fonksiyonuna gönderir.
 */
bilgi_degisim_riski(Kullanici) :-
    findall((DegisimZamani, DegisimID),
            islem(DegisimID, Kullanici, _, DegisimZamani, _, _, _, 'degisim', _, _, _),
            Degisimler),
    findall((IslemZamani, IslemID),
            islem(IslemID, Kullanici, _, IslemZamani, _, _, _, 'islem', _, _, _),
            Islemler),
    debug_message('Bilgi değişiklikleri: ~w', [Degisimler]),
    debug_message('İşlemler: ~w', [Islemler]),
    % Eğer degisim ya da islem listesi boşsa, yine de kontrol predikatına gidecek
    bilgi_degisim_kontrol(Degisimler, Islemler).

/*
 * 2) bilgi_degisim_kontrol/2
 *    => Her degisim kaydı için, tüm islem kayıtlarına bakar
 *       (zaman farkı ≤5) olursa alert verir.
 *    => Tüm degisim kayıtları işlendiğinde biter.
 */
bilgi_degisim_kontrol([], _) :-
    debug_message('Bilgi değişikliği sonrası işlem kontrolü tamamlandı. Tüm değişiklikler incelendi.'),
    !.

bilgi_degisim_kontrol([(DegisimZamani, DegisimID)|KalanDegisimler], Islemler) :-
    % Bu degisim kaydı için, islem'lerle karşılaştırma yapalım
    degisim_islemlerini_karsilastir(DegisimZamani, DegisimID, Islemler),
    % Sonra kalan degisim kayıtlarına geçelim
    bilgi_degisim_kontrol(KalanDegisimler, Islemler).

/*
 * 3) degisim_islemlerini_karsilastir/3
 *    => Tek bir degisim kaydını, tüm islem kayıtlarıyla karşılaştırır.
 *       Eşleşen (fark ≤5) bulursak alert mesajı verir.
 *       Hiçbiri uymuyorsa sadece debug bir mesaj olabilir.
 */
degisim_islemlerini_karsilastir(_, _, []) :-
    debug_message('Bu degisim için yakın zamanlı işlem bulunamadı.'),
    !.

degisim_islemlerini_karsilastir(DegisimZamani, DegisimID, [(IslemZamani, IslemID)|KalanIslemler]) :-
    abs(IslemZamani - DegisimZamani) =< 5,
    alert_message(
        'Şüpheli işlem tespit edildi: Bilgi Değişikliği ID: ~w => İşlem ID: ~w (D:~w, I:~w)',
        [DegisimID, IslemID, DegisimZamani, IslemZamani]
    ),
    % Sonraki islem kaydına da bakmaya devam ederiz (farklı ID, belki daha çok olay)
    degisim_islemlerini_karsilastir(DegisimZamani, DegisimID, KalanIslemler).

degisim_islemlerini_karsilastir(DegisimZamani, DegisimID, [_|KalanIslemler]) :-
    % Burada abs(fark) > 5 demek
    degisim_islemlerini_karsilastir(DegisimZamani, DegisimID, KalanIslemler).

/*
 * 4) test_bilgi_degisim_riski/0
 *    => Belirli kullanıcılar üzerinde toplu test yapar.
 *       Her kullanıcıda "degisim -> islem" şüpheli durum var mı" diye bakar.
 */
test_bilgi_degisim_riski :-
    writeln('--- [TEST] Kural 14: Bilgi Değişikliği Sonrası İşlem Kontrolü Başlıyor... ---'),
    set_debug(true),
    forall(
        member(Kullanici, [kullanici1, kullanici4, kullanici5, kullanici6]),
        (
            writeln('----------------------------------'),
            format('Kullanıcı: ~w~n', [Kullanici]),
            (   bilgi_degisim_riski(Kullanici)
            ->  format(' - Kontrol tamamlandı, olası şüpheli işlemler üstte listelenir.~n', [])
            ;   format(' - Kontrol başarısız veya veri yok.~n', [])
            )
        )
    ),
    set_debug(false),
    writeln('----------------------------------'),
    writeln('--- [TEST] Tamamlandı. ---').
