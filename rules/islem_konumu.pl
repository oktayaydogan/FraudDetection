:- module(islem_konumu, [konum_uyusmazligi/1, test_konum_uyusmazligi/0]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug'). % Debug mesajları
:- use_module('../utils/alert'). % Uyarı mesajları

% Kullanıcının önceki işlemlerindeki konumları listeleme
kullanici_konumlari(Kullanici, Konumlar) :-
    findall(Konum, islem(_, Kullanici, _, _, Konum, _, _, _, _, _, _), Konumlar),
    debug_message('Kullanıcı konumları: ~w => ~w', [Kullanici, Konumlar]).

% Konum uyuşmazlığını kontrol eden kural
konum_uyusmazligi(Kullanici) :-
    kullanici_konumlari(Kullanici, Konumlar),
    list_to_set(Konumlar, UnikKonumlar), % Konumların benzersiz listesini oluştur
    length(UnikKonumlar, Say),
    debug_message('Farklı konum sayısı: ~w => ~w', [Kullanici, Say]),
    Say > 1, % Birden fazla farklı konum varsa uyuşmazlık var
    alert_message('Konum uyuşmazlığı tespit edildi!').

% Test konum uyuşmazlığı
test_konum_uyusmazligi :-
    writeln('Test: konum_uyusmazligi kontrolü başlıyor...'),
    set_debug(true),
    forall(member(Kullanici, [kullanici1, kullanici2, kullanici3, kullanici4]),
           (writeln('----------------------------------'),
            (konum_uyusmazligi(Kullanici) ->
                format('Kullanıcı: ~w, Konum uyuşmazlığı tespit edildi.~n', [Kullanici]);
                format('Kullanıcı: ~w, Konum uyuşmazlığı tespit edilemedi.~n', [Kullanici])))),
    set_debug(false),
    writeln('----------------------------------'),
    writeln('Test tamamlandı.').