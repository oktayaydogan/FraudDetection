:- use_module('data/islem_verileri'). % Veriler
:- use_module('rules/islem_sikligi'). % İşlem sıklığı kuralları
:- use_module('rules/islem_miktari'). % İşlem miktarı kuralları
:- use_module('rules/islem_konumu'). % İşlem konumu kuralları

% Merkezi sorgu
sorgula :-
    writeln('1. Kullanıcının işlem sayısı:'),
    islem_sikligi:islem_sayisi(kullanici1, 0, 24, Sayi),
    writeln(Sayi),

    writeln('2. Kullanıcının şüpheli işlem durumu:'),
    (islem_sikligi:supheli_islem(kullanici1, 0, 24) ->
        writeln('Şüpheli işlem var!');
        writeln('Şüpheli işlem yok.')),

    writeln('3. Anormal işlem durumu:'),
    (islem_miktari:anormal_islem(kullanici1, 5000) ->
        writeln('Anormal işlem tespit edildi!');
        writeln('Anormal işlem yok.')),

    writeln('4. Konum uyuşmazlığı kontrolü:'),
    (islem_konumu:konum_uyusmazligi(kullanici1) ->
        writeln('Konum uyuşmazlığı var!');
        writeln('Konum uyuşmazlığı yok.')).
