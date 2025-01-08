:- use_module('data/islem_verileri'). % Veriler
:- use_module('rules/islem_sikligi'). % İşlem sıklığı kuralları
:- use_module('rules/islem_miktari'). % İşlem miktarı kuralları
:- use_module('rules/islem_konumu'). % İşlem konumu kuralları
:- use_module('rules/farkli_konum'). % Farklı konum kuralları
:- use_module('rules/yeni_cihaz'). % Yeni cihaz kuralları
:- use_module('rules/davranis_analizi'). % Kullanıcı davranış analizi

% Merkezi sorgu
sorgula :-
    writeln('1. Kullanıcının işlem sayısı:'),
    islem_sikligi:islem_sayisi(kullanici1, 0, 24, Sayi),
    writeln(Sayi),

    writeln('-----------------------------------'),

    writeln('2. Kullanıcının şüpheli işlem durumu:'),
    (islem_sikligi:supheli_islem(kullanici1, 0, 24) ->
        writeln('Şüpheli işlem var!');
        writeln('Şüpheli işlem yok.')),

    writeln('-----------------------------------'),

    writeln('3. Anormal işlem durumu:'),
    (islem_miktari:anormal_islem(kullanici1, 5000) ->
        writeln('Anormal işlem tespit edildi!');
        writeln('Anormal işlem yok.')),

    writeln('-----------------------------------'),

    writeln('4. Konum uyuşmazlığı kontrolü:'),
    (islem_konumu:konum_uyusmazligi(kullanici1) ->
        writeln('Konum uyuşmazlığı var!');
        writeln('Konum uyuşmazlığı yok.')),

    writeln('-----------------------------------'),

    writeln('5. Farklı konumlarda kısa süreli işlem risk skoru:'),
    farkli_konum:farkli_konum_risk(kullanici1, Risk),
    format('Toplam risk puanı: ~w~n', [Risk]),
    (Risk > 10 -> writeln('Risk eşik değeri aşıldı: Şüpheli işlem!'); writeln('Risk eşik değeri aşılmadı: Normal işlem.')),

    writeln('-----------------------------------'),

    writeln('6. Yeni cihaz tespiti:'),
    (yeni_cihaz:yeni_cihaz_tespiti(kullanici1) ->
        writeln('Yeni cihazdan işlem tespit edildi!');
        writeln('Yeni cihaz kullanılmadı.')),

    writeln('-----------------------------------'),

    writeln('7. Kullanıcı davranış sapması kontrolü:'),
    (davranis_analizi:davranis_sapmasi(kullanici1) ->
        writeln('Davranış süresi normalden sapıyor!');
        writeln('Davranış süresi normal.')).
