:- use_module('data/islem_verileri'). % Veriler
:- use_module('rules/islem_sikligi'). % İşlem sıklığı kuralları
:- use_module('rules/islem_miktari'). % İşlem miktarı kuralları
:- use_module('rules/islem_konumu'). % İşlem konumu kuralları
:- use_module('rules/farkli_konum'). % Farklı konum kuralları
:- use_module('rules/yeni_cihaz'). % Yeni cihaz kuralları
:- use_module('rules/davranis_analizi'). % Kullanıcı davranış analizi
:- use_module('rules/ortak_ip_kullanimi'). % Aynı IP adresinden farklı kullanıcı kontrolü

sorgula(Kullanici) :-
    format('~nKullanıcı: ~w~n', [Kullanici]),
    islem_sikligi:islem_sayisi(Kullanici, 0, 24, Sayi),
    format('1. Kullanıcının işlem sayısı: ~w~n', [Sayi]),
    writeln('-----------------------------------'),

    writeln('2. Kullanıcının şüpheli işlem durumu:'),
    (islem_sikligi:supheli_islem(Kullanici, 0, 24) ->
        writeln('-> Şüpheli işlem var!');
        writeln('-> Şüpheli işlem yok.')),
    writeln('-----------------------------------'),

    writeln('3. Anormal işlem durumu:'),
    (islem_miktari:anormal_islem(Kullanici, 5000) ->
        writeln('-> Anormal işlem tespit edildi!');
        writeln('-> Anormal işlem yok.')),
    writeln('-----------------------------------'),

    writeln('4. Konum uyuşmazlığı kontrolü:'),
    (islem_konumu:konum_uyusmazligi(Kullanici) ->
        writeln('-> Konum uyuşmazlığı var!');
        writeln('-> Konum uyuşmazlığı yok.')),
    writeln('-----------------------------------'),

    writeln('5. Farklı konumlarda kısa süreli işlem risk skoru:'),
    farkli_konum:farkli_konum_risk(Kullanici, Risk),
    format('-> Toplam risk puanı: ~w~n', [Risk]),
    (Risk > 10 -> writeln('-> Risk eşik değeri aşıldı: Şüpheli işlem!'); writeln('-> Risk eşik değeri aşılmadı: Normal işlem.')),
    writeln('-----------------------------------'),

    writeln('6. Yeni cihaz tespiti:'),
    (yeni_cihaz:yeni_cihaz_tespiti(Kullanici) ->
        writeln('-> Yeni cihazdan işlem tespit edildi!');
        writeln('-> Yeni cihaz kullanılmadı.')),
    writeln('-----------------------------------'),

    writeln('7. Kullanıcı davranış sapması kontrolü:'),
    (davranis_analizi:davranis_sapmasi(Kullanici) ->
        writeln('-> Davranış süresi normalden sapıyor!');
        writeln('-> Davranış süresi normal.')),
    writeln('-----------------------------------'),

    writeln('8. Aynı IP adresinden işlem kontrolü:'),
    findall(IP, islem(_, _, _, _, _, _, IP), IPListesi),
    list_to_set(IPListesi, UnikIPListesi),
    forall(member(IP, UnikIPListesi), (
        ayni_ip_farkli_kullanici:ayni_ip_kontrol(IP, Sonuc),
        format('-> IP: ~w => ~w~n', [IP, Sonuc])
    )).

tüm_kullanicilari_sorgula :-
    findall(Kullanici, islem(Kullanici, _, _, _, _, _, _), KullaniciListesi),
    list_to_set(KullaniciListesi, UnikKullanicilar),
    forall(member(Kullanici, UnikKullanicilar), sorgula(Kullanici)).
