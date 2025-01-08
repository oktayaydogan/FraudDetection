:- module(ortak_ip_kullanimi, [ayni_ip_kontrol/2]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor

% Belirli bir IP adresinden işlem yapan kullanıcıları listeleme
ip_adresinden_kullanicilar(IP, KullaniciListesi) :-
    findall(Kullanici, islem(Kullanici, _, _, _, _, _, IP), TumKullanicilar),
    list_to_set(TumKullanicilar, KullaniciListesi), % Eşsiz kullanıcılar
    writeln(['IP adresinden işlem yapan kullanıcılar:', IP, '=>', KullaniciListesi]). % Debugging

% Aynı IP adresinden birden fazla kullanıcı işlem yapmış mı kontrol et
ayni_ip_kontrol(IP, Sonuc) :-
    ip_adresinden_kullanicilar(IP, KullaniciListesi),
    length(KullaniciListesi, KullaniciSayisi),
    writeln(['Kullanıcı sayısı:', KullaniciSayisi]), % Debugging
    (KullaniciSayisi > 1 ->
        Sonuc = 'Şüpheli: Aynı IP üzerinden birden fazla kullanıcı işlem yaptı.';
        Sonuc = 'Normal: Aynı IP üzerinden yalnızca bir kullanıcı işlem yaptı.'
    ).
