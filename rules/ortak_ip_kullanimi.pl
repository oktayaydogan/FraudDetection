:- module(ortak_ip_kullanimi, [ayni_ip_kontrol/2, ip_adresinden_islemler/2]).
:- use_module('../data/islem_verileri'). % Veriler dahil ediliyor
:- use_module('../utils/debug'). % Debug mesajları
:- use_module('../utils/alert'). % Alert mesajları

% Belirli bir IP adresinden işlem yapan kullanıcıları listeleme
ip_adresinden_kullanicilar(IP, KullaniciListesi) :-
    findall(Kullanici, islem(_, Kullanici, _, _, _, _, _, _, IP, _, _), TumKullanicilar),
    list_to_set(TumKullanicilar, KullaniciListesi), % Eşsiz kullanıcılar
    debug_message('IP adresinden işlem yapan kullanıcılar: ~w => ~w', [IP, KullaniciListesi]).

% Aynı IP adresinden birden fazla kullanıcı işlem yapmış mı kontrol et
ayni_ip_kontrol(IP, Sonuc) :-
    ip_adresinden_kullanicilar(IP, KullaniciListesi),
    length(KullaniciListesi, KullaniciSayisi),
    debug_message('Kullanıcı sayısı: ~w', [KullaniciSayisi]),
    (KullaniciSayisi > 1 ->
        alert_message('Şüpheli: Aynı IP üzerinden birden fazla kullanıcı işlem yaptı. IP: ~w, Kullanıcılar: ~w', 
                      [IP, KullaniciListesi]),
        Sonuc = 'Şüpheli';
        debug_message('Normal: Aynı IP üzerinden yalnızca bir kullanıcı işlem yaptı.'),
        Sonuc = 'Normal').

% Belirtilen IP adresinden yapılan tüm işlemleri listeleme
ip_adresinden_islemler(IP, Islemler) :-
    findall((ID, Kullanici, Zaman, Cihaz, OdemeYontemi),
        islem(ID, Kullanici, _, Zaman, _, Cihaz, _, _, IP, OdemeYontemi, _),
        Islemler),
    debug_message('IP adresinden yapılan işlemler: ~w => ~w', [IP, Islemler]).

% Test sorgusu
% ortak_ip_kullanimi:ayni_ip_kontrol('192.168.1.1', Sonuc).
% ortak_ip_kullanimi:ayni_ip_kontrol('10.0.0.2', Sonuc).
% ortak_ip_kullanimi:ip_adresinden_islemler('192.168.1.1', Islemler).
% ortak_ip_kullanimi:ip_adresinden_islemler('10.0.0.3', Islemler).
