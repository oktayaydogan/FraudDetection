:- module(islem_verileri, [islem/6]).

% İşlem kayıtları: Kullanıcı, İşlem Miktarı, Zaman Damgası, Konum, Cihaz, Davranış Süresi
islem(kullanici1, 500, 10, 'Türkiye', 'Chrome', 30).
islem(kullanici1, 300, 12, 'Almanya', 'Chrome', 28).  % Türkiye => Almanya (risk)
islem(kullanici1, 200, 14, 'Türkiye', 'Chrome', 35).  % Almanya => Türkiye (risk)
islem(kullanici1, 100, 16, 'ABD', 'Safari', 5).       % Türkiye => ABD (risk)
islem(kullanici1, 5000, 18, 'Fransa', 'Chrome', 150). % ABD => Fransa (risk)
