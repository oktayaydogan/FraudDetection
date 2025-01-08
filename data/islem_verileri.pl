:- module(islem_verileri, [islem/6]).

% İşlem kayıtları: Kullanıcı, İşlem Miktarı, Zaman Damgası, Konum, Cihaz, Davranış Süresi
islem(kullanici1, 500, 10, 'Türkiye', 'Chrome', 30). % Ortalama süreye yakın
islem(kullanici1, 300, 12, 'Türkiye', 'Chrome', 28).
islem(kullanici1, 200, 15, 'Türkiye', 'Chrome', 35).
islem(kullanici1, 100, 20, 'Türkiye', 'Safari', 5).  % Çok hızlı işlem
islem(kullanici1, 5000, 25, 'Almanya', 'Chrome', 150). % Çok yavaş işlem

