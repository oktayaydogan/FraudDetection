:- module(islem_verileri, [islem/4]).

% İşlem kayıtları: Kullanıcı, İşlem Miktarı, Zaman Damgası, Konum
islem(kullanici1, 500, 10, 'Türkiye').
islem(kullanici1, 300, 12, 'Türkiye').
islem(kullanici1, 200, 15, 'Türkiye').
islem(kullanici1, 100, 20, 'Türkiye').
islem(kullanici1, 5000, 25, 'Almanya'). % Farklı konum
islem(kullanici2, 700, 5, 'ABD').
islem(kullanici2, 800, 8, 'ABD').
islem(kullanici1, 500, 10, 'Türkiye').
islem(kullanici1, 300, 12, 'Almanya'). % Farklı konum, kısa süre
islem(kullanici1, 200, 20, 'Türkiye').
islem(kullanici1, 100, 22, 'ABD'). % Farklı konum, kısa süre
islem(kullanici1, 5000, 35, 'Türkiye').
