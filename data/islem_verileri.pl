:- module(islem_verileri, [islem/11]).

% İşlem kayıtları: ID, Kullanıcı, İşlem Miktarı, Zaman Damgası, Konum, Cihaz, Davranış Süresi, İşlem Türü, IP Adresi, Ödeme Yöntemi, Alan (E-posta veya Telefon)

% Kullanıcı 1
islem(1, kullanici1, 500, 10, 'Türkiye', 'Chrome', 30, 'islem', '192.168.1.1', 'Kredi Kartı', 'user1@example.com').
islem(2, kullanici1, 300, 15, 'Türkiye', 'Chrome', 28, 'islem', '192.168.1.1', 'Banka Kartı', 'user1@example.com').
islem(3, kullanici1, 200, 20, 'ABD', 'Edge', 35, 'islem', '10.0.0.2', 'Havale', 'user1@example.com').
islem(4, kullanici1, 1000, 25, 'Türkiye', 'Firefox', 45, 'islem', '192.168.1.1', 'E-Cüzdan', 'user1@example.com').
islem(5, kullanici1, 0, 8, 'Türkiye', 'Safari', 20, 'degisim', '192.168.1.1', 'Banka Kartı', 'user1@example.com'). % Bilgi değişikliği

% Kullanıcı 2
islem(6, kullanici2, 700, 12, 'Türkiye', 'Chrome', 25, 'islem', '192.168.1.1', 'Kredi Kartı', 'user2@example.com').
islem(7, kullanici2, 3000, 18, 'Türkiye', 'Safari', 50, 'islem', '192.168.1.1', 'Banka Kartı', 'user2@example.com').
islem(8, kullanici2, 0, 20, 'Türkiye', 'Edge', 15, 'degisim', '192.168.1.1', 'Havale', 'user2@example.com'). % Bilgi değişikliği
islem(9, kullanici2, 1500, 22, 'Türkiye', 'Edge', 40, 'islem', '192.168.1.1', 'Havale', 'user1@example.com'). % Hızlı işlem sonrası

% Kullanıcı 3
islem(10, kullanici3, 500, 5, 'ABD', 'Firefox', 20, 'islem', '10.0.0.2', 'E-Cüzdan', 'user3@example.com').
islem(11, kullanici3, 600, 7, 'ABD', 'Firefox', 18, 'iade', '10.0.0.2', 'E-Cüzdan', 'user3@example.com'). % Para iadesi
islem(12, kullanici3, 400, 9, 'Türkiye', 'Chrome', 22, 'islem', '192.168.1.1', 'Kredi Kartı', 'user2@example.com'). % Farklı konum
islem(13, kullanici3, 100, 14, 'Türkiye', 'Chrome', 15, 'islem', '192.168.1.1', 'Kredi Kartı', 'user3@example.com').

% Kullanıcı 4
islem(14, kullanici4, 1000, 30, 'Fransa', 'Safari', 40, 'islem', '10.0.0.3', 'Banka Kartı', 'user4@example.com').
islem(15, kullanici4, 0, 28, 'Fransa', 'Safari', 35, 'degisim', '10.0.0.3', 'Banka Kartı', 'user4@example.com'). % Bilgi değişikliği
islem(16, kullanici4, 1500, 32, 'Almanya', 'Safari', 50, 'islem', '10.0.0.3', 'Banka Kartı', 'user4@example.com'). % Farklı konum

% Ekstra senaryolar
islem(17, kullanici5, 2000, 50, 'Türkiye', 'Chrome', 60, 'islem', '192.168.1.5', 'Kredi Kartı', 'user5@example.com'). % Yüksek işlem miktarı
islem(18, kullanici5, 100, 52, 'Türkiye', 'Safari', 25, 'iade', '192.168.1.5', 'Banka Kartı', 'user5@example.com'). % Para iadesi
islem(19, kullanici5, 0, 51, 'Türkiye', 'Safari', 20, 'degisim', '192.168.1.5', 'Banka Kartı', 'user5@example.com'). % Bilgi değişikliği

% Kullanıcı 6: Tutarlı ve normal işlemler
islem(20, kullanici6, 500, 10, 'Türkiye', 'Chrome', 30, 'islem', '192.168.1.10', 'Kredi Kartı', 'user6@example.com').
islem(21, kullanici6, 700, 15, 'Türkiye', 'Chrome', 32, 'islem', '192.168.1.10', 'Kredi Kartı', 'user6@example.com').
islem(22, kullanici6, 800, 20, 'Türkiye', 'Chrome', 35, 'islem', '192.168.1.10', 'Kredi Kartı', 'user6@example.com').
islem(23, kullanici6, 900, 25, 'Türkiye', 'Chrome', 40, 'islem', '192.168.1.10', 'Kredi Kartı', 'user6@example.com').

% Kullanıcı 7: Aynı cihaz ve konumda tekrar eden işlemler
islem(24, kullanici7, 300, 5, 'ABD', 'Firefox', 20, 'islem', '10.0.0.20', 'Havale', 'user7@example.com').
islem(25, kullanici7, 400, 10, 'ABD', 'Firefox', 25, 'islem', '10.0.0.20', 'Havale', 'user7@example.com').
islem(26, kullanici7, 500, 15, 'ABD', 'Firefox', 30, 'islem', '10.0.0.20', 'Havale', 'user7@example.com').
islem(27, kullanici7, 600, 20, 'ABD', 'Firefox', 35, 'islem', '10.0.0.20', 'Havale', 'user7@example.com').

% Kullanıcı 8: Ödeme yöntemi değişikliği ancak tutarlı işlem davranışı
islem(28, kullanici8, 200, 10, 'Türkiye', 'Safari', 20, 'islem', '192.168.1.20', 'Kredi Kartı', 'user8@example.com').
islem(29, kullanici8, 300, 15, 'Türkiye', 'Safari', 25, 'islem', '192.168.1.20', 'Banka Kartı', 'user8@example.com').
islem(30, kullanici8, 400, 20, 'Türkiye', 'Safari', 30, 'islem', '192.168.1.20', 'Havale', 'user8@example.com').
islem(31, kullanici8, 500, 25, 'Türkiye', 'Safari', 35, 'islem', '192.168.1.20', 'E-Cüzdan', 'user8@example.com').

% Kullanıcı 9: Küçük miktarlarda düzenli işlemler
islem(32, kullanici9, 50, 5, 'Türkiye', 'Edge', 15, 'islem', '192.168.1.30', 'Kredi Kartı', 'user9@example.com').
islem(33, kullanici9, 60, 10, 'Türkiye', 'Edge', 18, 'islem', '192.168.1.30', 'Kredi Kartı', 'user9@example.com').
islem(34, kullanici9, 70, 15, 'Türkiye', 'Edge', 20, 'islem', '192.168.1.30', 'Kredi Kartı', 'user9@example.com').
islem(35, kullanici9, 80, 20, 'Türkiye', 'Edge', 22, 'islem', '192.168.1.30', 'Kredi Kartı', 'user9@example.com').

