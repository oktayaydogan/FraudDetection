% dolandiricilik_analiz_sistemi.pl
%
% Açıklama:
%   Bu modül, kullanıcılar ve işlemler üzerinde dolandırıcılık riski analizi yapar.
%   Sistem, kullanıcıların ve işlemlerin dolandırıcılık risk puanlarını hem GPT
%   tabanlı bir model hem de yerel bir dolandırıcılık tespit modülü (fraud_detection)
%   kullanarak hesaplar. Kullanıcılar, bir menü aracılığıyla kullanıcı veya işlem
%   sorgulaması yapabilir ve sonuçları görüntüleyebilir.
%
% Kullanım:
%   1) Prolog ortamında bu dosyayı yükleyin:
%      ?- [dolandiricilik_analiz_sistemi].
%
%   2) Program otomatik olarak ana menüyü başlatır. Kullanıcıdan seçim yapması istenir:
%      - 1: Kullanıcı sorgulama
%      - 2: İşlem sorgulama
%      - 3: Çıkış
%
% Gereksinimler:
%   - 'chatgpt.pl' ve 'fraud_detection.pl' modüllerinin yüklenmiş olması.
%   - Bu modüllerin `gpt_kullanici_sorgula/2`, `gpt_islem_sorgula/2`,
%     `fraud_kullanici_sorgula/2`, `fraud_islem_sorgula/2` predikatlarını tanımlaması.
%
% Sınırlamalar:
%   - Kullanıcı ve işlem sorgulamaları sırasında hatalı girişlerde kullanıcıya bilgi verilir.
%   - Sistem, sadece belirli bir formatla (sayı veya string) giriş yapılmasını bekler.
%
% Gelecek Geliştirmeler:
%   - Daha detaylı raporlama ve loglama özellikleri eklenebilir.
%   - Kullanıcı dostu bir arayüz (GUI) eklenebilir.
%
% Modül Tanımı ve İhracı:
:- use_module('chatgpt').
:- use_module('fraud_detection').

% ----------------------------------------------------------------------
% kullanici_sorgula/0
%
% Açıklama:
%   Kullanıcıdan bir kullanıcı adı alır ve bu kullanıcı için GPT ve yerel dolandırıcılık
%   tespit modüllerinden risk puanlarını hesaplar. Sonuçları ekrana yazdırır.
%
% Örnek Kullanım:
%   ?- kullanici_sorgula.
%   Kullanıcı adı giriniz: kullanici1
%   -----------------------------------------------------------
%   | Kullanıcı kullanici1 için GPT dolandırıcılık riski puanı: 75 |
%   -----------------------------------------------------------
%   | Kullanıcı kullanici1 için PL dolandırıcılık riski puanı: 80  |
%   -----------------------------------------------------------
% ----------------------------------------------------------------------
kullanici_sorgula :-
    writeln("Kullanıcı adı giriniz: "),
    read_line_to_string(user_input, User),
    (   gpt_kullanici_sorgula(User, GptPuan)
        ->  
            writeln("-----------------------------------------------------------"),
            format("| Kullanıcı ~w için GPT dolandırıcılık riski puanı: ~w |~n", [User, GptPuan]),
            writeln("-----------------------------------------------------------")
        ;   
            writeln("Belirtilen kullanıcı bulunamadı veya analiz yapılamadı.")
    ),
    (   fraud_kullanici_sorgula(User, FraudPuan)
        ->  
            writeln("----------------------------------------------------------"),
            format("| Kullanıcı ~w için PL dolandırıcılık riski puanı: ~w |~n", [User, FraudPuan]),
            writeln("----------------------------------------------------------")
        ;   
            writeln("Belirtilen kullanıcı bulunamadı veya analiz yapılamadı.")
    ).

% ----------------------------------------------------------------------
% islem_sorgula/0
%
% Açıklama:
%   Kullanıcıdan bir işlem ID'si alır ve bu işlem için GPT ve yerel dolandırıcılık
%   tespit modüllerinden risk puanlarını hesaplar. Sonuçları ekrana yazdırır.
%
% Örnek Kullanım:
%   ?- islem_sorgula.
%   İşlem ID giriniz: 12345
%   -------------------------------------------------------
%   | İşlem ID: 12345 için GPT dolandırıcılık riski puanı: 65 |
%   -------------------------------------------------------
%   | İşlem ID: 12345 için PL dolandırıcılık riski puanı: 70  |
%   -------------------------------------------------------
% ----------------------------------------------------------------------
islem_sorgula :-
    writeln("İşlem ID giriniz: "),
    read_line_to_string(user_input, TxIDStr),
    (   atom_number(TxIDStr, TxIDNum)
    ->  (   
            (
                gpt_islem_sorgula(TxIDNum, GptPuan)
                ->    
                writeln("-------------------------------------------------------"),
                format("| İşlem ID: ~w için GPT dolandırıcılık riski puanı: ~w |~n", [TxIDNum, GptPuan]),
                writeln("-------------------------------------------------------")
                ;   
                writeln("Belirtilen işlem ID bulunamadı veya analiz yapılamadı.")
            )
            ,
            (
                fraud_islem_sorgula(TxIDNum, FraudPuan)
                ->    
                writeln("------------------------------------------------------"),
                format("| İşlem ID: ~w için PL dolandırıcılık riski puanı: ~w |~n", [TxIDNum, FraudPuan]),
                writeln("------------------------------------------------------")
                ;   
                writeln("Belirtilen işlem ID bulunamadı veya analiz yapılamadı.")
            )
        )
    ;   writeln("Geçersiz işlem ID formatı. Lütfen bir sayı giriniz.")
    ).

% ----------------------------------------------------------------------
% ana_menu/0
%
% Açıklama:
%   Ana menüyü görüntüler ve kullanıcıdan bir seçim yapmasını ister. Seçime göre
%   ilgili sorgulama predikatını çağırır veya programdan çıkar.
%
% Örnek Kullanım:
%   ?- ana_menu.
%   ===== Dolandırıcılık Riski Analiz Sistemi =====
%   1. Kullanıcı Sorgulama
%   2. İşlem Sorgulama
%   3. Çıkış
%   Seçiminizi yapınız (1/2/3): 1
% ----------------------------------------------------------------------
ana_menu :-
    writeln("===== Dolandırıcılık Riski Analiz Sistemi ====="),
    writeln("1. Kullanıcı Sorgulama"),
    writeln("2. İşlem Sorgulama"),
    writeln("3. Çıkış"),
    writeln("Seçiminizi yapınız (1/2/3): "),
    read_line_to_string(user_input, Secim),
    (   Secim = "1"
    ->  kullanici_sorgula, devam_mı
    ;   Secim = "2"
    ->  islem_sorgula, devam_mı
    ;   Secim = "3"
    ->  writeln("Çıkış yapılıyor..."),
        !
    ;   writeln("Geçersiz seçim. Lütfen tekrar deneyiniz."),
        ana_menu
    ).

% ----------------------------------------------------------------------
% devam_mı/0
%
% Açıklama:
%   Kullanıcıya devam edip etmeyeceğini sorar. Eğer kullanıcı devam etmek isterse
%   ana menüye döner, aksi takdirde programdan çıkar.
%
% Örnek Kullanım:
%   ?- devam_mı.
%   Devam etmek istiyor musunuz? (e/h)
% ----------------------------------------------------------------------
devam_mı:-
    writeln("Devam etmek istiyor musunuz? (e/h)"),
    read_line_to_string(user_input, Cevap),
    (   Cevap = "e"
    ->  ana_menu
    ;   Cevap = "h"
    ->  writeln("Çıkış yapılıyor..."),
        !
    ;   writeln("Geçersiz seçim. Lütfen tekrar deneyiniz."),
        devam_mı
    ).

% Programın Başlangıç Noktası
:- initialization(ana_menu).