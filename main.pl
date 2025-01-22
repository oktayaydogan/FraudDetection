:- use_module('chatgpt').
:- use_module('fraud_detection').

% Kullanıcı Sorgulama Predikatı
kullanici_sorgula :-
    writeln("Kullanıcı adı giriniz: "),
    read_line_to_string(user_input, User),
    (   gpt_kullanici_sorgula(User, GptPuan)
        ->  
            writeln("---------------------------------------------------------"),
            format("| Kullanıcı ~w için GPT dolandırıcılık riski puanı: ~w |~n", [User, GptPuan]),
            writeln("---------------------------------------------------------")
        ;   
            writeln("Belirtilen kullanıcı bulunamadı veya analiz yapılamadı.")
    ),
    (   fraud_kullanici_sorgula(User, FraudPuan)
        ->  
            writeln("---------------------------------------------------------"),
            format("| Kullanıcı ~w için PL dolandırıcılık riski puanı: ~w |~n", [User, FraudPuan]),
            writeln("---------------------------------------------------------")
        ;   
            writeln("Belirtilen kullanıcı bulunamadı veya analiz yapılamadı.")
    ).

% İşlem Sorgulama Predikatı
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
                writeln("-------------------------------------------------------"),
                format("| İşlem ID: ~w için PL dolandırıcılık riski puanı: ~w |~n", [TxIDNum, FraudPuan]),
                writeln("-------------------------------------------------------")
                ;   
                writeln("Belirtilen işlem ID bulunamadı veya analiz yapılamadı.")
            )
        )
    ;   writeln("Geçersiz işlem ID formatı. Lütfen bir sayı giriniz.")
    ).

% Ana Menü Predikatı
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
