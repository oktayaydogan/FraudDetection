:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(pcre)). % Regex desteği için

/*
  Bu kod SWI-Prolog'da çalışır. "run." yazarak
  ChatGPT ile etkileşimli bir şekilde konuşmaya başlayabilirsiniz.

  - conversation/1: Konuşma geçmişini dinamik olarak saklar.
  - chatgpt_api/2: Kullanıcıdan alınan mesaja göre API'ye istek atar.
  - explain_response/2: Dönen JSON cevaptan asistan mesajını ayıklar.
  - kullanici_sorusuna_gore_bilgi/2: Kullanıcının sorusuna göre bilgi çeker.
  - load_json_files/0: JSON dosyalarını yükler.
  - chat/0: Kullanıcı etkileşimini yönetir (exit yazarak çıkılabilir).
  - run/0: Konuşmayı başlatır.
*/

:- dynamic conversation/1.
:- dynamic islem_verisi/1.
:- dynamic kural/1.

% Konuşma geçmişine başlangıç olarak 'system' mesajı ekliyoruz.
conversation([
    _{ role: "system", content: "Sen bir fraud uzmanısın. Sana verilen kuralları ve verileri kullanarak işlemlerin dolandırıcılık risk analizini yapıyorsun. Analiz sonucunda işleme 100 üzerinden bir puan veriyorsun." }
]).

%----------------------------------------------------------
% JSON Dosyalarını Yükleme
%----------------------------------------------------------
load_json_files :-
    writeln("JSON dosyaları yükleniyor..."),
    (   load_islem_verileri
    ->  writeln("İşlem verileri başarıyla yüklendi.")
    ;   writeln("İşlem verilerini yüklerken hata oluştu."),
        fail
    ),
    (   load_kurallar
    ->  writeln("Kurallar başarıyla yüklendi.")
    ;   writeln("Kuralları yüklerken hata oluştu."),
        fail
    ).

% islem_verileri.json dosyasını yükler
load_islem_verileri :-
    writeln("islem_verileri.json dosyası okunuyor..."),
    (   catch(
            open('data/islem_verileri.json', read, Stream),
            Error,
            (
                format("Hata: ~w~n", [Error]),
                fail
            )
        )
    ->  catch(
            json_read_dict(Stream, IslemDict),
            JsonError,
            (
                format("JSON Okuma Hatası: ~w~n", [JsonError]),
                close(Stream),
                fail
            )
        )
    ;   fail
    ),
    close(Stream),
    retractall(islem_verisi(_)),
    (   IslemDict = _{islemler: IslemList},
        maplist(assert_islem_verisi, IslemList)
    ->  writeln("İşlem verileri Prolog faktları olarak eklendi.")
    ;   writeln("İşlem verilerini assert ederken hata oluştu."),
        fail
    ).

% islem_verisi'yı assert eder
assert_islem_verisi(Item) :-
    assertz(islem_verisi(Item)).

% kurallar.json dosyasını yükler
load_kurallar :-
    writeln("kurallar.json dosyası okunuyor..."),
    (   catch(
            open('data/kurallar.json', read, Stream),
            Error,
            (
                format("Hata: ~w~n", [Error]),
                fail
            )
        )
    ->  catch(
            json_read_dict(Stream, KuralDict),
            JsonError,
            (
                format("JSON Okuma Hatası: ~w~n", [JsonError]),
                close(Stream),
                fail
            )
        )
    ;   fail
    ),
    close(Stream),
    retractall(kural(_)),
    (   KuralDict = _{rules: KuralList},
        maplist(assert_kural, KuralList)
    ->  writeln("Kurallar Prolog faktları olarak eklendi.")
    ;   writeln("Kuralları assert ederken hata oluştu."),
        fail
    ).

% kural'ı assert eder
assert_kural(Item) :-
    assertz(kural(Item)).

%----------------------------------------------------------
% Bilgi Kaynakları
%----------------------------------------------------------

%----------------------------------------------------------
% kullanici_sorusuna_gore_bilgi/2
%
%  - Kullanıcının sorusuna göre ilgili bilgiyi çeker.
%  - İstenilen puanlama türüne göre işlem yapar.
%----------------------------------------------------------
kullanici_sorusuna_gore_bilgi(Soru, Bilgi) :-
    downcase_atom(Soru, SoruKucuk),
    (   sub_string(SoruKucuk, _, _, _, "kredi kartı")
    ->  findall(KStr, (islem_verisi(K), term_string(K, KStr)), IslemStrList),
        atomic_list_concat(IslemStrList, '\n', IslemStr),
        format(string(Bilgi), "Kredi Kartı Kullanılan İşlemler:\n~w", [IslemStr])

    ;   sub_string(SoruKucuk, _, _, _, "kural")
    ->  (   sub_string(SoruKucuk, _, _, _, "id")
        ->  get_kural_id(SoruKucuk, KuralId),
            (   integer(KuralId),
                kural(Kural),
                Kural.id = KuralId
            ->  term_string(Kural, KuralStr),
                string_concat("Kural Detayı:\n", KuralStr, Bilgi)
            ;   Bilgi = "Belirtilen ID'ye sahip bir kural bulunamadı."
            )
        ;   sub_string(SoruKucuk, _, _, _, "label")
        ->  get_kural_label(SoruKucuk, KuralLabel),
            (   atom_string(LabelAtom, KuralLabel),
                kural(Kural),
                Kural.label = LabelAtom
            ->  term_string(Kural, KuralStr),
                string_concat("Kural Detayı:\n", KuralStr, Bilgi)
            ;   Bilgi = "Belirtilen label'a sahip bir kural bulunamadı."
            )
        ;   findall(KStr, (kural(K), term_string(K, KStr)), KuralStrList),
            atomic_list_concat(KuralStrList, '\n', KuralStr),
            format(string(Bilgi), "Tüm Kurallar:\n~w", [KuralStr])
        )

    ;   sub_string(SoruKucuk, _, _, _, "kullanici puanla")
    ->  get_user_id_from_query(SoruKucuk, Kullanici),
        (   string_length(Kullanici, 0)
        ->  Bilgi = "Lütfen bir kullanıcı adı belirtin."
        ;   score_user_transactions(Kullanici, Scores),
            (   Scores \= []
            ->  format(string(Bilgi), "~w Kullanıcısının İşlemlerinin Risk Puanları:\n~w", [Kullanici, Scores])
            ;   format(string(Bilgi), "~w kullanıcısına ait işlem bulunamadı.", [Kullanici])
            )
        )

    ;   sub_string(SoruKucuk, _, _, _, "işlem puanla")
    ->  get_transaction_id_from_query(SoruKucuk, TransactionId),
        (   integer(TransactionId),
            islem_verisi(Transaction),
            Transaction.id = TransactionId
        ->  calculate_total_risk(Transaction, TotalRisk),
            format(string(Bilgi), "İşlem ID: ~w için Toplam Risk Puanı: ~w", [TransactionId, TotalRisk])
        ;   Bilgi = "Belirtilen ID'ye sahip bir işlem bulunamadı."
        )

    ;   sub_string(SoruKucuk, _, _, _, "risk puanı")
    ->  get_risk_puani(Soru, RiskPuani),
        (   integer(RiskPuani)
        ->  findall(KStr, (kural(K), K.risk_score >= RiskPuani, term_string(K, KStr)), RiskliKurallarStrList),
            (   RiskliKurallarStrList \= []
            ->  atomic_list_concat(RiskliKurallarStrList, '\n', RiskliKurallarStr),
                format(string(Bilgi), "~w ve üzeri risk puanına sahip kurallar:\n~w", [RiskPuani, RiskliKurallarStr])
            ;   format(string(Bilgi), "~w ve üzeri risk puanına sahip kural bulunamadı.", [RiskPuani])
            )
        ;   Bilgi = "Lütfen geçerli bir risk puanı belirtin."
        )

    ;   sub_string(SoruKucuk, _, _, _, "odeme yöntemi")
    ->  get_odeme_yontemi(SoruKucuk, OdemeYontemi),
        (   string_length(OdemeYontemi, 0)
        ->  Bilgi = "Lütfen bir ödeme yöntemi belirtin."
        ;   findall(KStr, (islem_verisi(K), K.odemeYontemi = OdemeYontemi, term_string(K, KStr)), OdemeYontemiIslemStrList),
            (   OdemeYontemiIslemStrList \= []
            ->  atomic_list_concat(OdemeYontemiIslemStrList, '\n', OdemeYontemiIslemStr),
                format(string(Bilgi), "~w Ödeme Yöntemi Kullanılan İşlemler:\n~w", [OdemeYontemi, OdemeYontemiIslemStr])
            ;   format(string(Bilgi), "~w ödeme yöntemiyle yapılmış işlem bulunamadı.", [OdemeYontemi])
            )
        )

    ;   Bilgi = "" % Tanımlı olmayan sorular için boş bırak
    ).

%----------------------------------------------------------
% Yardımcı Predikatlar
%----------------------------------------------------------

% Kullanıcı sorusundan kural ID'sini çıkarır
get_kural_id(Soru, KuralId) :-
    re_matchsub("id\\s*:\\s*(\\d+)", Soru, Sub, [capture(dotall)]),
    (   Sub.1 = KuralIdAtom,
        atom_number(KuralIdAtom, KuralId)
    ->  true
    ;   KuralId = -1 % Geçersiz ID
    ).

% Kullanıcı sorusundan kural label'ını çıkarır
get_kural_label(Soru, KuralLabel) :-
    re_matchsub("label\\s*:\\s*([\\w-]+)", Soru, Sub, [capture(dotall)]),
    (   Sub.1 = KuralLabel
    ->  true
    ;   KuralLabel = ""
    ).

% Kullanıcı sorusundan kullanıcı adını çıkarır
get_user_id_from_query(Soru, Kullanici) :-
    re_matchsub("kullanici\\s*puanla\\s*(\\w+)", Soru, Sub, [capture(dotall)]),
    (   Sub.1 = Kullanici
    ->  true
    ;   Kullanici = ""
    ).

% Kullanıcı sorusundan risk puanını çıkarır
get_risk_puani(Soru, RiskPuani) :-
    re_matchsub("risk puanı\\s*(\\d+)", Soru, Sub, [capture(dotall)]),
    (   Sub.1 = RiskPuaniAtom,
        atom_number(RiskPuaniAtom, RiskPuani)
    ->  true
    ;   RiskPuani = -1 % Geçersiz risk puanı
    ).

% Kullanıcı sorusundan ödeme yöntemini çıkarır
get_odeme_yontemi(Soru, OdemeYontemi) :-
    re_matchsub("odeme yöntemi\\s*:\\s*([\\w-]+)", Soru, Sub, [capture(dotall)]),
    (   Sub.1 = OdemeYontemi
    ->  true
    ;   OdemeYontemi = ""
    ).

% İşlem ID'sini sorgudan çıkarır
get_transaction_id_from_query(Soru, TransactionId) :-
    re_matchsub("işlem\\s*puanla\\s*(\\d+)", Soru, Sub, [capture(dotall)]),
    (   Sub.1 = TransactionIdAtom,
        atom_number(TransactionIdAtom, TransactionId)
    ->  true
    ;   TransactionId = -1 % Geçersiz ID
    ).

%----------------------------------------------------------
% Kural Uygulama ve Risk Puanı Hesaplama
%----------------------------------------------------------

% Genel kural uygulama predikatı
apply_rule_general(Kural, Transaction, RiskScore) :-
    Kural.risk_score = RiskScore,
    Kural.conditions = Conditions,
    % Her koşulu tek tek kontrol ediyoruz
    check_conditions(Conditions, Transaction).

% Koşulları kontrol eden predikat
check_conditions(Conditions, Transaction) :-
    forall(member(ConditionKey-ConditionValue, Conditions), 
           check_condition(ConditionKey, ConditionValue, Transaction)).

% Tek bir koşulu kontrol eden predikatlar
% Burada her koşul türüne göre kontrol ekleyebilirsiniz

% Örnek: High-Frequency Kuralı (ID:1)
check_condition(time_window_hours, Hours, Transaction) :-
    % 'Hours' burada the ConditionValue for 'time_window_hours'
    findall(T, (islem_verisi(T), 
               T.kullanici = Transaction.kullanici, 
               T.zaman >= Transaction.zaman - Hours * 3600), Transactions),
    length(Transactions, Count),
    % 'max_transactions' koşulunu burada manuel olarak ayarladık
    % Gerçek uygulamada, bu değer Kural.conditions içinden alınmalıdır
    MaxTransactions = 10,
    Count > MaxTransactions.

% Örnek: Sudden Amount Increase Kuralı (ID:2)
check_condition(current_amount_gt, Threshold, Transaction) :-
    findall(Miktar, (islem_verisi(T), 
                    T.kullanici = Transaction.kullanici, 
                    T.zaman < Transaction.zaman, 
                    Miktar = T.miktar), Amounts),
    max_list(Amounts, PreviousMax),
    PreviousMax < Threshold,
    Transaction.miktar > Threshold.

% Örnek: Multiple Accounts Same IP Kuralı (ID:3)
check_condition(ip_address, same, Transaction) :-
    % 'max_accounts' koşulunu burada manuel olarak ayarladık
    MaxAccounts = 5,
    findall(T, (islem_verisi(T),
               T.ipAdresi = Transaction.ipAdresi), Transactions),
    length(Transactions, Count),
    Count > MaxAccounts.

% Diğer koşullar için benzer kontroller ekleyin
% ...

% Bir işlem için toplam risk puanını ChatGPT'den alır
calculate_total_risk(Transaction, TotalRisk) :-
    % İşlem detaylarını formatla
    format_transaction(Transaction, TransactionDetails),

    % Kuralları formatla
    format_rules(RulesStr),

    % ChatGPT'ye göndermek üzere prompt hazırla
    format(string(Prompt), 
        "Aşağıda bir işlem detayları ve kurallar bulunmaktadır. Bu işlemin dolandırıcılık riski olup olmadığını değerlendir ve 100 üzerinden bir puan ver. Kuralları kullanarak analizi yap.\n\nİşlem Detayları:\n~w\n\nKurallar:\n~w", 
        [TransactionDetails, RulesStr]),

    % ChatGPT API'sine istek gönder ve yanıtı al
    chatgpt_api(Prompt, AssistantMessage),

    % Asistan mesajından risk puanını çıkar
    parse_risk_score(AssistantMessage, TotalRisk).

%----------------------------------------------------------
% Puanlama Fonksiyonları
%----------------------------------------------------------

% Kullanıcının tüm işlemleri için risk puanlarını hesaplar
score_user_transactions(UserId, TransactionScores) :-
    findall(Transaction, (islem_verisi(Transaction), Transaction.kullanici = UserId), Transactions),
    findall(Score, (member(T, Transactions), calculate_total_risk(T, Score)), TransactionScores).

%----------------------------------------------------------
% ChatGPT API Entegrasyonu
%----------------------------------------------------------

%----------------------------------------------------------
% chatgpt_api/2
%
%  - Kullanıcının mesajını alır (Message).
%  - RAG kullanarak ilgili bilgiyi ekler.
%  - OpenAI Chat API'ye istek atar.
%  - Dönen cevabı (Response) JSON olarak tutar, ayrıca ekrana yazdırır.
%----------------------------------------------------------
chatgpt_api(Message, Response) :-
    % API adresi
    URL = 'https://api.openai.com/v1/chat/completions',

    API_KEY = 'Bearer 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',

    % Mevcut konuşma geçmişini alıyoruz
    conversation(History),

    % Kullanıcının yeni mesajını konuşma geçmişine ekliyoruz
    append(History, [_{role: "user", content: Message}], NewHistory),

    % Gövde (Body) yapısı
    BODY = _{
        model: "gpt-4",
        messages: NewHistory
    },

    writeln("Gönderilen JSON Body:"),
    writeln(BODY),

    % HTTP isteğini 'catch/3' ile sarmalıyoruz ki hata durumunda yönetebilelim
    catch(
        http_post(
            URL,
            json(BODY),
            Reply,
            [
                request_header('Content-Type'='application/json'),
                request_header('Authorization'=API_KEY)
            ]
        ),
        Error,
        (
            writeln("API isteği başarısız oldu:"),
            writeln(Error),
            fail
        )
    ),

    % Dönen yanıtı JSON (term) biçiminde değişkene bağlamış oluyoruz
    Reply = json(JSONResponse),

    % Asistanın döndürdüğü cevabı ayıklıyoruz
    explain_response(JSONResponse, AssistantMessage),

    % Ekrana asistan yanıtını yazdıralım
    writeln("Asistanın cevabı:"),
    writeln(AssistantMessage),

    % Risk puanını parse ediyoruz
    parse_risk_score(AssistantMessage, RiskScore),

    % Konuşma geçmişine asistanın cevabını da ekleyelim
    append(NewHistory, [_{role: "assistant", content: AssistantMessage}], UpdatedHistory),

    % Eski conversation/1 kaydını silip güncelini ekliyoruz
    retractall(conversation(_)),
    assertz(conversation(UpdatedHistory)),

    % Dönen tam JSON'u dışarıya veriyoruz
    Response = JSONResponse.

%----------------------------------------------------------
% explain_response/2
%
%  - API cevabından sadece asistanın metin içeriğini (content) çıkartır.
%----------------------------------------------------------
explain_response(Response, Content) :-
    Response.choices = [Choice],
    Choice = json(ChoiceJSON),
    ChoiceJSON.message = Message,
    Message = json(MessageJSON),
    Content = MessageJSON.content.

%----------------------------------------------------------
% parse_risk_score/2
%
%  - Asistan mesajından risk puanını çıkartır.
%----------------------------------------------------------
parse_risk_score(Message, RiskScore) :-
    % Risk puanını içeren bir cümle bekliyoruz, örneğin: "Bu işlemin risk puanı 75'tir."
    re_matchsub("risk puanı\\s*(\\d+)", Message, Sub, [caseless(true)]),
    (   Sub.1 = RiskScoreAtom,
        atom_number(RiskScoreAtom, RiskScore)
    ->  true
    ;   writeln("Risk puanı çıkarılamadı."),
        RiskScore = 0 % Varsayılan puan
    ).

%----------------------------------------------------------
% chat/0
%
%  - Komut satırında kullanıcıdan sürekli metin alır.
%  - 'exit' girildiğinde döngüyü sonlandırır.
%----------------------------------------------------------
chat :-
    writeln("Lütfen bir soru girin:"),
    writeln("Çıkmak için 'exit' yazın."),
    read_line_to_string(user_input, Question),
    (   Question = "exit"
    ->  writeln("Çıkış yapıldı.")
    ;   (   handle_question(Question)
        ->  chat
        ;   writeln("Bir hata oluştu, tekrar deneyin."),
            chat
        )
    ).

%----------------------------------------------------------
% handle_question/1
%
%  - Kullanıcının sorusunu işler ve ChatGPT API'sine gönderir.
%----------------------------------------------------------
handle_question(Question) :-
    kullanici_sorusuna_gore_bilgi(Question, Bilgi),
    (   Bilgi \= ""
    ->  chatgpt_api(Bilgi, _Response)
    ;   writeln("Üzgünüm, bu konuda bir bilgiye sahip değilim.")
    ).

%----------------------------------------------------------
% run/0
%
%  - Konuşmayı başlatır.
%----------------------------------------------------------
run :-
    load_json_files,
    chat.

%----------------------------------------------------------
% Yardımcı Predikatlar
%----------------------------------------------------------

% API anahtarını çevresel değişkenden alır
get_api_key(API_KEY) :-
    (   getenv('OPENAI_API_KEY', API_KEY_PART)
    ->  string_concat('Bearer ', API_KEY_PART, API_KEY)
    ;   writeln("API anahtarı bulunamadı. Lütfen OPENAI_API_KEY çevresel değişkenini ayarlayın."),
        fail
    ).

%----------------------------------------------------------
% Test Predikatı
%
% - Basit bir API çağrısı yaparak entegrasyonu test eder.
%----------------------------------------------------------
test_api :-
  API_KEY = 'Bearer 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',
      BODY = _{
        model: "gpt-4",
        messages: [
            _{role: "system", content: "Sen bir fraud uzmanısın."},
            _{role: "user", content: "Merhaba, nasılsın?"}
        ]
    },
    catch(
        http_post(
            'https://api.openai.com/v1/chat/completions',
            json(BODY),
            Reply,
            [
                request_header('Content-Type'='application/json'),
                request_header('Authorization'=API_KEY)
            ]
        ),
        Error,
        (
            writeln("API isteği başarısız oldu:"),
            writeln(Error),
            fail
        )
    ),
    Reply = json(JSONResponse),
    explain_response(JSONResponse, AssistantMessage),
    writeln("Asistanın cevabı:"),
    writeln(AssistantMessage).
