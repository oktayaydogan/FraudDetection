:- module(chatgpt, [gpt_kullanici_sorgula/2, gpt_islem_sorgula/2]).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(pcre)). % Regex desteği için

/*
  Bu kod SWI-Prolog'da çalışır. "run." yazarak
  verileri yükleyebilir ve ardından belirli kullanıcılar veya işlemler için sorgular yapabilirsiniz.
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
% Yardımcı Predikatlar
%----------------------------------------------------------

% JSON sözlüğünü JSON string'e dönüştürür
dict_to_json_string(Dict, JSONString) :-
    with_output_to(string(JSONString), json_write_dict(current_output, Dict)).

%----------------------------------------------------------
% Kullanıcı Sorgulama
%----------------------------------------------------------
gpt_kullanici_sorgula(UserId, Response) :-
    run,
    % Sadece belirtilen kullanıcının işlemlerini al
    findall(Transaction, (islem_verisi(Transaction), Transaction.kullanici = UserId), UserTransactions),
    % Tüm işlemleri al (farklı bir değişken ismi kullanarak)
    findall(T, islem_verisi(T), AllTransactions),
    % Eğer en az bir işlem varsa devam et
    AllTransactions \= [],
    % Kuralları JSON string olarak al
    load_rules_as_string(RulesStr),
    % Prompt'u oluştur
    format_data_kullanici(UserId, UserTransactions, AllTransactions, RulesStr, Prompt),
    % ChatGPT API'sine gönder
    chatgpt_api(Prompt, Response),
    % writeln("ChatGPT Yanıtı:"),
    writeln(Response).

gpt_kullanici_sorgula(_UserId, _Response) :-
    writeln("Belirtilen kullanıcı bulunamadı veya hiç işlem yapmamış.").

%----------------------------------------------------------
% İşlem Sorgulama
%----------------------------------------------------------
gpt_islem_sorgula(TransactionId, Response) :-
    run,
    % Belirtilen işlem ID'sini bul
    islem_verisi(Transaction),
    Transaction.id = TransactionId,
    % Tüm işlemleri al (farklı bir değişken ismi kullanarak)
    findall(T, islem_verisi(T), AllTransactions),
    AllTransactions \= [],
    % Kuralları JSON string olarak al
    load_rules_as_string(RulesStr),
    % Prompt'u oluştur
    format_data_islem(Transaction, AllTransactions, RulesStr, Prompt),
    % ChatGPT API'sine gönder
    chatgpt_api(Prompt, Response),
    writeln("ChatGPT Yanıtı:"),
    writeln(Response).

gpt_islem_sorgula(_TransactionId, _Result) :-
    writeln("Belirtilen işlem ID'si bulunamadı.").

%----------------------------------------------------------
% Kuralları JSON String Olarak Yükleme
%----------------------------------------------------------
load_rules_as_string(RulesStr) :-
    findall(
        KuralStr, 
        (kural(Kural), 
         dict_to_json_string(Kural, KuralStr)),
        KuralStrList
    ),
    atomic_list_concat(KuralStrList, '\n\n', RulesStr).

%----------------------------------------------------------
% Prompt Oluşturma
%----------------------------------------------------------
format_data_kullanici(UserId, UserTransactions, AllTransactions, RulesStr, Prompt) :-
    % Kullanıcının işlemlerini JSON string'e dönüştür
    with_output_to(string(UserTransactionsStr), json_write_dict(current_output, _{islemler: UserTransactions})),
    % Tüm işlemleri JSON string'e dönüştür
    with_output_to(string(AllTransactionsStr), json_write_dict(current_output, _{islemler: AllTransactions})),
    % Prompt'u oluştur
    format(string(Prompt), 
        "Aşağıda kullanıcı ~w'ın tüm işlem verileri ve tüm işlemler ile kurallar bulunmaktadır. Lütfen bu verileri kullanarak kullanıcı için dolandırıcılık riski analizi yapınız ve 100 üzerinden bir puan veriniz. Sadece bir puan veriniz ve başka detay eklemeyiniz.\n\nKullanıcı İşlemleri:\n~w\n\nDiğer Tüm İşlemler:\n~w\n\nKurallar:\n~w", 
        [UserId, UserTransactionsStr, AllTransactionsStr, RulesStr]).

format_data_islem(Transaction, AllTransactions, RulesStr, Prompt) :-
    % İşlemi JSON string'e dönüştür
    with_output_to(string(TransactionStr), json_write_dict(current_output, Transaction)),
    % Tüm işlemleri JSON string'e dönüştür
    with_output_to(string(AllTransactionsStr), json_write_dict(current_output, _{islemler: AllTransactions})),
    % Prompt'u oluştur
    format(string(Prompt), 
        "Aşağıda işlem ID: ~w için işlem verisi, tüm işlemler ve kurallar bulunmaktadır. Lütfen bu verileri kullanarak işlemin dolandırıcılık riski analizi yapınız ve 100 üzerinden bir puan veriniz. Sadece bir puan veriniz ve başka detay eklemeyiniz.\n\nİşlem Detayı:\n~w\n\nDiğer Tüm İşlemler:\n~w\n\nKurallar:\n~w", 
        [Transaction.id, TransactionStr, AllTransactionsStr, RulesStr]).

%----------------------------------------------------------
% ChatGPT API Entegrasyonu
%----------------------------------------------------------
chatgpt_api(Prompt, Response) :-
    % API adresi
    URL = 'https://api.openai.com/v1/chat/completions',

    API_KEY = 'Bearer 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',

    % Mevcut konuşma geçmişini alıyoruz
    conversation(History),

    % Kullanıcının yeni mesajını konuşma geçmişine ekliyoruz
    append(History, [_{role: "user", content: Prompt}], NewHistory),

    % Gövde (Body) yapısı
    BODY = _{
        model: "gpt-4o-mini",
        messages: NewHistory
    },

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
    % writeln("Asistanın cevabı:"),
    % writeln(AssistantMessage),

    % Konuşma geçmişine asistanın cevabını da ekleyelim
    append(NewHistory, [_{role: "assistant", content: AssistantMessage}], UpdatedHistory),

    % Eski conversation/1 kaydını silip güncelini ekliyoruz
    retractall(conversation(_)),
    assertz(conversation(UpdatedHistory)),

    % Dönen tam JSON'u dışarıya veriyoruz
    Response = AssistantMessage.

%----------------------------------------------------------
% Explain Response
%----------------------------------------------------------
explain_response(Response, Content) :-
    Response.choices = [Choice],
    Choice = json(ChoiceJSON),
    ChoiceJSON.message = Message,
    Message = json(MessageJSON),
    Content = MessageJSON.content.

%----------------------------------------------------------
% Main Run Predicate
%----------------------------------------------------------
run :-
    load_json_files.
