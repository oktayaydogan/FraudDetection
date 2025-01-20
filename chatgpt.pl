:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(lists)).

/*
  Bu kod SWI-Prolog'da çalışır. "run." yazarak
  ChatGPT ile etkileşimli bir şekilde konuşmaya başlayabilirsiniz.

  - conversation/1: Konuşma geçmişini dinamik olarak saklar.
  - chatgpt_api/2: Kullanıcıdan alınan mesaja göre API'ye istek atar.
  - explain_response/2: Dönen JSON cevaptan asistan mesajını ayıklar.
  - kullanici_sorusuna_gore_bilgi/2: Kullanıcının sorusuna göre bilgi çeker.
  - load_json_files/0: JSON dosyalarını yükler.
  - chat/0: Kullanıcı etkileşimini yönetir (exit yazarak çıkılabilir).
*/

:- dynamic conversation/1.
:- dynamic islem_verisi/1.
:- dynamic kural/1.

% Konuşma geçmişine başlangıç olarak 'system' mesajı ekliyoruz.
conversation([
    _{ role: "system", content: "Sen bir fraud uzmanısın, sana verilen kuralları ve verileri işleyerek işlemlerin fraud analizini yapıyorsun. Analiz sonucunda işleme 100 üzerinden bir puan veriyorsun." }
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
    writeln("Assert edilecek Item:"),
    writeln(Item),
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
    writeln("Assert edilecek Kural:"),
    writeln(Item),
    assertz(kural(Item)).

%----------------------------------------------------------
% Bilgi Kaynakları
%----------------------------------------------------------

%----------------------------------------------------------
% kullanici_sorusuna_gore_bilgi/2
%
%  - Kullanıcının sorusuna göre ilgili bilgiyi çeker.
%----------------------------------------------------------
kullanici_sorusuna_gore_bilgi(Soru, Bilgi) :-
    downcase_atom(Soru, SoruKucuk),
    (   sub_string(SoruKucuk, _, _, _, "kredi kartı")
    ->  findall(K, islem_verisi(K), IslemList),
        atomic_list_concat(IslemList, ', ', IslemStr),
        format(string(Bilgi), "İşlem Verileri: ~w", [IslemStr])
    ;   sub_string(SoruKucuk, _, _, _, "kural")
    ->  findall(K, kural(K), KuralList),
        atomic_list_concat(KuralList, ', ', KuralStr),
        format(string(Bilgi), "Kurallar: ~w", [KuralStr])
    ;   Bilgi = "Üzgünüm, bu konuda bir bilgiye sahip değilim."
    ).

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

    % API anahtarı
    API_KEY = 'Bearer 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',

    % Mevcut konuşma geçmişini alıyoruz
    conversation(History),

    % Kullanıcının yeni mesajını konuşma geçmişine ekliyoruz
    append(History, [_{role: "user", content: Message}], NewHistory),

    % RAG: Kullanıcının sorusuna göre bilgi çekiyoruz
    kullanici_sorusuna_gore_bilgi(Message, EkBilgi),

    % RAG: Ek bilgiyi sistem mesajına ekliyoruz
    append([
        _{ role: "system", content: EkBilgi }
    ], NewHistory, HistoryWithEkBilgi),

    % Gövde (Body) yapısı
    BODY = _{
        model: "gpt-4o-mini",
        messages: HistoryWithEkBilgi
    },

    writeln(BODY),
    

    % HTTP isteğini 'catch/3' ile sarmalıyoruz ki hata durumunda yönetebilelim
    catch(
        http_post(
            URL,
            json(BODY),         % JSON olarak isteği gönderiyoruz
            Reply,              % Dönen cevabı Reply içine alıyoruz
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

    % Konuşma geçmişine asistanın cevabını da ekleyelim
    append(HistoryWithEkBilgi, [_{role: "assistant", content: AssistantMessage}], UpdatedHistory),

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
    ;   (   chatgpt_api(Question, _Response)
        ->  chat
        ;   writeln("Bir hata oluştu, tekrar deneyin."),
            chat
        )
    ).

%----------------------------------------------------------
% run/0
%
%  - Konuşmayı başlatır.
%----------------------------------------------------------
run :-
    load_json_files,
    chat.
