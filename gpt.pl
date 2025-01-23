% ----------------------------------------------------------------------
% chatgpt_api.pl
%
% Açıklama:
%   Bu modül, OpenAI GPT API'si ile etkileşim kurarak kullanıcıdan alınan
%   mesajları işler ve GPT modelinden yanıt alır. Özellikle dolandırıcılık
%   analizi gibi görevler için kullanılabilir.
%
% Kullanım:
%   1) Prolog ortamında bu dosyayı yükleyin:
%      ?- [chatgpt_api].
%
%   2) API'yi test etmek için:
%      ?- test.
%      Lütfen bir soru girin: <sorunuz>
%      GPT yanıtı: <yanıt>
%
% Gereksinimler:
%   - SWI-Prolog kütüphaneleri: http/http_client, http/http_json.
%   - OpenAI API anahtarı (API_KEY).
%
% Sınırlamalar:
%   - API istekleri internet bağlantısı gerektirir.
%   - API anahtarının geçerli olması gerekmektedir.
%
% Gelecek Geliştirmeler:
%   - Hata yönetimi ve loglama özellikleri eklenebilir.
%   - Farklı GPT modelleri (örneğin GPT-4) desteklenebilir.
%   - Kullanıcı dostu bir arayüz (GUI) entegre edilebilir.
%
% Modül Tanımı:
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).

%-----------------------------------------------------------------------------
% chatgpt_api/2
%
% Açıklama:
%   OpenAI GPT API'sine bir mesaj gönderir ve yanıtı alır. Mesaj, kullanıcıdan
%   alınan bir soru veya komut olabilir. Yanıt, GPT modeli tarafından üretilir.
%
% Parametreler:
%   - Message:  GPT'ye gönderilecek mesaj (girdi).
%   - Response: GPT'den alınan yanıt (çıktı).
%
% Kullanım:
%   ?- chatgpt_api("Merhaba, nasılsın?", Response).
%-----------------------------------------------------------------------------
chatgpt_api(Message, Response) :-
    % OpenAI API'sinin URL'si
    URL = 'https://api.openai.com/v1/chat/completions',

    % OpenAI API anahtarı
    API_KEY = 'Bearer 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',

    % Gövde (Body) yapısı
    BODY = _{
        model: "gpt-4o-mini",
        messages: [
            _{role: "system", content: "Sen bir fraud uzmanısın"},
            _{role: "user", content: Message}
        ]
    },

    % HTTP POST isteği gönder
    http_post(
        URL,
        json(BODY),
        Reply,
        [
            request_header('Content-Type'='application/json'),
            request_header('Authorization'=API_KEY)
        ]
    ),

    % Yanıtı JSON formatında al
    Reply = json(Response).

%-----------------------------------------------------------------------------
% explain_response/2
%
% Açıklama:
%   GPT API'sinden alınan yanıtı işler ve içeriği ayıklar. Yanıt, JSON formatında
%   gelir ve bu predikat, yanıtın içindeki mesajı çıkarır.
%
% Parametreler:
%   - Response: GPT API'sinden alınan ham yanıt (girdi).
%   - Content:  Yanıtın içeriği (çıktı).
%
% Kullanım:
%   ?- explain_response(Response, Content).
%-----------------------------------------------------------------------------
explain_response(Response, Content) :-
    Response.choices = [Choice],
    Choice = json(ChoiceJSON),
    ChoiceJSON.message = Message,
    Message = json(MessageJSON),
    Content = MessageJSON.content.

%-----------------------------------------------------------------------------
% test/0
%
% Açıklama:
%   Kullanıcıdan bir soru alır, GPT API'sine gönderir ve yanıtı ekrana yazdırır.
%   Bu predikat, API'nin çalışıp çalışmadığını test etmek için kullanılabilir.
%
% Kullanım:
%   ?- test.
%   Lütfen bir soru girin: <sorunuz>
%   GPT yanıtı: <yanıt>
%-----------------------------------------------------------------------------
test:- 
    writeln("Lütfen bir soru girin:"),
    read_line_to_string(user_input, Question),
    (   chatgpt_api(Question, Response)
    ->  explain_response(Response, Content),
        writeln(Content)
    ),
    test.