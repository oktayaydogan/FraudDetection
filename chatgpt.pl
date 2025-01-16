:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).

/*
  Bu kod SWI-Prolog'da çalışır. "test." yazarak
  ChatGPT ile etkileşimli bir şekilde konuşmaya başlayabilirsiniz.

  - conversation/1: Konuşma geçmişini dinamik olarak saklar.
  - chatgpt_api/2: Kullanıcıdan alınan mesaja göre API'ye istek atar.
  - explain_response/2: Dönen JSON cevaptan asistan mesajını ayıklar.
  - test/0: Kullanıcı etkileşimini yönetir (exit yazarak çıkılabilir).
*/

:- dynamic conversation/1.

% Konuşma geçmişine başlangıç olarak 'system' mesajı ekliyoruz.
conversation([
    _{ role: "system", content: "Sen bir fraud uzmanısın" }
]).

%----------------------------------------------------------
% chatgpt_api/2
%
%  - Kullanıcının mesajını alır (Message).
%  - Mevcut konuşma geçmişine ekleyerek OpenAI Chat API'ye gönderir.
%  - Dönen cevabı (Response) JSON olarak tutar, ayrıca ekrana yazdırır.
%----------------------------------------------------------
chatgpt_api(Message, Response) :-
    % API adresi
    URL = 'https://api.openai.com/v1/chat/completions',

    % API anahtarınız (düz metin olarak, güvenlik açısından önerilmez)
    API_KEY = 'Bearer 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',

    % Mevcut konuşma geçmişini alıyoruz
    conversation(History),

    % Kullanıcının yeni mesajını konuşma geçmişine ekliyoruz
    append(History, [_{role: "user", content: Message}], NewHistory),

    % Gövde (Body) yapısı
    BODY = _{
        model: "gpt-4o-mini",
        messages: NewHistory
    },

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
    append(NewHistory, [_{role: "assistant", content: AssistantMessage}], UpdatedHistory),

    % Eski conversation/1 kaydını silip güncelini ekliyoruz
    retractall(conversation(_)),
    assertz(conversation(UpdatedHistory)),

    % Dönen tam JSON'u dışarıya veriyoruz (istemiyorsanız AssistantMessage da verebilirsiniz)
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
    ;   chatgpt_api(Question, _Response),
        chat
    ).
