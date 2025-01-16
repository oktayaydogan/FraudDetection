:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).


chatgpt_api(Message, Response) :-
    URL = 'https://api.openai.com/v1/chat/completions',
    API_KEY = 'Bearer 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',
    BODY = _{
        model: "gpt-4o-mini",
        messages: [
            _{role: "system", content: "Sen bir fraud uzmanısın"},
            _{role: "user", content: Message}
        ]
    },

    http_post(
        URL,
        json(BODY),
        Reply,
        [
            request_header('Content-Type'='application/json'),
            request_header('Authorization'=API_KEY)
        ]
    ),

    Reply = json(Response).

explain_response(Response, Content) :-
    Response.choices = [Choice],
    Choice = json(ChoiceJSON),
    ChoiceJSON.message = Message,
    Message = json(MessageJSON),
    Content = MessageJSON.content.

test:- 
    writeln("Lütfen bir soru girin:"),
    read_line_to_string(user_input, Question),
    (   chatgpt_api(Question, Response)
    ->  explain_response(Response, Content),
        writeln(Content)
    ),
    test.