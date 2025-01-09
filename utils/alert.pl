:- module(alert_utils, [alert_message/1, alert_message/2]).

% Tek mesajlı alert
alert_message(Message) :-
    format('[ALERT] ~w~n', [Message]).

% Formatlı alert
alert_message(Format, Args) :-
    format('[ALERT] '),
    format(Format, Args),
    nl.
