:- module(alert_utils, [alert_message/1, alert_message/2, set_alert/1]).

:- dynamic alert_enabled/1.
alert_enabled(false). % Varsayılan olarak alert kapalıdır.

% Debug modunu aç/kapat
set_alert(Enabled) :-
    retractall(alert_enabled(_)), % Mevcut durumu kaldır
    assertz(alert_enabled(Enabled)). % Yeni durumu ekle

% Tek mesajlı alert
alert_message(Message) :-
    ( alert_enabled(true) ->
        format('[ALERT] ~w~n', [Message])
    ; true % Debug kapalıysa hiçbir şey yapma, başarılı dön
    ).

% Formatlı alert
alert_message(Format, Args) :-
    ( alert_enabled(true) ->
        format('[ALERT] '),
        format(Format, Args),
        nl
    ; true % Debug kapalıysa hiçbir şey yapma, başarılı dön
    ).
