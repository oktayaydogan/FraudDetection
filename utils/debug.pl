:- module(debug_utils, [debug_message/1, debug_message/2, set_debug/1]).

:- dynamic debug_enabled/1.
debug_enabled(false). % Varsayılan olarak debug kapalıdır.

% Debug modunu aç/kapat
set_debug(Enabled) :-
    retractall(debug_enabled(_)), % Mevcut durumu kaldır
    assertz(debug_enabled(Enabled)). % Yeni durumu ekle

% Tek mesajlı debug
debug_message(Message) :-
    ( debug_enabled(true) ->
        format('[DEBUG] ~w~n', [Message])
    ; true % Debug kapalıysa hiçbir şey yapma, başarılı dön
    ).

% Formatlı debug
debug_message(Format, Args) :-
    ( debug_enabled(true) ->
        format('[DEBUG] '),
        format(Format, Args),
        nl
    ; true % Debug kapalıysa hiçbir şey yapma, başarılı dön
    ).
