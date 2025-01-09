:- module(debug_utils, [debug_message/1, debug_message/2, set_debug/1]).

:- dynamic debug_enabled/1.
debug_enabled(false). % Varsayılan olarak debug kapalıdır.

% Debug modunu aç/kapat
set_debug(Enabled) :-
    retractall(debug_enabled(_)),
    assertz(debug_enabled(Enabled)).

% Tek mesajlı debug
debug_message(Message) :-
    debug_enabled(true), % Debug modu açıksa yazdır
    format('[DEBUG] ~w~n', [Message]).

debug_message(Message) :-
    debug_enabled(false), % Debug modu kapalıysa hiçbir şey yapma
    !.

% Formatlı debug
debug_message(Format, Args) :-
    debug_enabled(true), % Debug modu açıksa yazdır
    format('[DEBUG] '),
    format(Format, Args),
    nl.

debug_message(_, _) :-
    debug_enabled(false), % Debug modu kapalıysa hiçbir şey yapma
    !.
