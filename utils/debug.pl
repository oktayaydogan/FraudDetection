% debug_utils.pl
%
% Açıklama:
%   Bu modül, debug mesajlarını yönetmek için yardımcı predikatlar içerir.
%   Debug mesajları, programın çalışması sırasında detaylı bilgi vermek için
%   kullanılır. Debug modu, `set_debug/1` predikatı ile açılıp kapatılabilir.
%
% Kullanım:
%   1) Prolog ortamında bu dosyayı yükleyin:
%      ?- [debug_utils].
%
%   2) Predikatları aşağıdaki gibi test edebilirsiniz:
%      ?- set_debug(true).
%      ?- debug_message('Bu bir debug mesajıdır.').
%      ?- debug_message('Kullanıcı: ~w, İşlem: ~w', [kullanici1, işlem1]).
%
% Gereksinimler:
%   - Bu modül, başka bir modüle bağımlı değildir.
%
% Sınırlamalar:
%   - Debug mesajları sadece `debug_enabled(true)` olduğunda görüntülenir.
%   - Varsayılan olarak debug modu kapalıdır (`debug_enabled(false)`).
%
% Gelecek Geliştirmeler:
%   - Debug mesajlarını bir log dosyasına yazdırma özelliği eklenebilir.
%   - Farklı debug seviyeleri (örneğin, düşük, orta, yüksek) eklenebilir.
%
% Modül Tanımı ve İhracı:
:- module(debug_utils, [debug_message/1, debug_message/2, set_debug/1]).

% Dinamik predikat tanımı: debug_enabled/1
:- dynamic debug_enabled/1.
debug_enabled(false). % Varsayılan olarak debug kapalıdır.

% ----------------------------------------------------------------------
% set_debug/1
%
% Açıklama:
%   Debug modunun açık veya kapalı olmasını kontrol eder.
%   `Enabled` parametresi `true` veya `false` olabilir.
%
% Parametreler:
%   - Enabled: Debug modunun açık (`true`) veya kapalı (`false`) olmasını belirler.
%
% Örnek Kullanım:
%   ?- set_debug(true).  % Debug modunu açar
%   ?- set_debug(false). % Debug modunu kapatır
% ----------------------------------------------------------------------
set_debug(Enabled) :-
    retractall(debug_enabled(_)), % Mevcut durumu kaldır
    assertz(debug_enabled(Enabled)). % Yeni durumu ekle

% ----------------------------------------------------------------------
% debug_message/1
%
% Açıklama:
%   Basit bir debug mesajı görüntüler. Eğer debug modu kapalıysa (`debug_enabled(false)`),
%   mesaj görüntülenmez.
%
% Parametreler:
%   - Message: Görüntülenecek debug mesajı.
%
% Örnek Kullanım:
%   ?- debug_message('Bu bir debug mesajıdır.').
%   [DEBUG] Bu bir debug mesajıdır.
% ----------------------------------------------------------------------
debug_message(Message) :-
    ( debug_enabled(true) ->
        format('[DEBUG] ~w~n', [Message])
    ; true % Debug kapalıysa hiçbir şey yapma, başarılı dön
    ).

% ----------------------------------------------------------------------
% debug_message/2
%
% Açıklama:
%   Formatlı bir debug mesajı görüntüler. Eğer debug modu kapalıysa (`debug_enabled(false)`),
%   mesaj görüntülenmez.
%
% Parametreler:
%   - Format: Görüntülenecek mesajın formatı (örneğin, 'Kullanıcı: ~w, İşlem: ~w').
%   - Args:   Formatlı mesajın argümanları (örneğin, [kullanici1, işlem1]).
%
% Örnek Kullanım:
%   ?- debug_message('Kullanıcı: ~w, İşlem: ~w', [kullanici1, işlem1]).
%   [DEBUG] Kullanıcı: kullanici1, İşlem: işlem1
% ----------------------------------------------------------------------
debug_message(Format, Args) :-
    ( debug_enabled(true) ->
        format('[DEBUG] '),
        format(Format, Args),
        nl
    ; true % Debug kapalıysa hiçbir şey yapma, başarılı dön
    ).