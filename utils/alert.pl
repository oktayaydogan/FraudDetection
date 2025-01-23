% alert_utils.pl
%
% Açıklama:
%   Bu modül, uyarı (alert) mesajlarını yönetmek için yardımcı predikatlar içerir.
%   Uyarı mesajları, belirli bir durumun şüpheli veya riskli olduğunu bildirmek için
%   kullanılır. Uyarılar, `set_alert/1` predikatı ile açılıp kapatılabilir.
%
% Kullanım:
%   1) Prolog ortamında bu dosyayı yükleyin:
%      ?- [alert_utils].
%
%   2) Predikatları aşağıdaki gibi test edebilirsiniz:
%      ?- set_alert(true).
%      ?- alert_message('Bu bir uyarı mesajıdır.').
%      ?- alert_message('Şüpheli işlem tespit edildi: ~w', [kullanici1]).
%
% Gereksinimler:
%   - Bu modül, başka bir modüle bağımlı değildir.
%
% Sınırlamalar:
%   - Uyarı mesajları sadece `alert_enabled(true)` olduğunda görüntülenir.
%   - Varsayılan olarak uyarılar kapalıdır (`alert_enabled(false)`).
%
% Gelecek Geliştirmeler:
%   - Uyarı mesajlarını bir log dosyasına yazdırma özelliği eklenebilir.
%   - Farklı uyarı seviyeleri (örneğin, düşük, orta, yüksek) eklenebilir.
%
% Modül Tanımı ve İhracı:
:- module(alert_utils, [alert_message/1, alert_message/2, set_alert/1]).

% Dinamik predikat tanımı: alert_enabled/1
:- dynamic alert_enabled/1.
alert_enabled(false). % Varsayılan olarak alert kapalıdır.

% ----------------------------------------------------------------------
% set_alert/1
%
% Açıklama:
%   Uyarı mesajlarının görüntülenip görüntülenmeyeceğini kontrol eder.
%   `Enabled` parametresi `true` veya `false` olabilir.
%
% Parametreler:
%   - Enabled: Uyarıların açık (`true`) veya kapalı (`false`) olmasını belirler.
%
% Örnek Kullanım:
%   ?- set_alert(true).  % Uyarıları açar
%   ?- set_alert(false). % Uyarıları kapatır
% ----------------------------------------------------------------------
set_alert(Enabled) :-
    retractall(alert_enabled(_)), % Mevcut durumu kaldır
    assertz(alert_enabled(Enabled)). % Yeni durumu ekle

% ----------------------------------------------------------------------
% alert_message/1
%
% Açıklama:
%   Basit bir uyarı mesajı görüntüler. Eğer uyarılar kapalıysa (`alert_enabled(false)`),
%   mesaj görüntülenmez.
%
% Parametreler:
%   - Message: Görüntülenecek uyarı mesajı.
%
% Örnek Kullanım:
%   ?- alert_message('Bu bir uyarı mesajıdır.').
%   [ALERT] Bu bir uyarı mesajıdır.
% ----------------------------------------------------------------------
alert_message(Message) :-
    ( alert_enabled(true) ->
        format('[ALERT] ~w~n', [Message])
    ; true % Debug kapalıysa hiçbir şey yapma, başarılı dön
    ).

% ----------------------------------------------------------------------
% alert_message/2
%
% Açıklama:
%   Formatlı bir uyarı mesajı görüntüler. Eğer uyarılar kapalıysa (`alert_enabled(false)`),
%   mesaj görüntülenmez.
%
% Parametreler:
%   - Format: Görüntülenecek mesajın formatı (örneğin, 'Şüpheli işlem: ~w').
%   - Args:   Formatlı mesajın argümanları (örneğin, [kullanici1]).
%
% Örnek Kullanım:
%   ?- alert_message('Şüpheli işlem tespit edildi: ~w', [kullanici1]).
%   [ALERT] Şüpheli işlem tespit edildi: kullanici1
% ----------------------------------------------------------------------
alert_message(Format, Args) :-
    ( alert_enabled(true) ->
        format('[ALERT] '),
        format(Format, Args),
        nl
    ; true % Debug kapalıysa hiçbir şey yapma, başarılı dön
    ).