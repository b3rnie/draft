-ifndef(__DRAFT_HRL__).
-define(__DRAFT_HRL__, false).

-define(is_draft_severity(X),
        (X =:= debug orelse
         X =:= info  orelse
         X =:= notice orelse
         X =:= warning orelse
         X =:= error orelse
         X =:= critical orelse
         X =:= alert orelse
         X =:= emergency)).

-define(debug(F),       ?debug(F,A)).
-define(debug(F,A),     ?call(debug,F,A)).

-define(info(F),        ?info(F,[])).
-define(info(F,A),      ?call(info,F,A)).

-define(notice(F),      ?notice(F,[])).
-define(notice(F,A),    ?call(notice,F,A)).

-define(warning(F),     ?warning(F,[])).
-define(warning(F,A),   ?call(warning,F,A)).

-define(error(F),       ?error(F,A)).
-define(error(F,A),     ?call(error,F,A)).

-define(critical(F),    ?critical(F,[])).
-define(critical(F,A),  ?call(critical,F,A)).

-define(alert(F),       ?alert(F,[])).
-define(alert(F,A),     ?call(alert,F,A)).

-define(emergency(F),   ?emergency(F,[])).
-define(emergency(F,A), ?call(emergency,F,A)).

-define(call(S,F,A),    draft:call(S,F,A,[{module,?MODULE},{line,?LINE}])).

-endif.
