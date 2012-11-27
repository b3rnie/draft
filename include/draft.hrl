-ifndef(_DRAFT_HRL_).
-define(_DRAFT_HRL_, false).

-define(debug,     debug).
-define(info,      info).
-define(notice,    notice).
-define(warning,   warning).
-define(error,     error).
-define(critical,  critical).
-define(alert,     alert).
-define(emergency, emergency).

-define(is_draft_severity(X),
        (X =:= ?debug orelse
         X =:= ?info  orelse
         X =:= ?notice orelse
         X =:= ?warning orelse
         X =:= ?error orelse
         X =:= ?critical orelse
         X =:= ?alert orelse
         X =:= ?emergency)).

-endif.
