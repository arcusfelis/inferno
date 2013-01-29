-module(infern_event).
-export([add_application/3,
         remove_application/2,
         update_application/4,
         add_application_directory/3,
         remove_application_directory/3]).

%% Application is a `#info_application{}' record.
add_application(EM, AppName, AppDir) ->
    gen_event:notify(EM, {new_application, AppName, AppDir}).

remove_application(EM, AppName) ->
    gen_event:notify(EM, {remove_application, AppName}).

update_application(EM, AppName, OldDir, NewDir) ->
    gen_event:notify(EM, {update_application, AppName, OldDir, NewDir}).



%% Application directory is a element of `#info_application.directories'.
add_application_directory(EM, AppName, AppDir) ->
    gen_event:notify(EM, {add_application_directory, AppName, AppDir}).

remove_application_directory(EM, AppName, AppDir) ->
    gen_event:notify(EM, {remove_application_directory, AppName, AppDir}).

