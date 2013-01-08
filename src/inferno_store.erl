-module(inferno_store).
-include_lib("inferno/include/inferno.hrl").
-export([application/1,
         application_modules/1]).


-spec application(AppName) -> App | undefined when
    AppName :: atom(),
    App :: #info_application{}.

application(AppName) ->
    undefined.


-spec application_modules(AppName) -> [Mod] when
    AppName :: atom(),
    Mod :: #info_module{}.
    
application_modules(AppName) ->
    [].
