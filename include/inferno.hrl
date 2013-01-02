-record(info_module, {
        name, 
        application_name,
        title,
        description,
        compiled_filename,
        source_filename,
        refman_filename,

        %% Initial mode is fixed.
        parser_mode = init :: atom(),

        %% Non-persistent field
        functions = []
}).

-record(info_function, {
        mfa,
        name, 
        arity, 
        module_name, 
        title, 
        description, 
        is_exported = false :: boolean(),
        %% Line number
        position
}).

-record(info_application, {
        name,
        title,
        source_directory,
        %% optional
        compiled_directory
}).

