-record(info_module, {
        name, 
        application_name,
        title,
        description,
        compiled_filename,
        source_filename,
        refman_filename,
        parser_mode,

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
        compiled_directory
}).

