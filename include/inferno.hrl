-record(info_module, {
        name, 
        analyzed, %% date
        application_name,
        title,
        description,
        compiled_filename,
        source_filename,
        refman_filename,
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

