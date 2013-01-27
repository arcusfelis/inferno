-record(info_module, {
        name, 
        analyzed, %% date or atom()
        application_name,
        title,
        description,
        compiled_filename,
        source_filename,
        refman_filename,
        %% Non-persistent field
        functions = [],
        %% If is_analysed=false, then only filenames are declared.
        is_analysed = false,
        behaviours = [] :: [atom()]
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
        directories = [],
        source_pie,
        compiled_pie,
        refman_pie,
        app_pie
}).

