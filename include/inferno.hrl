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
        %% Is this module a test suite?
        is_test,
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
        %% Is this module a test callback?
        is_test,
        %% Line number
        position,
        behaviours = [] :: [atom()]
}).

-record(info_application, {
        name,
        title,
        directories = []
}).
