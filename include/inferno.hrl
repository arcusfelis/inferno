-record(info_module, {
        name, 
        title,
        description,
        functions = [],
        compiled_filename
}).

-record(info_function, {
        mfa,
        name, 
        arity, 
        module_name, 
        title, 
        description, 
        is_exported = false :: boolean(),
        position
}).
