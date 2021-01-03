record_lib
=====

record_lib library

Build
-----

    $ rebar3 compile

Use
---

Add the dependence to your rebar config:

    {deps, [
        {record_lib, {git, "https://github.com/HaotaoYang/record_lib.git", {branch, "master"}}}
    ]}.

Add the config to your rebar config:

    %% record_util_opts
    {record_util_opts, [
        {hrl_dirs, ["include"]},
        {dest_dir, "src"},
        {module_name, record_helper}
    ]}.

    %% provider_hooks
    {provider_hooks, [
        {pre, [
            {compile, {record_util, pre_compile}},
            {clean, {record_util, pre_clean}}
        ]},
        {post, [
            {compile, {record_util, post_compile}},
            {clean, {record_util, post_clean}}
        ]}
    ]}.
