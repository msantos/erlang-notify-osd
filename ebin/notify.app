{application, notify,
    [
    {description, "libnotify interface"},
    {vsn, "0.01"},
    {modules, [
        notify,
        rss
            ]},
    {registered, []},
    {applications, [
        kernel,
        stdlib
            ]},
    {env, []}
    ]}.

