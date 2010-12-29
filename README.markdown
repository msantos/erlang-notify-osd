
An Erlang NIF interface to the libnotify library. libnotify is used to
send desktop notification bubbles. See:

https://wiki.ubuntu.com/NotifyOSD


# SCREENSHOTS

## Default Notification

<img alt="Default Notification" src="http://ubuntuone.com/p/VlU/" width="317" height="81" />

## Using a libnotify icon

To see the available icons:

    dpkg -L notify-osd-icons

Then:

    > notify:osd([{icon, "notification-gpm-phone-020"}]).

<img alt="Setting an Icon" src="http://ubuntuone.com/p/VlV/" width="333" height="88" />

## RSS Feed

<img alt="Setting an Icon" src="http://ubuntuone.com/p/Vla/" />


# HOW TO BUILD IT

    sudo apt-get install libnotify-dev
    make

# HOW TO USE IT

    osd(Options) -> ok | enomem
        Types   Options = [Opt]
                Opt = [{summary, string()}, {body, string()}, {icon, string()},
                        {category, string()}, {urgency, int()}, {timeout, int()},
                        Hints]
                Hints = [string() | {string(), Value}]
                Value = [string() | integer() | double() | Byte]
                Byte = {byte, uchar()}


# EXAMPLES

    > notify:osd([]).
    > notify:osd([{summary, "hello"}, {body, "world"}]).
    
    > notify:osd([{icon, "notification-audio-volume-medium"},
            {hints, [{"value", 75}, "x-canonical-private-synchronous"]},
            {summary, "Volume"},
            {body, ""}]).
    
    > rss:start().

# TODO

* support growl on Mac OS X

