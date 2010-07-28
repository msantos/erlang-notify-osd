%% Copyright (c) 2010, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%% 
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%% 
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%% 
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
-module(notify).

-export([
        osd/1, urgency/1,
        privdir/1,
        notify/6
    ]).

-define(NOTIFY_NAME, "erlang-notify").

-define(NOTIFY_SUMMARY, "you ! Erlang.").
-define(NOTIFY_BODY, "receive after 10000 -> ok end.").
-define(NOTIFY_ICON, icon()).                       % or notification-message-im
-define(NOTIFY_CATEGORY, "message").
-define(NOTIFY_URGENCY, urgency(normal)).           % NOTIFY_URGENCY_LOW, NOTIFY_URGENCY_NORMAL, NOTIFY_URGENCY_CRITICAL
-define(NOTIFY_TIMEOUT, 10000).                     % 10 seconds

-on_load(on_load/0).


on_load() ->
    erlang:load_nif(niflib(), ?NOTIFY_NAME).

notify(_,_,_,_,_,_) ->
    erlang:error(not_implemented).

osd(Opt) when is_list(Opt) ->
    Summary = proplists:get_value(summary, Opt, ?NOTIFY_SUMMARY),
    Body = proplists:get_value(body, Opt, ?NOTIFY_BODY),
    Icon = proplists:get_value(icon, Opt, ?NOTIFY_ICON),
    Category = proplists:get_value(category, Opt, ?NOTIFY_CATEGORY),
    Urgency = proplists:get_value(urgency, Opt, ?NOTIFY_URGENCY),
    Timeout = proplists:get_value(timeout, Opt, ?NOTIFY_TIMEOUT),

    notify(Summary, Body, Icon, Category, Urgency, Timeout).

urgency(low) -> 0;
urgency(normal) -> 1;
urgency(critical) -> 2.

privdir(File) ->
    filename:join([
        filename:dirname(code:which(?MODULE)),
        "..",
        "priv",
        File
    ]).


icon() ->
    "file://" ++ privdir("erlang-logo.png").

niflib() ->
    privdir(?MODULE).

