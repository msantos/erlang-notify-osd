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
-module(rss).
-include_lib("xmerl/include/xmerl.hrl").

-export([start/0, start/1, parse/1]).

-record(state, {
        feed,           % URL
        icon,
        poll = 1,       % minutes
        number = 5      % number of RSS entries
    }).


start() ->
    start("rss.cfg").
start(Cfg) ->
    inets:start(),
    State = parse(notify:privdir(Cfg)),
    spawn(fun() -> loop(State, []) end).

loop(#state{feed = Feed, poll = Poll, number = N, icon = Icon} = State, Digest) ->
    RSS = lists:sublist(retrieve(Feed), N),
    Display = match(RSS, Digest),
    [ notify:osd([{summary, S},{body, B},{icon, Icon}]) || {S,B} <- Display ],
    timer:sleep(timer:minutes(Poll)),
    loop(State, erlang:phash2(hd(RSS))).

match(RSS, Digest) ->
    match(RSS, Digest, []).
match([], _, Acc) ->
    Acc;
match([H|T], Digest, Acc) ->
    error_logger:info_report([{rss, H}, {phash, erlang:phash2(H)}, {digest, Digest}]),
    case erlang:phash2(H) of
        Digest -> 
            Acc;
        _ ->
            match(T, Digest, [H|Acc])
    end.

retrieve(Feed) ->
    {ok, {Response, _Headers, Body}} = httpc:request(Feed),
    response(Response, Body).

%%
%% How to write an RSS aggregator, by Tobbe
%% http://www.trapexit.org/How_to_write_an_RSS_aggregator
%%
response({_ ,200, _}, Body) ->
    {Doc, _Misc} = xmerl_scan:string(Body),
    printItems(getElementsByTagName(Doc, item)).

getElementsByTagName([H|T], Item) when H#xmlElement.name == Item ->
    [H|getElementsByTagName(T, Item)];
getElementsByTagName([H|T], Item) when is_record(H, xmlElement) ->
    getElementsByTagName(H#xmlElement.content, Item) ++
    getElementsByTagName(T, Item);                                                                  
getElementsByTagName(X, Item) when is_record(X, xmlElement) ->
    getElementsByTagName(X#xmlElement.content, Item);
getElementsByTagName([_|T], Item) ->
    getElementsByTagName(T, Item);
getElementsByTagName([], _) ->
    [].

printItems(Items) ->
    [ printItem(Item) || Item <- Items ].

printItem(Item) ->
    Title = textOf(first(Item, title)),
    Descr = textOf(first(Item, description)),
    {unicode:characters_to_binary(Title), unicode:characters_to_binary(Descr)}.

first(Item, Tag) ->
    hd([X || X <- Item#xmlElement.content,
        X#xmlElement.name == Tag]).

textOf(Item) ->
    lists:flatten([X#xmlText.value || X <- Item#xmlElement.content,
        element(1,X) == xmlText]).


parse(File) ->
    {ok, [Config]} = file:consult(File),
    #state{
        feed = proplists:get_value(feed, Config),
        poll = proplists:get_value(poll, Config, #state.poll),
        number = proplists:get_value(number, Config, #state.number),
        icon = proplists:get_value(icon, Config, notify:icon())
    }.

