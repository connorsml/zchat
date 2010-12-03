%% @author Michael Connors <michael@bring42.net>
%% @copyright 2010 Michael Connors <michael@bring42.net>

%% Copyright 2010 Michael Connors <michael@bring42.net>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(scomp_chat_chat).
-behaviour(gen_scomp).

-export([vary/2, render/3]).

-include("zotonic.hrl").

vary(_Params, _Context) -> nocache.
render(_Params, _Vars, Context) ->
    mod_chat:add_chat_box(Context),
    {ok, []}.
