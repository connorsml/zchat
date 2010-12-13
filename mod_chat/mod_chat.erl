%% @author Michael Connors <michael@bring42.net>
%% @copyright 2010 Michael Connors
%% @date 2010-11-29
%% @doc Simple chat module.

%% Copyright 2010 Michael Connors
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

-module(mod_chat).
-author("Michael Connors <michael@bring42.net>").
-behaviour(gen_server).

-mod_title("Chat").
-mod_description("Implements a simple chatroom.").

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([start_link/1]).

%% interface functions
-export([
    event/2,
    add_chat_box/1
]).

-include_lib("zotonic.hrl").

-record(chat_box, {pid, name}).
-record(state, {context, chat_boxes=[]}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

handle_call({{add_chat_box, ChatBox}, Ctx}, _From, State) ->
    ?PRINT(ChatBox),
    ChatBoxes = [ X || X <- [ChatBox|State#state.chat_boxes], is_process_alive(X#chat_box.pid)],
    %% add new chat box to all
    render_userlist_row([{name, ChatBox#chat_box.name}], ChatBoxes, Ctx),
    %% add existing chat boxes to new
    [ render_userlist_row([{name, CBox#chat_box.name}], [ChatBox], Ctx) || CBox <- tl(ChatBoxes) ],
    {reply, ok, State#state{chat_boxes=ChatBoxes}};

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast({{chat_done, Chat}, Ctx}, State) ->
    case State#state.chat_boxes of
        [] -> nop;
        ChatBoxes ->
	    [Name] = [ X#chat_box.name || X <- ChatBoxes, X#chat_box.pid == Ctx#context.page_pid ],
            case catch z_template:render_to_iolist("_chat_row.tpl", [{chat, Chat}, {name, Name}], Ctx) of
                {error, {template_not_found,"_chat_row.tpl",enoent}} ->
                    % We can get a template_not_found error when the system is still starting.
                    nop;
                {error, Reason} ->
                    ?DEBUG(Reason),
                    nop;
                {Tpl, _Ctx} ->
                    F = fun(Pid) ->
                                z_session_page:add_script([
                                    "$('", z_utils:js_escape(Tpl), 
                                    "').appendTo('#chat-list');"], Pid),
                                z_session_page:add_script(["$('#chat').val(\"\");"], Pid),
                                z_session_page:add_script(["$('#chat-list-div').animate({ scrollTop: $('#chat-list-div').attr('scrollHeight') }, 3000);"], Pid)
                        end,
                    [F(P) || #chat_box{pid=P} <- ChatBoxes]
            end
    end,
    {noreply, State};

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

handle_info(_Info, State) ->
    {noreply, State}.


%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    {ok, State}.


%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

add_chat_box(Context) ->
    z_notifier:first({add_chat_box, get_chat_box(Context)}, Context).

%% @doc Handle the submit event of a new comment
event({submit, {newmessage, _Args}, _TriggerId, _TargetId}, Context) ->
    %% case z_auth:is_auth(Context) of
    %%     false ->
    %%         Name = "anonymous";
    %%     true ->
    %%         UserID = z_context:get_session(auth_user_id, Context),
    %%         Name = m_rsc:p(UserID, name_first, "Anonymous", Context)
    %% end,
    Chat = z_context:get_q_validated("chat", Context),
    z_notifier:notify({chat_done, Chat}, Context). %, Name}, Context).

get_chat_box(Context=#context{page_pid=Pid}) ->
    Anonymous = io_lib:format("Anonymous (~p)", [Pid]),
    case z_auth:is_auth(Context) of
	false ->
	    Name = Anonymous;
	true ->
	    UserID = z_context:get_session(auth_user_id, Context),
	    Name = m_rsc:p(UserID, name_first, Anonymous, Context)
    end,
    #chat_box{pid=Pid, name=Name}.


render_userlist_row(Args, ChatBoxes, Ctx) ->
    ?PRINT(ChatBoxes),
    case catch z_template:render_to_iolist("_chat_user_row.tpl", Args, Ctx) of
	{error, Reason} ->
	    ?DEBUG(Reason),
	    nop;
	{Tpl, _TplCtx} ->
	    F = fun(#chat_box{pid = Pid}) ->
			z_session_page:add_script([
						   "$('", z_utils:js_escape(Tpl),
						   "').appendTo('#chat-list-users');"],
						  Pid)
		end,
	    [ F(Cb) || Cb <- ChatBoxes ]
    end.


init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    z_notifier:observe(add_chat_box, self(), Context),
    z_notifier:observe(chat_done, self(), Context),
    {ok, #state{context=z_context:new(Context)}}.
    
