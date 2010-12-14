<section id="chat-box" style="height: 240px; width: 500px; border: 4px ridge blue;">

   <div id="chat-list-users-div" class="zp-40" style="height: 180px; float:right; overflow:scroll; overflow-x: hidden;">
        <ul id="chat-list-users" style="list-style:none;"></ul>
   </div>

    <div id="chat-list-div" class="zp-60" style="height: 180px; overflow: scroll; overflow-x: hidden;">
        <ul id="chat-list" style="list-style:none;"></ul>
    </div>

    {% include "_chat_form.tpl" %}
</section>
