<section id="chat-box" style="height: 220px; border: 1px solid green;">
    <div id="chat-list-div" style="height: 180px; border: 1px solid green; overflow: scroll;">
        <ul id="chat-list"></ul>
    </div>
    <script type="text/javascript">
        $(document).ready(function(){
            $("#chat-list-div").animate({ scrollTop: $("#chat-list-div").attr("scrollHeight") }, 3000);
        });
    </script>
    {% include "_chat_form.tpl" %}
</section>