  {% wire id="chat-form" 
              type="submit" 
              postback={newmessage} 
              delegate="mod_chat" %}
  <form id="chat-form" 
              method="post" action="postback">
    <div class="clearfix" style="width: 490px; margin: 5px; height: 35px; line-height: 32px; border: 1px dashed blue;">
          <div class="left">
              <input style="margin-left: 5px; margin-right: 10px; width: 410px;" type="text" name="chat" id="chat" />
          </div>
          <div class="left">
       	      <input type="submit" style="width: 45px;" value="chat">
              {% validate id="chat" type={presence} %}
          </div>
    </div>
  </form>
{% chat %}