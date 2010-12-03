  {% wire id="chat-form" 
              type="submit" 
              postback={newmessage} 
              delegate="mod_chat" %}
  <form id="chat-form" 
              method="post" action="postback">
    <div class="clearfix zp-100" style="height: 32px; border: 1px solid green;">
      <div class="left zp-10">
          <label for="chat">Chat</label>
      </div>
      <div class="left zp-40">
          <input type="text" name="chat" id="chat" />
          {% validate id="chat" type={presence} %}
      </div>
      <div class="left button-wrapper zp-10">
       	<button type="submit">{_ Post _}</button>
      </div>
    </div>
  </form>
{% chat %}