# Requirements #

Mercurial + Zotonic and its dependencies

# Instructions #

## Clone zchat ##
> `hg clone https://zchat.googlecode.com/hg/ mod_chat`

## Link to it from Zotonic ##
  1. Change to the directory in which you store your sites Zotonic modules
  1. Do this, assuming zchat is in your home directory
> > `ln -s ~/mod_chat`
  1. Rebuild Zotonic
> > `make`
  1. Enable the chat module via the Zotonic admin
  1. Include the chat in your template
> > ` {% include "_chat_box.tpl" %} `