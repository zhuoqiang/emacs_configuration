# Qiang's Emacs Configuration

Emacs is known to be <del>one of</del> the most powerful editor in the world. Yet its power is hidden in the absence of proper configuration.

Emacs configuration is hard and fun. After quite a while hard time fighting against emacs lisp I finally have some fun with my Emacs and I just open source my personal Emacs configuration hoping it's helpful to you.

This configuration has minimal configuraion for the sake of simplicity:

- Chinese & English fonts setting with font scale function
- Programming language mode
- Jump around/bookmark
- More emacs theme
- Auto restore files when re-open
- Auto typing using snippers and auto insert
- Auto maxmize frame when startup emacs

It works well for Emacs 24 under Mac and Windows (not tested under Linux though). All you need to do is checkout the code to the *~/.emacs.d/* directory. It also serves as a pretty good start point for you to customize emacs further, good luck hacking!

---
Note:

- Please *delete ~/.emacs* file to let Emacs use *~/.emacs.d/init.el* for startup configuration instead.
