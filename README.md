# Qiang's Emacs Configuration

Emacs is known to be <del>one of</del> the most powerful editor in the world. Yet its power is hidden in the absence of proper configuration.

Emacs configuration is hard and fun. After quite a while hard time fighting against emacs lisp I finally have some fun with my Emacs and here I just open source my personal Emacs configuration, hoping it's helpful to you.

This configuration has minimal configuraion for the sake of simplicity:

- Chinese & English fonts setting with font scale function
- Programming language mode
- Jump around/bookmark
- More emacs theme
- Auto restore files when re-open
- Auto typing using snippets and auto insert
- Auto maxmize frame when startup emacs

It works for Emacs 24 under Mac and Windows (not tested under Linux though). All you need to do is checkout the code to the `~/.emacs.d/` directory. If you already have `~/.emacs` file under home directory, you have to remove it so that  Emacs could use ``~/.emacs.d/init.el`` instead.

I hope it could be a start point to enhance your emacs further, good luck hacking!
