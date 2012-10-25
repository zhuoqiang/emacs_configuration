Emacs is known to be one of the most powerful editor in the world. Yet its power is hidden in the absence of proper configuration.

Emacs configuration is hard and fun. After quite a while hard time fighting against elisp I finally have some fun with my Emacs and I just open source my personal Emacs configuration hoping it's helpful to you.

The functions in the configuration are minimal for the sake of  simplicity:

- Chinese & English font setting with font scale function
- Key re-mapping on Mac
- Some programming language mode setting
- Jump functions
- Blackboard emacs theme
- auto restore files when re-open
- snippers (using yasnipper)

It works well on my Emacs 24 Mac OS X (and should be OK in Windows & Linux). All you need to do is checkout the code to .emacs.d folder under your home directory and start emacs (If you already have .emacs file under home directory, just remove it. Emacs should use ~/.emacs.d/init.el instead)
