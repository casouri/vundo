To use vundo, type M-x vundo RET in the buffer you want to undo.
A undo tree buffer should pop up. To move around, type:

  f   to go forward
  b   to go backward
  n   to go to the node below when you at a branching point
  p   to go to the node above
  q   to quit, you can also type C-g

By default, you need to press RET to “commit” your change and if
you quit with q or C-g, the change made by vundo are rolled back.
You can set ‘vundo-roll-back-on-quit’ to nil to disable rolling
back.

If you bring up the vundo buffer and make some modification in the
original buffer, the tree in the vundo buffer doesn’t automatically
update. Vundo catches up the next time you invoke any command:
instead of performing that command, it updates the tree.

Note: vundo.el requires Emacs 28.

Changelog:

<2022-03-23 Wed>: UI now defaults to ASCII mode. ASCII mode also draws
differently now, it now draws

    o--o--o     instead of      o--o--o
    |  `--x                     |  +--*
    |--o                        |--o
    `--o                        +--o

<2021-11-26 Fri>: Variable vundo-translate-alist changed to
vundo-glyph=alist and has different value now.
