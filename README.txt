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

Faces:

- vundo-default
- vundo-node
- vundo-stem
- vundo-highlight

If you want to use prettier Unicode characters to draw the tree like
this:

    ○──○──○
    │  └──●
    ├──○
    └──○

set vundo-glyph-alist by

    (setq vundo-glyph-alist vundo-unicode-symbols)

Your default font needs to contain these Unicode characters, otherwise
they look terrible and don’t align. You can find a font that covers
these characters (eg, Symbola, Unifont), and set ‘vundo-default’ face
to use that font:

    (set-face-attribute 'vundo-default nil :family "Symbola")

Tests:

The following command can be used to run tests.

     emacs -batch \
           -l vundo.el \
           -l test/vundo-test.el \
           -f ert-run-tests-batch-and-exit

Changelog:

<2022-03-29 Tue> vundo--mode and vundo--mode-map are now vundo-mode
and vundo-mode-map. A new custom option vundo-compact-display is added.

<2022-03-23 Wed>: UI now defaults to ASCII mode. ASCII mode also draws
differently now, it now draws

    o--o--o     instead of      o--o--o
    |  `--x                     |  +--*
    |--o                        |--o
    `--o                        +--o

<2021-11-26 Fri>: Variable vundo-translate-alist changed to
vundo-glyph-alist and has different value now.


Comparing to undo-tree:

Vundo doesn’t need to be turned on all the time nor replace the undo
commands like undo-tree does. Vundo displays the tree horizontally,
whereas undo-tree displays a tree vertically. Vundo doesn’t have many
advanced features that undo-tree does (like showing diff), and most
probably will not add those features in the future.
