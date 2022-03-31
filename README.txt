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

Comparing to undo-tree:

I don’t think vundo has any real advantage over undo-tree. On the
contrary, undo-tree has much more features like diff, etc. (And vundo
most probably will not add these features.) Vundo is really just a
code challenge (can we construct an undo tree from linear history)
came true. One thing I like about vundo is that it lays out the undo
tree horizontally instead of horizontally.
