Vundo (visual undo) displays the undo history as a tree and lets you
move in the tree to go back to previous buffer states. To use vundo,
type M-x vundo RET in the buffer you want to undo. An undo tree buffer
should pop up. To move around, type:

  f   to go forward
  b   to go backward

  n   to go to the node below when you at a branching point
  p   to go to the node above

  a   to go back to the last branching point
  e   to go forward to the end/tip of the branch

  q   to quit, you can also type C-g

n/p may need some more explanation. In the following tree, n/p can
move between A and B because they share a parent (thus at a branching
point), but not C and D.

         A  C
    ──○──○──○──○──○
      │  ↕
      └──○──○──○
         B  D

By default, you need to press RET to “commit” your change and if you
quit with q or C-g, the changes made by vundo are rolled back. You can
set ‘vundo-roll-back-on-quit’ to nil to disable rolling back.

Note: vundo.el requires Emacs 28.

Customizable faces:

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

Vundo doesn’t need to be turned on all the time nor replace the undo
commands like undo-tree does. Vundo displays the tree horizontally,
whereas undo-tree displays a tree vertically. Vundo doesn’t have many
advanced features that undo-tree does (like showing diff), and most
probably will not add those features in the future.

Tests:

You can run tests by loading test/vundo-test.el and M-x ert RET t RET
to run those tests interactively, or use the following batch command:

     emacs --batch \
           -l vundo.el \
           -l test/vundo-test.el \
           -f ert-run-tests-batch-and-exit


Changelog (full changelog in NEWS.txt):

<2022-04-04 Mon>: Version 1.0.0

<2022-03-29 Tue>: vundo--mode and vundo--mode-map are now vundo-mode
and vundo-mode-map. A new custom option vundo-compact-display is added.

<2022-03-23 Wed>: UI now defaults to ASCII mode. ASCII mode also draws
differently now, it now draws

    o--o--o     instead of      o--o--o
    |  `--x                     |  +--*
    |--o                        |--o
    `--o                        +--o

<2021-11-26 Fri>: Variable vundo-translate-alist changed to
vundo-glyph-alist and has different value now.
