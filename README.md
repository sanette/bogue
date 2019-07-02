# bogue

_bogue_ is a GUI library for ocaml, with animations, based on SDL2.

This library can be used for games or for adding GUI elements to any
ocaml program.

It uses the __SDL2 renderer__ library, which makes it quite fast.

It is __themable__, and does not try to look like your
desktop. Instead, it will look the same on every platform.

Graphics output is scalable, and hence easily adapts to __Hi-DPI
displays__.

Programming with _bogue_ is easy if you're used to GUIs with widgets,
layouts, callbacks, and of course it has a functional flavor.  ​It uses
__Threads__ when non-blocking reactions are needed.

# features

## widgets

Widgets are the building bricks, reponsible for graphic elements that
respond to events (mouse, touchscreen, keyboard, etc.).

For a more functional use, they can be "connected" (by pairs at this
moment) instead of reacting with callbacks (see examples).

* check box
* push button (with labels or images)
* rich text display
* image (all usual formats)
* slider (horizontal, vertical, or circular)
* text input

## layouts

widgets can be combined in various ways into layouts. For instance, a
check box followed by a text label is a common layout.

Several predefined layouts are available:

* scrollable list (that can easily handle a large number of elements)
* multi-column table
* tabs
* popup
* various menus (menu bar, drop down menus with submenus)
* select list
* radio list
* TODO file dialog

Layouts can be __animated__ (slide-in, transparency, rotation)

# Installation

## Compiling

Download the git archive, cd into the `bogue` dir, and then:
```
dune build
opam install .
```

## Using the opam package
TODO

# Documentation

The doc can be found [here](http://sanette.github.io/bogue/Bogue.html).

# Examples

You should first try a
[minimal example](http://sanette.github.io/bogue/Bogue.html#example).

The `examples` directory contains more sophisticated examples. If you installed (or compiled) the opam package, these examples are available via the `boguex` program. For instance, run examples 34 and 41 by:

```
boguex 34 41
```

Type `boguex -h` to have the list of all examples.

# A minimal app using Bogue

See [here](https://github.com/sanette/randomize).
