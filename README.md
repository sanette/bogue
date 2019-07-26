# Bogue ![bogue-icon](https://raw.githubusercontent.com/sanette/bogue/master/bogue-icon.png)

_bogue_ is a GUI library for [ocaml](https://ocaml.org/), with
animations, based on [SDL2](http://www.libsdl.org/).

This library can be used for games or for adding GUI elements to any
ocaml program.

It uses the __SDL2 renderer__ library, which makes it quite fast.

It is __themable__, and does not try to look like your
desktop. Instead, it will look the same on every platform.

Graphics output is scalable, and hence easily adapts to __Hi-DPI
displays__.

Programming with _bogue_ is easy if you're used to GUIs with widgets,
layouts, callbacks, and of course it has a functional flavor.  ​It uses
__[Threads](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Thread.html)__
when non-blocking reactions are needed.

# Features

## Widgets

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

## Layouts

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

Layouts can be __animated__ (slide-in, transparency, rotation)


# Screenshots

![demo](https://raw.githubusercontent.com/sanette/bogue/master/docs/images/bogue_demo-s.png)

## Videos

[randomize](https://www.youtube.com/watch?v=b7rBCctJ7Cw), [demo 1907](https://youtu.be/isFLxnDooL8)

# Installation

## Using the opam package
This is the easiest way unless you want to try out the development version.

```
opam install bogue
```

That's it.

## Bulding from sources

### Prerequisites

You need a working `ocaml` installation with `opam`, see the [ocaml doc](https://ocaml.org/docs/install.html). Then, make sure
you have `dune`, `tsdl`, `tsdl-image` and `tsdl-ttf`:
```
opam install dune tsdl tsdl-image tsdl-ttf
```

### Get the latest source

Download the
[git archive](https://github.com/sanette/bogue/archive/master.zip),
unzip it, cd into the `bogue-master` dir, and then:

```
dune build
opam install .
```

# Documentation

It's good to first have a look at Bogue's
[general principles](http://sanette.github.io/bogue/Principles.html).

A much more complete doc can be found
[here](http://sanette.github.io/bogue/Bogue.html).  It does not cover
all available features (yet), but it's already a good start.

# Examples

You should first try a
[minimal example](http://sanette.github.io/bogue/Bogue.html#example).

The `examples` directory contains more sophisticated examples. If you
installed the `bogue` package with `opam` (as described above), these
examples are available via the `boguex` program. For instance, run
examples 34 and 41 by:

```
boguex 34 41
```

Type `boguex -h` to have the list of all examples.

# A minimal app using Bogue

See [here](https://github.com/sanette/randomize).
