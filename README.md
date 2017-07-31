# bogue

_bogue_ is a GUI library for ocaml, with animations, based on SDL2.

This library can be used for games or for adding nice GUI elements to
any ocaml program.

It uses the _SDL2 renderer_ library, which makes it quite fast.

It is _themable_, and does not try to look like your desktop. Instead,
it will look the same on every platform.

Graphics output is scalable, and will hence easily adapt to _Hi-DPI
displays_.

Programming is easy if you're used to GUIs with widgets, layouts,
callbacks, and of course it has a functional flavor.  ​ # features

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

Layouts can be _animated_ (slide-in, transparency, rotation)
