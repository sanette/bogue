(lang dune 1.10)
(name bogue)
(version "1907")
(generate_opam_files true)
(license GPL2)
(maintainers "Vu Ngoc San <san.vu-ngoc@laposte.net>")
(authors "Vu Ngoc San <san.vu-ngoc@laposte.net>")
(source (github sanette/bogue))
(documentation "http://sanette.github.io/bogue/Bogue.html")

(package
 (name bogue)
 (synopsis "GUI library for ocaml, with animations, based on SDL2")
 (description "bogue is a GUI library for ocaml, with animations, based on SDL2.

This library can be used for games or for adding GUI elements to any
ocaml program.

It uses the SDL2 renderer library, which makes it quite fast.

It is themable, and does not try to look like your desktop. Instead,
it will look the same on every platform.

Graphics output is scalable, and hence easily adapts to Hi-DPI
displays.

Programming with bogue is easy if you're used to GUIs with widgets,
layouts, callbacks, and of course it has a functional flavor. ​It uses
Threads when non-blocking reactions are needed.")
 (tags ("gui"))
 (depends
  (tsdl-image :build)
  (tsdl-ttf :build)
  (ocaml (>= 4.03.0))
  (ocamlfind :build)
  (tsdl (>= 0.9.0))))