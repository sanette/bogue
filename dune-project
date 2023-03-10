(lang dune 2.7)
(cram enable)
(name bogue)
(version "20230310")
(generate_opam_files true)
(license ISC)
(maintainers "Vu Ngoc San <san.vu-ngoc@laposte.net>")
(authors "Vu Ngoc San <san.vu-ngoc@laposte.net>")
(source (github sanette/bogue))
(documentation "http://sanette.github.io/bogue/Bogue.html")

(package
 (name bogue)
 (synopsis "GUI library for ocaml, with animations, based on SDL2")
 (description "Bogue is an all-purpose GUI library for ocaml, with animations, based on SDL2.

This library can be used for games or for adding GUI elements to any
ocaml program.

It uses the SDL2 renderer library, which makes it quite fast.

It is themable, and does not try to look like your desktop. Instead,
it will look the same on every platform.

Graphics output is scalable, and hence easily adapts to Hi-DPI
displays.

Programming with bogue is easy if you're used to GUIs with widgets,
layouts, callbacks, and of course it has a functional flavor. It uses
Threads when non-blocking reactions are needed.")
 (tags ("gui"))
 (depends
  (tsdl-image (>= 0.3.0))
  (tsdl-ttf (>= 0.3))
  (ocaml (>= 4.08.0))
  (tsdl (and (>= 0.9.7) (< 0.9.9)))
  directories
))
