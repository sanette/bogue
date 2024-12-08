# Changelog

#### Warning: this changelog is not complete for changes before 2023

All notable changes to the Bogue project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased](https://github.com/sanette/bogue/compare/20240928...HEAD)

### Added

- `Flat` and `Tower` have new resizing strategies.
- `Selection` new functions `is_empty`, `size`, `range`, `first`, `last`, `invert`.
- `Table` layouts can now be resized dynamically.
- Can now navigate `tabs` with TAB!  ;) (preliminary)
- Can now enable/disable a layout with `Layout.set_enabled`. New examples 53, 53bis.
- changing text color in `Text_display` is now possible, using
  `Widget.html`, see exemple 47.
- underlining text in `Text_display` is now possible, using
  `Widget.html` (but spaces between words are not underlined at this
  point), see example 6.
- `Theme.get_path` is now public.
- `Theme.get_font_path_opt`
- `Label.font_from_file` allows the use of arbitrary fonts in addition
  to the Theme fonts (which is not recommended anyways for good
  design).
- font example 52.

### Changed

- Options for `Layout.flat*` and `Layout.tower*` have changed. If you
  were using any of `scale_content` or `keep_resize` you should use
  the new `resize` option instead.
- `Layout.unload_textures` is renamed `Layout.iter_unload_textures`

### Fixed

- `Table` layouts can now be resized dynamically.
- Better resizing for `tabs`: menu height is fixed.
- Better resizing for `flat` and `tower`: margins are not scaled.
- `Long_list` layouts can now be resized dynamically.
- `Label` `style` parameter is now usable (ocamldoc invocation was
  wrong for `tsdl_ttf`), and can also be used for `Widget.label`.

### Developement

- New `Resize` module inside `Layout`.
- clarify that `dst` field of `blit` is `None` means NO RENDERING.
- experimental `Detect` for detecting flat and tower layouts.

## [20240928](https://github.com/sanette/bogue/compare/20240225...20240928)

### Added

- `Avar.reset` is now public.
- `Layout.is_shown`.
- `Layout.inside` is now public.
- `Snapshot.to_cursor` (experimental).
- `Main.get_monitors_refresh_rate`.
- "NO_VSYNC" theme variable.

### Changed

- `Main.run` and `Time.adaptive_fps` have an optional argument `vsync`.
- Enable adaptive vsync (through openGL) by default.
- There is now no difference between the types `sdl_event` and `Sdl.Event.enum`.

### Developement

- New `Time.adaptive_fps` using swap interval.
- `Main.get_window_refresh_rate`
- `Draw.surface_from_texture`
- We now use tsdl >= 1.0.0.
- We don't un/lock audio automatically anymore.

## [20240225](https://github.com/sanette/bogue/compare/20231209...20240225)

### Warning: Last version compatible with tsdl < 0.9.9 (and >= 0.9.7)

### Added

- `Text_input.activate` is now public
- `Box.get_style` and `Box.set_style` are now public.

### Fixed

- Take advantage of recent SDL version to (greatly!) reduce power
  saving with idle.
- Fix too large cursor position when using Text_input.set

### Developement

- `mouse_at_rest` now uses `Timeout`.
- Switch from `directories` to `xdg` to work on Windows.
- Remove symlinks in doc.
- The `example.ml` file is now called `examples.ml`, which is much
  more logical since it contains more than 50 examples.
- Trying to enforce that all used submodules are aliased (to make it
  clear they are used) in the header of each file, example `module
  Time = B_time`.

## [20231209](https://github.com/sanette/bogue/compare/20221112...20231209)

### Added

- Use `directories` to compute home and config paths, and add special
  care for Windows OS.
- Deal with systems without audio device

### Changed

### Fixed

- fix flickering menus
- Workaround for Mac OS 13.0.1 cocoa bug when closing windows

#### etc.

## [20221112](https://github.com/sanette/bogue/compare/20221002...20221112)

#### older versions are not documented here yet...
