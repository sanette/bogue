# Changelog

### WIP! This changelog file is not complete!

All notable changes to the Bogue project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased](https://github.com/sanette/bogue/compare/20231209...HEAD)

### Added

- `Text_input.activate` is now public
- `Box.get_style` and `Box.set_style` are now public.

### Fixed

- fix too large cursor position when using Text_input.set

### Developement

- Remove symlinks in doc.
- The `example.ml` file is now called `examples.ml`, which is much
  more logical since it contains more than 50 examples.
- Trying to enforce that all used submodules are aliased (to make it
  clear they are used) in the header of each file, example `module
  Time = B_time`.

## [20231209](https://github.com/sanette/bogue/compare/20221112...20231209)

### Added

- Use `xdg` to compute home and config paths, and add special
  care for Windows OS.
- Deal with systems without audio device

### Changed

### Fixed

- fix flickering menus
- Workaround for Mac OS 13.0.1 cocoa bug when closing windows

#### etc.

## [20221112](https://github.com/sanette/bogue/compare/20221002...20221112)

#### older versions are not documented here yet...
