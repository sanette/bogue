#!/bin/bash

export BOGUE_DEBUG=true
export SDL_VIDEODRIVER=dummy
dune exec ./example.exe 00
