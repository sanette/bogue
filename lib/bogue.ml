(** BOGUE *)
(** A GUI Library for Ocaml, using SDL2 *)

(* This is file bogue.ml *)

(** Vu Ngoc San, December 2013 -- now *)

module Avar = B_avar
module Bogue = B_main
module Box = B_box
module Button = B_button
module Chain = B_chain
module Check = B_check
module Draw = B_draw
module Empty = B_empty
module File = B_file
module Flow = B_flow
module Image = B_image
module Label = B_label
module Layout = B_layout
module Long_list = B_long_list
module Main = B_main
module Menu = B_menu
module Mixer = B_mixer
module Mouse = B_mouse
module Popup = B_popup
module Print = B_print
module Radiolist = B_radiolist
module Sdl_area = B_sdl_area
module Select = B_select
module Selection = B_selection
module Slider = B_slider
module Snapshot = B_snapshot
module Space = B_space
module Style = B_style
module Sync = B_sync
module Table = B_table
module Tabs = B_tabs
module Text_display = B_text_display
module Text_input = B_text_input
module Theme = B_theme
module Time = B_time
module Timeout = B_timeout
module Trigger =  B_trigger
module Tvar = B_tvar
module Update = B_update
module Utf8 = B_utf8
module Utils = B_utils
module Var = B_var
module Widget = B_widget
module Window = B_window

let run_test test name =
  print_endline
    (Utils.xterm_blue ^ "* " ^ Utils.xterm_light_grey ^ name ^ Utils.xterm_nc);
  let t0 = Unix.gettimeofday () in
  test ();
  print_endline (Printf.sprintf "    [%s] successful in %f s"
                   name (Unix.gettimeofday () -. t0))


let run_tests () =

  begin let open Utils in
    run_test test_list_next_check "test_list_next_check";
    run_test test_list_prev "test_list_prev";
    run_test test_list_prev_check "test_list_prev_check";
  end;

  begin let open File in
    run_test Diff.test_diff "test_diff";
    run_test test_monitor "test_monitor";
    run_test Mime.test "Mime.test";
    run_test test_size_to_string "test_size_to_string";
    run_test test_find_index_sorted "test_find_index_sorted";
    run_test test_sorted_subarray_to_selection "test_sorted_subarray_to_selection"
  end;

  begin let open Layout in
    run_test Detect.test_almost_constant "test_almost_constant"
  end;

  begin let open Flow in
    run_test test "Flow.test"
  end;

  begin let open Utf8 in
    run_test test "Utf8.test";
    run_test test_perf "Utf8.test_perf";
    (* run_test test_perf_tmc "Utf8.test_perf_tmc" *)
  end
