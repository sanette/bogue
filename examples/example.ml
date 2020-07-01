(** all sorts of examples *)
open Tsdl
open Bogue
open Main
module W = Widget
module L = Layout
module T = Trigger
open Printf
    
let lorem = "Sed ut perspiciatis,
unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam eaque ipsa, quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt, explicabo.
Nemo enim ipsam voluptatem, quia voluptas sit, aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos, qui ratione voluptatem sequi nesciunt, neque porro quisquam est, qui dolorem ipsum, quia dolor sit, amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt, ut labore et dolore magnam aliquam quaerat voluptatem.";;

let europe = [| 
  "Austria";
  "Belgium";
  "Bulgaria";
  "Croatia";
  "Cyprus";
  "Czech Republic";
  "Denmark";
  "Estonia";
  "Finland";
  "France";
  "Germany";
  "Greece";
  "Hungary";
  "Ireland";
  "Italy";
  "Latvia";
  "Lithuania";
  "Luxembourg";
  "Malta";
  "Netherlands";
  "Poland";
  "Portugal";
  "Romania";
  "Slovakia";
  "Slovenia";
  "Spain";
  "Sweden";
  "United Kingdom" |]
;;

let sandybrown = Draw.find_color "sandybrown";;
let cornsilk = Draw.find_color "cornsilk";;

let desc0 = "Just a check button."
let example0 () =
  let b = W.check_box () in
  let layout = L.resident b in
  let board = make [] [layout] in
  run board;;

let desc1h = "Horizontal layout, box with image and border."
let example1h () =
  let b = W.check_box () in
  let td = W.text_display lorem in
  let border = Style.(border ~radius:10
                        (line ~color:Draw.(opaque grey) ~width:3 ~style:Solid ())) in

  (* the image is used as a pattern background, and so will not be scaled by
     theme (is this good ??) *)
  let p = Image.create "images/chl.png" in
  let box = W.box ~border ~background:(Style.Image p) () in
  let layout = L.flat_of_w [b;td;box] in
  (* L.animate layout (Anim.show ~duration:600 (100)); *)
  let board = make [] [layout] in
  run board;;

let desc1v = "Rich text and vertical layout sliding from right."
let example1v () =
  let b = W.check_box () in
  let h = 50 in
  let title = W.rich_text ~size:20 ~h:30 Text_display.(page [para "Text samples"]) in
  let td = W.rich_text ~h Text_display.(page [underline (para "Original:"); example]) in
  let td_normal = W.rich_text ~h 
      (let open Text_display in 
       [underline (para "Force normal style:"); normal example]) in
  let td_bold = W.rich_text ~h 
      Text_display.(page [underline (raw "Force bold:"); bold example]) in
  let td_italic = W.rich_text ~h 
      Text_display.(page [underline (para "Force italic:"); italic example]) in
  let box = W.box () in
  let layout = L.tower_of_w [b;title;td;td_normal;td_bold;td_italic;box] in
  L.slide_in ~dst:layout layout;
  let board = make [] [layout] in
  run board;;

let desc2 = "the two check buttons are independent"
let example2 () =
  let b1 = W.check_box () in
  let b2 = W.check_box () in
  let layout = L.flat_of_w [b1;b2] in
  let board = make [] [layout] in
  run board;;

let desc3 = "the first button changes the second, but not vice-versa"
let example3 () =
  let open W in
      let b1 = check_box () in
      let b2 = check_box () in
      let action w1 w2 _ =
        print_endline "---> action";
        set_check_state w2 (get_state w1) in
      let c = connect b1 b2 action T.buttons_down in
      (* add_connection b1 c;  *)(* TODO à faire autom *)
      let layout = L.flat_of_w [b1;b2] in
      let board = make [c] [layout] in
      run board;;

let desc4 = "rounded box; the whole layout is animated"
let example4 () =
  let b1 = W.check_box () in
  let b2 = W.check_box () in
  let border = Style.(border ~radius:25
                        (line ~color:Draw.(transp red) ~width:0 ~style:Solid ())) in (* try width=0 to see no antialias.. *)
  let box = W.box ~border ~background:(Style.color_bg Draw.(transp blue)) () in
  let layout = L.flat_of_w [b1;box;b2] in
  (* L.animate layout (Anim.translate 300 300); *)
  L.animate_x layout (Avar.fromto 300 0);
  L.animate_y layout (Avar.fromto 300 0);
  let board = make [] [layout] in
  run board;;

let desc5 = "We pack two layouts in a (horizontal) flat. Layouts have names (press CTRL-I). The second button is draggable"
let example5 () =
  let b1 = W.check_box () in
  let b2 = W.check_box () in
  let box = W.box () in
  let l1 = L.flat_of_w ~name:"Button 1 and the Box" [b1;box] in
  let l2 = L.resident ~name:"Button 2" ~draggable:true b2 in (* compare with (= no margin) *)
  (* let l2 = L.resident canvas b2 in *)

  let layout = L.flat ~name:"The main pack" [l1;l2] in
  let board = make [] [layout] in
  run board;;

let desc6 = "a button and a colored label. Global shortcut (ESC)."
let example6 () =
  let b = W.check_box () in
  let l = W.label ~fg:(Draw.(opaque (find_color "firebrick")))
      "Merry Christmas !" in
  let layout = L.flat_of_w ~align:Draw.Center [b;l] in
  let shortcuts = [exit_on_escape] in
  let board = make ~shortcuts [] [layout] in
  run board;;

let desc7 = "click on the button to change the label"
let example7 () = (* TODO à vérifier ! parfois ce n'est pas synchro *)
  let b = W.check_box () in
  let l = W.label "Merry Christmas !" in
  let action w1 w2 _ = print_endline "action";
    let text = if W.get_state w1
      then "Happy New Year !"
      else "Merry Christmas !" in
    Label.set (W.get_label w2) text in
  let c = W.connect b l action T.buttons_down in
  (* W.add_connection b c; *) (* TODO à faire autom *)
  let layout = L.flat_of_w [b;l] in
  let board = make [c] [layout] in
  run board;;

let desc8 = "the button and the label are inter-connected"
let example8 () =
  let b,l = W.check_box_with_label "you may click here too" in
  let layout = L.flat_of_w [b;l] in
  let board = make [] [layout] in
  run board;;

let desc9 = "the button is attached to the mouse"
let example9 () =
  let b = W.check_box () in
  let btn = L.flat_of_w [b] in
  let layout = L.flat [btn] in
  L.set_width layout 500; L.set_height layout 500;
  (* L.animate layout (Anim.follow_mouse ()); *)
  L.follow_mouse ~dx:20 ~dy:20 btn;
  let board = make [] [layout] in
  run board;;

(* BUG: the check box is active only after the window has focus (hence we have
   to click twice) *)
let desc10 = "two independent windows should open. ESC to quit."
let example10 () =
  let b1 = W.check_box () in
  let b2 = W.check_box () in
  let l1 = L.flat_of_w ~name:"Window#1" [b1; W.label "First window"] in
  Draw.use_new_layer (); (* we start a new window *)
  let l2 = L.flat_of_w ~name:"Window#2" [b2; W.label "Win 2"] in

  let shortcuts = [exit_on_escape] in
  let board = make ~shortcuts [] [l1;l2] in
  (* window position can be set after "make" and before "run" *)
  L.set_window_pos l1 (200,200); L.set_window_pos l2 (400,200);
  run board;;

let desc11 = "two connected windows: the button in the first window sets the check_box in the second window"
let example11 () = (* attention ne marche pas avec DEBUG=false !! OK problème résolu: le main thread ne laissait pas assez de temps aux autres *)
  let b1 = W.check_box () in
  let b2 = W.check_box () in
  let action  w1 w2 _ =
    W.set_check_state w2 (W.get_state w1) in
  let c = W.connect b1 b2 action T.buttons_down in
  (* W.add_connection b1 c;  *)(* TODO à faire autom *)
  let l1 = L.flat_of_w [b1; W.label "Window 1 = the master"] in
  Draw.use_new_layer ();
  let l2 = L.flat_of_w [b2; W.label "Window 2"] in
  let shortcuts = [exit_on_escape] in
  let board = make ~shortcuts [c] [l1;l2] in
  L.set_window_pos l1 (200,200); L.set_window_pos l2 (400,200);
  run board;;

let desc12 = "a check_box and a text input in a line editor"
let example12 () =
  let b = W.check_box () in
  let ti = W.text_input ~size:16 ~prompt:"Click and enter some text " () in
  let l = L.tower_of_w [b;ti] in
  let board = make [] [l] in
  run board;;

let desc13 = "circular sliders"
let example13 () =
  let background = L.bg_color in
  let s = W.slider ~kind:Slider.Circular 100 ~tick_size:4 in
  let s' = W.slider ~kind:Slider.Circular 10 ~length:100 ~thickness:6 ~tick_size:3 in
  let l =  L.flat [L.resident ~background s; L.resident s'] in
  let board = make [] [l] in
  run board;;

let desc14 = "a slider connected to a text label, and fit window"
let example14 () =
  let s = W.slider ~step:10 100 in
  let l = W.label "       Click on the slider         " in
  let s' = W.slider ~kind:Slider.Vertical 100 in
  let l' = W.label "       Click on the slider         " in
  let s'' = W.slider ~kind:Slider.HBar 100 in
  let l'' = W.label "       Click on the slider         " in
  let action w1 w2 _ =
    let x = Slider.value (W.get_slider w1) in
    Label.set (W.get_label w2) (sprintf "You have selected: %u%%" x) in
  let events = List.flatten [T.buttons_down; T.buttons_up; T.pointer_motion; [Sdl.Event.key_down]] in
  (* NOTE instead of using this connection/events, one can use
     W.slider_with_action. Cf example/bounce *)
  let c = W.connect_main s l action events in
  let c' = W.connect_main s' l' action events in
  let c'' = W.connect_main s'' l'' action events in
  (* notice that action is a pure function, we can use it for both connections *)
  let slider = L.resident ~background:L.bg_color s in
  let slider' = L.resident ~background:(L.color_bg Draw.(transp green)) s' in
  let slider'' = L.resident ~background:(L.color_bg Draw.(transp red)) s'' in
  let lay =  L.tower [
      L.flat ~align:Draw.Center [slider; L.resident l];
      L.flat ~align:Draw.Center [slider'; L.resident l'];
      L.flat ~align:Draw.Center [slider''; L.resident l'']
    ] in
  let board = make [c;c';c''] [lay] in
  run board;;

let desc15 = "two (independent) clocks in text label; one is starting \
              automatically"
let example15 () =
  let clock w1 _ ev =
    let rec loop () =
      let tm = Unix.localtime (Unix.time ()) in
      let s = Unix.(sprintf "%02u:%02u:%02u" tm.tm_hour tm.tm_min tm.tm_sec) in
      Label.set (W.get_label w1) s;
      W.update w1;
      let drift = Unix.gettimeofday () -. (Unix.time ()) in
      (* printf "Drift = %f\n" drift; *) (* we try to keep the drift near 0.5 *)
      if drift < 0.45 then Thread.delay (0.51 -. drift)
      else if drift > 0.55 then Thread.delay (1.49 -. drift) (* we are too late *)
      else Trigger.nice_delay ev 0.999;
      if T.should_exit ev
      then (print_endline "Stopping Clock"; T.will_exit ev)
      else loop () in
    print_endline "Starting new clock";
    loop () in
  let l = W.label "Click to start clock" in
  let l' = W.label ~size:40 "Autostarts" in
  let c = W.connect l l clock T.buttons_down in
  let c' = W.connect l' l' clock [T.startup] in
  let lay =  L.flat_of_w [l;l'] in
  let board = make [c;c'] [lay] in
  run board;;

let desc16 = "buttons with label"
let example16 () =
  let b = W.button "Press Me" in
  let c = W.button ~kind:Button.Switch "Click Me" in
  let fg = Draw.(opaque black) in
  let bg_off = Style.color_bg Draw.none in
  (* let bg_on = Style.color_bg Draw.(opaque blue) in *)
  let bg_over = Style.color_bg Draw.(opaque grey) in
  let d = W.button ~bg_off (* ~bg_on *) ~bg_over ~kind:Button.Switch
      ~label_on:(Label.icon ~fg "train")
      ~label_off:(Label.icon ~fg:(Draw.(lighter (lighter fg))) "train")
      "" in
  let layout = L.flat_of_w [b;c;d] in
  let board = make [] [layout] in
  run board;;

let desc17 = "a simple image at original pixel size"
let example17 () =
  let img = W.image (*~w:300*) ~noscale:true ~bg:Draw.(opaque white) "images/chl.png" in
  let layout = L.flat_of_w [img] in
  let board = make [] [layout] in
  run board;;

(* TODO *)
let desc18 = "zoom_in and oscillate animations"
let example18 () =
  let img1 = W.image ~w:300 ~h:300 ~bg:Draw.(opaque white) "images/chl.png" in
  let img2 = W.image ~w:250 ~h:300 ~bg:Draw.(opaque grey) "images/chl.png" in
  let l1 = L.resident img1 in
  let l2 = L.resident img2 in
  let layout = L.flat [l1; l2] in
  L.set_width layout 700;
  L.oscillate ~frequency:10. 20 l2;
  (* put this AFTER creating the layout, otherwise the x-pos of l2 is 0 *)
  L.zoom ~from_factor:0.1 ~to_factor:1. l1;
  let board = make [] [layout] in
  run board;;

let desc19 = "tabs"
let example19 () =
  let img = W.image ~w:300 ~h:300 ~bg:Draw.(opaque white) "images/chl.png" in
  let ti = W.text_input ~size:16 ~prompt:"Click and enter some text " () in
  let b,l = W.check_box_with_label "you may click here too" in
  let tab1 = L.flat_of_w [img] in
  let tab2 = L.flat_of_w [ti] in
  let tab3 = L.tower_of_w [b;l] in
  let tabs = Tabs.create ~slide:Avar.Right ~adjust:Layout.Nothing
      ["Check box", tab3; "Image", tab1; "Text entry", tab2; ] in
  let board = make [] [tabs] in
  run board;;

let desc20 = "two images"
let example20 () =
  let img1 = W.image ~w:300 ~h:300 ~bg:Draw.(opaque white) "images/chl.png" in
  let img2 = W.image ~w:300 ~h:300 ~bg:Draw.(opaque grey) "images/chl.png" in
  let l1 = L.flat_of_w [img1] in
  let l2 = L.flat_of_w [img2] in
  (* this has no effct: L.animate_w l1 (Avar.fromto ~duration:600 10 300); *)
  let layout = L.flat [l1;l2] in
  let board = make [] [layout] in
  run board;;

let desc21 = "popup"
let example21 () =
  let b = W.check_box () in
  let ti = W.text_input ~size:16 ~prompt:"Click and enter some text " () in
  let td = W.text_display lorem in
  let l = W.label " This is on top of the other widgets " in
  let close_btn = W.button ~border_radius:3 ~border_color:Draw.(opaque blue) "Close" in
  let popup = L.tower_of_w [l;ti;close_btn] in
  let layout = L.tower_of_w [b;td] in
  let screen = Popup.attach ~show:false ~bg:(Draw.(set_alpha 220 (pale green))) layout popup in
  let button = W.button ~kind:Button.Switch
      ~border_radius:4 ~border_color:Draw.(opaque grey) "Popup" in
  let release b =
    let state = Button.state (W.get_button b) in
    L.set_show popup state;
    L.set_show screen state in
  W.on_release ~release button;
  let close b =
    Button.reset (W.get_button b);
    release b  in
  let c = W.connect_main close_btn button (fun _ b _ -> close b) T.buttons_up in
  
  let global = L.tower [L.resident button; layout] in
  let board = make [c] [global] in
  run board;;

let desc21bis = "Close popup"
let example21bis () =
  let td = W.text_display lorem in
  let layout = L.tower_of_w [W.check_box (); td] in
  Popup.info ~w:100 ~h:70 "Click on Close to close the popup" layout;
  let board = make [] [layout] in
  run board;;

let desc21ter = "Yes/No popup"
let example21ter () =
  let td = W.text_display lorem in
  let layout = L.tower_of_w [td] in
  let yes_action () = print_endline "YES!" in
  let no_action () = print_endline "NO!" in
  Popup.yesno ~w:100 ~h:50 "Are you happy?" ~yes_action ~no_action layout;
  let board = make [] [layout] in
  run board;;

(* TODO this one does not work as expected. Cf menus *)
(* BUG: when you hide the inner box, and then the outer box; and then open the
   outer box again, then the offset position of the innex box isn't
   correct. After two clicks, it comes back to the right position. *)
let desc22 = "hide/show"
let example22 () =
  let b = W.check_box ~state:true () in
  let l = W.label "Click button to hide/show" in
  let td = W.text_display lorem in
  let b2 = W.check_box ~state:true () in
  let l2 = W.label "Click button to hide/show" in
  let td2 = W.text_display lorem in
  let button2 = L.flat_of_w [b2;l2] in
  let hide_show2 = L.flat_of_w [td2] in
  let hide_show = L.flat [L.flat_of_w [td]; button2; hide_show2] in
  let button = L.flat_of_w [b;l] in
  let layout = L.tower [button; hide_show] in
  let action room w _ _ =
    if W.get_state w
    then (L.show (* ~from:Anim.Top *) room; L.fade_in room)
    else (L.hide (* ~towards:Anim.Top *) room; L.fade_out room)  in
  let c = W.connect b td (action hide_show) T.buttons_down in
  let c2 = W.connect b2 td2 (action hide_show2) T.buttons_down in
  let board = make [c;c2] [layout] in
  run board;;

let desc23 = "fade-in"
let example23 () =
  let td = W.text_display lorem in
  let layout = L.tower_of_w [td] in
  L.fade_in ~duration:2000 layout;
  (* layout.L.anim <- Some (Anim.fade_in ~duration:600 ()); *)
  let board = make [] [layout] in
  run board;;

let desc23bis = "fade-out"
let example23bis () = 
  let td = W.text_display lorem in
  let layout = L.tower_of_w [td] in
  L.fade_out ~duration:2000 layout;
  (* layout.L.anim <- Some (Anim.fade_in ~duration:600 ()); *)
  let board = make [] [layout] in
  run board;;

(* TODO this doesn't work anymore after modification of mouse_focus in main. Cf
   check_mouse_motion not being called when animating? *)
let desc24 = "two moving check buttons covered by a screen"
let example24 () =
  let b1 = W.check_box () in
  let b2 = W.check_box () in
  let blank = "[________________________________]" in
  let l = W.label blank in
  let btns = L.flat_of_w [b1;b2] in
  L.oscillate ~frequency:10. 20 btns;
  let layout = L.flat [btns; L.resident l] in
  let screen = Popup.add_screen btns in
  let action _ w2 _ =
    Label.set (W.get_label w2) "You clicked on the screen layer!";
    W.update w2;
    Thread.delay 5.;
    L.hide screen;
    Label.set (W.get_label w2) "Now we remove the screen"
  in

  let c = W.connect (L.widget screen) l action T.buttons_down in

  let board = make [c] [layout] in
  run board;;

let desc25 = "a menu bar"
let example25 () =

  (* First we define a dummy layout, just to see how the menu will cover it.*)
  let border = Style.(border ~radius:25
                        (line ~color:Draw.(opaque red) ~width:6 ~style:Solid ())) in
  let box = W.box ~border ~background:(Style.color_bg Draw.(transp blue)) ~w:400 ~h:300 () in
  let menu_placeholder = L.empty ~w:400 ~h:40 () in
  let main = L.tower
      [menu_placeholder;
       L.superpose
         [L.tower [L.flat_of_w [W.label "   Hello there"; W.check_box ()];
                   L.empty ~w:0 ~h:100 ();
                   L.resident (W.check_box ())];
          L.flat_of_w [box]]] in
  (* : recall that for the moment, the first item in the list of rooms gets
     focus before the next ones (bug, we should change, this is not usual) *)

  (* Now we construct the menu... *)
  let () = let open Menu in
    let action1 () = print_endline "Action = Item 1" in
    let action2 () = print_endline "Action = Item 2" in

    (* We define the About entry: *)
    let about () = Popup.info "This is Bogue example 25:\n\nA menu bar.." main in
    let about =  { label = Text "About..."; content = Action about } in

    (* We define the File menu: *)
    let save () = print_endline "Saving..." in
    let save = { label = Text "Save"; content = Action save } in
    let quit () = print_endline "Quitting."; raise Bogue.Exit in
    let quit = { label = Text "Quit"; content = Action quit } in
    let file_menu = [save; quit] in
    let file = { label = Text "File";
                 content = Tower file_menu} in

    (* We define another menu with two submenus: *)
    (* It is ok to re-use an entry or an action because everything is immutable
       here. *)
    let label1 = { label = Text "Copy of action 1";
                   content = Action action1 } in
    let label2 = { label = Text "Entry with action 2";
                   content = Action action2 } in
    let submenu = [ label1; label2 ] in
    let submenu2 = (List.concat [submenu;
                                 [{ label = Text "submenu";
                                    content = Tower submenu }]]) in
    let submenu3 = List.concat [submenu2; [separator];
                                [{ label = Text "submenu2";
                                   content = Tower submenu2 }]] in
    let menu2 = { label = Text "This a long menu title";
                  content = Tower submenu3 } in

    (* Now we define the complete menu bar: *)
    bar ~dst:main [file; menu2; about]
  in
  let layout = L.tower ~margins:0
      ~background:(L.color_bg(Draw.(lighter (opaque pale_grey)))) [main] in
  let board = make [] [layout] in
  run board;;

let desc26 = "the mouse enter/leave event"
let example26 () =
  let box = W.box ~w:200 () in
  let room = L.resident box in
  let l = W.label "Put the mouse over the box" in
  let action_enter _ w2 _ = print_endline "action_enter";
    L.animate_w room (Avar.fromto ~duration:100 (L.width room) 210);
    Label.set (W.get_label w2) "Mouse entered" in
  let ce = W.connect box l action_enter [T.mouse_enter] in
  let action_leave _ w2 _ = print_endline "action_leave";
    L.animate_w room (Avar.fromto ~duration:100 (L.width room) 200);
    Label.set (W.get_label w2) "Mouse left";
    W.update w2; 
    (* after leaving, the box is not active, so it is possible that no event get
       triggered, therefore we manually update the target. *)
  in
  let cf = W.connect box l action_leave [T.mouse_leave] in
  let layout = L.flat ~margins:20 [room; L.resident l] in
  let board = make [ce;cf] [layout] in
  run board;;

let desc26bis = "the mouse enter/leave event + box shadow"
let example26bis () =
  (* same as example26, here one uses l as a global variable, and no thread *)
  (* Which one is the best?? *)
  let background = Style.color_bg Draw.(opaque grey) in
  let shadow = Style.shadow () in
  let box = W.box ~w:200 ~shadow ~background () in
  let room = L.resident box in
  let l = W.label "Put the mouse over the box" in
  let enter _ = print_endline "action_enter";
    L.animate_w room (Avar.fromto ~duration:100 (L.width room) 210);
    Label.set (W.get_label l) "Mouse entered" in
  let leave _ = print_endline "action_leave";
    L.animate_w room (Avar.fromto ~duration:100 (L.width room) 200);
    Label.set (W.get_label l) "Mouse left";
    W.update l; 
    (* after leaving, the box is not active, so it is possible that no event get
       triggered, therefore we manually update the target. *)
  in
  W.mouse_over ~enter ~leave box;
  let layout = L.flat ~margins:20 [room; L.resident l] in
  let board = make [] [layout] in
  run board;;

let desc27 = "scrolling and print layout information"
let example27 () =
  let hello = L.resident (W.label "Hello") in
  let td () = L.resident (W.text_display ~w:600 ~h:300 lorem) in
  let fin = L.resident (W.label "END") in
  let debut = L.resident (W.label "START") in
  let long = L.tower ~sep:0 [debut; td (); td (); td (); td (); td (); td (); td (); fin] in
  let long2 = L.tower ~sep:0 [td (); td (); td (); td (); td (); td (); td ()] in
  let container = L.make_clip ~h:300 long in
  let container2 = L.make_clip ~h:300 long2 in
  let layout = L.flat [hello; container; container2] in
  (* L.scroll_to ~duration:2000 300 long; *)
  let board = make [] [layout] in
  print_endline (Print.layout_down container);

  run board;;

let desc28 = "select list + Timeout"
let example28 () =
  let l = L.resident (W.label "Please select your country") in
  let fruits =[| "apple"; "orange"; "banana"; "strawberry" |] in
  let box = L.flat_of_w ~sep:0 [W.box ~w:500 ~h:100 ()] in
  let select = Select.create europe 0 in
  let select_fruit = Select.create fruits 2 in
  let layout = L.tower [L.flat [l; select; select_fruit]; box] in
  let board = make [] [layout] in
  let _ = Timeout.add 5000 (fun () -> print_endline "HELLO!---------------------") in
  run board;;
              
let desc29 = "radiolist"
let example29 () =
  let radio = Radiolist.vertical [|"only one can be selected"; "AAA"; "BBB"; "CCC"|] in
  let board = make [] [Radiolist.layout radio] in
  run board;;

let desc30 = "radiolist and interaction + a timeout"
let example30 () =
  let radio = Radiolist.vertical
      [|"only one can be selected"; "AAA"; "BBB"; "CCC"|] in
  let label = W.label "   Please make your choice   " in
  let action _ l _ =
    let sel = Utils.(default (map_option (Radiolist.get_index radio) string_of_int) "nothing") in
    Label.set (W.get_label l) (sprintf "You have selected: %s" sel) in

  let cs = List.map (fun w -> W.connect w label action T.(update::buttons_down))
      (Radiolist.active_widgets radio) in
  (* we create the list of connections for each radiolist entry to the label: *)
  (* Here it would also work with T.buttons_down instead of [T.update], but the
     latter is preferable in case the radio buttons are modified directly
     without clicking, cf Timeout below. (or via TAB, not implemented yet) *)
  
  let layout = L.flat ~align:Draw.Center [Radiolist.layout radio; L.resident label] in
  let board = make cs [layout] in
  let _ = Timeout.add 5000 (fun () ->
      print_endline "SET INDEX TO 3";
      Radiolist.set_index radio 3) in
  run board;;

let desc31 = "a Long List"
let example31 () =
  Random.self_init ();
  let sizes = Array.init (Array.length europe) (fun _ -> 100+(Random.int 100)) in
  let generate i = L.resident ~h:(sizes.(i)) (W.label europe.(i)) in
  let long = Long_list.create ~w:300 ~h:400 ~generate
      ~length:(Array.length europe) ~max_memory:700000 () in
  (* max_memory = 700000 is chosen to force the algo to do some garbage
     collection, but in most cases max_memory can be much larger *)
  let board = make [] [long] in
  run board;;

let desc32 = "a very Long List"
let example32 () =
  let w = 200 in
  let generate i = let background = if i mod 2 = 0
                     then Some (L.color_bg Draw.(transp (pale (pale green))))
                     else None in
    L.resident ~h:50 ~w ?background (W.label (string_of_int i)) in
  (* for very long lists a height_fn should be provided, otherwise it can be
     very slow (and blocking) *)
  let height_fn _ = Some 50 in
  let long = Long_list.create ~w ~h:400 ~generate ~height_fn
      ~length:100000 ~max_memory:700000 () in

  let board = make [] [long] in
  run board;;

let desc33 = "a very Long List with varying sizes and colors"
let example33 () =
  let w = 200 in
  let height_fn i = Some (Utils.round (50. *. sin (float i /. 10.) +. 60.)) in
  let generate i =
    let background = Some (L.color_bg Draw.(set_alpha ((27*i) mod 255) green)) in
    L.resident ?h:(height_fn i) ~w ?background (W.label (string_of_int i)) in

  let long = Long_list.create ~w ~h:400 ~generate ~height_fn
      ~length:100000 ~max_memory:1000000 () in

  let board = make [] [long] in
  run board;;

let desc34 = "a very Long List (one million entries) with persistent checks and nonlinear slider"
let example34 () =
  let length = 1_000_000 in
  (* in long lists, layouts are (re)created on-the-fly, hence we need to store
     their data separately if we want to use it... *)
  let data = Array.make length false in
  let w = 200 and h = 30 in
  let generate i = let background = if i mod 2 = 0
                     then Some (L.color_bg Draw.(transp (pale (pale green))))
                     else None in
    let state = data.(i) in
    let b = W.check_box ~state () in
    let click w = data.(i) <- W.get_state w in
    W.on_click ~click b; (* we need to add connections dynamically *)
    let bl = L.resident ~w:20 ~h b in
    let l = L.resident ~w:(w-20) ~h (W.label (string_of_int i)) in
    L.flat ~sep:0 ~margins:10 ?background [bl;l] in
  let height_fn _ = Some (h+20) in
  let long = Long_list.create ~w ~h:400 ~generate ~height_fn ~linear:false
      ~length ~max_memory:900000 () in
  (* this max_memory is not enough for 1000000 entries; the program will change
     it automatically *)

  let board = make [] [long] in
  run board;;

let desc35 = "a table"
let example35 () =
  let length = Array.length europe in
  let col1 = Table.{
      title = "Country";
      length;
      rows = (fun i -> L.resident (W.label europe.(i)));
      compare = Some (fun i j -> compare europe.(i) europe.(j));
      width = Some 100} in
  let col2 = Table.{
      title = "Initial";
      length;
      rows = (fun i -> L.resident (W.label (String.sub europe.(i) 0 1)));
      compare = None;
      width = Some 50} in
  let col3 = Table.{
      title = "Length";
      length;
      rows = (fun i -> L.resident (W.label (string_of_int
                                              (String.length europe.(i)))));
      compare = Some (fun i j -> compare
                         (String.length europe.(i))
                         (String.length europe.(j)));
      width = Some 70} in
  let table, _ = Table.create ~h:400 [col1; col2; col3] in

  let board = make [] [table] in
  run board;;

let desc36 = "playing sound"
let example36 () =
  Mixer.test ();;

let desc37 = "playing sound when clicking"               
let example37 () =
  let devname = Mixer.init () in
  let mixer = Mixer.create_mixer devname in
  let check_sound = Mixer.load_chunk mixer "../tests/audio/button.wav" in
  let uncheck_sound = Mixer.load_chunk mixer "../tests/audio/swoosh.wav" in
  Mixer.change_volume 0.1 uncheck_sound;
  let b = W.check_box () in
  let click b = 
    let sound = if W.get_state b then check_sound else uncheck_sound in 
    ignore (Mixer.play_chunk mixer sound) in
  W.on_click ~click b;
  let td = W.text_display lorem in
  let border = Style.(border ~radius:10
                        (line ~color:Draw.(opaque grey) ~width:3 ~style:Solid ())) in

  let p = Image.create "images/chl.png" in
  let box = W.box ~border ~background:(Style.Image p) () in
  let layout = L.flat_of_w [b;td;box] in
  let board = make [] [layout] in
  Mixer.unpause mixer;
  run board;
  Mixer.close mixer;;
  (* Mixer.free_chunk check_sound;
   * Mixer.free_chunk uncheck_sound *)

let desc38 = "load SVG at different sizes"
let example38 () =
  (* SVGs are loaded at optimal resolution, which means that the Theme.scale is
     taken into account *)
  
  (* one can load an svg image as a background; it will be repeated as a pattern
     to fill the box: *)
  let bg = Image.create_from_svg ~width:300 ~height:100 "images/w3c-logo-white.svg" in
  let box = W.box ~w:300 ~h:300 ~background:(Style.Image bg) () in

  (* one can load an svg image as a widget; it will be scaled to fit the
     size: *)
  let img = W.image_from_svg ~h:300 ~bg:Draw.(opaque red)
      "images/koala.svg" in

  let layout = L.flat_of_w [box; img] in
  let board = make [] [layout] in
  run board;;

let desc39 = "the hfill/vfill elements. Try to resize the window."
let example39 () =
  let background = L.bg_color in
  let label1 = W.label "Left" in
  let label2 = W.label "right" in
  let line1 = L.flat ~background
      [L.resident label1; Space.hfill (); L.resident label2] in
  (* We set initial width to 500: *) 
  L.set_width line1 500;
  (* But then the width will follow the width of the container (here, the
     window): *)
  Space.full_width ~margin:0 line1; 
  let label3 = W.label "Bottom" in
  let room3 = L.resident label3 in
  let line2 = L.flat ~background
      [room3; (*Space.hfill ();*) L.resident (W.label "Bottom right")] in

  Space.full_width ~margin:0 line2;
  Space.make_hfill room3;

  let label4= W.label "VCenter" in
  let room4 = L.resident label4 in
  let center_area = L.tower ~sep:0 [room4] in
  Space.vcenter room4; (* does not work very well because there is a conflict with vfill in layout below *)
  Space.make_vfill ~margin:5 center_area;

  let layout = L.tower ~sep:5 [line1; center_area; (*Space.vfill ~margin:5 ();*) line2] in
  L.set_height layout 500;

  let board = make [] [layout] in
  run board;;

let desc40 = "rearrange, and gradient background"
let example40 () =
  let b1 = W.check_box () in
  let l = W.text_display ~w:100 ~h:80 "Click on the button to rearrange layout"
          |> L.resident in
  let border = Style.(border ~radius:5
                        (line ~color:Draw.(transp red) ~width:0 ~style:Solid ())) in
  let box = W.box ~border
      ~background:(Style.gradient ~angle:45. Draw.[opaque sandybrown; opaque cornsilk]) () in
  let layout = L.tower [L.resident b1;L.resident box;l] in
  L.set_width layout 400;
  let click w =
    if W.get_state w
    then (print_endline "Rearranging layout to a flat layout";
          L.reflat layout)
    else (print_endline "Rearranging layout to a tower layout";
          L.retower layout) in
  W.on_click ~click b1;
  let board = make [] [layout] in
  run board;;

let desc41 = "game (fake)"
let example41 () =
  let image = W.image ~w:1024 ~h:768 "images/nasa_black_hole_cygx1_ill.jpg" in
  let image = L.flat ~name:"image" [L.resident image] in
  let title = W.label ~size:32 ~fg:(Draw.(opaque (find_color "firebrick")))
      "The Black Hole Game"
              |> L.resident in
  let border = Style.(border (line ~color:Draw.(opaque grey) ~width:3 ~style:Solid ())) in
  let bg = Box.create ~border () in
  let fg = Draw.(opaque white) in
  let make_btn x y text =
    let l = W.label ~fg text in
    (* alternative: *)
    (*let b = W.button ~border_radius:7 ~bg_off:Draw.(transp grey) ~fg text in *)
    let r = L.tower ~name:"game button" ~margins:0
        [L.resident ~w:100 ~h:40 ~background:(L.box_bg bg)
           (*b*) l] in
    L.setx r x; L.sety r y; r in
  let start_btn = make_btn 800 500 "Start" in
  let quit_btn = make_btn 800 600 "Quit" in
  print_endline(Printf.sprintf "quit_btn pos = (%i,%i)"
                  (Layout.getx quit_btn) (Layout.gety quit_btn));
  let entries = let open Menu in
    [ { label = Layout start_btn;
        content = Action (fun () -> print_endline "START!") };
      { label = Layout quit_btn;
        content = Action (fun () -> print_endline "QUIT!";
                           Trigger.push_quit ()) } ] in
  let _ = Menu.create ~dst:image (Menu.Custom entries) in
  let layout = L.superpose [image; title] in
  L.setx title 35; L.sety title 150;
  L.rotate ~duration:1000 ~angle:360. title;
  let board = make [] [layout] in
  run board;;

let desc42 = "effect of rotate on a composite room"
let example42 () =
  let l = W.label "Rotation" in
  let td = W.text_display lorem in
  let background = L.bg_color in
  let layout = L.tower_of_w ~background [l;td] in
  L.rotate ~duration:5000 ~angle:180. layout;
  let board = make [] [layout] in
  run board;;

let desc43 = "snapshot, rotation and zoom"
let example43 () =
  let left = W.label "LEFT" in
  let top = W.label "TOP" in
  let l = W.label "We snapshot this pack:" in
  let b = W.check_box () in
  let box = W.box () in
  let background = L.bg_color in
  let room = L.tower ~background
      [ L.resident l;
        L.flat_of_w [b;box] ] in
  let s = Snapshot.create room in
  let snap = L.resident s in
  let duration = 500 in
  L.rotate ~angle:360. ~duration snap;
  L.zoom ~from_factor:0.1 ~to_factor:1. ~duration snap;
  let layout = L.tower ~align:Draw.Center
      [ L.resident top;
        L.flat ~align:Draw.Center
          [ L.resident left;
            L.flat [room; snap]
          ]
      ] in
  let board = make [] [layout] in
  run board;;

let desc44 = "tooltips"
let example44 () =
  let b = W.button "Some Button" in
  let b' = W.button "Another Button" in
  let td = W.text_display lorem in
  let target = L.resident b in
  let target' = L.resident b' in
  let layout = L.tower [L.flat [target; target']; L.resident td] in
  
  Popup.tooltip "I'm a tooltip located near the mouse pointer"
    ~position:Popup.Mouse ~target b layout;
  Popup.tooltip "Tooltip below the button"
    ~position:Popup.Below ~target:target' b' layout;
  
  let board = make [] [layout] in
  run board;;

let desc45 = "layout shadow" 
let example45 () =
  let shadow = Style.shadow () in
  let background = Style.color_bg Draw.(transp blue) in
  let b = W.box ~shadow ~background ~w:50 ~h:50 () in (* OK *)
  let bg = L.color_bg Draw.(transp green) in
  let l = L.flat ~margins:50 ~shadow ~background:bg [L.resident b] in(* BUG *)
  let b2 = Box.create ~shadow ~background ~width:50 ~height:50 () in
  let l2 = L.flat ~margins:50 ~shadow ~background:(L.box_bg b2)
             [L.empty ~w:50 ~h:50 ()] in
  let large = L.flat ~margins:60 [l; l2] in
  let board = make [] [large] in
  run board;;

let desc46 = "Do things without any window! (tic every second)"
let example46 () =
  let rec tic () =
    print_endline "tic";
    Timeout.add 1000 tic
    |> ignore in
  tic ();
  let board = make [] [] in
  run board;;
    

  
let _ =
  let examples = [  "0", (example0, desc0) ;
                    "1h", (example1h, desc1h) ;
                    "1v", (example1v, desc1v) ;
                    "2", (example2, desc2) ;
                    "3", (example3, desc3) ;
                    "4", (example4, desc4) ;
                    "5", (example5, desc5) ;
                    "6", (example6, desc6) ;
                    "7", (example7, desc7) ;
                    "8", (example8, desc8) ;
                    "9", (example9, desc9) ;
                    "10", (example10, desc10) ;
                    "11", (example11, desc11) ;
                    "12", (example12, desc12) ;
                    "13", (example13, desc13) ;
                    "14", (example14, desc14) ;
                    "15", (example15, desc15) ;
                    "16", (example16, desc16) ;
                    "17", (example17, desc17) ;
                    "18", (example18, desc18) ;
                    "19", (example19, desc19) ;
                    "20", (example20, desc20) ;
                    "21", (example21, desc21) ;
                    "21bis", (example21bis, desc21bis) ;
                    "21ter", (example21ter, desc21ter) ;
                    "22", (example22, desc22) ;
                    "23", (example23, desc23) ;
                    "23bis", (example23bis, desc23bis) ;
                    "24", (example24, desc24) ;
                    "25", (example25, desc25) ;
                    "26", (example26, desc26) ;
                    "26bis", (example26bis, desc26bis) ;
                    "27", (example27, desc27) ;
                    "28", (example28, desc28) ;
                    "29", (example29, desc29) ;
                    "30", (example30, desc30) ;
                    "31", (example31, desc31) ;
                    "32", (example32, desc32) ;
                    "33", (example33, desc33) ;
                    "34", (example34, desc34) ;
                    "35", (example35, desc35) ;
                    "36", (example36, desc36) ;
                    "37", (example37, desc37) ;
                    "38", (example38, desc38) ;
                    "39", (example39, desc39) ;
                    "40", (example40, desc40) ;
                    "41", (example41, desc41) ;
                    "42", (example42, desc42) ;
                    "43", (example43, desc43) ;
                    "44", (example44, desc44) ;
                    "45", (example45, desc45) ;
                    "46", (example46, desc46) ;
                    

                 ] in
  let all = List.map fst examples in
  let to_run = List.tl (Array.to_list Sys.argv)
               |> List.filter (fun s -> s <> "-h") in
  let help = List.length to_run <> Array.length (Sys.argv) - 1 in
  let to_run = if to_run == [] then all else to_run  in              
  (* for instance to_run = [ "23"; "23bis" ] *) 
  let exs = try (List.map (fun key -> key, List.assoc key examples) to_run)
            with Not_found -> failwith "Cannot find requested example"
  in
  List.iter (fun (key, (ex,de)) ->
      print_endline (key ^ " = " ^ de);
      if not help then ex ()) exs;

  Draw.quit ();
  if help then (
    print_newline ();
    print_endline (sprintf "Usage: %s [-h] [id1] [id2 ...]" Sys.argv.(0));
    print_endline "Where `id1`, `id2`, etc. are the identifiers of the example \
                   demos you whish to run. If no `id` is given, all examples are \
                   selected.";
    print_endline "Use `-h` if you want to print the description without running \
                   the demo.";
    print_newline ()
  );
  Stdlib.exit 0;;

(* Attention le 16 ne marche pas après le 15: on reste bloqué sur 
   Thread: Waiting for locked variable to unlock...==> corrigé *)
