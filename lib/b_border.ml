(* draw fancy borders to arbitrary shapes *)

(* f : parameterize the boundary *)
(* h : distance from boundary (<0 inside) *)

open Tsdl
open B_utils   
module Theme = B_theme
module Draw = B_draw

type physicalpoint = Sdl.point
type logicalpoint = { x:float; y:float } (* points or vectors *)
(* for both, origin is top-left *)

type boundary = float -> logicalpoint
type normal = float -> logicalpoint (* normal unit vector *)
type distance = logicalpoint -> float

type shape = {
  boundary : boundary; (* parameterize the boundary from t in [0,1], ideally
                          when t is the rescaled arclength coordinate. *)
  normal : normal; (* unit normal vector at parameter t *)
  distance : distance; (* some function R² -> R that defines the shape as the
                          set of points where it is negative *)
  size : logicalpoint (* box size *)
}

let add p1 p2 =
  { x = p1.x +. p2.x; y = p1.y +. p2.y }

let mult s p =
  { x = p.x *. s; y = p.y *. s }

let dot p1 p2 =
  p1.x *. p2.x +. p1.y *. p2.y

let norm_square p = dot p p

let norm p = sqrt (dot p p)
    
type colorfn = float -> float -> Draw.color
(* t -> s -> color at boundary t and distance s along the normal.  t and s are
   in [0,1]*)

let getx = Sdl.Point.x
let gety = Sdl.Point.y
             
let logical_to_physical p =
  Sdl.Point.create
    ~x:(Theme.scale_from_float p.x) ~y:(Theme.scale_from_float p.y)

let physical_to_logical p =
  { x = Theme.unscale_to_float (getx p);
    y = Theme.unscale_to_float (gety p)}

(* approximates a 0.5+N(0,1) law (random float in [0,1]) *)
let gaussian_float () =
  let n = 10 in
  let rec loop i x =
    if i = 0 then x /. (float n)
    else loop (i-1) (x +. Random.float 1.)
  in
  loop n 0.

(* draw (rectangular) pixels *)
let draw_pixel renderer ?(size=1.0) p =
  let psize = Theme.scale_from_float size in
  print_endline (Printf.sprintf "PSIZE=%i" psize);
  if psize = 1 (* TODO subpixel rendering *)
  then go (Sdl.render_draw_point renderer (getx p) (gety p))
  else if psize > 1
  then Draw.box renderer (getx p) (gety p) psize psize

let draw_random renderer ?(border_width = 10.) shape colorfn npoints =
  for _ = 1 to npoints do
    let t = Random.float 1. in
    let s = 2. *. abs_float (gaussian_float () -. 0.5) in
    let p = shape.boundary t in
    let n = shape.normal t
            |> mult (s *. border_width) |> add p |> logical_to_physical in
    let r,g,b,a = colorfn t s in
    go (Sdl.set_render_draw_color renderer r g b a);
    print_endline (Printf.sprintf "POINT = %i,%i, COLOR = %i,%i,%i,%i" (getx n) (gety n) r g b a);
    draw_pixel renderer n
  done

(* various shapes *)
let rectangle x0 y0 w h =
  let t1 = w /. (2. *. ( w +. h))
  and t2 = h /. (2. *. ( w +. h)) in
  let boundary t =
    let x, y = if t < t1 then t *. w /. t1, 0.
      else if t < t1 +. t2 then w, (t -. t1) *. h /. t2
      else if t < 2. *. t1 +. t2 then w -. (t -. t1 -. t2) *. w /. t1, h
      else 0., (1. -. t) *. h /. t2 in
    { x = x0 +. x; y = y0 +. y} in
  let normal t =
    let x, y = if t < t1 then (0., -1.)
      else if t < t1 +. t2 then (1., 0.)
      else if t < 2. *. t1 +. t2 then (0., 1.)
      else (-1., 0.) in
    { x; y} in 
  let distance (* p *) _ = 0. (* TODO *) in
  { boundary; normal; distance; size = { x = w; y = h}}

let ellipse center a b =
  let pp = 2. *. pi in
 { boundary = (fun t ->
        let t = pp *. t in
        let x = center.x +. a *. (cos t) in
        let y = center.y +. b *. (sin t) in
        {x; y} (* Warning t is not the arc length coordinate if a <> b *)
      );
    normal = (fun t ->
        let t = pp *. t in
        let x = (cos t) in
        let y = (a /. b) *. (sin t) in
        mult (1. /. (norm {x; y})) {x; y}
      );
    distance = (fun p ->
        sqrt ( p.x *. p.x /. (a *. a) +. p.y *. p.y /. (b *. b)) -. 1.);
    size = { x = 2. *. a; y = 2. *. b } 
  }
  
(* about 50ms here *)
let essai renderer =
  print_endline "ESSAI BORDER";
  let bw = 10. in
  let npoints = 1000 in
  (* let a, b = 50., 20. in *)
  (* let center = { x = a +. bw; y = b +. bw } in *)
  (* let shape = ellipse center a b in *)
  let shape = rectangle bw bw 100. 40. in
  let colorfn _ s = let c = round ((1. -. s) *. 255.) in c,0,0,c in
  (* let s = logical_to_physical (shape.size) in
   * let tex = Draw.create_target renderer (getx s) (gety s) in
   * let save = Draw.push_target renderer tex in *)
  draw_random renderer ~border_width:bw shape colorfn npoints
  (* Draw.pop_target renderer save;
   * tex *)
