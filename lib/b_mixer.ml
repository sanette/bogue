(* A simple SDL mixer *)
(* (c) San Vu Ngoc, 2017-2023 *)
(* for more features (like loading mp3), see SDL_mixer
   https://github.com/sanette/tsdl-mixer *)

open Bigarray
open B_utils (* can easily make this independent of Utils if needed *)
open Tsdl
module Time = B_time
module Theme = B_theme

type audio_spec = (*(int, Bigarray.int16_signed_elt)*) Sdl.audio_spec

type sound = (int, Bigarray.int16_signed_elt) Sdl.bigarray

type repeat = Repeat of int | Forever

type sound_effect = sound -> unit

type track = {
  mutable soundpos : int;  (* this should only be modified by the callback *)
  soundlen : int;
  mutable repeat : repeat; (* note that (Repeat n) in fact means "play n times" *)
  sound : sound;
  mutable volume : float;  (* factor between 0 and 1. It is applied
                              dynamically. Can be changed on the fly. *)
  effects : sound_effect list;
}

type t = {
  mutable dev_id : Sdl.audio_device_id option; (* set when creating mixer *)
  devname : string option; (* None if no audio is available *)
  mutable callback : Sdl.audio_callback option;
  mutable have: audio_spec; (* idem *)
  tracks : (track option) array (* this array is manipulated by the callback thread. Any other manipulation thus requires locking = TODO *)
}

(* Currently this only returns either None or Some "default". *)
let init  () =
  match Sdl.(init_sub_system Init.audio) with
  | Error (`Msg e) ->
    printd (debug_error + debug_io) "Cannot initialize audio. SDL error! %s" e;
    None
  | Ok () ->
    begin
      match Sdl.get_current_audio_driver () with
      | None -> printd (debug_error + debug_io)
                  "mixer.ml: cannot find audio driver."
      | Some s -> printd debug_io "Using audio driver: %s." s
    end;
    match Sdl.get_audio_device_name 0 false with
    | Error (`Msg e) ->
      printd (debug_error + debug_io) "Cannot get Audio device name. SDL error! %s" e;
      None
    | Ok devname ->
      printd debug_io "Using audio device: %s." devname;
      Some devname

let print_spec spec =
  let open Sdl in
  printd debug_io "as_freq=%d, as_format=%d, as_channels=%d, as_silence=%d \
                   as_samples=%d as_size = %ld"
    spec.as_freq
    spec.as_format
    spec.as_channels
    spec.as_silence
    spec.as_samples
    spec.as_size

let bytes_to_value_s16le b1 b2 =
  let value = b1 lor (b2 lsl 8) in
  if value land 32768 <> 0 (* negative sign *)
  then -1 - value lxor 65535
  else value

let value_to_bytes_s16le value =
  let value = if value < 0
    then (-value) lxor 65535 + 1
    else value in
  value land 255, value lsr 8

(* clipping is performed only if last=true *)
(* if first=true, we don't need to sum, we just apply the volume *)
let blit_or_sum first last volume chunk output =
  if first && abs_float (1. -. volume) < 0.0001
  then Array1.blit chunk output
  else if Array1.dim chunk <> Array1.dim output
  then failwith "chunk and output arrays must have same dim."
  else
    let clipping = ref 0 in
    let d = Array1.dim chunk in
    if first
    then for i = 0 to d-1 do
        let value = round (volume *. (float (Array1.unsafe_get chunk i))) in
        Array1.unsafe_set output i value
      done
    else begin
      for i = 0 to d-1 do
        let value1 = Array1.unsafe_get chunk i in
        let value2 = Array1.unsafe_get output i in
        let value' = (round (volume *. (float value1)) + value2) in
        let value' = if last then
            (* saturation should be clipped only for last sum, because the idea
               is than when you sum many sounds, in general values cancel each
               others and saturation is less common than with only 2
               sounds... *)
            if value' > 32767 (* signed 16 bits *)
            then (incr clipping; 32767)
            else if value' < -32768
            then (incr clipping; -32768)
            else value'
          else value' in
        Array1.unsafe_set output i value';
      done;
      if !clipping > 0
      then printd (debug_io + debug_warning) "Sound had to be clipped %u times for saturation" !clipping
    end

(* This is the main application *)
(* NOTE: this callback is executed in a different thread. Hence, when used in
   Bogue, there is no need to create an additional new thread: use
   Widget.connect_main, not widget.connect *)
let callback mixer =
  let no_sound = ref true in
  fun output ->
    let chunk_length =  Array1.dim output in
    let first = ref true in

    (* last non-empty track (or 0) *)
    let last = let rec loop i = if i=0 || mixer.tracks.(i) <> None then i
                 else loop (i-1) in
      loop (Array.length mixer.tracks - 1) in
    let filled = ref 0 in
    for i = 0 to Array.length mixer.tracks - 1 do
      do_option mixer.tracks.(i) (fun track ->
          if track.soundpos = track.soundlen
          then printd (debug_io + debug_error)
              "Trying to play a finished sound. This should not happen"
          else begin
            printd debug_io "Track #%d playing %u/%u."
              i track.soundpos track.soundlen;
            let waveleft = track.soundlen - track.soundpos in
            let cpy = min chunk_length waveleft in
            let chunk = Array1.sub track.sound track.soundpos cpy in
            List.iter (fun f -> f chunk) track.effects;
            if cpy = chunk_length
            then blit_or_sum !first (last = i) track.volume chunk output
            else blit_or_sum !first (last = i) track.volume chunk
                (Array1.sub output 0 cpy);
            track.soundpos <- track.soundpos + cpy;
            filled := imax !filled cpy;
            first := false;
            if track.soundpos = track.soundlen (* sound is finished *)
            then (match track.repeat with
                | Repeat n ->
                  track.repeat <- Repeat (n-1);
                  if n <= 1 then begin
                    printd debug_io "Track #%u is available for playing." i;
                    mixer.tracks.(i) <- None
                    (* this makes the track available again *)
                  end
                  else begin
                    track.soundpos <- 0;
                    printd debug_io
                      "%u repeat%s remaining for sample in track #%u."
                      (n-1) (if n>2 then "s" else "") i
                  end
                | Forever ->
                  printd debug_io "Repeat sample forever in track #%u." i)
          end)
    done;
    if !filled <> chunk_length
    then Array1.fill
        (Array1.sub output !filled (chunk_length - !filled))
        0 (*mixer.have.Sdl.as_silence*);
    if !first (* no sound played *)
    then begin
      if not !no_sound
      then printd debug_io "No sound to play. Disabling further messages.";
      no_sound := true;
      (* do_option mixer.dev_id (fun d -> *)
      (* Sdl.pause_audio_device d true) *)
    end
    else no_sound := false

(* change volume in-place *)
(* warning, volume is here just a linear factor (from 0 to 1 in principle) *)
(* the energy is proportional to the square of the intensity. Thus, sqrt(volume)
   is a more meaningful quantity. To reduce volume by half, use 0.25. *)
let change_volume factor sound =
  blit_or_sum true true factor sound sound

(* create the mixer an open sound device. Only format s16le is supported by the
   callback at this time. *)
(* SDL DOC: This structure is used by SDL_OpenAudioDevice() and
   SDL_LoadWAV(). While all fields are used by SDL_OpenAudioDevice(), only freq,
   format, channels, and samples are used by SDL_LoadWAV().  *)
let create_mixer ?(tracks=8) ?(freq=44100) devname =
  let format = Sdl.Audio.s16_lsb in
  let tmp_spec = {
    Sdl.as_freq = freq;
    as_format   = format;
    as_channels = 2;
    as_silence  = 0;
    as_callback = None;
    as_samples  = 4096;
    as_size     = 8192l;
    (*as_ba_kind  = int16_signed;*)
  } in
  let mixer = {
    dev_id = None;
    have = tmp_spec;
    devname;
    callback = None;
    tracks = Array.make tracks None
  } in
  match devname with
  | None ->
    printd (debug_io + debug_warning)
      "Creating a dummy mixer: no audio available.";
    mixer
  | Some devname ->
    let devname = if devname = "default" then None else Some devname in
    let callback = Sdl.audio_callback int16_signed (callback mixer) in
    mixer.callback <- Some callback;
    let spec = { tmp_spec with Sdl.as_callback = Some callback } in
    let () = match Sdl.open_audio_device devname false spec 0 with
      (* try also Sdl.Audio.allow_any_change *)
      | Error (`Msg e) ->
        printd (debug_io + debug_error)
          "Cannot open audio device. SDL error: %s" e
      | Ok (dev_id, spec') ->
        print_spec spec';
        if spec'.Sdl.as_format <> format
        then printd (debug_io + debug_error)
            "Audio device doesn't support s16le format. Prepare to hear weird \
             sounds.";
        mixer.dev_id <- Some dev_id;
        mixer.have <- spec';
    in
    mixer

(* TODO verify it works for signed too *)
let convert_from_32le _ b2 =
  b2

let convert_from_32be b1 _ =
  b1

let convert_from_s16be b =
  (b lsr 8) lor ((b land 255) lsl 8)

let convert mixer spec sound =
  let t = Time.now() in
  let target_format = mixer.have.Sdl.as_format in
  if target_format <> Sdl.Audio.s16_lsb
  then
    (printd debug_error "Convert sound to format %u not implemented." target_format;
     sound)
  else
    let ba_bytes_size = 2 in (* 16 bits Bigarray *)
    let target_bps = 2 in (* s16le encoding *)
    let sound_format = spec.Sdl.as_format in
    let target_channels = mixer.have.Sdl.as_channels in
    let sound_channels = spec.Sdl.as_channels in
    if sound_format = target_format && target_channels = sound_channels then sound
    else begin
      if target_channels <> 2
      then printd (debug_error + debug_io) "Only 2 audio channels are implemented.";
      let bitsize = sound_format land 255 in
      let sound_bps = bitsize lsr 3 in (* / 8 *)
      (* see: https://wiki.libsdl.org/SDL_AudioFormat *)
      if sound_bps = 0 then failwith "invalid sound format";
      let soundlen = Array1.dim sound in
      let targetlen = (target_bps / ba_bytes_size) * target_channels * soundlen / (sound_bps / ba_bytes_size) / sound_channels in
      printd debug_io "Converting sound with length %u ==> %u."
        soundlen targetlen;
      let target = Array1.create int16_signed c_layout targetlen in
      let () =
        if sound_bps = 1
        then (* TODO check this is indeed u8 *)
          let () = printd debug_io "Converting (u8,%u) to (s16le,%u)."
              sound_channels target_channels in
          for j = 0 to soundlen - 1 do
            (* j is pos in the original sound *)
            (* i is the index of the target array *)
            let i = 2 * target_channels * j / sound_channels in
            let v = Array1.unsafe_get sound j in
            Array1.unsafe_set target i (v land 255);
            Array1.unsafe_set target (i+1) (v lsr 8);
          done
        else if sound_bps = 2
        then if sound_format = Sdl.Audio.s16_msb ||
                (sound_format = Sdl.Audio.s16_sys && Sys.big_endian)
          then
            let () = printd debug_io "Converting (s16be,%u) to (s16le,%u)."
                sound_channels target_channels in
            for i = 0 to targetlen - 1 do
              let j = sound_channels * i / target_channels in
              (* j = index of the original array *)
              Array1.unsafe_set target i (convert_from_s16be
                                            (Array1.unsafe_get sound j))
            done
          else
            let () = printd debug_io "Converting (s16le,%u) to (s16le,%u)."
                sound_channels target_channels in
            (* in fact here sound_channels=1 and target_channels=2... *)
            for j = 0 to soundlen - 1 do
              let x = Array1.unsafe_get sound j in
              Array1.unsafe_set target (2*j) x;
              Array1.unsafe_set target (2*j+1) x
            done
        else if sound_bps = 4
        then let convert_sample = if sound_format land (1 lsl 12) <> 0
               then convert_from_32be (* TODO check *)
               else convert_from_32le in (* TODO check *)
          for i = 0 to targetlen - 1 do
            let j = 2 * sound_channels * i / target_channels in
            let v = convert_sample
                (Array1.unsafe_get sound j)
                (Array1.unsafe_get sound (j+1)) in
            Array1.unsafe_set target i v;
          done
        else printd (debug_io + debug_error)
            "Conversion from format %u not supported." sound_format in
      printd debug_io "Sound converted in %u msec." (Time.now() - t);
      target
    end

(* Linear interpolation n1=k*e1+1 points -> n2=k*e2+1 points *)
(*************************************************************)
(* Initial data is in data1 which has size 2*n1 (2 channels), data2 has size
   2*n2 and is filled with interpolated values.

   EXACT CASE : (n1-1)*e2 = (n2-1)*e1

   Then we multiply frequncy by (e2/e1) and both data1 and data2 play exactly in
   the same time length.

   Warning1 : the first and last elements of data2 coincide with those of
   data1. In case of streaming (concatenating sounds), be careful to remove the
   last one and restart there with the next chunk. (Physically: the last sample
   "doesn't have the time to sound". In our algo, having n samples from i=0 to
   i=n-1 means that the sound stops immediatly at the last sample, so there are
   only (n-1) periods, which means only (n-1) physically relevant values; the
   last one is just used to complute the correct interpolation.)

   Warning2 : for instance if you wish to double the frequency and stop exactly
   at the same time as data1, then n2 should be 2*n1-1 (not 2*n1). This
   corresponds to e1=1, e2=2, k=n1-1.

   We denote by i1 and i2 the virtual indices for data1 and data2 (in reality,
   since there are 2 channels, the corresponding indices for data1 are (2*i1,
   2*i1+1), and similarly for data2.) For means of discussion, we denote by f1
   the signal of data1, ie

   f1.(i1) = (data1.(2*i1), data1.(2*i1+1))

   We do linear interpolation, in the sense that the "true sound" f is assumed to
   be obtained by linearly joining the values of data1:

   f(t) = f1.(i1), where t = i1*h1, h1 is the period between two samples
   (frequency F1 = 1/h1, total time T = h1*(n1-1)), and for a general t,

   f(t) = f1.(i1) + x*(f1.(i1+1) - f1.(i1)), where i1 is the last sample index
   <= t (so i1 = floor (t/h1)), and x is the fractional part of t/h1.

   Hence, if we resample with n2 points, the period is h2 = h1*(n1-1)/(n2-1),
   and

   f2.(i2) = f(i2*h2) = f(i2*h1*(n1-1)/(n2-1)) = f(h1*i2*e1/e2).

   Now we notice that there are at most e2 different fractional parts of
   i2*e1/e2 (because if you add e2 to i2 the difference is an integer e1.) So a
   natural optimization is to loop over these e2 values, and for each of them
   fill data2 at the corresponding interpolated times.

   NON EXACT CASE:

   The algo still works. In general, f2.(i2) will be the sampling of
   f1.("i2*e1/e2"), (for which we need indices i2*e1/e2 and i2*e1/e2+1). This
   will work for any dimension n1. This means i2 stays in the range [>=0
   ... <(n1-1)*e2/e1]. (And we keep an extra room for the exact case where we
   add the final point.)

*)

(* Size of (one channel of) resampled sound (without the last sample) *)
let resampled_size e1 e2 n1 =
  let mf = float ((n1-1)*e2) /. float e1 in
  if mf = floor mf then int_of_float mf else int_of_float mf + 1

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)

let interpolate ?(last_point = true) e1 e2 data1 data2 =
  let ch = 2 in (* number of channels (2 = left + right) *)
  if (Array1.dim data1 - 2) * e2 > (Array1.dim data2 - 2) * e1
  then failwith (Printf.sprintf
                   "Invalid array sizes; data2 should have at least %f elements."
                   (2. +. (float ((Array1.dim data1 - 2) * e2) /. float e1 )));
  if Array1.dim data1 < ch || Array1.dim data1 mod ch <> 0
  then failwith "invalid format";
  let n1 = (Array1.dim data1)/ch in
  let rec loop_frac m i =
    if i < e2 then
      (* frac = fractional part of e1 *. i/. e2 *)
      let frac = float ((e1 * i) mod e2) /. float e2 in
      let rec loop i1 i2 =
        if i1 < n1 - 1 then begin
          (* print "i1=%i  i2=%i" i1 i2; *)
          (* left sample *)
          let j1 = i1*ch in
          let j2 = i2*ch in
          let yA = Array1.unsafe_get data1 j1 in
          let yB = Array1.unsafe_get data1 (j1 + ch) in
          let value = if frac = 0. (* ce test n'améliore pas la vitesse... *)
            then yA
            else round (float yA +. frac *. (float (yB - yA))) in
          (* print "LEFT A=%i, B=%i, value=%i" yA yB value; *)
          Array1.unsafe_set data2 j2 value;

          (* right sample *)
          let yA = Array1.unsafe_get data1 (j1 + 1) in
          let yB = Array1.unsafe_get data1 (j1 + ch + 1) in
          let value = if frac = 0. (* ce test n'améliore pas la vitesse... *)
            then yA
            else round (float yA +. frac *. (float (yB - yA))) in
          (* print "RIGHT A=%i, B=%i, value=%i" yA yB value; *)
          Array1.unsafe_set data2 (j2 + 1) value;
          loop (i1 + e1) (i2 + e2)
        end
        else i2 - e2 in
      let m2 = loop (i*e1/e2) i in
      loop_frac (imax m m2) (i+1)
    else m
  in
  let m = loop_frac 0 0 in
  (* Last point: *)
  if last_point && ((n1-1) * e2) mod e1 = 0
  (* if (Array1.dim data1 - 2) * e2 = (Array1.dim data2 - 2) * e1 *) then begin
    printd debug_io
      "In [interpolate]: Final sample point is added to the output signal.";
    let n2 = 1+((n1-1) * e2) / e1 in
    Array1.unsafe_set data2 (2*(n2-1)) (Array1.unsafe_get data1 (Array1.dim data1 - 2));
    Array1.unsafe_set data2 (2*(n2-1)+1) (Array1.unsafe_get data1 (Array1.dim data1 - 1))
  end;
  let mt = resampled_size e1 e2 n1 - 1 in
  (* print "max i2 index=%i, theory %i." m mt; *)
  if !debug then assert (m = mt)

let to_array data =
  Array.init (Array1.dim data) (Array1.unsafe_get data)

let test_interpolate () =
  let data1 = Array1.of_array Int16_signed c_layout [|0;1; 2;3; 4;5|] in
  let data2 = Array1.create Int16_signed c_layout 12 in
  (* 3 values --> 6 values *)
  let () = interpolate 2 5 data1 data2 in
  assert (to_array data2 = [|0;1; 1;2; 2;3; 2;3; 3;4; 4;5|]);
  (* in other words valus for left channel are 0 1 2 2 3 4 *)
  let data1 = Array1.of_array Int16_signed c_layout [|0;0; 1;1; 2;2|] in
  let () = interpolate 2 5 data1 data2 in
  assert (to_array data2 = [|0;0; 0;0; 1;1; 1;1; 2;2; 2;2|])

(* Resample frequency (f1 => f2) for a s16le sound with 2 channels.  [sound] was
   initially mixed at frequency f1; we want to produce a sound at frequency f2
   which "sounds the same" (in particular has same playing length). We take
   advantage of the fact that quite often frequency ratios f1/f2 are simple
   rationals. *)
let resample f1 f2 sound =
  let t = Sys.time () in
  let ch = 2 in
  let n1 = Array1.dim sound / ch in
  let d = gcd f1 f2 in
  let e1, e2 = f1/d, f2/d in
  (* e1 samples should be played in the same time (1/d sec) as e2 samples. *)
  (* However, we have (n1-1) samples (we consider that the last one is not
     played), and there is no reason why (n1-1) should be a multiple of
     e1. Hence we do a non-exact linear interpolation (the last sample of
     [sound] may not be at the exact same time as the last sample of the
     resampled sound.) *)
  let n2 = resampled_size e1 e2 n1 + 1 in
  printd debug_io "Resampling %u Hz => %u Hz (%u => %u), size %u => %u"
    f1 f2 e1 e2 (ch * n1) (ch * n2);
  let output = Array1.create int16_signed c_layout (ch * n2) in
  interpolate e1 e2 sound output;
  printd debug_io "Sound was resampled in %f sec." (Sys.time () -. t);
  output

let load_chunk mixer filename =
  let open Sdl in
  let filename = Theme.get_path filename in
  let file = go (rw_from_file filename "rb") in
  let audio_spec, sound =
    let rec loop i =
      if i = 0
      then (printd (debug_io + debug_error) "Cannot load WAV file.";
            let data = Array1.create int16_signed c_layout 1024 in
            Array1.fill data 0;
            mixer.have, data)
      else
        let spec, data = go (load_wav_rw file mixer.have int16_signed) in
        if spec.as_channels = 0
        then begin
          printd (debug_io + debug_error) "WAV file corrupt. We try again.";
          Time.delay 100;
          loop (i-1)
        end else spec,data in
    loop 3 in
  go (rw_close file);
  printd debug_io "Loading WAV file %s." filename;
  print_spec audio_spec;
  let chunk =
    if audio_spec.as_format <> mixer.have.as_format
    || audio_spec.as_channels <> mixer.have.as_channels
    then begin
      printd (debug_io + debug_warning)
        "WAV chunk has different format (%u,%u) than the mixer (%u,%u). Will try \
         to convert." audio_spec.as_format audio_spec.as_channels
        mixer.have.as_format mixer.have.as_channels;
      convert mixer audio_spec sound
    end
    else sound in
  let chunk =
    if audio_spec.as_freq <> mixer.have.as_freq
    then begin
      printd (debug_io + debug_warning)
        "WAV chunk has different freq (%u) than the mixer (%u). We try to \
         interpolate." audio_spec.as_freq mixer.have.as_freq;
      resample audio_spec.as_freq mixer.have.as_freq chunk
    end
    else chunk in
  chunk

(* play chunk on the desired track number. If track is not specified, find an
   available track. Return chosen track number, or None *)
(* Warning: this should be called by the main Thread (I think...) *)
let play_chunk ?track ?(effects=[]) ?(volume=1.) ?(repeat = Repeat 1)
    mixer sound =
  (* printd debug_io "Locking audio device..."; *)
  (* do_option mixer.dev_id Sdl.lock_audio_device; *) (* this may freeze... *)
  (* find available track: *)
  let tracks = Array.length mixer.tracks in
  let track = match track with
    | None -> let rec loop i =
                if i = tracks then None
                else if mixer.tracks.(i) = None then Some i
                else loop (i+1) in
      loop 0
    | Some i -> if 0 <= i && i < tracks && mixer.tracks.(i) = None
      then Some i else None
  in
  let () = match track with
    | None ->
      printd (debug_io + debug_error) "No available track for playing chunk.";
      (* printd debug_io "Unlocking audio."; *)
      (* do_option mixer.dev_id Sdl.unlock_audio_device; *)
    | Some i -> begin
        let soundlen = Array1.dim sound in
        printd debug_io "Playing sound of length %u on track #%u." soundlen i;
        let track = {
          soundpos = 0;
          soundlen;
          repeat;
          sound;
          volume;
          effects
        } in
        mixer.tracks.(i) <- Some track;
        (* do_option mixer.dev_id (fun d -> *)
        (*     (\* printd debug_io "Unlocking audio."; *\) *)
        (*     (\* Sdl.unlock_audio_device d; *\) *)
        (*     (\* printd debug_io "Unpause mixer"; *\) *)
        (*     (\* REMARK: using Sld.pause_audio_device too often may lead to *)
        (*        lockups... Don't know why. Hence we remove it from here. The user *)
        (*        has to invoke Mixer.unpause manually. Idem with lock ??? *\) *)
        (*     (\* Sdl.pause_audio_device d false *\) *)
        (*   ); *)
      end in
  track

let stop_track mixer i =
  mixer.tracks.(i) <- None

let pause mixer =
  do_option mixer.dev_id (fun d ->
      printd debug_io "Pause mixer";
      Sdl.pause_audio_device d true)

let unpause mixer =
  do_option mixer.dev_id (fun d ->
      printd debug_io "Unpause mixer";
      Sdl.pause_audio_device d false)

let close mixer =
  (*pause mixer;*) (* useful ? *)
  printd debug_io "Closing mixer";
  do_option mixer.dev_id (fun id ->
      Time.delay 50;
      Sdl.close_audio_device id);
  mixer.dev_id <- None;
  mixer.callback <- None;
  for i = 0 to Array.length mixer.tracks - 1 do
    mixer.tracks.(i) <- None
  done

(* let free_chunk = Sdl.free_wav *)
(* DONT USE THIS, the chunks will be freed (hopefully) by Ocaml's GC. *)

let test () =
  print_endline "MIXER TEST";
  Gc.compact ();
  let devname = init () in
  let mixer = create_mixer devname in
  let chunk2 = load_chunk mixer "%assets/audio/sia.wav" in

  let chunk3 = load_chunk mixer "%assets/audio/sample.wav" in
  (* 22050 Hz, converted to 2 channels 1ms, stretched in 35-47ms *)

  let chunk1 = load_chunk mixer "%assets/audio/chunk.wav" in
  (* 16000 Hz, stretched in < 6ms *)

  Gc.compact (); (* this makes it crash with tsdl 0.9.1 *)
  unpause mixer;
  print_endline "Playing music...";
  let sia = play_chunk ~volume:0.8 mixer chunk2 in
  Sdl.delay 1000l;
  print_endline "Adding repeated sound on top...";
  let _ = play_chunk ~repeat:(Repeat 5) mixer chunk1 in
  Sdl.delay 5000l;
  print_endline "Switching to another tune...";
  do_option sia (stop_track mixer);
  let _ = play_chunk ~volume:0.5 mixer chunk3 in
  Sdl.delay 1000l;
  print_endline "Pausing...";
  pause mixer;
  Sdl.delay 1000l;
  print_endline "Resuming...";
  unpause mixer;
  Sdl.delay 3000l;

  close mixer;
  print_endline "Done!";

  (* DON'T FREE, it causes double free segfault next time you run
     test () and Gc.compact.

   * free_chunk chunk1;
   * free_chunk chunk2;
   * free_chunk chunk3; *)

  Sdl.quit()
