(* A simple SDL mixer *)
(* (c) San Vu Ngoc, 2107 *)
(* for more features (like loading mp3), see SDL_mixer *)

open Bigarray
open B_utils (* can easily make this independent of Utils if needed *)
open Tsdl
module Time = B_time
  
type audio_spec = (*(int, Bigarray.int16_signed_elt)*) Sdl.audio_spec
                                                               
type sound = (int, Bigarray.int16_signed_elt) Sdl.bigarray

type repeat = Repeat of int | Forever

type effect = sound -> unit
                        
type track = {
    mutable soundpos : int;  (* this should only be modified by the callback *)
    soundlen : int;
    mutable repeat : repeat; (* note that (Repeat n) in fact means "play n times" *)
    sound : sound;
    mutable volume : float;  (* factor between 0 and 1. It is applied
                               dynamically. Can be changed on the fly. *)
    effects : effect list;
  }

type t = {
  mutable dev_id : Sdl.audio_device_id option; (* set when creating mixer *)
  devname : string option;
  mutable callback : Sdl.audio_callback option;
  mutable have: audio_spec; (* idem *)
  tracks : (track option) array (* this array is manipulated by the callback thread. Any other manipulation thus requires locking= TODO *)
}

(* Currently this always returns None. *)
let init  () =
  go (Sdl.(init_sub_system Init.audio));
  begin
    match Sdl.get_current_audio_driver () with
    | None -> printd debug_error "mixer.ml: cannot find audio driver."
    | Some s -> printd debug_io "Using audio driver: %s." s
  end;
  if go(Sdl.get_num_audio_drivers ()) < 1
  then failwith "Don't see any specific audio devices!";
  (*let devname = go(Sdl.get_audio_device_name 0 false) in
    printd debug_io "Using audio device #%d: ('%s')..." 0 devname; *)
  (*Some devname*) None (* this will select the default device; I don't know how
  to find out its name... *)

let print_spec spec =
  let open Sdl in
  printd debug_io "as_freq=%d, as_format=%d, as_channels=%d, as_silence=%d as_samples=%d as_size = %ld"
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
                         (* saturation should be clipped only for last sum,
                         because the idea is than when you sum many sounds, in
                         general values cancel each others and saturation is
                         less common than with only 2 sounds... *)
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
    
(* this is the main application *)
(* NOTE: this callback is executed in a different thread. Hence, when used in
   Bogue, there is no need to create an additional new thread: use
   Widget.connect_main, not widget.connect *)
let callback mixer =
  let no_sound = ref true in  (* inutilisé *)
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
            else blit_or_sum !first (last = i) track.volume chunk (Array1.sub output 0 cpy);
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
                    printd debug_io "%u repeat%s remaining for sample in track #%u."
                      (n-1) (if n>2 then "s" else "") i
                  end
                | Forever -> printd debug_io "Repeat sample forever in track #%u." i)
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
  let callback = Sdl.audio_callback int16_signed (callback mixer) in
  mixer.callback <- Some callback;
  let spec = { tmp_spec with Sdl.as_callback = Some callback } in
  let dev_id, spec' =
    go(Sdl.open_audio_device devname false spec 0 (* try also Sdl.Audio.allow_any_change*) ) in
  print_spec spec';
  (* (OLD) note that the new spec has no callback. Therefore we need to save the
     previous one, otherwise the callback will be reclaimed by Ocaml's GC,
     leading to Fatal error: exception Ctypes_ffi_stubs.CallToExpiredClosure.
     This still has to be corrected in tsdl. For the time being, I have made a
     hack in tsdl.ml. *)
  if spec'.Sdl.as_format <> format
  then printd (debug_io + debug_error) "Audio device doesn't support s16le format. Prepare to hear weird sounds.";
  mixer.dev_id <- Some dev_id;
  mixer.have <- spec';
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

let rec gcd a b =
  if a < b then gcd b a
  else let r = a mod b in
       if r = 0 then b
       else gcd b r;;

(* linear interpolation e1 points -> e2 points *)
(* data2 is filled with interpolated values *)
let interpolate e1 e2 data1 data2 =
  let ch = 2 in

  (* TODO optimize this (inline) *)
  (* let interpolate_sample i j x value1 value2 = (\* not used: inlined below *\)
   *   begin
   *     value2 := Array1.unsafe_get data1 (j + ch);
   *     let value = if x = 0. (\* ce test n'améliore  pas la vitesse... *\)
   *                 then !value1 
   *                 else round (float !value1 +. x *. (float (!value2 - !value1))) in
   *     Array1.unsafe_set data2 i value;
   *     value1 := !value2
   *   end in *)
  if Array1.dim data1 < ch
  then failwith "invalid format"
  else begin
      (* il n'y a qu'un petit (e2) nombre de "x", on les stocke (efficace ??) *)
      (* x.(i) = fractional part of e1 *. i/. e2 *)
      let x = Array.init e2 (fun i -> (float ((e1 * i) mod e2) /. float e2)) in
      let value1left = ref (Array1.unsafe_get data1 0) in
      let value2left = ref 0 in
      let value1right = ref (Array1.unsafe_get data1 1) in
      let value2right = ref 0 in
      let ii = ref 0 in
      for i = 0 to (Array1.dim data2)/ch - 1 do
        let pos = (e1 * i) / e2 in
        let rest = (*(float ((e1 * i) mod e2) /. float e2)*) Array.unsafe_get x !ii in
        (* ii = i mod e2 *)
        let j = (pos + 1) * ch and jj = i * ch in
        
        (* interpolate_sample jj j rest value1left value2left; *) (* left sample *)
        (* we inline this: (but this doesn't speed up) *)
        value2left := Array1.unsafe_get data1 j;
        let value = if rest = 0. (* ce test n'améliore  pas la vitesse... *)
                    then !value1left 
                    else round (float !value1left +. rest *. (float (!value2left - !value1left))) in
        Array1.unsafe_set data2 jj value;
        value1left := !value2left;

        (* interpolate_sample (jj + 1) (j + 1) rest value1right value2right; *) (* right sample *)
        (* inlined: *)
        value2right := Array1.unsafe_get data1 (j + 1);
        let value = if rest = 0. (* ce test n'améliore  pas la vitesse... *)
                    then !value1right 
                    else round (float !value1right +. rest *. (float (!value2right - !value1right))) in
        Array1.unsafe_set data2 (jj+1) value;
        value1right := !value2right;

        incr ii; if !ii = e2 then ii:=0;
      done
    end;;
        
(* stretch frequency (f1 => f2) for a s16le sound with 2 channels *)
let stretch f1 f2 sound =
  let t = Time.now() in
  let ch = 2 in
  let f1, f2 = if f1 < f2 then f1, f2 else f2, f1 in
  let d = gcd f1 f2 in
  let e1, e2 = f1/d, f2/d in
  (* e1 samples should be played in the same time as e2 samples *)
  (* we do a linear interpolation *)
  let l1 = Array1.dim sound / ch in
  let l2 = if l1 mod e1 = 0 then l1 * e2 / e1 else e2 * (l1 / e1 + 1) in
  (* we want l2 to be a multiple of e2 (not necessary ?) *)
  printd debug_io "Stretching %u => %u (%u => %u), size=%u" f1 f2 e1 e2 (ch * l2);
  let output = Array1.create int16_signed c_layout (ch * l2) in
  Array1.fill output 0;
  printd debug_io "Interpolate e1=%u e2=%u size1=%u size2=%u."
    e1 e2 (ch * (e1+1)) (ch * e2);
  interpolate e1 e2 (* main part *)
    (Array1.sub sound 0 ((l1/e1 - 2) * ch * e1 + ch))
    (Array1.sub output 0 ((l1/e1 - 2) * ch * e2));
  let finalpos = ch * (l1/e1 - 1) * e1 in
  interpolate e1 e2 (* final small part *)
    (Array1.sub sound finalpos (imin (ch * e1) (Array1.dim sound - finalpos)))
    (Array1.sub output (ch * (l1/e1 - 1) * e2) (Array1.dim sound - finalpos - ch));
  (* this goes up to bps * (e2*(l1/e1), which is the l2 size of output, in case
     l1 is a multiple of e1, or a bit less than l2 otherwise; we should compute
     more precisely what we need... *)

  (* TODO remove trailing zeroes *)
  printd debug_io "Sound was stretched in %u msec." (Time.now() -t);
  output;;
  
let load_chunk mixer filename =
  let open Sdl in
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
      printd (debug_io+debug_warning) "WAV chunk has different format (%u,%u) than the mixer (%u,%u). Will try to convert." audio_spec.as_format audio_spec.as_channels mixer.have.as_format mixer.have.as_channels;
      convert mixer audio_spec sound
    end
    else sound in
  let chunk =
    if audio_spec.as_freq <> mixer.have.as_freq
    then begin
      printd (debug_io+debug_warning) "WAV chunk has different freq (%u) than the mixer (%u). We try to interpolate." audio_spec.as_freq mixer.have.as_freq;
      stretch audio_spec.as_freq mixer.have.as_freq chunk
    end
    else chunk in
  chunk

(* play chunk on the desired track number. If track is not specified, find an
   available track. Return chosen track number, or None *)
(* Warning: this should be called by the main Thread (I think...) *)
let play_chunk ?track ?(effects=[]) ?(volume=1.) ?(repeat = Repeat 1) mixer sound =
  printd debug_io "Locking audio device...";
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
       printd (debug_io + debug_error)
         "No available track for playing chunk.";
       printd debug_io "Unlocking audio.";
       do_option mixer.dev_id Sdl.unlock_audio_device;
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
        do_option mixer.dev_id (fun d ->
            printd debug_io "Unlocking audio.";
            Sdl.unlock_audio_device d;
          (* printd debug_io "Unpause mixer"; *)
          (* REMARK: using Sld.pause_audio_device too often may lead to
          lockups... Don't know why. Hence we remove it from here. The user has
          to invoke Mixer.unpause manually. Idem with lock ??? *)
          (* Sdl.pause_audio_device d false *)
          );
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
      Sdl.lock_audio_device id;
      Sdl.close_audio_device id);
  mixer.dev_id <- None;
  mixer.callback <- None;
  for i = 0 to Array.length mixer.tracks - 1 do
    mixer.tracks.(i) <- None
  done;;
  
(* let free_chunk = Sdl.free_wav *)
(* DONT USE THIS, the chunks will be free (hopefully) by Ocaml's GC. *)
                   
let test () =
  print_endline "MIXER TEST";
  Gc.compact ();
  let devname = init () in
  let mixer = create_mixer devname in
  let chunk2 = load_chunk mixer "../tests/audio/sia.wav" in

  let chunk3 = load_chunk mixer "../tests/audio/sample.wav" in
  (* 22050 Hz, converted to 2 channels 1ms, stretched in 35-47ms *)

  let chunk1 = load_chunk mixer "../tests/audio/chunk.wav" in
  (* 16000 Hz, stretched in < 6ms *)

  Gc.compact (); (* this makes it crash with original tsdl 0.9.1 *)
  unpause mixer;
  let sia = play_chunk ~volume:0.8 mixer chunk2 in
  Sdl.delay 1000l;
  let _ = play_chunk ~repeat:(Repeat 5) mixer chunk1 in
  Sdl.delay 5000l;
  do_option sia (stop_track mixer);
  let _ = play_chunk ~volume:0.5 mixer chunk3 in
  Sdl.delay 1000l;
  pause mixer;
  Sdl.delay 1000l;
  unpause mixer;
  Sdl.delay 3000l;

  close mixer;

  (* DON'T FREE, it causes double free segfault next time you run 
     test () and Gc.compact.

   * free_chunk chunk1;
   * free_chunk chunk2;
   * free_chunk chunk3; *)

  Sdl.quit()
    
