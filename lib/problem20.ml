open Core

type pulse = High | Low

let pulse_compare pulse1 pulse2 = 
    match (pulse1, pulse2) with
    | (High, High) -> true
    | (High, Low) -> false
    | (Low, High) -> false
    | (Low, Low) -> true

type conjunction = {name: string; recent_pulses: (string, pulse) Core.Hashtbl.t; dest: string list}
type flipflop = {name: string; onoff: int; dest: string list}
type broadcaster = {name: string; recent_pulse: pulse; dest: string list}
type modt = 
    | Conjunction of conjunction
    | FlipFlop of flipflop
    | Broadcaster of broadcaster

(*type moduleconfig = {name: string; mtype: moduletype; recentpulse: pulse; dest: string list}*)

(* Input file always has 2 character modules. Test file doesn't, but can easily be modified *)
let parse_module dict list_chars = 
    let (name, mtype, list_chars) = (match list_chars with
    | 'b' :: 'r' :: 'o' :: 'a' :: 'd' :: 'c' :: 'a' :: 's' :: 't' :: 'e' :: 'r' :: ' ' :: '-' :: '>' :: ' ' :: t -> ("broadcaster", 0, t)
    | '%' :: h0 :: h1 :: ' ' :: '-' :: '>' :: ' ' :: t -> (String.of_char_list [h0;h1], 1, t)
    | '&' :: h0 :: h1 :: ' ' :: '-' :: '>' :: ' ' :: t -> (String.of_char_list [h0;h1], 2, t)
    ) in

    let dests = String.of_char_list list_chars |> String.split ~on:',' in
    let dests = List.map ~f:String.strip dests in
    let mod1 = (match mtype with
                | 0 -> Broadcaster {name=name; recent_pulse=Low; dest=dests}
                | 1 -> FlipFlop {name=name; onoff=0; dest=dests}
                | 2 -> 
                         let dict1 = Hashtbl.create (module String) in
                         Conjunction {name=name; recent_pulses=dict1; dest=dests}
               ) in
    Hashtbl.set dict ~key:name ~data:mod1;
    ()

let init_conj_pulse (dict: (string, modt) Core.Hashtbl.t) (key: string) =
    let mod1 = Hashtbl.find_exn dict key in
    let dests = (match mod1 with
                 | Broadcaster b -> b.dest
                 | Conjunction c -> c.dest
                 | FlipFlop f -> f.dest
                ) in
    let icp_aux name_dest = 
        let mod_dest = Hashtbl.find dict name_dest in
        match mod_dest with
        | Some Conjunction a -> Hashtbl.set a.recent_pulses ~key:key ~data:Low;
        | _ -> ()
    in
    List.map ~f:icp_aux dests;
    ()

let map_conjunctions dict =
    let keys = Hashtbl.keys dict in
    List.map ~f:(init_conj_pulse dict) keys;
    ()

let rec parse_input dict list_list_chars =
    match list_list_chars with
    | [] -> ()
    | h :: t ->
            parse_module dict h;
            parse_input dict t

(* Sends a pulse to a specific module and returns a pulse and a list of destination modules to which new pulses must be sent. Also returns the number of high and low pulses sent *)
let send_pulse dict pulse from name =
    let mod1 = Hashtbl.find dict name in
    match mod1 with
    | Some Conjunction c ->
            let _ = Hashtbl.set c.recent_pulses ~key:from ~data:pulse in
            let ar = Hashtbl.data c.recent_pulses in
            let allhigh = List.reduce_exn ~f:(&&) (List.map ~f:(pulse_compare High) ar) in
            let pulse_new = if allhigh then Low else High in
            (name, pulse_new, c.dest)
            
    | Some FlipFlop f ->
            let sendpulse = 
                if pulse_compare pulse High then
                    false 
                else 
                    (* Flip the onoff and store the new FlipFlop in the hash table *)
                    let f_new = {f with onoff=(1-f.onoff)} in
                    let _ = Hashtbl.set dict ~key:name ~data:(FlipFlop f_new) in
                    true
            in
            if sendpulse then
                (* If f is on then it flips off and sends Low. If it is off it flips on and sends High *)
                if f.onoff = 1 then
                    (name, Low, f.dest)
                else
                    (name, High, f.dest)
            else
                (name, High, [])


    | Some Broadcaster b ->
            let b_new = {b with recent_pulse=pulse} in
            let _ = Hashtbl.set dict ~key:name ~data:(Broadcaster b_new) in
            (name, pulse, b.dest)

    | None ->
            (name, High, [])

let rec queue_all_pulses queue new_pulses = 
    match new_pulses with
    | [] -> ()
    | h :: t -> 
            let _ = Queue.enqueue queue h in
            queue_all_pulses queue t

let rec send_all_pulses_and_count_rxlow npush dict queue = 
    match (Queue.dequeue queue) with
    | None -> ()
    | Some (from, pulse, dest) ->
            let _ = if (List.mem ~equal:String.equal dest "zh") && (pulse_compare pulse High) then
                let _ = Out_channel.output_string stdout (Int.to_string npush) in
                let _ = Out_channel.output_string stdout " " in
                let _ = Out_channel.output_string stdout from in
                let _ = Out_channel.output_string stdout "\n" in
                ()
            in
            let new_pulses = List.map ~f:(send_pulse dict pulse from) dest in
            let _ = queue_all_pulses queue new_pulses in
            send_all_pulses_and_count_rxlow npush dict queue

let rec send_all_pulses_and_count numhigh numlow dict queue =
    match (Queue.dequeue queue) with
    | None -> (numhigh, numlow)
    | Some (from, pulse, dest) -> 
            let (numhigh, numlow) = if pulse_compare pulse High then (numhigh + List.length dest, numlow) else (numhigh, numlow + List.length dest) in
            let new_pulses = List.map ~f:(send_pulse dict pulse from) dest in
            let _ = queue_all_pulses queue new_pulses in
            send_all_pulses_and_count numhigh numlow dict queue

let rec push_button_ntimes numhigh numlow ntimes dict = 
    let queue = Queue.create ~capacity:100 () in
    Queue.enqueue queue ("button", Low, ["broadcaster"]);
   
    if ntimes > 0 then
        let (numhigh, numlow) = send_all_pulses_and_count numhigh numlow dict queue in
        push_button_ntimes numhigh numlow (ntimes-1) dict
    else
        (numhigh, numlow)

let rec push_button_rx1 ntimes dict =
    let queue = Queue.create ~capacity:100 () in
    Queue.enqueue queue ("button", Low, ["broadcaster"]);

    send_all_pulses_and_count_rxlow ntimes dict queue;
    push_button_rx1 (ntimes+1) dict
    
let part1 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let dict = Hashtbl.create (module String) in
    parse_input dict list_list_chars;
    map_conjunctions dict;
    let (nhigh, nlow) = push_button_ntimes 0 0 1000 dict in
    nhigh*nlow

let part2 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let dict = Hashtbl.create (module String) in
    parse_input dict list_list_chars;
    map_conjunctions dict;
    let _ = push_button_rx1 1 dict in
    1

let proclem20_part2b =
    (* Just product of differences in these arrays *)
    let ns_high = [3766; 7533; 11300; 15067; 18834; 22601] in
    let dl_high = [3778; 7557; 11336; 15115; 18894; 22673] in
    let vd_high = [3880; 7761; 11642; 15523; 19404; 23285] in
    let bh_high = [7521; 11282; 15043; 18804; 22565; 26326] in
    1

