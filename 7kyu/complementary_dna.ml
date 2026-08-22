let make_complement_strand (dna : string) : string =
  let get_nucleotide_complement = function
    | 'A' -> 'T'
    | 'C' -> 'G'
    | 'G' -> 'C'
    | 'T' -> 'A'
    | chr -> failwith (Printf.sprintf "Unknown nucleotide %c." chr) in
  String.map get_nucleotide_complement dna
