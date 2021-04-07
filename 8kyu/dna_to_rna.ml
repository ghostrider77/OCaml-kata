
let dna_to_rna dna = String.map (fun c -> if c = 'T' then 'U' else c) dna
