module internal MultiSet
    type MultiSet<'a when 'a : comparison> = MS of Map: Map<'a, uint32> * Size: uint32

    let empty : MultiSet<'a> = MS (Map.empty, 0u)
    let isEmpty (s: MultiSet<'a>) : bool = 
        match s with MS(_, size) -> (size=0u) 

    let size (s:MultiSet<'a>): uint32 = 
        match s with MS(_, size) -> size

    let numItems (x: 'a) (s: MultiSet<'a>) : uint32 =
        match s with 
        | MS(mp, size) -> if (mp.ContainsKey x) then mp.[x] else 0u

    let contains (x: 'a) (s: MultiSet<'a>) : bool = 
        (numItems x s > 0u)

    let add (k: 'a) (v: uint32) (s: MultiSet<'a>) : MultiSet<'a> =
        let count = numItems k s
        match s with 
        | MS(mp, size) -> MS(Map.add k (count+v) mp, size+v) 

    let addSingle k s = add k 1u s    
    let remove (k: 'a) (v: uint32) (s: MultiSet<'a>) : MultiSet<'a> =
        let count = numItems k s
        match s with
        | MS(mp, size) -> 
            match count with
            | c when c>v  -> MS(Map.add k (c-v) mp, size-v)
            | c when c<=v -> MS(Map.remove k mp, size-count)  
            | _           -> s
            
    type folder<'a, 'k, 'v> = ('a -> 'k -> 'v -> 'a)

    let removeSingle (k: 'a) (s:MultiSet<'a>) : MultiSet<'a> = remove k 1u s

    let fold (f: folder<'a, 'k, uint32>) (acc:'a) (s:MultiSet<'k>) : 'a =  
        match s with 
        | MS(mp, _) -> Map.fold f acc mp

    type backfolder<'k, 'v, 'a> = ('k -> 'v -> 'a -> 'a) 

    let foldBack (f: backfolder<'k, uint32, 'a>) (s:MultiSet<'k>) (acc:'a) : 'a =  
        match s with 
        | MS(_, 0u)  -> acc
        | MS(mp, _) -> Map.foldBack f mp acc