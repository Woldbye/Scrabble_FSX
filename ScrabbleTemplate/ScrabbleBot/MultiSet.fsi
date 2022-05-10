module internal MultiSet

    type MultiSet<'a when 'a: comparison> = MS of Map: Map<'a,uint32> * Size: uint32

    val empty: MultiSet<'a> when 'a: comparison

    val isEmpty: s: MultiSet<'a> -> bool when 'a: comparison

    val size: s: MultiSet<'a> -> uint32 when 'a: comparison

    val numItems: x: 'a -> s: MultiSet<'a> -> uint32 when 'a: comparison

    val contains: x: 'a -> s: MultiSet<'a> -> bool when 'a: comparison

    val add: k: 'a -> v: uint32 -> s: MultiSet<'a> -> MultiSet<'a> when 'a: comparison

    val addSingle: k: 'a -> s: MultiSet<'a> -> MultiSet<'a> when 'a: comparison

    val remove: k: 'a -> v: uint32 -> s: MultiSet<'a> -> MultiSet<'a> when 'a: comparison

    type folder<'a,'k,'v> = 'a -> 'k -> 'v -> 'a

    val removeSingle: k: 'a -> s: MultiSet<'a> -> MultiSet<'a> when 'a: comparison

    val fold: f: folder<'a,'k,uint32> -> acc: 'a -> s: MultiSet<'k> -> 'a when 'k: comparison

    type backfolder<'k,'v,'a> = 'k -> 'v -> 'a -> 'a

    val foldBack: f: backfolder<'k,uint32,'a> -> s: MultiSet<'k> -> acc: 'a -> 'a when 'k: comparison

    val toList: MultiSet<'k> -> ('k * uint32) list