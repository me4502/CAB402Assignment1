module DitherModule

// determine the highest bit position of a standard integer
let maxBit = sizeof<int>*8-1

// check if the bit as position pos of number v is set
let isBitSet v pos = (v &&& (1 <<< pos)) <> 0

// reverse the bits in the representation of number v, up to the given bit position
let reverseBits v length =
    let setReverse_set r i = r ||| (if (isBitSet v i) then 1 <<< (length-i-1) else 0) 
    List.fold setReverse_set 0 [0..(length-1)]

// if bit i of number v is set then set the bit corresponding to that position and combine with accumulated result r 
let extractBit v r i = 
    r ||| (if (isBitSet v i) then 1 <<< (i/2) else 0)

// extracts the odd bits in the representation for number v
let oddBits v = 
    List.fold (extractBit v) 0 [1 .. 2 .. maxBit]

// extracts the even bits in the representation for number v
let evenBits v = 
    List.fold (extractBit v) 0 [0 .. 2 .. maxBit]

// returns the coordinates of all pixels in the image in special dither order
let coordinates N =
    let width = 1 <<< N
    let height = 1 <<< N
    seq { 
        for i in [0 .. width*height-1] do
              let r = reverseBits i (2*N)
              let x = oddBits r
              let y = x ^^^ evenBits r
              yield x, y
        }

