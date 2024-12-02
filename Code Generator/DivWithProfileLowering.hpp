// Uses value profile info
// t1 = div a, b
//? Value profile for b: 4 - 90%, 56 - 7%, 234 - 3%
//! if(b == 4) t1 = shr a, 2
//! else t1 = div a, b