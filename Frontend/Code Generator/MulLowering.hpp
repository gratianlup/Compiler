// Not related, but should be done
//! (a<0) == (b<0)
//? not %edi (a = ~a)
//? xor %edi,%esi (b = a ^ b)
//? shr $31,%esi
//? mov %esi,%eax