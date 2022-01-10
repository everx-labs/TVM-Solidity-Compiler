TVM Solidity Compiler allows user to calculate hash of the stateInit using data and code cells. 
According to section 1.1.3 and 1.1.4 of the [TBLKCH][2] representation hash of the stateInit Cell *c*
is calculated with this formula:

```Math
  Hash(c) = SHA256(CellRepr(c))
```

where `CellRepr` is  “standard representation” of the Cell and consists of the following data:

1) Two descriptor bytes come first, sometimes denoted by *d1* and *d2*. The
   first of these bytes *d1* equals the number of references 0 ≤ *r* ≤ 4 in the cell.
   The second descriptor byte *d2* encodes the
   bit length *l* of the data part of the cell as follows: the first seven bits of
   *d2* equal `floor(l/8)`, the number of complete data bytes present in the cell,
   while the last bit of *d2* is the completion tag, equal to one if l is not
   divisible by eight. Therefore,

```
   d2 = 2 * floor(l/8) + [l mod 8 /= 0] = floor(l/8) + ceil(l/8)
```

StateInit structure:

```TLB
  split_depth:(Maybe (## 5)) special:(Maybe TickTock)
  code:(Maybe ^Cell) data:(Maybe ^Cell)
  library:(Maybe ^Cell) = StateInit;
```

StateInit with only data and code contain such binary data:
data bits: b00110
refs: 0 - code, 1 - data
*l* = 5
*r* = 2

For such stateInit descriptor bytes are equal:
*d1* = 2
*d2* = 1

2) Next, `ceil(l/8)` data bytes follow. This means that the *l* data bits of the
   cell are split into groups of eight, and each group is interpreted as a
   big-endian 8-bit integer and stored into a byte. If *l* is not divisible by
   eight, a single binary one and a suitable number of binary zeroes (up
   to six) are appended to the data bits, and the completion tag (the least
   significant bit of the descriptor byte *d2*) is set.

3) Finally, *r* references to other cells follow. Each reference is normally
   represented by 32 bytes containing the representation hash of the referenced
   cell and 2 bytes of cell depth.

Total input data for SHA256 is equal to:

```
  b00000010 + b00000001 + b00110 + b100 + depth(code_cell) + depth(data_cell) + hash(code_cell) + hash(data_cell)
      d1          d2       data     tag   depth of r0 cell   depth of r1 cell   hash of r0 cell   hash of r1 cell
```
