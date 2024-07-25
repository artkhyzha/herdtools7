Bad enumeration
  $ cat >bad-types.asl <<EOF
  > type t of enumeration {};
  > EOF

  $ aslref bad-types.asl
  File bad-types.asl, line 1, characters 23 to 24:
  ASL Error: Cannot parse.
  [1]

Invalid bitfields
==================

Bad fields
  $ cat >bad-types.asl <<EOF
  > type t of bits(12) {
  >   [10: 3] a,
  >   [2+:1] a,
  > };
  > EOF

  $ aslref bad-types.asl
  File bad-types.asl, line 1, character 0 to line 4, character 2:
  ASL Typing error: cannot declare already declared element "a".
  [1]

Overlapping slices
  $ cat >bad-types.asl <<EOF
  > type t of bits(64) {
  >   [23: 0] a,
  >   [10: 0, 3+: 2] b,
  > };
  > EOF

  $ aslref bad-types.asl
  File bad-types.asl, line 1, character 0 to line 4, character 2:
  ASL Typing error: overlapping slices 0+:11, 3+:2.
  [1]

Bad slices
  $ cat >bad-types.asl <<EOF
  > type t of bits(12) {
  >   [10: 3] a,
  >   [14:12] b,
  > };
  > EOF

  $ aslref bad-types.asl
  File bad-types.asl, line 1, character 0 to line 4, character 2:
  ASL Static error: Cannot extract from bitvector of length 12 slice 12+:3.
  [1]

  $ cat >bad-types.asl <<EOF
  > type t of bits(12) {
  >   [10: 3] a,
  >   [-2+:1] b,
  > };
  > EOF

  $ aslref bad-types.asl
  File bad-types.asl, line 1, character 0 to line 4, character 2:
  ASL Static error: Cannot extract from bitvector of length 12 slice (- 2)+:1.
  [1]

  $ cat >bad-types.asl <<EOF
  > type t of bits(12) {
  >   [10: 3] a,
  >   [7+:3] b {
  >     [1] d,
  >     [10:8] c,
  >   },
  > };
  > EOF

  $ aslref bad-types.asl
  File bad-types.asl, line 1, character 0 to line 7, character 2:
  ASL Static error: Cannot extract from bitvector of length 3 slice 8+:3.
  [1]
