
      * Copyright (C) 2006 Micro Focus (IP) Limited.
      * All rights reserved.
      *
      * Michael Wojcik, Micro Focus International Ltd
      * April 2006

      $set ans85 mf align"8" CallFH"FHREDIR"
      $set noalter comp nocheck nocheckdiv noqualproc noseg notrunc
      $set scheduler

      * cobmd5: An implementation of the MD5 message digest algorithm
      * in COBOL.  See the "mainline" paragraph for an example of using
      * it.  This program actually calculates the MD5 digest of a file
      * specified on the command line.

      *  NOTE: This implementation is inspired by free-for-use C source code
      *  contained in IETF RFC (Request For Comments) document 1321, which is
      *  adapted from RSA Data Security Incorporated's RSAREF library.
      *
      *  The license for the RSADSI-owned materials is as follows:
      *
      *  [Begin RSADSI license.]
      *
      *       Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All
      *       rights reserved.
      *
      *       License to copy and use this software is granted provided that it
      *       is identified as the "RSA Data Security, Inc. MD5 Message-Digest
      *       Algorithm" in all material mentioning or referencing this software
      *       or this function.
      *
      *       License is also granted to make and use derivative works provided
      *       that such works are identified as "derived from the RSA Data
      *       Security, Inc. MD5 Message-Digest Algorithm" in all material
      *       mentioning or referencing the derived work.
      *
      *       RSA Data Security, Inc. makes no representations concerning either
      *       the merchantability of this software or the suitability of this
      *       software for any particular purpose. It is provided "as is"
      *       without express or implied warranty of any kind.
      *
      *       These notices must be retained in any copies of any part of this
      *       documentation and/or software.
      *
      *  [End RSADSI license.]

      * In general, to compute the digest of arbitrary data:
      * 1. Perform MD5-Init to initialize the context.
      * 2. Perform MD5-Update on blocks of input until it's all been processed.
      *    Note that MD5-Update here uses a fixed-size input buffer, but really
      *    could take a linkage item of arbitrary size.
      * 3. Perform MD5-Final to finish.
      * 4. Extract the digest from MD5-Digest.  See Print-Digest for an example.


       identification division.
       program-id. CWMD5 INITIAL.
       author. Michael Wojcik, Micro Focus International Ltd.

       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT BINARIO ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS CWMD5-STATUS.

       data division.
       FILE SECTION.

       FD  BINARIO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS CWMD5-FILE.

       01  BINARIO-REG               PIC X(001).

       working-storage section.
       77  I                                   PIC S9(4) COMP-5.
       77  L                                   PIC X(4) COMP-X VALUE 4.
       77  MD5-CW1                             pic x(4) comp-5 sync.
       77  MD5-CW2                             pic x(4) comp-5 sync.
       77  MD5-CW3                             pic x(4) comp-5 sync.
       01  Program-Vars.
           03  Input-Handle                    pic x(4).
           03  Input-Access                    pic x comp-x sync.
           03  Input-Deny                      pic x comp-x sync.
           03  Input-Device                    pic x comp-x sync.
           03  Input-Flags                     pic x comp-x sync.
           03  Input-Offset                    pic x(8) comp-x sync.
           03  Input-Record                    pic x(4096).
           03  Input-Size                      pic x(8) comp-x sync.
           03        prev                      pic 9(8) comp-x sync.
           03        Lidos                     pic 9(8) comp-x sync.
           03  Input-Count                     pic x(4) comp-x sync.
           03  File-Stat                       pic xx comp-x sync.
           03                                  redefines File-Stat.
               05  FS-Byte-1                   pic x.
               05  FS-Byte-2                   pic x comp-x.
           03  FS-Display-2                    pic 9(3) display.
           03  RESPOSTA                        pic X(1) VALUE SPACE.

       78 Open-Read                            value 1.
       78 Deny-None                            value 3.
       78 Device-None                          value 0.
       78 Flags-None                           value 0.
       78 Flags-FSize                          value 128.


       01  Parameters.
           *> x, y, and z are the parameters to the basic MD5 functions
           03  MD5-X                           pic x(4) comp-5 sync.
           03  MD5-Y                           pic x(4) comp-5 sync.
           03  MD5-Z                           pic x(4) comp-5 sync.

           *> MD5-Basic is the result of a basic function
           03  MD5-Basic                       pic x(4) comp-5 sync.

           *> r and n are the parameters to the MD5 rotate-left function
           03  MD5-R                           pic x(4) comp-5 sync.
           03  MD5-N                           pic x(4) comp-5 sync.

           *> rl is the result of the MD5 rotate-left function
           03  MD5-RL                          pic x(4) comp-5 sync.

           *> a-d, v, s, and ac are the parameters to the MD5 transforms
           *> (RFC 1321 calls v "x", but we've already used x)
           *> a-d are state; v is input data; s and ac are constants
           03  MD5-A                           pic x(4) comp-5 sync.
           03  MD5-B                           pic x(4) comp-5 sync.
           03  MD5-C                           pic x(4) comp-5 sync.
           03  MD5-D                           pic x(4) comp-5 sync.
           03  MD5-V                           pic x(4) comp-5 sync.
           03  MD5-S                           pic x(4) comp-5 sync.
           03  MD5-AC                          pic x(4) comp-5 sync.

           *> invals is the table of 32-bit little-endian integers created from
           *> the input buffer
           03  MD5-InVals                      pic x(4) comp-5 sync
                                               occurs 16.

           *> buf and val are used by Encode and Decode, which convert between
           *> byte buffers and 32-bit integers (Decode is not actually used in
           *> the current implementation)
           03  MD5-Buf                         pic x(4).
           03  MD5-Val                         pic x(4) comp-5 sync.


           *> Parameters to the Update function, which actually processes the
           *> input data.  In a real implementation these would be in linkage,
           *> and MD5-Input would just be a pic x that we'd treat as an array
           *> of undefined length using reference modification.
           03  MD5-Input                       pic x(4096).
           03  MD5-InLen                       pic x(4) comp-5 sync.

           *> Final result - the digest
           03  MD5-Digest                      pic x(16).


       01  Temporaries.
           03  ROL-NthPowerOf2                 pic x(4) comp-5 sync.
           03  ROL-CmpPowerOf2                 pic x(4) comp-5 sync.
           03  ROL-Quotient                    pic x(4) comp-5 sync.
           03  ROL-Remainder                   pic x(4) comp-5 sync.
           03  ROL-PowersOf2                   pic x(4) comp-5 sync
                                               occurs 31.
      *                                       value 2 4 8 16 32 64 128 256
      *                                             512 1024 2048 4096 8192
      *                                             16384 32768 65536 131072
      *                                             262144 524288 1048576
      *                                             2097152 4194304 8388608
      *                                             16777216 33554432 67108864
      *                                             134217728 268435456
      *                                             536870912 1073741824
      *                                             2147483648.
      *
           *> BE-Val and BE-Rep are used by Encode and Decode
           03  BE-Val                          pic x(4) comp-x sync.
           03  BE-Rep         redefines BE-Val pic x(4).

           03  Byte-Idx                        pic x(4) comp-5 sync.
           03  InVal-Idx                       pic x(4) comp-5 sync.
           03  InVal-Bytes.
               05  InVal-Array                 pic x(4).
               05  InVal-Byte                  pic x comp-5 occurs 4
                                               redefines InVal-Array.

           03  InBuf-Used                      pic x(4) comp-5 sync.
           03  Input-Exists                    pic x(4) comp-5 sync.
           03  Input-Extra                     pic x(4) comp-5 sync.

           03  Final-Idx                       pic x(4) comp-5 sync.
           03  Final-Pad                       pic x(4) comp-5 sync.
           03  Final-Count-Hi                  pic x(4) comp-5 sync.
           03  Final-Count-Lo                  pic x(4) comp-5 sync.
           03  Final-Len                       pic x(8).

           03  Digest-Byte                     pic x comp-5 sync.
           03  Digest-ByteVal               pic x redefines Digest-Byte.
           03  Digest-Hi                       pic x comp-5 sync.
           03  Digest-Lo                       pic x comp-5 sync.
           03  Digest-String                   pic x(32).


       01  MD5-Context.
           *> State (MD5_ABCD)
           03  MD5-State                       pic x(16).
           *> State as 4-byte words
           03  MD5-State-Words                 pic x(4) comp-5
                                               occurs 4
                                               redefines MD5-State.
           *> Number of bits mod 2**64.  Note this value is
           *> incorporated into the hash during finalization.
           03  MD5-Count                       pic x(8) comp-5 sync.
           *> Input buffer
           03  MD5-Buffer                      pic x(64).

      * MD5 initialization constants
       78  MD5-Init-1                          value H"67452301".
       78  MD5-Init-2                          value H"efcdab89".
       78  MD5-Init-3                          value H"98badcfe".
       78  MD5-Init-4                          value H"10325476".

      * MD5 round rotary-shift constants
       78  MD5-S11                             value  7.
       78  MD5-S12                             value 12.
       78  MD5-S13                             value 17.
       78  MD5-S14                             value 22.
       78  MD5-S21                             value  5.
       78  MD5-S22                             value  9.
       78  MD5-S23                             value 14.
       78  MD5-S24                             value 20.
       78  MD5-S31                             value  4.
       78  MD5-S32                             value 11.
       78  MD5-S33                             value 16.
       78  MD5-S34                             value 23.
       78  MD5-S41                             value  6.
       78  MD5-S42                             value 10.
       78  MD5-S43                             value 15.
       78  MD5-S44                             value 21.

      * MD5 round additive constants
       78 MD5-AC01                             value H"d76aa478".
       78 MD5-AC02                             value H"e8c7b756".
       78 MD5-AC03                             value H"242070db".
       78 MD5-AC04                             value H"c1bdceee".
       78 MD5-AC05                             value H"f57c0faf".
       78 MD5-AC06                             value H"4787c62a".
       78 MD5-AC07                             value H"a8304613".
       78 MD5-AC08                             value H"fd469501".
       78 MD5-AC09                             value H"698098d8".
       78 MD5-AC10                             value H"8b44f7af".
       78 MD5-AC11                             value H"ffff5bb1".
       78 MD5-AC12                             value H"895cd7be".
       78 MD5-AC13                             value H"6b901122".
       78 MD5-AC14                             value H"fd987193".
       78 MD5-AC15                             value H"a679438e".
       78 MD5-AC16                             value H"49b40821".
       78 MD5-AC17                             value H"f61e2562".
       78 MD5-AC18                             value H"c040b340".
       78 MD5-AC19                             value H"265e5a51".
       78 MD5-AC20                             value H"e9b6c7aa".
       78 MD5-AC21                             value H"d62f105d".
       78 MD5-AC22                             value H"02441453".
       78 MD5-AC23                             value H"d8a1e681".
       78 MD5-AC24                             value H"e7d3fbc8".
       78 MD5-AC25                             value H"21e1cde6".
       78 MD5-AC26                             value H"c33707d6".
       78 MD5-AC27                             value H"f4d50d87".
       78 MD5-AC28                             value H"455a14ed".
       78 MD5-AC29                             value H"a9e3e905".
       78 MD5-AC30                             value H"fcefa3f8".
       78 MD5-AC31                             value H"676f02d9".
       78 MD5-AC32                             value H"8d2a4c8a".
       78 MD5-AC33                             value H"fffa3942".
       78 MD5-AC34                             value H"8771f681".
       78 MD5-AC35                             value H"6d9d6122".
       78 MD5-AC36                             value H"fde5380c".
       78 MD5-AC37                             value H"a4beea44".
       78 MD5-AC38                             value H"4bdecfa9".
       78 MD5-AC39                             value H"f6bb4b60".
       78 MD5-AC40                             value H"bebfbc70".
       78 MD5-AC41                             value H"289b7ec6".
       78 MD5-AC42                             value H"eaa127fa".
       78 MD5-AC43                             value H"d4ef3085".
       78 MD5-AC44                             value H"04881d05".
       78 MD5-AC45                             value H"d9d4d039".
       78 MD5-AC46                             value H"e6db99e5".
       78 MD5-AC47                             value H"1fa27cf8".
       78 MD5-AC48                             value H"c4ac5665".
       78 MD5-AC49                             value H"f4292244".
       78 MD5-AC50                             value H"432aff97".
       78 MD5-AC51                             value H"ab9423a7".
       78 MD5-AC52                             value H"fc93a039".
       78 MD5-AC53                             value H"655b59c3".
       78 MD5-AC54                             value H"8f0ccc92".
       78 MD5-AC55                             value H"ffeff47d".
       78 MD5-AC56                             value H"85845dd1".
       78 MD5-AC57                             value H"6fa87e4f".
       78 MD5-AC58                             value H"fe2ce6e0".
       78 MD5-AC59                             value H"a3014314".
       78 MD5-AC60                             value H"4e0811a1".
       78 MD5-AC61                             value H"f7537e82".
       78 MD5-AC62                             value H"bd3af235".
       78 MD5-AC63                             value H"2ad7d2bb".
       78 MD5-AC64                             value H"eb86d391".


       01  MD5-Padding.
           03  MD5-Padding-FirstByte           pic x comp-5 value H"80".
           03  MD5-Padding-Remainder         pic x(63) value low-values.


      * For hex display
       01  Hex-Chars.
           03  Hex-Digits                      pic x(16)
                                               value "0123456789abcdef".
           03  Hex-Char                        pic x occurs 16
                                               redefines Hex-Digits.

       LINKAGE SECTION.

       COPY CWMD5.
       01 BUFFER PIC X.

       procedure division USING PARAMETROS-CWMD5.

           ON 1
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 31
                   IF I = 1
                      MOVE 2 TO ROL-PowersOf2 (I)
                   ELSE
                      COMPUTE ROL-PowersOf2 (I)
                            = ROL-PowersOf2 (I - 1) * 2
                   END-IF
           END-PERFORM.
      * Test MD5 routines by generating and displaying digest of the file
      * specified on the command line
       mainline.
           *> Get input filename
      *    if CWMD5-FILE = spaces
      *        display "cobmd5: calculate MD5 digest for a file"
      *        display "Copyright (C) 2006 Micro Focus (IP) Limited"
      *        display "syntax: cobmd5 filename"
      *        goback returning 1
      *        end-if
      *
           IF CWMD5-FILE(1:1) NOT = SPACE
              *> Try to open it
              IF CWMD5-FILESHARE
                 OPEN INPUT BINARIO
                 IF CWMD5-STATUS NOT = '00'
                    GOBACK
                 END-IF
                 MOVE 1 TO INPUT-SIZE Input-Count
                 MOVE 0 TO Input-Offset
              ELSE
                 move Open-Read to Input-Access
      *          move Deny-None to Input-Deny
Exclus           move 0         to Input-Deny
                 move Device-None to Input-Device
                 call "CBL_OPEN_FILE" using
                     CWMD5-FILE
                     Input-Access
                     Input-Deny
                     Input-Device
                     Input-Handle
                 end-call
                 if return-code not = 0
                     move return-code to File-Stat
                     move FS-Byte-2 to FS-Display-2
      *              display
      *                  "unable to open input file: "
      *                  FS-Byte-1 "/" FS-Display-2
                     MOVE FS-Byte-1 TO CWMD5-STATUS(1:1)
                     MOVE FS-Byte-2 TO CWMD5-STATUS(2:1)
                     goback
                 ELSE
                     MOVE '00'      TO CWMD5-STATUS
                 end-if
                 *> Get file size
                 move 0 to Input-Count Input-Size Input-Offset
                 move Flags-FSize to Input-Flags
                 call "CBL_READ_FILE" using
                     Input-Handle
                     Input-Size
                     Input-Count
                     Input-Flags
                     Input-Record
                     end-call
                 if return-code not = 0
                     move return-code to File-Stat
                     move FS-Byte-2 to FS-Display-2
                     MOVE FS-Byte-1 TO CWMD5-STATUS(1:1)
                     MOVE FS-Byte-2 TO CWMD5-STATUS(2:1)
      *              display
      *                  "unable to get input file size: "
      *                  FS-Byte-1 "/" FS-Display-2
                     goback
                 end-if
                 move input-size to prev
              END-IF
           ELSE
              IF CWMD5-FIELD = NULL
                 MOVE 0 TO Input-Size
                           Input-Count
                 MOVE SPACES TO CWMD5-HASH
                 GOBACK
              ELSE
                 SET  ADDRESS OF BUFFER  TO CWMD5-FIELD
                 MOVE CWMD5-FIELD-LENGTH TO Input-Size
                                            Input-Count
              END-IF
           END-IF

           *> Start MD5 digest
           perform MD5-Init

           *> Read from file until EOF, generating digest
           move 0 to return-code
           move Flags-None to Input-Flags
           perform until Input-Size = 0
               *> Read up to buffer size or remaining bytes
               if Input-Size > length of Input-Record
                   move length of Input-Record to Input-Count
               else
                   move Input-Size to Input-Count
               end-if
               IF CWMD5-FILE(1:1) NOT = SPACE
                  IF CWMD5-FILESHARE
                     READ BINARIO INTO Input-Record (1:1)
                          AT END
                             MOVE 0 TO INPUT-SIZE
                     END-READ
                  ELSE
                     IF CWMD5-PROGRESS
                     compute lidos = Input-Offset +
                                     Input-Count
                     EXEC COBOLware PROCESS (SHOW)
                          HEADER 'Calculando MD5'
                          LINE 16 COLUMN 12
                          MESSAGE CWMD5-FILE
                          QUESTION "Deseja interromper ? "
                          CANCEL;RESPOSTA
                          EMPTY prev
                          FULL  lidos
                     END-EXEC
                     END-IF
                     call "CBL_READ_FILE" using
                         Input-Handle
                         Input-Offset
                         Input-Count
                         Input-Flags
                         Input-Record
                         end-call
                     if return-code not = 0
                         move return-code to File-Stat
                         move FS-Byte-2 to FS-Display-2
      *                  display
      *                      "unable to read input file: "
      *                      FS-Byte-1 "/" FS-Display-2
                         MOVE FS-Byte-1 TO CWMD5-STATUS(1:1)
                         MOVE FS-Byte-2 TO CWMD5-STATUS(2:1)
                         goback
                     end-if
                  END-IF
               ELSE
                  MOVE BUFFER (Input-Offset + 1 : Input-Count)
                    TO Input-Record
               END-IF
               *> Update offset, remaining size
               IF RESPOSTA = 'Y'
                  MOVE 0 TO Input-SIZE
               END-IF
               IF INPUT-SIZE NOT = 0
                  IF (NOT CWMD5-FILESHARE)
                  OR CWMD5-FILE = SPACES
                     add Input-Count to Input-Offset
                     subtract Input-Count from Input-Size
                  END-IF
                  *> Update digest
                  move Input-Record(1 : Input-Count) to MD5-Input
                  move Input-Count to MD5-InLen
                  perform MD5-Update
               END-IF
               IF INPUT-SIZE = 0
                  IF CWMD5-FILE(1:1) NOT = SPACE
                     IF CWMD5-FILESHARE
                        CLOSE BINARIO
                     ELSE
                        call "CBL_CLOSE_FILE" using Input-Handle
                     END-IF
                  END-IF
                  IF (CWMD5-SALT NOT = NULL)
                  AND (RESPOSTA NOT = 'Y')
                     SET ADDRESS OF BUFFER TO CWMD5-SALT
                     move 0                 to Input-Offset
                     MOVE CWMD5-SALT-LENGTH TO Input-Size
                                               Input-Count
                     SET CWMD5-SALT TO NULL
                     MOVE SPACES TO CWMD5-FILE
                  END-IF
               END-IF
           end-perform

           *> Finish MD5 digest
           perform MD5-Final

           *> Display MD5 digest
           perform MD5-Print

           exit program
           goback returning 0.


      * Optimization note: the docs say that sections are more efficient for
      * performing than paragraphs, so I've made the inner-loop paragraphs into
      * their own sections.

      * F, G, H, and I are the basic MD5 functions.

       MD5-F section.

           move MD5-X TO MD5-Basic
           CALL "CBL_AND" USING MD5-Y MD5-Basic BY VALUE L

           move MD5-X TO MD5-CW2
           CALL "CBL_NOT" USING MD5-CW2       BY VALUE L

           CALL "CBL_AND" USING MD5-Z MD5-CW2 BY VALUE L
           CALL "CBL_OR"  USING MD5-CW2 MD5-Basic BY VALUE L

      *   compute MD5-Basic =
      *       (MD5-X b-and MD5-Y) b-or ((b-not MD5-X) b-and MD5-Z)
      *   IF MD5-CW1 NOT = MD5-Basic
      *      STOP 'MF4 nÆo bateu com NE31'
      *   END-IF
           exit section.

       MD5-G section.

           move MD5-X TO MD5-Basic
           CALL "CBL_AND" USING MD5-Z MD5-Basic BY VALUE L

           move MD5-Z TO MD5-CW2
           CALL "CBL_NOT" USING MD5-CW2       BY VALUE L

           MOVE MD5-Y TO MD5-CW3
           CALL "CBL_AND" USING MD5-CW2 MD5-CW3 BY VALUE L
           CALL "CBL_OR"  USING MD5-CW3 MD5-Basic BY VALUE L

      *   compute MD5-Basic =
      *       (MD5-X b-and MD5-Z) b-or (MD5-Y b-and (b-not MD5-Z))
      *   IF MD5-CW1 NOT = MD5-Basic
      *      STOP 'MF4 nÆo bateu com NE31'
      *   END-IF
           exit section.

       MD5-H section.
           move MD5-X TO MD5-Basic
           CALL "CBL_XOR" USING MD5-Y MD5-Basic BY VALUE L
           CALL "CBL_XOR" USING MD5-Z MD5-Basic BY VALUE L
      *   compute MD5-Basic =
      *       (MD5-X b-xor MD5-Y) b-xor MD5-Z
      *   IF MD5-CW1 not = MD5-Basic
      *      STOP 'MF4 nÆo bateu com NE31'
      *   end-if
           exit section.

       MD5-I section.

           move MD5-Z TO MD5-CW1
           CALL "CBL_NOT" USING MD5-CW1 BY VALUE L
           move MD5-X TO MD5-CW2
           CALL "CBL_OR" USING MD5-CW1 MD5-CW2 BY VALUE L
           move MD5-Y TO MD5-Basic
           CALL "CBL_XOR" USING MD5-CW2 MD5-Basic BY VALUE L

      *   compute MD5-Basic =
      *       MD5-Y b-xor (MD5-X b-or (b-not MD5-Z))
      *   IF MD5-CW3 not = MD5-Basic
      *      STOP 'MF4 nÆo bateu com NE31'
      *   end-if
           exit section.


      * ROL rotates a 32-bit integer left by n bits.
      * Example usage:
      *    move H"12345678" to MD5-R
      *    move 20 to MD5-N
      *    display "before: " MD5-R
      *    perform MD5-ROL
      *    display "after: " MD5-RL

       MD5-ROL section.
           *> Determine the nth power of 2, which will be the shift multiplier, and
           *> the (32-n)th power of 2, which will be the partition between the left
           *> and right halves.  A table lookup here is TREMENDOUSLY faster than a
           *> couple of compute statements - total running time was reduced by a
           *> couple of orders of magnitude.  Amazing.

           move ROL-PowersOf2(MD5-N) to ROL-NthPowerOf2
      *   move ROL-PowersOf2(32 - MD5-N) to ROL-CmpPowerOf2
           compute I = 32 - MD5-N
           move ROL-PowersOf2(I)          to ROL-CmpPowerOf2

           *> R / ROL-CmpPowerOf2 is the left half, and R mod ROL-CmpPowerOf2 is the
           *> right half.

           divide MD5-R by ROL-CmpPowerOf2 giving ROL-Quotient
                                        remainder ROL-Remainder

           *> Swap the left and right halves by multiplying ROL-Remainder by
           *> ROL-NthPowerOf2 and adding the two results

           multiply ROL-Remainder by ROL-NthPowerOf2 giving MD5-RL
           add ROL-Quotient to MD5-RL
           exit section.


      * FF, GG, HH, and II are the MD5 transformations.

       MD5-FF section.
           *> compute F(b,c,d)
           move MD5-B to MD5-X
           move MD5-C to MD5-Y
           move MD5-D to MD5-Z
           perform MD5-F

           *> Add F(b,c,d), v, and ac to a, modulo 2**32
           add MD5-Basic to MD5-A
           add MD5-V to MD5-A
           add MD5-AC to MD5-A

           *> Rotate a by s
           move MD5-A to MD5-R
           move MD5-S to MD5-N
           perform MD5-ROL

           *> Add b to result of rotation
           add MD5-B to MD5-RL

           *> Rotate the four state words: a gets d, d gets c, c gets b, b gets the
           *> new value of a from the temp.
           move MD5-D to MD5-A
           move MD5-C to MD5-D
           move MD5-B to MD5-C
           move MD5-RL to MD5-B
           exit section.

      * GG, HH, and II are identical to FF except with a different basic function

       MD5-GG section.
           move MD5-B to MD5-X
           move MD5-C to MD5-Y
           move MD5-D to MD5-Z
           perform MD5-G
           add MD5-A to MD5-Basic giving MD5-R
           add MD5-V to MD5-R
           add MD5-AC to MD5-R
           move MD5-S to MD5-N
           perform MD5-ROL
           add MD5-B to MD5-RL
           move MD5-D to MD5-A
           move MD5-C to MD5-D
           move MD5-B to MD5-C
           move MD5-RL to MD5-B
           exit section.

       MD5-HH section.
           move MD5-B to MD5-X
           move MD5-C to MD5-Y
           move MD5-D to MD5-Z
           perform MD5-H
           add MD5-A to MD5-Basic giving MD5-R
           add MD5-V to MD5-R
           add MD5-AC to MD5-R
           move MD5-S to MD5-N
           perform MD5-ROL
           add MD5-B to MD5-RL
           move MD5-D to MD5-A
           move MD5-C to MD5-D
           move MD5-B to MD5-C
           move MD5-RL to MD5-B
           exit section.

       MD5-II section.
           move MD5-B to MD5-X
           move MD5-C to MD5-Y
           move MD5-D to MD5-Z
           perform MD5-I
           add MD5-A to MD5-Basic giving MD5-R
           add MD5-V to MD5-R
           add MD5-AC to MD5-R
           move MD5-S to MD5-N
           perform MD5-ROL
           add MD5-B to MD5-RL
           move MD5-D to MD5-A
           move MD5-C to MD5-D
           move MD5-B to MD5-C
           move MD5-RL to MD5-B
           exit section.


      * MD5 transformation function: update the 128 bits of state using 64 bytes
      * of input.

      * This consists of four rounds, each with 16 subrounds.  Each round uses
      * a different transformation (FF, GG, etc), and each subround applies it
      * to four bytes of the input, using the appropriate constants for S and
      * AC.  (In the first round the input bytes are processed in order; the
      * remaining rounds vary the order using a generator, but they're always
      * in groups of 4.)
      *
      * These rounds update A, B, C, and D.

      * At the end of the four rounds, the 128 bits of state are updated using
      * A, B, C, and D: A is added to the first four bytes (modulo 2**32), and
      * so on.

       MD5-Transform section.
           *> Set registers A-D from the state
           move MD5-State-Words(1) to MD5-A
           move MD5-State-Words(2) to MD5-B
           move MD5-State-Words(3) to MD5-C
           move MD5-State-Words(4) to MD5-D

           *> Convert input buffer interpreted as little-endian 32-bit integers
           *> to native integers
           perform MD5-SetInVals

           *> Round 1
           move MD5-InVals(1) to MD5-V
           move MD5-S11 to MD5-S
           move MD5-AC01 to MD5-AC
           perform MD5-FF
           move MD5-InVals(2) to MD5-V
           move MD5-S12 to MD5-S
           move MD5-AC02 to MD5-AC
           perform MD5-FF
           move MD5-InVals(3) to MD5-V
           move MD5-S13 to MD5-S
           move MD5-AC03 to MD5-AC
           perform MD5-FF
           move MD5-InVals(4) to MD5-V
           move MD5-S14 to MD5-S
           move MD5-AC04 to MD5-AC
           perform MD5-FF
           move MD5-InVals(5) to MD5-V
           move MD5-S11 to MD5-S
           move MD5-AC05 to MD5-AC
           perform MD5-FF
           move MD5-InVals(6) to MD5-V
           move MD5-S12 to MD5-S
           move MD5-AC06 to MD5-AC
           perform MD5-FF
           move MD5-InVals(7) to MD5-V
           move MD5-S13 to MD5-S
           move MD5-AC07 to MD5-AC
           perform MD5-FF
           move MD5-InVals(8) to MD5-V
           move MD5-S14 to MD5-S
           move MD5-AC08 to MD5-AC
           perform MD5-FF
           move MD5-InVals(9) to MD5-V
           move MD5-S11 to MD5-S
           move MD5-AC09 to MD5-AC
           perform MD5-FF
           move MD5-InVals(10) to MD5-V
           move MD5-S12 to MD5-S
           move MD5-AC10 to MD5-AC
           perform MD5-FF
           move MD5-InVals(11) to MD5-V
           move MD5-S13 to MD5-S
           move MD5-AC11 to MD5-AC
           perform MD5-FF
           move MD5-InVals(12) to MD5-V
           move MD5-S14 to MD5-S
           move MD5-AC12 to MD5-AC
           perform MD5-FF
           move MD5-InVals(13) to MD5-V
           move MD5-S11 to MD5-S
           move MD5-AC13 to MD5-AC
           perform MD5-FF
           move MD5-InVals(14) to MD5-V
           move MD5-S12 to MD5-S
           move MD5-AC14 to MD5-AC
           perform MD5-FF
           move MD5-InVals(15) to MD5-V
           move MD5-S13 to MD5-S
           move MD5-AC15 to MD5-AC
           perform MD5-FF
           move MD5-InVals(16) to MD5-V
           move MD5-S14 to MD5-S
           move MD5-AC16 to MD5-AC
           perform MD5-FF

           *> Round 2
           move MD5-InVals(2) to MD5-V
           move MD5-S21 to MD5-S
           move MD5-AC17 to MD5-AC
           perform MD5-GG
           move MD5-InVals(7) to MD5-V
           move MD5-S22 to MD5-S
           move MD5-AC18 to MD5-AC
           perform MD5-GG
           move MD5-InVals(12) to MD5-V
           move MD5-S23 to MD5-S
           move MD5-AC19 to MD5-AC
           perform MD5-GG
           move MD5-InVals(1) to MD5-V
           move MD5-S24 to MD5-S
           move MD5-AC20 to MD5-AC
           perform MD5-GG
           move MD5-InVals(6) to MD5-V
           move MD5-S21 to MD5-S
           move MD5-AC21 to MD5-AC
           perform MD5-GG
           move MD5-InVals(11) to MD5-V
           move MD5-S22 to MD5-S
           move MD5-AC22 to MD5-AC
           perform MD5-GG
           move MD5-InVals(16) to MD5-V
           move MD5-S23 to MD5-S
           move MD5-AC23 to MD5-AC
           perform MD5-GG
           move MD5-InVals(5) to MD5-V
           move MD5-S24 to MD5-S
           move MD5-AC24 to MD5-AC
           perform MD5-GG
           move MD5-InVals(10) to MD5-V
           move MD5-S21 to MD5-S
           move MD5-AC25 to MD5-AC
           perform MD5-GG
           move MD5-InVals(15) to MD5-V
           move MD5-S22 to MD5-S
           move MD5-AC26 to MD5-AC
           perform MD5-GG
           move MD5-InVals(4) to MD5-V
           move MD5-S23 to MD5-S
           move MD5-AC27 to MD5-AC
           perform MD5-GG
           move MD5-InVals(9) to MD5-V
           move MD5-S24 to MD5-S
           move MD5-AC28 to MD5-AC
           perform MD5-GG
           move MD5-InVals(14) to MD5-V
           move MD5-S21 to MD5-S
           move MD5-AC29 to MD5-AC
           perform MD5-GG
           move MD5-InVals(3) to MD5-V
           move MD5-S22 to MD5-S
           move MD5-AC30 to MD5-AC
           perform MD5-GG
           move MD5-InVals(8) to MD5-V
           move MD5-S23 to MD5-S
           move MD5-AC31 to MD5-AC
           perform MD5-GG
           move MD5-InVals(13) to MD5-V
           move MD5-S24 to MD5-S
           move MD5-AC32 to MD5-AC
           perform MD5-GG

           *> Round 3
           move MD5-InVals(6) to MD5-V
           move MD5-S31 to MD5-S
           move MD5-AC33 to MD5-AC
           perform MD5-HH
           move MD5-InVals(9) to MD5-V
           move MD5-S32 to MD5-S
           move MD5-AC34 to MD5-AC
           perform MD5-HH
           move MD5-InVals(12) to MD5-V
           move MD5-S33 to MD5-S
           move MD5-AC35 to MD5-AC
           perform MD5-HH
           move MD5-InVals(15) to MD5-V
           move MD5-S34 to MD5-S
           move MD5-AC36 to MD5-AC
           perform MD5-HH
           move MD5-InVals(2) to MD5-V
           move MD5-S31 to MD5-S
           move MD5-AC37 to MD5-AC
           perform MD5-HH
           move MD5-InVals(5) to MD5-V
           move MD5-S32 to MD5-S
           move MD5-AC38 to MD5-AC
           perform MD5-HH
           move MD5-InVals(8) to MD5-V
           move MD5-S33 to MD5-S
           move MD5-AC39 to MD5-AC
           perform MD5-HH
           move MD5-InVals(11) to MD5-V
           move MD5-S34 to MD5-S
           move MD5-AC40 to MD5-AC
           perform MD5-HH
           move MD5-InVals(14) to MD5-V
           move MD5-S31 to MD5-S
           move MD5-AC41 to MD5-AC
           perform MD5-HH
           move MD5-InVals(1) to MD5-V
           move MD5-S32 to MD5-S
           move MD5-AC42 to MD5-AC
           perform MD5-HH
           move MD5-InVals(4) to MD5-V
           move MD5-S33 to MD5-S
           move MD5-AC43 to MD5-AC
           perform MD5-HH
           move MD5-InVals(7) to MD5-V
           move MD5-S34 to MD5-S
           move MD5-AC44 to MD5-AC
           perform MD5-HH
           move MD5-InVals(10) to MD5-V
           move MD5-S31 to MD5-S
           move MD5-AC45 to MD5-AC
           perform MD5-HH
           move MD5-InVals(13) to MD5-V
           move MD5-S32 to MD5-S
           move MD5-AC46 to MD5-AC
           perform MD5-HH
           move MD5-InVals(16) to MD5-V
           move MD5-S33 to MD5-S
           move MD5-AC47 to MD5-AC
           perform MD5-HH
           move MD5-InVals(3) to MD5-V
           move MD5-S34 to MD5-S
           move MD5-AC48 to MD5-AC
           perform MD5-HH

           *> Round 4
           move MD5-InVals(1) to MD5-V
           move MD5-S41 to MD5-S
           move MD5-AC49 to MD5-AC
           perform MD5-II
           move MD5-InVals(8) to MD5-V
           move MD5-S42 to MD5-S
           move MD5-AC50 to MD5-AC
           perform MD5-II
           move MD5-InVals(15) to MD5-V
           move MD5-S43 to MD5-S
           move MD5-AC51 to MD5-AC
           perform MD5-II
           move MD5-InVals(6) to MD5-V
           move MD5-S44 to MD5-S
           move MD5-AC52 to MD5-AC
           perform MD5-II
           move MD5-InVals(13) to MD5-V
           move MD5-S41 to MD5-S
           move MD5-AC53 to MD5-AC
           perform MD5-II
           move MD5-InVals(4) to MD5-V
           move MD5-S42 to MD5-S
           move MD5-AC54 to MD5-AC
           perform MD5-II
           move MD5-InVals(11) to MD5-V
           move MD5-S43 to MD5-S
           move MD5-AC55 to MD5-AC
           perform MD5-II
           move MD5-InVals(2) to MD5-V
           move MD5-S44 to MD5-S
           move MD5-AC56 to MD5-AC
           perform MD5-II
           move MD5-InVals(9) to MD5-V
           move MD5-S41 to MD5-S
           move MD5-AC57 to MD5-AC
           perform MD5-II
           move MD5-InVals(16) to MD5-V
           move MD5-S42 to MD5-S
           move MD5-AC58 to MD5-AC
           perform MD5-II
           move MD5-InVals(7) to MD5-V
           move MD5-S43 to MD5-S
           move MD5-AC59 to MD5-AC
           perform MD5-II
           move MD5-InVals(14) to MD5-V
           move MD5-S44 to MD5-S
           move MD5-AC60 to MD5-AC
           perform MD5-II
           move MD5-InVals(5) to MD5-V
           move MD5-S41 to MD5-S
           move MD5-AC61 to MD5-AC
           perform MD5-II
           move MD5-InVals(12) to MD5-V
           move MD5-S42 to MD5-S
           move MD5-AC62 to MD5-AC
           perform MD5-II
           move MD5-InVals(3) to MD5-V
           move MD5-S43 to MD5-S
           move MD5-AC63 to MD5-AC
           perform MD5-II
           move MD5-InVals(10) to MD5-V
           move MD5-S44 to MD5-S
           move MD5-AC64 to MD5-AC
           perform MD5-II

           *> Update state by adding the appropriate register to each of the
           *> four words of state information, modulo 2**32
           add MD5-A to MD5-State-Words(1)
           add MD5-B to MD5-State-Words(2)
           add MD5-C to MD5-State-Words(3)
           add MD5-D to MD5-State-Words(4)
           exit section.


      * MD5 integer encode: translate a 32-bit integer in machine format into an
      * array of bytes in little-endian (LSB-first) ordering.  What I do here is
      * move it to the big-endian (comp-x) data item BE-Val, so it has a known
      * byte order, then extract the bytes in reverse order using the byte-array
      * redefinition BE-Rep to get the bytes in little-endian order.

       MD5-Encode section.
           move MD5-Val to BE-Val          *> convert to MSB byte ordering
           perform varying Byte-Idx from 1 by 1 until Byte-Idx > 4
               move BE-Rep(Byte-Idx : 1) to MD5-Buf(5 - Byte-Idx : 1)
               end-perform
           exit section.


      * MD5 integer decode: set a 32-bit integer in machine format from an array
      * of bytes in little-endian (LSB-first) ordering.  The inverse of Encode.

       MD5-Decode section.
           perform varying Byte-Idx from 1 by 1 until Byte-Idx > 4
               move MD5-Buf(Byte-Idx : 1) to BE-Rep(5 - Byte-Idx : 1)
               end-perform
           move BE-Val to MD5-Val
           exit section.


      * MD5 input conversion: MD5 operates on the input as a series of 32-bit
      * unsigned little-endian integers.  Here we convert the input byte buffer
      * into native-order integers.

       MD5-SetInVals section.
           perform varying InVal-Idx from 1 by 1 until InVal-Idx > 16
               compute Byte-Idx = (InVal-Idx - 1) * 4 + 1
               move MD5-Buffer(Byte-Idx : 4) to InVal-Array
               *> b-or doesn't work correctly with byte-size operands, or with
               *> exponentiation.  Addition should be safe here.
               compute MD5-InVals(InVal-Idx) =
                   (InVal-Byte(4) * 16777216) +
                   (InVal-Byte(3) *    65536) +
                   (InVal-Byte(2) *      256) +
                   (InVal-Byte(1) *        1)
               end-perform
           exit section.


      * MD5 initialization

       MD5-Init.
           move low-values to MD5-Context
           move MD5-Init-1 to MD5-State-Words(1)
           move MD5-Init-2 to MD5-State-Words(2)
           move MD5-Init-3 to MD5-State-Words(3)
           move MD5-Init-4 to MD5-State-Words(4)
           exit.


      * MD5 update
      * This is the function that takes an input buffer and updates the hash with
      * it.  The buffer can contain any number of bytes of data, up to the size
      * that can be represented by the length data-item.  A hash is generated by
      * calling MD5-Init, then MD5-Update one or more times, then MD5-Finalize
      * when all the data has been passed to Update.

       MD5-Update.
           ON 1 AND EVERY 1000 CALL 'CWATCH'.
           *> Compute number of bytes remaining in MD5 buffer from last chunk mod 64;
           *> this is the index into the buffer where the new data starts.
           compute Input-Exists = function rem( (MD5-Count / 8) 64 )

           *> Update number of bits: add input length * 8 to count field, which
           *> is a 64-bit unsigned integer.  (MD5 incorporates the input length in
           *> bits mod 2**64 into the hash during finalization.)
           compute MD5-Count = MD5-Count + (MD5-InLen * 8)

           *> If we have enough data to complete the partial block in the buffer,
           *> do so and process it.
           if Input-Exists > 0 and Input-Exists + MD5-InLen >= 64
               subtract Input-Exists from 64 giving InBuf-Used
               move MD5-Input(1 : InBuf-Used) to
                    MD5-Buffer(Input-Exists + 1 : InBuf-Used)
               perform MD5-Transform
               move 0 to Input-Exists      *> used up old input data
           else
               move 0 to InBuf-Used
               end-if

           *> Process any remaining full blocks of new input data
           perform until MD5-InLen - InBuf-Used < 64
               move MD5-Input(InBuf-Used + 1 : 64) to MD5-Buffer(1 : 64)
               add 64 to InBuf-Used
               perform MD5-Transform
               end-perform

           *> If any input remains, buffer it for the next Update call or for
           *> Finalize
           subtract InBuf-Used from MD5-InLen giving Input-Extra
           if Input-Extra > 0
               move MD5-Input(InBuf-Used + 1 : Input-Extra) to
                    MD5-Buffer(Input-Exists + 1 : Input-Extra)
               end-if
           exit.


      * MD5 finalization
      * All of the input data has been processed, except for any final remaining
      * partial block.  Encode the input length as a 64-bit little-endian integer.
      * Append MD5 padding to the remaining partial input (if any) so that we'll
      * have a complete block when we append the 8 bytes of the length, then
      * append the length (making a final full input block) and run the last
      * iteration of the transform.

       MD5-Final.
           *> Convert the length of input to a little-endian integer
           divide MD5-Count by 4294967296
               giving Final-Count-Hi remainder Final-Count-Lo
           move Final-Count-Lo to MD5-Val
           perform MD5-Encode
           move MD5-Buf to Final-Len(1:4)
           move Final-Count-Hi to MD5-Val
           perform MD5-Encode
           move MD5-Buf to Final-Len(5:4)

           *> Figure out how much padding we need.  We have 8 bytes of length to
           *> store, and blocks are 64 bytes, and the MD5 spec requires that there's
           *> always some padding, even if that means we have a whole block of
           *> padding.  So compute the length (in bytes - note the context stores
           *> it in bits) mod 64, then subtract that from 120 (= 56 mod 64), and
           *> reduce that mod 64.  The result, added to the current length, will be
           *> congruent to 56 modulo 64.
           compute Final-Idx = function rem( (MD5-Count / 8) 64 )
           compute Final-Pad = function rem( (120 - Final-Idx) 64 )

           *> Pad
           move MD5-Padding to MD5-Input
           move Final-Pad to MD5-InLen
           perform MD5-Update

           *> Append length and perform final transformation
           move Final-Len to MD5-Input
           move 8 to MD5-InLen
           perform MD5-Update

           *> Extract the digest from the state: the digest is the 128-bit little-
           *> endian number that represents the final value of the state
           move MD5-State-Words(1) to MD5-Val
           perform MD5-Encode
           move MD5-Buf(1:4) to MD5-Digest(1:4)
           move MD5-State-Words(2) to MD5-Val
           perform MD5-Encode
           move MD5-Buf(1:4) to MD5-Digest(5:4)
           move MD5-State-Words(3) to MD5-Val
           perform MD5-Encode
           move MD5-Buf(1:4) to MD5-Digest(9:4)
           move MD5-State-Words(4) to MD5-Val
           perform MD5-Encode
           move MD5-Buf(1:4) to MD5-Digest(13:4)
           exit.


      * Print a digest
       MD5-Print.
           perform varying Byte-Idx from 1 by 1 until Byte-Idx > 16
               move MD5-Digest(Byte-Idx : 1) to Digest-ByteVal
               compute Digest-Hi
                     = function rem( (Digest-Byte / 16) 16 ) + 1
               compute Digest-Lo = function rem( Digest-Byte 16 ) + 1
               move Hex-Char(Digest-Hi)
                 to Digest-String(Byte-Idx * 2 - 1 : 1)
               move Hex-Char(Digest-Lo)
                 to Digest-String(Byte-Idx * 2     : 1)
               end-perform
           IF RESPOSTA = 'Y'
              MOVE SPACES TO CWMD5-HASH RESPOSTA
           ELSE
              MOVE Digest-String TO CWMD5-HASH
           END-IF
           exit.

