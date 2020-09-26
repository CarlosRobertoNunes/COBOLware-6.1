       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWRKBD.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/06/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_READ_KBD_CHAR Modo Grafico        *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 FIELD-AREA        PIC  X(001) VALUE SPACE.
           05 VEZ               PIC  9(001) VALUE 1.
           05 CD-CTRL-FIELD-KEY PIC S9(004) COMP-5 VALUE 0.
           05 I                 PIC  9(003) VALUE 0.
           05 X                 PIC  9(002) COMP-X.
           05 curs-show         PIC  x(001) VALUE SPACE.
           05 CWCASE            PIC  X(003) VALUE SPACES.
           05 CURPOS.
              10 CURPOS-LIN     PIC  9(002) VALUE 1.
              10 CURPOS-COL     PIC  9(002) VALUE 1.
           05 SCREEN-POSITION.
              10 ROW-NUMBER     PIC  9(002) COMP-X VALUE 0.
              10 COLUMN-NUMBER  PIC  9(002) COMP-X VALUE 0.
           01   STATIC   PIC X(001).
           01   FIELD    PIC X(001).
           01   DISABLED PIC X(001).
           01   CURCOLOR PIC X(001).
       COPY CWSPWS.

       01  KEYS.
           05 KEY-CTRL-A          PIC X(11) VALUE "001 001    ".
           05 KEY-CTRL-B          PIC X(11) VALUE "002 002    ".
           05 KEY-CTRL-C          PIC X(11) VALUE "003 003    ".
           05 KEY-CTRL-D          PIC X(11) VALUE "004 004    ".
           05 KEY-CTRL-E          PIC X(11) VALUE "005 005    ".
           05 KEY-CTRL-F          PIC X(11) VALUE "006 006    ".
           05 KEY-CTRL-G          PIC X(11) VALUE "007 007    ".
           05 KEY-BACKSPAC        PIC X(11) VALUE "008 008    ".
           05 KEY-TAB             PIC X(11) VALUE "009 009    ".
           05 KEY-CTRL-J          PIC X(11) VALUE "010 010    ".
           05 KEY-CTRL-K          PIC X(11) VALUE "011 011    ".
           05 KEY-CTRL-L          PIC X(11) VALUE "012 012    ".
           05 KEY-ENTER           PIC X(11) VALUE "013 013    ".
           05 KEY-CTRL-N          PIC X(11) VALUE "014 014    ".
           05 KEY-CTRL-O          PIC X(11) VALUE "015 015    ".
           05 KEY-CTRL-P          PIC X(11) VALUE "016 016    ".
           05 KEY-CTRL-Q          PIC X(11) VALUE "017 017    ".
           05 KEY-CTRL-R          PIC X(11) VALUE "018 018    ".
           05 KEY-CTRL-S          PIC X(11) VALUE "019 019    ".
           05 KEY-CTRL-T          PIC X(11) VALUE "020 020    ".
           05 KEY-CTRL-U          PIC X(11) VALUE "021 021    ".
           05 KEY-CTRL-V          PIC X(11) VALUE "022 022    ".
           05 KEY-CTRL-W          PIC X(11) VALUE "023 023    ".
           05 KEY-CTRL-X          PIC X(11) VALUE "024 024    ".
           05 KEY-CTRL-Y          PIC X(11) VALUE "025 025    ".
           05 KEY-CTRL-Z          PIC X(11) VALUE "026 026    ".
           05 KEY-ESC             PIC X(11) VALUE "027 027    ".
           05 KEY-CTRL-BACKSLASH  PIC X(11) VALUE "028 028    ".
           05 KEY-CTRL-BRACKET    PIC X(11) VALUE "029 029    ".
           05 KEY-CTRL-MINUS      PIC X(11) VALUE "031 031    ".
           05 KEY-SPACEBAR        PIC X(11) VALUE "032 032    ".
           05 KEY-BACKTAB         PIC X(11) VALUE "271 000 015".
           05 KEY-ALT-Q           PIC X(11) VALUE "272 000 016".
           05 KEY-ALT-W           PIC X(11) VALUE "273 000 017".
           05 KEY-ALT-E           PIC X(11) VALUE "274 000 018".
           05 KEY-ALT-R           PIC X(11) VALUE "275 000 019".
           05 KEY-ALT-T           PIC X(11) VALUE "276 000 020".
           05 KEY-ALT-Y           PIC X(11) VALUE "277 000 021".
           05 KEY-ALT-U           PIC X(11) VALUE "278 000 022".
           05 KEY-ALT-I           PIC X(11) VALUE "279 000 023".
           05 KEY-ALT-O           PIC X(11) VALUE "280 000 024".
           05 KEY-ALT-P           PIC X(11) VALUE "281 000 025".
           05 KEY-ALT-A           PIC X(11) VALUE "286 000 030".
           05 KEY-ALT-S           PIC X(11) VALUE "287 000 031".
           05 KEY-ALT-D           PIC X(11) VALUE "288 000 032".
           05 KEY-ALT-F           PIC X(11) VALUE "289 000 033".
           05 KEY-ALT-G           PIC X(11) VALUE "290 000 034".
           05 KEY-ALT-H           PIC X(11) VALUE "291 000 035".
           05 KEY-ALT-J           PIC X(11) VALUE "292 000 036".
           05 KEY-ALT-K           PIC X(11) VALUE "293 000 037".
           05 KEY-ALT-L           PIC X(11) VALUE "294 000 038".
           05 KEY-ALT-Z           PIC X(11) VALUE "300 000 044".
           05 KEY-ALT-X           PIC X(11) VALUE "301 000 045".
           05 KEY-ALT-C           PIC X(11) VALUE "302 000 046".
           05 KEY-ALT-V           PIC X(11) VALUE "303 000 047".
           05 KEY-ALT-B           PIC X(11) VALUE "304 000 048".
           05 KEY-ALT-N           PIC X(11) VALUE "305 000 049".
           05 KEY-ALT-M           PIC X(11) VALUE "306 000 050".
           05 KEY-F1              PIC X(11) VALUE "315 000 059".
           05 KEY-F2              PIC X(11) VALUE "316 000 060".
           05 KEY-F3              PIC X(11) VALUE "317 000 061".
           05 KEY-F4              PIC X(11) VALUE "318 000 062".
           05 KEY-F5              PIC X(11) VALUE "319 000 063".
           05 KEY-F6              PIC X(11) VALUE "320 000 064".
           05 KEY-F7              PIC X(11) VALUE "321 000 065".
           05 KEY-F8              PIC X(11) VALUE "322 000 066".
           05 KEY-F9              PIC X(11) VALUE "323 000 067".
           05 KEY-F10             PIC X(11) VALUE "324 000 068".
           05 KEY-HOME            PIC X(11) VALUE "327 000 071".
           05 KEY-UP              PIC X(11) VALUE "328 000 072".
           05 KEY-PGUP            PIC X(11) VALUE "329 000 073".
           05 KEY-LEFT            PIC X(11) VALUE "331 000 075".
           05 KEY-RIGHT           PIC X(11) VALUE "333 000 077".
           05 KEY-END             PIC X(11) VALUE "335 000 079".
           05 KEY-DOWN            PIC X(11) VALUE "336 000 080".
           05 KEY-PGDN            PIC X(11) VALUE "337 000 081".
           05 KEY-INSERT          PIC X(11) VALUE "338 000 082".
           05 KEY-DELETE          PIC X(11) VALUE "339 000 083".
           05 KEY-SHIFT-F1        PIC X(11) VALUE "340 000 084".
           05 KEY-SHIFT-F2        PIC X(11) VALUE "341 000 085".
           05 KEY-SHIFT-F3        PIC X(11) VALUE "342 000 086".
           05 KEY-SHIFT-F4        PIC X(11) VALUE "343 000 087".
           05 KEY-SHIFT-F5        PIC X(11) VALUE "344 000 088".
           05 KEY-SHIFT-F6        PIC X(11) VALUE "345 000 089".
           05 KEY-SHIFT-F7        PIC X(11) VALUE "346 000 090".
           05 KEY-SHIFT-F8        PIC X(11) VALUE "347 000 091".
           05 KEY-SHIFT-F9        PIC X(11) VALUE "348 000 092".
           05 KEY-SHIFT-F10       PIC X(11) VALUE "349 000 093".
           05 KEY-CTRL-F1         PIC X(11) VALUE "350 000 094".
           05 KEY-CTRL-F2         PIC X(11) VALUE "351 000 095".
           05 KEY-CTRL-F3         PIC X(11) VALUE "352 000 096".
           05 KEY-CTRL-F4         PIC X(11) VALUE "353 000 097".
           05 KEY-CTRL-F5         PIC X(11) VALUE "354 000 098".
           05 KEY-CTRL-F6         PIC X(11) VALUE "355 000 099".
           05 KEY-CTRL-F7         PIC X(11) VALUE "356 000 100".
           05 KEY-CTRL-F8         PIC X(11) VALUE "357 000 101".
           05 KEY-CTRL-F9         PIC X(11) VALUE "358 000 102".
           05 KEY-CTRL-F10        PIC X(11) VALUE "359 000 103".
           05 KEY-ALT-F1          PIC X(11) VALUE "360 000 104".
           05 KEY-ALT-F2          PIC X(11) VALUE "361 000 105".
           05 KEY-ALT-F3          PIC X(11) VALUE "362 000 106".
           05 KEY-ALT-F4          PIC X(11) VALUE "363 000 107".
           05 KEY-ALT-F5          PIC X(11) VALUE "364 000 108".
           05 KEY-ALT-F6          PIC X(11) VALUE "365 000 109".
           05 KEY-ALT-F7          PIC X(11) VALUE "366 000 110".
           05 KEY-ALT-F8          PIC X(11) VALUE "367 000 111".
           05 KEY-ALT-F9          PIC X(11) VALUE "368 000 112".
           05 KEY-ALT-F10         PIC X(11) VALUE "369 000 113".
           05 KEY-CTRL-PRTSC      PIC X(11) VALUE "370 000 114".
           05 KEY-CTRL-LEFT       PIC X(11) VALUE "371 000 115".
           05 KEY-CTRL-RIGHT      PIC X(11) VALUE "372 000 116".
           05 KEY-CTRL-END        PIC X(11) VALUE "373 000 117".
           05 KEY-CTRL-PGDN       PIC X(11) VALUE "374 000 118".
           05 KEY-CTRL-HOME       PIC X(11) VALUE "375 000 119".
           05 KEY-ALT-1           PIC X(11) VALUE "376 000 120".
           05 KEY-ALT-2           PIC X(11) VALUE "377 000 121".
           05 KEY-ALT-3           PIC X(11) VALUE "378 000 122".
           05 KEY-ALT-4           PIC X(11) VALUE "379 000 123".
           05 KEY-ALT-5           PIC X(11) VALUE "380 000 124".
           05 KEY-ALT-6           PIC X(11) VALUE "381 000 125".
           05 KEY-ALT-7           PIC X(11) VALUE "382 000 126".
           05 KEY-ALT-8           PIC X(11) VALUE "383 000 127".
           05 KEY-ALT-9           PIC X(11) VALUE "384 000 128".
           05 KEY-ALT-0           PIC X(11) VALUE "385 000 129".
           05 KEY-ALT-MINUS       PIC X(11) VALUE "386 000 130".
           05 KEY-ALT-EQUAL       PIC X(11) VALUE "387 000 131".
           05 KEY-CTRL-PGUP       PIC X(11) VALUE "388 000 132".
           05 KEY-F11             PIC X(11) VALUE "389 000 133".
           05 KEY-F12             PIC X(11) VALUE "390 000 134".
           05 KEY-SHIFT-F11       PIC X(11) VALUE "391 000 135".
           05 KEY-SHIFT-F12       PIC X(11) VALUE "392 000 136".
           05 KEY-CTRL-F11        PIC X(11) VALUE "393 000 137".
           05 KEY-CTRL-F12        PIC X(11) VALUE "394 000 138".
           05 KEY-ALT-F11         PIC X(11) VALUE "395 000 139".
           05 KEY-ALT-F12         PIC X(11) VALUE "396 000 140".
           05 KEY-CTRL-UP         PIC X(11) VALUE "397 038    ".
           05 KEY-CTRL-DOWN       PIC X(11) VALUE "401 040    ".
           05 KEY-CTRL-INSERT     PIC X(11) VALUE "402 045    ".
           05 KEY-CTRL-DELETE     PIC X(11) VALUE "403 046    ".
           05 KEY-CTRL-TAB        PIC X(11) VALUE "404 047    ".
       01  REDEFINES KEYS.
           05 TECLAS OCCURS 140.
              10 SP2-KEY  PIC 9(003).
              10          PIC X(001).
              10 SP2-MF1  PIC 9(003).
              10          PIC X(001).
              10 SP2-MF2  PIC 9(003).

       LINKAGE SECTION.

       01   CHAR PIC X.

       PROCEDURE DIVISION USING CHAR.

       000-INICIO.

           ON 1
              CALL 'CWATTR' USING STATIC FIELD DISABLED CURCOLOR
              COPY CWSPPD.
              DISPLAY "CWCASE" UPON ENVIRONMENT-NAME
              ACCEPT CWCASE FROM ENVIRONMENT-VALUE
              INSPECT CWCASE CONVERTING MINUSCULAS TO MAIUSCULAS.

           DISPLAY "NEWOBJECTS" UPON ENVIRONMENT-NAME
           DISPLAY "X"          UPON ENVIRONMENT-VALUE
           MOVE X"00" TO CHAR

           CALL "CWKBBF" USING "G" SP2-CD-CTRL-FIELD-KEY
                                   SP2-CD-KEY
RDIG       IF  SP2-CD-KEY = -4
RDIG       AND SP2-CD-CTRL-FIELD-KEY > 0
RDIG       AND SP2-CD-CTRL-FIELD-KEY NOT = 500
RDIG           MOVE SP2-CD-CTRL-FIELD-KEY TO SP2-CD-KEY
RDIG           MOVE 0                     TO SP2-CD-CTRL-FIELD-KEY
RDIG       END-IF
RDIG       MOVE 1 TO VEZ
           IF   SP2-CD-KEY = SP2-KEY-CLOSE
           OR   SP2-KEY-SYS-SHUTDOWN
           OR   SP2-KEY-APP-CLOSE
                MOVE X"1B" TO CHAR
                CALL "CWKBBF" USING "s" SP2-CD-CTRL-FIELD-KEY
                                        SP2-CD-KEY
                GOBACK
           END-IF

           IF   SP2-CD-KEY = 0
           AND (SP2-CD-CTRL-FIELD-KEY = 500 OR 0)
                MOVE "1"      TO SP2-CD-WAIT-SW
RDIG            MOVE "CWMENU" TO SP2-PD-NAME
RDIG            CALL SP2   USING SP2-GET-PANEL-DEF SP2-PANEL-DEF
RDIG            MOVE SP2-KEY-INSERT   TO SP2-PD-CTRL-KEY (16)
RDIG            MOVE SP2-KEY-DELETE   TO SP2-PD-CTRL-KEY (37)
                call "CWCURS"  USING "G" CURPOS
      *         beto (1, 1) CURPOS
                move sp2-pd-curs-show to curs-show
                IF CURPOS NOT = ZEROS
                   MOVE 1 TO SP2-CD-ROW-COL-SW
                   compute SP2-CD-CURSOR-ROW = CURPOS-LIN - 1
                   compute SP2-CD-CURSOR-COL = CURPOS-COL - 1
                   move 'b'   to sp2-pd-curs-show
                else
                   MOVE 0 TO SP2-CD-ROW-COL-SW
                             SP2-CD-CURSOR-ROW
                             SP2-CD-CURSOR-COL
                END-IF
RDIG            CALL SP2   USING SP2-SET-PANEL-DEF SP2-PANEL-DEF

                MOVE LOW-VALUES TO SP2-FD-DATA
                MOVE 12         TO SP2-FD-WIDTH
                CALL "CWSPID" USING SP2-FD-ID "FInsert"
                MOVE SP2-FD-ID  TO SP2-CD-NEXT-FLD-ID
                MOVE "y"        TO SP2-FD-CURS-SHOW
SG              MOVE "b"        TO SP2-FD-CURS-SHOW
                MOVE "y"        TO SP2-FD-CURS-SKIP
                MOVE "o"        TO SP2-FD-PROG-CTRL
                MOVE 1          TO SP2-FD-PROG-LEN
                                   SP2-FD-MAX-LEN
                                   SP2-FD-INITIAL-LEN
                MOVE X"00"      TO SP2-FD-OUTPUT
      *         MOVE "h"      TO SP2-FD-OUTPUT
                IF CURPOS NOT = ZEROS
                   COMPUTE SP2-FD-ROW = CURPOS-LIN - 1
                   COMPUTE SP2-FD-COL = CURPOS-COL - 1
                   MOVE    SP2-FD-ROW TO ROW-NUMBER
                   MOVE    SP2-FD-COL TO COLUMN-NUMBER
                   MOVE    1          TO SP2-FD-PROG-LEN
                                         SP2-FD-MAX-LEN
                                         SP2-FD-INITIAL-LEN
                   MOVE    10         TO SP2-FD-HEIGHT
iso                MOVE    1          TO SP2-FD-FONT-ID
                   MOVE    CURCOLOR   TO SP2-FD-COLR
                   MOVE    "X"        TO SP2-FD-VAR-DATA(1:1)
                ELSE
                   MOVE "h"      TO SP2-FD-OUTPUT
                END-IF
                CALL SP2     USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
                IF CURPOS NOT = ZEROS
                   CALL "CWRCAS" USING SCREEN-POSITION
                                       FIELD-AREA
                                       X"0001"
                END-IF
                CALL SP2   USING SP2-GET-INPUT SP2-CONVERSE-DATA
                CALL SP2   USING SP2-DELETE-FIELD SP2-FIELD-DEF
                CALL "CWSPID" USING SP2-FD-ID "FDelete"

A               IF  SP2-CD-KEY = SP2-KEY-CTRL-FIELD
A                   MOVE SP2-CD-CTRL-FIELD-KEY TO SP2-CD-KEY
A               END-IF
                IF   SP2-CD-KEY = SP2-KEY-MOUSE
                     MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                END-IF
RDIG            MOVE SP2-KEY-ALT-EQUAL TO SP2-PD-CTRL-KEY (16)
RDIG            MOVE SP2-KEY-ALT-MINUS TO SP2-PD-CTRL-KEY (37)
RDIG            MOVE 8                 TO SP2-PD-BACKSPAC
                move curs-show to sp2-pd-curs-show
RDIG            CALL SP2   USING SP2-SET-PANEL-DEF SP2-PANEL-DEF
                IF  SP2-CD-KEY = SP2-KEY-MOUSE
                    MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                END-IF
                IF   SP2-CD-KEY = SP2-KEY-CLOSE
                OR   SP2-KEY-SYS-SHUTDOWN
                OR   SP2-KEY-APP-CLOSE
                     MOVE SP2-KEY-ESC TO SP2-CD-KEY
                     MOVE X"1B" TO CHAR
                     CALL "CWKBBF" USING "S" SP2-CD-CTRL-FIELD-KEY
                                             SP2-CD-KEY
                     GOBACK
                END-IF
           END-IF

           MOVE SP2-CD-KEY (1: 1) TO CHAR
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 140
                   IF  SP2-CD-KEY = SP2-KEY (I)
                       MOVE SP2-MF1 (I) TO X
                       MOVE X (1: 1) TO CHAR
                       IF  SP2-MF2 (I) NUMERIC
                           MOVE SP2-MF2 (I) TO X
                           MOVE 0           TO SP2-CD-KEY
                           MOVE 500         TO SP2-CD-CTRL-FIELD-KEY
                           MOVE X(1: 1)     TO SP2-CD-KEY (1: 1)
                           CALL "CWKBBF" USING "S" SP2-CD-CTRL-FIELD-KEY
                                                   SP2-CD-KEY
                       END-IF
                       MOVE 141 TO I
                   END-IF
           END-PERFORM

           INSPECT CHAR CONVERTING ACENTOS-WINDOWS TO ACENTOS-850

           IF CWCASE = 'UPP'
              INSPECT CHAR CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWRKBD.
