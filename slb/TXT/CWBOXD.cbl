       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWBOXD INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/09/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Caixa de di logo simples com at‚ 10 campos   *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 TECLA                    PIC  9(003) VALUE 0. COPY CWKEYS.
           05 P                        PIC  9(002) VALUE 0.
           05 M                        PIC  9(002) VALUE 0.
           05 N                        PIC  9(002) VALUE 0.
           05 LINX                     PIC  9(002) VALUE 0.
           05 FIELDS                   PIC  9(002) VALUE 0.
           05 L-PB                PIC  9(002) VALUE 0.
           05 C-PB                PIC  9(002) VALUE 0.
           05 m-PB                PIC  9(002) VALUE 0.
           05 PBS                 PIC  9(002) VALUE 0.
           05 LARGURA                  PIC  9(002) VALUE 0.
           05 ALTURA                   PIC  9(002) VALUE 0.
           05 I                        PIC  9(002) VALUE 0.
           05 w                   PIC  9(002) VALUE 0.
           05 I-LAST              PIC  9(002) VALUE 0.
           05 Y                        PIC  9(002) VALUE 0.
           05 JANW                     PIC  9(002) VALUE 0.
           05 MAXCAP                   PIC  9(002) VALUE 0.
           05 MAXLEN                   PIC  9(002) VALUE 0.
           05 HEADER                   PIC  X(050) VALUE SPACES.
           05 S              OCCURS 10 PIC  9(002).

       01  MATRIZ.
           05 MATRIZ-E OCCURS 10.
              10 POS-BD.
                 15 LIN-BD      PIC 9(002).
                 15 COL-BD      PIC 9(002).
              10 LEN-BD         PIC 9(002).
              10 ATTR-BD.
                 15 MODE-BD            PIC  X(001).
                 15 FORE-BD            PIC  X(001).
                 15 BACK-BD            PIC  X(001).
                 15 SECURE-BD          PIC  X(001).
                 15 BLINK-B            PIC  X(001).
                 15 BZERO-BD           PIC  X(001).
                 15 EMPTY-BD           PIC  X(001).
                 15 BEEP-BD            PIC  X(001).
                 15 REVERSE-BD         PIC  X(001).
                 15 AUTO-BD            PIC  X(001).
                 15 HIGH-BD            PIC  X(001).
                 15 UPPLOW-BD          PIC  X(001).
                 15 ADVANCE-BD         PIC  X(001).
                 15 LENF-BD     PIC 9(002).
                 15 FILLER      PIC X(003).
              10 FILLER         PIC X(013).
              10 ATTR-INIT-BD   PIC X(001).
              10 COM-SINAL-BD   PIC X(001).
              10 MODO-ACCEPT-BD PIC X(001).
              10 PIC-BD         PIC X(080).
              10 DATANAME-BD    PIC X(030).
              10 DATA-BD        PIC X(080).
              10 CTRL-BD        PIC X(080).

       01  TAMANHO-MATRIZ PIC 9(008) COMP-X.

       COPY CWOBJE.

       LINKAGE SECTION.

       COPY CWBOXD.

       PROCEDURE DIVISION USING PARAMETROS-CWBOXD.

       000-INICIO.

           CANCEL "CWAKEY"
           MOVE 0 TO TAMANHO-MATRIZ
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
                   IF CWBOXD-PUSH-BUTTON (I)
                       MOVE 0 TO CWBOXD-SIZE (I)
                       perform varying w
                                  from length of CWBOXD-CAPTION(i)
                                    by -1
                               until w = 1
                               or CWBOXD-CAPTION(i) (w:1) not = space
                                  continue
                       end-perform
                       if w > m-pb
                          move w to m-pb
                       end-if
                       add 1 to pbs
                       exit perform cycle
                   END-IF
                   IF  (CWBOXD-PIC (I) NOT = SPACES)
                        PERFORM VARYING P FROM 50 BY -1
                                  UNTIL P = 1
                                     OR CWBOXD-PIC(I) (P: 1) NOT = SPACE
                                 CONTINUE
                        END-PERFORM
                        IF (CWBOXD-PIC(I) (P: 1) = "9" OR "Z")
                        AND (NOT CWBOXD-NUMERIC (I))
                            SET CWBOXD-NUMERIC (I) TO TRUE
                        END-IF
                        MOVE P TO S (I)
                   ELSE
                        MOVE 0 TO S (I)
                   END-IF
                   IF   CWBOXD-SIZE (I) NUMERIC
                   AND (CWBOXD-SIZE (I) NOT = 0)
                        IF   CWBOXD-PIC (I) = SPACES
                             MOVE CWBOXD-SIZE (I) TO S(I)
                        END-IF
                        IF   S (I) > MAXLEN
                             MOVE S (I) TO MAXLEN
                        END-IF
                        ADD  1                TO FIELDS
                        PERFORM VARYING Y FROM 30 BY -1
                                UNTIL Y = 1
                                   OR CWBOXD-CAPTION(I)(Y:1) NOT = SPACE
                                      CONTINUE
                        END-PERFORM
                        IF  Y > MAXCAP
                            MOVE Y TO MAXCAP
                        END-IF
                   END-IF
           END-PERFORM
           COMPUTE TAMANHO-MATRIZ = LENGTH OF MATRIZ-E * (FIELDS * 2)
           COMPUTE Y = CWBOXD-COLUMN + 2
           IF   CWBOXD-HEADER = SPACES
                MOVE "Di logo"  TO CWBOXD-HEADER
           END-IF
           COMPUTE LARGURA = MAXCAP + MAXLEN + 3
           COMPUTE ALTURA  = (FIELDS * 2) + 1
           IF M-PB NOT = 0
              ADD 3 TO ALTURA
           END-IF
           IF PBS NOT = 0
              IF ((M-PB + 3)* PBS) > LARGURA
                 COMPUTE LARGURA = ((M-PB + 3) * PBS)
              END-IF
           END-IF
           PERFORM VARYING I FROM LENGTH OF CWBOXD-HEADER
                               BY -1
                               UNTIL CWBOXD-HEADER (I: 1) NOT = SPACES
                   CONTINUE
           END-PERFORM
           MOVE SPACES TO HEADER
           STRING " " CWBOXD-HEADER (1: I) DELIMITED BY SIZE INTO HEADER
           ADD 2 TO I
<>         COMPUTE JANW = I + 2
           IF  LARGURA < JANW
               MOVE JANW TO LARGURA
           END-IF
           EXEC COBOLware BoxWindow (OPEN)
                LINE   CWBOXD-LINE
                COLUMN CWBOXD-COLUMN
                WIDTH LARGURA
                HEIGHT ALTURA
                COLOR-FRAME  CWBOXD-COLOR
                COLOR-BORDER CWBOXD-COLOR
           END-EXEC
      *    ADD 1 TO Y
           DISPLAY (CWBOXD-LINE, Y) HEADER WITH SIZE I
SC    *    HIGH
           INITIALIZE MATRIZ
           MOVE CWBOXD-LINE TO LINX
           MOVE 0           TO Y
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > FIELDS
                IF (CWBOXD-SIZE (I) NOT = 0)
                AND(NOT CWBOXD-PUSH-BUTTON (I))
                   ADD     2                  TO LINX
                   ADD     1                  TO Y
                   MOVE    " fbsbzebrahua"    TO ATTR-BD (Y)
SC    *            MOVE   "H"                 TO HIGH-BD (Y)
                   MOVE    LINX               TO LIN-BD  (Y)
                   COMPUTE COL-BD (Y)          = CWBOXD-COLUMN + 2
                   MOVE    MAXCAP             TO LEN-BD  (Y)
                   MOVE    "V"                TO MODE-BD (Y)
                   MOVE    CWBOXD-CAPTION (I) TO DATA-BD (Y)

                   ADD     1               TO Y
                   MOVE    " fbsbzebrahua" TO ATTR-BD (Y)
      *            MOVE    CWBOXD-SIZE (I) TO LEN-BD  (Y)
                   MOVE    S (I)           TO LEN-BD  (Y)
                                              LENF-BD (Y)
                   MOVE    LINX            TO LIN-BD  (Y)
                   COMPUTE COL-BD (Y)       = CWBOXD-COLUMN + 3 + MAXCAP
                   MOVE "$CWBOXD"          TO DATANAME-BD (Y)
                   IF   CWBOXD-SECURE (I)
                        MOVE "S"     TO SECURE-BD (Y)
                   END-IF
                   IF   CWBOXD-AUTO   (I)
                        MOVE "A"     TO AUTO-BD (Y)
                   END-IF
                   IF   CWBOXD-PROTECTED (I)
                        MOVE "F"     TO MODE-BD    (Y)
                        MOVE "H"     TO HIGH-BD    (Y)
                        MOVE "R"     TO REVERSE-BD (Y)
                   ELSE
                        MOVE "U"     TO MODE-BD (Y)
                   END-IF
                   MOVE SPACES          TO DATA-BD (Y)
                   IF  (CWBOXD-PIC (I) NOT = SPACES)
                   AND  CWBOXD-NUMERIC (I)
                        MOVE CWBOXD-PIC (I)  TO PIC-BD (Y)
                        MOVE CWBOXD-SIZE (I) TO N
                        PERFORM VARYING M FROM S(I) BY -1
                                UNTIL M = 0
                                IF   CWBOXD-PIC (I) (M: 1) = "." OR ","
                                                 OR "/"
                                     MOVE CWBOXD-PIC (I) (M: 1)
                                       TO DATA-BD (Y) (M: 1)
                                ELSE
                                     MOVE CWBOXD-DATA (I) (N: 1)
                                       TO DATA-BD (Y) (M: 1)
                                     SUBTRACT 1 FROM N
                                END-IF
                        END-PERFORM
                   ELSE
                        MOVE S (I) TO M
                        MOVE CWBOXD-DATA (I) (1: M)
                           TO DATA-BD (Y) (1: M)
                        IF   CWBOXD-NUMERIC (I)
                             MOVE "9"  TO PIC-BD (Y)
                        ELSE
                             MOVE "X"  TO PIC-BD (Y)
                        END-IF
                   END-IF
                END-IF
           END-PERFORM
           COMPUTE L-PB = LINX + 2
           COMPUTE C-PB = CWBOXD-COLUMN + 2
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
                IF CWBOXD-PUSH-BUTTON (I)
                   MOVE L-PB               TO CWOBJE-LINE
                   MOVE C-PB               TO CWOBJE-COLUMN
                   MOVE CWBOXD-KEY (I)     TO CWOBJE-KEY
                   MOVE CWBOXD-CAPTION (I) TO CWOBJE-LABEL
                   MOVE CWBOXD-PIC(i)(1:1) TO CWOBJE-TYPE
                   MOVE m-PB               TO CWOBJE-WIDTH
                   ADD  m-PB               TO C-PB
                   ADD  2                  TO C-PB
      *            SET CWOBJE-TAB-OFF TO TRUE
                   CALL "CWOBJD" USING PARAMETROS-CWOBJE
                END-IF
           END-PERFORM

           COMPUTE TAMANHO-MATRIZ = LENGTH OF MATRIZ-E * Y
           CALL "CWUSER" USING "DCWBOXD  ," MATRIZ TAMANHO-MATRIZ
           CALL "CWUSER" USING "ACWBOXD  ," MATRIZ TAMANHO-MATRIZ
           CALL "CWAKEY" USING TECLA LENGTH OF TECLA
           IF PBS NOT = 0
              SET CWOBJE-DROP TO TRUE
              CALL "CWOBJD" USING PARAMETROS-CWOBJE
              DISPLAY ' ' LINE CWBOXD-LINE CWBOXD-COLUMN
              CANCEL "CWOBJD"
           END-IF
           MOVE 0 TO Y
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > FIELDS
                IF   CWBOXD-SIZE (I) NOT = 0
                     MOVE SPACES          TO CWBOXD-DATA (I)
                     MOVE CWBOXD-SIZE (I) TO N
                     IF   CWBOXD-NUMERIC (I)
                          MOVE ZEROS TO CWBOXD-DATA (I) (1: N)
                     END-IF
                     ADD  2           TO Y
                     IF   CWBOXD-PIC (I) NOT = SPACES
                     AND  CWBOXD-NUMERIC (I)
                          MOVE ZEROS TO CWBOXD-DATA (I) (1: N)
                          PERFORM VARYING M FROM S(I) BY -1
                                  UNTIL M = 0
                                  IF   DATA-BD (Y) (M: 1) NUMERIC
                                       MOVE DATA-BD (Y) (M: 1)
                                         TO CWBOXD-DATA (I) (N: 1)
                                       SUBTRACT 1 FROM N
                                  END-IF
                          END-PERFORM
                     ELSE
                          MOVE DATA-BD (Y) TO CWBOXD-DATA (I)
                     END-IF
                     MOVE 0           TO CWBOXD-SIZE (I)
                END-IF
           END-PERFORM

           IF   ESC
                SET CWBOXD-CANCELED TO TRUE
           ELSE
                SET CWBOXD-CONTINUE TO TRUE
           END-IF

           EXEC COBOLware BOXW (CLOSE) END-EXEC
           MOVE 10     TO CWBOXD-LINE
                          CWBOXD-COLUMN
           MOVE SPACES TO CWBOXD-HEADER
           MOVE 62     TO CWBOXD-COLOR.

       000-99-FIM. GOBACK.

       END PROGRAM CWBOXD.
