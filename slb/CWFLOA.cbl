       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWFLOA.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/07/2011.
       SECURITY.      *************************************************
                      *                                               *
                      * Convers∆o campo de ponto flutuante <-> sring  *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 LEN                 PIC 99 VALUE 0.
           05 DEC                 PIC 9  VALUE 0.
           05 VIRGULA             PIC X  VALUE SPACE.
           05 S                   PIC X  VALUE SPACE.
           05 S-INT               PIC S99 VALUE 0.
           05 S-INT-ED            PIC +99 VALUE 0.
           05 S-DEC               PIC 99 VALUE 0.
           05 I                   PIC 99 VALUE 0.
           05 Y                   PIC 99 VALUE 0.
           05 P                   PIC 99 VALUE 0.
           05 E                   PIC 99 VALUE 0.
           05 PM                  PIC X  VALUE SPACE.
           05 EXPO                PIC 99 VALUE 0.
           05 WORK-ED             PIC X(080) VALUE SPACES.
           05 WORK-INT            PIC X(080) VALUE SPACES.
           05 WORK-DEC            PIC X(080) VALUE SPACES.
           05 LONG         COMP-X PIC 9(008).

       LINKAGE SECTION.

       COPY CWFLOA replacing ==@== by ==X(22).==.

       01  CWFLOA-EDITED          PIC X.

       PROCEDURE DIVISION USING PARAMETROS-CWFLOA CWFLOA-EDITED.

       000-INICIO.

           EVALUATE TRUE
           WHEN CWFLOA-INTO NOT = NULL
                SET  ADDRESS CWFLOA-EDITED TO CWFLOA-INTO
                MOVE CWFLOA-INTO-LENGTH    TO LONG
                MOVE 0                     TO E P LEN
                MOVE SPACES                TO WORK-ED
                                              CWFLOA-EDITED(1:LONG)
                IF CWFLOA-DECIMAL-POINT = ','
                   MOVE '.' TO VIRGULA
                ELSE
                   MOVE ',' TO VIRGULA
                END-IF
                PERFORM VARYING I FROM 1 BY 1
                          UNTIL I > LENGTH CWFLOA-NUMERIC
                   IF  CWFLOA-NUMERIC (I:1) NUMERIC
                       ADD 1 TO LEN
                       IF I > 1
                          SUBTRACT 1 FROM I
                          IF CWFLOA-NUMERIC (I:1) = CWFLOA-DECIMAL-POINT
                             COMPUTE P = LEN - 1
                          END-IF
                          ADD 1 TO I
                       END-IF
                   END-IF
                   IF CWFLOA-NUMERIC (I:1) = 'E' OR 'e'
                      MOVE I                    TO E
                      ADD  1                    TO I
                      MOVE CWFLOA-NUMERIC (I:1) TO PM
                      ADD  1                    TO I
                      MOVE CWFLOA-NUMERIC (I:2) TO EXPO
                      EXIT PERFORM
                   END-IF
                END-PERFORM
                IF E NOT = 0
                   IF PM = '-'
                      SUBTRACT EXPO FROM P
                   END-IF
                   IF PM = '+'
                      ADD      EXPO   TO P
                   END-IF
                   ADD  1 TO P
                   MOVE 0 TO Y
                   MOVE SPACE TO S
                   IF  CWFLOA-NUMERIC (1:1) = '+' OR '-'
                       MOVE CWFLOA-NUMERIC (1:1) TO S
                   END-IF
                   PERFORM VARYING I FROM 1 BY 1 UNTIL I > E
                           IF CWFLOA-NUMERIC (I:1) NUMERIC
                              ADD 1 TO Y
                              IF Y = P
                                 MOVE CWFLOA-DECIMAL-POINT
                                   TO WORK-ED (Y:1)
                                 ADD 1 TO Y
                              END-IF
                              MOVE CWFLOA-NUMERIC (I:1)
                                TO WORK-ED (Y:1)
                           END-IF
                   END-PERFORM
                   IF P NOT = 0
                      PERFORM UNTIL P = 1
                           OR WORK-ED (Y:1) NOT = '0'
                           MOVE SPACE TO WORK-ED (Y:1)
                           SUBTRACT 1 FROM Y
                      END-PERFORM
                      IF WORK-ED (Y:1) = CWFLOA-DECIMAL-POINT
                         MOVE SPACE TO WORK-ED (Y:1)
                      END-IF
                   END-IF
                   MOVE SPACES TO WORK-INT
                                  WORK-DEC
                   MOVE 0      TO S-INT S-DEC
                   PERFORM VARYING I FROM 1 BY 1
                                     UNTIL I > LENGTH WORK-ED
                                   OR WORK-ED (I:1) = SPACE
                           IF WORK-ED (I:1) NUMERIC
                              IF I < P
                                 IF WORK-ED (I:1) NOT = '0'
                                 OR S-INT > 0
                                    ADD 1 TO S-INT
                                    MOVE WORK-ED (I:1)
                                      TO WORK-INT (S-INT: 1)
                                 END-IF
                              ELSE
                                 IF I > P
                                    ADD 1 TO S-DEC
                                    MOVE WORK-ED (I:1)
                                      TO WORK-DEC (S-DEC: 1)
                                 END-IF
                              END-IF
                           END-IF
                   END-PERFORM
                   MOVE 1      TO Y
                   MOVE SPACES TO CWFLOA-EDITED(1:LONG)
                                  WORK-ED
                   COMPUTE E = S-INT / 3
                   IF E > 1
                      SUBTRACT 1 FROM E
                   END-IF
                   ADD S-INT TO E
                   MOVE E    TO LEN
                   MOVE 0    TO P
                   PERFORM UNTIL S-INT = 0
                           MOVE WORK-INT (S-INT: 1) TO WORK-ED (E:1)
                           ADD  1                   TO P
                           SUBTRACT 1             FROM E
                           IF P = 3 AND E > 1
                              AND WORK-ED (E:1) = SPACE
                              MOVE VIRGULA TO WORK-ED (E:1)
                              MOVE 0 TO P
                              SUBTRACT 1 FROM E
                           END-IF
                           SUBTRACT 1 FROM S-INT
                   END-PERFORM
                   IF WORK-ED NOT = SPACES
                      MOVE 1 TO I
                      PERFORM UNTIL (WORK-ED (I:1) NOT = SPACE)
                                AND (WORK-ED (I:1) NOT = VIRGULA)
                              ADD 1 TO I
                              SUBTRACT 1 FROM LEN
                      END-PERFORM
                      MOVE WORK-ED(I:) TO CWFLOA-EDITED (Y: LONG - Y)
                      ADD  LEN     TO Y
                   ELSE
                      MOVE '0'     TO CWFLOA-EDITED (Y: LONG - Y)
                      ADD 1 TO Y
                   END-IF
                   IF WORK-DEC NOT = SPACES
                      PERFORM UNTIL WORK-DEC (S-DEC:1) NOT = '0'
                              MOVE SPACE TO WORK-DEC (S-DEC:1)
                              SUBTRACT 1 FROM S-DEC
                      END-PERFORM
                      MOVE CWFLOA-DECIMAL-POINT TO CWFLOA-EDITED
                                                   (Y: LONG - Y)
                      ADD 1 TO Y
                      MOVE WORK-DEC             TO CWFLOA-EDITED
                                                   (Y: LONG - Y)
                      ADD S-DEC TO Y
                   END-IF
                   MOVE S TO CWFLOA-EDITED (Y:1)
                END-IF
           WHEN CWFLOA-FROM NOT = NULL
                SET  ADDRESS CWFLOA-EDITED TO CWFLOA-FROM
                MOVE CWFLOA-FROM-LENGTH    TO LONG
                MOVE 0                     TO E P
                PERFORM VARYING I FROM 1 BY 1
                          UNTIL I > LENGTH CWFLOA-NUMERIC
                             OR E > 0
                        IF CWFLOA-NUMERIC (I: 1) = CWFLOA-DECIMAL-POINT
                           MOVE I TO P
                        END-IF
                        IF CWFLOA-NUMERIC (I: 1) = 'E' OR 'e'
                           MOVE I TO E
                        END-IF
                END-PERFORM
                IF E NOT = 0
                   MOVE '+' TO S
                   MOVE SPACES TO WORK-INT
                                  WORK-DEC
                   MOVE 0      TO S-INT S-DEC DEC
                   PERFORM VARYING I FROM 1 BY 1
                                     UNTIL I > LONG
                           IF (CWFLOA-EDITED (I:1) = '+' OR '-')
                               MOVE CWFLOA-EDITED (I:1)
                                 TO S
                           END-IF
                           IF CWFLOA-EDITED (I:1) = CWFLOA-DECIMAL-POINT
                              MOVE 1 TO DEC
                           END-IF
                           IF CWFLOA-EDITED (I:1) NUMERIC
                              IF DEC = 0
                                 IF S-INT < LENGTH WORK-INT
                                    ADD 1 TO S-INT
                                    MOVE CWFLOA-EDITED (I:1)
                                      TO WORK-INT      (S-INT:1)
                                  END-IF
                              ELSE
                                 IF S-DEC < LENGTH WORK-DEC
                                    ADD 1 TO S-DEC
                                    MOVE CWFLOA-EDITED (I:1)
                                      TO WORK-DEC      (S-DEC:1)
                                 END-IF
                              END-IF
                           END-IF
                   END-PERFORM
                   MOVE ALL '0' TO CWFLOA-NUMERIC (1: E)
                   STRING S
                          WORK-INT (1: S-INT)
                          WORK-DEC (1: S-DEC) DELIMITED BY SIZE
                     INTO CWFLOA-NUMERIC
                   SUBTRACT P FROM S-INT
                   IF P > 0
                      MOVE CWFLOA-NUMERIC (P:)  TO WORK-ED
                      MOVE CWFLOA-DECIMAL-POINT TO CWFLOA-NUMERIC (P:)
                      ADD  1                    TO P
                      MOVE WORK-ED              TO CWFLOA-NUMERIC (P:)
                      SUBTRACT 1              FROM P
                   END-IF
                   MOVE 'E'   TO CWFLOA-NUMERIC (E:)
                   ADD  2     TO S-INT
                   MOVE S-INT TO S-INT-ED
                   MOVE S-INT-ED (1:) TO CWFLOA-NUMERIC (E + 1:)
                END-IF
           END-EVALUATE.

       000-99-FIM. GOBACK.

       END PROGRAM CWFLOA.
