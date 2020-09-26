       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRINTS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  11/04/1997.
       SECURITY.      *************************************************
                      *  Rotina de impressao em multi-relatorios      *
                      *************************************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       77  CHECK-BEFORE PIC X(002) VALUE SPACES.
           88 SKIP-BEFORE          VALUE "ON" "on" "On" "oN".
       77  DETALHE      PIC X(221).
       77  CWIMPR       PIC X(006) VALUE "CWIMP.".
       77  I            PIC 9(003) VALUE 0.
       77  Y            PIC 9(003) VALUE 0.
       77  P            PIC 9(003) VALUE 0.

       01 TAB VALUE "123456789ABCDEF".
          05 R OCCURS 15 PIC X.

       01  CONTROLES VALUE SPACES.
           05 CONTROLE OCCURS 15.
              10 SALVA-CONTROLES PIC X(50).
              10 VEZ COMP-3   PIC 9(018).
              10 VEZ-BEFORE   PIC 9(001).
              10 SALTOS       PIC 9(003).
              10 PROGSCHAR    PIC 9(003).
              10 FLAG-BEFORE  PIC 9(003).
                 88 SALVA-BEFORE-OFF VALUE 0.
                 88 SALVA-BEFORE-ON  VALUE 1.

       COPY CWIMPR.
       COPY CWSEND.

       LINKAGE SECTION.

       01  PARAMETROS-PRINTS.
           05 PRINTS-PRINTER       PIC X(001).
           05 PRINTS-REPORT        PIC X(007).
           05 PRINTS-SIZE          PIC 9(003).
           05 PRINTS-COMMAND       PIC X(001).
              88 PRINTS-AFTER                 VALUE "A".
              88 PRINTS-BEFORE                VALUE "B".
              88 PRINTS-CLOSE                 VALUE "C".
              88 PRINTS-NULO                  VALUE " ".
           05 PRINTS-CTRL          PIC 9(003).
              88 PRINTS-PAGE                  VALUE 999.
           05 PRINTS-DETAIL.
              10 PRINTS-BYTE       PIC X(001) OCCURS 220.

       PROCEDURE DIVISION USING PARAMETROS-PRINTS.

       000-INICIO.

           IF   PARAMETROS-PRINTS (1: 9) = "CLOSE ALL"
                PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 15
                        MOVE R (Y) TO CWIMPR (6: 1)
                        SET CWIMPR-CLOSE TO TRUE
                        CALL CWIMPR USING PARAMETROS-CWIMPR
                             ON EXCEPTION
                                PERFORM 020-ERRO-CWIMPR THRU 020-99-FIM
                        END-CALL
                        CANCEL CWIMPR
                        INITIALIZE CONTROLE   (Y)
                        MOVE 1 TO VEZ         (Y)
                                  VEZ-BEFORE  (Y)
                END-PERFORM
                GOBACK
           END-IF

           ON   1
                INITIALIZE CONTROLES
                PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 15
                        MOVE 1 TO VEZ         (Y)
                                  VEZ-BEFORE  (Y)
                END-PERFORM
                DISPLAY "SKIP-BEFORE" UPON ENVIRONMENT-NAME
                ACCEPT  CHECK-BEFORE  FROM ENVIRONMENT-VALUE
                MOVE 99 TO CWIMPR-SIZE-PAGE.

           IF   PRINTS-CTRL = 0
           AND  PRINTS-NULO
           AND  PRINTS-BYTE (1) > X"20"
                MOVE 1 TO PRINTS-CTRL
                SET PRINTS-AFTER TO TRUE
           END-IF

           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y = 15
                        OR PRINTS-PRINTER = R (Y)
                   CONTINUE
           END-PERFORM
           MOVE PRINTS-PRINTER TO CWIMPR (6: 1)

           IF   PRINTS-CTRL = 0
           AND  NOT PRINTS-CLOSE
           AND  VEZ (Y) = 1
                PERFORM VARYING I FROM 1 BY 1
                            UNTIL I > 50
                               OR PRINTS-BYTE (I) = X"00" OR SPACE
                        ADD 1 TO PROGSCHAR (Y)
                        MOVE PROGSCHAR   (Y) TO P
                        MOVE PRINTS-BYTE (I) TO SALVA-CONTROLES (Y)
                                                (P: 1)
                        IF   PRINTS-BYTE (I) > X"1F"
                             MOVE 0      TO PROGSCHAR       (Y)
                             MOVE SPACES TO SALVA-CONTROLES (Y)
                             EXIT PERFORM
                        END-IF
                END-PERFORM
                IF  PROGSCHAR (Y) NOT = 0
                    GOBACK
                END-IF
           END-IF

           IF   SALVA-CONTROLES (Y) NOT = SPACES
                IF   PRINTS-CLOSE
                     MOVE SALVA-CONTROLES (Y) TO CWIMPR-DETAIL
                     CALL CWIMPR USING PARAMETROS-CWIMPR
                          ON EXCEPTION
                             PERFORM 020-ERRO-CWIMPR THRU 020-99-FIM
                     END-CALL
                     INITIALIZE CONTROLE   (Y)
                     MOVE 1 TO VEZ         (Y)
                               VEZ-BEFORE  (Y)
                END-IF
                MOVE SALVA-CONTROLES (Y) TO DETALHE
                ADD  1               TO PROGSCHAR (Y)
                MOVE PROGSCHAR (Y)   TO P
                MOVE PRINTS-DETAIL   TO DETALHE (P: )
                MOVE DETALHE         TO PRINTS-DETAIL
                MOVE SPACES          TO DETALHE
                MOVE 0               TO PROGSCHAR       (Y)
                MOVE 1               TO PROGSCHAR       (Y)
                MOVE SPACES          TO SALVA-CONTROLES (Y)
           END-IF

           MOVE PRINTS-REPORT TO CWIMPR-REPORT
           IF  PRINTS-SIZE < 81
               SET  CWIMPR-SIZE-080 TO TRUE
           ELSE
               IF  PRINTS-SIZE < 133
                   SET  CWIMPR-SIZE-132 TO TRUE
               ELSE
                   SET  CWIMPR-SIZE-220 TO TRUE
               END-IF
           END-IF

           IF   PRINTS-CLOSE
                SET CWIMPR-CLOSE TO TRUE
                CALL CWIMPR USING PARAMETROS-CWIMPR
                     ON EXCEPTION
                        PERFORM 020-ERRO-CWIMPR THRU 020-99-FIM
                END-CALL
                MOVE SPACES  TO CWIMPR-TIME-REPORT
                CANCEL CWIMPR
                GOBACK
           END-IF

           IF   PRINTS-AFTER
           AND  PRINTS-CTRL > 1
           AND  NOT PRINTS-PAGE
                PERFORM 010-SALTOS THRU 010-99-FIM
           END-IF

           IF ((PRINTS-PAGE
           AND  PRINTS-AFTER)
           OR   SALVA-BEFORE-ON (Y))
           AND  VEZ (Y) NOT = 1
                SET SALVA-BEFORE-OFF (Y) TO TRUE
                MOVE X"0C"               TO DETALHE (1: )
                MOVE PRINTS-DETAIL       TO DETALHE (2: )
           ELSE
                MOVE PRINTS-DETAIL       TO DETALHE
           END-IF
           ADD 1 TO VEZ (Y)

           IF  (PRINTS-PAGE
           AND  PRINTS-BEFORE)
                SET SALVA-BEFORE-ON (Y) TO TRUE
           END-IF

           IF   DETALHE = SPACES
           AND  PRINTS-BEFORE
           AND  VEZ-BEFORE (Y) = 1
           AND  SKIP-BEFORE
                GO TO S-BEFORE
           END-IF

           MOVE 2 TO VEZ-BEFORE (Y)

           MOVE DETALHE TO CWIMPR-DETAIL
           CALL CWIMPR USING PARAMETROS-CWIMPR
                ON EXCEPTION
                   PERFORM 020-ERRO-CWIMPR THRU 020-99-FIM
           END-CALL.

       S-BEFORE.

           IF   PRINTS-BEFORE
           AND  PRINTS-CTRL > 1
           AND  NOT PRINTS-PAGE
                PERFORM 010-SALTOS THRU 010-99-FIM
           END-IF.

       000-99-FIM. GOBACK.

       010-SALTOS.

           COMPUTE SALTOS (Y) = PRINTS-CTRL - 1
           PERFORM SALTOS (Y) TIMES
                   MOVE SPACES TO CWIMPR-DETAIL
                   CALL CWIMPR USING PARAMETROS-CWIMPR
                        ON EXCEPTION
                           PERFORM 020-ERRO-CWIMPR THRU 020-99-FIM
                   END-CALL
           END-PERFORM.

       010-99-FIM. EXIT.

       020-ERRO-CWIMPR.

           MOVE "Impossivel executar: " TO CWSEND-MSG
           MOVE CWIMPR                  TO CWSEND-MSG (22: )
           CALL "CWSEND" USING PARAMETROS-CWSEND.

       020-99-FIM. EXIT.
       END PROGRAM PRINTS.

