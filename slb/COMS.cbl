      $SET NOOSVS RM
      $Set Remove"TAB"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    COMS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  03/01/98.
       SECURITY.      *************************************************
                      *                                               *
                      * Emulador das fun‡äes SEND/RECEIVE do COMS     *
                      *                                               *
                      *************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY UNIVALUE.

       01  AREAS-DE-TRABALHO.
           05 NOME                     PIC X(17) VALUE SPACES.
           05 USERCODE.
              10                       PIC S9(11) BINARY.
           05 DESIGNACAO               REAL.
           05 SDF                      PIC  X(017) VALUE SPACES.
           05 LIXO                     PIC  X(017) VALUE SPACES.
           05 SQLERRMC                 PIC  X(080) VALUE SPACES.
           05 I                        PIC  9(006) VALUE 0.
           05 AGENDA                   PIC  X(017) VALUE SPACES.
           05 AGENDA-A                 PIC  X(017) VALUE SPACES.
           05 LIGADA                   PIC  9(001) VALUE 0.
           05 DESIG                    PIC  9(11)  BINARY.
           05 TRAVADO                  PIC  X(002) VALUE "99".
           05 TELA                     PIC  9(011) VALUE 0.
           05 TECLA                    PIC  9(003) VALUE 0. COPY CWKEYS.
           05 X91-RESULT        COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION      COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER     COMP-X PIC  9(002) VALUE 0.

165    01  CD-ARRAY-IN.
           03 COMS-IN-PROGRAM          PIC S9(11) BINARY.
           03 COMS-IN-FUNCTION-INDEX   PIC S9(11) BINARY.
           03 COMS-IN-USERCODE         PIC S9(11) BINARY.
CEDAE *    03 COMS-IN-SECURITY-DESG    PIC S9(11) BINARY.
CEDAE *    03 COMS-IN-DATE             PIC S9(11) BINARY.
CEDAE *    03 COMS-IN-TIMESTAMP        PIC S9(11) BINARY.
           03 COMS-IN-STATION          PIC S9(11) BINARY.
           03 COMS-IN-TEXT-LENGTH      PIC S9(11) BINARY.
CEDAE *    03 COMS-IN-END-KEY          PIC S9(11) BINARY.
           03 COMS-IN-STATUS-KEY       PIC S9(11) BINARY.
           03 COMS-IN-RST-LOCATOR      PIC S9(11) BINARY.
           03 COMS-IN-CONVERSATION.
CEDAE *       05 FILLER PIC X(110).
              05 FILLER PIC X(090).

135    01  CD-ARRAY-OUT.
           03  COMS-OUT-COUNT          PIC S9(11)  BINARY.
           03  COMS-OUT-TEXT-LENGTH    PIC S9(11)  BINARY.
           03  COMS-OUT-STATUS-KEY     PIC S9(11)  BINARY.
           03  COMS-OUT-DESTIN-TABLE.
               05 COMS-OUT-DESTIN-ERRO PIC S9(11)  BINARY.
               05 COMS-OUT-DESTINATION PIC S9(11)  BINARY.
           03  COMS-OUT-CONVERSATION-AREA.
               05  FILLER PIC X(110).

       01  PARAMETEROS-ALGOL.
           05 STATE                   PIC  9(001) VALUE 1.
           05 TAMANHO                 PIC  9(004) VALUE 0.
           05 TEXTOENT                PIC X(5000) VALUE SPACES.
           05 TEXTOSAI                PIC X(5000) VALUE SPACES.

       COPY TB27.

       LINKAGE SECTION.

       01  COMS-FUNCTION          PIC  X(001).
           88 COMS-SEND     VALUE 's' 'S'.
           88 COMS-RECEIVE  VALUE 'r' 'R'.
       01  COMS-IN-OUT            PIC  X(165).
       01    COMS-WORKING           PIC X(5000).
       01    COMS-SIZE       COMP-X PIC  9(008).
       01  COMS-SDF               PIC  X(017).

       PROCEDURE DIVISION USING COMS-FUNCTION
                                COMS-IN-OUT
                                COMS-WORKING
                                COMS-SIZE
                                COMS-SDF.

       000-INICIO.

           ON  1
               DISPLAY 'USERCODE' UPON ENVIRONMENT-NAME
               ACCEPT NOME     FROM ENVIRONMENT-VALUE
               CALL "UNIGETDES" USING NOME
                                      USERCODE
                                      DESIGNACAO.

           IF  COMS-SEND
               MOVE COMS-IN-OUT TO CD-ARRAY-OUT
           ELSE
               MOVE COMS-IN-OUT TO CD-ARRAY-IN
               MOVE USERCODE    TO COMS-IN-USERCODE
           END-IF
           MOVE ZERO TO COMS-IN-STATUS-KEY
                        COMS-OUT-STATUS-KEY
           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           IF   X91-PARAMETER > 4
           AND  COMS-FUNCTION = "S"
                MOVE SPACES TO SDF
                PERFORM VARYING I
                           FROM 1 BY 1
                            UNTIL I > LENGTH SDF
                          OR COMS-SDF (I: 1) = X'00'
                        MOVE COMS-SDF(I:1) TO SDF (I: 1)
                END-PERFORM
           END-IF

           IF  SDF NOT = SPACES
           AND (COMS-SEND OR COMS-RECEIVE)
               CALL SDF USING COMS-FUNCTION
                              COMS-WORKING
                              COMS-SIZE
                    ON EXCEPTION
                       IF  COMS-SEND
                           MOVE 23           TO COMS-OUT-STATUS-KEY
                           MOVE CD-ARRAY-OUT TO COMS-IN-OUT
                       ELSE
                           MOVE 23          TO COMS-IN-STATUS-KEY
                           MOVE CD-ARRAY-IN TO COMS-IN-OUT
                       END-IF
                       GOBACK
               END-CALL
               IF  COMS-RECEIVE
                   MOVE SPACES             TO SDF
                   PERFORM 010-TECLAS    THRU 010-99-FIM
                   MOVE COMS-SIZE          TO COMS-IN-TEXT-LENGTH
               END-IF
               PERFORM 020-SQL-KEY THRU 020-99-FIM
               IF  COMS-RECEIVE
                   MOVE CD-ARRAY-IN        TO COMS-IN-OUT
               ELSE
                   MOVE CD-ARRAY-OUT       TO COMS-IN-OUT
               END-IF
               GOBACK
           END-IF

           IF   COMS-FUNCTION = "D"
                MOVE COMS-WORKING (1: 17) TO AGENDA
                GOBACK
           ELSE
                IF   COMS-OUT-STATUS-KEY NOT = 0
                     MOVE COMS-OUT-STATUS-KEY TO DESIG
                     CALL "AGEDES" USING AGENDA DESIG "N"
                END-IF
           END-IF
           IF  AGENDA NOT = SPACES
           AND COMS-WORKING (1: 2) NOT = X"277F"
               MOVE 0 TO COMS-OUT-STATUS-KEY
               IF  AGENDA NOT = AGENDA-A
               AND AGENDA-A NOT = SPACES
                   MOVE SPACES TO TEXTOSAI
                   DISPLAY SPACE ERASE EOS AT 0101
                   MOVE 0 TO LIGADA
               END-IF
               MOVE AGENDA                      TO AGENDA-A
               IF   COMS-FUNCTION = "S"
                    MOVE COMS-WORKING (1: COMS-SIZE) TO TEXTOENT
               END-IF
               CALL AGENDA                   USING PARAMETEROS-ALGOL
               MOVE TAMANHO                     TO TB27-LENGTH
               MOVE TEXTOSAI                    TO TB27-BUFFER
               EVALUATE TRUE
                   WHEN COMS-FUNCTION = "S"
                   OR   LIGADA = 0
                        MOVE "s" TO TB27-FUNCTION
                        CALL "CWTB27" USING PARAMETROS-TB27
                        PERFORM 020-SQL-KEY THRU 020-99-FIM
                        IF   COMS-FUNCTION = "R"
                        AND  LIGADA = 1
                             MOVE 1 TO COMS-IN-TEXT-LENGTH
                        END-IF
                        MOVE 1 TO LIGADA
                   WHEN COMS-FUNCTION = "R"
                        IF   LIGADA = 1
                             MOVE "R"         TO TB27-FUNCTION
                             CALL "CWTB27" USING PARAMETROS-TB27
                             IF   TB27-BUFFER (5: 3) = "FIM"
                                  MOVE 99 TO COMS-IN-STATUS-KEY
                             ELSE
                                  MOVE TB27-BUFFER
                                    TO COMS-WORKING (1: COMS-SIZE)
                                  MOVE TB27-LENGTH
                                    TO COMS-IN-TEXT-LENGTH
                             END-IF
                             PERFORM 010-TECLAS THRU 010-99-FIM
                        ELSE
                             MOVE 1 TO COMS-IN-TEXT-LENGTH
                        END-IF
                        PERFORM 020-SQL-KEY THRU 020-99-FIM
                        MOVE CD-ARRAY-IN TO COMS-IN-OUT
               END-EVALUATE
           ELSE
               MOVE COMS-WORKING (1: COMS-SIZE) TO TB27-BUFFER
               MOVE COMS-FUNCTION TO TB27-FUNCTION
               IF   COMS-FUNCTION = "S"
                    MOVE "s" TO TB27-FUNCTION
               END-IF
               CALL "CWTB27" USING PARAMETROS-TB27
CEDAE          IF   COMS-FUNCTION = "R"
                    MOVE TB27-LENGTH  TO COMS-IN-TEXT-LENGTH
                    MOVE TB27-BUFFER  TO COMS-WORKING (1: COMS-SIZE)
                    MOVE TB27-LENGTH  TO COMS-SIZE
                    PERFORM 010-TECLAS THRU 010-99-FIM
               END-IF
           END-IF

           IF  COMS-SEND
               MOVE CD-ARRAY-OUT TO COMS-IN-OUT
           ELSE
               MOVE CD-ARRAY-IN  TO COMS-IN-OUT
           END-IF.

       000-99-FIM. GOBACK.

       010-TECLAS.

           ACCEPT TECLA FROM ESCAPE KEY
           EVALUATE TRUE
               WHEN ESC
                    MOVE 99 TO COMS-IN-STATUS-KEY
                               COMS-OUT-STATUS-KEY
               WHEN ENTER-KEY OR F12
                    MOVE 92 TO COMS-IN-STATUS-KEY
                               COMS-OUT-STATUS-KEY
      *        WHEN erro de SQL tratado em 020-SQL-KEY
      *             MOVE 93 TO COMS-IN-STATUS-KEY
      *        WHEN
      *             MOVE 97 TO COMS-IN-STATUS-KEY
      *        WHEN
      *             MOVE 89 TO COMS-IN-STATUS-KEY
           END-EVALUATE.

       010-99-FIM. EXIT.

       020-SQL-KEY.

           MOVE     SPACES      TO SQLERRMC
           DISPLAY "SQLERRMC" UPON ENVIRONMENT-NAME
           ACCEPT   SQLERRMC  FROM ENVIRONMENT-VALUE

           IF SQLERRMC NOT = SPACES
              DISPLAY "COMSQLERRMC" UPON ENVIRONMENT-NAME
              DISPLAY SQLERRMC      UPON ENVIRONMENT-VALUE
              MOVE SPACES TO SQLERRMC
              DISPLAY "SQLERRMC" UPON ENVIRONMENT-NAME
              DISPLAY  SQLERRMC  UPON ENVIRONMENT-VALUE
              MOVE 93 TO COMS-IN-STATUS-KEY
                         COMS-OUT-STATUS-KEY
           END-IF.

       020-99-FIM. EXIT.
       END PROGRAM COMS.
