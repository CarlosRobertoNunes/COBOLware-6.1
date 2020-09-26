      $SET CallFH"FHREDIR".
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWFSPW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  02/10/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Redireciona senhas para o FileShare          *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CWPASS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CWPASS-CHAVE
                  LOCK MODE     IS MANUAL
                  RESERVE       NO ALTERNATE AREA
                  FILE STATUS   IS FS-CWPASS.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT WORK ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS WORK-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-WORK.

       DATA DIVISION.
       FILE SECTION.

       FD  CWPASS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CWPASS.

       01  CWPASS-REG.
           05 CWPASS-CHAVE.
              10 CWPASS-TIPO                        PIC  X(002).
              10 REDEFINES CWPASS-TIPO.
                 15 CWPASS-S                        PIC  X(001).
                 15 CWPASS-SEQ               COMP-X PIC  9(002).
              10 CWPASS-NOME                        PIC  X(030).
           05 CWPASS-SIZE-PS                 COMP-X PIC  9(002).
           05 CWPASS-FATOR-PS                COMP-X PIC  9(002).
           05 CWPASS-SENHA                          PIC  X(030).
           05 CWPASS-ESQUECI-SIZE            COMP-X PIC  9(002).
           05 CWPASS-ESQUECI-FATOR           COMP-X PIC  9(002).
           05 CWPASS-ESQUECI                        PIC  X(060).

       FD  WORK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-WORK.

       01  WORK-REG.
           05 WORK-CHAVE                            PIC  X(032).
           05 WORK-DATA                             PIC  X(032).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 ER-CWPASS.
              10 FS-CWPASS                  PIC  X(002) VALUE "00".
              10 LB-CWPASS                  PIC  X(255) VALUE "cwpass".
           05 SEQ                           PIC  9(003) VALUE 0.
           05 REUSE                         PIC  9(003) VALUE 0.
           05 SAVES                         PIC  9(003) VALUE 0.
           05 ER-WORK.
              10 FS-WORK      PIC  X(002) VALUE "00".
              10 LB-WORK      PIC  X(255) VALUE "$TEMP/cwwork".

       COPY CWCONF.

       LINKAGE SECTION.

       01  CHAVE                 PIC  X(032).
       01  SENHA                 PIC  X(030).
       01  SIZE-PS        COMP-X PIC  9(002).
       01  FATOR-PS       COMP-X PIC  9(002).
       01  ESQUECI-SIZE   COMP-X PIC  9(002).
       01  ESQUECI-FATOR  COMP-X PIC  9(002).
       01  ESQUECI               PIC  X(060).

       PROCEDURE DIVISION USING CHAVE SENHA SIZE-PS FATOR-PS
                                ESQUECI-SIZE ESQUECI-FATOR ESQUECI.

       000-INICIO.

           IF  ESQUECI-SIZE(1:1) = "W"
               GO TO 180-SAVE-REUSE
           END-IF

           IF   CHAVE(1:1) = "S"
                OPEN INPUT CWPASS
                IF FS-CWPASS > "09"
                   MOVE "1" TO ESQUECI-SIZE(1:1)
                   GOBACK
                END-IF
           ELSE
                OPEN I-O CWPASS
           END-IF
           IF FS-CWPASS > "09"
              CALL "CWISAM" USING ER-CWPASS
              STOP RUN
           END-IF
           MOVE CHAVE                    TO CWPASS-CHAVE
           READ CWPASS
           IF  FS-CWPASS < "10"
               MOVE CWPASS-SENHA         TO SENHA
               MOVE CWPASS-SIZE-PS       TO SIZE-PS
               MOVE CWPASS-FATOR-PS      TO FATOR-PS
               IF   CHAVE(1:1) = "S"
                    MOVE "0"             TO ESQUECI-SIZE(1:1)
               ELSE
                    MOVE CWPASS-ESQUECI-SIZE  TO ESQUECI-SIZE
                    MOVE CWPASS-ESQUECI-FATOR TO ESQUECI-FATOR
                    MOVE CWPASS-ESQUECI       TO ESQUECI
               END-IF
           ELSE
               IF   CHAVE(1:1) = "S"
                    MOVE "1"             TO ESQUECI-SIZE(1:1)
               ELSE
                    INSPECT CHAVE CONVERTING MINUSCULAS TO MAIUSCULAS
                    MOVE CHAVE                          TO CWPASS-CHAVE
                    READ CWPASS
                    IF  FS-CWPASS < '10'
                        DELETE CWPASS RECORD
                    END-IF
                    MOVE SENHA                TO CWPASS-SENHA
                    MOVE SIZE-PS              TO CWPASS-SIZE-PS
                    MOVE FATOR-PS             TO CWPASS-FATOR-PS
                    MOVE ESQUECI-SIZE         TO CWPASS-ESQUECI-SIZE
                    MOVE ESQUECI-FATOR        TO CWPASS-ESQUECI-FATOR
                    MOVE ESQUECI              TO CWPASS-ESQUECI
                    WRITE CWPASS-REG
               END-IF
           END-IF
           CLOSE CWPASS.

       000-99-FIM. GOBACK.

       180-SAVE-REUSE.

           SET CWSQLC-OPEN  TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE "LG" TO CWCONF-REG
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF  FS-CWCONF = "00"
               IF   CWCONF-REUSE NOT NUMERIC
                    MOVE 0 TO CWCONF-REUSE
               END-IF
               MOVE CWCONF-REUSE TO REUSE
           END-IF
           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           IF   REUSE = 0
                GOBACK
           END-IF
           OPEN I-O CWPASS
           PERFORM VARYING SEQ
                      FROM 1
                        BY 1
                        UNTIL FS-CWPASS > "09"
                   MOVE "S"         TO CWPASS-S
                   MOVE SEQ         TO CWPASS-SEQ
                   MOVE CHAVE(3:)   TO CWPASS-NOME
                   READ CWPASS IGNORE LOCK
           END-PERFORM
           MOVE LENGTH OF CWPASS-SENHA TO CWPASS-SIZE-PS
           MOVE SENHA                  TO CWPASS-SENHA
           CALL "CWCODE" USING "C" CWPASS-SIZE-PS
                                   CWPASS-FATOR-PS
                                   CWPASS-SENHA
           MOVE SPACES                 TO CWPASS-ESQUECI-SIZE(1:)
                                          CWPASS-ESQUECI-FATOR(1:)
                                          CWPASS-ESQUECI
           WRITE CWPASS-REG
           IF  SEQ > REUSE
               MOVE 0 TO SAVES
               OPEN I-O WORK
               PERFORM VARYING SEQ FROM 256 BY -1
                         UNTIL SEQ = 0
                       MOVE "S"         TO CWPASS-S
                       MOVE SEQ         TO CWPASS-SEQ
                       MOVE CHAVE(3:)   TO CWPASS-NOME
                       READ CWPASS IGNORE LOCK INTO WORK-REG
                       IF  FS-CWPASS = "00"
                           ADD 1 TO SAVES
                           IF  SAVES NOT > REUSE
                               WRITE WORK-REG
                           END-IF
                           DELETE CWPASS RECORD
                       END-IF
               END-PERFORM
               MOVE LOW-VALUES TO WORK-REG
               MOVE 0          TO SEQ
               START WORK KEY NOT LESS WORK-CHAVE
               PERFORM UNTIL FS-WORK > "09"
                       READ WORK NEXT RECORD
                       IF  FS-WORK < "10"
                           ADD  1        TO SEQ
                           MOVE WORK-REG TO CWPASS-REG
                           MOVE SEQ      TO CWPASS-SEQ
                           MOVE SPACES   TO CWPASS-ESQUECI-SIZE(1:)
                                            CWPASS-ESQUECI-FATOR(1:)
                                            CWPASS-ESQUECI
                           WRITE CWPASS-REG
                       END-IF
               END-PERFORM
               CLOSE WORK
           END-IF
           CLOSE CWPASS.

       180-99-FIM. GOBACK.
       END PROGRAM CWFSPW.
