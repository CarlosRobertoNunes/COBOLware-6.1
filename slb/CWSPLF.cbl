       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWSPLF INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  04/07/2002.
       SECURITY.      *************************************************
                      *                                               *
                      *  Transfere arquivo para spool                 *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT RELATO ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  LOCK MODE IS EXCLUSIVE
                  FILE STATUS   IS FS-RELATO.

       DATA DIVISION.
       FILE SECTION.

       FD  RELATO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-RELATO.

       01  RELATO-REG.
           05 RELATO-TEXTO             PIC  X(001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 ANTERIOR                 PIC  X(001) VALUE SPACE.
           05 I                        PIC  9(018) VALUE 0.
           05 Y                        PIC  9(003) VALUE 0.
           05 ER-RELATO.
              10 FS-RELATO             PIC  X(002) VALUE "00".
              10 LB-RELATO             PIC  X(255) VALUE SPACES.

       COPY CWSEND.
       COPY CWIMPR.

       LINKAGE SECTION.

       COPY CWSPLF.

       PROCEDURE DIVISION USING PARAMETROS-CWSPLF.

       000-INICIO.

           IF   CWSPLF-OLDFILE = SPACES
                MOVE CWSPLF-FILE    TO LB-RELATO
           ELSE
                MOVE CWSPLF-OLDFILE TO LB-RELATO
           END-IF

           CALL "CWFILE" USING LB-RELATO LENGTH LB-RELATO
           OPEN INPUT RELATO
           IF   FS-RELATO NOT = "00"
                GOBACK
           END-IF
           MOVE CWSPLF-REPORT      TO CWIMPR-REPORT
           MOVE CWSPLF-FORM-TYPE   TO CWIMPR-FORM-TYPE
           EVALUATE TRUE
               WHEN CWSPLF-FLAG = '*'
                AND CWSPLF-WIDTH > 0
                    COMPUTE Y = CWSPLF-WIDTH + 3
                    MOVE 0   TO CWSPLF-WIDTH
               WHEN CWSPLF-FORM-TYPE = '2'
                    MOVE 083 TO Y
               WHEN CWSPLF-FORM-TYPE = '3'
                    MOVE 223 TO Y
               WHEN OTHER
                    MOVE 135 TO Y
           END-EVALUATE
           COMPUTE CWIMPR-HORIZONTAL-LENGTH = Y - 3
           MOVE CWSPLF-TITLE       TO CWIMPR-TITLE
           MOVE CWSPLF-NOTE        TO CWIMPR-NOTE
           MOVE 99                 TO CWIMPR-SIZE-PAGE
           MOVE 0                  TO I
           MOVE SPACES             TO CWIMPR-DETAIL
           PERFORM UNTIL FS-RELATO > "09"
                      OR CWIMPR-END-PRINT
                   READ RELATO
                     NOT AT END
                      EVALUATE TRUE
                          WHEN RELATO-REG = X"0A"
                           AND ANTERIOR = X"0D"
                               CONTINUE
                          WHEN RELATO-REG = X"0D" OR X"0A"
                               CALL "CWIMP2" USING PARAMETROS-CWIMPR
                               MOVE 0           TO I
                               MOVE SPACES      TO CWIMPR-DETAIL
                          WHEN I < Y
                               ADD  1           TO I
                               MOVE RELATO-REG  TO CWIMPR-DETAIL (I: 1)
                      END-EVALUATE
                      MOVE RELATO-REG TO ANTERIOR
           END-PERFORM
           IF  I NOT = 0
               CALL "CWIMP2" USING PARAMETROS-CWIMPR
           END-IF
           CLOSE RELATO
           DELETE FILE RELATO
           SET CWIMPR-CLOSE TO TRUE
           CALL "CWIMP2" USING PARAMETROS-CWIMPR.

       000-99-FIM. GOBACK.

       END PROGRAM CWSPLF.

