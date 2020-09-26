       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CW5050.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  03/10/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Ajusta cwconf                                *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CWCONF ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS SEQUENTIAL
                  RECORD  KEY   IS CWCONF-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-CWCONF.

       DATA DIVISION.
       FILE SECTION.

       FD  CWCONF
           LABEL RECORD IS STANDARD
           RECORDING MODE IS V
           RECORD CONTAINS 32 TO 2008 CHARACTERS
           VALUE OF FILE-ID IS LB-CWCONF.

       01  CWCONF-REG.
           05 CWCONF-CHAVE.
              10 CWCONF-TIPO            PIC  X(002).
              10 CWCONF-ELEMENTO        PIC  X(030).
            05 CWCONF-RESTO             PIC X(1976).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 ER-CWCONF.
              10 FS-CWCONF        PIC  X(002) VALUE "00".
              10 LB-CWCONF        PIC  X(255) VALUE "cwconf-OLD".
              10 LB-CWCONF-I      PIC  X(255) VALUE "cwconf-OLD.idx".
              10 OK-CWCONF        PIC  X(255) VALUE "cwconf".
              10 OK-CWCONF-I      PIC  X(255) VALUE "cwconf.idx".

       COPY CWSQLC.

       01  KCO       PIC  X(030) VALUE "CHAVE".
       01  PCO       PIC  X(002) VALUE SPACE.

       PROCEDURE DIVISION.

       000-INICIO.

            CALL "CBL_RENAME_FILE" USING OK-CWCONF   LB-CWCONF
            CALL "CBL_RENAME_FILE" USING OK-CWCONF-I LB-CWCONF-I
            OPEN INPUT CWCONF
            IF FS-CWCONF > "09"
               CALL "CWISAM" USING ER-CWCONF
            ELSE
               SET CWSQLC-CREATE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               SET CWSQLC-CLOSE  TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               SET CWSQLC-UPDATE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               PERFORM UNTIL FS-CWCONF > "09"
                       MOVE SPACES TO CWCONF-REG
                       READ CWCONF NEXT RECORD
                       IF FS-CWCONF < "09"
                          SET CWSQLC-WRITE TO TRUE
                          DISPLAY CWCONF-TIPO
                          CALL "CWCONF" USING CWSQLC CWCONF-REG
                                              FS-CWCONF KCO PCO
                          IF FS-CWCONF > "09"
                             CALL "CWCONF" USING "ISAM"
                          END-IF
                       END-IF
               END-PERFORM
               SET CWSQLC-CLOSE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               CLOSE CWCONF
            END-IF.

       000-99-FIM. GOBACK.
       END PROGRAM CW5050.
