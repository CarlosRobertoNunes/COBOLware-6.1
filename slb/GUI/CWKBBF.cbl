       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWKBBF.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  17/08/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Armazena tecla pressionada com a CWKBST      *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"

           SELECT BUFFER ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS BUFFER-ID
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-BUFFER
                  RESERVE NO ALTERNATE AREA.

       DATA DIVISION.
       FILE SECTION.

       FD  BUFFER
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-BUFFER.

       01  BUFFER-REG.
           05 BUFFER-ID         PIC S9(4) COMP-5.
           05 CD-CTRL-FIELD-KEY PIC S9(4) COMP-5.
           05 CD-KEY            PIC S9(4) COMP-5.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 ER-BUFFER.
              10 FS-BUFFER      PIC  X(002) VALUE "00".
              10 LB-BUFFER      PIC  X(255) VALUE "cwbuff.".

       LINKAGE SECTION.

       01  FUNCAO                PIC  X(1).
       01  SP2-CD-CTRL-FIELD-KEY PIC S9(4) COMP-5.
       01  SP2-CD-KEY            PIC S9(4) COMP-5.

       PROCEDURE DIVISION USING FUNCAO
                                SP2-CD-CTRL-FIELD-KEY
                                SP2-CD-KEY.

       000-INICIO.

           ON 1 OPEN I-O BUFFER
                INITIALIZE BUFFER-REG.

           EVALUATE TRUE
               WHEN FUNCAO = "S"
                    MOVE HIGH-VALUES TO BUFFER-ID(1:)
                    START BUFFER KEY NOT GREATER BUFFER-ID
                    IF   FS-BUFFER < '10'
                         READ BUFFER PREVIOUS RECORD
                    ELSE
                         MOVE 0 TO BUFFER-ID
                    END-IF
                    ADD  1                     TO BUFFER-ID
                    MOVE SP2-CD-CTRL-FIELD-KEY TO CD-CTRL-FIELD-KEY
                    MOVE SP2-CD-KEY            TO CD-KEY
                    WRITE BUFFER-REG
               WHEN FUNCAO = "s"
                    IF   FS-BUFFER < '10'
                         WRITE BUFFER-REG
                    END-IF
               WHEN FUNCAO = "G" OR "?"
                    MOVE LOW-VALUES TO BUFFER-ID(1:)
                    START BUFFER KEY NOT LESS BUFFER-ID
                    IF   FS-BUFFER < '10'
                         READ BUFFER NEXT RECORD
                         IF   FUNCAO = "G"
                              DELETE BUFFER RECORD
                         END-IF
                    ELSE
                         INITIALIZE BUFFER-REG
                    END-IF
                    MOVE CD-CTRL-FIELD-KEY TO SP2-CD-CTRL-FIELD-KEY
                    MOVE CD-KEY            TO SP2-CD-KEY
               WHEN FUNCAO = "C"
                    CLOSE BUFFER
                    OPEN I-O BUFFER
                    INITIALIZE BUFFER-REG
           END-EVALUATE.

       000-99-FIM. GOBACK.

       END PROGRAM CWKBBF.
