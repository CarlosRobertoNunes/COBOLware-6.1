      $Set CallFH"FHREDIR"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRMATA.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  30/11/1988.
       SECURITY.      *************************************************
                      *                                               *
                      *  Elimina arquivos indexados e area de indice  *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILENAME ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS FILENAME-CHAVE
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-FILENAME.

       DATA DIVISION.
       FILE SECTION.

       FD  FILENAME
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-FILENAME.

       01  FILENAME-REG.
           05 FILENAME-CHAVE.
              10 FILENAME-CODIGO   COMP-3 PIC  9(005).
           05 FILENAME-FILLER             PIC  X(001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 I                  COMP-X PIC  9(002) VALUE 0.
           05 SAVE-LABEL                PIC  X(050) VALUE SPACES.

       LINKAGE SECTION.

       01  ER-FILENAME.
           10 FS-FILENAME               PIC  X(002).
           10 LB-FILENAME               PIC  X(050).
           10 REDEFINES LB-FILENAME.
              15 BYTE OCCURS 50         PIC  X(001).

       PROCEDURE DIVISION USING ER-FILENAME.

       000-INICIO.

           MOVE LB-FILENAME TO SAVE-LABEL
           DELETE FILE FILENAME

           IF  (FS-FILENAME NOT = "00")
           AND (FS-FILENAME NOT = X"390D")
               PERFORM 100-ERRO THRU 100-99-FIM
           END-IF

           MOVE SAVE-LABEL TO LB-FILENAME.

       000-99-FIM. EXIT PROGRAM.

       100-ERRO.

           PERFORM TEST AFTER VARYING I FROM 50 BY -1
                              UNTIL BYTE (I) NOT = SPACE
                                 OR I = 0
                    CONTINUE
           END-PERFORM

           IF   I < 44
                ADD  2       TO I
                MOVE "(Del)" TO LB-FILENAME (I: )
           END-IF

           CALL "CWISAM"  USING ER-FILENAME.

       100-99-FIM. EXIT.

       END PROGRAM GRMATA.

