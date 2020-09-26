       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWSETF.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/08/2008.
       SECURITY.      *************************************************
                      *                                               *
                      * Posiciona cursor ou proteje/desprotege campos *
                      * para o proximo ACCEPT                         *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT OBJETOS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS OBJETOS-SEQUENCE
                  LOCK MODE     IS EXCLUSIVE
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-OBJETOS.

       DATA DIVISION.
       FILE SECTION.

       FD  OBJETOS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-OBJETOS.

       01  OBJETOS-REG.
           05 OBJETOS-SEQUENCE      COMP-X PIC  9(004).
           05 OBJETOS-DATA.
              10 OBJETOS-FIELD             PIC  X(030).
              10 OBJETOS-SUBSCRIPT         PIC  9(003).
              10 OBJETOS-OPTION            PIC  X(001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 SEQ           COMP-X PIC  9(004) VALUE 0.
           05 ER-OBJETOS.
              10 FS-OBJETOS        PIC  X(002) VALUE "00".
              10 LB-OBJETOS        PIC  X(255) VALUE "$TEMP/cwsetf$$".

       COPY CWUNIX.

       LINKAGE SECTION.

       COPY CWSETF.

       PROCEDURE DIVISION USING PARAMETROS-CWSETF.

       000-INICIO.

           ON 1
              CALL "CWUNIX" USING PARAMETROS-CWUNIX
              OPEN I-O OBJETOS.

           IF CWSETF-FIELD NOT = SPACES
              ADD  1                 TO SEQ
              MOVE SEQ               TO OBJETOS-SEQUENCE
              MOVE PARAMETROS-CWSETF TO OBJETOS-DATA
              INSPECT OBJETOS-FIELD CONVERTING MINUSCULAS
                                            TO MAIUSCULAS
              WRITE OBJETOS-REG
           ELSE
              IF CWSETF-SUBSCRIPT(1:3) NOT = '000'
                 IF  CWUNIX-GUI
                     CALL "CWSCRE" USING X"04" CWSETF-CURRENT
                 END-IF
                 GOBACK
              END-IF
              MOVE 0 TO SEQ
                        OBJETOS-SEQUENCE
              START OBJETOS KEY NOT LESS OBJETOS-SEQUENCE
              READ OBJETOS NEXT RECORD
                NOT AT END
                    MOVE OBJETOS-DATA TO PARAMETROS-CWSETF
                    DELETE OBJETOS RECORD
              END-READ
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWSETF.

