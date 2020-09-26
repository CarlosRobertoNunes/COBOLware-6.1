       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWFONT.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  21/10/2005.
       SECURITY.      *************************************************
                      *                                               *
                      *  Controle de fonts                            *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"

           SELECT FONTS  ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS FONTS-CHAVE
                  ALTERNATE KEY IS FONTS-ID
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-FONTS
                  RESERVE NO ALTERNATE AREA.

       DATA DIVISION.
       FILE SECTION.

       FD  FONTS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-FONTS.

       01  FONTS-REG.
           05 FONTS-CHAVE               PIC  X(047).
           05 FONTS-ID                  PIC S9(004) COMP-5.

       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       01  AREAS-DE-TRABALHO.
           05 ER-FONTS.
              10 FS-FONTS             PIC  X(002) VALUE "00".
              10 LB-FONTS             PIC  X(255) VALUE "cwfont.".
           05 FLAG-OPEN               PIC  9(001) VALUE 0.

       COPY SP2.

       LINKAGE SECTION.

       copy CWFONT.

       PROCEDURE DIVISION USING PARAMETROS-CWFONT.

       000-INICIO.

           IF   CWFONT-NAME (1: 1) = X"FF"
                IF   FLAG-OPEN = 1
                     MOVE LOW-VALUES TO FONTS-CHAVE
                     START FONTS KEY NOT LESS FONTS-CHAVE
                     PERFORM UNTIL FS-FONTS > "09"
                             READ FONTS NEXT RECORD
                             IF   FS-FONTS < "10"
                                  MOVE CWFONT-REFERENCE TO SP2-FO-ID
                                  CALL SP2   USING SP2-DELETE-FONT
                                                   SP2-FONT-DEF
                                  DELETE FONTS RECORD
                             END-IF
                     END-PERFORM
                     CLOSE FONTS
                     MOVE 0 TO FLAG-OPEN
                END-IF
           ELSE
                IF   FLAG-OPEN = 0
                     OPEN I-O FONTS
                     MOVE 1 TO FLAG-OPEN
                END-IF
                IF   CWFONT-REFERENCE NOT = 0
                     IF CWFONT-REFERENCE > 4
                        MOVE CWFONT-REFERENCE TO FONTS-ID
                        READ FONTS KEY IS FONTS-ID
                        IF FS-FONTS < '10'
                           MOVE FONTS-CHAVE TO PARAMETROS-CWFONT(1:47)
                        ELSE
                           INITIALIZE PARAMETROS-CWFONT
                        END-IF
                     END-IF
                     GOBACK
                END-IF
                MOVE PARAMETROS-CWFONT(1:47) TO FONTS-CHAVE
                READ FONTS
                IF   FS-FONTS < "10"
                     MOVE FONTS-ID TO CWFONT-REFERENCE
                ELSE
                     MOVE LOW-VALUES TO SP2-FO-DATA
                     MOVE CWFONT-NAME TO SP2-FO-NAME
                     IF   CWFONT-WIDTH NUMERIC
                     AND  CWFONT-WIDTH > 0
                          MOVE CWFONT-WIDTH TO SP2-FO-WIDTH
                     END-IF
                     IF   CWFONT-HEIGHT NUMERIC
                     AND  CWFONT-HEIGHT > 0
                          MOVE CWFONT-HEIGHT TO SP2-FO-HEIGHT
                     END-IF
                     IF   CWFONT-FIXED
                          MOVE "y" TO SP2-FO-PITCH
                     END-IF
                     IF   CWFONT-BOLD
                          MOVE "b" TO SP2-FO-WEIGHT
                     END-IF
                     IF   CWFONT-ITALIC
                          MOVE "y" TO SP2-FO-ITALIC
                     END-IF
                     IF   CWFONT-STRIKE-OUT
                          MOVE "y" TO SP2-FO-STRIKE-OUT
                     END-IF
                     IF   CWFONT-UNDERLINE
                          MOVE "y" TO SP2-FO-UNDERLINE
                     END-IF
                     CALL SP2   USING SP2-SET-FONT-DEF SP2-FONT-DEF
                     IF SP2-FO-RET-CODE = ZERO
                        MOVE SP2-FO-ID TO CWFONT-REFERENCE
                                          FONTS-ID
                        WRITE FONTS-REG
                     END-IF
                END-IF
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWFONT.
