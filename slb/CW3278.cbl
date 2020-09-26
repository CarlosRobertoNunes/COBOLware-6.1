      $SET NoOsVS
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CW3278.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/09/2008.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula tratamento de telas nÆo-formatadas    *
                      *  para o simulador de CICS.                    *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 KEYBOARD-STATUS     PIC  9(002) COMP-X VALUE 0.
           05 UNIX                PIC  X(001) VALUE SPACE.
           05 GETKEY              pic  9(001) value 0.
           05 I                   pic  9(004) comp-5.
           05 Y                   pic  9(004) comp-5.
           05 CURPOS.
              10 CURPOS-LIN       PIC  9(002) VALUE 1.
              10 CURPOS-COL       PIC  9(002) VALUE 1.
           05 TECLA-EDIT          PIC  9(003) VALUE 0. COPY CWEDIT.
           05 KEY-STATUS               PIC  X(003) VALUE SPACES.
              88 TOOBJECT    VALUE X"320B09" X"320C00".
              88 UPDOWN      VALUE X"320B00" X"320C00" X"320B09"
                                   X"320400" X"320300" X"320900".
           05 REPINS                   PIC  X(001) VALUE "?".
              88 INSERT-OFF                        VALUE "0".
              88 INSERT-ON                         VALUE "1".
           05 CMDLINE VALUE LOW-VALUES.
              10 CA OCCURS 24 PIC X(80).
           05 CARACTER            PIC X(001) VALUE SPACES.
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER     PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER  PIC  9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER   PIC X(1920) VALUE SPACES.
              10 ATTRIBUTE-BUFFER  PIC X(1920) VALUE ALL X"02".
              10 STRING-LENGTH     PIC  9(004) COMP-X VALUE 1920.
           05 TRMID                PIC  X(004) VALUE SPACES.

       COPY DFHAID.
       LINKAGE SECTION.

       01  OPTION      PIC X.
       01     AID      PIC X.
       01  BUFFER      PIC X(1920).
       01  TRANSACTION PIC X(4).
       01  TECLA       PIC 9(002) COMP-X.
           88 CVDA-ANYKEY                          VALUE  100.
           88 cvda-CLEAR                           VALUE    1.
           88 CVDA-CLRPARTN                        VALUE  903.
           88 CVDA-ENTER                           VALUE    0.
           88 cvda-LIGHTPEN                        VALUE  417.
           88 CVDA-OPERID                          VALUE  904.
           88 CVDA-PA1                             VALUE   32.
           88 CVDA-PA2                             VALUE   33.
           88 CVDA-PA3                             VALUE   34.
           88 CVDA-PF1                             VALUE    2.
           88 CVDA-PF2                             VALUE    3.
           88 CVDA-PF3                             VALUE    4.
           88 CVDA-PF4                             VALUE    5.
           88 CVDA-PF5                             VALUE    6.
           88 CVDA-PF6                             VALUE    7.
           88 CVDA-PF7                             VALUE    8.
           88 CVDA-PF8                             VALUE    9.
           88 CVDA-PF9                             VALUE   10.
           88 CVDA-PF10                            VALUE   11.
           88 CVDA-PF11                            VALUE   12.
           88 CVDA-PF12                            VALUE   13.
           88 CVDA-PF13                            VALUE   14.
           88 CVDA-PF14                            VALUE   15.
           88 CVDA-PF15                            VALUE   16.
           88 CVDA-PF16                            VALUE   17.
           88 CVDA-PF17                            VALUE   18.
           88 CVDA-PF18                            VALUE   19.
           88 CVDA-PF19                            VALUE   20.
           88 CVDA-PF20                            VALUE   21.
           88 CVDA-PF21                            VALUE   22.
           88 CVDA-PF22                            VALUE   23.
           88 CVDA-PF23                            VALUE   24.
           88 CVDA-PF24                            VALUE   25.
           88 CVDA-TRIGGER                         VALUE  905.

       PROCEDURE DIVISION USING OPTION AID BUFFER TRANSACTION TECLA.

       000-INICIO.

           ON 1
              EXEC COBOLware System
                   OS-CODE UNIX
              END-EXEC.

           DISPLAY "TRMID" UPON ENVIRONMENT-NAME
           ACCEPT  TRMID   FROM ENVIRONMENT-VALUE
           IF OPTION = 'S' OR 's'
              IF BUFFER = LOW-VALUES
                 PERFORM 020-DISPLAY-TELA THRU 020-99-FIM
              END-IF
              PERFORM 010-GETSCREEN THRU 010-99-FIM
                      UNTIL BUFFER = LOW-VALUES
           ELSE
               MOVE LOW-VALUE TO EIBAID TECLA(1:)
               MOVE SPACES    TO TRANSACTION
               MOVE 1         TO GETKEY
               PERFORM 010-GETSCREEN THRU 010-99-FIM
                       UNTIL EIBAID NOT = LOW-VALUE
               INSPECT CMDLINE CONVERTING MINUSCULAS TO MAIUSCULAS
               INSPECT TRANSACTION CONVERTING MINUSCULAS TO MAIUSCULAS
               MOVE EIBAID     TO AID
               MOVE LOW-VALUES TO BUFFER
               MOVE 0 TO Y
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 1920
                       IF CMDLINE(I:1) NOT = LOW-VALUE
                          ADD 1 TO Y
                          MOVE CMDLINE(I:1) TO BUFFER(Y:1)
                       END-IF
               END-PERFORM
               MOVE BUFFER TO CMDLINE
               PERFORM 020-DISPLAY-TELA THRU 020-99-FIM
           END-IF.

       000-99-FIM. GOBACK.

       010-GETSCREEN.

           IF BUFFER NOT = LOW-VALUES
              MOVE 0               TO TECLA-EDIT
              MOVE BUFFER(1:1)     TO CARACTER
              MOVE BUFFER(2:)      TO CARACTER-BUFFER
              MOVE LOW-VALUE       TO CARACTER-BUFFER(1920:1)
              MOVE CARACTER-BUFFER TO BUFFER
           ELSE
              IF GETKEY = 1
                 MOVE 1 TO CURPOS-LIN
                           CURPOS-COL
                 MOVE 0 TO GETKEY
              END-IF
              COMPUTE ROW-NUMBER    = CURPOS-LIN - 1
              COMPUTE COLUMN-NUMBER = CURPOS-COL - 1
              MOVE 1 TO STRING-LENGTH
              CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                          CA(CURPOS-LIN)(CURPOS-COL:1)
                                                 X'20'
                                                 STRING-LENGTH
              DISPLAY '      ' AT 2550
              CALL "CWKBDC" USING CURPOS CARACTER TECLA-EDIT
                                  KEY-STATUS "B"
                                  REPINS
              DISPLAY 'SYSTEM' AT 2550
           END-IF
           IF OPTION = 'G' OR 'g'
              MOVE CMDLINE(1:4)  TO TRANSACTION
           END-IF
           EVALUATE TRUE
               WHEN EDIT-ENTER      MOVE DFHENTER      TO EIBAID
                                     SET CVDA-ENTER    TO TRUE
               WHEN EDIT-ESC        MOVE DFHCLEAR      TO EIBAID
                                     SET CVDA-CLEAR    TO TRUE
                                    MOVE LOW-VALUES    TO CMDLINE
      *        WHEN                 MOVE DFHCLRP       TO EIBAID
      *                              SET CVDA-CLRPARTN TO TRUE
      *        WHEN                 MOVE DFHPEN        TO EIBAID
      *                              SET CVDA-LIGHTPEN TO TRUE
      *        WHEN                 MOVE DFHOPID       TO  EIBAID
      *                              SET CVDA-OPERID   TO TRUE
      *        WHEN                 MOVE DFHMSRE       TO EIBAID
      *        WHEN                 MOVE DFHSTRF       TO EIBAID
      *        WHEN                 MOVE DFHTRIG       TO EIBAID
      *                              SET CVDA-TRIGGER  TO TRUE
               WHEN EDIT-ALT-F1     MOVE DFHPA1        TO EIBAID
                                     SET CVDA-PA1      TO TRUE
                                    MOVE LOW-VALUES    TO CMDLINE
               WHEN EDIT-ALT-F2     MOVE DFHPA2        TO EIBAID
                                     SET CVDA-PA2      TO TRUE
                                    MOVE LOW-VALUES    TO CMDLINE
               WHEN EDIT-ALT-F3     MOVE DFHPA3        TO EIBAID
                                     SET CVDA-PA3      TO TRUE
                                    MOVE LOW-VALUES    TO CMDLINE
               WHEN EDIT-F1         MOVE DFHPF1        TO EIBAID
                                     SET CVDA-PF1      TO TRUE
               WHEN EDIT-F2         MOVE DFHPF2        TO EIBAID
                                     SET CVDA-PF2      TO TRUE
               WHEN EDIT-F3         MOVE DFHPF3        TO EIBAID
                                     SET CVDA-PF3      TO TRUE
               WHEN EDIT-F4         MOVE DFHPF4        TO EIBAID
                                     SET CVDA-PF4      TO TRUE
               WHEN EDIT-F5         MOVE DFHPF5        TO EIBAID
                                     SET CVDA-PF4      TO TRUE
               WHEN EDIT-F6         MOVE DFHPF6        TO EIBAID
                                     SET CVDA-PF6      TO TRUE
               WHEN EDIT-F7         MOVE DFHPF7        TO EIBAID
                                     SET CVDA-PF7      TO TRUE
               WHEN EDIT-F8         MOVE DFHPF8        TO EIBAID
                                     SET CVDA-PF8      TO TRUE
               WHEN EDIT-F9         MOVE DFHPF9        TO EIBAID
                                     SET CVDA-PF9      TO TRUE
               WHEN EDIT-F10        MOVE DFHPF10       TO EIBAID
                                     SET CVDA-PF10     TO TRUE
               WHEN EDIT-F11        MOVE DFHPF11       TO EIBAID
                                     SET CVDA-PF11     TO TRUE
               WHEN EDIT-F12        MOVE DFHPF12       TO EIBAID
                                     SET CVDA-PF12     TO TRUE
               WHEN EDIT-SHIFT-F3   MOVE DFHPF13       TO EIBAID
                                     SET CVDA-PF13     TO TRUE
               WHEN EDIT-SHIFT-F4   MOVE DFHPF14       TO EIBAID
                                     SET CVDA-PF14     TO TRUE
               WHEN EDIT-SHIFT-F5   MOVE DFHPF15       TO EIBAID
                                     SET CVDA-PF15     TO TRUE
               WHEN EDIT-SHIFT-F6   MOVE DFHPF16       TO EIBAID
                                     SET CVDA-PF16     TO TRUE
               WHEN EDIT-SHIFT-F7   MOVE DFHPF17       TO EIBAID
                                     SET CVDA-PF17     TO TRUE
               WHEN EDIT-SHIFT-F8   MOVE DFHPF18       TO EIBAID
                                     SET CVDA-PF18     TO TRUE
               WHEN EDIT-SHIFT-F9   MOVE DFHPF19       TO EIBAID
                                     SET CVDA-PF19     TO TRUE
               WHEN EDIT-SHIFT-F10  MOVE DFHPF20       TO EIBAID
                                     SET CVDA-PF20     TO TRUE
               WHEN EDIT-CONTROL-F1 MOVE DFHPF21       TO EIBAID
                                     SET CVDA-PF21     TO TRUE
               WHEN EDIT-CONTROL-F2 MOVE DFHPF22       TO EIBAID
                                     SET CVDA-PF22     TO TRUE
               WHEN EDIT-CONTROL-F3 MOVE DFHPF23       TO EIBAID
                                     SET CVDA-PF23     TO TRUE
               WHEN EDIT-CONTROL-F4 MOVE DFHPF24       TO EIBAID
                                     SET CVDA-PF24     TO TRUE
               WHEN EDIT-DEL
                    COMPUTE I = CURPOS-COL + ((CURPOS-LIN - 1) * 80) + 1
                    MOVE CMDLINE (I:) TO CARACTER-BUFFER
                    SUBTRACT 1 FROM I
                    MOVE CARACTER-BUFFER TO CMDLINE(I:)
               WHEN EDIT-END
                    MOVE 80 TO CURPOS-COL
                    MOVE 24 TO CURPOS-LIN
                    PERFORM UNTIL CA(CURPOS-LIN)(CURPOS-COL:1)
                              NOT = SPACE
                            SUBTRACT 1 FROM CURPOS-COL
                            IF CURPOS-COL = 0
                               MOVE 80 TO CURPOS-COL
                               SUBTRACT 1 FROM CURPOS-LIN
                               IF CURPOS-LIN = 0
                                  MOVE 1 TO CURPOS-LIN
                                  EXIT PERFORM
                               END-IF
                            END-IF
                    END-PERFORM
                    IF CA(CURPOS-LIN)(CURPOS-COL:1) NOT = SPACE
                       ADD 1 TO CURPOS-COL
                       IF CURPOS-COL > 80
                          ADD 1 TO CURPOS-LIN
                          MOVE 1 TO CURPOS-COL
                          IF CURPOS-LIN > 24
                             MOVE 1 TO CURPOS-LIN
                          END-IF
                       END-IF
                    END-IF
               WHEN EDIT-HOME
                    MOVE 1 TO CURPOS-LIN CURPOS-COL
               WHEN EDIT-CURSOR-UP
                    IF CURPOS-LIN < 2
                       MOVE 24 TO CURPOS-LIN
                    ELSE
                       SUBTRACT 1 FROM CURPOS-LIN
                    END-IF
               WHEN EDIT-CURSOR-DOWN
                    IF CURPOS-LIN > 23
                       MOVE 1 TO CURPOS-LIN
                    ELSE
                       ADD 1 TO CURPOS-LIN
                    END-IF
               WHEN EDIT-CURSOR-RIGHT
                    IF CURPOS-COL > 79
                       MOVE 1 TO CURPOS-COL
                    ELSE
                      ADD   1 TO CURPOS-COL
                    END-IF
               WHEN EDIT-CURSOR-LEFT
                 OR EDIT-BACKSPACE
                    IF CURPOS-COL < 2
                       MOVE 80 TO CURPOS-COL
                    ELSE
                       SUBTRACT 1 FROM CURPOS-COL
                    END-IF
                    IF EDIT-BACKSPACE
                       COMPUTE I = CURPOS-COL
                                 + ((CURPOS-LIN - 1) * 80) + 1
                       MOVE CMDLINE (I:) TO CARACTER-BUFFER
                       SUBTRACT 1 FROM I
                       MOVE CARACTER-BUFFER TO CMDLINE(I:)
                    END-IF
               WHEN TECLA-EDIT = 0
                    IF INSERT-ON
                       COMPUTE I = CURPOS-COL
                               + ((CURPOS-LIN - 1) * 80) - 1
                       IF I = 0
                          MOVE 1 TO I
                       END-IF
                       MOVE CMDLINE (I:) TO CARACTER-BUFFER
                       ADD 1 TO I
                       MOVE CARACTER-BUFFER TO CMDLINE(I:)
                    END-IF
                    MOVE CARACTER TO CA(CURPOS-LIN)(CURPOS-COL:1)
                    ADD 1 TO CURPOS-COL
                    IF CURPOS-COL > 80
                       ADD 1 TO CURPOS-LIN
                       MOVE 1 TO CURPOS-COL
                       IF CURPOS-LIN > 24
                          MOVE 1 TO CURPOS-LIN
                       END-IF
                    END-IF
           END-EVALUATE

           IF BUFFER(2:) = LOW-VALUES
              PERFORM 020-DISPLAY-TELA THRU 020-99-FIM.

       010-99-FIM. EXIT.

       020-DISPLAY-TELA.

           MOVE 1920       TO STRING-LENGTH
           MOVE LOW-VALUES TO SCREEN-POSITION
           MOVE CMDLINE    TO CARACTER-BUFFER
           INSPECT CARACTER-BUFFER CONVERTING LOW-VALUE TO SPACE
           CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                              CARACTER-BUFFER
                                              ATTRIBUTE-BUFFER
                                              STRING-LENGTH
           DISPLAY ' ' AT 2560 CURPOS-LIN ',' CURPOS-COL
           DISPLAY TRMID   AT 2570 WITH REVERSE-VIDEO.

       020-99-FIM. EXIT.

       END PROGRAM CW3278.
