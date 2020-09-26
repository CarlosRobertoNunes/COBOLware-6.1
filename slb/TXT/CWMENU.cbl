       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMENU.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  05/12/1987.
       SECURITY.      *************************************************
                      *                                               *
                      *   Menu geral                                  *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA
                      CALL-CONVENTION 74 IS WINAPI.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT adisctrl ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  FILE STATUS   IS FS-adisctrl.

           SELECT TASKW  ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-TASKW
                  RESERVE NO ALTERNATE AREA.

       DATA DIVISION.
       FILE SECTION.

       FD  adisctrl
           VALUE OF FILE-ID IS 'adisctrl'.

       01  adisctrl-REG PIC X(1).

       FD  TASKW
           VALUE OF FILE-ID IS LB-TASKW.

       01  TASKW-REG            PIC X(2000).

       WORKING-STORAGE SECTION.
       COPY CWSHWS.
       01  AREAS-DE-TRABALHO-1.
           05 CWCONSOLE                PIC  X(004) VALUE SPACES.
           05 FS-adisctrl              PIC  X(002) VALUE '00'.
           05 function-code     comp-x pic  9(001) value 18.
           05 vc-flags          comp-5 pic  x(004) value 0.
           05 vc-timeout        comp-5 pic s9(009) value 0.
           05 vc-identifier            pic  x(007) VALUE Z'CWMENU'.
           05 BUFFER30.
              10                       PIC  X(003).
              10 BF30                  PIC  X(255) VALUE SPACES.
           05 B30                      PIC  9(003) VALUE 0.
           05 CmdShow                  PIC  9(004) COMP-5.
           05 CmdStatus                PIC  9(004) COMP-5.
           05 CmdLine                  PIC  X(255) VALUE SPACES.
           05 COBVER                   PIC  X(002) VALUE SPACE.
           05 VCDEBUG                  PIC  X(002) VALUE SPACE.
           05 JANELA-GUI.
              10                       PIC  X(008) VALUE SPACES.
              10 JANELA-T              PIC  X(247) VALUE SPACES.
           05 J                        PIC  9(003) VALUE 0.
           05 TIME8                    PIC  X(014) VALUE SPACES.
           05 LC                       PIC  X(255) VALUE SPACES.
           05 LC-I                     PIC  9(003) VALUE 0.
           05 CMDLINE-TASKW            PIC  X(255) VALUE SPACES.
           05 FROMGUI                  PIC  X(006) VALUE SPACE.
           05 ER-TASKW.
              10 FS-TASKW              PIC  X(002) VALUE "00".
              10 LB-TASKW              PIC  X(255) VALUE "cwtask".
           05 CRON                     PIC  X(003) VALUE SPACES.
           05 CWRTSW                   PIC  X(050) VALUE SPACES.
           05 LENW01                   PIC  9(008) COMP-X VALUE 2000.
           05 LENW02                   PIC  9(008) COMP-X VALUE 2000.
           05 LENW03                   PIC  9(008) COMP-X VALUE 2000.
           05 LENW04                   PIC  9(008) COMP-X VALUE 2000.
           05 LENW05                   PIC  9(008) COMP-X VALUE 2000.
           05 LENW06                   PIC  9(008) COMP-X VALUE 2000.
           05 LENW07                   PIC  9(008) COMP-X VALUE 2000.
           05 LENW08                   PIC  9(008) COMP-X VALUE 2000.
           05 LENW09                   PIC  9(008) COMP-X VALUE 2000.
           05 LENW10                   PIC  9(008) COMP-X VALUE 2000.
           05 CWCHAIN                  PIC  X(050) VALUE SPACES.
           05 CA-POSI                  PIC  9(004) VALUE 0.
           05 CA-SIZE                  PIC  9(002) VALUE 0.
           05 USUARIO-SENHA-CL         PIC  X(200) VALUE SPACES.
           05 RC                       PIC  9(001) VALUE 0.
           05 PRINTER-STATUS           PIC  9(002) COMP-X VALUE 0.
      *    05 XRX                                VALUE X"0C06220508DB".
      *       10 XRX-BYTE OCCURS 6     PIC  9(002) COMP-X.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 RUN-ID                   PIC  9(004) COMP-5 VALUE 0.
           05 HORA-A                   PIC  X(006) VALUE SPACES.
           05 PROG-LOWER               PIC  X(050) VALUE SPACES.
           05 HORA-B                   PIC  X(006) VALUE SPACES.
           05 ERRO-CALL                PIC  9(001) VALUE 0.
           05 I                        PIC  9(004) VALUE 0.
           05 Y                        PIC  9(004) VALUE 0.
           05 P                        PIC  9(004) VALUE 1.
           05 MOUSE-STATUS             PIC  9(001) VALUE 0.
           05 CFG                      PIC  9(002) VALUE 0.
           05 PAGINA                   PIC  9(004) VALUE 1.
           05 TASK                     PIC  9(006) VALUE ZERO.
           05 COMMAREAS                            VALUE ALL X"B0".
              10 COMMAREA01            PIC  X(2000).
              10 COMMAREA02            PIC  X(2000).
              10 COMMAREA03            PIC  X(2000).
              10 COMMAREA04            PIC  X(2000).
              10 COMMAREA05            PIC  X(2000).
              10 COMMAREA06            PIC  X(2000).
              10 COMMAREA07            PIC  X(2000).
              10 COMMAREA08            PIC  X(2000).
              10 COMMAREA09            PIC  X(2000).
              10 COMMAREA10            PIC  X(2000).
           05 RELATORIO                PIC  X(007) VALUE SPACES.
           05 PROGRAMA                 PIC  X(008) VALUE "CWMENU".
           05 REDEFINES PROGRAMA.
              10 JOB-FLAG              PIC  X(001).
              10 JOB-NOME              PIC  X(007).
           05 PGM                      PIC  X(007) VALUE SPACES.
           05 ERRO                     PIC  9(001) VALUE 0.
           05 JOB                      PIC  X(007) VALUE SPACES.
           05 OK                       PIC  X(006) VALUE
              X"ADACAFFACADA".
           05 PROGRAMA2                PIC  X(008) VALUE SPACES.
           05 PROXIMO                  PIC  X(007) VALUE SPACES.
           05 NOME                     PIC  X(030) VALUE "LOGON".
           05 GRUPO                    PIC  X(022) VALUE SPACES.
           05 CHECK-NIVEL              PIC  9(001) VALUE ZERO.
           05 CWLOGP                   PIC  X(001) VALUE SPACE.
           05 USUARIO                  PIC  X(030) VALUE SPACES.
           05 SISTEMA                  PIC  X(030) VALUE SPACES.
           05 SET-LOG                  PIC  X(001) VALUE "9".
           05 MODO-MENU                PIC  9(001) VALUE ZERO.
           05 CURSOR-POSITION.
              10                       PIC  9(004) COMP-X VALUE 00.
              10                       PIC  9(004) COMP-X VALUE 00.
           05 NADA                     PIC  X(001) VALUE SPACE.
           05 IMPRESSORA               PIC  X(008) VALUE SPACES.
           05 QUADRO                   PIC  9(002) VALUE 99.
           05 PARAMETRO-BIN.
              10 SIZE-BINARIO          PIC 9(002) COMP-X VALUE 0.
              10 BINARIO.
                 15 BYTE-BINARIO       PIC X(001) OCCURS 50.
           05 MOUSE-HANDLE             PIC  X(004) COMP-X VALUE 1.
           05 MOUSE-BUTTONS            PIC  9(002) COMP-X VALUE 3.
           05 MOUSE-POSITION.
              10 ROW-MOUSE             PIC  9(004) COMP-X VALUE 24.
              10 COLUMN-MOUSE          PIC  9(004) COMP-X VALUE 00.
           05 CWRUN                    PIC  X(001) VALUE '0'.
           05 CWRUN-CMD                PIC  X(255) VALUE SPACES.
           05 CWDEBUG                  PIC  X(255) VALUE SPACES.
           05 LINHA-COMANDO            PIC  X(255) VALUE SPACES.
           05 LINHA-COMANDO-UPPER      PIC  X(255) VALUE SPACES.
           05 PARAMETROS VALUE SPACES.
              10 CWPROG   PIC X(50).
              10 PARM-1   PIC X(50).
              10 PARM-2   PIC X(50).
              10 PARM-3   PIC X(50).
              10 PARM-4   PIC X(50).
              10 PARM-5   PIC X(50).
              10 PARM-6   PIC X(50).
              10 PARM-7   PIC X(50).
              10 PARM-8   PIC X(50).
              10 PARM-9   PIC X(50).
           05 REDEFINES PARAMETROS.
              10 PARM OCCURS 10 PIC X(50).
           05 MSG-3 PIC X(44)
              VALUE "Componente em desenvolvimento ou manuten‡Æo:".
           05 MSG-6 PIC X(20) VALUE "Job nÆo cadastrado: ".
           05 CWTEST                   PIC  X(255) VALUE SPACES.
           05 NEWCMD                   PIC  X(255) VALUE SPACES.
           05 TI                       PIC  9(003) VALUE 0.
           05 TN                       PIC  9(003) VALUE 0.

       01  AREAS-DE-TRABALHO-2.
           05 MSG-1 PIC X(13) VALUE "Falta mem¢ria".
           05 MSG-2 PIC X(30) VALUE "Erro na carga do m¢dulo CWGETU".

       01  MAPA.
           05 SAVE-CWLINE-OPTION    PIC  9(003).
           05 LOGON-TYPE            PIC  9(001) VALUE 0.
           05 NIVEL-ATUAL           PIC  9(001) VALUE 0.
           05 SUB-CHAR              PIC X(2000) VALUE SPACES.
           05 SUB-ATTR              PIC X(2000) VALUE SPACES.
           05 SUB-MENUS OCCURS 5.
              10 SUB-PAGINA         PIC  X(020).
              10 SUB-OPTION         PIC  9(002).
              10 SUB-POSITION       PIC  9(004).
              10 SUB-CHAR-S         PIC X(2000).
              10 SUB-ATTR-S         PIC X(2000).
           05 HELP                  PIC  9(001).
           05 SENHA                 PIC  X(031).
           05 INSTALL-FLAG            PIC 9(002) COMP-X VALUE 0.
           05 INSTALL-PARMS.
              10 INSTALL-ADDRS        PROCEDURE-POINTER.
              10 INSTALL-PRRTY        PIC 9(002) COMP-X VALUE 0.

       COPY CWMOUS.
       COPY CWSEND.
       COPY CWGETL.
       COPY CWEXEC.
       COPY CWCONF.
       COPY CWIMPR.
       COPY CWUNIX.
       COPY CONSOLE.

       SCREEN SECTION.

       01  WINDOWS.
           05 LINE 01 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 02 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 03 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 04 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 05 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 06 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 07 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 08 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 09 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 10 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 11 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 12 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 13 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 14 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 15 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 16 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 17 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 18 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 19 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 20 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 21 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 22 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 23 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 24 COLUMN 01 PIC X(80) FROM SPACES.
           05 LINE 25 COLUMN 01 PIC X(80) FROM SPACES.

       PROCEDURE DIVISION.

       INICIO.

           DISPLAY 'COBVER' UPON ENVIRONMENT-NAME
           ACCEPT   COBVER  FROM ENVIRONMENT-VALUE
           IF COBVER = 'VC'
              OPEN INPUT adisctrl
              IF FS-ADISCTRL NOT = '00'
                 CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                             PRINTER-STATUS
                 CALL "CBL_READ_DIR"   USING OLD-DIRECTORY
                                             SIZE-OLD-DIR
                 DISPLAY "COBOLWARE" UPON ENVIRONMENT-NAME
                 ACCEPT COBWARE      FROM ENVIRONMENT-VALUE
                 IF COBWARE (2:1) = ':'
                    MOVE COBWARE(1:1) TO NEW-DRIVE
                    MOVE COBWARE(3:)  TO NEW-DIRECTORY
                    CALL "PC_SET_DRIVE"  USING NEW-DRIVE
                                     RETURNING RETURN-CODE
                 ELSE
                    MOVE COBWARE      TO NEW-DIRECTORY
                 END-IF
                 INSPECT NEW-DIRECTORY CONVERTING X"20" TO X"00"
                 CALL "CBL_CHANGE_DIR" USING NEW-DIRECTORY
                                   RETURNING RETURN-CODE
                 call x"AF" using function-code ' '
                 INSPECT OLD-DIRECTORY CONVERTING X"20" TO X"00"
                 IF COBWARE (2:1) = ':'
                    CALL "PC_SET_DRIVE"  USING OLD-DRIVE
                                     RETURNING RETURN-CODE
                    MOVE SPACES TO NEW-DIRECTORY
                    STRING '\' DELIMITED BY SIZE
                           OLD-DIRECTORY DELIMITED BY X'00'
                           INTO NEW-DIRECTORY
                    INSPECT NEW-DIRECTORY CONVERTING X"20" TO X"00"
                    CALL "CBL_CHANGE_DIR" USING NEW-DIRECTORY
                                      RETURNING RETURN-CODE
                 ELSE
                    CALL "CBL_CHANGE_DIR" USING OLD-DIRECTORY
                                      RETURNING RETURN-CODE
                 END-IF
              ELSE
                 CLOSE adisctrl
              END-IF
              DISPLAY 'VCDEBUG' UPON ENVIRONMENT-NAME
              ACCEPT   VCDEBUG  FROM ENVIRONMENT-VALUE
              IF VCDEBUG = 'ON'
                 call "CBL_DEBUGBREAK"
                 CALL "CBL_DEBUG_START" using  by value vc-flags
                                            vc-timeout
                              by reference  vc-identifier
              END-IF
           END-IF
           MOVE "cobwaret" TO MODO
           PERFORM ATALHO THRU FIM-ATALHO
KS         DISPLAY 'SKIPUSER' UPON ENVIRONMENT-NAME
KS         DISPLAY 'ON'       UPON ENVIRONMENT-VALUE

           CALL "COBWARE" USING OK "W"
                ON EXCEPTION
                   CALL "cobware.lbr/cobware" USING OK "U"
                     ON EXCEPTION
                        STOP RUN
                   END-CALL
           END-CALL
           CANCEL 'COBWARE' CANCEL 'cobware'

KS         DISPLAY 'SKIPUSER' UPON ENVIRONMENT-NAME
KS         DISPLAY SPACES     UPON ENVIRONMENT-VALUE
           DISPLAY "CWCRON" UPON ENVIRONMENT-NAME
           ACCEPT  CRON   FROM ENVIRONMENT-VALUE
           INSPECT CRON
                   CONVERTING MINUSCULAS TO MAIUSCULAS

           DISPLAY "CWDEBUG" UPON ENVIRONMENT-NAME
           ACCEPT   CWDEBUG  FROM ENVIRONMENT-VALUE
           IF  CWDEBUG NOT = SPACES
               DISPLAY CWDEBUG UPON COMMAND-LINE
           END-IF
      *    PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
      *            ADD 20 TO XRX-BYTE (I)
      *    END-PERFORM
      *    IF   OK NOT = XRX
      *         STOP RUN
      *    END-IF
           DISPLAY "CWSTOP" UPON ENVIRONMENT-NAME
           ACCEPT CWRTSW FROM ENVIRONMENT-VALUE
           INSPECT CWRTSW CONVERTING MINUSCULAS TO MAIUSCULAS
           IF CWRTSW NOT = "OFF"
              SET INSTALL-ADDRS TO ENTRY "CWSTOP"
              CALL "CBL_EXIT_PROC" USING INSTALL-FLAG
                                         INSTALL-PARMS
           END-IF
           DISPLAY "CWRTSW" UPON ENVIRONMENT-NAME
           MOVE 'CWRTSW' TO CWRTSW
           ACCEPT CWRTSW FROM ENVIRONMENT-VALUE
           INSPECT CWRTSW CONVERTING MINUSCULAS TO MAIUSCULAS
           IF (CWRTSW NOT = "OFF")
           AND  (CRON NOT = 'ON')
              SET INSTALL-ADDRS TO ENTRY CWRTSW
              CALL "CBL_ERROR_PROC" USING INSTALL-FLAG
                                          INSTALL-PARMS
           END-IF
           INITIALIZE MAPA
           DISPLAY " " AT 2480
txt   *    CALL "CWTEXT" USING AREAS-DE-TRABALHO-2
txt   *              LENGTH OF AREAS-DE-TRABALHO-2
           CANCEL "CWTASK"
           ACCEPT LINHA-COMANDO FROM COMMAND-LINE
           MOVE LINHA-COMANDO TO LINHA-COMANDO-UPPER
           INSPECT LINHA-COMANDO-UPPER CONVERTING MINUSCULAS
                                               TO MAIUSCULAS
           MOVE 0 TO CWRUN TI TN
           MOVE SPACES TO FROMGUI CWTEST NEWCMD
           PERFORM VARYING I FROM 1 BY 1
                     UNTIL I > (LENGTH OF LINHA-COMANDO - 4)
                  IF   LINHA-COMANDO-UPPER (I:8) = '/NOFRAME'
                       ADD 9  TO I
                       MOVE 1 TO NOFRAME
                       DISPLAY 'CWNOFRAME' UPON ENVIRONMENT-NAME
                       DISPLAY 'ON'        UPON ENVIRONMENT-VALUE
                  END-IF
                  IF   LINHA-COMANDO (I: 3) = "/R:" OR "/r:"
                       MOVE 1 TO CWRUN
                       ADD  3 TO I
                       MOVE LINHA-COMANDO (I: ) TO CWRUN-CMD
                  END-IF
                  IF   LINHA-COMANDO (I: 3) = "/T:"
                       ADD  3 TO I
                       MOVE LINHA-COMANDO (I: 6) TO FROMGUI
                       ADD 6 TO I
                       DISPLAY 'CWFROMGUI' UPON ENVIRONMENT-NAME
                       DISPLAY FROMGUI     UPON ENVIRONMENT-VALUE
                  END-IF
           END-PERFORM
           IF FROMGUI = SPACES
              CALL "CWTASK" USING "1" TASK
           END-IF
           PERFORM VARYING I FROM 1 BY 1
                     UNTIL I > (LENGTH OF LINHA-COMANDO - 4)
                  IF LINHA-COMANDO-UPPER (I:3) = '/U:'
                                              OR '/S:' OR '/P:'
                                              OR '/D:' OR '/C:'
                  OR LINHA-COMANDO-UPPER (I:8) = '/NOFRAME'
                     MOVE SPACES TO BUFFER30
                     MOVE 1      TO B30
                     DISPLAY 'CWLIXO' UPON ENVIRONMENT-NAME
                     IF LINHA-COMANDO-UPPER (I:3) = '/U:'
                        DISPLAY 'CWUSERNAME' UPON ENVIRONMENT-NAME
                     END-IF
                     IF LINHA-COMANDO-UPPER (I:3) = '/S:' OR '/P:'
                        DISPLAY 'CWPASSWORD' UPON ENVIRONMENT-NAME
                     END-IF
                     PERFORM UNTIL I > (LENGTH OF LINHA-COMANDO - 4)
                                OR LINHA-COMANDO-UPPER (I:1) = SPACE
                              ADD 1 TO TI
                             MOVE LINHA-COMANDO (I:1)
                               TO CWTEST (TI:1)
                                  BUFFER30 (B30:1)
                             ADD 1 TO I B30
                     END-PERFORM
                     ADD 1 TO TI
                     DISPLAY BF30 UPON ENVIRONMENT-VALUE
                  ELSE
                     ADD 1 TO TN
                     MOVE LINHA-COMANDO (I:1)
                       TO NEWCMD (TN:1)
                  END-IF
           END-PERFORM
           IF (CWTEST NOT = SPACES)
           AND FROMGUI = SPACES
              DISPLAY 'CWTEST' UPON ENVIRONMENT-NAME
              DISPLAY  CWTEST  UPON ENVIRONMENT-VALUE
              MOVE NEWCMD TO LINHA-COMANDO LINHA-COMANDO-UPPER
              DISPLAY NEWCMD UPON COMMAND-LINE
              INSPECT LINHA-COMANDO-UPPER CONVERTING MINUSCULAS
                                                  TO MAIUSCULAS
           END-IF
           IF FROMGUI NOT = SPACES
              MOVE FROMGUI  TO TASK
           END-IF

           CALL "CWUNIX" USING PARAMETROS-CWUNIX
           IF FROMGUI = SPACES
              DISPLAY WINDOWS
           ELSE
              IF  NOFRAME = 0
                  DISPLAY " " AT 2480
              END-IF
           END-IF
           IF   CWRUN = 1
                DISPLAY 'CWRUN'   UPON ENVIRONMENT-NAME
                DISPLAY CWRUN     UPON ENVIRONMENT-VALUE
                MOVE CWRUN-CMD TO LINHA-COMANDO LC
                INSPECT LC CONVERTING MINUSCULAS TO MAIUSCULAS
                MOVE 0         TO Y
                PERFORM VARYING I FROM 1 BY 1 UNTIL I = 255
                                                 OR P = 11
                        IF   LC(I:3) = "/U:" OR "/S:" OR "/D:" OR "/C:"
                        OR   LC (1: 8) = '/NOFRAME'
                             PERFORM UNTIL LC (I:1) = SPACE
                                        OR I > 254
                                     ADD 1 TO LC-I
                                     MOVE LINHA-COMANDO (I: 1)
                                       TO USUARIO-SENHA-CL (LC-I: 1)
                                     MOVE SPACES
                                        TO LINHA-COMANDO (I: 1)
                                     ADD 1 to I
                              END-PERFORM
                              ADD 1 TO LC-I
                        END-IF
                        IF  (PARM (P) NOT = SPACES)
                        AND  LINHA-COMANDO (I: 1) = SPACE
                             ADD  1 TO P
                             MOVE 0 TO Y
                        END-IF
                        IF   LINHA-COMANDO (I: 1) NOT = SPACE
                        AND  P < 11
                             IF   Y < 50
                                  ADD  1 TO Y
                                  MOVE LINHA-COMANDO (I: 1)
                                    TO PARM (P) (Y: 1)
                             END-IF
                        END-IF
                END-PERFORM
                INSPECT PARAMETROS (51: ) CONVERTING "_" TO SPACE
                IF    USUARIO-SENHA-CL NOT = SPACES
                      DISPLAY USUARIO-SENHA-CL UPON COMMAND-LINE
                END-IF
                MOVE LINHA-COMANDO TO LINHA-COMANDO-UPPER
                PERFORM VARYING I FROM 1 BY 1
                                UNTIL LINHA-COMANDO-UPPER (I: 1) = SPACE
                        CONTINUE
                END-PERFORM
                IF LINHA-COMANDO-UPPER (I: ) NOT = SPACES
cedae              PERFORM VARYING I FROM I BY 1
                           UNTIL LINHA-COMANDO-UPPER (I: 1) NOT = SPACE
                           CONTINUE
                   END-PERFORM
                END-IF
                move LINHA-COMANDO-UPPER (I: ) TO LINHA-COMANDO
                inspect LINHA-COMANDO converting '0' to x'01'
                CALL 'CWPACK' USING LINHA-COMANDO
                             LENGTH LINHA-COMANDO
                inspect LINHA-COMANDO converting x'01' to '0'
           END-IF

           DISPLAY LINHA-COMANDO UPON COMMAND-LINE.

       RELOAD-CWMEN0.

           CALL "CWGETL" USING PARAMETROS-CWGETL
           IF FROMGUI = TASK
              MOVE SPACES TO CWGETL-LOGIN
                             CWGETL-LOGOUT
                             CWGETL-CALLIN
                             CWGETL-CALLOUT
           END-IF
           MOVE CWGETL-LOG        TO SET-LOG
           CALL "CWGETU" USING NOME TASK PROGRAMA SET-LOG
             ON OVERFLOW
                 DISPLAY MSG-2
                 STOP RUN
           END-CALL
           CALL "CWATCH"
           IF   MOUSE-STATUS = 1
           AND  CWGETL-MOUSE = 0
                MOVE 0 TO MOUSE-STATUS
                CALL "CBL_HIDE_MOUSE" USING MOUSE-HANDLE
                     ON OVERFLOW
                        CONTINUE
                END-CALL
                CALL "CBL_TERM_MOUSE" USING MOUSE-HANDLE
                     ON OVERFLOW
                        CONTINUE
                END-CALL
           END-IF
           MOVE SPACES TO CMDLINE-TASKW
           PERFORM 250-LIGA-MOUSE THRU 250-99-FIM
           IF  FROMGUI NOT = SPACES
               DISPLAY "TASKTIME" UPON ENVIRONMENT-NAME
               ACCEPT  TIME8      FROM ENVIRONMENT-VALUE
               MOVE SPACES  TO LB-TASKW
               STRING '$TEMP/cwtaskE.' TIME8 DELIMITED BY SIZE
                       INTO LB-TASKW
               OPEN INPUT TASKW
               IF FS-TASKW NOT = '00'
                  CALL 'CWISAM' USING ER-TASKW
                  STOP RUN
               END-IF
               READ TASKW
               IF FS-TASKW NOT = '00'
                  CALL 'CWISAM' USING ER-TASKW
                  STOP RUN
               ELSE
                  DISPLAY 'CWAPPLICATION'   UPON ENVIRONMENT-NAME
                  READ TASKW
                  DISPLAY TASKW-REG         UPON ENVIRONMENT-VALUE
                  READ TASKW INTO CMDLINE-TASKW
                  READ TASKW INTO COMMAREA01
                  READ TASKW INTO COMMAREA02
                  READ TASKW INTO COMMAREA03
                  READ TASKW INTO COMMAREA04
                  READ TASKW INTO COMMAREA05
                  READ TASKW INTO COMMAREA06
                  READ TASKW INTO COMMAREA07
                  READ TASKW INTO COMMAREA08
                  READ TASKW INTO COMMAREA09
                  READ TASKW INTO COMMAREA10
                  CLOSE TASKW
                  DELETE FILE TASKW
                  MOVE SPACES  TO LB-TASKW
                  STRING '$TEMP/cwtaskR.' TIME8 DELIMITED BY SIZE
                       INTO LB-TASKW
                  OPEN OUTPUT TASKW
               END-IF
           END-IF
           CALL "CWMEN0" USING
                         NOME     CHECK-NIVEL TASK GRUPO MAPA
                         PROGRAMA PAGINA      MODO-MENU  CFG
                         IMPRESSORA QUADRO    RELATORIO COMMAREA01
                                                        COMMAREA02
                                                        COMMAREA03
                                                        COMMAREA04
                                                        COMMAREA05
                                                        COMMAREA06
                                                        COMMAREA07
                                                        COMMAREA08
                                                        COMMAREA09
                                                        COMMAREA10
                         CWRUN CWPROG
                ON OVERFLOW
                   DISPLAY (1, 1) ERASE MSG-1
                   ACCEPT SET-LOG AT 0000
                STOP RUN
              END-CALL
           CANCEL "CWMEN0".
       roda.
           IF   CWRUN = 0
                MOVE PROGRAMA TO CWPROG
           END-IF

           PERFORM 220-REMOVE-OVERLAYS THRU 220-99-FIM

           IF  (PROGRAMA = "CWMENU" OR "CWMEN4")
           AND  CWRUN = 0
                GO TO RELOAD-CWMEN0
           END-IF

           IF   PROGRAMA(1:5) = "CWMEN"
                MOVE SPACES TO RELATORIO
           END-IF

           IF   PROGRAMA = "CWMENG"
                MOVE 0 TO MOUSE-STATUS
                CALL "CBL_HIDE_MOUSE" USING MOUSE-HANDLE
                     ON OVERFLOW
                        CONTINUE
                END-CALL
                CALL "CBL_TERM_MOUSE" USING MOUSE-HANDLE
                     ON OVERFLOW
                        CONTINUE
                END-CALL
           END-IF
           IF   PROGRAMA = "CWMEN8"
                CALL "CWMEN8" USING CHECK-NIVEL NOME
                     ON OVERFLOW
                        MOVE SPACES TO CWSEND-MSG
                        STRING MSG-3 PROGRAMA DELIMITED BY SIZE
                                INTO CWSEND-MSG
                        PERFORM 210-PAUSA THRU 210-99-FIM
                     END-CALL
                CANCEL "CWMEN8"
           ELSE
                IF   JOB-FLAG = "/"
                OR  (RELATORIO (1:1) = '&' AND (NOT CWUNIX-ON))
                     IF   JOB-FLAG = "/"
                          MOVE JOB-NOME TO JOB
                          PERFORM 240-EXEC-JOB THRU 240-99-FIM
                     ELSE
                          PERFORM 242-EXEC-WIN THRU 242-99-FIM
                     END-IF
                ELSE
                     IF   CWGETL-CALLIN NOT = SPACES
                     AND (PROGRAMA (1: 5) NOT = "CWMEN")
                          CALL CWGETL-CALLIN USING COMMAREA01
                                                   COMMAREA02
                                                   COMMAREA03
                                                   COMMAREA04
                                                   COMMAREA05
                                                   COMMAREA06
                                                   COMMAREA07
                                                   COMMAREA08
                                                   COMMAREA09
                                                   COMMAREA10
                           ON OVERFLOW
                              MOVE SPACES TO CWSEND-MSG
                              STRING         MSG-3 DELIMITED BY SIZE
                                     CWGETL-CALLIN DELIMITED BY SPACE
                                INTO CWSEND-MSG
                              PERFORM 360-ABEND THRU 360-99-FIM
                          END-CALL
                     END-IF
                     IF   PROGRAMA = "CWREL2" OR "XSDRUN"
                     OR   PROGRAMA (1:4) = "CICS"
                          IF   PROGRAMA (1:4) = "CICS"
                               CALL "CICS" USING PROGRAMA(5:4)
                               CANCEL "CICS"
                               CANCEL "CWCICS"
                          ELSE
                              CALL PROGRAMA USING RELATORIO COMMAREA01
                              CANCEL PROGRAMA
                          END-IF
                     ELSE
                          PERFORM VARYING P FROM 50 BY -1
                                  UNTIL CWPROG (P: 1) NOT = SPACE
                                  CONTINUE
                          END-PERFORM
                          DISPLAY 'CONSOLE' UPON ENVIRONMENT-NAME
                          ACCEPT  CWCONSOLE FROM ENVIRONMENT-VALUE
                          INSPECT CWCONSOLE CONVERTING MINUSCULAS
                                                    TO MAIUSCULAS
                          IF CWCONSOLE = 'ON'
                          AND (CWPROG (1: P) NOT = 'CONSOLE')
                             MOVE CWPROG (1: P) TO CONSOLE-PROGRAM
                             MOVE 'STARTED'     TO CONSOLE-MSG
                             DISPLAY 'CWSTARTED' UPON ENVIRONMENT-NAME
                             DISPLAY CONSOLE-PROGRAM
                                     UPON ENVIRONMENT-VALUE
                             CALL "CONSOLE"  USING PARAMETROS-CONSOLE
                          END-IF
cedae                     DISPLAY LINHA-COMANDO UPON COMMAND-LINE
                          MOVE 0             TO ERRO-CALL
                          MOVE CWPROG (1: P) TO PROG-LOWER
                          INSPECT PROG-LOWER CONVERTING
                                  MAIUSCULAS TO MINUSCULAS
                          IF   PARAMETROS (51: ) = SPACES
                               IF (FROMGUI NOT = SPACES)
                               AND (CMDLINE-TASKW NOT = SPACES)
                               AND LINHA-COMANDO = SPACES
                                   DISPLAY CMDLINE-TASKW
                                      UPON COMMAND-LINE
                               END-IF
                               CALL CWPROG (1: P) USING COMMAREA01
                                                        COMMAREA02
                                                        COMMAREA03
                                                        COMMAREA04
                                                        COMMAREA05
                                                        COMMAREA06
                                                        COMMAREA07
                                                        COMMAREA08
                                                        COMMAREA09
                                                        COMMAREA10
                                 ON OVERFLOW
                                    CALL PROG-LOWER USING COMMAREA01
                                                          COMMAREA02
                                                          COMMAREA03
                                                          COMMAREA04
                                                          COMMAREA05
                                                          COMMAREA06
                                                          COMMAREA07
                                                          COMMAREA08
                                                          COMMAREA09
                                                          COMMAREA10
                                         ON OVERFLOW
                                            MOVE 1 TO ERRO-CALL
                                           IF CRON = 'ON'
                                              CALL CWPROG (1: P)
                                           END-IF
                                         END-CALL
                                 END-CALL
                          ELSE
                               CALL CWPROG (1: P)
                                   USING PARM-1 PARM-2 PARM-3 PARM-4
                                         PARM-5 PARM-6 PARM-7 PARM-8
                                         PARM-9
                                    ON OVERFLOW
                                       CALL PROG-LOWER
                                           USING PARM-1 PARM-2 PARM-3
                                                 PARM-4 PARM-5 PARM-6
                                                 PARM-7 PARM-8 PARM-9
                                       ON OVERFLOW
                                          MOVE 1 TO ERRO-CALL
                                           IF CRON = 'ON'
                                              CALL CWPROG (1: P)
                                           END-IF
                                       END-CALL
                               END-CALL
                          END-IF
                          IF CWCONSOLE = 'ON'
                          AND (CWPROG (1: P) NOT = 'CONSOLE')
                             MOVE CWPROG (1: P) TO CONSOLE-PROGRAM
                             MOVE 'ENDED'       TO CONSOLE-MSG
                             CALL "CONSOLE"  USING PARAMETROS-CONSOLE
                          END-IF
                          DISPLAY "CWCANCELED" UPON ENVIRONMENT-NAME
                          MOVE    CWPROG (1: P) TO CWSEND-MSG
                          DISPLAY CWSEND-MSG    UPON ENVIRONMENT-VALUE
                          CALL "CWSQLC" USING "K*"
                          CANCEL CWPROG (1: P)
                          CANCEL PROG-LOWER
                          IF  ERRO-CALL = 1
                              MOVE SPACES TO CWSEND-MSG
                              STRING MSG-3 DELIMITED BY SIZE
                                  CWPROG (1: P) DELIMITED BY SPACE
                              INTO CWSEND-MSG
                              PERFORM 210-PAUSA THRU 210-99-FIM
                          END-IF
                     END-IF
                     IF   CWGETL-CALLOUT NOT = SPACES
                     AND (PROGRAMA (1: 5) NOT = "CWMEN")
                          CALL CWGETL-CALLOUT USING COMMAREA01
                                                    COMMAREA02
                                                    COMMAREA03
                                                    COMMAREA04
                                                    COMMAREA05
                                                    COMMAREA06
                                                    COMMAREA07
                                                    COMMAREA08
                                                    COMMAREA09
                                                    COMMAREA10
                           ON OVERFLOW
                              MOVE SPACES TO CWSEND-MSG
                              STRING MSG-3         DELIMITED BY SIZE
                                     CWGETL-CALLOUT DELIMITED BY SPACE
                              INTO CWSEND-MSG
                              PERFORM 360-ABEND THRU 360-99-FIM
                          END-CALL
                     END-IF
                     MOVE SPACES TO RELATORIO
                END-IF
           END-IF
           DISPLAY "CWCHAIN" UPON ENVIRONMENT-NAME
           MOVE SPACES TO CWCHAIN
           ACCEPT   CWCHAIN  FROM ENVIRONMENT-VALUE
           IF   CWCHAIN NOT = SPACES
                MOVE CWCHAIN TO PROGRAMA CWPROG
                MOVE SPACES  TO COMMAREAS
                CALL "CWLOCK" USING "M" NOME TASK PROGRAMA
                MOVE 2000 TO LENW01
                             LENW02
                             LENW03
                             LENW04
                             LENW05
                             LENW06
                             LENW07
                             LENW08
                             LENW09
                             LENW10
                CALL "CWCHAIN" USING BY VALUE "GET"
                     BY REFERENCE COMMAREA01 BY REFERENCE LENW01
                     BY REFERENCE COMMAREA02 BY REFERENCE LENW02
                     BY REFERENCE COMMAREA03 BY REFERENCE LENW03
                     BY REFERENCE COMMAREA04 BY REFERENCE LENW04
                     BY REFERENCE COMMAREA05 BY REFERENCE LENW05
                     BY REFERENCE COMMAREA06 BY REFERENCE LENW06
                     BY REFERENCE COMMAREA07 BY REFERENCE LENW07
                     BY REFERENCE COMMAREA08 BY REFERENCE LENW08
                     BY REFERENCE COMMAREA09 BY REFERENCE LENW09
                     BY REFERENCE COMMAREA10 BY REFERENCE LENW10
                MOVE SPACES TO CWCHAIN
                DISPLAY "CWCHAIN" UPON ENVIRONMENT-NAME
                DISPLAY  CWCHAIN UPON ENVIRONMENT-VALUE
                GO TO RODA
           END-IF
           IF  FROMGUI NOT = SPACES
               WRITE TASKW-REG FROM 'C'
               WRITE TASKW-REG FROM COMMAREA01
               WRITE TASKW-REG FROM COMMAREA02
               WRITE TASKW-REG FROM COMMAREA03
               WRITE TASKW-REG FROM COMMAREA04
               WRITE TASKW-REG FROM COMMAREA05
               WRITE TASKW-REG FROM COMMAREA06
               WRITE TASKW-REG FROM COMMAREA07
               WRITE TASKW-REG FROM COMMAREA08
               WRITE TASKW-REG FROM COMMAREA09
               WRITE TASKW-REG FROM COMMAREA10
               CLOSE TASKW
           END-IF
           IF   CWRUN = 1
                IF  VCDEBUG = 'ON'
                AND COBVER = 'VC'
                    CALL "CBL_DEBUG_STOP" using  by value vc-flags
                END-IF
                STOP RUN
           END-IF
           DISPLAY 'CWLOGN'      UPON ENVIRONMENT-NAME
           ACCEPT   NOME         FROM ENVIRONMENT-VALUE
           DISPLAY 'CWLOGG'      UPON ENVIRONMENT-NAME
           ACCEPT   GRUPO        FROM ENVIRONMENT-VALUE
           DISPLAY SPACES        UPON ENVIRONMENT-VALUE
           DISPLAY 'CWLOGI'      UPON ENVIRONMENT-NAME
           ACCEPT  IMPRESSORA    FROM ENVIRONMENT-VALUE
           DISPLAY SPACES        UPON ENVIRONMENT-VALUE
           DISPLAY 'CWLOGP'      UPON ENVIRONMENT-NAME
           ACCEPT  CWLOGP        FROM ENVIRONMENT-VALUE
           IF CWLOGP NOT = SPACE
              MOVE CWLOGP TO CHECK-NIVEL
              MOVE SPACES TO CWLOGP
           END-IF
           DISPLAY SPACES        UPON ENVIRONMENT-VALUE
           CALL "CWPICT" USING X"00"
           PERFORM 220-REMOVE-OVERLAYS THRU 220-99-FIM

           GO TO RELOAD-CWMEN0.

       210-PAUSA.

           IF   CRON = 'ON'
                DISPLAY CWSEND-MSG UPON CONSOLE
           ELSE
                CALL "CWSEND" USING PARAMETROS-CWSEND
                  ON EXCEPTION
                     CALL "CWMSGW" USING "230370"
                                          CWSEND-MSG
                     ON EXCEPTION
                        DISPLAY (23, 3) CWSEND-MSG WITH SIZE 70
                     END-CALL
                END-CALL
          END-IF.

       210-99-FIM. EXIT.

       220-REMOVE-OVERLAYS.

           COPY CWOVRL.
           SET CWSQLC-OPEN TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE "RT" TO CWCONF-CHAVE
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           IF   FS-CWCONF < "10"
                PERFORM VARYING I FROM 1 BY 1
                          UNTIL I > CWCONF-TOT-ROTINAS
                             OR CWCONF-ROTINA (I) = SPACES
                           IF   (NOT CWCONF-ROTINA (I) = "CWGETU")
                           AND  (NOT CWCONF-ROTINA (I) = "CWMENU")
                                CANCEL CWCONF-ROTINA (I)
                           END-IF
                END-PERFORM
           END-IF

           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       220-99-FIM. EXIT.

       240-EXEC-JOB.

           MOVE SPACES TO PROXIMO
           SET CWSQLC-OPEN TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           PERFORM TEST AFTER UNTIL FS-CWCONF NOT = "9D"
                   MOVE "JB"     TO CWCONF-CHAVE
                   MOVE JOB      TO CWCONF-JOB
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-EQUAL TO TRUE
                   CALL "CWCONF" USING CWSQLC
                                       CWCONF-REG
                                       FS-CWCONF
                                       KCO PCO
                   IF   FS-CWCONF = "9D"
                        CALL "CWCONF" USING "ISAM"
                   END-IF
           END-PERFORM

           IF   FS-CWCONF = "23"
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                CALL X"E5"
                MOVE SPACES TO CWSEND-MSG
                STRING MSG-6 JOB DELIMITED BY SIZE INTO CWSEND-MSG
                PERFORM 210-PAUSA THRU 210-99-FIM
           ELSE
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                MOVE 0 TO ERRO
                IF   CWCONF-JOB-COBOL
                     MOVE CWCONF-JOB-MODULO TO PROGRAMA2
                     IF  CWCONF-JOB-PARAMETRO (1: 1) = "$"
                         MOVE CWCONF-JOB-PARAMETRO (2: 4) TO CA-POSI
                         MOVE CWCONF-JOB-PARAMETRO (7: 2) TO CA-SIZE
                         IF  (CA-POSI NOT = 0)
                         AND (CA-SIZE NOT = 0)
                         MOVE CWCONF-JOB-PARAMETRO (10: )
                              TO COMMAREA01(CA-POSI: CA-SIZE)
                         END-IF
                         CALL PROGRAMA2 USING COMMAREA01
                                              COMMAREA02
                                              COMMAREA03
                                              COMMAREA04
                                              COMMAREA05
                                              COMMAREA06
                                              COMMAREA07
                                              COMMAREA08
                                              COMMAREA09
                                              COMMAREA10
                         ON OVERFLOW
                            PERFORM 241-ERRO-COBOL THRU 241-99-FIM
                         END-CALL
                     ELSE
                         CALL PROGRAMA2 USING CWCONF-JOB-PARAMETRO
                                              COMMAREA01
                                              COMMAREA02
                                              COMMAREA03
                                              COMMAREA04
                                              COMMAREA05
                                              COMMAREA06
                                              COMMAREA07
                                              COMMAREA08
                                              COMMAREA09
                                              COMMAREA10
                         ON OVERFLOW
                            PERFORM 241-ERRO-COBOL THRU 241-99-FIM
                         END-CALL
                     END-IF
                     DISPLAY "CWCANCELED" UPON ENVIRONMENT-NAME
                     DISPLAY PROGRAMA2    UPON ENVIRONMENT-VALUE
                     CALL "CWSQLC" USING "K*"
                     CANCEL PROGRAMA2
                ELSE
                     MOVE CWCONF-JOB-MODULO TO BINARIO
                     MOVE SPACES TO CWEXEC-COMANDO
                     PERFORM VARYING SIZE-BINARIO FROM 50 BY -1
                             UNTIL SIZE-BINARIO = 0
                                OR BYTE-BINARIO (SIZE-BINARIO) NOT = " "
                             CONTINUE
                     END-PERFORM
                     STRING BINARIO (1: SIZE-BINARIO) DELIMITED BY SIZE
                                                  " " DELIMITED BY SIZE
                                 CWCONF-JOB-PARAMETRO DELIMITED BY SIZE
                                    INTO CWEXEC-COMANDO
                     IF  CWCONF-JOB-MENSAGEM NOT = SPACES
                         CALL "CWMSGW" USING "230350"
                                             CWCONF-JOB-MENSAGEM
                     END-IF
                     MOVE 0 TO CWEXEC-RETORNO
                     IF   CWCONF-JOB-WINDOWS
                          SET CWEXEC-ASSYNCRONE TO TRUE
                     END-IF
                     CALL "CWEXEC" USING PARAMETROS-CWEXEC
                     IF   CWEXEC-RETORNO NOT = 0
                          MOVE 1 TO ERRO
                     END-IF
                     PERFORM 250-LIGA-MOUSE THRU 250-99-FIM
                END-IF
                CALL "CWATCH"
                IF   ERRO = 1
                     MOVE CWCONF-JOB-PROXIMO-NAO-OK TO PROXIMO
                     IF   PROXIMO = SPACES
                          MOVE SPACES TO CWSEND-MSG
                          CALL X"E5"
                          STRING "Job com erro: " JOB DELIMITED BY SIZE
                                             INTO CWSEND-MSG
                          PERFORM 210-PAUSA THRU 210-99-FIM
                     END-IF
                ELSE
                     MOVE CWCONF-JOB-PROXIMO-RC-OK  TO PROXIMO
                END-IF
                IF   PROXIMO NOT = SPACES
                     MOVE PROXIMO TO JOB
                     GO TO 240-EXEC-JOB
                END-IF
           END-IF.

       240-99-FIM. EXIT.

       241-ERRO-COBOL.

           CALL X"E5"
           MOVE SPACES TO CWSEND-MSG
           STRING MSG-3 PROGRAMA2 DELIMITED BY SIZE
                   INTO CWSEND-MSG
           PERFORM 210-PAUSA THRU 210-99-FIM
           MOVE 1 TO ERRO.

       241-99-FIM. EXIT.

       242-EXEC-WIN.

           PERFORM VARYING I FROM 30 BY -1 UNTIL I = 0
                   OR (NOME(I:1) NOT = SPACE)
                   CONTINUE
           END-PERFORM
           PERFORM VARYING Y FROM 30 BY -1 UNTIL Y = 0
                   OR (SENHA(Y:1) NOT = SPACE)
                   CONTINUE
           END-PERFORM
           MOVE SPACES TO CmdLine JANELA-GUI
           DISPLAY 'CWPOSIT' UPON ENVIRONMENT-NAME
           ACCEPT JANELA-GUI FROM ENVIRONMENT-VALUE
           DISPLAY 'CWRUNTITLE' UPON ENVIRONMENT-NAME
           ACCEPT JANELA-T FROM ENVIRONMENT-VALUE
           PERFORM VARYING J FROM LENGTH JANELA-GUI BY -1
                   UNTIL J = 9
                     OR (JANELA-GUI (J:1) NOT = SPACE)
                     CONTINUE
           END-PERFORM
           INSPECT JANELA-GUI(1:J) CONVERTING SPACE TO '_'
           STRING COBWARE DELIMITED BY SPACE
                  '\cwmenuG.EXE /U:'
                  NOME(1:I) ' /S:' SENHA(1:Y)
                  ' /W:' JANELA-GUI(1:J)
                  ' /R:' PROGRAMA X"00"
               DELIMITED BY SIZE INTO CmdLine
           MOVE 5                      TO CmdShow
           CALL WINAPI "WinExec" USING BY REFERENCE CmdLine
                                       BY VALUE     CmdShow
                                       RETURNING    CmdStatus.

       242-99-FIM. EXIT.

       250-LIGA-MOUSE.

           IF   CWGETL-MOUSE = 1
           AND  MOUSE-STATUS = 0
                MOVE 1 TO MOUSE-STATUS
                CALL "CBL_INIT_MOUSE"         USING MOUSE-HANDLE
                                                    MOUSE-BUTTONS
                     ON OVERFLOW
                        CONTINUE
                END-CALL
                CALL "CBL_SET_MOUSE_POSITION" USING MOUSE-HANDLE
                                                    MOUSE-POSITION
                     ON OVERFLOW
                        CONTINUE
                END-CALL
                CALL "CBL_SHOW_MOUSE"         USING MOUSE-HANDLE
                     ON OVERFLOW
                        CONTINUE
                END-CALL
           END-IF

           IF  (CWGETL-MOUSE NOT = 1)
           AND  MOUSE-STATUS = 1
                MOVE 0 TO MOUSE-STATUS
                CALL "CBL_HIDE_MOUSE"         USING MOUSE-HANDLE
                     ON OVERFLOW
                        CONTINUE
                END-CALL
                CALL "CBL_TERM_MOUSE" USING MOUSE-HANDLE
                     ON OVERFLOW
                        CONTINUE
                END-CALL
           END-IF.

       250-99-FIM. EXIT.

       360-ABEND.

           PERFORM 210-PAUSA THRU 210-99-FIM
           STOP RUN.

       360-99-FIM. EXIT.

       COPY CWSHPD.

       END PROGRAM CWMENU.

