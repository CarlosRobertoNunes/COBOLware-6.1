       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMENU.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  05/12/1987.
       SECURITY.      *************************************************
                      *                                               *
                      *   Menu geral  SP2                             *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA
                      CALL-CONVENTION 74 IS WINAPI.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FSINI  ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-FSINI.

           SELECT TASKW  ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-TASKW
                  RESERVE NO ALTERNATE AREA.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"

           SELECT CUITAB ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CUITAB-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-CUITAB
                  RESERVE NO ALTERNATE AREA.

       DATA DIVISION.
       FILE SECTION.

       FD  TASKW
           VALUE OF FILE-ID IS LB-TASKW.

       01  TASKW-REG            PIC X(2000).

       FD  FSINI
           VALUE OF FILE-ID IS LB-FSINI.

       01  FSINI-REG                   PIC X(5080).

       FD  CUITAB
           RECORD VARYING FROM 52 TO 307 DEPENDING ON SZ-CUITAB
           VALUE OF FILE-ID IS LB-CUITAB.

       01  CUITAB-REG.
           05 CUITAB-CHAVE    PIC X(050).
           05 CUITAB-JANELA   PIC X(001).
           05 CUITAB-NOFRAME  PIC X(001).
           05 CUITAB-CMDLINE  PIC X(255).

       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       01  LOAD-CBLPRNT USAGE PROCEDURE-POINTER.
       COPY CWSHWS.
       01  AREAS-DE-TRABALHO-1.
           05 CWRTSW                  PIC  X(050) VALUE 'CWRTSW'.
           05 CWCONSOLE               PIC  X(004) VALUE SPACES.
           05 vc-flags         comp-5 pic  x(004) value 0.
           05 vc-timeout       comp-5 pic s9(009) value 0.
           05 vc-identifier           pic  x(007) VALUE Z'CWMENU'.
           05 COBVER                  PIC  X(002) VALUE SPACE.
           05 VCDEBUG                 PIC  X(002) VALUE SPACE.
           05 CWLOGP                  PIC  X(001) VALUE SPACES.
           05 JANELA-GUI              PIC  X(255) VALUE SPACES.
           05 BUFFER30.
              10                      PIC  X(003).
              10 BF30                 PIC  X(255) VALUE SPACES.
           05 NOclear                 PIC  9(001) VALUE 0.
           05 B30                     PIC  9(003) VALUE 0.
           05 J                       PIC  9(003) VALUE 0.
           05 LC                      PIC  X(255) VALUE SPACES.
           05 LC-I                    PIC  9(003) VALUE 0.
           05 CWNOFRAME               PIC  X(003) VALUE SPACES.
           05 TIME8.
              10 DATE-8               PIC  X(006) VALUE SPACES.
              10 TIME-8               PIC  X(008) VALUE SPACES.
           05 ASPA                    PIC  X(001) VALUE SPACE.
           05 SZ-CUITAB               PIC  9(004) VALUE 0.
           05 TXT                     PIC  X(001) VALUE '0'.
           05 VEZ                     PIC  9(004) VALUE 0.
           05 CmdShow                 PIC  9(005) COMP-5.
           05 CmdStatus               PIC  9(005) COMP-5.
           05 CmdLine                 PIC  X(255) VALUE SPACES.
           05 SP2END                  PIC  X(001) VALUE SPACE.
           05 CIL                     PIC  X(050) VALUE SPACE.
           05 JANELA                  PIC  X(001) VALUE SPACE.
           05 ER-TASKW.
              10 FS-TASKW             PIC  X(002) VALUE "00".
              10 LB-TASKW             PIC  X(255) VALUE "cwtask".
           05 ER-FSINI.
              10 FS-FSINI             PIC  X(002) VALUE "00".
              10 LB-FSINI             PIC  X(255) VALUE "cwmenut.ini".
           05 ER-CUITAB.
              10 FS-CUITAB            PIC  X(002) VALUE "00".
              10 LB-CUITAB            PIC  X(255) VALUE "cwmenut.".
           05 LENW01                  PIC  9(008) COMP-X VALUE 2000.
           05 LENW02                  PIC  9(008) COMP-X VALUE 2000.
           05 LENW03                  PIC  9(008) COMP-X VALUE 2000.
           05 LENW04                  PIC  9(008) COMP-X VALUE 2000.
           05 LENW05                  PIC  9(008) COMP-X VALUE 2000.
           05 LENW06                  PIC  9(008) COMP-X VALUE 2000.
           05 LENW07                  PIC  9(008) COMP-X VALUE 2000.
           05 LENW08                  PIC  9(008) COMP-X VALUE 2000.
           05 LENW09                  PIC  9(008) COMP-X VALUE 2000.
           05 LENW10                  PIC  9(008) COMP-X VALUE 2000.
           05 CWCHAIN                 PIC  X(255) VALUE SPACES.
           05 CWMENU-ACTIVE           PIC  X(003) VALUE SPACES.
           05 CWFULLMODE              PIC  X(004) VALUE SPACES.
           05 OK                      PIC  X(001) VALUE SPACE.
           05 CA-POSI                 PIC  9(004) VALUE 0.
           05 CA-SIZE                 PIC  9(002) VALUE 0.
           05 USUARIO-SENHA-CL        PIC  X(200) VALUE SPACES.
           05 RC                      PIC  9(001) VALUE 0.
           05 PRINTER-STATUS          PIC  9(002) COMP-X VALUE 0.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 RUN-ID                  PIC  9(004) COMP-5 VALUE 0.
           05 HORA-A                  PIC  X(006) VALUE SPACES.
           05 PROG-LOWER              PIC  X(008) VALUE SPACES.
           05 HORA-B                  PIC  X(006) VALUE SPACES.
           05 ERRO-CALL               PIC  9(001) VALUE 0.
           05 I                       PIC  9(004) VALUE 0.
           05 T                       PIC  9(004) VALUE 0.
           05 Y                       PIC  9(004) VALUE 0.
           05 Z                       PIC  9(004) VALUE 0.
           05 P                       PIC  9(004) VALUE 1.
           05 NF                      PIC  9(002) VALUE 0.
           05 MOUSE-STATUS            PIC  9(001) VALUE 0.
           05 CFG                     PIC  9(002) VALUE 0.
           05 PAGINA                  PIC  9(004) VALUE 1.
           05 TASK                    PIC  9(006) VALUE ZERO.
           05 COMMAREAS                           VALUE ALL X"B0".
              10 COMMAREA01           PIC  X(2000).
              10 COMMAREA02           PIC  X(2000).
              10 COMMAREA03           PIC  X(2000).
              10 COMMAREA04           PIC  X(2000).
              10 COMMAREA05           PIC  X(2000).
              10 COMMAREA06           PIC  X(2000).
              10 COMMAREA07           PIC  X(2000).
              10 COMMAREA08           PIC  X(2000).
              10 COMMAREA09           PIC  X(2000).
              10 COMMAREA10           PIC  X(2000).
           05 RELATORIO               PIC  X(007) VALUE SPACES.
           05 PROGRAMA                PIC  X(008) VALUE "CWMENU".
           05 REDEFINES PROGRAMA.
              10 JOB-FLAG             PIC  X(001).
              10 JOB-NOME             PIC  X(007).
           05 ERRO                    PIC  9(001) VALUE 0.
           05 JOB                     PIC  X(007) VALUE SPACES.
           05 PROGRAMA2               PIC  X(008) VALUE SPACES.
           05 PROXIMO                 PIC  X(007) VALUE SPACES.
           05 NOME                    PIC  X(030) VALUE "LOGON".
           05 GRUPO                   PIC  X(022) VALUE SPACES.
           05 CHECK-NIVEL             PIC  9(001) VALUE ZERO.
           05 USUARIO                 PIC  X(030) VALUE SPACES.
           05 SISTEMA                 PIC  X(030) VALUE SPACES.
           05 SET-LOG                 PIC  X(001) VALUE "9".
           05 MODO-MENU               PIC  9(001) VALUE ZERO.
           05 CURSOR-POSITION.
              10                      PIC  9(004) COMP-X VALUE 00.
              10                      PIC  9(004) COMP-X VALUE 00.
           05 NADA                    PIC  X(001) VALUE SPACE.
           05 IMPRESSORA              PIC  X(008) VALUE SPACES.
           05 QUADRO                  PIC  9(002) VALUE 99.
           05 PARAMETRO-BIN.
              10 SIZE-BINARIO         PIC  9(002) COMP-X VALUE 0.
              10 BINARIO.
                 15 BYTE-BINARIO      PIC  X(001) OCCURS 50.
           05 MOUSE-HANDLE            PIC  X(004) COMP-X VALUE 1.
           05 MOUSE-BUTTONS           PIC  9(002) COMP-X VALUE 3.
           05 MOUSE-POSITION.
              10 ROW-MOUSE            PIC  9(004) COMP-X VALUE 24.
              10 COLUMN-MOUSE         PIC  9(004) COMP-X VALUE 00.
           05 CWRUN                   PIC  X(001) VALUE '0'.
           05 CWRUN-CMD               PIC  X(255) VALUE SPACES.
           05 CWDEBUG                 PIC  X(255) VALUE SPACES.
           05 LINHA-COMANDO           PIC  X(255) VALUE SPACES.
           05 LINHA-COMANDO-UPPER     PIC  X(255) VALUE SPACES.
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
           05 INSTALL-FLAG          PIC 9(002) COMP-X VALUE 0.
           05 INSTALL-PARMS.
              10 INSTALL-ADDRS        PROCEDURE-POINTER.
              10 INSTALL-PRRTY        PIC 9(002) COMP-X VALUE 0.

       COPY CWMOUS.
       COPY CWSEND.
       COPY CWGETL.
       COPY CWEXEC.
       COPY CWSPWS.
       COPY CWCONF.
       COPY CWIMPR.
       COPY CONSOLE.

       PROCEDURE DIVISION.

       INICIO.

           DISPLAY 'COBVER' UPON ENVIRONMENT-NAME
           ACCEPT   COBVER  FROM ENVIRONMENT-VALUE
           IF COBVER = 'VC'
              DISPLAY 'VCDEBUG' UPON ENVIRONMENT-NAME
              ACCEPT   VCDEBUG  FROM ENVIRONMENT-VALUE
              IF VCDEBUG = 'ON'
                 call "CBL_DEBUGBREAK"
                 CALL "CBL_DEBUG_START" using  by value vc-flags
                                            vc-timeout
                              by reference  vc-identifier
      *                       returning     status-code
              END-IF
           END-IF
           ACCEPT DATE-8 FROM DATE
           ACCEPT TIME-8 FROM TIME
           DISPLAY "TASKTIME" UPON ENVIRONMENT-NAME
           DISPLAY TIME8      UPON ENVIRONMENT-VALUE
           CALL "COB32API"
           SET LOAD-CBLPRNT TO ENTRY "CBLPRNT.DLL"
           COPY CWSPPD.
           MOVE "cobwareg"   TO MODO
           PERFORM ATALHO  THRU FIM-ATALHO
           CALL "CWATTR"
             ON EXCEPTION
                MOVE SPACES TO SP2DIR
                STRING  COBWARE DELIMITED BY SPACE
                        "\cobwareg.lbr\CWATTR" DELIMITED BY SIZE
                          INTO SP2DIR
                CALL SP2DIR
                END-CALL
           END-CALL
           DISPLAY "SP2DIR" UPON ENVIRONMENT-NAME
           MOVE SPACES TO SP2DIR
           STRING ".;"   DELIMITED BY SIZE
                  COBDIR DELIMITED BY SPACE
                  ";" DELIMITED BY SIZE
                  INTO SP2DIR
           DISPLAY SP2DIR   UPON ENVIRONMENT-VALUE
           MOVE SPACES TO SP2DIR
           STRING  COBWARE DELIMITED BY SPACE
                   "\cobware.msg" DELIMITED BY SIZE
                     INTO SP2DIR
           DISPLAY "SP2MSG" UPON ENVIRONMENT-NAME
           DISPLAY SP2DIR   UPON ENVIRONMENT-VALUE
      *    DISPLAY "SP2KBF" UPON ENVIRONMENT-NAME
      *    DISPLAY "2"      UPON ENVIRONMENT-VALUE
           MOVE SPACES TO SP2-BF-DATA
KS         DISPLAY 'SKIPUSER' UPON ENVIRONMENT-NAME
KS         DISPLAY 'ON'       UPON ENVIRONMENT-VALUE
KS         CALL "CWGETL" USING PARAMETROS-CWGETL
           CALL "cobwareg"
KS         DISPLAY 'SKIPUSER' UPON ENVIRONMENT-NAME
KS         DISPLAY SPACES     UPON ENVIRONMENT-VALUE
           DISPLAY "CWDEBUG" UPON ENVIRONMENT-NAME
           ACCEPT   CWDEBUG  FROM ENVIRONMENT-VALUE
           IF  CWDEBUG NOT = SPACES
               DISPLAY CWDEBUG UPON COMMAND-LINE
           END-IF
           SET INSTALL-ADDRS TO ENTRY "CWSTOP"
           CALL "CBL_EXIT_PROC" USING INSTALL-FLAG
                                      INSTALL-PARMS
           DISPLAY 'CWRTSW' UPON ENVIRONMENT-NAME
           ACCEPT   CWRTSW  FROM ENVIRONMENT-VALUE
           INSPECT  CWRTSW       CONVERTING MINUSCULAS
                                         TO MAIUSCULAS
      *    SET INSTALL-ADDRS TO ENTRY "CWRTSW"
           IF   CWRTSW NOT = 'OFF'
                SET INSTALL-ADDRS TO ENTRY CWRTSW
                CALL "CBL_ERROR_PROC" USING INSTALL-FLAG
                                            INSTALL-PARMS
           END-IF

           INITIALIZE MAPA
      *    DISPLAY " " AT 2480
           CALL "CWTASK" USING "1" TASK
           CALL "CWTEXT" USING AREAS-DE-TRABALHO-2
                     LENGTH OF AREAS-DE-TRABALHO-2
           CANCEL "CWTASK"
           ACCEPT LINHA-COMANDO FROM COMMAND-LINE
           MOVE LINHA-COMANDO TO LINHA-COMANDO-UPPER
           INSPECT LINHA-COMANDO-UPPER CONVERTING MINUSCULAS
                                               TO MAIUSCULAS
           MOVE 0 TO CWRUN TI TN
           MOVE SPACES TO CWTEST NEWCMD
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
           END-PERFORM

           IF   CWRUN = 1
                DISPLAY 'CWRUN' UPON ENVIRONMENT-NAME
                DISPLAY CWRUN   UPON ENVIRONMENT-VALUE
                PERFORM VARYING I FROM 1 BY 1
                          UNTIL I > (LENGTH OF LINHA-COMANDO - 4)
                       IF LINHA-COMANDO-UPPER (I:3) = '/U:'
                                                   OR '/S:' OR '/P:'
                                                   OR '/D:' OR '/C:'
                       OR LINHA-COMANDO-UPPER (I:8) = '/NOFRAME'
                          MOVE SPACES   TO BUFFER30
                          MOVE 1        TO B30
                          DISPLAY 'CWLIXO' UPON ENVIRONMENT-NAME
                          IF LINHA-COMANDO-UPPER (I:3) = '/U:'
                             DISPLAY 'CWUSERNAME' UPON ENVIRONMENT-NAME
                          END-IF
                          IF LINHA-COMANDO-UPPER (I:3) = '/S:' OR '/P:'
                             DISPLAY 'CWPASSWORD' UPON ENVIRONMENT-NAME
                          END-IF
                          PERFORM UNTIL I > (LENGTH LINHA-COMANDO - 4)
                                     OR LINHA-COMANDO-UPPER (I:1) = ' '
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
                IF CWTEST NOT = SPACES
                   DISPLAY 'CWTEST' UPON ENVIRONMENT-NAME
                   DISPLAY  CWTEST  UPON ENVIRONMENT-VALUE
                   DISPLAY NEWCMD UPON COMMAND-LINE
                   MOVE NEWCMD TO LINHA-COMANDO LINHA-COMANDO-UPPER
                   INSPECT LINHA-COMANDO-UPPER CONVERTING MINUSCULAS
                                                       TO MAIUSCULAS
                END-IF
                DISPLAY "CWFULLMODE" UPON ENVIRONMENT-NAME
                ACCEPT CWFULLMODE    FROM ENVIRONMENT-VALUE
                INSPECT CWFULLMODE CONVERTING MINUSCULAS TO MAIUSCULAS
                IF CWFULLMODE = "MENU"
                   DISPLAY "CWMENU-ACTIVE" UPON ENVIRONMENT-NAME
                   DISPLAY "NO"            UPON ENVIRONMENT-VALUE
                END-IF
                MOVE CWRUN-CMD TO LINHA-COMANDO LC
                INSPECT LC CONVERTING MINUSCULAS TO MAIUSCULAS
                MOVE 0         TO Y
                PERFORM VARYING I FROM 1 BY 1 UNTIL I = 255
                                                 OR P = 11
                        IF  LC (I: 3) = "/U:" OR "/S:" OR "/D:" OR "/C:"
                        OR  LC (1: 8) = '/NOFRAME'
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
                                  ADD  1                    TO Y
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
                   PERFORM VARYING I FROM I BY 1
                           UNTIL LINHA-COMANDO-UPPER (I: 1) NOT = SPACE
                           CONTINUE
                   END-PERFORM
                END-IF
                move LINHA-COMANDO-UPPER (I: ) TO LINHA-COMANDO
                inspect LINHA-COMANDO converting '0' to x'01'
                CALL 'CWPACK' USING LINHA-COMANDO
                             LENGTH LINHA-COMANDO
                inspect LINHA-COMANDO converting x'01' to '0'
           ELSE
                OPEN I-O CUITAB
                MOVE "T" TO JANELA
                PERFORM LOADINI THRU FIM-LOADINI
                MOVE "F" TO JANELA
                PERFORM LOADINI THRU FIM-LOADINI
           END-IF

           DISPLAY LINHA-COMANDO UPON COMMAND-LINE.

       RELOAD-CWMEN0.

           CALL "CWGETL" USING PARAMETROS-CWGETL
           MOVE CWGETL-LOG        TO SET-LOG
           CALL "CWGETU" USING NOME TASK PROGRAMA SET-LOG
             ON OVERFLOW
                 MOVE MSG-2 TO SP2-MS-TEXT
                 PERFORM SENDMSG
                 STOP RUN
           END-CALL
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
                    MOVE MSG-1 TO SP2-MS-TEXT
                    PERFORM SENDMSG
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
                OR   RELATORIO(1:1) = '&'
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
                          PERFORM 230-EXEC-COBOL THRU 230-99-FIM
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
           IF   CWRUN = 1
                IF  VCDEBUG = 'ON'
                AND COBVER = 'VC'
      *             move 1 to vc-flags
                    CALL "CBL_DEBUG_STOP" using  by value vc-flags
      *                        returning status-code
                END-IF
                STOP RUN
           END-IF
           DISPLAY 'CWLOGN'      UPON ENVIRONMENT-NAME
           ACCEPT   NOME         FROM ENVIRONMENT-VALUE
           DISPLAY SPACES        UPON ENVIRONMENT-VALUE
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
           IF NOclear = 0
GRITO *       CALL "CWSCRE" USING X"08"
GRITO         CALL "CWUSER" USING X"01" *> Reinicia tela
           ELSE
              MOVE 0 TO NOclear
           END-IF
           PERFORM 220-REMOVE-OVERLAYS THRU 220-99-FIM

           GO TO RELOAD-CWMEN0.

       210-PAUSA.

           CALL "CWSEND" USING PARAMETROS-CWSEND
             ON EXCEPTION
                CALL "CWMSGW" USING "230370"
                                     CWSEND-MSG
                ON EXCEPTION
                   MOVE CWSEND-MSG TO SP2-MS-TEXT
                   PERFORM SENDMSG
                END-CALL
           END-CALL.

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

       230-EXEC-COBOL.

           MOVE CWPROG (1:P) TO CUITAB-CHAVE
           EVALUATE TXT
               WHEN '0'
                    MOVE '23' TO FS-CUITAB
               WHEN '*'
                    MOVE '*' TO CUITAB-CHAVE
                    READ CUITAB
               WHEN OTHER
                    MOVE CWPROG (1:P) TO CUITAB-CHAVE
                    MOVE P            TO Y
                    MOVE '23' TO FS-CUITAB
                    PERFORM UNTIL FS-CUITAB = '00'
                               OR Y = 0
                            READ CUITAB
                            IF FS-CUITAB NOT = '00'
                               MOVE '*' TO CUITAB-CHAVE (Y:)
                               SUBTRACT 1 FROM Y
                               IF TXT = '1'
                                  EXIT PERFORM
                               END-IF
                            END-IF
                    END-PERFORM
           END-EVALUATE
           IF FS-CUITAB = '00'
           AND (TXT = '*' OR '?')
               MOVE '-'          TO CUITAB-CHAVE
               MOVE CWPROG (1:P) TO CUITAB-CHAVE(2:)
               READ CUITAB
               IF FS-CUITAB = '00'
                  MOVE '23' TO FS-CUITAB
               ELSE
                  COMPUTE Y = P + 1
                  MOVE '00' TO FS-CUITAB
                  PERFORM UNTIL FS-CUITAB = '23'
                             OR Y = 1
                          READ CUITAB
                          IF FS-CUITAB NOT = '00'
                             MOVE '*' TO CUITAB-CHAVE (Y:)
                             SUBTRACT 1 FROM Y
                             MOVE '00' TO FS-CUITAB
                          ELSE
                             MOVE '23' TO FS-CUITAB
                          END-IF
                  END-PERFORM
               END-IF
           END-IF
           IF FS-CUITAB = '00'
           AND (CWPROG (1:2) NOT = 'CW')
              STRING '$TEMP/cwtaskE.' TIME8 DELIMITED BY SIZE
                      INTO LB-TASKW
              OPEN OUTPUT TASKW
              WRITE TASKW-REG FROM '*'
              DISPLAY "CWAPPLICATION" UPON ENVIRONMENT-NAME
              MOVE SPACES TO TASKW-REG
              ACCEPT TASKW-REG FROM ENVIRONMENT-VALUE
              WRITE TASKW-REG
              IF SZ-CUITAB > 52
                 MOVE CUITAB-CMDLINE TO TASKW-REG
              ELSE
                 MOVE SPACES         TO TASKW-REG
              END-IF
              WRITE TASKW-REG
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
121217        move spaces to lb-taskw
121217        STRING '$TEMP/cwtaskR.' TIME8 DELIMITED BY SIZE
121217                INTO LB-TASKW
              PERFORM VARYING I FROM 30 BY -1 UNTIL I = 0
                      OR (NOME(I:1) NOT = SPACE)
                      CONTINUE
              END-PERFORM
              PERFORM VARYING Y FROM 30 BY -1 UNTIL Y = 0
                      OR (SENHA(Y:1) NOT = SPACE)
                      CONTINUE
              END-PERFORM
              MOVE SPACES TO CmdLine
              DISPLAY "SP2END" UPON ENVIRONMENT-NAME
              ACCEPT  SP2END   FROM ENVIRONMENT-VALUE
              DISPLAY SPACE    UPON ENVIRONMENT-VALUE
              DISPLAY "CWNOFRAME" UPON ENVIRONMENT-NAME
              ACCEPT CWNOFRAME FROM ENVIRONMENT-VALUE
              IF CUITAB-NOFRAME = 'Y'
                 DISPLAY 'ON'     UPON ENVIRONMENT-VALUE
              ELSE
                 DISPLAY SPACE    UPON ENVIRONMENT-VALUE
              END-IF
              CALL "CWLOCK" USING "U" NOME TASK PROGRAMA
              CALL "CWCOLOR" USING "OFF"
              IF CUITAB-JANELA = 'F'
                 STRING 'MODE CON: LINES=25|' DELIMITED BY SIZE
                        COBWARE DELIMITED BY SPACE
                       '\cwmenuF.EXE /U:'
                     NOME(1:I) ' /S:' SENHA(1:Y)
                     ' /T:' TASK
                     ' /R:' CWPROG (1:P) X"00"
                     DELIMITED BY SIZE INTO CmdLine
                 CALL "system"  USING CmdLine
              ELSE
                 STRING COBWARE DELIMITED BY SPACE
                        '\cwmenuT.EXE /U:'
                        NOME(1:I) ' /S:' SENHA(1:Y)
                        ' /T:' TASK
                        ' /R:' CWPROG (1:P) X"00"
                  DELIMITED BY SIZE INTO CmdLine
                 MOVE 5                      TO CmdShow
                 CALL WINAPI "WinExec" USING BY REFERENCE CmdLine
                                             BY VALUE     CmdShow
                                             RETURNING    CmdStatus
                 MOVE 0                      TO CmdShow
              END-IF
              DISPLAY "SP2END" UPON ENVIRONMENT-NAME
              DISPLAY SP2END   UPON ENVIRONMENT-VALUE
              EXEC COBOLware Wait
                   SECONDS 5
              END-EXEC
              PERFORM TEST AFTER UNTIL(FS-TASKW = '00' OR '35')
                 OPEN INPUT TASKW
              END-PERFORM
              MOVE 1 TO NOclear
              IF FS-TASKW = '00'
                 READ TASKW
                 IF TASKW-REG = 'C'
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
                 ELSE
                    CLOSE TASKW
                    DELETE FILE TASKW
                    DISPLAY 'CWFROMGUI' UPON ENVIRONMENT-NAME
                    DISPLAY '******'    UPON ENVIRONMENT-VALUE
                    STOP RUN
                 END-IF
              END-IF
              DISPLAY SPACES UPON ENVIRONMENT-VALUE
              DISPLAY "CWNOFRAME" UPON ENVIRONMENT-NAME
              DISPLAY CWNOFRAME   UPON ENVIRONMENT-VALUE
           ELSE
              IF   PARAMETROS (51: ) = SPACES
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
           END-IF.

       230-99-FIM. EXIT.

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
                              TO COMMAREA01 (CA-POSI: CA-SIZE)
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

           MOVE LOW-VALUES      TO SP2-WD-DATA
           CALL SP2 USING SP2-GET-WINDOW-DEF SP2-WINDOW-DEF

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
           MOVE SP2-WD-TITLE TO JANELA-GUI(9:)
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
                                       RETURNING    CmdStatus
           MOVE 0                      TO CmdShow.

       242-99-FIM. EXIT.

       360-ABEND.

           PERFORM 210-PAUSA THRU 210-99-FIM
           STOP RUN.

       360-99-FIM. EXIT.

       SENDMSG.

           MOVE "b"      TO SP2-MS-ICON
           MOVE "o"      TO SP2-MS-BUTTON
           MOVE "Aviso:" TO SP2-MS-TITLE
           MOVE 1        TO SP2-MS-LINE-CNT
           CALL SP2   USING SP2-DISPLAY-MESSAGE
                            SP2-MESSAGE-DATA.

       FIM-SENDMSG. EXIT.

       LOADINI.

           MOVE SPACES TO LB-FSINI
           STRING "cwmenu" JANELA '.ini'
                  DELIMITED BY SIZE INTO LB-FSINI
           DISPLAY LB-FSINI UPON ENVIRONMENT-NAME
           ACCEPT  LB-FSINI FROM ENVIRONMENT-VALUE
           OPEN INPUT FSINI
           IF FS-FSINI > '09'
              MOVE SPACES TO LB-FSINI
              STRING "..\cwmenu" JANELA '.ini'
                     DELIMITED BY SIZE INTO LB-FSINI
              OPEN INPUT FSINI
              IF FS-FSINI > '09'
                 MOVE SPACES TO LB-FSINI
                 STRING "$COBOLWARE\cwmenu" JANELA '.ini'
                        DELIMITED BY SIZE INTO LB-FSINI
                 OPEN INPUT FSINI
              END-IF
           END-IF

           MOVE 0      TO Y
           MOVE 52     TO SZ-CUITAB
           MOVE SPACES TO CUITAB-REG

           PERFORM UNTIL FS-FSINI > '09'
              READ FSINI
               NOT AT END
                PERFORM VARYING I FROM 1 BY 1
                  UNTIL I > LENGTH CUITAB-REG
                  OR FSINI-REG (I:) = SPACES
                     IF FSINI-REG (I:1) = SPACE
                     AND ASPA = SPACE
                         PERFORM GRAVA-CUITAB THRU FIM-GRAVA-CUITAB
                     ELSE
                        IF Y < 50
                           IF  ASPA NOT = SPACE
                           AND (FSINI-REG (I:1) NOT = ASPA)
                               IF SZ-CUITAB < 307
                                  ADD 1 TO SZ-CUITAB
                                  MOVE FSINI-REG (I:1)
                                    TO CUITAB-REG (SZ-CUITAB:1)
                               END-IF
                               EXIT PERFORM CYCLE
                           END-IF
                           IF  FSINI-REG (I:1) = '$'
                           AND Y = 0
                               MOVE 'Y' TO CUITAB-NOFRAME
                               EXIT PERFORM CYCLE
                           END-IF
                           IF ASPA = SPACE
                              IF FSINI-REG (I:1) = '"' OR "'"
                                 MOVE FSINI-REG (I:1) TO ASPA
                                 EXIT PERFORM CYCLE
                              END-IF
                           ELSE
                              IF FSINI-REG (I:1) = '"' OR "'"
                                 MOVE SPACE TO ASPA
                                 EXIT PERFORM CYCLE
                              END-IF
                           END-IF
                           ADD 1 TO Y
                           MOVE FSINI-REG (I:1) TO CUITAB-CHAVE (Y:1)
                           IF TXT = '0'
                              MOVE '1' TO TXT
                           END-IF
                           IF  FSINI-REG (I:1) = '*'
                           AND (TXT NOT = '*')
                               MOVE '?' TO TXT
                           END-IF
                        END-IF
                     END-IF
                END-PERFORM
                PERFORM GRAVA-CUITAB THRU FIM-GRAVA-CUITAB
              END-READ
           END-PERFORM

           PERFORM GRAVA-CUITAB THRU FIM-GRAVA-CUITAB
           CLOSE FSINI.

       FIM-LOADINI. EXIT.

       GRAVA-CUITAB.

           IF Y NOT = 0
              MOVE 0    TO Y
              MOVE JANELA TO CUITAB-JANELA
              INSPECT CUITAB-CHAVE CONVERTING MINUSCULAS TO MAIUSCULAS
              WRITE CUITAB-REG
              IF FS-CUITAB = '22'
                 REWRITE CUITAB-REG
              END-IF
              IF CUITAB-CHAVE = '*'
                 MOVE '*' TO TXT
              END-IF
              MOVE SPACES TO CUITAB-REG
              MOVE 52     TO SZ-CUITAB
           END-IF.

       FIM-GRAVA-CUITAB. EXIT.
       COPY CWSHPD.
       END PROGRAM CWMENU.

