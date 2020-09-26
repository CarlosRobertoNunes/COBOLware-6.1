      $Set NoOptional-File
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    COBWARE.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  18/01/2001.
       SECURITY.      *************************************************
                      *                                               *
                      *  Ativa rotinas COBOLware                      *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA
           CALL-CONVENTION 66 IS WIN32
           CALL-CONVENTION 74 IS WINAPI.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SORTWK ASSIGN TO DISK.

           SELECT FHCONF ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-FHCONF.

       DATA DIVISION.
       FILE SECTION.

       SD  SORTWK
           VALUE OF FILE-ID LB-SORTWK.

       01  SORTWK-REG.
           05 SORTWK-LIXO              PIC  X(001).

       FD  FHCONF
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-FHCONF.

       01  FHCONF-REG           PIC  X(080).

       WORKING-STORAGE SECTION.

       01 my-ret-code             pic x(4) comp-5 value zeroes.
       01 Windir                  pic x(100) value spaces.
       01 LB-FONT                 pic x(255) value spaces.
       01 TEST-BUFFER             PIC X(100) VALUE SPACES.
       01 HORA1                   pic X(5) VALUE SPACE.
       01 HORA2                   pic X(5) VALUE SPACE.
       01 REV                     pic 9(3) VALUE 0.
       01 FS-IN                   pic X(003) VALUE "00".
       01 LB-IN                   pic X(255) VALUE SPACES.
       78 HKEY-CURRENT-USER                       value h"80000001".
       78 HKEY-LOCAL-MACHINE                      value h"80000002".
       78 F98 VALUE Z"SOFTWARE\Microsoft\Windows\CurrentVersion\Fonts".
       78 FNT VALUE
          Z"SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts".
       78 MF VALUE
          Z"Software\Micro Focus\COBOLWB 4.0\Apps\CWMENUT\Text Window".
       78 NE VALUE
        Z"Software\Micro Focus\NetExpress\1.1\Apps\CWMENUT\Text Window".
       78 CW VALUE Z"Software\COBOLware\6.1\Revision".
       01 key-handle              pic x(4) comp-5 value zeroes.
       78 NON-VOLATILE                            value 0.
       78 KEY-ALL-ACCESS                          value h"3F".
       01 create-open-flag        pic x(4) comp-5 value zeroes.
       01 type-string             pic x(4) comp-5 value 1.
       01 type-hexa               pic x(4) comp-5 value 3.
       01 key-value-length        pic x(4) comp-5 value 73.
       01 key-value-buffer        pic x(100)      value X"49000000070000
      -   "00FCFFFFFFFCFFFFFF2403000040020000160000000A00000090010000FF0
      -   "00000004D4620535647412028383030783630302E43503433372900000000
      -   "0000000000".
       01 cw-value-length         pic x(4) comp-5 value 16.

       01  AREAS-DE-TRABALHO. COPY \cobware\cpy\CWCASE.
           05 LOAD-CBLPRNT    USAGE PROCEDURE-POINTER.
           05 LOAD-COBINTFN   USAGE PROCEDURE-POINTER.
           05 LOAD-FreeImage  USAGE PROCEDURE-POINTER.
           05 CWSQLC-SETS.
              10 LB-JORNAL         PIC X(255) VALUE SPACES.
              10 CWQUEUEINPUT      PIC  X(255) VALUE SPACES.
           05 CWGETL               PIC  X(003) VALUE SPACES.
           05 CRON                 PIC  X(003) VALUE SPACES.
           05 FS-OK                PIC  9(001) VALUE 0.
           05 CWPCAT               PIC  X(003) VALUE SPACES.
           05 JPEG                 PIC  X(003) VALUE SPACES.
           05 CWCONF-FS            PIC  X(002) VALUE SPACES.
           05 TEMP-UPP             PIC  X(077) VALUE SPACES.
           05 TEMP                             VALUE SPACES.
              10 TEMP-DRIVE        PIC  X(002).
                 88 TEMP-OK                    VALUE "A:" "B:" "C:"
                                                     "a:" "b:" "c:".
              10 TEMP-RESTO        PIC  X(075).
           05 CWLITS               PIC  X(003) VALUE SPACES.
           05 CWCLEAR              PIC  X(003) VALUE SPACES.
           05 CWACCENT             PIC  X(003) VALUE SPACES.
           05 BREAK                PIC  X(003) VALUE SPACE.
           05 X91-RESULT    COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION  COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER COMP-X PIC  9(002) VALUE 0.
           05 FSMSG                PIC  X(080) VALUE SPACES.
           05 L                    PIC  X(001) VALUE SPACE.
           05 CWTITLE              PIC  X(200) VALUE SPACES.
           05 RESPOSTA             PIC  X(001) VALUE SPACE.
           05 BARRA                PIC  X(001) VALUE SPACE.
           05 SW                   PIC  X(001) VALUE SPACE.
           05 SEPARA               PIC  X(001) VALUE SPACE.
           05 PONTO                PIC  X(001) VALUE SPACE.
           05 API                  PIC  9(001) VALUE 0.
           05 NOFRAME              PIC  9(001) VALUE 0.
           05 BRANCOS              PIC  X(080) VALUE SPACES.
           05 EXTRAS               PIC  X(006) VALUE "¯ÅÑÍÒÓ". *> Kamino
           05 REDEFINES EXTRAS.
              10 BYTE-E            PIC  X(001) OCCURS 6 COMP-X.
           05 PCATT                PIC  X(025) VALUE
              "CBL_SCR_SET_PC_ATTRIBUTES".
           05 TRAVADO              PIC  X(002) VALUE "9D".
           05 LEU                  PIC  9(001) VALUE 0.
           05 LEU-LC               PIC  9(001) VALUE 0.
           05 VEZ                  PIC  9(001) VALUE 1.
           05 VEZX                 PIC  9(006) VALUE 1.
           05 LB-SORTWK            PIC  X(010) VALUE "CWMENU.$$$".
           05 I                    pic x(4) comp-5 value 16.
           05 Y                    pic x(4) comp-5 value 16.
           05 I2                   PIC  9(004) VALUE 0.
           05 M                    PIC  9(004) VALUE 0.
           05 TODAY                PIC  9(008) VALUE 0.
           05 SOMA                 PIC  9(006) VALUE 0.
           05 DATA-L               PIC  9(008) VALUE 0.
           05 DECORRIDOS           PIC  9(002) VALUE 0.
           05 COBWARE              PIC  X(250) VALUE SPACES.
           05 PRINTER-STATUS       PIC  9(002) COMP-X VALUE 0.
           05 HOSTNAME             PIC  X(020) VALUE SPACES.
           05 FSSERVER             PIC  X(020) VALUE SPACES.
           05 CCITCP2              PIC  X(020) VALUE SPACES.
           05 FS.
              10 FS-FUNCTION-CODE PIC 9(02) COMP-X VALUE 1.
              10 FSUSERNAME       PIC X(20) VALUE SPACES.
              10 FSPASSWORD       PIC X(20) VALUE SPACES.
              10 FS-RETURN        PIC 9(02) COMP-X VALUE 0.
           05 FHREDIR             PIC  X(050) VALUE SPACES.
           05 ORACLE-STRING       PIC X(70) VALUE SPACES.
           05 LOGIN                       VALUE SPACES.
              10 LOGIN-USERNAME PIC X(10).
              10 LOGIN-PASSWORD PIC X(10).
           05 ER-FHCONF.
              10 FS-FHCONF        PIC  X(002) VALUE "00".
              10 LB-FHCONF        PIC  X(255) VALUE SPACES.
           05 EXCECOES.
              10                  PIC  X(020) VALUE "cwlang*".
              10                  PIC  X(020) VALUE "A:*  ".
              10                  PIC  X(020) VALUE "a:*  ".
              10                  PIC  X(020) VALUE "B:*  ".
              10                  PIC  X(020) VALUE "b:*  ".
              10                  PIC  X(020) VALUE "C:*  ".
              10                  PIC  X(020) VALUE "c:*  ".
              10                  PIC  X(020) VALUE "COM1 ".
              10                  PIC  X(020) VALUE "COM1:".
              10                  PIC  X(020) VALUE "COM2 ".
              10                  PIC  X(020) VALUE "COM2:".
              10                  PIC  X(020) VALUE "COM3 ".
              10                  PIC  X(020) VALUE "COM3:".
              10                  PIC  X(020) VALUE "COM4 ".
              10                  PIC  X(020) VALUE "COM4:".
              10                  PIC  X(020) VALUE "LPT1 ".
              10                  PIC  X(020) VALUE "LPT1:".
              10                  PIC  X(020) VALUE "LPT2 ".
              10                  PIC  X(020) VALUE "LPT2:".
              10                  PIC  X(020) VALUE "LPT3 ".
              10                  PIC  X(020) VALUE "LPT3:".
              10                  PIC  X(020) VALUE "LPT4 ".
              10                  PIC  X(020) VALUE "LPT4:".
              10                  PIC  X(020) VALUE "PRN  ".
              10                  PIC  X(020) VALUE "PRN: ".
           05 REDEFINES EXCECOES.
              10 EXCECAO          PIC  X(020) OCCURS 25.
           05 AFS.
              10                  PIC  X(020) VALUE SPACES.
              10                  PIC  X(020) VALUE SPACES.
              10                  PIC  X(020) VALUE SPACES.
              10                  PIC  X(020) VALUE SPACES.
              10                  PIC  X(020) VALUE SPACES.
              10                  PIC  X(020) VALUE SPACES.
              10                  PIC  X(020) VALUE SPACES.
              10                  PIC  X(020) VALUE "/dev/ttyS0".
              10                  PIC  X(020) VALUE "/dev/ttyS0".
              10                  PIC  X(020) VALUE "/dev/ttyS1".
              10                  PIC  X(020) VALUE "/dev/ttyS1".
              10                  PIC  X(020) VALUE "/dev/ttyS2".
              10                  PIC  X(020) VALUE "/dev/ttyS2".
              10                  PIC  X(020) VALUE "/dev/ttyS3".
              10                  PIC  X(020) VALUE "/dev/ttyS3".
              10                  PIC  X(020) VALUE "/dev/lp0".
              10                  PIC  X(020) VALUE "/dev/lp0".
              10                  PIC  X(020) VALUE "/dev/lp1".
              10                  PIC  X(020) VALUE "/dev/lp1".
              10                  PIC  X(020) VALUE "/dev/lp2".
              10                  PIC  X(020) VALUE "/dev/lp2".
              10                  PIC  X(020) VALUE "/dev/lp3".
              10                  PIC  X(020) VALUE "/dev/lp3".
              10                  PIC  X(020) VALUE "/dev/lp0".
              10                  PIC  X(020) VALUE "/dev/lp0".
           05 REDEFINES AFS.
              10 AF               PIC  X(020) OCCURS 25.

       01  CmdLine                     PIC X(256).
       01  CmdShow                     PIC 9(4) COMP-5.
       01  CmdStatus                   PIC 9(4) COMP-5.
       01 HINSTANCE             is   typedef  PIC X(04)  COMP-5.
       01 PPROH-WIN             HINSTANCE.
       01 NULLPtr               procedure-pointer value NULL.
       01 wHwnd                 pic xxxx comp-5.
       78 GCL-STYLE             value -26.
       01 long                  is   typedef  pic s9(9)  comp-5.
       01 ERRO-API              LONG.
       01 nindex                LONG.
       78 CS-NOCLOSE            value h"0200".
       01 App-hInstResource     HINSTANCE.

       01 FILENAME             pic x(256) value spaces.
  3479 01 path2                pic x(256) value spaces.
  3479 01 path-file            pic x(256) value spaces.
    66 77 uns-short          typedef pic 9(4) comp-5.
       01  LenShort            usage uns-short.
  3490 01  STRINGS.
  3479     05 bString              pic x(256).

       COPY CWTIME.
       COPY CWSEND.
       COPY CWUNIX.
       COPY CWREVS.
       COPY CWCONF.

       78  WS-SYSMENU        value h"00080000".
       78  GWL-STYLE         value         -16.
       01  ws-textwin-handle           pic s9(4) comp-5 value 0.
       01  ws-temp-handle              pic s9(4) comp-5.
       01  win-Style                   pic s9(9) comp-5.
       01  retlong                     pic s9(9) comp-5.
       01  ws-char                     pic x.

       LINKAGE SECTION.

       01   OK PIC X(6).
       01   W  PIC X(1).

       PROCEDURE DIVISION USING OK W.

       000-INICIO SECTION.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           ON   1
                DISPLAY "CWREVS" UPON ENVIRONMENT-NAME
                DISPLAY CWREVS-REVISAO UPON ENVIRONMENT-VALUE
                DISPLAY "CWCRON" UPON ENVIRONMENT-NAME
                ACCEPT  CRON   FROM ENVIRONMENT-VALUE
                INSPECT CRON
                   CONVERTING MINUSCULAS TO MAIUSCULAS
                DISPLAY "windir" UPON ENVIRONMENT-NAME
                ACCEPT   windir  FROM ENVIRONMENT-VALUE
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF EXTRAS
                        SUBTRACT 100 FROM BYTE-E (I)
                END-PERFORM.

           IF   X91-PARAMETER = 1
                IF OK (1:2) = 'FS'
                   MOVE 'ON' TO CWCONF-FS
                   PERFORM 005-FILESHARE THRU 005-99-FIM
                   GOBACK
                END-IF
           END-IF

           ON   1
                IF   X91-PARAMETER = 2
                     CALL "CWATTR"
                END-IF
                DISPLAY "COBOLWARE" UPON ENVIRONMENT-NAME
                ACCEPT COBWARE      FROM ENVIRONMENT-VALUE
                IF COBWARE (1:1) NOT = '/'
                   CALL "COB32API"
                   SET LOAD-FreeImage  TO ENTRY "FreeImage.dll"
                   PERFORM 700-API THRU 700-99-FIM
                   MOVE 1 TO API
                   move spaces to TEST-BUFFER
                   perform test after until my-ret-code not = 234
                   call WIN32 "RegQueryValueA"
                      using by value     HKEY-LOCAL-MACHINE
                            BY REFERENCE CW
                            BY REFERENCE TEST-BUFFER
                            BY REFERENCE cw-value-length
                      returning my-ret-code
                   END-PERFORM
                   IF   TEST-BUFFER NOT = spaces
                        MOVE TEST-BUFFER(3:3) TO REV(1:3)
                        IF REV NOT NUMERIC
                           MOVE SPACES TO TEST-BUFFER
                        END-IF
                   END-IF
                   IF   TEST-BUFFER = spaces
                   or  (my-ret-code not = 0)
                        PERFORM REGEDIT
                   ELSE
                        IF TEST-BUFFER not = CWREVISAO
                           call WIN32 "RegSetValueA"
                              using by value     HKEY-LOCAL-MACHINE
                                    BY REFERENCE CW
                                    by value     Type-string
                                    BY REFERENCE CWREVISAO
                                    by value     cw-value-length
                           end-call
                        END-IF
                   END-IF
                END-IF
                DISPLAY "BREAK" UPON ENVIRONMENT-NAME
                ACCEPT BREAK    FROM ENVIRONMENT-VALUE
                INSPECT BREAK CONVERTING MINUSCULAS
                                      TO MAIUSCULAS
                IF  BREAK NOT = "ON"
                AND (CRON NOT = 'ON')
                    CALL X"B0" USING X"04" X"01"
                END-IF
                DISPLAY "CWCLEAR" UPON ENVIRONMENT-NAME
                ACCEPT CWCLEAR  FROM ENVIRONMENT-VALUE
                INSPECT CWCLEAR CONVERTING MINUSCULAS
                                      TO MAIUSCULAS
                CALL "CWUNIX" USING PARAMETROS-CWUNIX
                IF  (CWCLEAR NOT = "OFF")
                AND (CRON    NOT = 'ON')
                    DISPLAY "CWGETL" UPON ENVIRONMENT-NAME
                    ACCEPT   CWGETL  FROM ENVIRONMENT-VALUE
                    IF CWGETL NOT = 'OFF'
                       DISPLAY (1, 1) ERASE
                    END-IF
                END-IF
                CALL "CWLOCK"
                CALL "CWUNIX" USING PARAMETROS-CWUNIX
                IF   CWUNIX-OFF
                     MOVE "\" TO BARRA
                     MOVE ";" TO SEPARA
                     MOVE "/" TO SW
                     IF   NOT CWUNIX-WIN32
                          SORT SORTWK ON ASCENDING KEY SORTWK-LIXO
                                INPUT PROCEDURE IS 500-COBWARE-SORT
                               OUTPUT PROCEDURE IS 600-SORT-COBWARE
                     END-IF
                ELSE
                     SET LOAD-COBINTFN TO ENTRY "cobintfn"
                     MOVE "/" TO BARRA
                     MOVE ":" TO SEPARA
                     MOVE "-" TO SW
                END-IF.

           IF   VEZ = 3
                STOP RUN
           END-IF

           IF   VEZ = 1
                DISPLAY "CWPCAT" UPON ENVIRONMENT-NAME
                ACCEPT  CWPCAT    FROM ENVIRONMENT-VALUE
                INSPECT CWPCAT  CONVERTING MINUSCULAS TO MAIUSCULAS
                IF  (CWPCAT  NOT = 'OFF')
                AND (CRON    NOT = 'ON')
                   CALL PCATT
                      ON EXCEPTION
                         CONTINUE
                   END-CALL
                END-IF
                DISPLAY "CWCONF_FS" UPON ENVIRONMENT-NAME
                ACCEPT  CWCONF-FS   FROM ENVIRONMENT-VALUE
                IF   CWCONF-FS = SPACES
                     DISPLAY "CWCONF-FS" UPON ENVIRONMENT-NAME
                     ACCEPT  CWCONF-FS   FROM ENVIRONMENT-VALUE
                END-IF
                INSPECT CWCONF-FS CONVERTING MINUSCULAS TO MAIUSCULAS
                DISPLAY CWCONF-FS UPON ENVIRONMENT-VALUE
                IF  CWCONF-FS = "ON"
                    PERFORM 005-FILESHARE THRU 005-99-FIM
                ELSE
                    PERFORM TEST AFTER UNTIL FS-CWCONF = '00'
                                          OR VEZ > 1
                            SET CWSQLC-OPEN   TO TRUE
                            CALL "CWCONF" USING CWSQLC CWCONF-REG
                                                FS-CWCONF KCO PCO
                            IF   FS-CWCONF < "10"
                                 SET CWSQLC-CLOSE  TO TRUE
                                 CALL "CWCONF" USING CWSQLC CWCONF-REG
                                                     FS-CWCONF KCO PCO
                                 PERFORM 005-FILESHARE THRU 005-99-FIM
                            ELSE
                                 IF   VEZ = 2
                                 OR ((FS-CWCONF NOT = "30")
                                 AND (FS-CWCONF NOT = "35"))
                                         CALL "CWCONF" USING "ISAM"
                                 END-IF
                                 IF   VEZ = 1
                                      MOVE 2 TO VEZ
                                      CALL "CW3050"
                                 END-IF
                            END-IF
                    END-PERFORM
                END-IF
                DISPLAY 'CWQUEUEFILE' UPON ENVIRONMENT-NAME
                ACCEPT  LB-JORNAL      FROM ENVIRONMENT-VALUE
                DISPLAY "CWQUEUEINPUT" UPON ENVIRONMENT-NAME
                ACCEPT  CWQUEUEINPUT   FROM ENVIRONMENT-VALUE
                IF CWSQLC-SETS = SPACES
                   MOVE SPACES TO LOGIN
                   DISPLAY "ORACLE_USERNAME" UPON ENVIRONMENT-NAME
                   ACCEPT  LOGIN-USERNAME   FROM ENVIRONMENT-VALUE
                   IF LOGIN-USERNAME = SPACES
                      DISPLAY "ORACLE-USERNAME" UPON ENVIRONMENT-NAME
                      ACCEPT  LOGIN-USERNAME   FROM ENVIRONMENT-VALUE
                   END-IF
                   IF LOGIN-USERNAME NOT = SPACES
                      DISPLAY "ORACLE_PASSWORD" UPON ENVIRONMENT-NAME
                      ACCEPT  LOGIN-PASSWORD   FROM ENVIRONMENT-VALUE
                      IF LOGIN-PASSWORD = SPACES
                         DISPLAY "ORACLE-PASSWORD" UPON ENVIRONMENT-NAME
                         ACCEPT  LOGIN-PASSWORD   FROM ENVIRONMENT-VALUE
                      END-IF
                      IF LOGIN-PASSWORD NOT = SPACES
                         CALL "CWORCN" USING LOGIN
                         DISPLAY 'CWORCN' UPON ENVIRONMENT-NAME
                         DISPLAY '*'      UPON ENVIRONMENT-VALUE
                      END-IF
                   ELSE
                      MOVE SPACES TO ORACLE-STRING
                      DISPLAY "ORACLE_STRING" UPON ENVIRONMENT-NAME
                      ACCEPT  ORACLE-STRING   FROM ENVIRONMENT-VALUE
                      IF ORACLE-STRING = SPACES
                         DISPLAY "ORACLE-STRING" UPON ENVIRONMENT-NAME
                         ACCEPT  ORACLE-STRING   FROM ENVIRONMENT-VALUE
                      END-IF
                      IF ORACLE-STRING NOT = SPACES
                         CALL "CWORCN" USING ORACLE-STRING
                         DISPLAY 'CWORCN' UPON ENVIRONMENT-NAME
                         DISPLAY '*'      UPON ENVIRONMENT-VALUE
                      END-IF
                   END-IF
                   MOVE SPACES TO LOGIN
                   END-IF
           END-IF


           MOVE 3 TO VEZ.


       000-99-FIM. GOBACK.

       005-FILESHARE.

           IF FS-OK = 1
              GO TO 005-API
           END-IF

           MOVE SPACES TO FSUSERNAME
                          FSPASSWORD
                          FSSERVER
                          CCITCP2

           IF  CWCONF-FS NOT = "ON"
               SET CWSQLC-UPDATE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               MOVE "LG" TO CWCONF-CHAVE
               SET CWSQLC-READ        TO TRUE
               SET CWSQLC-IGNORE-LOCK TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               IF   FS-CWCONF = "00"
                    IF  (CWCONF-FSSERVER NOT = SPACES)
                    AND (CWCONF-FSSERVER NOT = LOW-VALUES)
                        DISPLAY "FSSERVER"      UPON ENVIRONMENT-NAME
                        DISPLAY CWCONF-FSSERVER UPON ENVIRONMENT-VALUE
                    END-IF
                    IF  (CWCONF-CCITCP2 NOT = SPACES)
                    AND (CWCONF-CCITCP2 NOT = LOW-VALUES)
                        DISPLAY "CCITCP2"      UPON ENVIRONMENT-NAME
                        DISPLAY CWCONF-CCITCP2 UPON ENVIRONMENT-VALUE
                    END-IF
                    IF  (CWCONF-FSUSERNAME NOT = SPACES)
                    AND (CWCONF-FSUSERNAME NOT = LOW-VALUES)
                        DISPLAY "FSUSERNAME"      UPON ENVIRONMENT-NAME
                        DISPLAY CWCONF-FSUSERNAME UPON ENVIRONMENT-VALUE
                    END-IF
                    IF  (CWCONF-FSPASSWORD  NOT = SPACES)
                    AND (CWCONF-FSPASSWORD  NOT = LOW-VALUES)
                        DISPLAY "FSPASSWORD"      UPON ENVIRONMENT-NAME
                        DISPLAY CWCONF-FSPASSWORD UPON ENVIRONMENT-VALUE
                    END-IF
               END-IF
           END-IF

           DISPLAY "FSSERVER" UPON ENVIRONMENT-NAME
           ACCEPT FSSERVER FROM ENVIRONMENT-VALUE

           DISPLAY "CCITCP2" UPON ENVIRONMENT-NAME
           ACCEPT CCITCP2 FROM ENVIRONMENT-VALUE

           DISPLAY "FSPASSWORD" UPON ENVIRONMENT-NAME
           ACCEPT   FSPASSWORD FROM ENVIRONMENT-VALUE
           IF   FSPASSWORD = SPACES
                MOVE EXTRAS TO FSPASSWORD
           END-IF
           DISPLAY "FSUSERNAME" UPON ENVIRONMENT-NAME
           ACCEPT   FSUSERNAME FROM ENVIRONMENT-VALUE
           IF   FSUSERNAME = SPACES
                MOVE "COBOLware" TO FSUSERNAME
           END-IF

           IF  (FSSERVER NOT = SPACES)
           OR  (CCITCP2  NOT = SPACES)
                IF FSSERVER = SPACES
                   MOVE "COBSHARE" TO FSSERVER
                END-IF
                DISPLAY "FHREDIR" UPON ENVIRONMENT-NAME
                ACCEPT FHREDIR FROM ENVIRONMENT-VALUE
                IF  FHREDIR = SPACES
                    DISPLAY "TEMP"    UPON ENVIRONMENT-NAME
                    ACCEPT  TEMP      FROM ENVIRONMENT-VALUE
                    PERFORM VARYING I FROM 1 BY 1 UNTIL I > 75
                                        OR TEMP (I: 1) = SPACE
                            CONTINUE
                    END-PERFORM
                    MOVE BARRA TO TEMP (I: 1)
                    ADD  1     TO I
                    MOVE "*"   TO TEMP (I: 1)
                    MOVE TEMP  TO TEMP-UPP
                    IF   CWUNIX-OFF
                         INSPECT TEMP-UPP CONVERTING MINUSCULAS
                                                  TO MAIUSCULAS
                         MOVE "cwmenu.fsw" TO FHREDIR
                    ELSE
                         MOVE "cwmenu.fsu" TO FHREDIR
                    END-IF
                    DISPLAY "FHREDIR" UPON ENVIRONMENT-NAME
                    DISPLAY FHREDIR   UPON ENVIRONMENT-VALUE
                    MOVE FHREDIR TO LB-FHCONF
                    OPEN INPUT FHCONF
                    IF  FS-FHCONF = "30" OR "35"
                        OPEN OUTPUT FHCONF
                        PERFORM VARYING I FROM 1 BY 1 UNTIL I > 25
                            IF   CWUNIX-OFF
                            OR   AF (I) = SPACES
                                 MOVE SW           TO FHCONF-REG
                                 MOVE "f"          TO FHCONF-REG (2:)
                                 MOVE EXCECAO (I) TO FHCONF-REG (4:)
                            ELSE
                                 MOVE SPACES TO FHCONF-REG
                                 STRING SW  DELIMITED BY SIZE
                                       "f " DELIMITED BY SIZE
                                       EXCECAO (I) DELIMITED BY SPACE
                                       " -af " DELIMITED BY SIZE
                                       AF (I) DELIMITED BY SPACE
                                       INTO FHCONF-REG
                            END-IF
                            WRITE FHCONF-REG
                        END-PERFORM
                        IF   CWUNIX-ON
                             MOVE 'chmod 666 ' To TEST-BUFFER
                             MOVE FHREDIR      TO TEST-BUFFER (11:)
                             EXEC COBOLware EXECSystem
                                  COMMAND TEST-BUFFER
                             END-EXEC
                        END-IF
                    ELSE
                        IF  FS-FHCONF > "09"
                            CALL "CWISAM" USING ER-FHCONF
                        END-IF
                    END-IF
                    CLOSE FHCONF
                END-IF
                IF   NOT TEMP-OK
                     OPEN INPUT FHCONF
                     PERFORM UNTIL FS-FHCONF > "09"
                                OR FHCONF-REG (4: ) = TEMP-UPP
                             READ FHCONF
                             IF   CWUNIX-OFF
                                  INSPECT FHCONF-REG
                                  CONVERTING MINUSCULAS
                                          TO MAIUSCULAS
                             END-IF
                     END-PERFORM
                     IF  FS-FHCONF = "10"
                         CLOSE FHCONF
                         OPEN EXTEND FHCONF
                         MOVE SW           TO FHCONF-REG
                         MOVE "f"          TO FHCONF-REG (2:)
                         MOVE TEMP         TO FHCONF-REG (4:)
                         WRITE FHCONF-REG
                     END-IF
                     CLOSE FHCONF
                END-IF
                MOVE 1             TO FS-FUNCTION-CODE
                CALL "fhrdrpwd" USING FS-FUNCTION-CODE
                                      FSUSERNAME
                                      FSPASSWORD
                      ON EXCEPTION CONTINUE
                END-CALL
           END-IF

           IF  CWCONF-FS NOT = "ON"
               IF  CWCONF-FSCHANGED = 1
                   MOVE "?" TO L
                   CALL "CWLOCK" USING L
                   IF   L = "N"
                        IF  FSUSERNAME = "COBOLware"
                            MOVE SPACES TO FSUSERNAME
                                           FSPASSWORD
                        END-IF
                        MOVE SPACES TO FSMSG
                        STRING "Servidor " DELIMITED BY SIZE
                                FSSERVER   DELIMITED BY SPACE
                                " inacess¡vel" DELIMITED BY SIZE
                                INTO FSMSG
                        EXEC COBOLware BoxDialog
                             LINE 05 COLUMN 20
                             HEADER FSMSG
                             Caption(1) "Servidor"
                             Caption(2) "IP"
                             Caption(3) "Usu rio"
                             Caption(4) "Senha"
                        Size(1) 15 Data(1) FSSERVER;CWCONF-FSSERVER
                        Size(2) 15 Data(2) CCITCP2;CWCONF-CCITCP2
                        Size(3) 20 Data(3) FSUSERNAME;CWCONF-FSUSERNAME
                        Size(4) 20 Data(4) FSPASSWORD;CWCONF-FSPASSWORD
                                    (Secure(4))
                        END-EXEC
                        SET CWSQLC-REWRITE TO TRUE
                        CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF
                                                     KCO PCO
                        EXEC COBOLware Send
                          Message "Altera‡Æo de FileShare requer logon"
                        END-EXEC
                        DISPLAY "CWLG" UPON ENVIRONMENT-NAME
                        DISPLAY "ON"   UPON ENVIRONMENT-VALUE
                   ELSE
                        MOVE 0 TO CWCONF-FSCHANGED
                        SET CWSQLC-REWRITE TO TRUE
                        CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF
                                                     KCO PCO
                   END-IF
               END-IF
               SET CWSQLC-CLOSE TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF
                                                     KCO PCO
           END-IF
           MOVE 1 TO FS-OK.

       005-API.

           IF   API = 1
                PERFORM 700-API THRU 700-99-FIM
                MOVE 3 TO API
           END-IF.

       005-99-FIM. EXIT.



       700-API.

           SET LOAD-CBLPRNT TO ENTRY "CBLPRNT.DLL"

           DISPLAY "CWUNIX"  UPON ENVIRONMENT-NAME
           DISPLAY "DOS32"   UPON ENVIRONMENT-VALUE
           DISPLAY "CWTITLE" UPON ENVIRONMENT-NAME
           ACCEPT   CWTITLE  FROM ENVIRONMENT-VALUE

           IF  CWTITLE = SPACES
               MOVE "COBOLware 6.1"   TO CWTITLE
               MOVE CWREVS-REVISAO    TO CWTITLE (127: )
           ELSE
               DISPLAY "CWLITS" UPON ENVIRONMENT-NAME
               ACCEPT CWLITS FROM ENVIRONMENT-VALUE
               INSPECT CWLITS CONVERTING MINUSCULAS TO MAIUSCULAS
               DISPLAY "CWACCENT" UPON ENVIRONMENT-NAME
               ACCEPT CWACCENT FROM ENVIRONMENT-VALUE
               INSPECT CWACCENT CONVERTING MINUSCULAS TO MAIUSCULAS
               IF   CWLITS = "LOW"
                    INSPECT CWTITLE CONVERTING MAIUSCULAS TO MINUSCULAS
               END-IF
               IF   CWLITS = "UPP"
                    INSPECT CWTITLE CONVERTING MINUSCULAS TO MAIUSCULAS
               END-IF
               IF   CWACCENT = "OFF"
                    INSPECT CWTITLE CONVERTING ACENTOS-850
                                            TO ACENTOS-OFF
               END-IF
           END-IF
           DISPLAY "CWTITLE" UPON ENVIRONMENT-NAME
           DISPLAY  CWTITLE  UPON ENVIRONMENT-VALUE
           PERFORM VARYING I FROM LENGTH OF CWTITLE BY -2
                       UNTIL CWTITLE (I: 1) NOT = SPACE
                   CONTINUE
           END-PERFORM
           ADD 2 TO I
           MOVE X"00" TO CWTITLE (I: 1)
           call "PC_WIN_HANDLE"
              using BY REFERENCE ws-textwin-handle
           end-call


           call win32 "SetWindowTextA" using
                       by value ws-textwin-handle
             BY REFERENCE CWTITLE
           end-call.

           MOVE ZEROES TO PPROH-WIN.
           CALL WINAPI "FindWindowA" using
                by value NULLptr
                by content CWTITLE
                   returning PPROH-WIN.

           IF   PPROH-WIN NOT = 0
                DISPLAY "CWUNIX" UPON ENVIRONMENT-NAME
                DISPLAY "WIN32"  UPON ENVIRONMENT-VALUE
           END-IF

           CALL "PC_WIN_HANDLE" using wHwnd.

           CALL WINAPI "GetClassLongA" using
                by value PPROH-WIN
                by VALUE gcl-style
                   returning ERRO-API.
           compute nindex = erro-api + cs-noclose.
           CALL WINAPI "SetClassLongA" using
                by value PPROH-WIN
                by VALUE gcl-style
                by value nindex
                   returning ERRO-API.

           CALL WinApi "FreeLibrary" using App-hInstResource.

           DISPLAY 'TEMP' UPON ENVIRONMENT-NAME
           ACCEPT bString FROM ENVIRONMENT-VALUE
           IF  bString = SPACES
               DISPLAY 'TMP' UPON ENVIRONMENT-NAME
               ACCEPT bString FROM ENVIRONMENT-VALUE
           END-IF
           MOVE SPACE TO PONTO
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH bString
                   IF bString (I: 1) = '.'
                      MOVE '.' TO PONTO
                   END-IF
           END-PERFORM
           IF  bString = SPACES
           OR (PONTO NOT = SPACE)
               MOVE 'C:\CWTMP' TO bString
               CALL "CBL_CREATE_DIR" USING bString
           ELSE
               IF bString (1:1) = '"' OR "'"
                  MOVE bString (2:) TO PATH-FILE
                  INSPECT PATH-FILE CONVERTING "'" TO SPACE
                  INSPECT PATH-FILE CONVERTING '"' TO SPACE
                  MOVE    PATH-FILE TO bString
               END-IF
           END-IF
           PERFORM VARYING I FROM LENGTH bString BY -1
                   UNTIL I = 1
                   OR bString (I: 1) NOT = SPACE
                   CONTINUE
           END-PERFORM
           PERFORM VARYING Y FROM 1 BY 1 UNTIL bString(Y:1) = SPACE
                   CONTINUE
           END-PERFORM
           IF Y < I
              MOVE bString(1:I) TO bstring
              PERFORM GET-SHORT-NAME THRU FIM-GET-SHORT-NAME
              MOVE PATH-FILE TO bString
           END-IF
           DISPLAY 'TEMP'  UPON ENVIRONMENT-NAME
           DISPLAY bString UPON ENVIRONMENT-VALUE
           DISPLAY 'TMP'   UPON ENVIRONMENT-NAME
           DISPLAY bString UPON ENVIRONMENT-VALUE.

       700-99-FIM. EXIT.

       GET-SHORT-NAME.

            MOVE SPACES TO  PATH-FILE
            PERFORM VARYING I FROM length of bstring BY -1
                    UNTIL bstring (I: 1) not = space
                       OR I = 1
                          CONTINUE
            END-PERFORM
            add 1 to i
            move x'00' to bstring (I: 1)
            move length of PATH-FILE to LenShort
            CALL WINAPI "GetShortPathNameA" using
                 BY REFERENCE bstring
                 BY REFERENCE PATH-FILE
                 BY REFERENCE LenShort
            INSPECT PATH-FILE CONVERTING LOW-VALUE TO SPACE
            IF  PATH-FILE = spaces
                move space to bstring (I: 1)
                subtract 1 from i
                perform varying i from i by -1
                        until i = 1
                        or   bstring (I: 1) = '\'
                        continue
                end-perform
                IF I > 1
                   move x'00' to bstring (I: 1)
                   add 1 to i
                   move bstring (I: ) TO FILENAME
                   CALL WINAPI "GetShortPathNameA" using
                        BY REFERENCE bstring
                        BY REFERENCE path2
                        BY REFERENCE LenShort
                  IF path2 NOT = SPACES
                     STRING path2     DELIMITED BY LOW-VALUE
                            '\'      DELIMITED BY SIZE
                            FILENAME DELIMITED BY SPACE
                     INTO PATH-FILE
                     INSPECT PATH-FILE
                             CONVERTING LOW-VALUE TO SPACE
                  END-IF
                END-IF
            ELSE
                PERFORM VARYING I FROM 1 BY 1
                        UNTIL PATH-FILE (I: 1) = space or low-value
                           OR I > LENGTH OF PATH-FILE
                              CONTINUE
                END-PERFORM
                IF I < LENGTH OF PATH-FILE
                AND  PATH-FILE (I: 1) = space
                   MOVE SPACES TO PATH-FILE (I: )
                END-IF
            END-IF.

       FIM-GET-SHORT-NAME. EXIT.
       REGEDIT.

           MOVE 'C:\CWTMP\6.1' TO LB-FHCONF
           OPEN INPUT FHCONF

           IF   FS-FHCONF < '10'
                READ FHCONF
                IF   FS-FHCONF < '10'
                AND (FHCONF-REG(1: LENGTH CWREVS-REVISAO)
                     NOT = CWREVS-REVISAO)
                     CLOSE FHCONF
                     OPEN OUTPUT FHCONF
                     WRITE FHCONF-REG FROM CWREVS-REVISAO
                END-IF
                CLOSE FHCONF
                EXIT PARAGRAPH
           ELSE
                OPEN OUTPUT FHCONF
                WRITE FHCONF-REG FROM CWREVS-REVISAO
                CLOSE FHCONF
           END-IF

           MOVE SPACES TO LB-FHCONF

           IF WINDIR = SPACES
              MOVE "C:\WINDOWS" TO WINDIR
           END-IF

           MOVE SPACES TO LB-IN LB-FONT
           STRING COBWARE        DELIMITED BY SPACE
                  "\MFVGA01.FON" DELIMITED BY SIZE
                      INTO LB-IN
           STRING windir DELIMITED BY SPACE
               "\fonts\MFVGA01.FON " DELIMITED BY SIZE
                      INTO LB-FONT
           CALL "CBL_COPY_FILE" USING LB-IN LB-FONT
              RETURNING my-ret-code
           IF my-ret-code not = 0
              MOVE LB-IN TO LB-FONT
           END-IF
           MOVE 0               TO CmdShow
           MOVE SPACES          TO CmdLine
           STRING COBWARE         DELIMITED BY SPACE
                  "\addfont.exe " DELIMITED BY SIZE
                  LB-FONT         DELIMITED BY SPACE
                 Z' "MF VGA (640x480.CP437)" 2>NUL:'
                                  DELIMITED BY SIZE INTO CmdLine
           CALL WIN32 "WinExec" USING BY REFERENCE CmdLine
                                      BY VALUE     CmdShow
                                      RETURNING    CmdStatus
           END-CALL

           MOVE SPACES TO LB-IN LB-FONT
           STRING COBWARE        DELIMITED BY SPACE
                  "\MFVGA02.FON" DELIMITED BY SIZE
                      INTO LB-IN
           STRING windir DELIMITED BY SPACE
               "\fonts\MFVGA02.FON " DELIMITED BY SIZE
                      INTO LB-FONT
           CALL "CBL_COPY_FILE" USING LB-IN LB-FONT
              RETURNING my-ret-code
           IF my-ret-code not = 0
              MOVE LB-IN TO LB-FONT
           END-IF
           MOVE 0               TO CmdShow
           MOVE SPACES          TO CmdLine
           STRING COBWARE         DELIMITED BY SPACE
                  "\addfont.exe " DELIMITED BY SIZE
                  LB-FONT         DELIMITED BY SPACE
                 Z' "MF SVGA (800x600.CP437)" 2>NUL:'
                                  DELIMITED BY SIZE INTO CmdLine
           CALL WIN32 "WinExec" USING BY REFERENCE CmdLine
                                      BY VALUE     CmdShow
                                      RETURNING    CmdStatus
           END-CALL

           MOVE SPACES TO LB-IN LB-FONT
           STRING COBWARE        DELIMITED BY SPACE
                  "\MFVGA03.FON" DELIMITED BY SIZE
                      INTO LB-IN
           STRING windir DELIMITED BY SPACE
               "\fonts\MFVGA03.FON " DELIMITED BY SIZE
                      INTO LB-FONT
           CALL "CBL_COPY_FILE" USING LB-IN LB-FONT
              RETURNING my-ret-code
           IF my-ret-code not = 0
              MOVE LB-IN TO LB-FONT
           END-IF
           MOVE 0               TO CmdShow
           MOVE SPACES          TO CmdLine
           STRING COBWARE         DELIMITED BY SPACE
                  "\addfont.exe " DELIMITED BY SIZE
                  LB-FONT         DELIMITED BY SPACE
                 Z' "MF XGA (1024x768.CP437)" 2>NUL:'
                                  DELIMITED BY SIZE INTO CmdLine
           CALL WIN32 "WinExec" USING BY REFERENCE CmdLine
                                      BY VALUE     CmdShow
                                      RETURNING    CmdStatus
           END-CALL


              call WIN32 "RegCreateKeyA"
                          using by value     HKEY-LOCAL-MACHINE
                                BY REFERENCE CW
                                BY REFERENCE key-handle
              END-CALL


              call WIN32 "RegSetValueA"
                 using by value     HKEY-LOCAL-MACHINE
                       BY REFERENCE CW
                       by value     Type-string
                       BY REFERENCE CWREVISAO
                       by value     cw-value-length
              end-call

              call WIN32 "RegCreateKeyExA"
                          using by value     HKEY-CURRENT-USER
                                BY REFERENCE MF
                                by value     0
                                BY REFERENCE Z" "
                                by value     NON-VOLATILE
                                by value     KEY-ALL-ACCESS
                                by value     0
                                BY REFERENCE key-handle
                                BY REFERENCE create-open-flag
              END-CALL
              call WIN32 "RegSetValueExA"
                 using by value          key-handle
                       BY REFERENCE      z"Options"
                       by value          0
                       by value          Type-hexa
                       BY REFERENCE      key-value-buffer
                       by value          key-value-length
              end-call
              call WIN32 "RegCreateKeyExA"
                          using by value     HKEY-CURRENT-USER
                                BY REFERENCE NE
                                by value     0
                                BY REFERENCE Z" "
                                by value     NON-VOLATILE
                                by value     KEY-ALL-ACCESS
                                by value     0
                                BY REFERENCE key-handle
                                BY REFERENCE create-open-flag
              end-call
              call WIN32 "RegSetValueExA"
                 using by value          key-handle
                       BY REFERENCE      z"Options"
                       by value          0
                       by value          Type-hexa
                       BY REFERENCE      key-value-buffer
                       by value          key-value-length
              end-call

              call WIN32 "RegCreateKeyExA"
                          using by value     HKEY-LOCAL-MACHINE
                                BY REFERENCE MF
                                by value     0
                                BY REFERENCE Z" "
                                by value     NON-VOLATILE
                                by value     KEY-ALL-ACCESS
                                by value     0
                                BY REFERENCE key-handle
                                BY REFERENCE create-open-flag
              end-call
              call WIN32 "RegSetValueExA"
                 using by value          key-handle
                       BY REFERENCE      z"Options"
                       by value          0
                       by value          Type-hexa
                       BY REFERENCE      key-value-buffer
                       by value          key-value-length
              end-call
              call WIN32 "RegCreateKeyExA"
                          using by value     HKEY-LOCAL-MACHINE
                                BY REFERENCE NE
                                by value     0
                                BY REFERENCE Z" "
                                by value     NON-VOLATILE
                                by value     KEY-ALL-ACCESS
                                by value     0
                                BY REFERENCE key-handle
                                BY REFERENCE create-open-flag
              end-call
              call WIN32 "RegSetValueExA"
                 using by value          key-handle
                       BY REFERENCE      z"Options"
                       by value          0
                       by value          Type-hexa
                       BY REFERENCE      key-value-buffer
                       by value          key-value-length
              end-call.

       FIM-REGEDIT. EXIT.
       500-COBWARE-SORT SECTION.

           MOVE LOW-VALUES TO SORTWK-LIXO
           RELEASE SORTWK-REG.

       600-SORT-COBWARE SECTION.

           RETURN SORTWK
                  AT END CONTINUE.

       600-FIM SECTION.

       END PROGRAM COBWARE.

