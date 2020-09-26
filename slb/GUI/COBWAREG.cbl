       IDENTIFICATION DIVISION.
       PROGRAM-ID.    COBWAREG.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  31/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Carrega objetos modo grafico                 *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.  CALL-CONVENTION 74 IS WINAPI
                       DECIMAL-POINT      IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

   mov*    SELECT SP2MOV ASSIGN TO DISK
   mov*           ORGANIZATION  IS BINARY SEQUENTIAL
   mov*           FILE STATUS   IS FS-SP2MOV.

       DATA DIVISION.

   mov*FD  SP2MOV
   mov*    LABEL RECORD IS STANDARD
   mov*    VALUE OF FILE-ID IS LB-SP2MOV.
   mov*
   mov*01  SP2MOV-REG PIC X(10).

       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       78  SM-CXSCREEN       value           0.
       78  SM-CYSCREEN       value           1.
       1   GetSystemMetricsReturn  pic xxxx    comp-5.
       1   screenHeight            pic xxxx    comp-5.
       1   screenWidth             pic xxxx    comp-5.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 SP2VAL                   PIC  X(003) VALUE SPACE.
           05 LINHA-COMANDO            PIC  X(255) VALUE SPACES.
           05 LINHA-COMANDO-UPPER      PIC  X(255) VALUE SPACES.
           05 CWTEST                   PIC  X(255) VALUE '24800000'.
           05 REDEFINES CWTEST.
              10 W24                   PIC  9(002).
              10 W80                   PIC  9(002).
              10 W-L                   PIC  9(002).
              10 W-C                   PIC  9(002).
              10 RUNTITLE              PIC  X(080).
           05 NEWCMD                   PIC  X(255) VALUE SPACES.
           05 TI                       PIC  9(003) VALUE 0.
           05 TN                       PIC  9(003) VALUE 0.
           05 CWMENU-CLOSE      PIC  X(2)   VALUE 'OF'.
           05 CWFULLCLOSE       PIC  X(050) VALUE SPACES.
           05 COLOR-STATUS      PIC  X      VALUE '0'.
              88 COLOR-ON                   VALUE '1'.
              88 COLOR-OFF                  VALUE '0' '3'.
              88 COLOR-CLOSED               VALUE '3'.
           05 CWGETL            PIC  X(003) VALUE SPACES.
           05 CWINDF            PIC  X(010) VALUE SPACES.
           05 CWENDK            PIC  X(003) VALUE SPACES.
           05 FONTE             PIC  X(030) VALUE "0".
           05 RESOLUTION        PIC  X(001) VALUE "0".
              88 RESOLUTION-OK  VALUE "1" "2" "3" "4".
           05 NUMERO            PIC  9(018) VALUE 0.
           05 CWNUMERO          PIC  X(018) VALUE SPACES.
           05 MF                PIC  X(080) VALUE SPACES.
           05 OS2LIBPATH        PIC  X(050) VALUE SPACES.
   mov*    05 ER-SP2MOV.
   mov*       10 FS-SP2MOV      PIC  X(002) VALUE "00".
   mov*       10 LB-SP2MOV      PIC  X(050) VALUE "SP2.MOV".
      *    05 WIDTH             PIC  9(002) VALUE 0.
      *    05 HEIGHT            PIC  9(002) VALUE 0.
           05 OS                PIC  X(010) VALUE SPACES.
           05 CW                PIC  9(001) VALUE 0.
           05 CWLITS            PIC  X(003) VALUE SPACES.
           05 CWACCENT          PIC  X(003) VALUE SPACES.
           05 B          COMP-X PIC  9(002) VALUE 0.
           05 F          COMP-X PIC  9(002) VALUE 0.
           05 RETRY      COMP-X PIC  9(002) VALUE 0.
           05 I                 PIC  9(003) VALUE 0.
           05 II                PIC  9(003) VALUE 0.
           05 OK                PIC  X(006) VALUE X"ADACAFFACADA".
      *    05 XRX                           VALUE X"0C06220508DB".
      *       10 XRX-BYTE OCCURS 6    PIC  9(002) COMP-X.
           05 STATIC                  PIC  X(001) VALUE X"70".
           05 ENTRY-ATTR              PIC  X(001) VALUE X"F0".
           05 DISABLE-ATTR            PIC  X(001) VALUE X"70".
           05 CURCOLOR                PIC  X(001) VALUE X"00".
           05 FW                      PIC  9(002) VALUE 0.
           05 FH                      PIC  9(002) VALUE 0.
       77  hwnd       COMP-5 PIC S9(009) VALUE 0.
       77  RETORNO    COMP-5 PIC S9(009) VALUE 0.
       77  CWCONF-FS                   PIC X(003) VALUE SPACES.

       COPY CWSPWS.

       LINKAGE SECTION.

       01   WD-TITLE   PIC X(300).
       01   TASK       PIC 9(006).
       01   STATUS-DEF PIC X(124).

       PROCEDURE DIVISION USING WD-TITLE TASK STATUS-DEF.

           DISPLAY "CWGETL" UPON ENVIRONMENT-NAME
           ACCEPT   CWGETL  FROM ENVIRONMENT-VALUE
           ON 1
              PERFORM CMDLINE THRU FIM-CMDLINE
              DISPLAY "OS2LIBPATH" UPON ENVIRONMENT-NAME
              ACCEPT   OS2LIBPATH FROM ENVIRONMENT-VALUE
              DISPLAY "CWCONF_FS" UPON ENVIRONMENT-NAME
              ACCEPT  CWCONF-FS   FROM ENVIRONMENT-VALUE
              IF   CWCONF-FS = SPACES
                   DISPLAY "CWCONF-FS" UPON ENVIRONMENT-NAME
                   ACCEPT  CWCONF-FS   FROM ENVIRONMENT-VALUE
              END-IF
              INSPECT CWCONF-FS CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY CWCONF-FS UPON ENVIRONMENT-VALUE
              IF  CWCONF-FS = "ON"
                  CALL "COBWARE" USING "FS"
              END-IF
              GOBACK.

           ON 1 GO TO API.
           DISPLAY "CWCOLOR-STATUS" UPON ENVIRONMENT-NAME
           ACCEPT  COLOR-STATUS FROM ENVIRONMENT-VALUE
           IF COLOR-CLOSED
              DISPLAY 'CWMENU-CLOSE' UPON ENVIRONMENT-NAME
              MOVE SPACES TO CWMENU-CLOSE
              ACCEPT CWMENU-CLOSE FROM ENVIRONMENT-VALUE
              GO TO API
           END-IF
           GO TO PAN.

       API.

           CALL "CWATTR" USING STATIC ENTRY-ATTR DISABLE-ATTR CURCOLOR

           DISPLAY "CWLITS" UPON ENVIRONMENT-NAME
           ACCEPT CWLITS FROM ENVIRONMENT-VALUE
           INSPECT CWLITS CONVERTING MINUSCULAS TO MAIUSCULAS
           DISPLAY "CWACCENT" UPON ENVIRONMENT-NAME
           ACCEPT CWACCENT FROM ENVIRONMENT-VALUE
           INSPECT CWACCENT CONVERTING MINUSCULAS TO MAIUSCULAS
           COPY CWSPPD.
           CANCEL "CWUNIX".

      *    DISPLAY "SP2SGN" UPON ENVIRONMENT-NAME
      *    DISPLAY "1"      UPON ENVIRONMENT-VALUE
           DISPLAY "SP2END" UPON ENVIRONMENT-NAME
           MOVE "6"           TO SP2VAL
           ACCEPT SP2VAL    FROM ENVIRONMENT-VALUE
           DISPLAY SP2VAL   UPON ENVIRONMENT-VALUE
      *    DISPLAY "SP2HID" UPON ENVIRONMENT-NAME
      *    DISPLAY "1"      UPON ENVIRONMENT-VALUE
           DISPLAY "SP2DEC" UPON ENVIRONMENT-NAME
      *    DISPLAY "3"      UPON ENVIRONMENT-VALUE
           MOVE "1"           TO SP2VAL
           ACCEPT SP2VAL    FROM ENVIRONMENT-VALUE
           DISPLAY SP2VAL   UPON ENVIRONMENT-VALUE
bush  *    DISPLAY "0"      UPON ENVIRONMENT-VALUE
           DISPLAY "SP2EDT" UPON ENVIRONMENT-NAME
           MOVE "6"           TO SP2VAL
           ACCEPT SP2VAL    FROM ENVIRONMENT-VALUE
           DISPLAY SP2VAL   UPON ENVIRONMENT-VALUE
           DISPLAY "SP2F10" UPON ENVIRONMENT-NAME
           MOVE "1"           TO SP2VAL
           ACCEPT SP2VAL    FROM ENVIRONMENT-VALUE
           DISPLAY SP2VAL   UPON ENVIRONMENT-VALUE
           DISPLAY "SP2RSM" UPON ENVIRONMENT-NAME
           MOVE "#"           TO SP2VAL
           ACCEPT SP2VAL    FROM ENVIRONMENT-VALUE
           DISPLAY SP2VAL   UPON ENVIRONMENT-VALUE
           DISPLAY "SP2VST" UPON ENVIRONMENT-NAME
           MOVE "1"           TO SP2VAL
           ACCEPT SP2VAL    FROM ENVIRONMENT-VALUE
           DISPLAY SP2VAL   UPON ENVIRONMENT-VALUE

           DISPLAY "CWRESOLUTION" UPON ENVIRONMENT-NAME
           ACCEPT  RESOLUTION     FROM ENVIRONMENT-VALUE
           MOVE "MF SVGA (800x600.CP437)" TO MF
           IF  NOT RESOLUTION-OK
               call WINAPI 'GetSystemMetrics'
                   using by value SM-CYSCREEN size 4
                   returning GetSystemMetricsReturn
               move GetSystemMetricsReturn to screenHeight
               call WINAPI  'GetSystemMetrics'
                   using by value SM-CXSCREEN size 4
                   returning GetSystemMetricsReturn
               move GetSystemMetricsReturn to screenWidth
               MOVE 0                      TO RESOLUTION
           END-IF
           evaluate true
           when (screenWidth = 1024 AND screenHeight = 768)
             OR RESOLUTION = 1
                MOVE "MF XGA (1024x768.CP437)" TO MF
                MOVE 1                         TO RESOLUTION
                DISPLAY "SP2OIC" UPON ENVIRONMENT-NAME
                MOVE "1"           TO SP2VAL
                ACCEPT SP2VAL    FROM ENVIRONMENT-VALUE
                DISPLAY SP2VAL   UPON ENVIRONMENT-VALUE
                DISPLAY "SP2OWD" UPON ENVIRONMENT-NAME
                MOVE "850"         TO SP2VAL
                ACCEPT SP2VAL    FROM ENVIRONMENT-VALUE
                DISPLAY SP2VAL   UPON ENVIRONMENT-VALUE
      *         DISPLAY "800"    UPON ENVIRONMENT-VALUE
                DISPLAY "SP2OHT" UPON ENVIRONMENT-NAME
                MOVE "600"         TO SP2VAL
                ACCEPT SP2VAL    FROM ENVIRONMENT-VALUE
                DISPLAY SP2VAL   UPON ENVIRONMENT-VALUE
      *         DISPLAY "555"    UPON ENVIRONMENT-VALUE
           when (screenWidth = 640 AND screenHeight = 480)
             OR RESOLUTION = 2
                MOVE "MF VGA (640x480.CP437)" TO MF
                MOVE 2                        TO RESOLUTION
                DISPLAY "SP2OIC" UPON ENVIRONMENT-NAME
                MOVE "1"           TO SP2VAL
                ACCEPT SP2VAL    FROM ENVIRONMENT-VALUE
                DISPLAY SP2VAL   UPON ENVIRONMENT-VALUE
                DISPLAY "SP2OWD" UPON ENVIRONMENT-NAME
                MOVE "800"         TO SP2VAL
                ACCEPT SP2VAL    FROM ENVIRONMENT-VALUE
                DISPLAY SP2VAL   UPON ENVIRONMENT-VALUE
                DISPLAY "SP2OHT" UPON ENVIRONMENT-NAME
                MOVE "600"         TO SP2VAL
                ACCEPT SP2VAL    FROM ENVIRONMENT-VALUE
                DISPLAY SP2VAL   UPON ENVIRONMENT-VALUE
ana        when (screenWidth = 12801 AND screenHeight = 10241)
             OR RESOLUTION = 3
                MOVE "MF XGA (1024x768.CP437)" TO MF
                MOVE 3                         TO RESOLUTION
                DISPLAY "SP2OIC" UPON ENVIRONMENT-NAME
                MOVE "1"           TO SP2VAL
                ACCEPT SP2VAL    FROM ENVIRONMENT-VALUE
                DISPLAY SP2VAL   UPON ENVIRONMENT-VALUE
                DISPLAY "SP2OWD" UPON ENVIRONMENT-NAME
                MOVE "850"         TO SP2VAL
                ACCEPT SP2VAL    FROM ENVIRONMENT-VALUE
                DISPLAY SP2VAL   UPON ENVIRONMENT-VALUE
                DISPLAY "SP2OHT" UPON ENVIRONMENT-NAME
                MOVE "600"         TO SP2VAL
                ACCEPT SP2VAL    FROM ENVIRONMENT-VALUE
                DISPLAY SP2VAL   UPON ENVIRONMENT-VALUE
ana        when RESOLUTION = 4
                MOVE "MF SVGA (800x600.CP437)" TO MF
                MOVE 4                         TO RESOLUTION
                DISPLAY "SP2OIC" UPON ENVIRONMENT-NAME
                MOVE "1"           TO SP2VAL
                ACCEPT SP2VAL    FROM ENVIRONMENT-VALUE
                DISPLAY SP2VAL   UPON ENVIRONMENT-VALUE
           end-evaluate
           DISPLAY "CWRESOLUTION" UPON ENVIRONMENT-NAME
           DISPLAY RESOLUTION     UPON ENVIRONMENT-VALUE
           MOVE LOW-VALUES TO SP2-FO-DATA

           DISPLAY "OS"            UPON ENVIRONMENT-NAME
           ACCEPT   OS             FROM ENVIRONMENT-VALUE
           IF  OS = "Windows_NT"
               MOVE "Lucida Console" TO SP2-FO-NAME *> Erro no 98 (Proporcional)
           ELSE
               MOVE "Courier"  TO SP2-FO-NAME
           END-IF
           DISPLAY "CWFONT-STATIC-NAME"   UPON ENVIRONMENT-NAME
           MOVE    SP2-FO-NAME              TO FONTE
           ACCEPT  SP2-FO-NAME            FROM ENVIRONMENT-VALUE
           DISPLAY "CWFONT-STATIC-WEIGHT" UPON ENVIRONMENT-NAME
           MOVE    "b"                      TO SP2-FO-WEIGHT
           ACCEPT  SP2-FO-WEIGHT          FROM ENVIRONMENT-VALUE
           IF   SP2-FO-WEIGHT = "0"
                MOVE X"00" TO SP2-FO-WEIGHT
           END-IF

           DISPLAY "CWFONT-STATIC-WIDTH"  UPON ENVIRONMENT-NAME
           PERFORM AJUSTA
           IF  NUMERO NOT = 0
               MOVE NUMERO TO SP2-FO-WIDTH
           ELSE
               MOVE 9      TO SP2-FO-WIDTH
           END-IF

           DISPLAY "CWFONT-STATIC-HEIGHT" UPON ENVIRONMENT-NAME
           PERFORM AJUSTA
           IF  NUMERO NOT = 0
               MOVE NUMERO TO SP2-FO-HEIGHT
           ELSE
               MOVE 0      TO SP2-FO-HEIGHT
           END-IF

           MOVE 1          TO SP2-FO-ID
           CALL SP2     USING SP2-SET-FONT-DEF SP2-FONT-DEF

           IF   SP2-FO-NAME NOT = FONTE
                DISPLAY "CWSCRE-STATIC"  UPON ENVIRONMENT-NAME
                DISPLAY "1"              UPON ENVIRONMENT-VALUE
           END-IF

           MOVE "Courier New"  TO SP2-FO-NAME
           DISPLAY "CWFONT-FIELD-NAME"  UPON ENVIRONMENT-NAME
           MOVE    SP2-FO-NAME            TO FONTE
           ACCEPT  SP2-FO-NAME     FROM ENVIRONMENT-VALUE
           DISPLAY SP2-FO-NAME     UPON ENVIRONMENT-VALUE
           DISPLAY "CWFONT-FIELD-WEIGHT" UPON ENVIRONMENT-NAME
           MOVE    X"00"             TO SP2-FO-WEIGHT
           ACCEPT  SP2-FO-WEIGHT   FROM ENVIRONMENT-VALUE
           IF   SP2-FO-WEIGHT = "0"
                MOVE X"00" TO SP2-FO-WEIGHT
           END-IF

           DISPLAY "CWFONT-FIELD-WIDTH"  UPON ENVIRONMENT-NAME
           PERFORM AJUSTA
           IF  NUMERO NOT = 0
               MOVE NUMERO TO SP2-FO-WIDTH
           ELSE
               MOVE 9      TO SP2-FO-WIDTH
           END-IF

           DISPLAY "CWFONT-FIELD-HEIGHT" UPON ENVIRONMENT-NAME
           PERFORM AJUSTA
           IF  NUMERO NOT = 0
               MOVE NUMERO TO SP2-FO-HEIGHT
           ELSE
               MOVE 0      TO SP2-FO-HEIGHT
           END-IF
           MOVE 2          TO SP2-FO-ID
           CALL SP2     USING SP2-SET-FONT-DEF SP2-FONT-DEF

           IF   SP2-FO-NAME NOT = FONTE
                DISPLAY "CWUSER-FIELD"  UPON ENVIRONMENT-NAME
                DISPLAY "1"             UPON ENVIRONMENT-VALUE
           END-IF

           MOVE "Courier"  TO SP2-FO-NAME
           MOVE "b"        TO SP2-FO-WEIGHT
           MOVE 6          TO SP2-FO-WIDTH
           MOVE 0          TO SP2-FO-HEIGHT
           MOVE 3          TO SP2-FO-ID
           CALL SP2     USING SP2-SET-FONT-DEF SP2-FONT-DEF

ana        MOVE LOW-VALUES TO SP2-FO-DATA
           MOVE MF         TO SP2-FO-NAME
           MOVE 4          TO SP2-FO-ID
           CALL SP2     USING SP2-SET-FONT-DEF SP2-FONT-DEF

           MOVE LOW-VALUES      TO SP2-WD-DATA
           MOVE "n"             TO SP2-WD-SBAR-SW
           MOVE "m"             TO SP2-WD-BOR-TYPE
      *    MOVE X"20"           TO SP2-WD-MORE-OPTIONS
zoo   *    MOVE X"31"           TO SP2-WD-MORE-OPTIONS
LeÆo       MOVE X"30"           TO SP2-WD-MORE-OPTIONS
NOmim *    IF  OS = "Windows_NT"
NOmim *    AND os2libpath = SPACES
NOmim *        MOVE X"03"       TO SP2-WD-OPTIONS-3
ksat           MOVE X"02"       TO SP2-WD-OPTIONS-3
NOmim *    END-IF
           DISPLAY 'CWMENU-WD-OPTIONS-3' UPON ENVIRONMENT-NAME
           ACCEPT SP2-WD-OPTIONS-3  FROM ENVIRONMENT-VALUE
JK         MOVE "r"             TO SP2-WD-BOR-TYPE
           MOVE 'cwmenug'       TO SP2-ND-NAME
           DISPLAY "CWICON"  UPON ENVIRONMENT-NAME
           ACCEPT SP2-ND-NAME FROM ENVIRONMENT-VALUE
           CALL SP2   USING SP2-SET-ICON-FILE-NAME SP2-NAME-DEF

           MOVE 10              to sp2-WD-CELL-WIDTH
           move 20              to SP2-WD-CELL-HEIGHT
           DISPLAY "CWCELL-WIDTH"  UPON ENVIRONMENT-NAME
           PERFORM AJUSTA
           IF  NUMERO NOT = 0
               MOVE NUMERO TO sp2-WD-CELL-WIDTH
           END-IF
           DISPLAY "CWCELL-HEIGHT" UPON ENVIRONMENT-NAME
           PERFORM AJUSTA
           IF  NUMERO NOT = 0
               MOVE NUMERO TO sp2-WD-CELL-HEIGHT
           END-IF

           MOVE W80             TO SP2-WD-WIDTH
                                   SP2-WD-TOT-WIDTH
           MOVE W24             TO SP2-WD-HEIGHT
                                   SP2-WD-TOT-HEIGHT
           IF W80 = 80 AND W24 = 24
              DISPLAY "CWLINES"  UPON ENVIRONMENT-NAME
              PERFORM AJUSTA
              IF  NUMERO NOT = 0
                  MOVE NUMERO TO SP2-WD-HEIGHT
              END-IF
31            MOVE 15         TO SP2-WD-ROW
              IF NUMERO = 25
                 MOVE 13      TO SP2-WD-ROW
              END-IF
agua  *          MOVE 00      TO SP2-WD-ROW
              MOVE 10         TO SP2-WD-COL
              EVALUATE RESOLUTION
                  WHEN 1 ADD 15 TO SP2-WD-COL
                  WHEN 3 ADD 15 TO SP2-WD-COL
              END-EVALUATE
           ELSE
              MOVE W-L TO SP2-WD-ROW
              MOVE W-C TO SP2-WD-COL
           END-IF
           MOVE -1              TO SP2-WD-TITLE-ROWS
           MOVE -1              TO SP2-WD-MENU-ROWS
           MOVE "CWMENU"        TO SP2-WD-NAME
                                   SP2-WD-PANEL-NAME
           MOVE X"03"           TO SP2-WD-COLR
           MOVE STATIC          TO SP2-WD-COLR
           MOVE WD-TITLE        TO SP2-WD-TITLE

           IF   CWLITS = "LOW"
                INSPECT SP2-WD-TITLE CONVERTING MAIUSCULAS TO MINUSCULAS
           END-IF
           IF   CWLITS = "UPP"
                INSPECT SP2-WD-TITLE CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF
           IF   CWACCENT = "OFF"
                INSPECT SP2-WD-TITLE CONVERTING ACENTOS-850
                                             TO ACENTOS-OFF
           ELSE
                INSPECT SP2-WD-TITLE CONVERTING ACENTOS-850
                                             TO ACENTOS-WINDOWS
           END-IF

>>         move "n"             to sp2-wd-init-sw
      *    DISPLAY "HIDE" UPON ENVIRONMENT-NAME
      *    ACCEPT sp2-wd-hide-sw FROM ENVIRONMENT-VALUE
      *    move "d"             to sp2-wd-hide-sw
      *    CALL SP2          USING SP2-SET-WINDOW-DEF SP2-WINDOW-DEF
           IF COLOR-CLOSED
           AND CWMENU-CLOSE = 'OF' OR 'of' OR 'Of' OR 'oF'
               GO TO PAN
           END-IF
           DISPLAY "CWINDF" UPON ENVIRONMENT-NAME
           ACCEPT   CWINDF  FROM ENVIRONMENT-VALUE
           IF CWINDF NOT = SPACES
              CALL CWINDF USING SP2-WINDOW-DEF
                ON EXCEPTION
                   CONTINUE
              END-CALL
           END-IF
   mov*    PERFORM TEST AFTER UNTIL FS-SP2MOV > "09" OR RETRY > 200
   mov*            OPEN INPUT SP2MOV
   mov*            IF   FS-SP2MOV < "10"
   mov*                 CLOSE SP2MOV
   mov*                 ADD  1 TO RETRY
   mov*            END-IF
   mov*    END-PERFORM
   mov*    CLOSE SP2MOV
           MOVE X"14"     TO SP2-WD-SYSTEM-MENU
alteni*    MOVE X"10"     TO SP2-WD-SYSTEM-MENU
   mov*    IF RETRY < 200
   mov*       OPEN OUTPUT SP2MOV
   mov*       MOVE LOW-VALUES TO SP2MOV-REG
   mov*       WRITE SP2MOV-REG
   mov*       CLOSE SP2MOV
   mov*       CALL SP2          USING SP2-OPEN-WINDOW SP2-WINDOW-DEF
   mov*       DELETE FILE SP2MOV
   mov*    ELSE
              CALL SP2          USING SP2-OPEN-WINDOW SP2-WINDOW-DEF
   mov*    END-IF
           MOVE "CWMENU" TO SP2-ND-NAME
           CALL SP2   USING SP2-ACTIVATE-WINDOW SP2-NAME-DEF

      *    CALL SP2          USING SP2-GET-WINDOW-DEF SP2-WINDOW-DEF.
           MOVE LOW-VALUES        TO SP2-PD-DATA
sav   *    move "y"               to sp2-pd-assume-dec
           MOVE SP2-WD-WIDTH      TO SP2-PD-WIDTH
           MOVE SP2-WD-HEIGHT     TO SP2-PD-HEIGHT
           MOVE SP2-WD-TOT-WIDTH  TO SP2-PD-TOT-WIDTH
           MOVE SP2-WD-TOT-HEIGHT TO SP2-PD-TOT-HEIGHT
           MOVE SP2-WD-ROW        TO SP2-PD-ROW
agua2            MOVE 00      TO SP2-PD-ROW
           MOVE SP2-WD-COL        TO SP2-PD-COL
agua2      MOVE 00                TO SP2-PD-COL
           MOVE "CWMENU"          TO SP2-PD-NAME
           MOVE X"21"             TO SP2-PD-MORE-OPTIONS
agua       MOVE X"00"             TO SP2-PD-MORE-OPTIONS
alla  *    MOVE X"00"             TO SP2-PD-MORE-OPTIONS
           MOVE SP2-KEY-LEFT      TO SP2-PD-LEFT
           MOVE SP2-KEY-RIGHT     TO SP2-PD-RIGHT
           MOVE SP2-KEY-UP        TO SP2-PD-UP
           MOVE SP2-KEY-DOWN      TO SP2-PD-DOWN
           MOVE SP2-KEY-TAB       TO SP2-PD-TAB
           MOVE SP2-KEY-BACKTAB   TO SP2-PD-BACKTAB
           MOVE SP2-KEY-BACKSPAC  TO SP2-PD-BACKSPAC
           MOVE SP2-KEY-DELETE    TO SP2-PD-DELETE
           MOVE SP2-KEY-INSERT    TO SP2-PD-INSERT
           MOVE SP2-KEY-HOME      TO SP2-PD-HOME
           DISPLAY "CWENDK"        UPON ENVIRONMENT-NAME
           ACCEPT  CWENDK          FROM ENVIRONMENT-VALUE
           INSPECT CWENDK    CONVERTING MINUSCULAS TO MAIUSCULAS
           IF CWENDK NOT = "ON"
              MOVE SP2-KEY-END    TO SP2-PD-END
           END-IF
           MOVE SP2-KEY-CTRL-HOME TO SP2-PD-HOME-PAN
           MOVE SP2-KEY-CTRL-END  TO SP2-PD-END-PAN
           MOVE SP2-KEY-ALT-0     TO SP2-PD-CTRL-KEY (01)
           MOVE SP2-KEY-ALT-1     TO SP2-PD-CTRL-KEY (02)
           MOVE SP2-KEY-ALT-2     TO SP2-PD-CTRL-KEY (03)
           MOVE SP2-KEY-ALT-3     TO SP2-PD-CTRL-KEY (04)
           MOVE SP2-KEY-ALT-4     TO SP2-PD-CTRL-KEY (05)
           MOVE SP2-KEY-ALT-5     TO SP2-PD-CTRL-KEY (06)
           MOVE SP2-KEY-ALT-6     TO SP2-PD-CTRL-KEY (07)
           MOVE SP2-KEY-ALT-7     TO SP2-PD-CTRL-KEY (08)
           MOVE SP2-KEY-ALT-8     TO SP2-PD-CTRL-KEY (09)
           MOVE SP2-KEY-ALT-9     TO SP2-PD-CTRL-KEY (10)
           MOVE SP2-KEY-ALT-A     TO SP2-PD-CTRL-KEY (11)
           MOVE SP2-KEY-ALT-B     TO SP2-PD-CTRL-KEY (12)
           MOVE SP2-KEY-ALT-C     TO SP2-PD-CTRL-KEY (13)
           MOVE SP2-KEY-ALT-D     TO SP2-PD-CTRL-KEY (14)
           MOVE SP2-KEY-ALT-E     TO SP2-PD-CTRL-KEY (15)
           MOVE SP2-KEY-ALT-EQUAL TO SP2-PD-CTRL-KEY (16)
           MOVE SP2-KEY-ALT-F     TO SP2-PD-CTRL-KEY (17)
           MOVE SP2-KEY-ALT-F1    TO SP2-PD-CTRL-KEY (18)
           MOVE SP2-KEY-ALT-F10   TO SP2-PD-CTRL-KEY (19)
           MOVE SP2-KEY-ALT-F11   TO SP2-PD-CTRL-KEY (20)
           MOVE SP2-KEY-ALT-F12   TO SP2-PD-CTRL-KEY (21)
           MOVE SP2-KEY-ALT-F2    TO SP2-PD-CTRL-KEY (22)
           MOVE SP2-KEY-ALT-F3    TO SP2-PD-CTRL-KEY (23)
           MOVE SP2-KEY-ALT-F4    TO SP2-PD-CTRL-KEY (24)
           MOVE SP2-KEY-ALT-F5    TO SP2-PD-CTRL-KEY (25)
           MOVE SP2-KEY-ALT-F6    TO SP2-PD-CTRL-KEY (26)
           MOVE SP2-KEY-ALT-F7    TO SP2-PD-CTRL-KEY (27)
           MOVE SP2-KEY-ALT-F8    TO SP2-PD-CTRL-KEY (28)
           MOVE SP2-KEY-ALT-F9    TO SP2-PD-CTRL-KEY (29)
           MOVE SP2-KEY-ALT-G     TO SP2-PD-CTRL-KEY (30)
           MOVE SP2-KEY-ALT-H     TO SP2-PD-CTRL-KEY (31)
           MOVE SP2-KEY-ALT-I     TO SP2-PD-CTRL-KEY (32)
           MOVE SP2-KEY-ALT-J     TO SP2-PD-CTRL-KEY (33)
           MOVE SP2-KEY-ALT-K     TO SP2-PD-CTRL-KEY (34)
           MOVE SP2-KEY-ALT-L     TO SP2-PD-CTRL-KEY (35)
           MOVE SP2-KEY-ALT-M     TO SP2-PD-CTRL-KEY (36)
           MOVE SP2-KEY-ALT-MINUS TO SP2-PD-CTRL-KEY (37)
           MOVE SP2-KEY-ALT-N     TO SP2-PD-CTRL-KEY (38)
           MOVE SP2-KEY-ALT-O     TO SP2-PD-CTRL-KEY (39)
           MOVE SP2-KEY-ALT-P     TO SP2-PD-CTRL-KEY (40)
           MOVE SP2-KEY-CTRL-P    TO SP2-PD-CTRL-KEY (40)
           MOVE SP2-KEY-ALT-Q     TO SP2-PD-CTRL-KEY (41)
           MOVE SP2-KEY-ALT-R     TO SP2-PD-CTRL-KEY (42)
           MOVE SP2-KEY-ALT-S     TO SP2-PD-CTRL-KEY (43)
           MOVE SP2-KEY-ALT-T     TO SP2-PD-CTRL-KEY (44)
           MOVE SP2-KEY-ALT-U     TO SP2-PD-CTRL-KEY (45)
           MOVE SP2-KEY-ALT-V     TO SP2-PD-CTRL-KEY (46)
           MOVE SP2-KEY-ALT-W     TO SP2-PD-CTRL-KEY (47)
           MOVE SP2-KEY-ALT-X     TO SP2-PD-CTRL-KEY (48)
           MOVE SP2-KEY-ALT-Y     TO SP2-PD-CTRL-KEY (49)
           MOVE SP2-KEY-ALT-Z     TO SP2-PD-CTRL-KEY (50)
           MOVE SP2-KEY-BACKTAB   TO SP2-PD-CTRL-KEY (51)
           MOVE SP2-KEY-CTRL-F11  TO SP2-PD-CTRL-KEY (52)
           MOVE SP2-KEY-CTRL-F12  TO SP2-PD-CTRL-KEY (53)
           MOVE SP2-KEY-CTRL-PGDN TO SP2-PD-CTRL-KEY (54)
           MOVE SP2-KEY-CTRL-PGUP TO SP2-PD-CTRL-KEY (55)
           MOVE SP2-KEY-DOWN      TO SP2-PD-CTRL-KEY (56)
           MOVE SP2-KEY-END       TO SP2-PD-CTRL-KEY (57)
           MOVE SP2-KEY-ENTER     TO SP2-PD-CTRL-KEY (58)
           MOVE SP2-KEY-ESC       TO SP2-PD-CTRL-KEY (59)
           MOVE SP2-KEY-F1        TO SP2-PD-CTRL-KEY (60)
           MOVE SP2-KEY-F10       TO SP2-PD-CTRL-KEY (61)
           MOVE SP2-KEY-F11       TO SP2-PD-CTRL-KEY (62)
           MOVE SP2-KEY-F12       TO SP2-PD-CTRL-KEY (63)
           MOVE SP2-KEY-F2        TO SP2-PD-CTRL-KEY (64)
           MOVE SP2-KEY-F3        TO SP2-PD-CTRL-KEY (65)
           MOVE SP2-KEY-F4        TO SP2-PD-CTRL-KEY (66)
           MOVE SP2-KEY-F5        TO SP2-PD-CTRL-KEY (67)
           MOVE SP2-KEY-F6        TO SP2-PD-CTRL-KEY (68)
           MOVE SP2-KEY-F7        TO SP2-PD-CTRL-KEY (69)
           MOVE SP2-KEY-F8        TO SP2-PD-CTRL-KEY (70)
           MOVE SP2-KEY-F9        TO SP2-PD-CTRL-KEY (71)
           MOVE SP2-KEY-HOME      TO SP2-PD-CTRL-KEY (72)
           MOVE SP2-KEY-PGDN      TO SP2-PD-CTRL-KEY (73)
           MOVE SP2-KEY-PGUP      TO SP2-PD-CTRL-KEY (74)
           MOVE SP2-KEY-SHIFT-F1  TO SP2-PD-CTRL-KEY (75)
           MOVE SP2-KEY-SHIFT-F10 TO SP2-PD-CTRL-KEY (76)
           MOVE SP2-KEY-SHIFT-F11 TO SP2-PD-CTRL-KEY (77)
           MOVE SP2-KEY-SHIFT-F12 TO SP2-PD-CTRL-KEY (78)
           MOVE SP2-KEY-SHIFT-F2  TO SP2-PD-CTRL-KEY (79)
           MOVE SP2-KEY-SHIFT-F3  TO SP2-PD-CTRL-KEY (80)
           MOVE SP2-KEY-SHIFT-F4  TO SP2-PD-CTRL-KEY (81)
           MOVE SP2-KEY-SHIFT-F5  TO SP2-PD-CTRL-KEY (82)
           MOVE SP2-KEY-SHIFT-F6  TO SP2-PD-CTRL-KEY (83)
           MOVE SP2-KEY-SHIFT-F7  TO SP2-PD-CTRL-KEY (84)
           MOVE SP2-KEY-SHIFT-F8  TO SP2-PD-CTRL-KEY (85)
           MOVE SP2-KEY-SHIFT-F9  TO SP2-PD-CTRL-KEY (86)
           MOVE SP2-KEY-TAB       TO SP2-PD-CTRL-KEY (87)
           MOVE SP2-KEY-UP        TO SP2-PD-CTRL-KEY (88)
           MOVE SP2-KEY-TIMEOUT   TO SP2-PD-CTRL-KEY (89)
           MOVE X"04"             TO SP2-PD-OPTIONS-3
KSAT       MOVE X"24"             TO SP2-PD-OPTIONS-3
           DISPLAY 'CWMENU-PD-OPTIONS-3' UPON ENVIRONMENT-NAME
           ACCEPT SP2-PD-OPTIONS-3  FROM ENVIRONMENT-VALUE
           MOVE "m"               TO SP2-PD-WIN-BOR-TYPE
      *    MOVE X"0C"             TO SP2-PD-OPTIONS-3
           MOVE "n"               TO SP2-PD-CURS-SKIP
SG         MOVE "b"               TO SP2-PD-CURS-SHOW
           IF   CWGETL NOT = 'OFF'
                MOVE -1           TO SP2-PD-MSG-LEN
           END-IF.
      *    move "y" to sp2-pd-initial-sw.

       PAN.

           IF   CWGETL NOT = 'OFF'
                MOVE STATUS-DEF     TO SP2-PD-MSG-TEXT
           END-IF
      *    CALL "CWRESE"
           CALL SP2    USING SP2-RESERVE-MEMORY SP2-NULL-PARM.
           CALL "CWCOLOR" USING "ON"
           CALL SP2       USING SP2-SET-PANEL-DEF SP2-PANEL-DEF

           IF  CW = 0
           AND (CWGETL NOT = 'OFF')
               MOVE 1 TO CW
               call "cobware" USING OK
      *        PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF XRX
      *                 ADD 20 TO XRX-BYTE (I)
      *        END-PERFORM
      *        IF   OK NOT = XRX
      *             STOP RUN
      *        END-IF
           END-IF
           IF COLOR-CLOSED
              DISPLAY "CWFULLCLOSE" UPON ENVIRONMENT-NAME
              ACCEPT   CWFULLCLOSE  FROM ENVIRONMENT-VALUE
              IF  CWFULLCLOSE NOT = SPACES
                  DISPLAY "CWFULL"      UPON ENVIRONMENT-NAME
                  DISPLAY  CWFULLCLOSE  UPON ENVIRONMENT-VALUE
              END-IF
              CALL 'CWUSER' USING X'04'
              SET COLOR-ON TO TRUE
           END-IF
           GOBACK.

       GUI-START.

       AJUSTA.

           MOVE SPACES TO CWNUMERO
           MOVE 0      TO NUMERO
           ACCEPT CWNUMERO FROM ENVIRONMENT-VALUE
           MOVE 18 TO II
           PERFORM VARYING I FROM LENGTH OF CWNUMERO BY -1
                   UNTIL I = 0
                   IF  CWNUMERO (I: 1) NUMERIC
                       MOVE CWNUMERO (I: 1)
                         TO   NUMERO (II: 1)
                       SUBTRACT 1 FROM II
                   END-IF
           END-PERFORM.

       FIM-AJUSTA. EXIT.

       CMDLINE.

           ACCEPT LINHA-COMANDO FROM COMMAND-LINE
           IF LINHA-COMANDO NOT = SPACES
              MOVE LINHA-COMANDO TO LINHA-COMANDO-UPPER
              INSPECT LINHA-COMANDO-UPPER CONVERTING MINUSCULAS
                                                  TO MAIUSCULAS
              PERFORM VARYING I FROM 1 BY 1
                        UNTIL I > (LENGTH OF LINHA-COMANDO - 4)
                     IF LINHA-COMANDO-UPPER (I:3) = '/W:'
                        ADD 3 TO I
                        PERFORM UNTIL I > (LENGTH LINHA-COMANDO - 4)
                                   OR LINHA-COMANDO-UPPER (I:1) = ' '
                                ADD 1 TO TI
                                MOVE LINHA-COMANDO (I:1)
                                  TO CWTEST (TI:1)
                                ADD 1 TO I
                        END-PERFORM
                        ADD 1 TO TI
                     ELSE
                        ADD 1 TO TN
                        MOVE LINHA-COMANDO (I:1)
                          TO NEWCMD (TN:1)
                     END-IF
              END-PERFORM
              IF W80 NOT NUMERIC
              OR W80 > 80
                 MOVE 80 TO W80
              END-IF
              IF W24 NOT NUMERIC
              OR W24 > 24
                 MOVE 24 TO W24
              END-IF
              IF W-L NOT NUMERIC
                 MOVE 0 TO W-L
              END-IF
              IF W-C NOT NUMERIC
                 MOVE 0 TO W-C
              END-IF
              DISPLAY 'CWRUNTITLE' UPON ENVIRONMENT-NAME
              DISPLAY RUNTITLE     UPON ENVIRONMENT-VALUE
              DISPLAY NEWCMD UPON COMMAND-LINE
           END-IF.

       FIM-CMDLINE. EXIT.

       END PROGRAM COBWAREG.
