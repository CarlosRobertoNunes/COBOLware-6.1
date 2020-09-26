       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWPATH.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  26/09/2000.
       SECURITY.      *************************************************
                      *                                               *
                      *  Seleciona arquivo de pastas                  *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA
           call-convention 74 is WinAPI.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PASTAS ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-PASTAS.

           select teste assign to disk
                  organization is binary sequential
                  file status  is fs-teste.

       DATA DIVISION.
       FILE SECTION.

       FD  PASTAS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-PASTAS.

       01  PASTAS-REG.
           05 PASTAS-FN-16              PIC  X(008).
           05 FILLER                    PIC  X(001).
           05 PASTAS-FT-16              PIC  X(003).
           05 FILLER                    PIC  X(034).
           05 PASTAS-NOME-32            PIC  X(050).
           05 FILLER                    PIC  X(050).

       fd  teste value of file-id lb-teste.
       01  teste-reg pic x.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 er-teste.
              10 fs-teste              pic  x(002) value spaces.
              10 lb-teste              pic  x(255) value spaces.
           05 FILENAME                 PIC  X(100) VALUE SPACES.
           05 PATH                     PIC  X(255) VALUE SPACES.
           05 COBOLWARE                PIC  X(050) VALUE SPACES.
           05 CWTITLE                  PIC  X(255) VALUE SPACES.
           05 EXTENSAO                 PIC  X(050) VALUE SPACES.
           05 TES                      PIC  X(005) VALUE SPACES.
           05 PONTO                    PIC  9(003) VALUE 0.
           05 MIN                      PIC  9(002) VALUE 0.
           05 BUFFER                   PIC  X(078) VALUE SPACES.
           05 COMANDO-UNIX             PIC  X(255) VALUE SPACES.
           05 NOME                     PIC  X(030) VALUE SPACES.
           05 TASK                     PIC  9(006) VALUE 0.
           05 ENVTASK                  PIC  X(006) VALUE SPACES.
           05 TIPO                     PIC  X(001) VALUE SPACES.
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 FIM                      PIC  9(003) VALUE 0.
           05 FIM2                     PIC  9(003) VALUE 0.
           05 LS                       PIC  9(003) VALUE 0.
           05 DIRMODE                  PIC  9(001) VALUE 0.
           05 TABELA-CORES.
              10 COR PIC 9(002) COMP-X OCCURS 256.
           05 TABELA-MOLDURA.
              10 BASE-MOLDURA      PIC  X(008) OCCURS 9.
           05 F                    PIC  9(003) VALUE 0.
           05 B                    PIC  9(003) VALUE 0.
           05 X.
              10 LX                PIC  9(002) VALUE 0.
              10 CX                PIC  9(002) VALUE 0.
              10 SX                PIC  9(002) VALUE 0.
           05 DEFAULTx             PIC  X(050) VALUE SPACES.
           05 TITULO               PIC  X(068) VALUE SPACES.
           05 RESIDENTE            PIC  9(001) VALUE 0.
           05 CORINGA              PIC  X(050) VALUE SPACES.
           05 RETURN-STATUS        PIC  9(002) COMP-X VALUE 0.
           05 SIZE-OLD-DIR         PIC  9(002) COMP-X VALUE 255.
           05 OLD-DRIVE            PIC  X(001) VALUE SPACE.
           05 OLD-DIRECTORY        PIC  X(255) VALUE SPACES.
           05 NEW-DRIVE            PIC  X(001) VALUE SPACE.
           05 NEW-DIRECTORY        PIC  X(255) VALUE SPACES.
           05 NEWDIR               PIC  X(255) VALUE SPACES.
           05 ER-PASTAS.
              10 FS-PASTAS         PIC  X(002) VALUE "00".
              10 LB-PASTAS         PIC  X(255) VALUE SPACES.
           05 DRIVES               PIC  9(008) COMP-X.
           05 FILES                PIC  9(009) VALUE 0.
           05 ATT                  PIC  9(002) VALUE 0.
           05 DIR                  PIC  9(001) VALUE 0.
           05 MAX                  PIC  9(002) VALUE 0.
           05 ROOT                 PIC  9(001) VALUE 0.
           05 RESTO                PIC  9(001) VALUE 0.
           05 I                    PIC  9(003) VALUE 0.
           05 E                    PIC  9(003) VALUE 0.
           05 I2                   PIC  9(002) VALUE 0.
           05 Y                    PIC  9(002) VALUE 0.
           05 K                    PIC  9(002) VALUE 0.
           05 WS-SAVE              PIC  X(078) VALUE SPACES.
           05 WS                   PIC  X(078) VALUE SPACES.
           05 WS2                  PIC  X(078) VALUE SPACES.
           05 DRIVES-TXT           VALUE SPACES.
              10 DRIVE             OCCURS 32 PIC X(001).
           05 LETRA                PIC 9(002) COMP-X VALUE 0.

       01  RESULT.
           05 F-ERROR              PIC  9(002) COMP-X VALUE 0.
           05 F-HANDLE             PIC  9(004) COMP-X VALUE 0.
           05 F-ATTROUT            PIC  9(002) COMP-X VALUE 0.
           05 F-TIME               PIC  9(004) COMP-X VALUE 0.
           05 F-DATE               PIC  9(004) COMP-X VALUE 0.
           05 F-SIZE               PIC  9(008) COMP-X VALUE 0.
           05 F-FILEOUT            PIC  X(078)        VALUE SPACES.
        01 FUNCTION-CODE           PIC  9(002) COMP-X VALUE 69.
        01 PARAMETER.
           05 F-ACTION             PIC  9(002) COMP-X VALUE 0.
           05 F-ATTRIN             PIC  9(002) COMP-X VALUE 0.
           05 F-FILEIN             PIC  X(078)        VALUE SPACES.

       COPY CWBOXC.
       COPY CWEXEC.
       COPY CWBOXW.
       COPY CWSEND.
       COPY CWUNIX.
      * API
       01  NULLPtr               procedure-pointer value NULL.
    43 78 SZ                 VALUE 9.
    58 77 int                typedef pic s9(SZ) comp-5.
    95 01 BOOL     is typedef int.
    51 77 WCHAR              typedef pic xx.
    55 77 data-pointer       typedef pointer.
    61 77 long               typedef pic s9(9) comp-5.
    62 77 ulong              typedef pic 9(9) comp-5.
    63 77 uns-long           typedef ulong.
    66 77 uns-short          typedef pic 9(4) comp-5.
   433 01 tagOFNA           is typedef.
   434    02 lstructsize       usage uns-long.
   435    02 hwndowner         usage data-pointer.
   436    02 1hinstance        usage data-pointer.
   437    02 lpstrfilter       usage data-pointer.
   438    02 lpstrcustomfilter usage data-pointer.
   439    02 nmaxcustfilter    usage uns-long.
   440    02 nfilterindex      usage uns-long.
   441    02 lpstrfile         usage data-pointer.
   442    02 nmaxfile          usage uns-long.
   443    02 lpstrfiletitle    usage data-pointer.
   444    02 nmaxfiletitle     usage uns-long.
   445    02 lpstrinitialdir   usage data-pointer.
   446    02 lpstrtitle        usage data-pointer.
   447    02 flags             usage uns-long.
   448    02 nfileoffset       usage uns-short.
   449    02 nfileextension    usage uns-short.
   450    02 lpstrdefext       usage data-pointer.
   451    02 lcustdata         usage long.
   452    02 lpfnhook          usage procedure-pointer.
   453    02 lptemplatename    usage data-pointer.
   455 01  OPENFILENAME      is typedef       usage tagofna.
  3477 01  OFNS                 OPENFILENAME.
  2500 78  GMEM-MOVEABLE                      value h"0002".
  2503 78  GMEM-ZEROINIT                      value h"0040".
  2507 78  GMEM-SHARE                         value h"2000".
  3486 01  hstyle               pic 9(8) comp-5.
       01  LenShort            usage uns-short.
  3500 77  boola                bool.
  3501     88  boolTRUE         value 1.
  3502     88  boolFALSE        value 0.
  2594 78  OFN-OVERWRITEPROMPT                value h"00000002".
  2595 78  OFN-HIDEREADONLY                   value h"00000004".
  2597 78  OFN-SHOWHELP                       value h"00000010".
  2604 78  OFN-PATHMUSTEXIST                  value h"00000800".
  2605 78  OFN-FILEMUSTEXIST                  value h"00001000".
       78  OFN-LONGNAMES                      value h"00200000".
       78  OFN-NOLONGNAMES                    value h"00040000".
       78  OFN-EXTENSIONDIFFERENT             value h"00000400".
  3490 01  STRINGS.
  3490     05 aString              pic x(999).
  3479     05 bString              pic x(256).
  3481     05 cString              pic x(256).
  3482     05 dString              pic x(256).
  3483     05 eString              pic x(256).

       LINKAGE SECTION.

       01  PARAMETROS-CWPATH.
           05 CWPATH-LINE                    PIC  9(002).
           05 CWPATH-COLUMN                  PIC  9(002).
           05 CWPATH-PATH                    PIC  X(078).
           05 CWPATH-PATH-LINE               PIC  9(002).
           05 CWPATH-PATH-COLUMN             PIC  9(002).
           05 CWPATH-DEFAULT                 PIC  X(012).
           05 CWPATH-TITLE                   PIC  X(078).
           05 CWPATH-COLOR-FRAME      COMP-X PIC  9(002).
           05 CWPATH-COLOR-BORDER     COMP-X PIC  9(002).
           05 CWPATH-COLOR-SHADE      COMP-X PIC  9(002).
           05 CWPATH-COLOR-BARR-MENU  COMP-X PIC  9(002).
           05 CWPATH-VERTICAL-LENGTH         PIC  9(002).
           05 CWPATH-DIR                     PIC  9(001).
              88 CWPATH-WITHOUT-DIR                      VALUE 0.
              88 CWPATH-WITH-DIR                         VALUE 1.
           05 CWPATH-DRIVES                  PIC  9(001).
              88 CWPATH-WITHOUT-DRIVES                   VALUE 0.
              88 CWPATH-WITH-DRIVES                      VALUE 1.
           05 CWPATH-NEWDIR                  PIC  9(001).
              88 CWPATH-WITHOUT-NEWDIR                   VALUE 0.
              88 CWPATH-WITH-NEWDIR                      VALUE 1.
           05 CWPATH-NEWFILE                 PIC  9(001).
              88 CWPATH-WITHOUT-NEWFILE                  VALUE 0.
              88 CWPATH-WITH-NEWFILE                     VALUE 1.
           05 CWPATH-FILE                    PIC  X(078).
           05 CWPATH-LONGNAME                PIC  X(255).
           66 CWPATH-ATTR RENAMES CWPATH-DIR THRU CWPATH-NEWFILE.

       PROCEDURE DIVISION USING PARAMETROS-CWPATH.

       000-INICIO.

           ON   1
                CALL "CWMOLD" USING TABELA-CORES TABELA-MOLDURA
                CALL "CWUNIX" USING PARAMETROS-CWUNIX
                CALL "CWGETU" USING NOME TASK PROGRAMA "?"
                MOVE SPACES TO LB-PASTAS
                STRING "CW"            DELIMITED BY SIZE
                       TASK            DELIMITED BY SIZE
                       ".DIR"  DELIMITED BY SIZE
                      INTO LB-PASTAS.

           IF  CWPATH-DEFAULT = SPACES
           AND CWPATH-PATH = SPACES
      *        MOVE '.'   TO CWPATH-PATH
               MOVE '*.*' TO CWPATH-DEFAULT
           END-IF

           INITIALIZE STRINGS extensao
           perform varying i
                   from length of CWPATH-DEFAULT
                   by -1 until i = 0
                   if CWPATH-DEFAULT (i: 1) = "."
                      add 1 to i
                      move CWPATH-DEFAULT (i: ) to extensao
                      exit perform
                   end-if
           end-perform
           if extensao = spaces
              perform varying i from length of CWPATH-PATH by -1
                        until i = 0
                      if CWPATH-PATH  (i: 1) = "."
                         add 1 to i
                         move CWPATH-PATH (i: ) to extensao
                         exit perform
                      end-if
              end-perform
           end-if

KS         IF  CWPATH-DEFAULT = SPACES
KS             If  CWPATH-WITH-NEWFILE
KS                 STRING 'novo-'
KS                         task
KS                        '.' extensao DELIMITED BY SIZE
KS                   INTO CWPATH-DEFAULT
KS             ELSE
KS                 STRING '*.' extensao DELIMITED BY SIZE
KS                           INTO CWPATH-DEFAULT
KS             END-IF
KS         END-IF

           MOVE COR (CWPATH-COLOR-FRAME + 1) TO F
           MOVE 0                            TO B
           PERFORM UNTIL F < 16
                   SUBTRACT 16 FROM F
                   ADD      1    TO B
           END-PERFORM
           MOVE CWPATH-PATH-LINE   TO LX
           MOVE CWPATH-PATH-COLUMN TO CX

           IF   CWUNIX-OFF
                INSPECT CWPATH-PATH CONVERTING "/" TO "\"
                INSPECT CWPATH-FILE CONVERTING "/" TO "\"
                CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                            RETURN-STATUS
           ELSE
                INSPECT CWPATH-PATH  CONVERTING "\" TO "/"
                INSPECT CWPATH-FILE  CONVERTING "\" TO "/"
           END-IF

           PERFORM VARYING E FROM LENGTH EXTENSAO BY -1
                           UNTIL E = 0 OR (EXTENSAO (E:1) NOT = SPACE)
                   CONTINUE
           END-PERFORM

           CALL "CBL_READ_DIR"   USING OLD-DIRECTORY
                                       SIZE-OLD-DIR
           IF   CWPATH-PATH = SPACES
                IF   CWUNIX-OFF
                     STRING OLD-DRIVE DELIMITED BY SPACE
                            ":\"           DELIMITED BY SIZE
                            OLD-DIRECTORY  DELIMITED BY size
                     INTO CWPATH-PATH
                     IF OLD-DIRECTORY NOT = SPACES
                        IF CWPATH-WITH-NEWFILE
                           STRING cwpath-path  delimited by space
                                  "\"          delimited by size
                                  CWPATH-DEFAULT delimited by "."
                                  into bString
                        ELSE
                           STRING cwpath-path  delimited by space
                                  "\*." Extensao(1:e) delimited by size
                                  into bString
                        END-IF
                     ELSE
                        IF CWPATH-WITH-NEWFILE
                           STRING cwpath-path  delimited by space
                                  CWPATH-DEFAULT delimited by "."
                                  into bString
                        ELSE
                           STRING cwpath-path  delimited by space
                                  "*." Extensao(1:e) delimited by size
                                  into bString
                        END-IF
                     END-IF
                ELSE
                     MOVE SPACES TO CWPATH-PATH
                     STRING "/"            DELIMITED BY SIZE
                            OLD-DIRECTORY  DELIMITED BY SPACE
                     INTO CWPATH-PATH
                END-IF
                MOVE SPACES TO CORINGA
           ELSE
                PERFORM VARYING I FROM 78 BY -1
                        UNTIL CWPATH-PATH (I: 1) = "/" OR "\"
                           OR I = 1
                              CONTINUE
                END-PERFORM
                IF CWPATH-WITH-NEWFILE
                   STRING cwpath-path (1: i) delimited by size
                          CWPATH-DEFAULT delimited by "."
                          into bString
                ELSE
                   STRING cwpath-path (1: i)
                          "*." Extensao(1:e) delimited by size
                          into bString
                END-IF
                MOVE CWPATH-PATH (I: ) TO CORINGA
                IF   CWUNIX-OFF
                AND (CWPATH-PATH (2: 1) NOT = ":")
                     MOVE SPACES TO WS
                     perform varying fim
                             from length OLD-DIRECTORY
                               by -1
                               until OLD-DIRECTORY (FIM: 1) not = space
                             continue
                     end-perform
                     perform varying fim2
                             from length OLD-DIRECTORY
                               by -1
                               until CWPATH-PATH   (FIM2: 1) not = space
                             continue
                     end-perform
                     STRING OLD-DRIVE DELIMITED BY SPACE
                            ":\"           DELIMITED BY SIZE
                            OLD-DIRECTORY  (1: FIM)
                                           DELIMITED BY size
                            "\"            DELIMITED BY SIZE
                            CWPATH-PATH    (1: FIM2)
                            DELIMITED BY size
                       INTO WS
                     MOVE WS TO CWPATH-PATH
                END-IF
                IF   CWUNIX-ON
                AND (CWPATH-PATH (1: 1) NOT = "/")
                     MOVE SPACES TO WS
                     STRING OLD-DIRECTORY  DELIMITED BY SPACE
                            "/"            DELIMITED BY SIZE
                            CWPATH-PATH    DELIMITED BY SPACE
                     INTO WS
                     MOVE WS TO CWPATH-PATH
                END-IF
           END-IF

           IF CWUNIX-WIN32
           OR CWUNIX-DOS32
              PERFORM 020-WINOPEN THRU 020-99-FIM
              if (extensao NOT = spaces)
              and (CWPATH-FILE NOT = SPACES)
                 MOVE 0 TO PONTO
                 PERFORM VARYING I FROM LENGTH OF CWPATH-FILE  BY -1
                           UNTIL I = 0
                          IF (CWPATH-FILE (I: 1) = ".")
                             MOVE I TO PONTO
                             exit perform
                          END-IF
                 END-PERFORM
                 IF PONTO = 0
                     PERFORM VARYING I FROM 1 BY 1
                               UNTIL I = LENGTH OF CWPATH-FILE
                               OR (CWPATH-FILE (I: 1) = SPACE)
                              CONTINUE
                     END-PERFORM
                     MOVE "." TO CWPATH-FILE (I: 1)
                     ADD  1   TO I
                     MOVE EXTENSAO TO CWPATH-FILE (I: )
                 END-IF
              END-IF
              MOVE CWPATH-FILE TO BUFFER
              MOVE SPACES      TO CWPATH-FILE
              STRING BUFFER DELIMITED BY X"00" INTO CWPATH-FILE
              IF CWPATH-FILE NOT = SPACES
                 MOVE 0 TO Y
                 PERFORM VARYING I FROM LENGTH OF CWPATH-FILE
                              BY -1 UNTIL I = 1
                              OR CWPATH-FILE (I: 1) = "\"
                          IF (CWPATH-FILE (I: 1) NOT = SPACE)
                          AND Y = 0
                             MOVE I TO Y
                          END-IF
                 END-PERFORM
                 MOVE CWPATH-FILE TO BUFFER
                 INSPECT BUFFER (1: Y) CONVERTING SPACE TO "_"
                 IF BUFFER NOT = CWPATH-FILE
                    IF  CWPATH-FILE (I: 1) = "\"
                        ADD 1 TO I
                    END-IF
                    MOVE CWPATH-FILE (I: ) TO BUFFER
                    DISPLAY "COBOLWARE" UPON ENVIRONMENT-NAME
                    ACCEPT COBOLWARE    FROM ENVIRONMENT-VALUE
                    DISPLAY "TASK"      UPON ENVIRONMENT-NAME
                    ACCEPT ENVTASK      FROM ENVIRONMENT-VALUE
                    MOVE SPACES TO CWEXEC-COMMAND
                    STRING COBOLWARE DELIMITED BY SPACE
                           "\getdir.exe" DELIMITED BY SIZE
                           INTO CWEXEC-COMMAND
                    CALL "CWEXEC" USING PARAMETROS-CWEXEC
                    MOVE SPACES TO LB-PASTAS
                    STRING "C:CW"         DELIMITED BY SIZE
                           ENVTASK         DELIMITED BY SPACE
                           ".BAT"  DELIMITED BY SIZE
                          INTO LB-PASTAS
                    OPEN INPUT PASTAS
                    MOVE SPACES TO PASTAS-REG
                    READ PASTAS
                    MOVE PASTAS-REG (11: 1) TO NEW-DRIVE
                    MOVE PASTAS-REG (14: )  TO NEW-DIRECTORY
                    CLOSE PASTAS
                    DELETE FILE PASTAS
                    PERFORM VARYING I FROM LENGTH OF BUFFER
                                 BY -1 UNTIL I = 1
                                 OR BUFFER (I: 1) NOT = SPACE
                             CONTINUE
                    END-PERFORM
                    IF    CWPATH-WITH-NEWFILE
                          ADD  1        TO I
                          MOVE "."      TO BUFFER (I: 1)
                          ADD  1        TO I
                          MOVE EXTENSAO TO BUFFER (I: )
                    ELSE
                          MOVE SPACES TO CWEXEC-COMMAND
                          STRING 'dir "' buffer (1: I) '"/x > '
                                  DELIMITED BY SIZE
                          LB-PASTAS DELIMITED BY SPACE
                          X"00" DELIMITED BY SIZE
                            INTO CWEXEC-COMMAND
                          CALL "system" USING CWEXEC-COMMAND
                          OPEN INPUT PASTAS
                          PERFORM 6 TIMES
                                  MOVE SPACES TO PASTAS-REG
                                  READ PASTAS
                          END-PERFORM
                          CLOSE PASTAS
                          DELETE FILE PASTAS
                          IF  PASTAS-REG (37: 12) NOT = SPACES
                              MOVE PASTAS-REG (37: 12) TO BUFFER
                              PERFORM VARYING I FROM LENGTH OF BUFFER
                                           BY -1 UNTIL I = 1
                                         OR (BUFFER (I: 1) NOT = SPACE)
                                  CONTINUE
                              END-PERFORM
                          END-IF
                    END-IF
                    MOVE SPACES TO CWPATH-FILE
                    IF  NEW-DIRECTORY NOT = SPACES
                        PERFORM VARYING Y FROM LENGTH OF NEW-DIRECTORY
                                BY -1 UNTIL Y = 1
                                      OR (NEW-DIRECTORY(Y:1) <> SPACE)
                              CONTINUE
                        END-PERFORM
                        STRING NEW-DRIVE ":\" DELIMITED BY SIZE
                               NEW-DIRECTORY(1:Y)
                                              DELIMITED BY SIZE
                               "\"            DELIMITED BY SIZE
                               BUFFER(1:I)    DELIMITED BY SIZE
                               INTO CWPATH-FILE
                    ELSE
                        STRING NEW-DRIVE ":\" DELIMITED BY SIZE
                               BUFFER(1:I)    DELIMITED BY SIZE
                               INTO CWPATH-FILE
                    END-IF
      *          ELSE
      *             IF    CWPATH-WITH-NEWFILE
      *                   ADD  1        TO Y
      *                   MOVE "."      TO CWPATH-FILE (Y: 1)
      *                   ADD  1        TO Y
      *                   MOVE EXTENSAO TO CWPATH-FILE (Y: )
      *             END-IF
                 END-IF
              END-IF
              CALL "PC_SET_DRIVE"   USING OLD-DRIVE
                                          RETURN-STATUS
              MOVE OLD-DIRECTORY TO BUFFER
              MOVE "\"           TO OLD-DIRECTORY
              MOVE BUFFER        TO OLD-DIRECTORY (2: )
              CALL "CBL_CHANGE_DIR" USING OLD-DIRECTORY
                                          SIZE-OLD-DIR
              MOVE ALL "0" TO CWPATH-ATTR
              GOBACK
           END-IF

           IF   DRIVES-TXT = SPACES
           AND  CWUNIX-OFF
                MOVE 64                  TO LETRA
                CALL "PC_FIND_DRIVES" USING DRIVES
                      ON EXCEPTION
                         CONTINUE
                      NOT ON EXCEPTION
                          MOVE 32 TO I
                          PERFORM UNTIL I = 0
                                  DIVIDE 2 INTO DRIVES GIVING DRIVES
                                       REMAINDER RESTO
                                  ADD 1 TO LETRA
                                  IF   RESTO = 1
                                       MOVE LETRA (1: 1) TO DRIVE (I)
                                  END-IF
                                  SUBTRACT 1 FROM I
                          END-PERFORM
                END-CALL
           END-IF
           MOVE 16                       TO F-ATTRIN
           MOVE CWPATH-PATH              TO F-FILEIN
           MOVE CWPATH-LINE              TO CWBOXC-LINE
           MOVE CWPATH-COLUMN            TO CWBOXC-COLUMN
           MOVE CWPATH-COLOR-FRAME       TO CWBOXC-COLOR-FRAME
                                            CWBOXW-COLOR-FRAME
           MOVE CWPATH-COLOR-BORDER      TO CWBOXC-COLOR-BORDER
                                            CWBOXW-COLOR-BORDER
           MOVE CWPATH-COLOR-SHADE       TO CWBOXC-COLOR-SHADE
                                            CWBOXW-COLOR-SHADE
           MOVE CWPATH-COLOR-BARR-MENU   TO CWBOXC-COLOR-BARR-MENU.
           IF  CWBOXC-COLOR-BARR-MENU = 111
               MOVE 112 TO CWBOXC-COLOR-BARR-MENU
           END-IF.

       010-MONTAR.

           PERFORM VARYING Y FROM 78 BY -1
                      UNTIL F-FILEIN (Y: 1) NOT = SPACE
                   CONTINUE
           END-PERFORM

           MOVE Y TO K

           IF   CWPATH-TITLE NOT = SPACES
                PERFORM VARYING Y FROM 78 BY -1
                           UNTIL CWPATH-TITLE (Y: 1) NOT = SPACE
                        CONTINUE
                END-PERFORM
                MOVE Y TO MIN
           ELSE
                MOVE 0 TO Y MIN
           END-IF

           MOVE CWPATH-TITLE           TO CWBOXC-TITLE
           MOVE CWPATH-VERTICAL-LENGTH TO CWBOXC-VERTICAL-LENGTH
           MOVE 0                      TO F-ACTION DIR FILES
           SET  CWBOXC-LOAD            TO TRUE
           MOVE NEWDIR                 TO DEFAULTx
           MOVE SPACES                 TO NEWDIR TIPO

           IF   CWBOXC-OPTION = "[-?-]"
                MOVE "Drive" TO CWBOXC-TITLE TIPO
                MOVE 7       TO CWBOXC-HORIZONTAL-LENGTH Y
                                CWBOXC-STRING-1-LENGTH
                MOVE 0       TO CWBOXC-STRING-2-LENGTH
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > 32
                        IF   DRIVE (I) NOT = SPACE
                             MOVE "[-"      TO CWBOXC-STRING-1
                             MOVE DRIVE (I) TO CWBOXC-STRING-1 (3: 1)
                             MOVE "-]"      TO CWBOXC-STRING-1 (4: )
                             MOVE CWBOXC-STRING-1
                               TO CWBOXC-STRING-2
                             ADD  1           TO FILES
                             CALL "CWBOXC" USING PARAMETROS-CWBOXC
                        END-IF
                END-PERFORM
           ELSE
                MOVE 1 TO ROOT
                MOVE MIN TO MAX
      *         MOVE 0 TO MAX
                MOVE F-FILEIN (1: K) TO WS-SAVE
                INSPECT F-FILEIN (1: K) CONVERTING SPACE TO "?"
                IF  (WS-SAVE (1: K) NOT = F-FILEIN (1: K))
                AND CWUNIX-OFF
                    INSPECT WS-SAVE CONVERTING X"FF" TO SPACE
                    MOVE WS-SAVE TO F-FILEIN (1: K)
                    PERFORM VARYING I FROM K BY -1
                            UNTIL F-FILEIN (I: 1) = "/" OR "\"
                                  CONTINUE
                    END-PERFORM
                    MOVE SPACES TO F-FILEIN (I: )
                    COMPUTE I2 = I - 1
                    PERFORM VARYING I FROM I BY -1
                            UNTIL F-FILEIN (I: 1) = "/" OR "\"
                                  CONTINUE
                    END-PERFORM
                    MOVE SPACES TO CWEXEC-COMANDO
                    STRING "DIR "          DELIMITED BY SIZE
                           F-FILEIN (1: I) DELIMITED BY SIZE
                           "> "            DELIMITED BY SIZE
                           LB-PASTAS       DELIMITED BY SPACE
                      INTO CWEXEC-COMANDO
                    CALL "CWEXEC" USING PARAMETROS-CWEXEC
                    ADD  1              TO I
                    MOVE F-FILEIN (I: ) TO WS
                    OPEN INPUT PASTAS
                    PERFORM TEST AFTER
                            UNTIL FS-PASTAS > "09"
                               OR WS = PASTAS-NOME-32
                            READ PASTAS
                    END-PERFORM
                    IF   FS-PASTAS < "10"
                         MOVE SPACES TO WS
                         IF   PASTAS-FT-16 = SPACES
                              STRING PASTAS-FN-16 DELIMITED BY SPACE
                                     "\.."        DELIMITED BY SIZE
                              INTO WS
                         ELSE
                              STRING PASTAS-FN-16 DELIMITED BY SPACE
                                     "."          DELIMITED BY SIZE
                                     PASTAS-FT-16 DELIMITED BY SPACE
                                     "\.."        DELIMITED BY SIZE
                              INTO WS
                         END-IF
                         MOVE WS TO F-FILEIN (I: )
                    END-IF
                    CLOSE PASTAS
                    DELETE FILE PASTAS
                END-IF
                MOVE 0          TO F-ACTION
                PERFORM VARYING I FROM 78 BY -1
                        UNTIL F-FILEIN (I: 1) = "/" OR "\"
                              CONTINUE
                END-PERFORM
                IF   CORINGA = SPACES
                     IF   CWUNIX-ON
                          ADD 1    TO I
                          MOVE "*" TO F-FILEIN (I: )
                     ELSE
                          ADD 1      TO I
                          MOVE "*.*" TO F-FILEIN (I: )
                     END-IF
                ELSE
                     MOVE CORINGA TO F-FILEIN (I: )
                END-IF
                MOVE F-FILEIN TO TITULO
                MOVE 0        TO RESIDENTE
                IF   CWUNIX-ON
                     MOVE ALL X"00" TO COMANDO-UNIX
                     STRING "ls -la " DELIMITED BY SIZE
                            F-FILEIN  DELIMITED BY SPACE
                            " 2> /dev/null > "  DELIMITED BY SIZE
                            LB-PASTAS DELIMITED BY SPACE
                                 INTO COMANDO-UNIX
                     INSPECT COMANDO-UNIX CONVERTING "\" TO "/"
                     CALL "system" USING COMANDO-UNIX
                     OPEN INPUT PASTAS
                     MOVE 0 TO LS DIRMODE
                END-IF
                PERFORM TEST AFTER UNTIL F-ERROR NOT = 0
                        MOVE SPACES   TO F-FILEOUT
                        IF   CWUNIX-OFF
                             CALL X"91" USING RESULT
                                              FUNCTION-CODE
                                              PARAMETER
                        ELSE
                             READ PASTAS
                                  AT END
                                     MOVE 1 TO F-ERROR
                                  NOT AT END
                                  MOVE 0 TO F-ERROR
                                  IF  PASTAS-REG (1: 1) NOT = "-"
                                      MOVE 16 TO F-ATTROUT
                                  ELSE
                                      PERFORM 010-POSICOES-LS
                                         THRU 010-99-FIM
                                      MOVE PASTAS-REG (LS: )
                                        TO F-FILEOUT
                                      MOVE 15 TO F-ATTROUT
                                  END-IF
                             END-READ
                        END-IF
                        MOVE 1        TO F-ACTION
                        IF   F-ERROR = 0
                        AND (F-ATTROUT NOT = 16)
                        AND (F-ATTROUT NOT = 17)
                             IF   F-FILEOUT = CWPATH-DEFAULT
                             AND (DEFAULTx = SPACES)
                                  MOVE CWPATH-DEFAULT TO DEFAULTx
                                  MOVE 1              TO RESIDENTE
                             END-IF
                             MOVE SPACES      TO CWBOXC-STRING-2
                             MOVE F-FILEOUT   TO CWBOXC-STRING-1
                                                 CWBOXC-STRING-2 (7: )
                             PERFORM VARYING I FROM 40 BY -1
                                     UNTIL CWBOXC-STRING-1 (I: 1)
                                           NOT = SPACE
                                     CONTINUE
                             END-PERFORM
                             IF   I > MAX
                                  MOVE I TO MAX
                             END-IF
                             IF   F-FILEOUT = CWBOXC-STRING-2 (7: )
                                  CALL "CWBOXC" USING PARAMETROS-CWBOXC
                                  ADD   1          TO FILES
                             END-IF
                        END-IF
                END-PERFORM
                CLOSE PASTAS
                DELETE FILE PASTAS
                IF  CWPATH-WITH-DIR
                    MOVE F-FILEIN TO WS
                    PERFORM VARYING I FROM 78 BY -1
                            UNTIL F-FILEIN (I: 1) = "/" OR "\"
                                  CONTINUE
                    END-PERFORM
                    IF   CWUNIX-ON
                         ADD 1    TO I
                         MOVE "*" TO F-FILEIN (I: )
                    ELSE
                         ADD 1      TO I
                         MOVE "*.*" TO F-FILEIN (I: )
                    END-IF
                    IF   CWUNIX-ON
                         MOVE ALL X"00" TO COMANDO-UNIX
                         STRING "ls -lad " DELIMITED BY SIZE
                                F-FILEIN   DELIMITED BY SPACE
                                " 2> /dev/null > "   DELIMITED BY SIZE
                                LB-PASTAS  DELIMITED BY SPACE
                                      INTO COMANDO-UNIX
                         INSPECT COMANDO-UNIX CONVERTING "\" TO "/"
                         CALL "system" USING COMANDO-UNIX
                         OPEN INPUT PASTAS
                         MOVE 0 TO LS Y
                         MOVE 1 TO DIRMODE
                         IF   F-FILEIN NOT = SPACES
                              PERFORM VARYING Y FROM 1 BY 1
                                        UNTIL F-FILEIN (Y: 1) = SPACE
                                                             OR "*"
                                      CONTINUE
                              END-PERFORM
                              SUBTRACT 1 FROM Y
                         END-IF
                    END-IF
                    MOVE 0 TO F-ACTION
                    PERFORM TEST AFTER UNTIL F-ERROR NOT = 0
                        MOVE SPACES   TO F-FILEOUT
                        IF   CWUNIX-OFF
                             CALL X"91" USING RESULT
                                              FUNCTION-CODE
                                              PARAMETER
                        ELSE
                             READ PASTAS
                                  AT END
                                     MOVE 1 TO F-ERROR
                                  NOT AT END
                                  MOVE 0 TO F-ERROR
                                  IF  PASTAS-REG (1: 1) NOT = "d"
                                      MOVE 15 TO F-ATTROUT
                                  ELSE
                                      PERFORM 010-POSICOES-LS
                                         THRU 010-99-FIM
                                      MOVE PASTAS-REG (LS: )
                                        TO F-FILEOUT
                                      MOVE 16 TO F-ATTROUT
                                      IF (Y NOT = 0)
                                      AND F-FILEIN (1: Y)
                                       = F-FILEOUT (1: Y)
                                         MOVE F-FILEOUT TO WS
                                         COMPUTE I2 = Y + 1
                                         MOVE WS (I2: ) TO F-FILEOUT
                                      END-IF
                                  END-IF
                             END-READ
                        END-IF
                        MOVE 1        TO F-ACTION
                        IF   F-ERROR = 0
                        AND (F-FILEOUT NOT = ".")
                        AND (F-ATTROUT = 16 OR 17)
                             MOVE F-FILEOUT TO CWBOXC-STRING-1
                             MOVE "<dir>"   TO CWBOXC-STRING-2
                             MOVE F-FILEOUT TO CWBOXC-STRING-2 (7: )
                             MOVE 1         TO DIR
                             PERFORM VARYING I FROM 40 BY -1
                                     UNTIL CWBOXC-STRING-1 (I: 1)
                                           NOT = SPACE
                                     CONTINUE
                             END-PERFORM
                             IF   I > MAX
                                  MOVE I TO MAX
                             END-IF
                             IF   F-FILEOUT = CWBOXC-STRING-2 (7: )
                                  CALL "CWBOXC" USING PARAMETROS-CWBOXC
                                  ADD   1          TO FILES
                                  IF   F-FILEOUT = ".."
                                       MOVE 0 TO ROOT
                                  END-IF
                             END-IF
                        END-IF
                    END-PERFORM
                END-IF
                IF   CWUNIX-ON
                     CLOSE PASTAS
                     DELETE FILE PASTAS
                END-IF
                IF  (ROOT = 1 OR F-FILEIN (2: 3) = ":\*")
                AND  CWPATH-WITH-DRIVES
                AND (F-FILEIN (1: 1) = "/"
                     AND (F-FILEIN (2: 1) NOT = "*")
                 OR  F-FILEIN (2: 1) = ":")
                     MOVE ".."          TO CWBOXC-STRING-1
                     IF   MAX < 3
                          MOVE 3 TO MAX
                     END-IF
                     IF   CWUNIX-OFF
                          MOVE "[-?-]"       TO CWBOXC-STRING-2
                     ELSE
                          MOVE "<dir>"       TO CWBOXC-STRING-2
                     END-IF
                     CALL "CWBOXC"  USING PARAMETROS-CWBOXC
                     ADD   1           TO FILES
                END-IF
                IF   CWPATH-WITH-NEWDIR
                     MOVE SPACES           TO CWBOXC-STRING-2
                     MOVE "[ Nova pasta ]" TO CWBOXC-STRING-1
                     MOVE CWBOXC-STRING-1  TO CWBOXC-STRING-2 (7: )
                     CALL "CWBOXC"     USING PARAMETROS-CWBOXC
                     ADD   1               TO FILES
                     IF   MAX < 14
                          MOVE 14 TO MAX
                     END-IF
                END-IF
                IF   CWPATH-WITH-NEWFILE
                     MOVE SPACES             TO CWBOXC-STRING-2
                     MOVE "[ Novo arquivo ]" TO CWBOXC-STRING-1
                     MOVE CWBOXC-STRING-1    TO CWBOXC-STRING-2 (7: )
                     CALL "CWBOXC"        USING PARAMETROS-CWBOXC
                     ADD   1                 TO FILES
                     IF   MAX < 16
                          MOVE 16 TO MAX
                     END-IF
                END-IF
           END-IF

           IF   FILES NOT = 0
                IF   CWBOXC-OPTION = "[-?-]"
                     MOVE 0 TO CWBOXC-STRING-2-LENGTH
                ELSE
                     ADD  1   TO MAX
                     MOVE MAX TO CWBOXC-HORIZONTAL-LENGTH
                                 CWBOXC-STRING-1-LENGTH
                     IF   DIR = 1
                     OR  (ROOT = 1
                     AND  CWPATH-WITH-DRIVES)
                          ADD 7  TO CWBOXC-HORIZONTAL-LENGTH
                          MOVE 6 TO CWBOXC-STRING-2-LENGTH
                     ELSE
                          MOVE 0 TO CWBOXC-STRING-2-LENGTH
                     END-IF
                     IF   Y > CWBOXC-HORIZONTAL-LENGTH
                         MOVE Y TO CWBOXC-HORIZONTAL-LENGTH
                         ADD  2 TO CWBOXC-HORIZONTAL-LENGTH
                     END-IF
                     IF   FILES < CWBOXC-VERTICAL-LENGTH
                          MOVE FILES TO CWBOXC-VERTICAL-LENGTH
                     END-IF
                END-IF
                SET   CWBOXC-SHOW   TO TRUE
                MOVE  DEFAULTx      TO CWBOXC-OPTION
                MOVE  2             TO CWBOXC-RETURN
                IF   CWBOXC-TITLE = "Drive"
                     MOVE "Selecione a unidade" TO TITULO
                     MOVE 0       TO CWBOXC-STRING-2-LENGTH
                     MOVE 7       TO CWBOXC-HORIZONTAL-LENGTH
                ELSE
                     IF   CWBOXC-STRING-1-LENGTH < 8
                          COMPUTE CWBOXC-HORIZONTAL-LENGTH =
                                  CWBOXC-HORIZONTAL-LENGTH +
                              (8 - CWBOXC-STRING-1-LENGTH - 3)
                          MOVE 8 TO CWBOXC-STRING-1-LENGTH
                     END-IF
                END-IF
                IF  (CWPATH-PATH-LINE   NOT = 0)
                AND (CWPATH-PATH-COLUMN NOT = 0)
                    MOVE CWPATH-PATH-LINE   TO LX
                    MOVE CWPATH-PATH-COLUMN TO CX
                    MOVE 68                 TO SX
                    CALL "CWMSGW" USING X TITULO
                END-IF
                IF   TIPO NOT = "D"
                     COMPUTE CWBOXC-HORIZONTAL-LENGTH =
                             CWBOXC-STRING-1-LENGTH +
                             CWBOXC-STRING-2-LENGTH + 1
                END-IF
                CALL "CWBOXC"    USING PARAMETROS-CWBOXC
                MOVE SPACES TO TITULO
                IF  (CWPATH-LINE   NOT = 0)
                AND (CWPATH-COLUMN NOT = 0)
                    MOVE CWPATH-PATH-LINE   TO LX
                    MOVE CWPATH-PATH-COLUMN TO CX
                    MOVE 68                 TO SX
                    CALL "CWMSGW" USING X TITULO
                END-IF
                MOVE  CWBOXC-OPTION TO CWPATH-FILE
                SET   CWBOXC-DELETE TO TRUE
                CALL "CWBOXC"    USING PARAMETROS-CWBOXC
           END-IF

           IF   CWBOXC-OPTION (1: 2) = "[-"
                PERFORM VARYING I FROM 78 BY -1
                           UNTIL F-FILEIN (I: 1) = "/" OR "\"
                        CONTINUE
                END-PERFORM
                MOVE F-FILEIN (I: ) TO WS
                MOVE CWBOXC-OPTION (3: 1) TO F-FILEIN
                MOVE ":"                  TO F-FILEIN (2: )
                MOVE WS                   TO F-FILEIN (3: )
                GO TO 010-MONTAR
           END-IF

           IF   CWBOXC-OPTION = "[-?-]"
                GO TO 010-MONTAR
           END-IF

           IF   CWBOXC-OPTION (1: 5) = "<dir>"
                MOVE CWBOXC-OPTION (7: ) TO WS
                MOVE WS                  TO CWBOXC-OPTION
                IF   CWBOXC-OPTION (1: 2) = ".."
                     PERFORM VARYING I FROM 78 BY -1
                                UNTIL F-FILEIN (I: 1) = "/" OR "\"
                             CONTINUE
                     END-PERFORM
                     MOVE F-FILEIN (I: ) TO WS
                     SUBTRACT 1 FROM I
                     PERFORM VARYING I FROM I BY -1
                             UNTIL F-FILEIN (I: 1) = "/" OR "\" OR ":"
                                   CONTINUE
                     END-PERFORM
                     MOVE WS TO F-FILEIN (I: )
                ELSE
                     PERFORM VARYING I FROM 78 BY -1
                                UNTIL F-FILEIN (I: 1) = "/" OR "\"
                             CONTINUE
                     END-PERFORM
                     MOVE F-FILEIN (I: ) TO WS
                     ADD  1              TO I
                     MOVE CWBOXC-OPTION  TO F-FILEIN (I: )
                     PERFORM VARYING I FROM 78 BY -1
                                UNTIL F-FILEIN (I: 1) NOT = SPACE
                             CONTINUE
                     END-PERFORM
                     ADD  1              TO I
                     MOVE WS             TO F-FILEIN (I: )
                END-IF
                MOVE 0      TO Y
                MOVE SPACES TO WS
                perform varying fim from 78 by -1
                           until f-filein (fim: 1) not = space
                        continue
                end-perform
                PERFORM VARYING I FROM 1 BY 1
                          UNTIL I > FIM
                        IF  F-FILEIN (I: 1) = SPACE
                            MOVE X"FF" TO F-FILEIN (I: 1)
                        END-IF
                        COMPUTE I2 = I + 1
                        IF  (F-FILEIN (I: 1)  = "/" OR "\")
                        AND (F-FILEIN (I2: 1) = "/" OR "\")
                             ADD 1 TO I
                        ELSE
                             ADD 1 TO Y
                             MOVE F-FILEIN (I: 1) TO WS (Y: 1)
                        END-IF
                END-PERFORM
                IF   CWUNIX-OFF
                     INSPECT WS CONVERTING "/" TO "\"
                ELSE
                     INSPECT WS  CONVERTING "\" TO "/"
                END-IF
                MOVE WS TO F-FILEIN
                GO TO 010-MONTAR
           END-IF

           MOVE SPACES TO CWPATH-FILE

           IF   CWBOXC-OPTION (7: ) = "[ Nova pasta ]"
                MOVE SPACES TO NEWDIR
                PERFORM TEST AFTER UNTIL RETURN-STATUS = 0
                                      OR NEWDIR = SPACES
                        EXEC COBOLware BOXDialog
                             LINE CWPATH-LINE
                             COLUMN CWPATH-COLUMN
                             HEADER "Criar nova pasta"
                             Caption(1) "Nova pasta "
                             Size(1) 20
                             Data(1) NEWDIR;NEWDIR
                        END-EXEC
                        IF    NEWDIR NOT = SPACES
                              PERFORM VARYING I FROM 78 BY -1
                                      UNTIL F-FILEIN (I: 1) = "/" OR "\"
                                      CONTINUE
                              END-PERFORM
                              MOVE SPACES TO CWPATH-FILE
                              PERFORM VARYING FIM
                                      FROM LENGTH OF NEWDIR
                                       BY -1
                                  UNTIL NEWDIR (FIM: 1) NOT = SPACE
                              END-PERFORM
                              PERFORM VARYING FIM2
                                      FROM 1 BY 1
                                    UNTIL NEWDIR (FIM2: 1) = SPACE
                              END-PERFORM
                              SUBTRACT 1 FROM FIM2
                              STRING F-FILEIN (1: I) DELIMITED BY SIZE
                                     NEWDIR          DELIMITED BY SIZE
                              INTO CWPATH-FILE
                              IF   FIM NOT = FIM2
                                   MOVE 13 TO RETURN-STATUS
                              ELSE
                                   CALL "CBL_CREATE_DIR"
                                        USING CWPATH-FILE
                                    RETURNING RETURN-STATUS
                              END-IF
                              IF   RETURN-STATUS NOT = 0
                                   MOVE SPACES TO CWSEND-MSG
                                   STRING "NÆo ‚ possivel criar "
                                          DELIMITED BY SIZE
                                       NEWDIR (1: FIM)
                                       DELIMITED BY SIZE
                                   INTO CWSEND-MSG
                                   CALL "CWSEND" USING PARAMETROS-CWSEND
                              END-IF
                        END-IF
                END-PERFORM
                GO TO 010-MONTAR
           END-IF

           IF   CWBOXC-OPTION (7: ) = "[ Novo arquivo ]"
                MOVE CWPATH-DEFAULT TO NEWDIR
                IF   RESIDENTE = 1
                     MOVE SPACES TO NEWDIR
                END-IF
                EXEC COBOLware BOXDialog
                     LINE CWPATH-LINE
                     COLUMN CWPATH-COLUMN
                     HEADER "Criar novo arquivo"
                     Caption(1) "Novo arquivo"
                     Size(1) 12
                     Data(1) NEWDIR;NEWDIR
                END-EXEC
                MOVE SPACES        TO CWBOXC-OPTION
                IF    NEWDIR NOT = SPACES
                      PERFORM VARYING I FROM 78 BY -1
                              UNTIL F-FILEIN (I: 1) = "/" OR "\"
                              CONTINUE
                      END-PERFORM
                      MOVE SPACES TO CWPATH-FILE
                      STRING F-FILEIN (1: I) DELIMITED BY SIZE
                             NEWDIR          DELIMITED BY SIZE
                      INTO CWPATH-FILE
                ELSE
                      GO TO 010-MONTAR
                END-IF
           END-IF

           IF   CWBOXC-OPTION NOT = SPACES
                MOVE CWBOXC-OPTION (7: ) TO WS
                MOVE WS                  TO CWBOXC-OPTION
                PERFORM VARYING I FROM 78 BY -1
                          UNTIL F-FILEIN (I: 1) = "/" OR "\"
                        CONTINUE
                END-PERFORM
                MOVE SPACES TO CWPATH-FILE
                STRING F-FILEIN (1: I) DELIMITED BY SIZE
                       WS              DELIMITED BY SIZE
                INTO CWPATH-FILE
                IF   CWPATH-COLOR-BARR-MENU = 111
                AND  CWUNIX-ON
                     MOVE  CWPATH-FILE TO CWPATH-LONGNAME
                END-IF
           END-IF

           MOVE ALL "0" TO CWPATH-ATTR.

       000-99-FIM. GOBACK.

       010-POSICOES-LS.

           IF   LS NOT = 0
                GO TO 010-99-FIM
           END-IF

           PERFORM VARYING LS FROM LENGTH OF PASTAS-REG
                                BY -1
                                UNTIL PASTAS-REG (LS: 1) NOT = SPACE
                   CONTINUE
           END-PERFORM
           PERFORM VARYING LS FROM LS
                                BY -1
                                UNTIL PASTAS-REG (LS: 1) = SPACE
                                   OR (DIRMODE = 0
                                  AND PASTAS-REG (LS: 1) = "/")
                   CONTINUE
           END-PERFORM
           ADD 1 TO LS.

       010-99-FIM. EXIT.

       020-WINOPEN.

           move space to aString

           If  extensao = spaces
               string
               Z"Todos os formatos (*.*)" Z"*.*"
               Z"Arquivos texto (*.TXT)"  Z"*.TXT" x"00"
                delimited by size into aString
           Else
               move extensao to tes
               inspect tes converting minusculas to maiusculas
               if tes = "HTM" OR "HTML"
                   string Z"Formato HTML (*.HTM;*.HTML)"
                          Z"*.htm;*.html" x"00"
                    delimited by size into aString
               else
                    if tes = "JPG"
                        move spaces to aString bString
                        string
                     Z"Arquivos de Bitmap (*.bmp;*.dib)" Z"*.bmp;*.dib"
                     Z"Todos os arquivos de imagem"
                       "*.bmp;"  "*.cut;"  "*.dds;"  "*.dib;"   "*.exr;"
                       "*.g3;"   "*.gif;"  "*.ico;"  "*.iff;"   "*.j2c;"
                       "*.j2k;"  "*.jfif;" "*.jif;"  "*.jng;"   "*.jp2;"
                       "*.jpe;"  "*.jpeg;" "*.jpg;"  "*.koa;"   "*.lbm;"
                       "*.mng;"  "*.pbm;"            "*.pcd;"   "*.pct;"
                       "*.pcx;"  "*.pfm;"  "*.pgm;"             "*.pic;"
                       "*.pict;" "*.png;"  "*.ppm;"             "*.psd;"
                       "*.ras;"  "*.raw;"  "*.sgi;"  "*.targa;" "*.tga;"
                       "*.tif;"  "*.tiff;" "*.wbmp;" "*.xbm;"  Z"*.xmp"
                     Z"JPEG (*.jpg;*.jpeg;*.jpe;*.jif)"
                                          Z"*.jpg;*.jpeg;*.jpe;*.jif"
                     Z"GIF (*.gif)" Z"*.gif"
                     Z"TIFF (*.tiff;*.tif)" Z"*.tiff;*.tif"
                     Z"PNG (*.png)" Z"*.png"
                     Z"ICO (*.ico)" Z"*.ico" x"00"
                         delimited by size into aString
                    else
                        string "Formato " extensao(1:e) " (*."
                                          extensao(1:e) ")"
                               x"00" "*." extensao(1:e) x"00"  x"00"
                         delimited by size into aString
                    End-if
               End-if
           End-if

           initialize OFNS.
           set lStructSize of ofns to length of ofns.
      *    call WinAPI "GetActiveWindow"
      *         returning hwndOwner of ofns
           DISPLAY "CWTITLE" UPON ENVIRONMENT-NAME
           ACCEPT   CWTITLE  FROM ENVIRONMENT-VALUE
           PERFORM VARYING I FROM LENGTH OF CWTITLE BY -1
                   UNTIL I = 0
                      OR (CWTITLE (I: 1) NOT = SPACE)
                   CONTINUE
           END-PERFORM
           ADD  1     TO I
           MOVE X"00" TO CWTITLE (I: 1)
           CALL WinAPI "FindWindowA" using
                by value NULLptr
                by content CWTITLE
                   returning hwndOwner of ofns

           add GMEM-MOVEABLE GMEM-ZEROINIT GMEM-SHARE giving hStyle
           call WinAPI "GlobalAlloc" using
                by value hStyle
                by value 256
                returning 1hInstance of ofns
           end-call.

           set lpstrFilter of ofns to address of aString.

           move 0 to nMaxCustFilter of ofns.
           move 2 to nFilterIndex of OfnS.
           move x"00" to bString (255:1).
           set lpstrFile of ofns to address of bString.
           move 255 to nMaxFile of ofns.

           move space to cstring.
           move x"00" to cstring (256:1).
           set lpstrFileTitle of ofns to address of cString.
           move 256 to nMaxFileTitle of ofns.

           move CWPATH-PATH TO dString.
           move x"00" to dString (256:1).
           set lpstrInitialDir to address of dString.

           If  CWPATH-WITH-NEWFILE
           OR  CWPATH-WITH-NEWDIR
               Perform FileSave Thru End-FileSave
           Else
               Perform FileOpen Thru End-FileOpen
           End-If

           call WinAPI "GlobalFree" using
                by value 1hInstance of ofns
                returning boola
           end-call.

       020-99-FIM. EXIT.

       FileOpen.

           IF CWPATH-TITLE = SPACES
              move z"Selecione o arquivo a ser lido" to estring
           else
              move spaces to estring
              perform varying i from 1 by 1
                         until CWPATH-TITLE (i: 1) not = "_"
                     continue
              end-perform
              string CWPATH-TITLE (i: ) delimited by space
                     X"00" delimited by size
                     into estring
              inspect estring converting "_" to space
              INSPECT estring CONVERTING ACENTOS-850
                                      TO ACENTOS-WINDOWS
           end-if
           set lpstrTitle of ofns to address of estring.

           move 0 to Flags of OFNS.
           add OFN-HIDEREADONLY           to Flags of OFNS
           add OFN-FILEMUSTEXIST          to Flags of OFNS
           add OFN-PATHMUSTEXIST          to Flags of OFNS
           add OFN-NOLONGNAMES            to Flags of OFNS

           move 0 to nFileOffset    of ofns.
           move 0 to nFileExtension of ofns.
           move 0 to lCustData      of ofns.
           set lpfnHook of ofns to null.

           CALL WINAPI "GetForegroundWindow" RETURNING hwndowner of ofns
           call WinAPI "GetOpenFileNameA" using
                  by reference ofns
                  returning boola
           end-call.

           if boola = 0
              call WinAPI "CommDlgExtendedError"
              MOVE SPACES              TO CWPATH-FILE
           else
              PERFORM GET-SHORT-NAME THRU FIM-GET-SHORT-NAME
           end-if.

       End-FileOpen. exit.

       GET-SHORT-NAME.

            MOVE SPACES TO  CWPATH-FILE
            PERFORM VARYING I FROM 1 BY 1
                      UNTIL I > length of bstring
                         OR bstring (I: 1) = LOW-VALUE
                          CONTINUE
            END-PERFORM
            MOVE SPACES TO bstring (I:)
            PERFORM VARYING I FROM length of bstring BY -1
                    UNTIL (bstring (I: 1) not = space)
                       OR I = 1
                          CONTINUE
            END-PERFORM

            IF   CWPATH-COLOR-BARR-MENU = 111
                 MOVE bstring (1: I) TO CWPATH-LONGNAME
            END-IF

            add 1 to i
            move x'00' to bstring (I: )
            move length of CWPATH-FILE to LenShort
            CALL WINAPI "GetShortPathNameA" using
                 by reference bstring
                 by reference CWPATH-FILE
                 by reference LenShort
            INSPECT CWPATH-FILE CONVERTING LOW-VALUE TO SPACE
            IF  CWPATH-FILE = spaces
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
                        by reference bstring
                        by reference PATH
                        by reference LenShort
                  IF PATH NOT = SPACES
                     STRING PATH     DELIMITED BY LOW-VALUE
                            '\'      DELIMITED BY SIZE
                            FILENAME DELIMITED BY SPACE
                     INTO CWPATH-FILE
                     INSPECT CWPATH-FILE
                             CONVERTING LOW-VALUE TO SPACE
                  END-IF
                END-IF
            ELSE
                PERFORM VARYING I FROM 1 BY 1
                        UNTIL CWPATH-FILE (I: 1) = space or low-value
                           OR I > LENGTH OF CWPATH-FILE
                              CONTINUE
                END-PERFORM
                IF I < LENGTH OF CWPATH-FILE
                AND  CWPATH-FILE (I: 1) = space
                   MOVE SPACES TO  CWPATH-FILE (I: )
                END-IF
            END-IF

            if extensao(4:) not = spaces
               move cwpath-file to lb-teste
               open input teste
               if fs-teste = '35'
                  move spaces to lb-teste
                  string cwpath-file delimited by '.'
                         '.' delimited by size
                         extensao(1:3) delimited by size
                         into lb-teste
                  open input teste
                  if fs-teste not = '35'
                     move lb-teste to cwpath-file
                  end-if
               end-if
               close teste
            end-if.


       FIM-GET-SHORT-NAME. EXIT.

       FileSave.

           IF CWPATH-TITLE = SPACES
              move z"Defina o arquivo a ser gravado" to estring
           else
              move spaces to estring
              perform varying i from 1 by 1
                         until CWPATH-TITLE (i: 1) not = "_"
                     continue
              end-perform
              string CWPATH-TITLE (i: ) delimited by space
                     X"00" delimited by size
                     into estring
              inspect estring converting "_" to space
              INSPECT estring CONVERTING ACENTOS-850
                                      TO ACENTOS-WINDOWS
           end-if
           set lpstrTitle of ofns to address of estring.

           move 0 to Flags of OFNS.
           add  OFN-SHOWHELP               to Flags of OFNS.
           add  OFN-HIDEREADONLY           to Flags of OFNS.

           move 0 to nFileOffset of ofns.
           move 0 to nFileExtension of ofns.
           move 0 to lCustData of ofns.
           set lpfnHook of ofns to null.

           call WinAPI "GetSaveFileNameA" using
                by reference ofns
                returning boola
           end-call

           if boola = 0
              call WinAPI "CommDlgExtendedError"
              move spaces              to CWPATH-FILE
           else
              PERFORM GET-SHORT-NAME THRU FIM-GET-SHORT-NAME
           end-if.

       End-FileSave. Exit.

       END PROGRAM CWPATH.


