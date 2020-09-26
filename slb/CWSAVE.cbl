       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWSAVE.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/06/2005.
       SECURITY.      *************************************************
                      *                                               *
                      *  Gera arquivo de exporta�ao de dados          *
                      *  Formatos Texto, DBF, RPX, XML e JSON.        *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT BINARIO ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS.

           SELECT XSL     ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-XSL.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT OBJECTS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS OBJECTS-ID
                  LOCK MODE     IS EXCLUSIVE
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-OBJECTS.

       DATA DIVISION.
       FILE SECTION.

       FD  BINARIO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-BINARIO.

       01  BINARIO-REG               PIC X(001).

       FD  XSL
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-XSL.

       01  XSL-REG                   PIC X(500).

       FD  OBJECTS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-OBJECTS.

       01  OBJECTS-REG.
           05 OBJECTS-ID           PIC  X(255).
           05 OBJECTS-FILE         PIC  X(255).
           05 OBJECTS-REC   COMP-5 PIC S9(004).
           05 OBJECTS-ROWS  COMP-5 PIC S9(008).
           05 EXTW                 PIC  X(004).
              88 DBF                           VALUE "DBF".
              88 RPX                           VALUE "RPX".
              88 FileTypeXML                   VALUE "XML".
              88 FileTypeJSON                  VALUE "JSON".
           05 OBJECTS-DELM         PIC  X(001) VALUE SPACE.
           05 handle               PIC  X(004).
           05 offset               pic  x(008) comp-x.
           05 DBF-DATA.
              07 DBF-HEADER.
                 10 FILLER1                PIC  X(001).
                 10 DBF-HEADER-AA          PIC  9(002) COMP-X.
                 10 DBF-HEADER-MM          PIC  9(002) COMP-X.
                 10 DBF-HEADER-DD          PIC  9(002) COMP-X.
                 10 DBF-HEADER-RECORDS     PIC S9(004) COMP-5.
                 10                        PIC  X(002).
                 10 DBF-HEADER-TOPLEN      PIC S9(004) COMP-5.
                 10 DBF-HEADER-LENGTH      PIC S9(004) COMP-5.
                 10                        PIC  X(020).
              07 DBF-FIELD.
                 10 DBF-FIELD-DATANAME      PIC  X(010).
                 10                         PIC  X(001).
                 10 DBF-FIELD-TYPE          PIC  X(001).
                 10                         PIC  X(004).
                 10 DBF-FIELD-LENGTH        PIC  9(002) COMP-X.
                 10 DBF-FIELD-DEC           PIC  9(002) COMP-X.
                 10                         PIC  X(002).
                 10 FILLER2                 PIC  X(001).
                 10                         PIC  X(011).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 WS-FILE             PIC  X(255) VALUE SPACES.
           05 fim                 pic  x      value spaces.
           05 teste-num           pic x(1024) value spaces.
              88 boolean-null                 value 'false' 'true'
                                                    'null'.
           05 enter-flag          pic  9(01)  value 1.
              88 ENTER-ON                     value 1.
              88 ENTER-OFF                    value 0.
           05 dia                 pic  x(10)  value spaces.
           05 hora                pic  x(08)  value spaces.
           05 programa            pic  x(08)  value spaces.
           05 usuario             pic  x(30)  value spaces.
           05 empresa             pic  x(30)  value spaces.
           05 sistema             pic  x(30)  value spaces.
           05 byte-count   comp-x pic  x(4)   value 1.
           05 REVISAO             PIC  X(016) VALUE SPACES.
           05 ANTES               PIC  X(001) VALUE SPACE.
           05 SAVE-FS             PIC  X(002) VALUE SPACE.
           05 xml-field           PIC  9(001) VALUE 0.
           05 xml-rec             PIC  9(018) VALUE 0.
           05 xml-buffer          PIC  x(005) VALUE spaces.
           05 xml-data            PIC  x(32768) VALUE spaces.
           05 xml-w               PIC  x(32768) VALUE spaces.
           05 xml-len             PIC  9(018) VALUE 0.
           05 nz                  PIC  9(002) VALUE 0.
           05 b-r                 PIC  X(001) VALUE SPACE.
           05 SM                  PIC  X(002) VALUE SPACE.
           05 XMLNUM              PIC  X(003) VALUE SPACE.
           05 BUFFER              PIC  X(32768) VALUE SPACES.
           05 SINAL               PIC  X(001) VALUE SPACE.
           05 X            COMP-5 PIC S9(004) VALUE 0.
           05 I            COMP-5 PIC S9(004) VALUE 0.
           05 W            COMP-5 PIC S9(004) VALUE 0.
           05 F            COMP-5 PIC S9(004) VALUE 0.
           05 PT           COMP-5 PIC S9(004) VALUE 0.
           05 PS           COMP-5 PIC S9(004) VALUE 0.
           05 P            COMP-5 PIC S9(004) VALUE 0.
           05 XN           COMP-X PIC  9(002) VALUE 0.
           05 XI           COMP-5 PIC S9(004) VALUE 0.
           05 LEN                 PIC  9(004) VALUE 0.
           05 LEN2                PIC  9(003) VALUE 0.
           05 OB                  PIC  9(001) VALUE 0.
           05 HOJE                PIC  9(006) VALUE 0.
           05 REDEFINES HOJE.
              10 AA               PIC  9(002).
              10 MM               PIC  9(002).
              10 DD               PIC  9(002).
           05 NEW                 PIC  X(050) VALUE SPACES.
           05 FS                  PIC  X(002) VALUE "00".
           05 ER-BINARIO.
              10 FS-BINARIO       PIC  X(002) VALUE "00".
              10 LB-BINARIO       PIC  X(255) VALUE SPACES.
           05 ER-OBJECTS.
              10 FS-OBJECTS       PIC  X(002) VALUE "00".
              10 LB-OBJECTS       PIC  X(255) VALUE "$TEMP/cwsave.".
           05 ER-FILES.
              10 FS-FILES         PIC  X(002) VALUE "00".
              10 LB-FILES         PIC  X(255) VALUE "$TEMP/cwfiles.".
           05 ER-XSL.
              10 FS-XSL           PIC  X(002) VALUE "00".
              10 LB-XSL           PIC  X(255) VALUE SPACES.
           05 X91-RESULT    COMP-X     PIC  9(002) VALUE 0.
           05 X91-FUNCTION  COMP-X     PIC  9(002) VALUE 16.
           05 X91-PARAMETER COMP-X     PIC  9(002) VALUE 0.

       01   XML-TAB.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value ".".
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value ".".
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value "-".
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value "!".
            03 pic x(5) value '"'.
            03 pic x(5) value "#".
            03 pic x(5) value "$".
            03 pic x(5) value "%".
            03 pic x(5) value "&amp;".
            03 pic x(5) value "'".
            03 pic x(5) value "(".
            03 pic x(5) value ")".
            03 pic x(5) value "*".
            03 pic x(5) value "+".
            03 pic x(5) value ",".
            03 pic x(5) value "-".
            03 pic x(5) value ".".
            03 pic x(5) value "/".
            03 pic x(5) value "0".
            03 pic x(5) value "1".
            03 pic x(5) value "2".
            03 pic x(5) value "3".
            03 pic x(5) value "4".
            03 pic x(5) value "5".
            03 pic x(5) value "6".
            03 pic x(5) value "7".
            03 pic x(5) value "8".
            03 pic x(5) value "9".
            03 pic x(5) value ":".
            03 pic x(5) value ";".
            03 pic x(5) value "&lt;".
            03 pic x(5) value "=".
            03 pic x(5) value "&gt;".
            03 pic x(5) value "?".
            03 pic x(5) value "@".
            03 pic x(5) value "A".
            03 pic x(5) value "B".
            03 pic x(5) value "C".
            03 pic x(5) value "D".
            03 pic x(5) value "E".
            03 pic x(5) value "F".
            03 pic x(5) value "G".
            03 pic x(5) value "H".
            03 pic x(5) value "I".
            03 pic x(5) value "J".
            03 pic x(5) value "K".
            03 pic x(5) value "L".
            03 pic x(5) value "M".
            03 pic x(5) value "N".
            03 pic x(5) value "O".
            03 pic x(5) value "P".
            03 pic x(5) value "Q".
            03 pic x(5) value "R".
            03 pic x(5) value "S".
            03 pic x(5) value "T".
            03 pic x(5) value "U".
            03 pic x(5) value "V".
            03 pic x(5) value "W".
            03 pic x(5) value "X".
            03 pic x(5) value "Y".
            03 pic x(5) value "Z".
            03 pic x(5) value "[".
            03 pic x(5) value "\".
            03 pic x(5) value "]".
            03 pic x(5) value "^".
            03 pic x(5) value "_".
            03 pic x(5) value "`".
            03 pic x(5) value "a".
            03 pic x(5) value "b".
            03 pic x(5) value "c".
            03 pic x(5) value "d".
            03 pic x(5) value "e".
            03 pic x(5) value "f".
            03 pic x(5) value "g".
            03 pic x(5) value "h".
            03 pic x(5) value "i".
            03 pic x(5) value "j".
            03 pic x(5) value "k".
            03 pic x(5) value "l".
            03 pic x(5) value "m".
            03 pic x(5) value "n".
            03 pic x(5) value "o".
            03 pic x(5) value "p".
            03 pic x(5) value "q".
            03 pic x(5) value "r".
            03 pic x(5) value "s".
            03 pic x(5) value "t".
            03 pic x(5) value "u".
            03 pic x(5) value "v".
            03 pic x(5) value "w".
            03 pic x(5) value "x".
            03 pic x(5) value "y".
            03 pic x(5) value "z".
            03 pic x(5) value "{".
            03 pic x(5) value "|".
            03 pic x(5) value "}".
            03 pic x(5) value "~".
            03 pic x(5) value "".
            03 pic x(5) value "Ç".
            03 pic x(5) value "ü".
            03 pic x(5) value "é".
            03 pic x(5) value "â".
            03 pic x(5) value "ä".
            03 pic x(5) value "à".
            03 pic x(5) value "å".
            03 pic x(5) value "ç".
            03 pic x(5) value "ê".
            03 pic x(5) value "ë".
            03 pic x(5) value "è".
            03 pic x(5) value "ï".
            03 pic x(5) value "î".
            03 pic x(5) value "ì".
            03 pic x(5) value "Ä".
            03 pic x(5) value "Å".
            03 pic x(5) value "É".
            03 pic x(5) value "æ".
            03 pic x(5) value "Æ".
            03 pic x(5) value "ô".
            03 pic x(5) value "ö".
            03 pic x(5) value "ò".
            03 pic x(5) value "û".
            03 pic x(5) value "ù".
            03 pic x(5) value "ÿ".
            03 pic x(5) value "Ö".
            03 pic x(5) value "Ü".
            03 pic x(5) value "ø".
            03 pic x(5) value "£".
            03 pic x(5) value "Ø".
            03 pic x(5) value "×".
            03 pic x(5) value "ƒ".
            03 pic x(5) value "á".
            03 pic x(5) value "í".
            03 pic x(5) value "ó".
            03 pic x(5) value "ú".
            03 pic x(5) value "ñ".
            03 pic x(5) value "Ñ".
            03 pic x(5) value "ª".
            03 pic x(5) value "º".
            03 pic x(5) value "¿".
            03 pic x(5) value "®".
            03 pic x(5) value spaces.
            03 pic x(5) value "½".
            03 pic x(5) value "¼".
            03 pic x(5) value "¡".
            03 pic x(5) value "«".
            03 pic x(5) value "»".
            03 pic x(5) value "░".
            03 pic x(5) value "▒".
            03 pic x(5) value "▓".
            03 pic x(5) value "│".
            03 pic x(5) value "┤".
            03 pic x(5) value "Á".
            03 pic x(5) value "Â".
            03 pic x(5) value "À".
            03 pic x(5) value "©".
            03 pic x(5) value "╣".
            03 pic x(5) value "║".
            03 pic x(5) value "╗".
            03 pic x(5) value "╝".
            03 pic x(5) value "¢".
            03 pic x(5) value "¥".
            03 pic x(5) value "┐".
            03 pic x(5) value "└".
            03 pic x(5) value "┴".
            03 pic x(5) value "┬".
            03 pic x(5) value "├".
            03 pic x(5) value "─".
            03 pic x(5) value "┼".
            03 pic x(5) value "ã".
            03 pic x(5) value "Ã".
            03 pic x(5) value "╚".
            03 pic x(5) value "╔".
            03 pic x(5) value "╩".
            03 pic x(5) value "╦".
            03 pic x(5) value "╠".
            03 pic x(5) value "═".
            03 pic x(5) value "╬".
            03 pic x(5) value "¤".
            03 pic x(5) value "ð".
            03 pic x(5) value "Ð".
            03 pic x(5) value "Ê".
            03 pic x(5) value "Ë".
            03 pic x(5) value "È".
            03 pic x(5) value "ı".
            03 pic x(5) value "Í".
            03 pic x(5) value "Î".
            03 pic x(5) value "Ï".
            03 pic x(5) value "┘".
            03 pic x(5) value "┌".
            03 pic x(5) value "█".
            03 pic x(5) value "▄".
            03 pic x(5) value "¦".
            03 pic x(5) value "Ì".
            03 pic x(5) value "▀".
            03 pic x(5) value "Ó".
            03 pic x(5) value "ß".
            03 pic x(5) value "Ô".
            03 pic x(5) value "Ò".
            03 pic x(5) value "õ".
            03 pic x(5) value "Õ".
            03 pic x(5) value "µ".
            03 pic x(5) value "þ".
            03 pic x(5) value "Þ".
            03 pic x(5) value "Ú".
            03 pic x(5) value "Û".
            03 pic x(5) value "Ù".
            03 pic x(5) value "ý".
            03 pic x(5) value "Ý".
            03 pic x(5) value "¯".
            03 pic x(5) value "´".
            03 pic x(5) value "­".
            03 pic x(5) value "±".
            03 pic x(5) value "‗".
            03 pic x(5) value "¾".
            03 pic x(5) value spaces.
            03 pic x(5) value "§".
            03 pic x(5) value "÷".
            03 pic x(5) value "¸".
            03 pic x(5) value spaces.
            03 pic x(5) value "¨".
            03 pic x(5) value spaces.
            03 pic x(5) value "¹".
            03 pic x(5) value "³".
            03 pic x(5) value "²".
            03 pic x(5) value "■".
            03 pic x(5) value spaces.
       01   redefines XML-TAB.
            03 xml-string pic x(5) occurs 256.

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(006) VALUE SPACES.
           05 FILLER                         PIC  X(021) VALUE ALL "*".
           05 FILLER                         PIC  X(021) VALUE ALL "*".
           05 FILLER                         PIC  X(017) VALUE ALL "*".
       02  LINHA-02.
           05 FILLER                         PIC  X(006) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "*".
           05 FILLER                         PIC  X(055) VALUE
              "    COBOLware 6.1 Lay-out de arquivo texto".
           05 FILLER                         PIC  X(003) VALUE "  *".
       02  LINHA-03.
           05 FILLER                         PIC  X(006) VALUE SPACES.
           05 FILLER                         PIC  X(021) VALUE ALL "*".
           05 FILLER                         PIC  X(021) VALUE ALL "*".
           05 FILLER                         PIC  X(017) VALUE ALL "*".
       02  LINHA-04.
           05 FILLER                         PIC  X(007) VALUE SPACES.
           05 FILLER                         PIC  X(004) VALUE "FD  ".
           05 FILENAME                       PIC  X(050) VALUE SPACES.
       02  LINHA-05.
           05 FILLER                         PIC  X(011) VALUE SPACES.
           05 FILLER                         PIC  X(016) VALUE
              "RECORD CONTAINS ".
           05 REG-SIZE                       PIC  9(004) VALUE ZEROS.
           05 FILLER                         PIC  X(011) VALUE
              " CHARACTERS".
       02  LINHA-06.
           05 FILLER                         PIC  X(011) VALUE SPACES.
           05 FILLER                         PIC  X(024) VALUE
              "LABEL RECORD IS STANDARD".
       02  LINHA-07.
           05 FILLER                         PIC  X(011) VALUE SPACES.
           05 FILLER                         PIC  X(020) VALUE
              "VALUE OF FILE-ID IS".
           05 FILE-LABEL                     PIC  X(053) VALUE SPACES.
       02  LINHA-07L.
           05 FILLER                         PIC  X(017) VALUE SPACES.
           05 FILE-LABEL2                    PIC  X(053) VALUE SPACES.
       02  LINHA-08.
           05 FILLER                         PIC  X(007) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "0".
           05 FILLER                         PIC  X(003) VALUE "1  ".
           05 FILENAME2                      PIC  X(055) VALUE SPACES.
       02  LINHA-09.
           05 FILLER                         PIC  X(011) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE "0".
           05 FILLER                         PIC  X(002) VALUE "5 ".
           05 DATANAME                       PIC  X(030) VALUE SPACES.
           05 FILLER                         PIC  X(007) VALUE " PIC  ".
           05 MASCARA                        PIC  X(050) VALUE SPACES.

       COPY CWUNIX.
       COPY CWUTF8.

       LINKAGE SECTION.

       COPY CWSAVE.
       01  MORE255 PIC X.
       PROCEDURE DIVISION USING PARAMETROS-CWSAVE CWSAVE-FILE.
       DECLARATIVES.

       DECLARATIVES-BINARIO SECTION.

           USE AFTER ERROR PROCEDURE ON BINARIO.

       CHECK-BINARIO.

           IF   FS-BINARIO > "09"
           AND (FS-BINARIO NOT = "42")
                CALL "CWISAM" USING ER-BINARIO
           END-IF.

       END DECLARATIVES.

       000 SECTION.

       000-INICIO.

           ON 1
              MOVE X'0A' TO xml-string (11)
              MOVE X'0D' TO xml-string (14)
              DISPLAY 'CWREVS' UPON ENVIRONMENT-NAME
              ACCEPT  REVISAO  FROM ENVIRONMENT-VALUE
              DISPLAY "CWXMLNUM" UPON ENVIRONMENT-NAME
              ACCEPT  XMLNUM   FROM ENVIRONMENT-VALUE
              INSPECT XMLNUM  CONVERTING
                              MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWSAVE" UPON ENVIRONMENT-NAME
              DISPLAY "ON"     UPON ENVIRONMENT-VALUE
              CALL "CWUNIX" USING PARAMETROS-CWUNIX.

           IF   CWSAVE-OLDFILE = SPACES
                CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
                IF   X91-PARAMETER > 1
                     MOVE CWSAVE-FILE TO WS-FILE
                ELSE
                     MOVE SPACES      TO WS-FILE
                END-IF
           ELSE
                MOVE CWSAVE-OLDFILE   TO WS-FILE
           END-IF

           IF   OB = 0
                OPEN I-O OBJECTS
                MOVE 1 TO OB
           END-IF

           IF   CWSAVE-CLOSE-ALL
                SET CWUTF8-CLOSE TO TRUE
                CALL "CWUTF8" USING PARAMETROS-CWUTF8
                CANCEL "CWUTF8"
                MOVE LOW-VALUES TO OBJECTS-ID
                START OBJECTS KEY NOT LESS OBJECTS-ID
                READ OBJECTS NEXT RECORD
                IF   FS-OBJECTS = "00"
                     IF  CWSAVE-OLDFILE = SPACES
                         IF   X91-PARAMETER > 1
                              MOVE OBJECTS-FILE TO CWSAVE-FILE
                         END-IF
                     ELSE
                         MOVE OBJECTS-FILE TO CWSAVE-OLDFILE
                     END-IF
                ELSE
                     MOVE SPACES TO PARAMETROS-CWSAVE
                     CLOSE OBJECTS
                     MOVE 0 TO OB
                     GO TO 000-99-FIM
                END-IF
           END-IF

           MOVE WS-FILE    TO OBJECTS-ID

           READ OBJECTS
           CALL 'CWATCH'
           IF  FS-OBJECTS = "23"
               INITIALIZE OBJECTS-REG
               MOVE LOW-VALUES  TO DBF-DATA
               MOVE X'03'       TO FILLER1
               MOVE X'01'       TO FILLER2
               MOVE WS-FILE     TO OBJECTS-ID OBJECTS-FILE
               CALL 'CWFILE' USING OBJECTS-FILE LENGTH OBJECTS-FILE
               MOVE CWSAVE-DELIMITER TO OBJECTS-DELM
           END-IF
           EVALUATE TRUE
               WHEN FS-OBJECTS = "00"
                AND (CWSAVE-CLOSE OR CWSAVE-CLOSE-ALL)
                    SET CWUTF8-CLOSE TO TRUE
                    CALL "CWUTF8" USING PARAMETROS-CWUTF8
                    CANCEL "CWUTF8"
                    MOVE OBJECTS-FILE TO LB-BINARIO
                    EVALUATE TRUE
                        WHEN DBF
                             call "CBL_CLOSE_FILE" using handle
                             OPEN EXTEND BINARIO
                             WRITE BINARIO-REG FROM X"1B"
                             CLOSE BINARIO
                             OPEN I-O BINARIO
                             READ BINARIO
                             READ BINARIO
                             READ BINARIO
                             READ BINARIO
                             READ BINARIO
                             REWRITE BINARIO-REG FROM OBJECTS-REC(1:1)
                             READ BINARIO
                             REWRITE BINARIO-REG FROM OBJECTS-REC(2:1)
                             CLOSE BINARIO
                        WHEN FileTypeXML
                             MOVE "   </Data> " TO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             STRING '</'     DELIMITED BY SIZE
                                    FILENAME DELIMITED BY SPACE
                                    '>'      DELIMITED BY SIZE
                                  INTO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             call "CBL_CLOSE_FILE" using handle
                        WHEN FileTypeJSON
                             IF OBJECTS-ROWS = 0
                                MOVE "{}" TO BUFFER
                             END-IF
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             MOVE "]}" TO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             call "CBL_CLOSE_FILE" using handle
                        WHEN OTHER
                             call "CBL_CLOSE_FILE" using handle
                    END-EVALUATE
                    DELETE OBJECTS RECORD
                    IF   CWSAVE-CLOSE-ALL
                         GO TO 000-INICIO
                    ELSE
                         MOVE SPACES TO PARAMETROS-CWSAVE
                    END-IF
               WHEN FS-OBJECTS = "23"
                    MOVE SPACES TO FILE-LABEL
                    STRING '"'          DELIMITED BY SIZE
                           OBJECTS-FILE  DELIMITED BY SPACE
                           '".'         DELIMITED BY SIZE
                                   INTO FILE-LABEL
                    MOVE 0 TO OBJECTS-REC OBJECTS-ROWS
                    ACCEPT HOJE FROM DATE
                    MOVE OBJECTS-FILE TO LB-BINARIO
                    PERFORM VARYING I FROM LENGTH OF LB-BINARIO
                                       BY -1
                                       UNTIL I = 1
                                       OR LB-BINARIO(I: 1) = "\" OR "/"
                            CONTINUE
                    END-PERFORM
                    IF  LB-BINARIO(I: 1) = "\" OR "/"
                        ADD 1 TO I
                    END-IF
                    MOVE LB-BINARIO(I: ) TO FILENAME
                    PERFORM VARYING I FROM LENGTH OF FILENAME
                                        BY -1
                                     UNTIL I = 1
                                        OR FILENAME(I: 1) = "."
                            CONTINUE
                    END-PERFORM
                    IF  FILENAME (I: 1) = "."
                        MOVE SPACES TO FILENAME (I: )
                    END-IF
                    PERFORM CREATE-FILE
                    MOVE SPACES TO EXTW
                    PERFORM VARYING I FROM 1 BY 1
                              UNTIL I > LENGTH OBJECTS-FILE
                                  OR (EXTW NOT = SPACES)
                             IF OBJECTS-FILE (I: 1) = "."
                                ADD 1 TO I
                                MOVE OBJECTS-FILE (I: ) TO EXTW
                                INSPECT EXTW CONVERTING
                                        MINUSCULAS TO MAIUSCULAS
                             END-IF
                    END-PERFORM
                    MOVE LB-BINARIO TO OBJECTS-FILE
                    EVALUATE TRUE
                        WHEN DBF
                             PERFORM VER-PREFIXO THRU FIM-VER-PREFIXO
                             MOVE 1  TO DBF-HEADER-LENGTH
                             MOVE 33 TO DBF-HEADER-TOPLEN
                             MOVE AA TO DBF-HEADER-AA
                             MOVE MM TO DBF-HEADER-MM
                             MOVE DD TO DBF-HEADER-DD
                             PERFORM VARYING F FROM 1 BY 1
                                       UNTIL F > 1024
                                       OR CWSAVE-DATANAME (F) = SPACES
                                          ADD 32 TO DBF-HEADER-TOPLEN
                                          IF  CWSAVE-LEN (F) NUMERIC
                                              ADD CWSAVE-LEN (F)
                                               TO DBF-HEADER-LENGTH
                                          ELSE
                                              ADD CWSAVE-MORE255 (F)
                                               TO DBF-HEADER-LENGTH
                                          END-IF
                                          IF  CWSAVE-NUMERIC (F)
                                              ADD 1 TO DBF-HEADER-LENGTH
                                              IF  CWSAVE-DEC (F) NUMERIC
                                              AND CWSAVE-DEC (F) > ZERO
                                                  ADD 1
                                                   TO DBF-HEADER-LENGTH
                                              END-IF
                                          END-IF
                             END-PERFORM
                             PERFORM VARYING I FROM 1 BY 1
                                       UNTIL I > LENGTH OF DBF-HEADER
                                       move DBF-HEADER(I:1) to b-r
                                       perform write-b
                             END-PERFORM
                             PERFORM VARYING F FROM 1 BY 1
                                       UNTIL F > 1024
                                       OR CWSAVE-DATANAME (F) = SPACES
                                     MOVE CWSAVE-DATANAME (F)(P:)
                                       TO DBF-FIELD-DATANAME
                                       INSPECT DBF-FIELD-DATANAME
                                        CONVERTING "-" TO "_"
                                       INSPECT DBF-FIELD-DATANAME
                                        CONVERTING MINUSCULAS
                                                TO MAIUSCULAS
                                     IF DBF-FIELD-DATANAME (10: 1) = "_"
                                        MOVE SPACE
                                          TO DBF-FIELD-DATANAME (10: 1)
                                     END-IF
                                     INSPECT DBF-FIELD-DATANAME
                                             CONVERTING SPACE
                                                     TO LOW-VALUE
                                     IF   CWSAVE-LEN (F) NUMERIC
                                          MOVE CWSAVE-LEN (F)
                                            TO DBF-FIELD-LENGTH
                                     ELSE
                                          MOVE CWSAVE-MORE255 (F)
                                            TO DBF-FIELD-LENGTH
                                     END-IF
                                     MOVE 0 TO DBF-FIELD-DEC
                                     IF   CWSAVE-NUMERIC (F)
                                          MOVE "N" TO DBF-FIELD-TYPE
                                          ADD 1 TO DBF-FIELD-LENGTH
                                          IF   CWSAVE-DEC (F) NUMERIC
                                               MOVE CWSAVE-DEC (F)
                                                 TO DBF-FIELD-DEC
                                               ADD 1 TO DBF-FIELD-LENGTH
                                          END-IF
                                     ELSE
                                          MOVE "C" TO DBF-FIELD-TYPE
                                     END-IF
                                     PERFORM VARYING I FROM 1 BY 1
                                           UNTIL I > LENGTH OF DBF-FIELD
                                              move DBF-FIELD(I:1) to b-r
                                              perform write-b
                                     END-PERFORM
                             END-PERFORM
                             move X"0D" to b-r perform write-b
                        WHEN FileTypeXML
                             PERFORM VER-PREFIXO THRU FIM-VER-PREFIXO
                             MOVE '<?xml version="1.0"?>' TO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             MOVE SPACES TO LB-XSL
                             PERFORM VARYING X FROM LENGTH LB-BINARIO
                                         BY -1
                                         UNTIL X = 1
                                         OR LB-BINARIO (X:1) = '\'
                                         OR LB-BINARIO (X:1) = '/'
                                     CONTINUE
                             END-PERFORM
                             IF X > 1
                                ADD 1 TO X
                             END-IF
                             STRING LB-BINARIO (X:) DELIMITED BY '.'
                                    '.xsl' DELIMITED BY SIZE
                                INTO LB-XSL
                             MOVE SPACES TO BUFFER
                             INSPECT LB-XSL converting '\' TO '/'
                             STRING
                               '<?xml-stylesheet type="text/xsl" href="'
                                      DELIMITED BY SIZE
                               LB-XSL DELIMITED BY SPACE
                               '"?>'  DELIMITED BY SIZE
                                 INTO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             MOVE "<!-- COBOLware 6.1"  TO BUFFER
                             MOVE REVISAO TO BUFFER(20:)
                             MOVE "-->"   TO BUFFER(37:)
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             EXEC COBOLware Time Today Normal
                                  DATE-FINAL;dia(1:8)
                                  TIME-FINAL;hora(1:6)
                             END-EXEC
                             EXEC COBOLware Time Edit Normal
                                  DATE dia(1:8)
                                  TIME hora(1:6)
                                  DATE-EDITED;dia
                                  TIME-EDITED;hora
                             END-EXEC
                             EXEC COBOLware GetSystem
                                  SCREEN-COMPANY;empresa
                                  SCREEN-APLICATION;sistema
                                  PROGRAM;programa
                                  USER;usuario
                             END-EXEC
                             INSPECT empresa converting
                                     ACENTOS-850 to acentos-off
                             INSPECT empresa converting
                                     ACENTOS-437 to acentos-off
                             INSPECT sistema converting
                                     ACENTOS-850 to acentos-off
                             INSPECT sistema converting
                                     ACENTOS-437 to acentos-off
                             MOVE SPACES TO BUFFER
                             STRING '<!-- ' dia ' ' hora ' '
                                    empresa ' ' sistema ' '
                                    programa ' ' usuario
                                    ' -->' DELIMITED BY SIZE
                               INTO BUFFER
                             CALL 'CWPACK' USING BUFFER LENGTH BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             STRING '<'      DELIMITED BY SIZE
                                    FILENAME DELIMITED BY SPACE
                                    '>'      DELIMITED BY SIZE
                                  INTO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             MOVE "   <Layout>" TO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             PERFORM VARYING F FROM 1 BY 1
                                       UNTIL F > 1024
                                       OR CWSAVE-DATANAME (F) = SPACES
                                       INSPECT CWSAVE-DATANAME (F)
                                               CONVERTING "-" TO "_"
                                       INSPECT CWSAVE-DATANAME (F)
                                               CONVERTING MAIUSCULAS
                                                       TO MINUSCULAS
                                       INSPECT CWSAVE-DATANAME(F) (P: 1)
                                               CONVERTING MINUSCULAS
                                                       TO MAIUSCULAS
                                       IF CWSAVE-NUMERIC (F)
                                          INSPECT CWSAVE-DATA (F)
                                               CONVERTING "+" TO " "
                                       END-IF
                                    MOVE "      <Field>" TO BUFFER
                                    PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                                    STRING "         <Name>"
                                           DELIMITED BY SIZE
                                           CWSAVE-DATANAME (F)(P:)
                                           DELIMITED BY SPACE
                                           "</Name>"
                                           DELIMITED BY SIZE
                                      INTO BUFFER
                                    PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                                    IF  CWSAVE-NUMERIC (F)
                                        MOVE
                             "         <Type>Numeric</Type>"
                                          TO BUFFER
                                    ELSE
                                        MOVE
                          "         <Type>Character</Type>"
                                          TO BUFFER
                                    END-IF
                                    PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                                    IF CWSAVE-LEN (F) NUMERIC
                                       MOVE CWSAVE-LEN (F) TO LEN
                                    ELSE
                                       MOVE CWSAVE-MORE255 (F) TO LEN
                                    END-IF
                                    IF  CWSAVE-NUMERIC (F)
                                        IF   CWSAVE-NUMERIC (F)
                                        AND  CWSAVE-DEC (F) > ZERO
                                             INSPECT CWSAVE-DATA(F)
                                                     CONVERTING ","
                                                             TO "."
                                        END-IF
                                    END-IF
                                    PERFORM VARYING I FROM 1 BY 1
                                              UNTIL I > LENGTH OF LEN
                                                 OR LEN(I:1) NOT = "0"
                                            CONTINUE
                                    END-PERFORM
                                    STRING "         <Length>"
                                           LEN(I:)
                                           "</Length>"
                                           DELIMITED BY SIZE
                                      INTO BUFFER
                                    PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                                    IF   CWSAVE-DEC (F) NUMERIC
                                    AND  CWSAVE-DEC (F) > ZERO
                                         MOVE CWSAVE-DEC (F) TO LEN
                                         PERFORM VARYING I FROM 1 BY 1
                                                UNTIL I > LENGTH OF LEN
                                                   OR LEN(I:1) NOT = "0"
                                                 CONTINUE
                                         END-PERFORM
                                         STRING "         <Decimals>"
                                                LEN(I:)
                                                "</Decimals>"
                                                DELIMITED BY SIZE
                                           INTO BUFFER
                                    ELSE
                                         MOVE "         <Decimals />"
                                           TO BUFFER
                                    END-IF
                                    PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                                    MOVE "      </Field>" TO BUFFER
                                    PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             END-PERFORM
                             MOVE "   </Layout>" TO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             MOVE "   <Data>" TO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             PERFORM GRAVA-XSL THRU FIM-GRAVA-XSL
                        WHEN FileTypeJSON
                             PERFORM VARYING I FROM 50 BY -1
                                        UNTIL I = 0
                                      OR LB-BINARIO (I: 1) = "\" OR "/"
                                     CONTINUE
                             END-PERFORM
                             MOVE SPACES TO BUFFER
                             ADD  1      TO I
                             STRING '{"'  DELIMITED BY SIZE
                                    LB-BINARIO(I:) DELIMITED BY '.'
                                    '": [' DELIMITED BY SIZE
                               INTO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                        WHEN RPX
                             STRING 'template='  DELIMITED BY SIZE
                                    LB-BINARIO   DELIMITED BY "."
                                    '.RPV'       DELIMITED BY SIZE
                                  INTO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                        WHEN OBJECTS-DELM NOT = SPACES
                             MOVE OBJECTS-FILE TO LB-BINARIO
                        WHEN OTHER
                             call "CBL_CLOSE_FILE" using handle
                             MOVE SPACES TO FILENAME2
                             PERFORM VARYING I FROM 1 BY 1
                                        UNTIL I > 50
                                      OR LB-BINARIO (I: 1) = "." OR " "
                                     CONTINUE
                             END-PERFORM
                             MOVE 0 TO REG-SIZE
                             PERFORM VARYING F FROM 1 BY 1
                                       UNTIL F > 1024
                                       OR CWSAVE-DATANAME (F) = SPACES
                                     IF CWSAVE-LEN (F) NUMERIC
                                        MOVE CWSAVE-LEN (F) TO LEN
                                     ELSE
                                        MOVE CWSAVE-MORE255 (F) TO LEN
                                     END-IF
                                     IF   CWSAVE-SIGNAL (F)
                                          ADD 1 TO LEN
                                     END-IF
                                     ADD LEN TO REG-SIZE
                             END-PERFORM
                             MOVE ".cpy" TO LB-BINARIO (I: )
                             PERFORM CREATE-FILE
                             STRING FILENAME DELIMITED BY SPACE
                                    "-REG." DELIMITED BY SIZE
                                INTO FILENAME2
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             MOVE LINHA-01 TO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             MOVE LINHA-02 TO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             MOVE LINHA-03 TO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             MOVE LINHA-04 TO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             MOVE LINHA-05 TO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             MOVE LINHA-06 TO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             IF  LINHA-07 (73: ) = SPACES
                                 MOVE LINHA-07 TO BUFFER
                                 PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                                 PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             ELSE
                                 MOVE FILE-LABEL TO FILE-LABEL2
                                 MOVE SPACES     TO FILE-LABEL
                                 MOVE LINHA-07 TO BUFFER
                                 PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                                 MOVE LINHA-07L TO BUFFER
                                 PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             END-IF
                             MOVE LINHA-08 TO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             PERFORM VER-PREFIXO THRU FIM-VER-PREFIXO
                             PERFORM VARYING F FROM 1 BY 1
                                       UNTIL F > 1024
                                       OR CWSAVE-DATANAME (F) = SPACES
                                       MOVE CWSAVE-DATANAME (F)(P:)
                                         TO DATANAME
                                       MOVE SPACES TO MASCARA
                                       IF  CWSAVE-NUMERIC (F)
                                           IF  CWSAVE-SIGNAL (F)
                                               MOVE ')-.' TO SM
                                           ELSE
                                               MOVE ').' TO SM
                                           END-IF
                                           IF   CWSAVE-DEC (F) NUMERIC
                                           AND  CWSAVE-DEC (F) > 0
                                                COMPUTE LEN2 =
                                                        CWSAVE-LEN (F)
                                                      - CWSAVE-DEC (F)
                                                STRING "9("
                                                       LEN2
                                                       ")V9("
                                                       CWSAVE-DEC (F)
                                                       SM
                                                       DELIMITED BY SIZE
                                                  INTO MASCARA
                                           ELSE
                                                STRING "9("
                                                       CWSAVE-LEN (F)
                                                       SM
                                                       DELIMITED BY SIZE
                                                  INTO MASCARA
                                           END-IF
                                       ELSE
                                           IF CWSAVE-LEN (F) NUMERIC
                                              STRING "X("
                                                  CWSAVE-LEN (F)
                                                  ")."
                                                  DELIMITED BY SIZE
                                               INTO MASCARA
                                           ELSE
                                              MOVE CWSAVE-MORE255 (F)
                                                TO LEN
                                              STRING "X("
                                                  LEN
                                                  ")."
                                                  DELIMITED BY SIZE
                                               INTO MASCARA
                                           END-IF
                                       END-IF
                                       MOVE LINHA-09 TO BUFFER
                                       PERFORM GRAVA-SEQ
                                          THRU FIM-GRAVA-SEQ
                             END-PERFORM
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             call "CBL_CLOSE_FILE" using handle
                             MOVE OBJECTS-FILE TO LB-BINARIO
                             PERFORM CREATE-FILE
                    END-EVALUATE
                    WRITE OBJECTS-REG
                    GO TO 000-INICIO
               WHEN FS-OBJECTS = "00"
                    ADD 1 TO OBJECTS-REC OBJECTS-ROWS
                    EVALUATE TRUE
                        WHEN DBF
                             move SPACE to b-r perform write-b
                             PERFORM VARYING F FROM 1 BY 1
                                       UNTIL F > 1024
                                       OR CWSAVE-DATANAME (F) = SPACES
                                  IF CWSAVE-LEN (F) NUMERIC
                                     MOVE CWSAVE-LEN (F) TO LEN
                                  ELSE
                                     MOVE CWSAVE-MORE255 (F) TO LEN
                                  END-IF
                                  IF   CWSAVE-NUMERIC (F)
                                       PERFORM JUST-NUM
                                          THRU FIM-JUST-NUM
                                  END-IF
                                  IF CWSAVE-LEN (F) NOT NUMERIC
                                     SET ADDRESS MORE255 TO
                                         CWSAVE-ADDRESS (F)
                                     PERFORM VARYING I FROM 1 BY 1
                                               UNTIL I > LEN
                                         move MORE255(I:1) to b-r
                                         perform write-b
                                     END-PERFORM
                                  ELSE
                                     PERFORM VARYING I FROM 1 BY 1
                                               UNTIL I > LEN
                                         move CWSAVE-DATA(F)(I:1) to b-r
                                         perform write-b
                                     END-PERFORM
                                  END-IF
                             END-PERFORM
                        WHEN FileTypeXML
                             PERFORM VER-PREFIXO THRU FIM-VER-PREFIXO
                             IF XMLNUM = 'ON'
                             MOVE OBJECTS-ROWS   TO xml-rec
                             perform varying nz from 1 by 1
                                   until xml-rec (nz:1) not = '0'
                             end-perform
                             STRING "      <Record_" DELIMITED BY SIZE
                                             xml-rec (nz:)
                                                     DELIMITED BY SIZE
                                                 ">" DELIMITED BY SIZE
                               INTO BUFFER
                             ELSE
                             MOVE "      <Record>" TO BUFFER
                             END-IF
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             PERFORM VARYING F FROM 1 BY 1
                                       UNTIL F > 1024
                                       OR CWSAVE-DATANAME (F) = SPACES
                             IF CWSAVE-LEN (F) NUMERIC
                                MOVE CWSAVE-LEN (F) TO LEN
                             ELSE
                                MOVE CWSAVE-MORE255 (F) TO LEN
                             END-IF
                             IF   CWSAVE-NUMERIC (F)
                                  PERFORM JUST-NUM THRU FIM-JUST-NUM
                             END-IF
                             MOVE SPACES to xml-w
                             INSPECT CWSAVE-DATANAME (F)
                                     CONVERTING "-" TO "_"
                             INSPECT CWSAVE-DATANAME (F)
                                     CONVERTING MAIUSCULAS
                                             TO MINUSCULAS
                             INSPECT CWSAVE-DATANAME(F) (P:1)
                                     CONVERTING MINUSCULAS
                                             TO MAIUSCULAS
                             IF CWSAVE-LEN (F) NUMERIC
                                IF CWSAVE-NUMERIC (F)
                                   INSPECT CWSAVE-DATA (F)
                                           CONVERTING "+" TO " "
                                END-IF
                                PERFORM VARYING I FROM 1 BY 1
                                     UNTIL I > 255
                                        OR (CWSAVE-DATA (F) (I: 1)
                                           NOT = SPACE)
                                        or len = zero
                                     SUBTRACT 1 FROM LEN
                                END-PERFORM
                                move CWSAVE-DATA (F) (I: LEN)
                                  to xml-w
                             ELSE
                                SET ADDRESS MORE255 TO
                                    CWSAVE-ADDRESS (F)
                                PERFORM VARYING I FROM 1 BY 1
                                     UNTIL I > LEN
                                        OR (MORE255 (I: 1)
                                           NOT = SPACE)
                                        or len = zero
                                     SUBTRACT 1 FROM LEN
                                END-PERFORM
                                move MORE255 (I: LEN)
                                  to xml-w
                             END-IF
                             inspect xml-data (1: xml-len)
                                     converting low-values TO space
                             IF   xml-w = SPACES
                                  MOVE 1 TO I
                                  MOVE 0 TO LEN
                             ELSE
                                  PERFORM VARYING LEN FROM LEN BY -1
                                     UNTIL LEN = 0
                                        OR (xml-w (LEN: 1) NOT = SPACE)
                                     CONTINUE
                                  END-PERFORM
                             END-IF
                             perform grava-xml thru fim-xml
                             STRING "         <"
                                    DELIMITED BY SIZE
                                    CWSAVE-DATANAME (F)(P:)
                                    DELIMITED BY SPACE
                                    ">"
                                    DELIMITED BY SIZE
                                    xml-data (1: xml-len)
                                    DELIMITED BY SIZE
                                    "</"
                                    DELIMITED BY SIZE
                                    CWSAVE-DATANAME (F)(P:)
                                    DELIMITED BY SPACE
                                    ">"
                                    DELIMITED BY SIZE
                               INTO BUFFER
                               PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             END-PERFORM
                             IF XMLNUM = 'ON'
                             STRING "      </Record_" DELIMITED BY SIZE
                                             xml-rec (nz:)
                                                     DELIMITED BY SIZE
                                                 ">" DELIMITED BY SIZE
                               INTO BUFFER
                             ELSE
                             MOVE "      </Record>" TO BUFFER
                             END-IF
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                        WHEN FileTypeJSON
                             if objects-rows > 1
                                MOVE ',' TO BUFFER
                                PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             end-if
                             SET ENTER-OFF TO TRUE
                             MOVE '{' TO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             PERFORM VARYING F FROM 1 BY 1
                                       UNTIL F > 1024
                                       OR CWSAVE-DATANAME (F) = SPACES
                             IF CWSAVE-LEN (F) NUMERIC
                                MOVE CWSAVE-LEN (F) TO LEN
                             ELSE
                                MOVE CWSAVE-MORE255 (F) TO LEN
                             END-IF
                             IF   CWSAVE-NUMERIC (F)
                                  PERFORM JUST-NUM THRU FIM-JUST-NUM
                             END-IF
                             MOVE SPACES to xml-w
                             IF CWSAVE-DATANAME (F + 1) = SPACES
                                MOVE SPACE TO FIM
                             ELSE
                                MOVE ','   TO FIM
                             END-IF
                             INSPECT CWSAVE-DATANAME (F)
                                     CONVERTING "-" TO "_"
                             IF CWSAVE-LEN (F) NUMERIC
                                IF CWSAVE-NUMERIC (F)
                                   INSPECT CWSAVE-DATA (F)
                                           CONVERTING "+" TO " "
                                END-IF
                                PERFORM VARYING I FROM 1 BY 1
                                     UNTIL I > 255
                                        OR (CWSAVE-DATA (F) (I: 1)
                                           NOT = SPACE)
                                        or len = zero
                                     SUBTRACT 1 FROM LEN
                                END-PERFORM
                                move CWSAVE-DATA (F) (I: LEN)
                                  to xml-w
                             ELSE
                                SET ADDRESS MORE255 TO
                                    CWSAVE-ADDRESS (F)
                                PERFORM VARYING I FROM 1 BY 1
                                     UNTIL I > LEN
                                        OR (MORE255 (I: 1)
                                           NOT = SPACE)
                                        or len = zero
                                     SUBTRACT 1 FROM LEN
                                END-PERFORM
                                move MORE255 (I: LEN)
                                  to xml-w
                             END-IF
                             inspect xml-data (1: xml-len)
                                     converting low-values TO space
                             IF   xml-w = SPACES
                                  MOVE 1 TO I
                                  MOVE 0 TO LEN
                             ELSE
                                  PERFORM VARYING LEN FROM LEN BY -1
                                     UNTIL LEN = 0
                                        OR (xml-w (LEN: 1) NOT = SPACE)
                                     CONTINUE
                                  END-PERFORM
                             END-IF
                             perform grava-xml thru fim-xml
                             inspect xml-data (1: xml-len)
                                     converting '"' TO "'"
                             move xml-data (1: xml-len) to teste-num
                             inspect teste-num (1: xml-len)
                                 converting '1234567890.' TO ZERO
                             inspect teste-num (1: 1)
                                 converting '-' TO ZERO
                             inspect teste-num (1: xml-len)
                                 converting 'TRUEFALS' TO 'truefals'
                             if  cwsave-data(F) (1: len) = low-values
                                 move 4      to xml-len
                                 move 'null' to xml-data (1:)
                                                teste-num
                             end-if
                             if  CWSAVE-NUMERIC (F)
                             and xml-len = zero
                                 move 1 to xml-len
                                 move '0' to xml-data (1: xml-len)
                             end-if
                             if (CWSAVE-NUMERIC (F)
                             and teste-num (1: xml-len) numeric)
                              or boolean-null
                                 STRING '"'
                                        DELIMITED BY SIZE
                                        CWSAVE-DATANAME (F)
                                        DELIMITED BY SPACE
                                        '":'
                                        DELIMITED BY SIZE
                                        xml-data (1: xml-len)
                                        DELIMITED BY SIZE
                                        FIM
                                        DELIMITED BY SIZE
                                   INTO BUFFER
                             else
                                 STRING '"'
                                        DELIMITED BY SIZE
                                        CWSAVE-DATANAME (F)
                                        DELIMITED BY SPACE
                                        '":"'
                                        DELIMITED BY SIZE
                                        xml-data (1: xml-len)
                                        DELIMITED BY SIZE
                                        '"'
                                        DELIMITED BY SIZE
                                        FIM
                                        DELIMITED BY SIZE
                                   INTO BUFFER
                             end-if
                               PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             END-PERFORM
                             MOVE '}'  TO BUFFER
                             PERFORM GRAVA-SEQ THRU FIM-GRAVA-SEQ
                             SET ENTER-ON TO TRUE
                        WHEN RPX
                             move "�" to b-r perform write-b
                             PERFORM VARYING F FROM 1 BY 1
                                       UNTIL F > 1024
                                       OR CWSAVE-DATANAME (F) = SPACES
                                  IF CWSAVE-LEN (F) NUMERIC
                                     PERFORM VARYING I FROM 1 BY 1
                                               UNTIL I > CWSAVE-LEN (F)
                                         IF CWSAVE-DATA(F) (I: 1) = "�"
                                            move "|" to b-r
                                            perform write-b
                                         ELSE
                                            move CWSAVE-DATA(F)(I:1)
                                              to b-r
                                            perform write-b
                                         END-IF
                                     END-PERFORM
                                  ELSE
                                     SET ADDRESS MORE255 TO
                                         CWSAVE-ADDRESS (F)
                                     MOVE CWSAVE-MORE255 (F) TO LEN
                                     PERFORM VARYING I FROM 1 BY 1
                                               UNTIL I > LEN
                                         IF MORE255 (I: 1) = "�"
                                            move "|" to b-r
                                            perform write-b
                                         ELSE
                                            move MORE255 (I:1)
                                              to b-r
                                            perform write-b
                                         END-IF
                                     END-PERFORM
                                  END-IF
                                     move "�" to b-r perform write-b
                             END-PERFORM
                             IF   CWUNIX-OFF
                                  move X"0D" to b-r perform write-b
                             END-IF
                             move X"0A" to b-r perform write-b
                        WHEN OTHER
                             PERFORM VARYING F FROM 1 BY 1
                                    UNTIL F > 1024
                                    OR CWSAVE-DATANAME (F) = SPACES
                               IF CWSAVE-LEN (F) NOT NUMERIC
                                  SET ADDRESS MORE255 TO
                                  CWSAVE-ADDRESS (F)
                                  MOVE CWSAVE-MORE255 (F) TO LEN
                                  MOVE MORE255 (1: LEN) TO xml-w
                               ELSE
                                   MOVE CWSAVE-LEN (F) TO LEN
                                   MOVE CWSAVE-DATA (F) TO xml-w
                               END-IF
                               IF   CWSAVE-NUMERIC (F)
                                    INSPECT CWSAVE-DATA(F)
                                            CONVERTING "+" TO " "
                                    IF CWSAVE-SIGNAL(F)
                                       ADD 1 TO LEN
                                    END-IF
                                    IF   CWSAVE-DEC (F) NUMERIC
                                    AND  CWSAVE-DEC (F) > ZERO
                                         ADD 1 TO LEN
                                    END-IF
                               END-IF
                               MOVE '*' TO ANTES
                               PERFORM VARYING I FROM 1 BY 1
                                         UNTIL I > LEN
                                  IF  (OBJECTS-DELM NOT = SPACE)
                                  AND CWSAVE-NUMERIC (F)
                                  AND ANTES = '*'
                                  AND xml-w (I: 1) = '0'
                                      EXIT PERFORM CYCLE
                                  END-IF
                                  IF  (OBJECTS-DELM NOT = SPACE)
                                  AND xml-w (I:1) = OBJECTS-DELM
                                     IF OBJECTS-DELM = '_'
                                       MOVE ';' TO xml-w (I: 1)
                                     ELSE
                                       MOVE '_' TO xml-w (I: 1)
                                     END-IF
                                  END-IF
                                  IF   CWSAVE-NUMERIC (F)
                                  AND((xml-w (I: 1) =
                                   '.' OR "," AND OBJECTS-DELM = SPACE)
                                   OR ((OBJECTS-DELM  NOT = SPACE)
                                  AND (xml-w (I: 1) = ' ')))
                                       CONTINUE
                                  ELSE
                                     IF (OBJECTS-DELM  NOT = SPACE)
                                     AND I = LEN
                                     AND xml-w(I:1) = SPACE
                                         EXIT PERFORM CYCLE
                                     END-IF
                                     IF (OBJECTS-DELM  NOT = SPACE)
                                     AND F < LEN
                                     AND xml-w(I:1) = SPACE
                                     AND xml-w(I + 1: 1) = SPACE
                                         EXIT PERFORM CYCLE
                                     END-IF
                                     move xml-w(I:1) to b-r
                                     perform write-b
                                     MOVE xml-w (I: 1)
                                       TO ANTES
                                  END-IF
                               END-PERFORM
                               IF OBJECTS-DELM NOT = SPACE
                               AND (CWSAVE-DATANAME (F + 1)
                                NOT = SPACE)
                                  move OBJECTS-DELM to b-r
                                  perform write-b
                               END-IF
                             END-PERFORM
                             IF   CWUNIX-OFF
                                  move X"0D" to b-r
                                  perform write-b
                             END-IF
                             move X"0A" to b-r
                             perform write-b
                    END-EVALUATE
                    REWRITE OBJECTS-REG
           END-EVALUATE.

       000-99-FIM. GOBACK.

       JUST-NUM.

           ADD 1 TO LEN
           IF   CWSAVE-DEC (F) NUMERIC
           AND  CWSAVE-DEC (F) > ZERO
                ADD 1 TO LEN
                INSPECT CWSAVE-DATA(F) CONVERTING "," TO "."
           END-IF
           MOVE CWSAVE-DATA(F)  TO BUFFER
           MOVE ZEROS           TO CWSAVE-DATA(F)
           MOVE BUFFER (LEN: 1) TO SINAL
           IF  SINAL = "+"
               MOVE SPACE TO SINAL
           END-IF
           MOVE SPACE        TO BUFFER (LEN: 1)
           MOVE BUFFER (1: ) TO CWSAVE-DATA(F) (2: )
           MOVE SPACES       TO BUFFER
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > LEN
                      OR CWSAVE-DATA(F) (I:1) > "0"
                      OR (I = LEN AND FileTypeJSON)
                   MOVE SPACE TO CWSAVE-DATA(F) (I:1)
           END-PERFORM
           IF SINAL NOT = SPACE
           AND I > 1
               SUBTRACT 1 FROM I
               MOVE SINAL TO CWSAVE-DATA(F) (I:1)
           END-IF
           IF   CWSAVE-DEC (F) NUMERIC
           AND  CWSAVE-DEC (F) > ZERO
                COMPUTE I = LEN - CWSAVE-DEC (F) + 1
                INSPECT CWSAVE-DATA(F) (I:1) CONVERTING SPACE TO ZERO
                PERFORM VARYING I FROM LEN BY -1
                          UNTIL I = 0
                             OR CWSAVE-DATA(F) (I:1) = '.' OR SPACE
                          CONTINUE
                END-PERFORM
                IF I > 0
                   IF CWSAVE-DATA(F) (I:1) = ' '
                      MOVE '.' TO CWSAVE-DATA(F) (I:1)
                      IF I > 1
                         SUBTRACT 1 FROM I
                         IF CWSAVE-DATA(F) (I:1) = ' '
                            MOVE '0' TO CWSAVE-DATA(F) (I:1)
                         END-IF
                      END-IF
                   END-IF
                END-IF
           END-IF.

       FIM-JUST-NUM. EXIT.

       GRAVA-SEQ.

           IF BUFFER NOT = SPACES
              PERFORM VARYING byte-count FROM LENGTH OF BUFFER BY -1
                            UNTIL byte-count = 0
                               OR (BUFFER(byte-count: 1) NOT = SPACE)
                      CONTINUE
              END-PERFORM
              call "CBL_WRITE_FILE" using handle
                                          offset
                                          byte-count
                                          X"00"
                                          BUFFER
              add  byte-count          to offset
              MOVE 1 TO byte-count
              MOVE SPACES TO BUFFER.

       GRAVA-ENTER.

           IF   ENTER-ON
                IF   CWUNIX-OFF
                     move X"0D" to b-r perform write-b
                END-IF
                move X"0A" to b-r perform write-b
           END-IF.

       FIM-GRAVA-SEQ. EXIT.

       grava-xml.

           move spaces to xml-data
           move 0      to xml-len
           PERFORM VARYING W FROM 1 BY 1 UNTIL W > LEN
                if xml-w (W: 1) = space
                   add 1 to xml-len
                else
                   MOVE xml-w (W: 1) TO XN (1:1)
                   COMPUTE XI = XN + 1
                   MOVE xml-string (XI) to xml-buffer
                   perform varying xi
                      from 1 by 1
                      until xi > 5
                         or xml-buffer(xi:1) = space
                      add 1 to xml-len
                      move xml-buffer(xi:1) to xml-data(xml-len:1)
                   end-perform
                end-if
           end-perform.

       FIM-xml. EXIT.

       GRAVA-XSL.

           MOVE LB-BINARIO TO LB-XSL
           PERFORM VARYING X FROM LENGTH LB-XSL
                       BY -1
                       UNTIL X = 1
                       OR LB-XSL (X:1) = '.'
                   CONTINUE
           END-PERFORM
           ADD 1 TO X
           MOVE 'xsl' TO LB-XSL (X:)
           OPEN OUTPUT XSL
           WRITE XSL-REG FROM '<?xsl version="1.0" ?>'
           WRITE XSL-REG FROM
           ' <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/
      -    '1999/XSL/Transform">'
           WRITE XSL-REG FROM ' <xsl:template match="/">'
           MOVE " <!-- COBOLware 6.1"  TO XSL-REG
           MOVE REVISAO TO XSL-REG(21:)
           MOVE "-->"   TO XSL-REG(38:)
           WRITE XSL-REG
           MOVE SPACES TO BUFFER XSL-REG
           STRING ' <!-- ' dia ' ' hora ' '
                  empresa ' ' sistema ' '
                  programa ' ' usuario
                  ' -->' DELIMITED BY SIZE
             INTO BUFFER
           CALL 'CWPACK' USING BUFFER LENGTH BUFFER
           MOVE BUFFER TO XSL-REG (2: )
           WRITE XSL-REG
           WRITE XSL-REG FROM '   <html>'
           WRITE XSL-REG FROM '   <body>'
           MOVE SPACES TO BUFFER XSL-REG
           STRING '    <h2>' delimited by size
           filename delimited by space '.xml '
                  dia ' ' hora ' '
                  empresa ' ' sistema
                  '</h2>' DELIMITED BY SIZE
             INTO BUFFER
           CALL 'CWPACK' USING BUFFER LENGTH BUFFER
           MOVE BUFFER TO XSL-REG (6: )
           WRITE XSL-REG
           WRITE XSL-REG FROM '     <table border="0">'
           WRITE XSL-REG FROM '       <tr bgcolor="#33CCFF">'
           PERFORM VER-PREFIXO THRU FIM-VER-PREFIXO
           PERFORM VARYING F FROM 1 BY 1
                     UNTIL F > 1024
                     OR CWSAVE-DATANAME (F) = SPACES
                   MOVE SPACES TO XSL-REG
                   STRING '         <th>'
                         DELIMITED BY SIZE
                         CWSAVE-DATANAME (F)(P:)
                         DELIMITED BY SPACE
                         '</th>'
                         DELIMITED BY SIZE
                   INTO XSL-REG
                   WRITE XSL-REG
           END-PERFORM
           WRITE XSL-REG FROM '       </tr>'
           MOVE SPACES TO XSL-REG
           STRING
                '       <xsl:for-each select="'
                 DELIMITED BY SIZE
                 FILENAME
                         DELIMITED BY SPACE
                 '/Data/Record">'
                 DELIMITED BY SIZE
                 INTO XSL-REG
           WRITE XSL-REG
           WRITE XSL-REG FROM '         <tr>'
           PERFORM VARYING F FROM 1 BY 1
                     UNTIL F > 1024
                     OR CWSAVE-DATANAME (F) = SPACES
           MOVE SPACES TO XSL-REG
           IF   CWSAVE-NUMERIC (F)
                STRING
                '           <td><p align="right"><xsl:value-of select="'
                 DELIMITED BY SIZE
                 CWSAVE-DATANAME (F)(P:)
                         DELIMITED BY SPACE
                 '"/></p></td>'
                 DELIMITED BY SIZE
                 INTO XSL-REG
           ELSE
                STRING
                '           <td><xsl:value-of select="'
                 DELIMITED BY SIZE
                 CWSAVE-DATANAME (F)(P:)
                         DELIMITED BY SPACE
                 '"/></td>'
                 DELIMITED BY SIZE
                 INTO XSL-REG
           END-IF
           WRITE XSL-REG
           END-PERFORM
           WRITE XSL-REG FROM '         </tr>'
           WRITE XSL-REG FROM '       </xsl:for-each>'
           WRITE XSL-REG FROM '     </table>'
           WRITE XSL-REG FROM '   </body>'
           WRITE XSL-REG FROM '   </html>'
           WRITE XSL-REG FROM ' </xsl:template>'
           WRITE XSL-REG FROM '</xsl:stylesheet>'
           CLOSE XSL.

       FIM-GRAVA-XSL. EXIT.

       VER-PREFIXO.

           MOVE 0 TO P
                     PS
           PERFORM VARYING F FROM 1 BY 1
                     UNTIL F > 1024
                     OR CWSAVE-DATANAME (F) = SPACES
                        PERFORM VARYING PT FROM 1 BY 1
                                UNTIL CWSAVE-DATANAME (F) (PT:1) = SPACE
                                IF CWSAVE-DATANAME (F) (PT:1) = '_'
                                    OR '-'
                                   IF P = 0
                                      MOVE PT TO P
                                      ADD 1 TO PS
                                   ELSE
                                       IF P = PT
                                          ADD 1 TO PS
                                       END-IF
                                   END-IF
                                   EXIT PERFORM CYCLE
                                END-IF
                        END-PERFORM
           END-PERFORM
           SUBTRACT 1 FROM F
           IF PS = F
              COMPUTE P = P + 1
           ELSE
              MOVE 1 TO P
           END-IF.

       FIM-VER-PREFIXO. EXIT.

       CREATE-FILE.

           OPEN OUTPUT BINARIO
           MOVE FS TO SAVE-FS
           CLOSE BINARIO
           IF  SAVE-FS = '00'
               DELETE FILE BINARIO
           END-IF
           MOVE SAVE-FS TO FS
           move 0       to offset
           call "CBL_CREATE_FILE" using LB-BINARIO
                                        X"02" X"00" X"00"
                                        handle.

       write-b.

            call "CBL_WRITE_FILE" using handle
                             offset
                             byte-count
                             X"00"
                             b-r
            add  byte-count to offset.
      *     rewrite objects-reg.

       END PROGRAM CWSAVE.

