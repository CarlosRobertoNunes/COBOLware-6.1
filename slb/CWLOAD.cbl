       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWLOAD.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/06/2009.
       SECURITY.      *************************************************
                      *                                               *
                      *  Carga de arquivos                            *
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
                  FILE STATUS   IS FS-BINARIO.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT OBJECTS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS OBJECTS-FILE
                  LOCK MODE     IS EXCLUSIVE
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-OBJECTS.

           SELECT FILES   ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS FILES-KEY
                  LOCK MODE     IS EXCLUSIVE
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-FILES.

           SELECT UTF8 ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS UTF8-KEY
                  LOCK MODE     IS EXCLUSIVE
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-UTF8.

       DATA DIVISION.
       FILE SECTION.

       FD  BINARIO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-BINARIO.

       01  BINARIO-REG.
           05 BINARIO-CARACTER PIC X.
           05 BINARIO-ASCII REDEFINES BINARIO-CARACTER
                              PIC 99 COMP-X.

       FD  OBJECTS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-OBJECTS.

       01  OBJECTS-REG.
           05 OBJECTS-FILE          PIC  X(255).
           05 OBJECTS-ID     COMP-5 PIC S9(004).
           05 OBJECTS-REC    COMP-5 PIC S9(004).
           05 EXTW                  PIC  X(004).
              88 DBF                            VALUE "DBF".
              88 RPX                            VALUE "RPX".
              88 FileTypeXML                    VALUE "XML".
              88 FileTypeJSON                   VALUE "JSON".
              88 FileTypeUTF8                   VALUE "XML" "JSON".
           05 OBJECTS-DELM          PIC  X(001).
           05 OBJECTS-RECLEN COMP-5 PIC S9(004).
           05 OBJECTS-FIELDS        PIC  9(004).
           05 OBJECTS-STATUS        PIC  X(002).
           05 OBJECTS-POS           PIC  9(004) OCCURS 1024.

       FD  FILES
           LABEL RECORD IS STANDARD
           RECORD VARYING FROM 5 TO 32773 DEPENDING ON SZ-FILES
           VALUE OF FILE-ID IS LB-FILES.

       01  FILES-REG.
           05 FILES-KEY.
              10 FILES-ID  COMP-5 PIC S9(004).
              10 FILES-SEQ COMP-5 PIC S9(004).
           05 FILES-RECORD.
              10 OCCURS 1 TO 32768 DEPENDING ON OBJECTS-RECLEN
                                  PIC  X(001).
       FD  UTF8
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-UTF8.

       01  UTF8-REG.
           05 UTF8-KEY         PIC  X(005).
           05 UTF8-CHAR        PIC  X(001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 WS-FILE          PIC  X(255) VALUE SPACES.
           05 TESTE-NUM        PIC  X(030) VALUE SPACES.
           05 ACHOU            PIC  9(002) VALUE 0.
           05 POS-NUM          PIC  9(002) VALUE 0.
           05 ASPA             PIC  9(001) VALUE 0.
           05 FIELD            PIC  9(004) VALUE 0.
           05 POS              PIC  9(004) VALUE 0.
           05 LEN              PIC  9(004) VALUE 0.
           05 NS               PIC  9(002) VALUE 0.
           05 I                PIC  9(004) VALUE 0.
           05 S                PIC  9(004) VALUE 0.
           05 NEGATIVOS        PIC  X(010) VALUE 'pqrstuvwxy'.
           05 IDS      COMP-5  PIC S9(004) VALUE 0.
           05 SZ-FILES COMP-5  PIC S9(004) VALUE 0.
           05 ER-BINARIO.
              10 FS-BINARIO    PIC  X(002) VALUE "00".
              10 LB-BINARIO    PIC  X(255) VALUE SPACES.
           05 ER-OBJECTS.
              10 FS-OBJECTS    PIC  X(002) VALUE "00".
              10 LB-OBJECTS    PIC  X(255) VALUE "$TEMP/cwload".
           05 ER-FILES.
              10 FS-FILES      PIC  X(002) VALUE "00".
              10 LB-FILES      PIC  X(255) VALUE "$TEMP/cwload2".
           05 ER-UTF8.
              10 FS-UTF8       PIC  X(002) VALUE "00".
              10 LB-UTF8       PIC  X(255) VALUE "$TEMP/cwload3".
           05 NUMERO                   PIC  9(018) VALUE 0.
           05 REDEFINES NUMERO.
              10 DIGITO                PIC  9(001) OCCURS 18.
           05 X                        PIC  9(005) VALUE ZERO.
           05 X91-RESULT    COMP-X     PIC  9(002) VALUE 0.
           05 X91-FUNCTION  COMP-X     PIC  9(002) VALUE 16.
           05 X91-PARAMETER COMP-X     PIC  9(002) VALUE 0.

       01  AREAS-FILES.
       02  AREAS-DBF.
           05 Y                        PIC  9(005) VALUE ZERO.
           05 Z                        PIC  9(005) VALUE ZERO.
           05 N                        PIC  9(005) VALUE ZERO.
           05 R                        PIC  9(005) VALUE ZERO.
           05 L                        PIC  9(005) VALUE ZERO.
           05 END-HEADER               PIC  9(001) VALUE 0.
           05 DEFINE-CAMPO VALUE SPACES.
              10 NOME-CAMPO.
                 15 BYTE-N              PIC X OCCURS 10.
              10 TIPO-CAMPO             PIC X.
                 88 ALFA   VALUE "C" "L" "M".
                 88 LOGICA VALUE "L".
                 88 NUM    VALUE "N" "D".
                 88 DIA    VALUE "D".
                 88 VALOR  VALUE "N".
              10 SIZE-CAMPO             PIC 9(003).
              10 DECS-CAMPO             PIC 9(003).
                 88 COM-DECIMAIS VALUE 1 THRU 999.
              10 FIM-NOME               PIC X(001).
           05 REG-SIZE-DBASE3           PIC  9(005) VALUE 1.
           05 TESTE-ATIVO               PIC  X(001) VALUE SPACE.
              88 DELETADO                          VALUE "*".
           05 NUMERICO.
              10 INTEIRO     OCCURS 20 PIC X.
              10 DECIMAL     OCCURS 20 PIC X.
           05 SINAL                    PIC  X(001) VALUE SPACE.
       02  AREAS-XML.
           05 DATANAME         PIC  X(030) VALUE SPACES.
           05 XML-TABLE        PIC  9(001) VALUE 0.
           05 FLAG-TABLE       PIC  9(001) VALUE 0.
           05 MNIVEL    COMP-5 PIC S9(004) VALUE 0.
           05 TS        COMP-5 PIC S9(004) VALUE 0.
           05 NIVEL     COMP-5 PIC S9(004) VALUE 0.
           05 TAG-STATUS       PIC  9(001) VALUE 0.
           05 FECHA            PIC  X(001) VALUE SPACE.
           05 TAG              PIC X(5000) VALUE SPACES.
           05 PARTE-CONV       PIC  X(005) VALUE SPACES.

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
            03 pic x(5) value spaces. *> ".".
            03 pic x(5) value spaces.
            03 pic x(5) value spaces.
            03 pic x(5) value spaces. *> ".".
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

       LINKAGE SECTION.

       COPY CWLOAD.
       01  MORE255 PIC X.
       PROCEDURE DIVISION USING PARAMETROS-CWLOAD CWLOAD-FILE.

       000-INICIO.

           ON 1
              OPEN I-O OBJECTS FILES.

           IF   CWLOAD-OLDFILE = SPACES
                CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
                IF   X91-PARAMETER > 1
                     MOVE CWLOAD-FILE TO WS-FILE
                ELSE
                     MOVE SPACES      TO WS-FILE
                END-IF
           ELSE
                MOVE CWLOAD-OLDFILE   TO WS-FILE
           END-IF
           MOVE WS-FILE TO OBJECTS-FILE
           READ OBJECTS

           IF FS-OBJECTS = '23'
           AND (NOT CWLOAD-CLOSE)
              MOVE WS-FILE TO OBJECTS-FILE
              OPEN INPUT BINARIO
              IF FS-BINARIO NOT = '00'
                 MOVE FS-BINARIO TO CWLOAD-STATUS(1:2)
                 GOBACK
              END-IF
              INITIALIZE OBJECTS-REG
              MOVE 1 TO REG-SIZE-DBASE3
              MOVE WS-FILE TO OBJECTS-FILE
              ADD  1   TO IDS
              MOVE IDS TO OBJECTS-ID
              MOVE SPACES TO EXTW
              PERFORM VARYING I FROM LENGTH OF LB-BINARIO
                                 BY -1
                                 UNTIL I = 1
                                 OR LB-BINARIO(I: 1) = "."
                      CONTINUE
              END-PERFORM
              IF  LB-BINARIO (I: 1) = "."
                  ADD  1                TO I
                  MOVE LB-BINARIO (I: ) TO EXTW
                  INSPECT EXTW CONVERTING MINUSCULAS TO MAIUSCULAS
              END-IF
              MOVE 1 TO X
              PERFORM VARYING I FROM 1 BY 1
                        UNTIL I > 1024
                           OR CWLOAD-DATANAME (I) = SPACES
                      IF  FileTypeJSON
                          INSPECT CWLOAD-DATANAME (I)
                             CONVERTING MINUSCULAS TO MAIUSCULAS
                          INSPECT CWLOAD-DATANAME (I)
                             CONVERTING '-' TO '_'
                      END-IF
                      MOVE X              TO OBJECTS-POS (I)
                      IF   CWLOAD-LEN(I)  NUMERIC
                           ADD  CWLOAD-LEN(I)      TO X
                      ELSE
                           ADD  CWLOAD-MORE255(I)  TO X
                      END-IF
                      MOVE I              TO OBJECTS-FIELDS
              END-PERFORM
              COMPUTE OBJECTS-RECLEN = X - 1
              COMPUTE SZ-FILES = OBJECTS-RECLEN + 4
              INITIALIZE AREAS-FILES
                         FILES-REG
              MOVE SPACES TO FILES-RECORD
              EVALUATE TRUE
                  WHEN DBF
                       PERFORM 010-GET-DBF  THRU 010-99-FIM
                  WHEN FileTypeXML
                       PERFORM 020-GET-XML  THRU 020-99-FIM
                  WHEN FileTypeJSON
                       PERFORM 120-GET-JSON THRU 120-99-FIM
                       IF ACHOU = 0
                          MOVE '35' TO OBJECTS-STATUS(1:2)
                       END-IF
                  WHEN RPX
                       PERFORM 030-GET-RPX  THRU 030-99-FIM
                  WHEN CWLOAD-DELIMITER NOT = SPACES
                       PERFORM 040-GET-TXT  THRU 040-99-FIM
                  WHEN OTHER
                       PERFORM 050-GET-SEQ  THRU 050-99-FIM
              END-EVALUATE
              CLOSE BINARIO
              WRITE OBJECTS-REG
          END-IF

          MOVE OBJECTS-ID  TO FILES-ID
          MOVE 0           TO FILES-SEQ
          START FILES KEY NOT LESS FILES-KEY
           NOT INVALID KEY
             MOVE SPACES TO FILES-RECORD
             READ FILES NEXT RECORD
          END-START

          IF  CWLOAD-CLOSE
              PERFORM UNTIL FS-FILES > '09'
                         OR(FILES-ID NOT = OBJECTS-ID)
                      DELETE FILES RECORD
                      MOVE SPACES TO FILES-RECORD
                      READ FILES NEXT RECORD
              END-PERFORM
          END-IF

          IF FS-FILES > '09'
          OR FILES-ID NOT = OBJECTS-ID
             IF  CWLOAD-CLOSE
                 MOVE '00' TO CWLOAD-STATUS(1:2)
             ELSE
                 MOVE '10' TO CWLOAD-STATUS(1:2)
                 IF OBJECTS-STATUS NOT = SPACES
                    MOVE OBJECTS-STATUS TO CWLOAD-STATUS(1:2)
                 END-IF
             END-IF
             IF  OBJECTS-ID = IDS
                 SUBTRACT 1 FROM IDS
             END-IF
             DELETE OBJECTS RECORD
          ELSE
             MOVE '00' TO CWLOAD-STATUS(1:2)
             PERFORM VARYING FIELD FROM 1 BY 1
                                   UNTIL FIELD > OBJECTS-FIELDS
                     MOVE OBJECTS-POS (FIELD)     TO POS
                     IF CWLOAD-LEN  (FIELD) NUMERIC
                        MOVE CWLOAD-LEN  (FIELD) TO LEN
                        MOVE FILES-RECORD (POS: LEN)
                          TO CWLOAD-DATA (FIELD)
                        IF  CWLOAD-NUMERIC (FIELD)
                        AND (CWLOAD-DATA (FIELD) (1: LEN) = SPACES)
                            MOVE ALL "0" TO CWLOAD-DATA (FIELD) (1: LEN)
                        END-IF
                     ELSE
                        MOVE CWLOAD-MORE255 (FIELD)     TO LEN
                        SET ADDRESS MORE255 TO CWLOAD-ADDRESS (FIELD)
                        MOVE FILES-RECORD (POS: LEN)
                          TO MORE255 (1: LEN)
                     END-IF
             END-PERFORM
             DELETE FILES RECORD
          END-IF.

       000-99-FIM. GOBACK.

       010-GET-DBF.

           MOVE 1 TO R
                     REG-SIZE-DBASE3
           MOVE 0 TO X

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 32
                   READ BINARIO
           END-PERFORM

           PERFORM UNTIL END-HEADER = 1
                   MOVE SPACES TO DEFINE-CAMPO
                   PERFORM VARYING I FROM 1 BY 1 UNTIL I > 32
                           READ BINARIO
                           IF   BINARIO-ASCII = 13
                           AND  I = 1
                                MOVE 1  TO END-HEADER
                                MOVE 33 TO I
                           END-IF
                           IF   I < 11
                           AND  FIM-NOME NOT = "S"
                                IF   BINARIO-ASCII NOT = ZERO
                                     IF   BINARIO-CARACTER = "_"
                                          MOVE "-" TO BINARIO-CARACTER
                                     END-IF
                                     MOVE BINARIO-CARACTER TO BYTE-N (I)
                                ELSE
                                     MOVE "S" TO FIM-NOME
                                END-IF
                           END-IF
                           IF   I = 12
                                MOVE BINARIO-CARACTER TO TIPO-CAMPO
                           END-IF
                           IF   I = 17
                                MOVE BINARIO-ASCII TO SIZE-CAMPO
                                ADD  SIZE-CAMPO    TO REG-SIZE-DBASE3
                           END-IF
                           IF   I = 18
                           AND  BINARIO-ASCII NOT = ZERO
                                MOVE BINARIO-ASCII TO DECS-CAMPO
                           END-IF
                   END-PERFORM
                   IF   DEFINE-CAMPO NOT = SPACES
                        IF   X > 1023
                             EXEC COBOLware Send Message
                                   "Limite de 1.024 campos excedido"
                             END-EXEC
                             MOVE "36" TO CWLOAD-STATUS(1:2)
                             EXIT PERFORM
                        END-IF
                        INSPECT NOME-CAMPO CONVERTING ACENTOS-850
                                                   TO ACENTOS-OFF
                        INSPECT NOME-CAMPO CONVERTING ACENTOS-437
                                                   TO ACENTOS-OFF
                        INSPECT NOME-CAMPO CONVERTING ACENTOS-WINDOWS
                                                   TO ACENTOS-OFF
                        INSPECT NOME-CAMPO CONVERTING MINUSCULAS
                                                   TO MAIUSCULAS
                        ADD  1            TO             X
                        MOVE DEFINE-CAMPO TO CWLOAD-DATA(X)
                   END-IF
           END-PERFORM

           MOVE 0 TO POS
           PERFORM UNTIL FS-BINARIO > "09"
                   PERFORM VARYING I FROM 1 BY 1
                             UNTIL I > REG-SIZE-DBASE3
                           READ BINARIO
                           IF   R = 1
                                MOVE 2 TO R
                                IF   BINARIO-CARACTER = X"00"
                                     READ BINARIO
                                END-IF
                           END-IF
                           IF   FS-BINARIO = "10"
                                COMPUTE I = REG-SIZE-DBASE3 + 1
                                MOVE "*" TO TESTE-ATIVO
                           END-IF
                           IF   I = 1
                           AND (FS-BINARIO = "00")
                                MOVE BINARIO-CARACTER TO TESTE-ATIVO
                                MOVE "+"              TO FIM-NOME
                                MOVE ZERO             TO Y
                           ELSE
                                IF   NOT DELETADO
                                     PERFORM 015-FROM-DBF
                                        THRU 015-99-FIM
                                END-IF
                           END-IF
                   END-PERFORM
                   IF   NOT DELETADO
                   AND (FS-BINARIO = '00')
                   AND (FILES-REG NOT = SPACES)
                        PERFORM 400-GRAVAR THRU 400-99-FIM
                   END-IF
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 1024
                                         OR CWLOAD-DATANAME (I) = SPACES
                   MOVE SPACES         TO CWLOAD-DATA (I)
           END-PERFORM.

       010-99-FIM. EXIT.

       015-FROM-DBF.

           IF   FIM-NOME = "+"
                ADD  1               TO Y
                MOVE ZERO            TO Z
                                        N
                MOVE SPACES          TO NUMERICO
                MOVE "+"             TO SINAL
                MOVE CWLOAD-DATA (Y) TO DEFINE-CAMPO
                MOVE 0               TO POS
                PERFORM VARYING X FROM 1 BY 1
                                 UNTIL X > 1024
                                    OR CWLOAD-DATANAME (X) = SPACES
                         IF CWLOAD-DATANAME (X) = NOME-CAMPO
                            MOVE OBJECTS-POS (X) TO POS
                            EXIT PERFORM
                         END-IF
                END-PERFORM
           END-IF

           ADD 1 TO Z

           IF   Z NOT > SIZE-CAMPO
           AND  POS > 0
                PERFORM 200-GET-BYTE THRU 200-99-FIM
           END-IF

           IF  (I NOT < REG-SIZE-DBASE3)
           OR  (Z NOT < SIZE-CAMPO)
                IF   CWLOAD-NUMERIC (X)
                AND  POS > 0
                     PERFORM 300-PUT-NUM THRU 300-99-FIM
                END-IF
                MOVE "+" TO FIM-NOME
           END-IF.

       015-99-FIM. EXIT.

       020-GET-XML.

           PERFORM 060-OPEN-UFT8 THRU 060-99-FIM.

           MOVE 0 TO X
           IF CWLOAD-TABLE NOT = SPACES
              INSPECT CWLOAD-TABLE CONVERTING MINUSCULAS TO MAIUSCULAS
              MOVE 1 TO XML-TABLE
           END-IF
           PERFORM UNTIL FS-BINARIO > '09'
              READ BINARIO
              IF FS-BINARIO < '10'
                 EVALUATE TRUE
                    WHEN (BINARIO-CARACTER = X'0D' OR X'0A')
                      OR (BINARIO-CARACTER = SPACES
                      AND POS = 0)
                         CONTINUE
                    WHEN BINARIO-CARACTER = '<'
                     AND TAG-STATUS = 0
                     AND POS > 0
                         IF  CWLOAD-NUMERIC (X)
                             PERFORM 300-PUT-NUM THRU 300-99-FIM
                         END-IF
                         READ BINARIO
                         IF FS-BINARIO < '10'
                            IF BINARIO-CARACTER = '/'
                            OR CWLOAD-NUMERIC (X)
                               MOVE 0 TO POS
                               PERFORM UNTIL BINARIO-CARACTER = '>'
                                          OR FS-BINARIO > '09'
                                       READ BINARIO
                               END-PERFORM
                               MOVE 0 TO TAG-STATUS
                               SUBTRACT 1 FROM NIVEL
                            ELSE
                               MOVE '<'  TO FILES-RECORD(POS:1)
                               ADD  1    TO POS
                               PERFORM 200-GET-BYTE THRU 200-99-FIM
                            END-IF
                         END-IF
                    WHEN BINARIO-CARACTER = '<'
                     AND TAG-STATUS = 0
                         ADD  1      TO NIVEL
                         MOVE 1      TO TAG-STATUS
                         MOVE 0      TO TS
                         MOVE SPACES TO TAG
                    WHEN BINARIO-CARACTER = '>'
                     AND TAG-STATUS = 1
                     AND POS = 0
                     IF  XML-TABLE = 1
                         INSPECT TAG (1: 51) CONVERTING ACENTOS-WINDOWS
                                                     TO ACENTOS-OFF
                         INSPECT TAG (1: 51)  CONVERTING MINUSCULAS
                                                   TO MAIUSCULAS
                         IF  CWLOAD-TABLE = TAG (1: 51)
                             MOVE 0 TO XML-TABLE
                         END-IF
                     END-IF
                     IF  XML-TABLE = 0
                     AND(NIVEL = MNIVEL
                     OR  MNIVEL = 0)
                         MOVE TAG TO DATANAME
                         INSPECT DATANAME  CONVERTING ACENTOS-WINDOWS
                                                   TO ACENTOS-OFF
                         INSPECT DATANAME  CONVERTING MINUSCULAS
                                                   TO MAIUSCULAS
                         PERFORM VARYING X FROM 1 BY 1
                                   UNTIL X > 1024
                                      OR CWLOAD-DATANAME (X) = SPACES
                                  IF CWLOAD-DATANAME (X) = DATANAME
                                     MOVE OBJECTS-POS (X) TO POS
                                     EXIT PERFORM
                                  END-IF
                         END-PERFORM
                         IF  POS NOT = 0
                             IF  MNIVEL = 0
                                 MOVE NIVEL TO MNIVEL
                             END-IF
                             MOVE ZERO   TO Z N
                             MOVE SPACES TO NUMERICO
                             MOVE "+"    TO SINAL
                         END-IF
                     END-IF
                     MOVE 0 TO TAG-STATUS
                    WHEN TAG-STATUS = 1
                         MOVE SPACE TO FECHA
                         IF TS > 0
                            MOVE TAG(TS:1) TO FECHA
                         END-IF
                         IF TS < LENGTH OF TAG
                            ADD 1 TO TS
                         END-IF
                         IF (TS = 1
                         AND BINARIO-CARACTER = '/') OR FECHA = '/'
                             IF NIVEL = MNIVEL
                             AND MNIVEL > 0
                             AND XML-TABLE = 0
                                 PERFORM 400-GRAVAR THRU 400-99-FIM
                             END-IF
                             MOVE 0 TO POS FLAG-TABLE
                             IF (CWLOAD-TABLE NOT = SPACES)
                             AND BINARIO-CARACTER = '/'
                                 MOVE 0 TO TS
                                 MOVE 1 TO FLAG-TABLE
                                 MOVE SPACES TO TAG
                             END-IF
                             PERFORM UNTIL BINARIO-CARACTER = '>'
                                        OR FS-BINARIO > '09'
                                     READ BINARIO
                                     IF FLAG-TABLE = 1
                                     AND FS-BINARIO < '10'
                                     AND (BINARIO-CARACTER NOT = '>')
                                         ADD 1 TO TS
                                         MOVE BINARIO-CARACTER TO
                                              TAG (TS:1)
                                     END-IF
                             END-PERFORM
                             IF FLAG-TABLE = 1
                                INSPECT TS (1: 51)
                                CONVERTING ACENTOS-WINDOWS
                                        TO ACENTOS-OFF
                                INSPECT TS (1: 51) CONVERTING MINUSCULAS
                                                           TO MAIUSCULAS
                                MOVE 0 TO FLAG-TABLE
                                IF TS (1: 51) = CWLOAD-TABLE
                                   MOVE 1 TO XML-TABLE
                                END-IF
                             END-IF
                             MOVE 0 TO TAG-STATUS
                             SUBTRACT 2 FROM NIVEL
                         ELSE
                             MOVE BINARIO-CARACTER TO TAG (TS:1)
                         END-IF
                    WHEN POS > 0
                         PERFORM 200-GET-BYTE THRU 200-99-FIM
                 END-EVALUATE
              END-IF
           END-PERFORM.

       020-99-FIM. EXIT.

       030-GET-RPX.

           PERFORM TEST AFTER UNTIL FS-BINARIO > '09'
              READ BINARIO
              IF FS-BINARIO < '10'
                 IF BINARIO-CARACTER = X'0D'
                    EXIT PERFORM CYCLE
                 END-IF
                 IF BINARIO-CARACTER = X'0A'
                    MOVE 0 TO X POS
                    EXIT PERFORM CYCLE
                 END-IF
                 IF BINARIO-CARACTER = X'B3'
                    IF  X > 0
                    AND CWLOAD-NUMERIC (X)
                        PERFORM 300-PUT-NUM THRU 300-99-FIM
                    END-IF
                    ADD 1 TO X
                    MOVE ZERO            TO Z
                                            N
                    MOVE SPACES          TO NUMERICO
                    MOVE "+"             TO SINAL
                    IF X > 1024
                    OR CWLOAD-DATANAME(X) = SPACES
                       PERFORM 400-GRAVAR THRU 400-99-FIM
                       MOVE 0 TO X
                    END-IF
                    MOVE OBJECTS-POS (X) TO POS
                    EXIT PERFORM CYCLE
                 END-IF
                 IF POS NOT = 0
                    PERFORM 200-GET-BYTE THRU 200-99-FIM
                 END-IF
              END-IF
           END-PERFORM.

       030-99-FIM. EXIT.

       040-GET-TXT.

           MOVE 1 TO X POS
           PERFORM TEST AFTER UNTIL FS-BINARIO > '09'
              READ BINARIO
              IF BINARIO-CARACTER = X'0D'
                 EXIT PERFORM CYCLE
              END-IF
              IF (BINARIO-CARACTER = CWLOAD-DELIMITER OR X'0A')
              OR FS-BINARIO > '09'
                 IF  X > 0
                 AND CWLOAD-NUMERIC (X)
                     PERFORM 300-PUT-NUM THRU 300-99-FIM
                 END-IF
                 IF BINARIO-CARACTER = X'0A'
                 OR FS-BINARIO > '09'
                    PERFORM 400-GRAVAR THRU 400-99-FIM
                    MOVE    0            TO X
                 END-IF
                 ADD 1 TO X
                 MOVE ZERO            TO Z
                                         N
                 MOVE SPACES          TO NUMERICO
                 MOVE "+"             TO SINAL
                 MOVE OBJECTS-POS (X) TO POS
                 EXIT PERFORM CYCLE
              END-IF
              PERFORM 200-GET-BYTE THRU 200-99-FIM
           END-PERFORM.

       040-99-FIM. EXIT.

       050-GET-SEQ.

           PERFORM VARYING X FROM 1 BY 1
                     UNTIL X > 1024
                        OR CWLOAD-DATANAME (X) = SPACES
                   MOVE 0 TO CWLOAD-DEC (X)
           END-PERFORM
           MOVE 1 TO X POS
           MOVE 0 TO S
           PERFORM TEST AFTER UNTIL FS-BINARIO > '09'
              READ BINARIO
              IF BINARIO-CARACTER = X'0D'
                 EXIT PERFORM CYCLE
              END-IF
              IF CWLOAD-LEN (X) NUMERIC
                 MOVE CWLOAD-LEN (X) TO LEN
              ELSE
                 MOVE CWLOAD-MORE255 (X) TO LEN
              END-IF
              IF BINARIO-CARACTER = X'0A'
              OR FS-BINARIO > '09'
              OR (CWLOAD-SIGNAL (X) AND S > LEN)
              OR ((NOT CWLOAD-SIGNAL (X)) AND S = LEN)
                 IF  X > 0
                 AND CWLOAD-NUMERIC (X)
                     PERFORM 300-PUT-NUM THRU 300-99-FIM
      *              MOVE        LEN      TO L
      *              MOVE OBJECTS-POS (X) TO POS
      *              MOVE NUMERICO (1:L) TO NUMERO(1:L)
      *              IF   CWLOAD-SIGNAL (X)
      *                   IF   SINAL = '-'
      *                        COMPUTE DIGITO (L) = DIGITO (L) * -1
      *                   ELSE
      *                        ADD 0 TO DIGITO (L)
      *                   END-IF
      *              END-IF
      *              MOVE NUMERO(1:L) TO FILES-RECORD(POS:L)
                 END-IF
                 IF BINARIO-CARACTER = X'0A'
                 OR FS-BINARIO > '09'
                    PERFORM 400-GRAVAR THRU 400-99-FIM
                    MOVE    0            TO X
                 END-IF
                 ADD 1 TO X
                 MOVE ZERO            TO Z
                                         N
                                         S
                 MOVE SPACES          TO NUMERICO
                 MOVE "+"             TO SINAL
                 MOVE OBJECTS-POS (X) TO POS
                 IF BINARIO-CARACTER = X'0A'
                 OR FS-BINARIO > '09'
                    EXIT PERFORM CYCLE
                 END-IF
              END-IF
              PERFORM 200-GET-BYTE THRU 200-99-FIM
           END-PERFORM.

       050-99-FIM. EXIT.

       060-OPEN-UFT8.

           ON 1
              OPEN I-O UTF8
              PERFORM VARYING X FROM 1 BY 1 UNTIL X > 255
                  IF xml-string (X) NOT = SPACE
                     COMPUTE BINARIO-ASCII = X - 1
                     MOVE BINARIO-CARACTER TO UTF8-CHAR
                     IF xml-string (X) (2: 1) NOT = SPACE
                        MOVE xml-string (X) TO UTF8-KEY
                        WRITE UTF8-REG
                     END-IF
                  END-IF
              END-PERFORM.

       060-99-FIM. EXIT.

       120-GET-JSON.

           PERFORM 060-OPEN-UFT8 THRU 060-99-FIM.

           MOVE 0 TO X ACHOU
           IF CWLOAD-TABLE NOT = SPACES
              INSPECT CWLOAD-TABLE CONVERTING MINUSCULAS TO MAIUSCULAS
              MOVE 1 TO XML-TABLE
           END-IF
      * Localiza a tabela
           PERFORM UNTIL FS-BINARIO > '09'
              READ BINARIO
              IF FS-BINARIO < '10'
                 EVALUATE TRUE
                    WHEN (BINARIO-CARACTER = X'0D' OR X'0A')
                      OR (BINARIO-CARACTER = SPACES
                      AND POS = 0)
                         CONTINUE
                    WHEN BINARIO-CARACTER = '{'
                     AND FLAG-TABLE = 0
                         MOVE 1 TO FLAG-TABLE
                    WHEN FLAG-TABLE = 1
                     AND BINARIO-CARACTER = '"'
                         MOVE 2 TO FLAG-TABLE
                    WHEN FLAG-TABLE = 2
                     AND BINARIO-CARACTER = '"'
                         MOVE 3 TO FLAG-TABLE
                         MOVE 0 TO POS
                    WHEN FLAG-TABLE = 3
                     AND BINARIO-CARACTER = ':'
                         MOVE 4 TO FLAG-TABLE
                    WHEN FLAG-TABLE = 4
                     AND BINARIO-CARACTER = '['
                         MOVE 5 TO FLAG-TABLE
                         INSPECT TAG CONVERTING MINUSCULAS TO MAIUSCULAS
                         IF (TAG NOT = CWLOAD-TABLE)
                         AND XML-TABLE = 1
                             MOVE 0 TO ASPA FLAG-TABLE
                             PERFORM UNTIL BINARIO-CARACTER = ']'
                                READ BINARIO
                                IF FS-BINARIO > '09'
                                   EXIT PERFORM
                                END-IF
                                IF  BINARIO-CARACTER = '"'
                                    IF  ASPA = 0
                                        MOVE 1 TO ASPA
                                    ELSE
                                        MOVE 0 TO ASPA
                                    END-IF
                                ELSE
                                    IF   BINARIO-CARACTER = ']'
                                    AND  ASPA = 1
                                         MOVE SPACE TO BINARIO-CARACTER
                                    END-IF
                                END-IF
                             END-PERFORM
                         ELSE
                             MOVE 1 TO ACHOU
                         END-IF
                         MOVE SPACES TO TAG
                         MOVE ZERO   TO FLAG-TABLE POS
                         EXIT PERFORM
                    WHEN FLAG-TABLE = 2
                         ADD 1 TO POS
                         MOVE BINARIO-CARACTER TO TAG(POS: 1)
                 END-EVALUATE
              END-IF
           END-PERFORM
      * Processa a tabela
           IF  ACHOU = 0
               MOVE '35' TO FS-BINARIO
           END-IF
           PERFORM UNTIL FS-BINARIO > '09'
              READ BINARIO
              IF FS-BINARIO < '10'
                 EVALUATE TRUE
                    WHEN (BINARIO-CARACTER = X'0D' OR X'0A')
                      OR (BINARIO-CARACTER = SPACES
                      AND POS = 0)
                         CONTINUE
                    WHEN BINARIO-CARACTER = '{'
                     AND FLAG-TABLE = 0
                         MOVE 1 TO FLAG-TABLE
                    WHEN FLAG-TABLE = 1
                     AND BINARIO-CARACTER = '"'
                         MOVE 2 TO FLAG-TABLE
                    WHEN FLAG-TABLE = 2
                     AND BINARIO-CARACTER = '"'
                         MOVE 3   TO FLAG-TABLE
                         MOVE 0   TO POS
                    WHEN FLAG-TABLE = 3
                     AND BINARIO-CARACTER = ':'
                         MOVE 4 TO FLAG-TABLE
                         INSPECT TAG
                                 CONVERTING MINUSCULAS TO MAIUSCULAS
                         INSPECT TAG
                                 CONVERTING '-'        TO '_'
                         PERFORM VARYING X FROM 1 BY 1
                                 UNTIL TAG = CWLOAD-DATANAME (X)
                                   OR CWLOAD-DATANAME (X) = SPACES
                                 CONTINUE
                         END-PERFORM
                         IF  CWLOAD-DATANAME (X) = SPACES
                             MOVE 0      TO POS
                             MOVE 1      TO FLAG-TABLE
                             MOVE SPACES TO TAG
                         ELSE
                         MOVE OBJECTS-POS(X) TO POS
                         MOVE ZERO           TO Z N
                                                POS-NUM
                         MOVE SPACES         TO NUMERICO
                                                TESTE-NUM
                         MOVE "+"            TO SINAL
                         MOVE 0 TO ASPA
                         PERFORM UNTIL FS-BINARIO > '09'
                            READ BINARIO
                                 EVALUATE TRUE
                                     WHEN (BINARIO-CARACTER = X'0D'
                                                           OR X'0A')
                                          EXIT PERFORM CYCLE
                                     WHEN BINARIO-CARACTER NOT = SPACE
                                      AND BINARIO-CARACTER NOT = '"'
                                          PERFORM 200-GET-BYTE
                                             THRU 200-99-FIM
                                          IF POS < 30
                                             MOVE BINARIO-CARACTER
                                              TO TESTE-NUM(POS:1)
                                          END-IF
                                          EXIT PERFORM
                                    WHEN BINARIO-CARACTER = '"'
                                         MOVE 1 TO ASPA
                                 END-EVALUATE
                         END-PERFORM
                         PERFORM UNTIL FS-BINARIO > '09'
                            READ BINARIO
                                 EVALUATE TRUE
                                     WHEN (BINARIO-CARACTER = X'0D'
                                                           OR X'0A')
                                          EXIT PERFORM CYCLE
                                     WHEN BINARIO-CARACTER = '"'
                                      AND ASPA = 1
                                          MOVE 0 TO ASPA
                                     WHEN ASPA = 1
                                          PERFORM 200-GET-BYTE
                                             THRU 200-99-FIM
                                          IF POS < 30
                                             MOVE BINARIO-CARACTER
                                              TO TESTE-NUM(POS:1)
                                          END-IF
                                     WHEN BINARIO-CARACTER = ']'
                                          MOVE '10' TO FS-BINARIO
                                     WHEN BINARIO-CARACTER = ',' OR '}'
                                          IF  CWLOAD-NUMERIC (X)
                                              PERFORM 300-PUT-NUM
                                                 THRU 300-99-FIM
                                          END-IF
                                          INSPECT TESTE-NUM
                                           CONVERTING MINUSCULAS
                                                   TO MAIUSCULAS
                                          IF  TESTE-NUM = 'NULL'
                                              MOVE LOW-VALUES
                                                TO FILES-RECORD(POS:L)
                                          END-IF
                                          IF  TESTE-NUM = 'FALSE'
                                              MOVE '0'
                                                TO FILES-RECORD(POS:L)
                                          END-IF
                                          IF  TESTE-NUM = 'TRUE'
                                              MOVE '1'
                                                TO FILES-RECORD(POS:L)
                                          END-IF
                                          IF  BINARIO-CARACTER = '}'
                                              PERFORM 400-GRAVAR
                                                 THRU 400-99-FIM
                                              MOVE 0 TO FLAG-TABLE
                                          ELSE
                                              MOVE 1 TO FLAG-TABLE
                                          END-IF
                                          MOVE SPACES TO TAG
                                          MOVE 0      TO POS
                                          EXIT PERFORM
                                     WHEN BINARIO-CARACTER NOT = ' '
                                          PERFORM 200-GET-BYTE
                                             THRU 200-99-FIM
                                 END-EVALUATE
                         END-PERFORM
                         END-IF
                    WHEN FLAG-TABLE = 2
                         ADD 1 TO POS
                         MOVE BINARIO-CARACTER TO TAG(POS: 1)
                 END-EVALUATE
              END-IF
           END-PERFORM.

       120-99-FIM. EXIT.

       200-GET-BYTE.

           IF   FileTypeJSON
           AND (POS-NUM < LENGTH TESTE-NUM)
                ADD  1                TO POS-NUM
                MOVE BINARIO-CARACTER TO TESTE-NUM(POS-NUM:1)
           END-IF

           IF   NOT CWLOAD-NUMERIC (X)
                IF   FileTypeUTF8
                     PERFORM 210-CHECK-CODE THRU 210-99-FIM
                END-IF
                MOVE BINARIO-CARACTER TO FILES-RECORD(POS:1)
                ADD  1                TO POS S
           ELSE
                IF   BINARIO-CARACTER NUMERIC
                     ADD  1                TO N S
                     MOVE BINARIO-CARACTER TO NUMERICO (N:1)
                ELSE
                     ADD  1  TO S
                     IF   BINARIO-CARACTER = "-"
                          MOVE "-" TO SINAL
                     END-IF
                     IF   BINARIO-CARACTER = "." OR ','
                     AND  N < 20
                          MOVE 20 TO N
                     END-IF
                END-IF
           END-IF.

       200-99-FIM. EXIT.

       210-CHECK-CODE.

           MOVE 1      TO N
           MOVE SPACES TO PARTE-CONV
           MOVE '00'   TO FS-UTF8
           PERFORM UNTIL FS-UTF8 > '09'
                   MOVE BINARIO-CARACTER TO PARTE-CONV (N:1)
                   MOVE PARTE-CONV    TO UTF8-KEY
                   START UTF8 KEY NOT LESS UTF8-KEY
                      NOT INVALID KEY
                          READ UTF8 NEXT RECORD
                          IF FS-UTF8 = '00'
                             IF PARTE-CONV = UTF8-KEY
                                MOVE UTF8-CHAR
                                  TO BINARIO-CARACTER
                                MOVE '23' TO FS-UTF8
                             ELSE
                                IF PARTE-CONV (1: N)
                                 = UTF8-KEY (1: N)
                                 ADD 1 TO N
                                 READ BINARIO INTO
                                  PARTE-CONV (N:1)
                                ELSE
                                  MOVE '99' TO FS-UTF8
                                END-IF
                             END-IF
                          END-IF
                   END-START
           END-PERFORM.

       210-99-FIM. EXIT.

       300-PUT-NUM.

           MOVE 0 TO NUMERO

           IF NUMERICO(1:20) NOT = SPACES
           AND CWLOAD-LEN (X) > 0
               MOVE CWLOAD-LEN (X) TO L
               SUBTRACT CWLOAD-DEC (X) FROM L
               PERFORM VARYING N FROM 20 BY -1
                              UNTIL L = 0
                              OR N = 0
                       IF NUMERICO (N:1) NUMERIC
                          MOVE NUMERICO (N:1) TO NUMERO(L:1)
                          SUBTRACT 1 FROM L
                       END-IF
               END-PERFORM
           END-IF

           IF NUMERICO(21:) NOT = SPACES
           AND CWLOAD-DEC (X) > 0
               COMPUTE L = CWLOAD-LEN (X) - CWLOAD-DEC (X) + 1
               PERFORM VARYING N FROM 21 BY 1
                              UNTIL L > LENGTH OF NUMERO
                              OR N > 40
                       IF NUMERICO (N:1) NUMERIC
                          MOVE NUMERICO (N:1) TO NUMERO(L:1)
                          ADD 1 TO L
                       END-IF
               END-PERFORM
           END-IF

           MOVE CWLOAD-LEN (X) TO L

           IF   CWLOAD-SIGNAL (X)
                IF   SINAL = '-'
                     MOVE DIGITO (L)       TO NS
                     ADD  1                TO NS
                     MOVE NEGATIVOS (NS:1) TO DIGITO(L) (1:1)
                END-IF
           END-IF

           MOVE NUMERO(1:L) TO FILES-RECORD(POS:L).

       300-99-FIM. EXIT.

       400-GRAVAR.

           CALL "CWATCH"
           IF FILES-REG NOT = SPACES
              ADD   1           TO OBJECTS-REC
              MOVE  OBJECTS-ID  TO FILES-ID
              MOVE  OBJECTS-REC TO FILES-SEQ
              WRITE FILES-REG
           END-IF

           MOVE  SPACES      TO FILES-REG
           MOVE  0           TO POS.

       400-99-FIM. EXIT.

       END PROGRAM CWLOAD.
