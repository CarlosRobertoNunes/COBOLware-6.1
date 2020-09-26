       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWJSON.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  08/04/2019.
       SECURITY.      *************************************************
                      *                                               *
                      * Leitura e Grava��o de documentos JSON         *
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
                  FILE STATUS   IS CWXML-STATUS.

           SELECT DUMP   ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-DUMP.

      $Set DataCompress"1" KeyCompress"7"
           SELECT TAGS   ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS TAGS-CHAVE
                  ALTERNATE KEY IS TAGS-STRING
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-TAGS
                  RESERVE NO ALTERNATE AREA.
      $Set NoDataCompress NoKeyCompress

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"

           SELECT JSONCONV ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS JSONCONV-KEY
                  LOCK MODE     IS EXCLUSIVE
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-JSONCONV.

       DATA DIVISION.
       FILE SECTION.

       FD  BINARIO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS CWXML-FILE.

       01  BINARIO-REG.
           05 BINARIO-CARACTER PIC X.
              88 fecha-tag value '}' ','.
           05 BINARIO-ASCII REDEFINES BINARIO-CARACTER
                              PIC 99 COMP-X.
       FD  DUMP
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-DUMP.

       01  DUMP-REG.
           05 DUMP-CHAVE               PIC  Z(008).
           05 DUMP-DADOS.
              10 DUMP-ESTILO           PIC  X(001).
              10 DUMP-SINAL            PIC  X(001).
              10 DUMP-TIPO             PIC  X(001).
              10 DUMP-TAMANHO          PIC  Z(005).
              10 DUMP-DECIMAIS         PIC  Z(002).
              10 DUMP-OCORRENCIAS      PIC  Z(006).
              10 DUMP-OFFSET           PIC  Z(008).
              10 DUMP-LENGTH           PIC  Z(008).
              10 DUMP-POSICAO          PIC  Z(008).
              10 DUMP-DUPLICAR         PIC  Z(008).
              10 DUMP-TAG              PIC  X(030).
              10 DUMP-NIVEL            PIC  Z(002)B.
           05 DUMP-STRING              PIC  X(255).

       FD  TAGS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-TAGS.

       01  TAGS-REG.
           05 TAGS-CHAVE        COMP-X PIC  9(008).
           05 TAGS-STRING              PIC  X(255).
           05 TAGS-DADOS.
              10 TAGS-ESTILO           PIC  X(001).
              10 TAGS-SINAL            PIC  X(001).
              10 TAGS-TIPO             PIC  X(001).
              10 TAGS-TAMANHO          PIC  9(005).
              10 TAGS-DECIMAIS         PIC  9(002).
              10 TAGS-OCORRENCIAS      PIC  9(006).
              10 TAGS-NIVEL            PIC  9(002).
              10 TAGS-TAG              PIC  X(030).
              10 TAGS-OFFSET    COMP-X PIC  9(008).
              10 TAGS-LENGTH    COMP-X PIC  9(008).
              10 TAGS-POSICAO   COMP-X PIC  9(008).
              10 TAGS-DUPLICAR  COMP-X PIC  9(008).

       FD  JSONCONV
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-JSONCONV.

       01  JSONCONV-REG.
           05 JSONCONV-KEY         PIC  X(005).
           05 JSONCONV-CHAR        PIC  X(001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 ESTILO-NIVEL            pic  9(002) value 0.
           05 BARRA                   pic  x(001) value '/'.
           05 CWmd5DIR                pic  x(255) value spaces.
           05 pasta                   pic  x(003) value 'md5'.
           05 SINAL-S                 PIC  X(001) VALUE SPACE.
           05 STRING-ANTERIOR         PIC  X(255) VALUE SPACES.
           05 FIM              COMP-X PIC  9(008) VALUE 0.
           05 BUFFER-FIELD            PIC X(5000) VALUE SPACES.
           05 BUFFER-OUTPUT           PIC X(65536) VALUE SPACES.
           05 TEST-BUFFER             PIC  X(100) VALUE SPACES.
           05 TAG-W                   PIC X(5000) VALUE SPACES.
           05 TAMANHO-A               PIC  9(005) VALUE 0.
           05 STRING-W                PIC  X(255) VALUE SPACES.
           05 SEQUENCIA        COMP-X PIC  9(008) VALUE 0.
           05 PONTO            COMP-X PIC  9(008) VALUE 0.
           05 D                COMP-X PIC  9(008) VALUE 0.
           05 I                COMP-X PIC  9(008) VALUE 0.
           05 Y                COMP-X PIC  9(008) VALUE 0.
           05 O                COMP-X PIC  9(008) VALUE 0.
           05 A                COMP-X PIC  9(008) VALUE 0.
           05 U                COMP-X PIC  9(008) VALUE 0.
           05 POSICAO          COMP-X PIC  9(008) VALUE 1.
           05 OFFSET           COMP-X PIC  9(008) VALUE 1.
           05 OCORRENCIA              PIC  ZZZZZ9.
           05 ER-DUMP.
              10 FS-DUMP              PIC  X(002) VALUE "00".
              10 LB-DUMP              PIC  X(255) VALUE SPACES.
           05 ER-TAGS.
              10 FS-TAGS              PIC  X(002) VALUE "00".
              10 LB-TAGS              PIC  X(255) VALUE SPACES.
           05 ER-JSONCONV.
              10 FS-JSONCONV          PIC  X(002) VALUE "00".
              10 LB-JSONCONV          PIC  X(255) VALUE "$TEMP/cwjson".
           05 FIELD.
              10 ESTILO               PIC  X(001) VALUE SPACE.
              10 SINAL                PIC  X(001) VALUE SPACE.
              10 TIPO                 PIC  X(001) VALUE SPACE.
              10 TAMANHO              PIC  9(005) VALUE 0.
              10 DECIMAIS             PIC  9(002) VALUE 0.
              10 OCORRENCIAS          PIC  9(006) VALUE 0.
              10 NIVEL                PIC  9(002) VALUE 0.
              10 TAG                  PIC  X(030) VALUE SPACES.
           05 PARTE-CONV              PIC  X(005) VALUE SPACES.
           05 X                COMP-X PIC  9(008) VALUE 0.
           05 NUMERO                  PIC  9(018) VALUE 0.
           05 REDEFINES NUMERO.
              10 DIGITO               PIC S9(001) OCCURS 18.
           05 TABELA-SINAIS.
              10 VALUE X'7D4A4B4C4D4E4F50515270717273747576777879'.
                 15 NEGATIVO-ASCII  OCCURS 10 PIC X(001).
                 15 NEGATIVO-EBCDIC OCCURS 10 PIC X(001).
              10 VALUE X'7B414243444546474849'.
                 15 POSITIVO-EBCDIC OCCURS 10 PIC X(001).
           05 REDEFINES TABELA-SINAIS.
              10 SINAL-CHAR OCCURS 30 PIC X.

       01  AREAS-DE-TRABALHO-2.
           05 teste-bolean            pic  x(006) value 'false'.
           05 SAVE-TAG                pic  x(255) value spaces.
           05 vazio-chave             pic  9(001) value 0.
           05 vazio-colchete          pic  9(001) value 0.
           05 TESTE-vazio-chave       pic  9(001) value 0.
           05 byte-anterior           PIC  X(001) VALUE SPACE.
           05 chave                   PIC  X(001) VALUE SPACE.
           05 colchete                PIC  X(001) VALUE SPACE.
           05 fechar-tags             PIC  9(003) VALUE 0.
           05 FECHADURA               pic  x(001) occurs 200.
           05 SKIP                    PIC  9(002) VALUE 0.
           05 TAG-FLAG                PIC  9(001) VALUE 0.
           05 virgula                 PIC  9(001) VALUE 0.
           05 FLAG-OC                 PIC  9(002) VALUE 0.
           05 NIVEL-GRUPO             PIC  9(002) VALUE 0.
           05 NIVEL-TESTE             PIC  9(002) VALUE 0.
           05 ARRAY                   PIC  9(002) VALUE 0.
           05 ELEMENTO                PIC  9(006) OCCURS 200.
           05 ARRAY-TAG               PIC  X(255) OCCURS 100.
           05 ARRAY-TAG-MINUS         PIC  X(255) OCCURS 100.
           05 FECHA                   PIC  X(001) OCCURS 100.
           05 ABERTOS                 PIC  9(003) VALUE 0.
           05 ABERTOS-NIVEL           PIC  9(004) OCCURS 1000.
           05 ASPA                    PIC  X(001) VALUE SPACE.
           05 ASPA-OPEN               PIC  9(001) VALUE 0.
              88 ASPA-OPENED VALUE 1.
              88 ASPA-CLOSED VALUE 0.
           05 ERRO                    PIC  X(002) VALUE SPACES.
           05 ESPACO                  PIC  9(001) VALUE 0.
           05 IGUAL                   PIC  9(001) VALUE 0.
           05 TAG-LEVEL               PIC  9(002) VALUE 0.
           05 TAG-LONG                PIC X(5000) VALUE SPACES.
           05 TP                      PIC  9(004) VALUE 0.
           05 POS              COMP-X PIC  9(008) VALUE 0.
           05 D-NIVEL                 PIC  9(002) VALUE 0.
           05 D-NIVEL-1               PIC  9(002) VALUE 0.
           05 D-NIVEL-2               PIC  9(002) VALUE 0.
           05 NIVEIS.
              10 N                    PIC  9(002).
              10 OCCURS 99.
                 15 NIVEL-OC          PIC  9(002).
                 15 NIVEL-SEQ COMP-X  PIC  9(008).

       01   JSON-TAB.
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
       01   redefines JSON-TAB.
            03 xml-string pic x(5) occurs 256.

       COPY CWUNIX.
       COPY CWMD5.

       LINKAGE SECTION.

       COPY CWXML.
       01  BUFFER        PIC X.
       01  LAYOUT        PIC X.
       01  CWJSON-status PIC X(02).
           88 json-FIM value '10' thru x'FFFF'.
           88 json-OK  value '00' thru '09'.

       PROCEDURE DIVISION USING PARAMETROS-CWXML.

       000-INICIO.

           ON 1
              OPEN I-O JSONCONV
              PERFORM VARYING X FROM 1 BY 1 UNTIL X > 255
                  IF xml-string (X) NOT = SPACE
                     COMPUTE BINARIO-ASCII = X - 1
                     MOVE BINARIO-CARACTER TO JSONCONV-CHAR
                     IF xml-string (X) (2: 1) NOT = SPACE
                        MOVE xml-string (X) TO JSONCONV-KEY
                        WRITE JSONCONV-REG
                     END-IF
                  END-IF
              END-PERFORM
              CALL "CWUNIX" USING PARAMETROS-CWUNIX
              IF   CWUNIX-OFF
                   MOVE '\' TO BARRA
              END-IF
              DISPLAY 'CWMD5DIR' UPON ENVIRONMENT-NAME
              ACCEPT   CWmd5DIR  FROM ENVIRONMENT-VALUE
              IF CWmd5DIR = SPACES
                 DISPLAY 'CWXMLDIR' UPON ENVIRONMENT-NAME
                 ACCEPT   CWmd5DIR  FROM ENVIRONMENT-VALUE
                 IF       CWmd5DIR  NOT = SPACES
                    move 'xml' to pasta
                 END-IF
              else
                 move 'md5' to pasta
              END-IF
              IF   CWmd5DIR = SPACES
                   DISPLAY 'COBOLWARE' UPON ENVIRONMENT-NAME
                   ACCEPT   LB-TAGS    FROM ENVIRONMENT-VALUE
                   STRING LB-TAGS  DELIMITED BY SPACE
                          BARRA    DELIMITED BY SIZE
                          pasta    DELIMITED BY SIZE
                     INTO CWmd5DIR
              END-IF
              CALL "CBL_CREATE_DIR" USING CWMD5DIR.

           SET ADDRESS OF  CWJSON-status TO ADDRESS OF CWXML-STATUS
           IF CWXML-PUT
              OPEN OUTPUT BINARIO
           ELSE
              OPEN INPUT BINARIO
           END-IF

           IF JSON-FIM
              GOBACK
           END-IF

           INITIALIZE TAGS-REG NIVEIS AREAS-DE-TRABALHO-2 SEQUENCIA
           SET ADDRESS OF BUFFER TO CWXML-BUFFER
           SET ADDRESS OF LAYOUT TO CWXML-LAYOUT
           SET CWMD5-FIELD TO CWXML-LAYOUT
           MOVE CWXML-LAYOUT-LENGTH
             TO CWMD5-FIELD-LENGTH
           CALL 'CWMD5' USING PARAMETROS-CWMD5
           MOVE SPACES TO LB-TAGS
           STRING CWmd5DIR   DELIMITED BY SPACE
                  BARRA 'cw' pasta '-' CWMD5-HASH DELIMITED BY SIZE
             INTO LB-TAGS
           PERFORM TEST AFTER UNTIL FS-TAGS NOT = '9A'
                   OPEN INPUT TAGS
                   IF FS-TAGS = '9A'
                      CALL 'CWISAM' USING ER-TAGS
                   END-IF
           END-PERFORM
           IF FS-TAGS NOT = '00'
              DELETE FILE TAGS
              EXEC COBOLware BoxWindow (OPEN)
                   LINE   10
                   COLUMN 04
                   WIDTH 71
                   HEIGHT 3
                   COLOR-FRAME 127
                   COLOR-BORDER 127
              END-EXEC
              DISPLAY 'Primeiro uso do layout, Gerando tags...'
                   AT 1106 WITH BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 1
              DISPLAY LB-TAGS
                   AT 1206  WITH SIZE 70 BACKGROUND-COLOR 7
                                         FOREGROUND-COLOR 1
              OPEN I-O TAGS WITH LOCK
              MOVE 1 TO POSICAO OFFSET
              COMPUTE FIM = CWXML-LAYOUT-LENGTH + 1
              PERFORM UNTIL POSICAO = FIM
                        AND (TAGS-NIVEL NOT = 99)
                      IF   TAGS-NIVEL = 99
                           MOVE 0 TO NIVEL
                      ELSE
                           MOVE LAYOUT (POSICAO: 50) TO FIELD
                           MOVE 0                    TO OCORRENCIA
                      END-IF
                      IF  (NIVEL NOT > TAGS-NIVEL)
                      AND (N NOT = 0)
                      AND (NIVEL NOT > NIVEL-OC (N))
                          MOVE NIVEL-SEQ (N) TO TAGS-CHAVE
                          READ TAGS
                          MOVE TAGS-POSICAO TO POSICAO
                          MOVE LAYOUT (POSICAO: 50) TO FIELD
                          COMPUTE OCORRENCIA = OCORRENCIAS
                                             - TAGS-DUPLICAR
                                             + 1
                          SUBTRACT 1 FROM TAGS-DUPLICAR
                          IF  TAGS-DUPLICAR = 0
                              MOVE 0 TO NIVEL-OC (N)
                              MOVE 0 TO NIVEL-SEQ (N)
                              SUBTRACT 1 FROM N
                          END-IF
                          REWRITE TAGS-REG
                          MOVE 1 TO OCORRENCIAS
                      END-IF
                      IF   NIVEL < TAGS-NIVEL
                           MOVE LENGTH TAGS-STRING TO I
                           PERFORM UNTIL NIVEL NOT < TAGS-NIVEL
                                   PERFORM VARYING I
                                              FROM I
                                                BY -1
                                           UNTIL I = 1
                                              OR TAGS-STRING(I: 1) = '/'
                                       CONTINUE
                                   END-PERFORM
                                   MOVE SPACES TO TAGS-STRING (I:)
                                   READ TAGS KEY IS TAGS-STRING
                                   IF FS-TAGS NOT = '00'
                                      EXIT PERFORM
                                   END-IF
                           END-PERFORM
                      END-IF
                      ADD  1            TO SEQUENCIA
                      MOVE SEQUENCIA    TO TAGS-CHAVE
                      MOVE ESTILO       TO TAGS-ESTILO
                      MOVE SINAL        TO TAGS-SINAL
                      MOVE TIPO         TO TAGS-TIPO
                      MOVE TAMANHO      TO TAGS-TAMANHO
                      MOVE DECIMAIS     TO TAGS-DECIMAIS
                      MOVE OCORRENCIAS  TO TAGS-OCORRENCIAS
                      IF   OCORRENCIAS  > 1
                           ADD  1         TO N
                           MOVE NIVEL     TO NIVEL-OC (N)
                           MOVE SEQUENCIA TO NIVEL-SEQ (N)
                           MOVE 1         TO OCORRENCIA
                      END-IF
                      MOVE TAG          TO TAGS-TAG
                      MOVE OFFSET       TO TAGS-OFFSET
                      COMPUTE TAMANHO-A = TAMANHO + DECIMAIS
                      ADD  TAMANHO-A    TO OFFSET
                      MOVE TAMANHO-A    TO TAGS-LENGTH
                      IF TAMANHO-A = 0
                         MOVE 'G'       TO TAGS-TIPO
                      END-IF
                      MOVE POSICAO      TO TAGS-POSICAO
                      COMPUTE TAGS-DUPLICAR = TAGS-OCORRENCIAS - 1
                      IF  OCORRENCIA = '     0'
                          MOVE TAG TO TAG-W
                      ELSE
                          PERFORM VARYING I FROM 1 BY 1
                                  UNTIL OCORRENCIA (I: 1) NOT = SPACE
                                  CONTINUE
                          END-PERFORM
                          MOVE SPACES TO TAG-W
                          STRING TAG DELIMITED BY SPACE
                                 '('
                                 OCORRENCIA (I: )
                                 ')' DELIMITED BY SIZE
                                 INTO TAG-W
                      END-IF
                      EVALUATE TRUE
                          WHEN TAGS-NIVEL = 0
                               MOVE TAG-W TO TAGS-STRING
                          WHEN NIVEL > TAGS-NIVEL
                               MOVE TAGS-STRING TO STRING-W
                               MOVE SPACES      TO TAGS-STRING
                               STRING STRING-W DELIMITED BY SPACE
                                      '/' TAG-W  DELIMITED BY SIZE
                                 INTO TAGS-STRING
                          WHEN NIVEL = TAGS-NIVEL
                               PERFORM VARYING I FROM LENGTH TAGS-STRING
                                                   BY -1
                                         UNTIL I = 0
                                            OR TAGS-STRING (I: 1) = '/'
                                       CONTINUE
                               END-PERFORM
                               ADD 1 TO I
                               MOVE TAG-W TO TAGS-STRING (I: )
                      END-EVALUATE
                      MOVE NIVEL      TO TAGS-NIVEL
                      MOVE OCORRENCIA TO TAGS-OCORRENCIAS
                      WRITE TAGS-REG
                      DISPLAY (13, 6) TAGS-STRING
                         WITH SIZE 70 BACKGROUND-COLOR 7
                                      FOREGROUND-COLOR 1
                      IF  FS-TAGS NOT = '00'
                          EXEC COBOLware Send
                               Message TAGS-STRING
                          END-EXEC
                      END-IF
                      MOVE LENGTH TAGS-STRING TO I
                      PERFORM UNTIL FS-TAGS NOT = '00'
                              PERFORM VARYING I FROM I
                                           BY -1
                                      UNTIL I = 1
                                         OR TAGS-STRING (I: 1) = '/'
                                      CONTINUE
                              END-PERFORM
                              MOVE SPACES TO TAGS-STRING (I:)
                              READ TAGS KEY IS TAGS-STRING
                              IF FS-TAGS NOT = '00'
                                 MOVE SEQUENCIA TO TAGS-CHAVE
                                 READ TAGS
                                 MOVE '23' TO FS-TAGS
                              ELSE
                                 ADD TAMANHO-A TO TAGS-LENGTH
                                 REWRITE TAGS-REG
                              END-IF
                      END-PERFORM
                      ADD  50  TO  POSICAO
                      IF  POSICAO = FIM
                      AND (N NOT = 0)
                          MOVE 99 TO TAGS-NIVEL
                      END-IF
              END-PERFORM
              EXEC COBOLware BOXW (CLOSE) END-EXEC
              CLOSE TAGS
              OPEN INPUT TAGS
           END-IF
           DISPLAY 'CWJSON' UPON ENVIRONMENT-NAME
           ACCEPT LB-DUMP FROM ENVIRONMENT-VALUE
           IF LB-DUMP NOT = SPACES
              MOVE 1 TO TAGS-CHAVE
              START TAGS KEY NOT LESS TAGS-CHAVE
              OPEN OUTPUT DUMP
              PERFORM UNTIL FS-TAGS > '00'
                      READ TAGS NEXT RECORD
                      NOT AT END
                          MOVE TAGS-CHAVE        TO DUMP-CHAVE
                          MOVE TAGS-ESTILO       TO DUMP-ESTILO
                          MOVE TAGS-SINAL        TO DUMP-SINAL
                          MOVE TAGS-TIPO         TO DUMP-TIPO
                          MOVE TAGS-TAMANHO      TO DUMP-TAMANHO
                          MOVE TAGS-DECIMAIS     TO DUMP-DECIMAIS
                          MOVE TAGS-OCORRENCIAS  TO DUMP-OCORRENCIAS
                          MOVE TAGS-NIVEL        TO DUMP-NIVEL
                          MOVE TAGS-TAG          TO DUMP-TAG
                          MOVE TAGS-OFFSET       TO DUMP-OFFSET
                          MOVE TAGS-LENGTH       TO DUMP-LENGTH
                          MOVE TAGS-POSICAO      TO DUMP-POSICAO
                          MOVE TAGS-DUPLICAR     TO DUMP-DUPLICAR
                          MOVE TAGS-STRING       TO DUMP-STRING
                          WRITE DUMP-REG
                      END-READ
              END-PERFORM
              CLOSE DUMP
           END-IF.

           IF CWXML-PUT
              PERFORM 060-PUT-JSON THRU 060-99-FIM
           ELSE
              PERFORM 020-GET-JSON  THRU 020-99-FIM
              perform UNTIL fechar-tags = 0
                      perform 025-TAG-CLOSE THRU 025-99-FIM
                      subtract 1 from fechar-tags
              end-perform
           END-IF

           CLOSE TAGS

           IF ERRO NOT = SPACES
              CLOSE BINARIO
              MOVE ERRO TO CWJSON-STATUS
           ELSE
              CLOSE BINARIO
           END-IF.

       000-99-FIM. GOBACK.

       020-GET-JSON.

           MOVE SPACES TO TAGS-STRING
                          BUFFER (1: CWXML-BUFFER-LENGTH)
           MOVE 0 TO NIVEL
      *    PERFORM UNTIL JSON-FIM
      *               OR (BINARIO-CARACTER = ':'
      *               AND ASPA-CLOSED)
      *            PERFORM 030-LER-BINARIO THRU 030-99-FIM
      *    END-PERFORM
      *    IF JSON-FIM
      *       MOVE X'390B' TO ERRO
      *       EXIT PARAGRAPH
      *    END-IF
           PERFORM UNTIL JSON-FIM
              PERFORM 030-LER-BINARIO THRU 030-99-FIM
              IF  JSON-OK
              AND(ASPA-CLOSED OR TAG-FLAG = 1)
                  EVALUATE TRUE
                      WHEN BINARIO-CARACTER = '{' OR ','
                        OR TAG-FLAG = 1
                           IF   BINARIO-CARACTER = '{'
                           AND  FECHAR-TAGS > 0
                           AND  FECHADURA(FECHAR-TAGS) = ','
                                MOVE SPACE TO FECHADURA(FECHAR-TAGS)
                                SUBTRACT 1 FROM FECHAR-TAGS
                                IF ARRAY > 0
                                   PERFORM 036-ARRAY-PLUS
                                      THRU 036-99-FIM
                                END-IF
                           END-IF
                           PERFORM UNTIL FECHAR-TAGS = 0
                                   PERFORM 025-TAG-CLOSE THRU 025-99-FIM
                                   MOVE SPACE TO FECHADURA(FECHAR-TAGS)
                                   SUBTRACT 1 FROM FECHAR-TAGS
                           END-PERFORM
                           PERFORM UNTIL JSON-FIM
                                      OR ASPA-OPENED
                                      OR TAG-FLAG = 1
220519                                OR VAZIO-chave = 1
                                   PERFORM 030-LER-BINARIO
                                      THRU 030-99-FIM
                           END-PERFORM
                           MOVE 0 TO TAG-FLAG
220519                     MOVE 0 TO TESTE-VAZIO-chave
220519                     IF VAZIO-chave = 1
220519                        PERFORM 025-TAG-CLOSE THRU 025-99-FIM
220519                        MOVE 0 TO VAZIO-chave
220519                        EXIT PERFORM CYCLE
220519                     END-IF
                           IF JSON-FIM
                              MOVE X'390B' TO ERRO
                              EXIT PERFORM
                           END-IF
                           PERFORM 022-MONTA-TAG THRU 022-99-FIM
                           IF BINARIO-CARACTER = ':'
                              MOVE 1 TO TESTE-VAZIO-chave
                              PERFORM 030-LER-BINARIO
                                 THRU 030-99-FIM
220519                        if  vazio-colchete = 1
220519                            PERFORM 025-TAG-CLOSE THRU 025-99-FIM
220519                            MOVE 0 TO VAZIO-colchete
220519                            MOVE 1 TO skip
220519                            EXIT PERFORM CYCLE
220519                        end-if
                              IF  BINARIO-CARACTER NOT = '{'
220519                        OR  ASPA-OPENED
                                  MOVE BINARIO-CARACTER TO BUFFER-FIELD
                                  MOVE 1                TO POS
                                  PERFORM UNTIL JSON-FIM
                                      PERFORM 030-LER-BINARIO
                                         THRU 030-99-FIM
                                      IF JSON-FIM
                                         MOVE X'390B' TO ERRO
                                         EXIT PERFORM
                                      END-IF
                                      IF  ASPA-CLOSED
                                      AND fecha-tag
                                          if fs-tags < '10'
                                             PERFORM 040-SET-DATA
                                                THRU 040-99-FIM
                                          end-if
                                          perform until not fecha-tag
                                                  or JSON-FIM
                                               add 1 to fechar-tags
                                               MOVE BINARIO-CARACTER
                                               TO FECHADURA(FECHAR-TAGS)
                                               PERFORM 030-LER-BINARIO
                                                  THRU 030-99-FIM
                                          end-perform
                                          move 1 to skip
                                          if  aspa-opened
                                              MOVE 1 TO TAG-FLAG
                                          end-if
                                          EXIT PERFORM
                                      ELSE
                                          if byte-anterior = '\'
                                             ADD  1 TO POS
                                             MOVE '\'
                                               TO BUFFER-FIELD(POS:1)
                                             move space to byte-anterior
                                          end-if
                                          ADD  1 TO POS
                                          MOVE BINARIO-CARACTER
                                            TO BUFFER-FIELD(POS:1)
                                      END-IF
                                  END-PERFORM
                              ELSE
                                  MOVE 1 TO SKIP
                              END-IF
                           END-IF
                      WHEN fecha-tag
                           PERFORM 025-TAG-CLOSE THRU 025-99-FIM
                  END-EVALUATE
              END-IF
           END-PERFORM.

       020-99-FIM. EXIT.

       022-MONTA-TAG.

           ADD  1 TO TAG-LEVEL
           MOVE 1 TO TP
           MOVE BINARIO-CARACTER TO TAG-LONG
           PERFORM UNTIL aspa-closed
                     and BINARIO-CARACTER = ':'
                   perform test after
                           until BINARIO-CARACTER <> space
                              or JSON-FIM
                          PERFORM 030-LER-BINARIO THRU 030-99-FIM
                   end-perform
                   IF JSON-FIM
                      MOVE X'3911' TO ERRO
                      EXIT PERFORM
                   END-IF
                   IF  (aspa-opened)
                   AND (TAG-LONG NOT = SPACES)
                       IF   TP NOT < LENGTH TAG-LONG
                            MOVE X'390B' TO ERRO
                            EXIT PERFORM
                       ELSE
                            ADD 1 TO TP
                            MOVE BINARIO-CARACTER
                              TO TAG-LONG (TP: 1)
                       END-IF
                   END-IF
           END-PERFORM
           IF TAG-LONG (TP:1) = '/'
              MOVE SPACE TO TAG-LONG (TP:1)
              SUBTRACT 1 FROM TP
           END-IF
           IF JSON-FIM
              EXIT paragraph
           ELSE
              MOVE SPACES TO TAG
              STRING TAG-LONG DELIMITED BY SPACE
                     INTO TAG
              PERFORM VARYING I
                         FROM 1
                           BY 1
                      UNTIL I > LENGTH TAGS-STRING
                         OR TAGS-STRING (I: 1) = SPACE
                  CONTINUE
              END-PERFORM
              IF I = 1
                 MOVE TAG TO TAGS-STRING
              ELSE
                 STRING '/' DELIMITED BY SIZE
                        TAG DELIMITED BY SPACE
                   INTO TAGS-STRING (I: )
              END-IF
              READ TAGS ignore lock KEY IS TAGS-STRING
              IF  FS-TAGS > "09"
              AND ARRAY > 0
                  MOVE TAGS-STRING TO SAVE-TAG
                  MOVE SPACES TO TAGS-STRING
                  STRING ARRAY-TAG (ARRAY) DELIMITED BY SPACE
                         '/' DELIMITED BY SIZE
                         TAG DELIMITED BY SPACE
                        INTO TAGS-STRING
                  READ TAGS ignore lock KEY IS TAGS-STRING
                  IF  FS-TAGS > "09"
                      move save-tag to tags-string
                  end-if
              end-if
           END-IF.

       022-99-FIM. EXIT.

       025-TAG-CLOSE.

           if tag-level = 1
              move spaces to tag tags-string
              exit paragraph
           end-if
           SUBTRACT 1 FROM TAG-LEVEL
           PERFORM VARYING I FROM LENGTH TAGS-STRING
                               BY -1
                         UNTIL I = 1
                            OR TAGS-STRING (I: 1) = '/'
                   CONTINUE
           END-PERFORM
           MOVE SPACES TO TAGS-STRING (I:)
           MOVE SPACES TO TAG
           IF TAG-LEVEL NOT = 0
              PERFORM VARYING I FROM LENGTH TAGS-STRING
                                  BY -1
                            UNTIL I = 1
                               OR TAGS-STRING (I: 1) = '/'
                      CONTINUE
              END-PERFORM
              IF I > 1
                 ADD 1 TO I
              END-IF
              MOVE TAGS-STRING (I:) TO TAG
              PERFORM VARYING I FROM 1 BY 1
                UNTIL I > LENGTH TAG
                   OR TAG (I: 1) = SPACE
                   IF TAG (I: 1) = '('
                      MOVE SPACE TO TAG (I:)
                   END-IF
              END-PERFORM
           END-IF

           IF  ARRAY NOT = ZERO
           AND TAGS-STRING = ARRAY-TAG-MINUS(ARRAY)
               MOVE ARRAY-TAG(ARRAY) TO TAGS-STRING
           END-IF
           READ TAGS ignore lock KEY IS TAGS-STRING.

       025-99-FIM. EXIT.

       030-LER-BINARIO.

           if skip = 1
           OR NOT JSON-OK
              move 0 to skip
              exit paragraph
           end-if
           READ BINARIO
           IF   JSON-OK
                IF  ASPA-OPENED
                AND BINARIO-CARACTER = '\'
                    MOVE '\' TO BYTE-ANTERIOR
                    GO TO 030-LER-BINARIO
                END-IF
                IF((BINARIO-CARACTER = X'0D' OR X'0A')
                OR (BINARIO-CARACTER = SPACE
                AND ASPA-CLOSED))
                   GO TO 030-LER-BINARIO
                END-IF
                IF   BINARIO-CARACTER = '"'
                     EVALUATE TRUE
                         WHEN BYTE-ANTERIOR = '\'
                          AND ASPA-OPENED
                              move space to byte-anterior
                              go to 030-99-FIM
                         WHEN ASPA-CLOSED
                              SET ASPA-OPENED TO TRUE
                         WHEN ASPA-OPENED
                              SET ASPA-CLOSED TO TRUE
                     END-EVALUATE
                     GO TO 030-LER-BINARIO
                ELSE
                     IF  ASPA-CLOSED
                         EVALUATE TRUE
                             WHEN BINARIO-CARACTER = '['
                                  ADD  1 TO ARRAY
                                  MOVE 1 TO ELEMENTO(ARRAY)
                                  PERFORM 035-ARRAY THRU 035-99-FIM
220519                            move '[' to colchete
                                  GO       TO 030-LER-BINARIO
                             WHEN BINARIO-CARACTER = ']'
                                  IF ARRAY NOT = 0
                                     MOVE 0 TO ELEMENTO(ARRAY)
                                     SUBTRACT 1 FROM ARRAY
                                  END-IF
220519                            if colchete = '['
220519                               move 1 to vazio-colchete
220519                               move space to colchete
220519                            end-if
220519                            GO TO 030-LER-BINARIO
220519                       when BINARIO-CARACTER = '}'
                              AND TESTE-VAZIO-chave = 1
220519                        and chave = '{'
220519                            move space to chave
220519                            MOVE 1 TO VAZIO-chave
220519                       when BINARIO-CARACTER = '{'
                              AND TESTE-VAZIO-chave = 1
220519                            move '{' to chave
220519                       when other
220519                            move space to colchete chave
                         END-EVALUATE
                     END-IF
                END-IF
           END-IF.

       030-99-FIM. EXIT.

       035-ARRAY.

           MOVE SPACES          TO ARRAY-TAG(ARRAY)
           MOVE ELEMENTO(ARRAY) TO OCORRENCIA
           PERFORM VARYING O FROM 1 BY 1
                   UNTIL OCORRENCIA (O: 1)
                     NOT = SPACE
                   CONTINUE
           END-PERFORM
           STRING TAGS-STRING DELIMITED BY SPACE
                  '('
                  OCORRENCIA (O: )
                  ')' DELIMITED BY SIZE
             INTO ARRAY-TAG(ARRAY).

       035-99-FIM. EXIT.

       036-ARRAY-PLUS.

           MOVE ARRAY-TAG(ARRAY) TO ARRAY-TAG-MINUS(ARRAY)
           ADD 1 TO ELEMENTO(ARRAY)
           PERFORM VARYING Y
                      FROM LENGTH TAGS-STRING
                        BY -1
                     UNTIL Y = 1
                          OR ARRAY-TAG(ARRAY)(Y:1) = "("
               CONTINUE
           END-PERFORM
           MOVE ELEMENTO(ARRAY) TO OCORRENCIA
           PERFORM VARYING O FROM 1 BY 1
                   UNTIL OCORRENCIA (O: 1)
                     NOT = SPACE
                   CONTINUE
           END-PERFORM
           STRING '('
                  OCORRENCIA (O: )
                  ')' DELIMITED BY SIZE
             INTO ARRAY-TAG(ARRAY)(Y:).

       036-99-FIM. EXIT.

       040-SET-DATA.

           MOVE BUFFER-FIELD TO TAG-W TEST-BUFFER
           MOVE SPACES       TO BUFFER-FIELD
           MOVE 0            TO A
           PERFORM VARYING I FROM LENGTH TAG-W BY -1
             UNTIL I = 0
                OR (TAG-W (I:1) NOT = SPACE)
                   CONTINUE
           END-PERFORM
           PERFORM VARYING U FROM 1 BY 1 UNTIL U > I
                   MOVE 1          TO N
                   ADD  1          TO A
                   MOVE TAG-W(U:1) TO BUFFER-FIELD(A:1)
                   MOVE SPACES     TO PARTE-CONV
                   MOVE '00'       TO FS-JSONCONV
                   PERFORM UNTIL FS-JSONCONV > '09'
                           MOVE TAG-W (U:1)   TO PARTE-CONV (N:1)
                           MOVE PARTE-CONV    TO JSONCONV-KEY
                           START JSONCONV KEY NOT LESS JSONCONV-KEY
                              NOT INVALID KEY
                                  READ JSONCONV NEXT RECORD
                                  IF FS-JSONCONV = '00'
                                     IF PARTE-CONV = JSONCONV-KEY
                                        MOVE JSONCONV-CHAR
                                          TO BUFFER-FIELD (A:1)
                                        MOVE '23' TO FS-JSONCONV
                                     ELSE
                                        IF PARTE-CONV (1: N)
                                         = JSONCONV-KEY (1: N)
                                         ADD 1 TO N U
                                         MOVE TAG-W (U:1)
                                          TO PARTE-CONV (N:1)
                                        ELSE
                                          MOVE '99' TO FS-JSONCONV
                                        END-IF
                                     END-IF
                                  END-IF
                           END-START
                   END-PERFORM
           END-PERFORM

           IF TAGS-TIPO = '9'
              MOVE 0     TO PONTO
              MOVE SPACE TO SINAL
              PERFORM VARYING I FROM 1 BY 1
                UNTIL I > LENGTH BUFFER-FIELD
                     OR BUFFER-FIELD (I:1) = SPACE
                     IF BUFFER-FIELD (I:1) = '.'
                        MOVE I TO PONTO
                     END-IF
                     IF BUFFER-FIELD (I:1) = '+' OR '-'
                        MOVE BUFFER-FIELD (I:1) TO SINAL
                     END-IF
              END-PERFORM
              SUBTRACT 1 FROM I
              MOVE 0  TO NUMERO
              MOVE 18 TO D
              COMPUTE N = 18 - TAGS-DECIMAIS
              IF PONTO NOT = 0
                 PERFORM VARYING D FROM PONTO BY 1
                                   UNTIL D > I
                                      OR N > 18
                         IF BUFFER-FIELD (D:1) NUMERIC
                         AND N < 18
                             ADD 1 TO N
                             MOVE BUFFER-FIELD (D:1) TO NUMERO (N:1)
                         END-IF
                 END-PERFORM
                 COMPUTE N = 18 - TAGS-DECIMAIS
                 MOVE N TO D
                 COMPUTE I = PONTO - 1
              ELSE
                 MOVE 18 TO D
                 SUBTRACT TAGS-DECIMAIS FROM D
              END-IF
              PERFORM UNTIL I = 0
                         OR D = 0
                      IF BUFFER-FIELD (I:1) NUMERIC
                         MOVE BUFFER-FIELD (I:1) TO NUMERO (D:1)
                         SUBTRACT 1 FROM D
                      END-IF
                      SUBTRACT 1 FROM I
              END-PERFORM
              COMPUTE I = 18 - TAGS-DECIMAIS - TAGS-TAMANHO + 1
              IF SINAL = '-'
                 COMPUTE D = DIGITO (TAGS-LENGTH) + 1
                 IF PARAMETROS-CWXML (1:1) = X"4A"
                    MOVE NEGATIVO-EBCDIC(D) TO NUMERO(TAGS-LENGTH:1)
                 ELSE
                    MOVE NEGATIVO-ASCII (D) TO NUMERO(TAGS-LENGTH:1)
                 END-IF
              ELSE
                  IF  TAGS-SINAL = 'S'
                  AND PARAMETROS-CWXML (1:1) = X"4A"
                      COMPUTE D = DIGITO (TAGS-LENGTH) + 1
                      MOVE POSITIVO-EBCDIC(D) TO NUMERO(TAGS-LENGTH:1)
                  END-IF
              END-IF
              MOVE NUMERO (I: )
                TO BUFFER (TAGS-OFFSET: TAGS-LENGTH)
           ELSE
              MOVE BUFFER-FIELD (1: TAGS-LENGTH)
                TO BUFFER (TAGS-OFFSET: TAGS-LENGTH)
          END-IF

          INSPECT TEST-BUFFER CONVERTING 'NULTREFAS' TO 'nultrefas'
          EVALUATE test-buffer
              WHEN 'null'
                   MOVE LOW-VALUES TO BUFFER (TAGS-OFFSET: TAGS-LENGTH)
              WHEN 'false'
                   IF TAGS-LENGTH = 1
                      MOVE '0' TO BUFFER (TAGS-OFFSET: TAGS-LENGTH)
                   END-IF
              WHEN 'true'
                   IF TAGS-LENGTH = 1
                      MOVE '1' TO BUFFER (TAGS-OFFSET: TAGS-LENGTH)
                   END-IF
          END-EVALUATE.

       040-99-FIM. EXIT.

       060-PUT-JSON.

           MOVE 0 TO TAGS-CHAVE
                     TAG-LEVEL
                     NIVEL

           MOVE SPACES TO BUFFER-OUTPUT
           MOVE '{' TO BUFFER-OUTPUT
           PERFORM 065-WRITE-BUFFER THRU 065-99-FIM
           PERFORM 070-PUT-LINE THRU 070-99-FIM
           MOVE 1 TO TAGS-CHAVE
           START TAGS KEY NOT LESS TAGS-CHAVE
           PERFORM UNTIL FS-TAGS > '09'
              READ TAGS NEXT RECORD ignore lock
              IF  FS-TAGS < '10'
240519            IF   BUFFER (TAGS-OFFSET: TAGS-LENGTH) = SPACES
240519            OR  (BUFFER (TAGS-OFFSET: TAGS-LENGTH) = ZEROS
240519            AND  TAGS-TIPO = '9')
240519                 EXIT PERFORM CYCLE
240519            END-IF
                  MOVE SPACES TO BUFFER-OUTPUT
                  MOVE 1      TO I
                  IF   TAGS-OCORRENCIAS > 1
                       MOVE 1 TO FLAG-OC
                       WRITE BINARIO-REG FROM '}'
                       IF  (ABERTOS NOT = ZERO)
                       AND (TAGS-NIVEL NOT > ABERTOS-NIVEL(ABERTOS))
                       AND  '[' = FECHA(ABERTOS)
                            WRITE BINARIO-REG FROM ','
                            move  1    to virgula
                            move space to binario-reg
                       END-IF
                       PERFORM 070-PUT-LINE THRU 070-99-FIM
                       EXIT PERFORM CYCLE
                  END-IF
                  IF   FLAG-OC = 1
                       MOVE '{' TO BUFFER-OUTPUT
                       MOVE 2   TO I
                       MOVE 0   TO FLAG-OC
                  END-IF
                  IF  (ABERTOS NOT = ZERO)
                  AND (TAGS-NIVEL NOT > ABERTOS-NIVEL(ABERTOS))
                       PERFORM TEST AFTER
                               UNTIL ABERTOS = ZERO
                                  OR TAGS-NIVEL >
                                     ABERTOS-NIVEL(ABERTOS)
                       IF   '[' = FECHA(ABERTOS)
                             WRITE BINARIO-REG FROM '}'
                             PERFORM 070-PUT-LINE THRU 070-99-FIM
                             WRITE BINARIO-REG FROM ']'
                       END-IF
                       IF   '{' = FECHA(ABERTOS)
                             PERFORM 070-PUT-LINE THRU 070-99-FIM
                             WRITE BINARIO-REG FROM '}'
                       END-IF
                       move space to fecha(abertos)
                       IF  TAGS-NIVEL = ABERTOS-NIVEL(ABERTOS)
                           WRITE BINARIO-REG FROM ','
                           subtract 1 from abertos
                           PERFORM 070-PUT-LINE THRU 070-99-FIM
                           add      1   to abertos
                       END-IF
                       SUBTRACT 1 FROM ABERTOS
                  END-IF
                  IF   TAGS-NIVEL = NIVEL-TESTE
                  and  virgula = 0
                       WRITE BINARIO-REG FROM ','
240519                 PERFORM 070-PUT-LINE THRU 070-99-FIM
                  END-IF
                  move 0 to virgula
                  IF   TAGS-TIPO = 'G'
                  and  abertos > 1
                       PERFORM 070-PUT-LINE THRU 070-99-FIM
                  end-if
                  STRING '"'      DELIMITED BY SIZE
                         TAGS-TAG DELIMITED BY SPACE
                         '":'     DELIMITED BY SIZE
                             INTO BUFFER-OUTPUT(I:)
                  MOVE TAGS-NIVEL TO NIVEL-TESTE
                  PERFORM 065-WRITE-BUFFER THRU 065-99-FIM
                  IF   TAGS-TIPO = 'G'
                       ADD 1 TO ABERTOS
                       IF   TAGS-OCORRENCIAS = 1
                            MOVE '[' TO FECHA(ABERTOS)
                            move 1 to flag-oc
                       END-IF
                       IF   TAGS-OCORRENCIAS = 0
                            MOVE '{' TO FECHA(ABERTOS)
                       END-IF
                       MOVE TAGS-NIVEL TO ABERTOS-NIVEL(ABERTOS)
                       WRITE BINARIO-REG FROM FECHA(ABERTOS)
                       PERFORM 070-PUT-LINE THRU 070-99-FIM
                  ELSE
                       IF   BUFFER (TAGS-OFFSET: TAGS-LENGTH)
                          = LOW-VALUES
                            WRITE BINARIO-REG FROM 'n'
                            WRITE BINARIO-REG FROM 'u'
                            WRITE BINARIO-REG FROM 'l'
                            WRITE BINARIO-REG FROM 'l'
                       ELSE
                            PERFORM 083-GET-WS THRU 083-99-FIM
                            MOVE SPACES TO BUFFER-OUTPUT
                            IF   TAGS-TIPO = 'X'
                                 move BUFFER (TAGS-OFFSET: TAGS-LENGTH)
                                   to teste-bolean
                                 inspect teste-bolean converting
                                         'FALSETRU' to 'falsetru'
                                 IF teste-bolean = 'false' or 'true'
                                    move teste-bolean to buffer-output
                                    PERFORM 065-WRITE-BUFFER
                                       THRU 065-99-FIM
                                 ELSE
                                 MOVE '"' TO BUFFER-OUTPUT
                                 MOVE 1   TO Y
                                 PERFORM VARYING I FROM 1 BY 1
                                           UNTIL I > TAGS-LENGTH
                                    IF  BUFFER-FIELD (I: 1) = '"'
                                        ADD 1 TO Y
                                        MOVE '\'
                                          TO BUFFER-OUTPUT(Y:1)
                                    END-IF
                                    ADD 1 TO Y
                                    MOVE BUFFER-FIELD (I: 1)
                                      TO BUFFER-OUTPUT(Y:1)
                                 END-PERFORM
                                 PERFORM 065-WRITE-BUFFER
                                    THRU 065-99-FIM
                                 IF  BUFFER-FIELD (1: TAGS-LENGTH)
                                     = SPACES
                                     WRITE BINARIO-REG FROM ' '
                                 END-IF
                                 WRITE BINARIO-REG FROM '"'
                                 END-IF
                            ELSE
                                 MOVE BUFFER-FIELD TO BUFFER-OUTPUT
                                 PERFORM 065-WRITE-BUFFER
                                    THRU 065-99-FIM
                            END-IF
                       END-IF
                  END-IF
              END-IF
           END-PERFORM
           PERFORM UNTIL ABERTOS = 0
                   IF   '[' = FECHA(ABERTOS)
                         WRITE BINARIO-REG FROM '}'
                         PERFORM 070-PUT-LINE THRU 070-99-FIM
                         WRITE BINARIO-REG FROM ']'
                   END-IF
                   IF   '{' = FECHA(ABERTOS)
                         PERFORM 070-PUT-LINE THRU 070-99-FIM
                         WRITE BINARIO-REG FROM '}'
                   END-IF
                   SUBTRACT 1 FROM ABERTOS
           END-PERFORM
           PERFORM 070-PUT-LINE THRU 070-99-FIM
           WRITE BINARIO-REG FROM '}'.

       060-99-FIM. EXIT.

       065-WRITE-BUFFER.

           PERFORM VARYING X FROM 1 BY 1
                     UNTIL X = LENGTH BUFFER-OUTPUT
                        OR BUFFER-OUTPUT (X:) = SPACE
                    CONTINUE
           END-PERFORM
           IF   BUFFER-OUTPUT (X:1) = SPACE
                SUBTRACT 1 FROM X
           END-IF
           PERFORM VARYING I FROM 1 BY 1
                     UNTIL I > X
                   MOVE BUFFER-OUTPUT (I: 1)
                     TO BINARIO-REG
                PERFORM 085-GRAVA-UTF-8
                   THRU 085-99-FIM
           END-PERFORM.

       065-99-FIM. EXIT.

       070-PUT-LINE.

           IF   NOT CWXML-LINE-FEED
           OR   BINARIO-REG = X'0A'
                EXIT PARAGRAPH
           END-IF

           IF   CWUNIX-OFF
                WRITE BINARIO-REG FROM X'0D'
           END-IF
           WRITE BINARIO-REG FROM X'0A'

           IF FS-TAGS < '10'
              PERFORM 3 TIMES
                      WRITE BINARIO-REG FROM SPACE
              END-PERFORM
           END-IF

           COMPUTE O = ABERTOS * 3
           PERFORM O TIMES
                   WRITE BINARIO-REG FROM SPACE
           END-PERFORM.
           MOVE X'0A' TO BINARIO-REG.

       070-99-FIM. EXIT.

       083-GET-WS.

           IF TAGS-TIPO = '9'
              COMPUTE X = TAGS-OFFSET + TAGS-LENGTH - 1
              MOVE SPACES TO BUFFER-FIELD SINAL-S
              MOVE 1      TO D
              PERFORM VARYING I FROM 1 BY 1
                            UNTIL I > 30
                      IF BUFFER (X:1) = SINAL-CHAR (I)
                         COMPUTE N = I - 1
                         MOVE BUFFER (X:1) TO SINAL-S
                         MOVE N (2:1) TO BUFFER (X:1)
                         IF I < 21
                            MOVE '-' TO BUFFER-FIELD
                            MOVE 2   TO D
                         END-IF
                         EXIT PERFORM
                      END-IF
              END-PERFORM
              MOVE TAGS-OFFSET TO Y
              MOVE D           TO X
              PERFORM TAGS-TAMANHO TIMES
                   IF (BUFFER-FIELD (D:) NOT = SPACES)
                   OR (BUFFER (Y:1) NOT = '0')
                       MOVE BUFFER (Y:1)
                         TO BUFFER-FIELD (X:1)
                       ADD 1 TO X
                   END-IF
                   ADD 1 TO Y
              END-PERFORM
              IF  X = D
                  MOVE '0' TO BUFFER-FIELD (X:1)
                  ADD 1 TO X
              END-IF
              IF  TAGS-DECIMAIS NOT = 0
                  MOVE '.' TO BUFFER-FIELD (X:1)
                  ADD  1   TO X
                  MOVE BUFFER(Y:TAGS-DECIMAIS)
                    TO BUFFER-FIELD (X:)
                  ADD TAGS-DECIMAIS TO X
              END-IF
              IF BUFFER (TAGS-OFFSET: TAGS-LENGTH)
                 = SPACES
                 MOVE 0 TO X
              END-IF
              IF SINAL-S NOT = SPACE
                 COMPUTE Y = TAGS-OFFSET
                           + TAGS-LENGTH - 1
                 MOVE SINAL-S TO BUFFER (X:1)
              END-IF
           ELSE
              MOVE BUFFER (TAGS-OFFSET: TAGS-LENGTH)
                TO BUFFER-FIELD
              MOVE TAGS-LENGTH TO X
           END-IF

           PERFORM UNTIL X = 0
                   OR BUFFER-FIELD(X: 1) NOT = SPACE
                   SUBTRACT 1 FROM X
           END-PERFORM
230519
230519     IF  X = 0
230519     AND TAGS-TIPO = '9'
230519         MOVE '0' TO BUFFER-FIELD
230519     END-IF.

       083-99-FIM. EXIT.

       085-GRAVA-UTF-8.

           IF BINARIO-REG = SPACE
           OR BINARIO-REG NUMERIC
              WRITE BINARIO-REG
           ELSE
              COMPUTE O = BINARIO-ASCII + 1
              PERFORM VARYING D FROM 1 BY 1
                      UNTIL D > 5
                    OR xml-string (O) (D:1)
                       = SPACE
                    WRITE BINARIO-REG
                     FROM xml-string (O) (D:1)
              END-PERFORM
           END-IF.

       085-99-FIM. EXIT.
       END PROGRAM CWJSON.
