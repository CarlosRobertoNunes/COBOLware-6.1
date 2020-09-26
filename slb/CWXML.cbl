       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWXML.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  04/11/2010.
       SECURITY.      *************************************************
                      *                                               *
                      * Leitura e Grava��o de documentos XML          *
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

           SELECT controle  ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS controle-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-controle
                  RESERVE NO ALTERNATE AREA.

           SELECT XMLCONV ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS XMLCONV-KEY
                  LOCK MODE     IS EXCLUSIVE
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-XMLCONV.

       DATA DIVISION.
       FILE SECTION.

       FD  BINARIO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS CWXML-FILE.

       01  BINARIO-REG.
           05 BINARIO-CARACTER PIC X.
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

       FD  controle
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-controle.

       01  controle-REG.
           05 controle-CHAVE     COMP-X PIC  9(008).
           05 controle-FLAG             PIC  X(001).
           05 controle-USED      COMP-X PIC  9(008).

       FD  XMLCONV
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-XMLCONV.

       01  XMLCONV-REG.
           05 XMLCONV-KEY         PIC  X(005).
           05 XMLCONV-CHAR        PIC  X(001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 ESTILO-NIVEL            pic  9(002) value 0.
           05 BARRA                   pic  x(001) value '/'.
           05 CWmd5DIR                pic  x(255) value spaces.
           05 pasta                   pic  x(003) value 'md5'.
           05 dia                     pic  x(010) value spaces.
           05 hora                    pic  x(008) value spaces.
           05 programa                pic  x(008) value spaces.
           05 usuario                 pic  x(030) value spaces.
           05 empresa                 pic  x(030) value spaces.
           05 sistema                 pic  x(030) value spaces.
           05 REVISAO                 PIC  X(016) VALUE SPACES.
           05 XMLCOMMENT              PIC  X(255) VALUE SPACES.
           05 SINAL-S                 PIC  X(001) VALUE SPACE.
           05 STRING-ANTERIOR         PIC  X(255) VALUE SPACES.
           05 FIM              COMP-X PIC  9(008) VALUE 0.
           05 BUFFER-FIELD            PIC X(5000) VALUE SPACES.
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
           05 ER-controle.
              10 FS-controle          PIC  X(002) VALUE "00".
              10 LB-controle          PIC  X(255) VALUE "$TEMP/cwxml1".
           05 ER-XMLCONV.
              10 FS-XMLCONV           PIC  X(002) VALUE "00".
              10 LB-XMLCONV           PIC  X(255) VALUE "$TEMP/cwxml2".
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
           05 X                       PIC  9(005) VALUE ZERO.
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
           05 ASPA                    PIC  X(001) VALUE SPACE.
           05 ERRO                    PIC  X(002) VALUE SPACES.
           05 ESPACO                  PIC  9(001) VALUE 0.
           05 IGUAL                   PIC  9(001) VALUE 0.
           05 TAG-LEVEL               PIC  9(002) VALUE 0.
           05 TAG-STATUS              PIC  9(001) VALUE 0.
           05 TAG-LONG                PIC X(5000) VALUE SPACES.
           05 TP                      PIC  9(004) VALUE 0.
           05 POS                     PIC  9(004) VALUE 0.
           05 D-NIVEL                 PIC  9(002) VALUE 0.
           05 D-NIVEL-1               PIC  9(002) VALUE 0.
           05 D-NIVEL-2               PIC  9(002) VALUE 0.
           05 NIVEIS.
              10 N                    PIC  9(002).
              10 OCCURS 99.
                 15 NIVEL-OC          PIC  9(002).
                 15 NIVEL-SEQ COMP-X  PIC  9(008).

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

       COPY CWUNIX.
       COPY CWMD5.

       LINKAGE SECTION.

       COPY CWXML.
       01  BUFFER PIC X.
       01  LAYOUT PIC X.

       PROCEDURE DIVISION USING PARAMETROS-CWXML.

       000-INICIO.

           ON 1
              DISPLAY 'CWREVS' UPON ENVIRONMENT-NAME
              ACCEPT  REVISAO  FROM ENVIRONMENT-VALUE
              OPEN I-O XMLCONV
              PERFORM VARYING X FROM 1 BY 1 UNTIL X > 255
                  IF xml-string (X) NOT = SPACE
                     COMPUTE BINARIO-ASCII = X - 1
                     MOVE BINARIO-CARACTER TO XMLCONV-CHAR
                     IF xml-string (X) (2: 1) NOT = SPACE
                        MOVE xml-string (X) TO XMLCONV-KEY
                        WRITE XMLCONV-REG
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

           IF CWXML-PUT
              DISPLAY 'XMLCOMMENT' UPON ENVIRONMENT-NAME
              ACCEPT   XMLCOMMENT  FROM ENVIRONMENT-VALUE
              OPEN OUTPUT BINARIO
           ELSE
              OPEN INPUT BINARIO
           END-IF

           IF CWXML-STATUS > '09'
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
              DISPLAY 'Primeiro uso do layout, Gerando dicion�rio xml'
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
           DISPLAY 'CWXML' UPON ENVIRONMENT-NAME
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

           OPEN I-O controle

           IF CWXML-PUT
              PERFORM 060-PUT-XML THRU 060-99-FIM
           ELSE
              PERFORM 020-GET-XML THRU 020-99-FIM
           END-IF

           CLOSE TAGS controle

           IF ERRO NOT = SPACES
              CLOSE BINARIO
              MOVE ERRO TO CWXML-STATUS
           ELSE
              CLOSE BINARIO
           END-IF.

       000-99-FIM. GOBACK.

       020-GET-XML.

           MOVE SPACES TO TAGS-STRING
                          BUFFER (1: CWXML-BUFFER-LENGTH)

           PERFORM UNTIL CWXML-STATUS > '09'
              PERFORM 030-LER-BINARIO THRU 030-99-FIM
              IF CWXML-STATUS < '10'
                 EVALUATE TRUE
                    WHEN (BINARIO-CARACTER = X'0D' OR X'0A')
                      OR (BINARIO-CARACTER = SPACES
                      AND (POS = 0 OR TAG-STATUS = 0))
                         CONTINUE
                    WHEN TAG-STATUS = 2
                         IF BINARIO-CARACTER = '<'
                            MOVE 0      TO TP
                            MOVE SPACES TO TAG-W
                            PERFORM 030-LER-BINARIO THRU 030-99-FIM
                                 IF CWXML-STATUS > '09'
                                    MOVE X'3911' TO ERRO
                                    EXIT PERFORM
                                 END-IF
                            IF BINARIO-CARACTER = '/'
                               PERFORM UNTIL BINARIO-CARACTER = '>'
                                    PERFORM 030-LER-BINARIO
                                       THRU 030-99-FIM
                                    IF CWXML-STATUS > '09'
                                       MOVE X'3911' TO ERRO
                                       EXIT PERFORM
                                    END-IF
                                    IF BINARIO-CARACTER = '>'
                                       IF TAG-W NOT = TAG
                                          MOVE X'3911' TO ERRO
                                       ELSE
                                          PERFORM 040-SET-DATA
                                             THRU 040-99-FIM
                                          PERFORM 025-TAG-CLOSE
                                             THRU 025-99-FIM
                                       END-IF
                                       MOVE 0 TO TAG-STATUS
                                       EXIT PERFORM
                                    ELSE
                                       IF BINARIO-CARACTER NOT = SPACE
                                          ADD 1 TO TP
                                          IF   TP > LENGTH TAG-W
                                               MOVE X'390B' TO ERRO
                                               EXIT PERFORM
                                          END-IF
                                          MOVE BINARIO-CARACTER
                                            TO TAG-W (TP: 1)
                                       END-IF
                                    END-IF
                               END-PERFORM
                               IF ERRO NOT = SPACES
                                  EXIT PERFORM
                               END-IF
                            ELSE
                               ADD 1    TO POS
                               MOVE '<' TO BUFFER-FIELD(POS:1)
                               ADD 1    TO POS
                               MOVE BINARIO-CARACTER
                                 TO BUFFER-FIELD(POS:1)
                            END-IF
                         ELSE
                            ADD 1 TO POS
                            MOVE BINARIO-CARACTER TO BUFFER-FIELD(POS:1)
                         END-IF
                    WHEN BINARIO-CARACTER = '<'
                     AND TAG-STATUS = 0
                         MOVE 1 TO TAG-STATUS
                         PERFORM 030-LER-BINARIO THRU 030-99-FIM
                              IF CWXML-STATUS > '09'
                                 MOVE X'3911' TO ERRO
                                 EXIT PERFORM
                              END-IF
                         IF  BINARIO-CARACTER = '/'
                             IF  TAG-LEVEL NOT = 0
                                 MOVE SPACES TO TAG-W
                                 MOVE 0      TO TP
                                 PERFORM UNTIL BINARIO-CARACTER = '>'
                                         PERFORM 030-LER-BINARIO
                                            THRU 030-99-FIM
                                         IF CWXML-STATUS > '09'
                                            MOVE X'3911' TO ERRO
                                            EXIT PERFORM
                                         END-IF
                                       IF (BINARIO-CARACTER NOT = SPACE)
                                       AND (BINARIO-CARACTER NOT = '>')
                                          ADD 1 TO TP
                                          IF   TP > LENGTH TAG-W
                                               MOVE X'390B' TO ERRO
                                               EXIT PERFORM
                                          END-IF
                                          MOVE BINARIO-CARACTER
                                            TO TAG-W (TP: 1)
                                       END-IF
                                 END-PERFORM
                                 IF TAG = TAG-W
                                    PERFORM 025-TAG-CLOSE
                                       THRU 025-99-FIM
                                    EXIT PERFORM CYCLE
                                 END-IF
                             END-IF
                             MOVE X'3911' TO ERRO
                             EXIT PERFORM
                         END-IF
                         ADD  1 TO TAG-LEVEL
                         MOVE 1 TO TP
                         MOVE 0 TO ESPACO
                                   IGUAL
                         MOVE BINARIO-CARACTER TO TAG-LONG
                         PERFORM UNTIL BINARIO-CARACTER = '>'
                                 PERFORM 030-LER-BINARIO THRU 030-99-FIM
                                      IF CWXML-STATUS > '09'
                                         MOVE X'3911' TO ERRO
                                         EXIT PERFORM
                                      END-IF
                                 IF  (BINARIO-CARACTER NOT = '>')
                                 AND (TAG-LONG NOT = SPACES)
                                     ADD 1 TO TP
                                     IF   TP > LENGTH TAG-LONG
                                          MOVE X'390B' TO ERRO
                                          EXIT PERFORM
                                     ELSE
                                          MOVE BINARIO-CARACTER
                                            TO TAG-LONG (TP: 1)
                                     END-IF
                                     IF  BINARIO-CARACTER = '='
                                         MOVE 1 TO IGUAL
                                     END-IF
                                     IF  BINARIO-CARACTER = SPACE
                                         MOVE 1 TO ESPACO
                                     ELSE
                                         IF ESPACO = 1
                                            MOVE 2 TO ESPACO
                                         END-IF
                                     END-IF
                                 END-IF
                         END-PERFORM
                         IF TAG-LONG (TP:1) = '/'
                            IF IGUAL = 0
                               SUBTRACT 1 FROM TAG-LEVEL
                               MOVE 0 TO TAG-STATUS
                               EXIT PERFORM CYCLE
                            ELSE
                               MOVE 2 TO IGUAL
                               MOVE SPACE TO TAG-LONG (TP:1)
                               SUBTRACT 1 FROM TP
                            END-IF
                         END-IF
                         IF CWXML-STATUS > '09'
                            EXIT PERFORM
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
                            IF (FS-TAGS NOT = '00')
                            AND I > 1
                               MOVE SPACES TO TAGS-STRING (I: )
                               STRING '/' DELIMITED BY SIZE
                                      TAG DELIMITED BY SPACE
                                      '(1)' DELIMITED BY SIZE
                               INTO TAGS-STRING (I: )
                               READ TAGS ignore lock KEY IS TAGS-STRING
                               IF FS-TAGS < '10'
                                  MOVE TAGS-CHAVE TO controle-CHAVE
                                  READ controle
                                  IF FS-controle = '23'
                                     MOVE 0 TO controle-USED
                                     WRITE controle-REG
                                     READ controle
                                  END-IF
                                  ADD 1 TO controle-USED
                                  REWRITE controle-REG
                                  IF controle-USED > 1
                                     MOVE SPACES TO TAGS-STRING (I: )
                                     MOVE controle-USED TO OCORRENCIA
                                     PERFORM VARYING O FROM 1 BY 1
                                             UNTIL OCORRENCIA (O: 1)
                                               NOT = SPACE
                                             CONTINUE
                                     END-PERFORM
                                     STRING '/' DELIMITED BY SIZE
                                            TAG DELIMITED BY SPACE
                                            '('
                                            OCORRENCIA (O: )
                                            ')' DELIMITED BY SIZE
                                       INTO TAGS-STRING (I: )
                                       READ TAGS ignore lock
                                             KEY IS TAGS-STRING
                                  END-IF
                               END-IF
                            END-IF
                         END-IF
                         IF TAG (1:1) = '?' OR '!'
                            PERFORM 025-TAG-CLOSE THRU 025-99-FIM
                            EXIT PERFORM CYCLE
                         END-IF
                         IF IGUAL = 2
                            PERFORM 050-GET-DISPLAY
                            THRU    050-99-FIM
                            PERFORM 025-TAG-CLOSE THRU 025-99-FIM
                         ELSE
                         IF FS-TAGS < '10'
                            IF TAGS-TIPO = 'G'
                               MOVE 0 TO TAG-STATUS
                               IF IGUAL = 1
                                  PERFORM 050-GET-DISPLAY
                                     THRU 050-99-FIM
                               END-IF
                            ELSE
                               MOVE 2      TO TAG-STATUS
                               MOVE SPACES TO BUFFER-FIELD
                               MOVE 0      TO POS
                            END-IF
                         ELSE
                            MOVE SPACES TO TAGS-STRING (I: )
                            STRING '/' DELIMITED BY SIZE
                                   TAG DELIMITED BY SPACE
                              INTO TAGS-STRING (I: )
                            MOVE SPACES TO TAG-w
                            MOVE 0      TO TAG-STATUS TP
                            PERFORM UNTIL 0 > 1
                               PERFORM 030-LER-BINARIO THRU 030-99-FIM
                                    IF CWXML-STATUS > '09'
                                       MOVE X'3911' TO ERRO
                                       EXIT PERFORM
                                    END-IF
                               EVALUATE TRUE
                                   WHEN BINARIO-CARACTER = '<'
                                   AND  TAG-STATUS = 0
                                        MOVE 1 TO TAG-STATUS
                                   WHEN BINARIO-CARACTER = '/'
                                   AND  TAG-STATUS = 1
                                        MOVE 2 TO TAG-STATUS
                                   WHEN BINARIO-CARACTER = '>'
                                   AND  TAG-STATUS = 2
                                        IF TAG-W = TAG
                                           PERFORM 025-TAG-CLOSE
                                              THRU 025-99-FIM
                                           EXIT PERFORM
                                        ELSE
                                           MOVE SPACES TO TAG-w
                                           MOVE 0      TO TAG-STATUS TP
                                        END-IF
                                   WHEN (BINARIO-CARACTER NOT = SPACE)
                                   AND  TAG-STATUS = 2
                                        ADD 1 TO TP
                                        IF   TP > LENGTH TAG-W
                                             MOVE X'390B' TO ERRO
                                             EXIT PERFORM
                                        ELSE
                                           MOVE BINARIO-CARACTER
                                             TO TAG-W (TP: 1)
                                        END-IF
                               END-EVALUATE
                            END-PERFORM
                         END-IF
                         END-IF
                 END-EVALUATE
              END-IF
           END-PERFORM.

       020-99-FIM. EXIT.

       025-TAG-CLOSE.

           SUBTRACT 1 FROM TAG-LEVEL
           MOVE 0       TO TAG-STATUS
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
           END-IF.

       025-99-FIM. EXIT.

       030-LER-BINARIO.

           READ BINARIO
           IF CWXML-STATUS < '10'
           AND (BINARIO-CARACTER = X'0D' OR X'0A')
                GO TO 030-LER-BINARIO
           END-IF.

       030-99-FIM. EXIT.

       040-SET-DATA.

           MOVE BUFFER-FIELD TO TAG-W
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
                   MOVE '00'       TO FS-XMLCONV
                   PERFORM UNTIL FS-XMLCONV > '09'
                           MOVE TAG-W (U:1)   TO PARTE-CONV (N:1)
                           MOVE PARTE-CONV    TO XMLCONV-KEY
                           START XMLCONV KEY NOT LESS XMLCONV-KEY
                              NOT INVALID KEY
                                  READ XMLCONV NEXT RECORD
                                  IF FS-XMLCONV = '00'
                                     IF PARTE-CONV = XMLCONV-KEY
                                        MOVE XMLCONV-CHAR
                                          TO BUFFER-FIELD (A:1)
                                        MOVE '23' TO FS-XMLCONV
                                     ELSE
                                        IF PARTE-CONV (1: N)
                                         = XMLCONV-KEY (1: N)
                                         ADD 1 TO N U
                                         MOVE TAG-W (U:1)
                                          TO PARTE-CONV (N:1)
                                        ELSE
                                          MOVE '99' TO FS-XMLCONV
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
           END-IF.

       040-99-FIM. EXIT.

       050-GET-DISPLAY.

           PERFORM VARYING X FROM 1 BY 1 UNTIL TAGS-STRING (X:1) = SPACE
                   CONTINUE
           END-PERFORM

           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > TP
                   IF TAG-LONG (Y:1) = '='
                      PERFORM VARYING O FROM Y BY -1
                                UNTIL O = 1
                                   OR TAG-LONG (O:1) = SPACE
                              CONTINUE
                      END-PERFORM
                      ADD  1      TO O
                      MOVE X      TO I
                      MOVE '/'    TO TAGS-STRING (I:)
                      ADD  1      TO I
                      PERFORM VARYING O FROM O BY 1
                                UNTIL TAG-LONG (O:1) = '='
                              MOVE TAG-LONG (O:1) TO TAGS-STRING (I:1)
                              ADD  1 TO I
                      END-PERFORM
                      MOVE SPACES TO BUFFER-FIELD
                      COMPUTE I = Y + 1
                      PERFORM UNTIL TAG-LONG (I:1) NOT = SPACE
                              ADD 1 TO I
                      END-PERFORM
                      MOVE TAG-LONG (I:1) TO ASPA
                      ADD 1 TO I
                      PERFORM VARYING Y FROM 1 BY 1
                              UNTIL TAG-LONG (I:1) = ASPA
                              MOVE TAG-LONG(I:1) TO BUFFER-FIELD(Y:1)
                              ADD  1 TO I
                      END-PERFORM
                      MOVE I TO Y
                      READ TAGS ignore lock KEY TAGS-STRING
                      IF FS-TAGS < '10'
                         PERFORM 040-SET-DATA
                            THRU 040-99-FIM
                      END-IF
                      ADD 1 TO TAG-LEVEL
                      PERFORM 025-TAG-CLOSE
                         THRU 025-99-FIM
                   END-IF
           END-PERFORM.

       050-99-FIM. EXIT.

       060-PUT-XML.

           DISPLAY 'CWXML-SPECIALS' UPON ENVIRONMENT-NAME
           ACCEPT CWXML-SPECIALS FROM ENVIRONMENT-VALUE
           WRITE BINARIO-REG FROM '<'
           WRITE BINARIO-REG FROM '?'
           PERFORM VARYING X FROM LENGTH CWXML-SPECIALS
                               BY -1 UNTIL X = 0
                               OR (CWXML-SPECIALS (X: 1) NOT = SPACE)
                   CONTINUE
           END-PERFORM
           PERFORM VARYING I FROM 1 BY 1
                     UNTIL I > X
                   If CWXML-SPECIALS (I: 4) = ' xml' OR ' XML'
                      WRITE BINARIO-REG FROM '?'
                      WRITE BINARIO-REG FROM '>'
                      PERFORM 070-PUT-LINE THRU 070-99-FIM
                      WRITE BINARIO-REG FROM '<'
                      WRITE BINARIO-REG FROM '?'
                      ADD 1 TO I
                   end-if
                   MOVE CWXML-SPECIALS (I: 1) TO BINARIO-REG
                   PERFORM 085-GRAVA-UTF-8 THRU 085-99-FIM
           END-PERFORM
           WRITE BINARIO-REG FROM '?'
           WRITE BINARIO-REG FROM '>'
           PERFORM 070-PUT-LINE THRU 070-99-FIM
           IF XMLCOMMENT = SPACES
              MOVE "<!-- COBOLware 6.1"  TO XMLCOMMENT
              MOVE REVISAO TO XMLCOMMENT(20:)
              MOVE "-->"   TO XMLCOMMENT(37:)
              PERFORM VARYING I FROM 1 BY 1
                      UNTIL I > 39
                      WRITE BINARIO-REG FROM XMLCOMMENT (I: 1)
              END-PERFORM
              PERFORM 070-PUT-LINE THRU 070-99-FIM
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
              MOVE SPACES TO XMLCOMMENT
              STRING '<!-- ' dia ' ' hora ' '
                     empresa ' ' sistema ' '
                     programa ' ' usuario
                     ' -->' DELIMITED BY SIZE
                INTO XMLCOMMENT
              INSPECT XMLCOMMENT CONVERTING '0' TO '@'
              CALL 'CWPACK' USING XMLCOMMENT LENGTH XMLCOMMENT
              PERFORM VARYING X FROM LENGTH XMLCOMMENT
                               BY -1 UNTIL X = 0
                               OR (XMLCOMMENT (X: 1) NOT = SPACE)
                   CONTINUE
              END-PERFORM
              INSPECT XMLCOMMENT CONVERTING '@' TO '0'
              PERFORM VARYING I FROM 1 BY 1
                      UNTIL I > X
                      IF I = 1 OR I = X
                         WRITE BINARIO-REG FROM XMLCOMMENT (I: 1)
                      ELSE
                         MOVE XMLCOMMENT (I: 1) TO BINARIO-REG
                         PERFORM 085-GRAVA-UTF-8 THRU 085-99-FIM
                      END-IF
              END-PERFORM
              PERFORM 070-PUT-LINE THRU 070-99-FIM
              MOVE SPACES TO XMLCOMMENT
           ELSE
              WRITE BINARIO-REG FROM '<'
              WRITE BINARIO-REG FROM '!'
              WRITE BINARIO-REG FROM '-'
              WRITE BINARIO-REG FROM '-'
              WRITE BINARIO-REG FROM ' '
              PERFORM VARYING X FROM LENGTH XMLCOMMENT
                               BY -1 UNTIL X = 0
                               OR (XMLCOMMENT (X: 1) NOT = SPACE)
                   CONTINUE
              END-PERFORM
              PERFORM VARYING I FROM 1 BY 1
                      UNTIL I > X
                      MOVE XMLCOMMENT (I: 1) TO BINARIO-REG
                      PERFORM 085-GRAVA-UTF-8 THRU 085-99-FIM
              END-PERFORM
              WRITE BINARIO-REG FROM ' '
              WRITE BINARIO-REG FROM '-'
              WRITE BINARIO-REG FROM '-'
              WRITE BINARIO-REG FROM '>'
              PERFORM 070-PUT-LINE THRU 070-99-FIM
           END-IF
           MOVE 0 TO TAGS-CHAVE
                     TAG-LEVEL
                     NIVEL
           START TAGS KEY NOT LESS TAGS-CHAVE
           PERFORM UNTIL FS-TAGS > '09'
                   PERFORM TEST AFTER
                           UNTIL controle-FLAG = '1'
                             OR FS-TAGS > '09'
                      READ TAGS NEXT RECORD ignore lock
                      IF FS-TAGS < '10'
                         MOVE TAGS-CHAVE TO controle-CHAVE
                         READ controle
                         IF  FS-controle = '23'
                             MOVE '1' TO controle-FLAG
                             WRITE controle-REG
                         END-IF
                      END-IF
                   END-PERFORM
                   IF FS-TAGS > '09'
                   OR TAGS-NIVEL < NIVEL
                      IF FS-TAGS < '10'
                         MOVE TAGS-CHAVE TO SEQUENCIA
                         PERFORM UNTIL TAGS-NIVEL NOT < NIVEL
                            MOVE STRING-ANTERIOR TO TAGS-STRING
                            READ TAGS ignore lock KEY IS TAGS-STRING
                            IF TAGS-TIPO = 'G'
                               PERFORM 081-CLOSE-TAG THRU 081-99-FIM
                               SUBTRACT 1 FROM TAG-LEVEL
                            END-IF
                            PERFORM VARYING X FROM LENGTH TAGS-STRING
                                                       BY -1
                                 UNTIL X = 1
                                    OR TAGS-STRING (X:1) = '/'
                                       CONTINUE
                            END-PERFORM
                            MOVE SPACES      TO TAGS-STRING (X:)
                            IF TAGS-TIPO = 'G'
                               MOVE TAGS-STRING TO STRING-ANTERIOR
                               move tags-nivel  to nivel
                               MOVE SEQUENCIA   TO TAGS-CHAVE
                               READ TAGS ignore lock
                            END-IF
                         END-PERFORM
                      ELSE
                         PERFORM 082-END-TAGS THRU 082-99-FIM
                         EXIT PERFORM
                      END-IF
                   END-IF
                   IF FS-TAGS < '10'
                      IF (BUFFER (TAGS-OFFSET: TAGS-LENGTH) = SPACES
                      OR LOW-VALUES)
                      AND TAGS-TIPO = 'G'
                         PERFORM VARYING X FROM 1 BY 1
                           UNTIL TAGS-STRING (X:1) = SPACE
                                 CONTINUE
                         END-PERFORM
                         MOVE '/' TO TAGS-STRING (X:1)
                         MOVE TAGS-STRING (1: X) TO TAG-LONG
                         PERFORM UNTIL TAGS-STRING (1: X)
                                 NOT = TAG-LONG(1: X)
                                 READ TAGS NEXT RECORD ignore lock
                                 IF FS-TAGS > '09'
                                    MOVE SPACES TO TAG-LONG
                                 END-IF
                         END-PERFORM
                         IF FS-TAGS < '10'
                            READ TAGS PREVIOUS RECORD ignore lock
                         ELSE
                            PERFORM 082-END-TAGS THRU 082-99-FIM
                            EXIT PERFORM
                         END-IF
                      ELSE
                         MOVE SPACES     TO TAG-LONG
                         MOVE TAGS-CHAVE TO SEQUENCIA
                         MOVE 0          TO D-NIVEL
                         MOVE TAGS-NIVEL TO D-NIVEL-1
                                            D-NIVEL-2
                         PERFORM TEST AFTER
                                       UNTIL TAGS-ESTILO NOT = '='
                                          OR FS-TAGS > '09'
                                 READ TAGS NEXT RECORD ignore lock
                                 IF   FS-TAGS < '10'
                                 AND  TAGS-ESTILO = '='
                                      MOVE TAGS-CHAVE TO controle-CHAVE
                                      READ controle
                                      IF  FS-controle = '23'
                                          MOVE '1' TO controle-FLAG
                                          WRITE controle-REG
                                      END-IF
                                 END-IF
                                 IF   FS-TAGS < '10'
                                 AND  TAGS-ESTILO = '='
                                 AND  controle-FLAG = '1'
                                      MOVE 1   TO D-NIVEL
                                      MOVE '0' TO controle-FLAG
                                      REWRITE controle-REG
                                      PERFORM 083-GET-WS
                                         THRU 083-99-FIM
                                      IF X NOT = 0
                                         PERFORM VARYING TP FROM LENGTH
                                                            TAG-LONG
                                                            BY -1
                                                 UNTIL TP = 0
                                                 OR (TAG-LONG (TP: 1)
                                                    NOT = SPACE)
                                                 CONTINUE
                                         END-PERFORM
                                         IF TP = 0
                                            MOVE 1 TO TP
                                         ELSE
                                            ADD 2 TO TP
                                         END-IF
                                         MOVE '"' TO ASPA
                                         PERFORM VARYING I FROM 1 BY 1
                                                 UNTIL I > X
                                                 IF BUFFER-FIELD (I:1)
                                                    = ASPA
                                                    MOVE "'" TO ASPA
                                                 END-IF
                                         END-PERFORM
                                         STRING TAGS-TAG
                                                DELIMITED BY SPACE
                                                '=' ASPA
                                                BUFFER-FIELD(1: X)
                                                ASPA
                                                DELIMITED BY SIZE
                                           INTO TAG-LONG (TP:)
                                      END-IF
                                 ELSE
                                      IF   FS-TAGS < '10'
                                           MOVE TAGS-NIVEL TO D-NIVEL-2
                                      ELSE
                                           MOVE 0          TO D-NIVEL-2
                                      END-IF
                                 END-IF
                         END-PERFORM
                         IF   D-NIVEL = 1
                         AND  (D-NIVEL-1 NOT < D-NIVEL-2)
                              PERFORM TEST AFTER
                                      UNTIL controle-FLAG = '1'
                                 READ TAGS PREVIOUS RECORD ignore lock
                                 IF FS-TAGS < '10'
                                    MOVE TAGS-CHAVE TO controle-CHAVE
                                    READ controle
                                    IF  FS-controle = '23'
                                        MOVE '1' TO controle-FLAG
                                        WRITE controle-REG
                                    END-IF
                                 END-IF
                              END-PERFORM
                              ADD      1              TO TAG-LEVEL
                              PERFORM  084-OPEN-TAG THRU 084-99-FIM
                              SUBTRACT 1            FROM TAG-LEVEL
                              WRITE BINARIO-REG FROM '/'
                              WRITE BINARIO-REG FROM '>'
                              PERFORM 070-PUT-LINE THRU 070-99-FIM
                              EXIT PERFORM CYCLE
                         END-IF
                         MOVE SEQUENCIA  TO TAGS-CHAVE
                         READ TAGS ignore lock
                         MOVE TAGS-NIVEL TO NIVEL
                         IF TAGS-TIPO = 'G'
                            ADD  1           TO TAG-LEVEL
                            MOVE TAGS-NIVEL  TO NIVEL
                            MOVE TAGS-STRING TO STRING-ANTERIOR
                            PERFORM 084-OPEN-TAG THRU 084-99-FIM
                            WRITE BINARIO-REG FROM '>'
                            PERFORM 070-PUT-LINE THRU 070-99-FIM
                         ELSE
                            if buffer (tags-offset: tags-length)
                               = low-values
                               exit perform cycle
                            end-if
                            PERFORM 083-GET-WS THRU 083-99-FIM
                            ADD 1 TO TAG-LEVEL
                            PERFORM 084-OPEN-TAG THRU 084-99-FIM
                            SUBTRACT 1 FROM TAG-LEVEL
                            IF  X = 0 AND TAG-LONG = SPACES
                                WRITE BINARIO-REG FROM SPACE
                                WRITE BINARIO-REG FROM '/'
                                WRITE BINARIO-REG FROM '>'
                            ELSE
                                WRITE BINARIO-REG FROM '>'
                                PERFORM VARYING I FROM 1 BY 1
                                          UNTIL I > X
                                        MOVE BUFFER-FIELD (I: 1)
                                          TO BINARIO-REG
                                     PERFORM 085-GRAVA-UTF-8
                                        THRU 085-99-FIM
                                END-PERFORM
                                WRITE BINARIO-REG FROM '<'
                                WRITE BINARIO-REG FROM '/'
                                PERFORM VARYING I FROM 1 BY 1
                                  UNTIL I > LENGTH TAGS-TAG
                                     OR TAGS-TAG (I: 1) = SPACE
                                        WRITE BINARIO-REG
                                         FROM TAGS-TAG (I: 1)
                                END-PERFORM
                                WRITE BINARIO-REG FROM '>'
                            END-IF
                            PERFORM 070-PUT-LINE THRU 070-99-FIM
                         END-IF
                      END-IF
                   END-IF
           END-PERFORM.

       060-99-FIM. EXIT.

       070-PUT-LINE.

           IF   CWXML-LINE-FEED
                IF   CWUNIX-OFF
                     WRITE BINARIO-REG FROM X'0D'
                END-IF
                WRITE BINARIO-REG FROM X'0A'
           END-IF.

       070-99-FIM. EXIT.

       080-SHIFT.

           IF   CWXML-LINE-FEED
           AND  TAG-LEVEL > 0
                COMPUTE O = TAG-LEVEL - 1
                PERFORM O TIMES
                        WRITE BINARIO-REG FROM SPACE
                        WRITE BINARIO-REG FROM SPACE
                END-PERFORM
           END-IF.

       080-99-FIM. EXIT.

       081-CLOSE-TAG.

           PERFORM 080-SHIFT THRU 080-99-FIM
           WRITE BINARIO-REG FROM '<'
           WRITE BINARIO-REG FROM '/'
           PERFORM VARYING I FROM 1 BY 1
             UNTIL I > LENGTH TAGS-TAG
                OR TAGS-TAG (I: 1) = SPACE
                   WRITE BINARIO-REG
                    FROM TAGS-TAG (I: 1)
           END-PERFORM
           WRITE BINARIO-REG FROM '>'
           PERFORM 070-PUT-LINE THRU 070-99-FIM.

       081-99-FIM. EXIT.

       082-END-TAGS.

           MOVE STRING-ANTERIOR TO TAGS-STRING
           PERFORM UNTIL TAGS-STRING = SPACES
              READ TAGS ignore lock KEY IS TAGS-STRING
              PERFORM 081-CLOSE-TAG THRU 081-99-FIM
              SUBTRACT 1 FROM TAG-LEVEL
              PERFORM VARYING X FROM LENGTH TAGS-STRING
                                         BY -1
                   UNTIL X = 1
                      OR TAGS-STRING (X:1) = '/'
                         CONTINUE
              END-PERFORM
              MOVE SPACES TO TAGS-STRING (X:)
           END-PERFORM.

       082-99-FIM. EXIT.

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
           END-PERFORM.

       083-99-FIM. EXIT.

       084-OPEN-TAG.

           PERFORM 080-SHIFT THRU 080-99-FIM
           WRITE BINARIO-REG FROM '<'
           PERFORM VARYING I FROM 1 BY 1
              UNTIL I > LENGTH TAGS-TAG
                 OR TAGS-TAG (I: 1) = SPACE
                   WRITE BINARIO-REG
                    FROM TAGS-TAG (I: 1)
           END-PERFORM
           IF  TAG-LONG NOT = SPACES
               PERFORM VARYING I FROM LENGTH TAG-LONG
                                   BY -1
                  UNTIL I = 1
                     OR (TAG-LONG (I: 1) NOT = SPACE)
                     CONTINUE
               END-PERFORM
               WRITE BINARIO-REG FROM SPACE
               PERFORM VARYING TP FROM 1 BY 1
                  UNTIL TP > I
                        MOVE TAG-LONG (TP: 1) TO BINARIO-REG
                        PERFORM 085-GRAVA-UTF-8 THRU 085-99-FIM
               END-PERFORM
           END-IF.

       084-99-FIM. EXIT.

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

       END PROGRAM CWXML.
