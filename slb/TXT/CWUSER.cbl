      $Set NoWriteLock
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWUSER.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  28/11/2003.
       SECURITY.      *************************************************
                      *                                               *
                      *  Tratamento de interface com o usu rio        *
                      *  modo texto                                   *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT OBJETOS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS OBJETOS-KEY
                  ALTERNATE RECORD KEY IS OBJETOS-SEQ =
                                          OBJETOS-WINDOW
                                          OBJETOS-SEQUENCE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-OBJETOS.

           SELECT HOTS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS HOTS-HOT
                  ALTERNATE KEY IS HOTS-ALT WITH DUPLICATES
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-HOTS.

           SELECT GUIA    ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS GUIA-CHAVE         WITH DUPLICATES
                  ALTERNATE RECORD KEY IS GUIA-FIELD  WITH DUPLICATES
                  ALTERNATE RECORD KEY IS GUIA-OBJECT WITH DUPLICATES
                  ALTERNATE RECORD KEY IS GUIA-DATANAME
                                                      WITH DUPLICATES
                  ALTERNATE RECORD KEY IS GUIA-SEQ
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-GUIA
                  RESERVE NO ALTERNATE AREA.

           SELECT GUIA-O  ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS GUIA-O-CHAVE  WITH DUPLICATES
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-GUIA-O
                  RESERVE NO ALTERNATE AREA.

           SELECT HIDE   ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS HIDE-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-HIDE
                  RESERVE NO ALTERNATE AREA.

           COPY CRITICA.SEL.

           SELECT BUTTONS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS BUTTONS-CHAVE
                  RESERVE NO ALTERNATE AREA
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-BUTTONS.

           SELECT SAVEATTR ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS SAVEATTR-FIELD
                  RESERVE NO ALTERNATE AREA
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-SAVEATTR.

           SELECT WINWORK ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS WINWORK-WINDOW
                  LOCK MODE     IS EXCLUSIVE
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-WINWORK.

           SELECT THUMBS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS THUMBS-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-THUMBS
                  RESERVE NO ALTERNATE AREA.

       DATA DIVISION.
       FILE SECTION.

       FD  OBJETOS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-OBJETOS.

       01  OBJETOS-REG.
           05 OBJETOS-KEY.
              10 OBJETOS-WINDOW     COMP-X PIC  9(004).
              10 OBJETOS-SCREEN-POSITION.
                 15 OBJETOS-ROW     COMP-X PIC  9(002).
                 15 OBJETOS-COL     COMP-X PIC  9(002).
           05 OBJETOS-SEQUENCE      COMP-X PIC  9(004).
           05 OBJETOS-CHARACTER-BUFFER     PIC  X(080).
           05 OBJETOS-ATTRIBUTE-BUFFER     PIC  X(080).
           05 OBJETOS-STRING-LENGTH COMP-X PIC  9(004).
           05 OBJETOS-OBJECT        COMP-X PIC  9(004).

       FD  HOTS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-HOTS.

       01  HOTS-REG.
           05 HOTS-HOT                     PIC  X(001).
           05 HOTS-KEY                     PIC  9(003).
           05 HOTS-OBJECT           COMP-X PIC  9(004).
           05 HOTS-ALT                     PIC  9(003).

       FD  GUIA
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-GUIA.

       01  GUIA-REG.
           05 GUIA-CHAVE.
              10 GUIA-CURPOS               PIC  X(004).
              10 GUIA-CURPOS2              PIC  X(004).
           05 GUIA-FIELD            COMP-X PIC  9(004).
           05 GUIA-OBJECT           COMP-X PIC  9(004).
           05 GUIA-POP                     PIC  9(001).
           05 GUIA-SEQ.
              10 GUIA-TYPE                 PIC  9(001).
              10 GUIA-OBJECTS       COMP-X PIC  9(004).
           05 GUIA-DATANAME                PIC  X(030).

       FD  GUIA-O
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-GUIA-O.

       01  GUIA-O-REG.
           05 GUIA-O-CHAVE.
              10 GUIA-O-CURPOS               PIC  X(004).
              10 GUIA-O-CURPOS2              PIC  X(004).
           05 GUIA-O-FIELD            COMP-X PIC  9(004).
           05 GUIA-O-OBJECT           COMP-X PIC  9(004).
           05 GUIA-O-POP                     PIC  9(001).
           05 GUIA-O-SEQ.
              10 GUIA-O-TYPE                 PIC  9(001).
              10 GUIA-O-OBJECTS       COMP-X PIC  9(004).
           05 GUIA-O-DATANAME                PIC  X(030).

       FD  HIDE
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-HIDE.

       01  HIDE-REG.
           05 HIDE-CHAVE                   PIC  X(030).
           05 HIDE-FIELD                   PIC  9(004).
           05 HIDE-OBJECT           COMP-X PIC  9(004).
           05 HIDE-LIST                    PIC  9(001).
           05 HIDE-COMBO                   PIC  9(001).
           05 HIDE-NOEDIT                  PIC  9(001).
           05 HIDE-LENGTH           COMP-X PIC  9(002).
           05 HIDE-LENRET           COMP-X PIC  9(002).

       COPY CRITICA.FD.

       FD  BUTTONS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-BUTTONS.

       01  BUTTONS-REG.
           05 BUTTONS-CHAVE            PIC  X(002).
           05 BUTTONS-TEXTO            PIC  X(080).
           05 BUTTONS-ATTRIB.
              15 BUTTONS-ATT    COMP-X PIC  9(002) OCCURS 80.
           05 BUTTONS-LENGTH    COMP-X PIC  9(004).

       FD  SAVEATTR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-SAVEATTR.

       01  SAVEATTR-REG.
           05 SAVEATTR-FIELD           PIC  9(004).
           05 SAVEATTR-ATRR            PIC  X(018).
           05 SAVEATTR-MODO-ACCEPT     PIC  X(001).
           05 SAVEATTR-PIC             PIC  X(030).

       FD  WINWORK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-WINWORK.

       01  WINWORK-REG.
           02 WINWORK-CHAVE.
              05 WINWORK-WINDOW          PIC  9(004) COMP-X.
           02 WINWORK-DATA.
              05 AVANCO-PENDENTE         PIC  9(001).
              05 SEQUENCIA        COMP-X PIC  9(004) VALUE 0.
              05 CURPOS.
                 10 CURPOS-LIN           PIC  9(002).
                 10 CURPOS-COL           PIC  9(002).
Frango        05 cwboxw-attr.
Frango           10 BOX-LINE                        OCCURS 25.
Frango              15 BOX-LINE-n COMP-X PIC  9(02) occurs 80.
              05 RM-LINE-ERASE           PIC  9(02).
              05 CBL-READ-WRITE-SCR-CHARS-ATTR.
                 10 SCREEN-POSITION.
                    15 ROW-NUMBER        PIC  9(002) COMP-X.
                    15 COLUMN-NUMBER     PIC  9(002) COMP-X.
                 10 CHARACTER-BUFFER.
                    15 CHAR-LIN          PIC  X(080) OCCURS 25.
                 10 ATTRIBUTE-BUFFER.
                    15 ATTR-LIN          PIC  X(080) OCCURS 25.
                 10 STRING-LENGTH        PIC  9(004) COMP-X.
           05 MULTILINE                  PIC  9(004).
           05 JANSAVE                    PIC  X(004) OCCURS 2000.
           05 pos-c.
               10 lin-c                  PIC  9(002).
               10 col-c                  PIC  9(002).
           05 scrolls                    pic  9(002) comp-x.
           05 scroll-map.
              10 scroll-lin occurs 25.
                 15 scroll-col occurs 80 PIC  9(002) COMP-X.

       FD  THUMBS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-THUMBS.

       01  THUMBS-REG.
           05 THUMBS-CHAVE.
              10 THUMBS-WINDOW   PIC  9(004) COMP-X.
              10 THUMBS-ID       PIC  9(002) COMP-X.
           05 THUMBS-KEY         PIC  9(003).
           05 THUMBS-FULL        PIC  9(002).
           05 THUMBS-TYPE        PIC  X(001).
           05 THUMBS-POSITION.
              10 THUMBS-ROW      PIC  9(002) COMP-X.
              10 THUMBS-COL      PIC  9(002) COMP-X.
           05 THUMBS-INITIAL.
              10 THUMBS-INITROW  PIC  9(002) COMP-X.
              10 THUMBS-INITCOL  PIC  9(002) COMP-X.
           05 THUMBS-THUMB    POINTER.
           05 THUMBS-CHA         PIC  X(080).
           05 THUMBS-ATT         PIC  X(080).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 SL-BOX             COMP-X PIC  9(004) VALUE 0.
           05 lenc               COMP-X PIC  9(004) VALUE 0.
           05 RESTO                     PIC  9(004) VALUE 0.
           05 ARROWS.
              10 ARROW-UP               PIC  X(001) VALUE X'18'.
              10 ARROW-DOWN             PIC  X(001) VALUE X'19'.
              10 ARROW-RIGHT            PIC  X(001) VALUE X'1A'.
              10 ARROW-LEFT             PIC  X(001) VALUE X'1B'.
           05 POSITS                    PIC  9(002) VALUE 0.
           05 THUMB-SAVE                PIC  X(001) VALUE SPACE.
           05 CWCONSOLE                 PIC  X(003) VALUE SPACES.
           05 FOREBACK                  PIC  X(003) VALUE SPACES.
           05 CBLS.
              10 CBL-READ-SCR-CHATTRS   PIC  X(020) VALUE
                 "CBL-READ-SCR-CHATTRS".
              10 CBL-WRITE-SCR-CHATTRS  PIC  X(021) VALUE
                 "CBL-WRITE-SCR-CHATTRS".
           05 BUFFER-LENGTH      COMP-X PIC  9(002) VALUE 0.
           05 POP                COMP-X PIC  9(002) VALUE 0.
           05 JL                 COMP-X PIC  9(002) VALUE 0.
           05 JK                 COMP-X PIC  9(002) VALUE 0.
           05 JJ                 COMP-X PIC  9(002) VALUE 0.
           05 ESPACO             COMP-X PIC  9(004) VALUE 0.
           05 JC                 COMP-X PIC  9(002) VALUE 0.
           05 JA                 COMP-X PIC  9(002) VALUE 0.
           05 JW                 COMP-X PIC  9(002) VALUE 0.
           05 FLOAT-ED                  PIC  X(030) VALUE SPACES.
           05 FLOAT-FIND                PIC  9(001) VALUE 0.
           05 DP-F                      PIC  X(001) VALUE SPACE.
           05 DP-F-P                    PIC  9(002) VALUE 0.
           05 SECUREMODE                PIC  X(008) VALUE SPACES.
           05 TAMANHO-CHECK             PIC  9(008) COMP-X.
           05 from-combo                PIC  9(01) VALUE 0.
           05 CWBOXF-OFF                PIC  9(01) VALUE 0.
           05 CWBOXF-OFF-SAVE           PIC  9(01) VALUE 0.
           05 JUST-D                    PIC  9(01) VALUE 0.
           05 JUST-FIELD                PIC  9(04) VALUE 0.
           05 JUST-SAVE                 PIC  9(04) VALUE 0.
           05 CRON                      PIC  X(03) VALUE SPACES.
           05 CWR                       PIC  9(03) VALUE 0.
           05 IB                        PIC  9(03) VALUE 0.
           05 SUBSCRIPT                 PIC  9(03) VALUE 0.
           05 SECURECHAR                PIC  X(80) VALUE SPACES.
           05 MB-SAVE                   PIC X(400) VALUE SPACES.
           05 MB-STRING                 PIC  X(80) VALUE SPACES.
           05 MB-LEN                    PIC  9(02) VALUE 0.
           05 MB-SHIFT                  PIC  9(02) VALUE 0.
           05 no-arrow                  PIC  9(01) VALUE 0.
           05 FROM-KBDC                 PIC  9(01) VALUE 0.
           05 SALTA                     PIC  9(01) VALUE 0.
           05 skip-next                 PIC  9(01) VALUE 0.
           05 from-tab                  PIC  9(01) VALUE 0.
           05 from-stab                 PIC  9(01) VALUE 0.
           05 COL-ED-sf                 PIC  9(02) VALUE 0.
           05 CURPOS-COL-sf             PIC  9(02) VALUE 0.
           05 CO-S                      PIC  9(02) VALUE 0.
           05 LI-S                      PIC  9(02) VALUE 0.
           05 FI-S                      PIC  9(004) VALUE 0.
           05 FA                        PIC  9(01) VALUE 0.
           05 att-BMS.
              06 att-bms-n pic 9(2) comp-x.
           05 ok                        PIC  9(01) VALUE 0.
           05 att                       PIC  9(03) VALUE 0.
           05 save-prot                 PIC  9(03) VALUE 0.
           05 ENTER-TERMINATE           PIC  X(03) VALUE SPACES.
           05 d-num              comp-3 PIC  9(18) VALUE 0.
           05 d-num-a            comp-3 PIC  9(18) VALUE 0.
           05 MB-ATTRIB                            VALUE LOW-VALUES.
              10 MB-ATTRIB-N            PIC  9(2) COMP-X.
           05 CWBACKDEFAULT             PIC  X     VALUE SPACE.
           05 NAO-EXIBE-VERIFICA        PIC  9(1)  VALUE 0.
           05 TIROU                     PIC  X(1)  VALUE SPACE.
           05 CWOBJE                    PIC  X(6)  VALUE SPACE.
           05 ZEROSUPRESS               PIC  X(2)  VALUE SPACE.
           05 pi-z                      PIC  9(2)  VALUE 0.
           05 pos-k.
               10 lin-k                 PIC  9(2)  VALUE 0.
               10 col-k                 PIC  9(2)  VALUE 0.
           05 dataname-k                pic  x(30) value spaces.
           05 pos-ss                    PIC  X(4)  VALUE SPACES.
           05 len-c                     PIC  9(4)  VALUE 0.
           05 p-sinal                   PIC  9(2)  VALUE 0.
           05 DINAMICA.
               10 DINLIN                PIC S9(2)  VALUE 0.
               10 DINCOL                PIC S9(2)  VALUE 0.
           05 JANELA.
               10 JANPOS.
                  15 JANLIN             PIC  9(2)  VALUE 0.
                  15 JANCOL             PIC  9(2)  VALUE 0.
               10 JANWID                PIC  9(2)  VALUE 80.
               10 JANHEI                PIC  9(2)  VALUE 25.
               10 POP-WINDOW     COMP-X PIC  9(4)  VALUE 7.
               10 JANMAX                PIC  9(2)  VALUE 80.
               10 JANLIMAX              PIC  9(2)  VALUE 25.
           05 AGORA                     PIC  X(6)  VALUE SPACES.
           05 TIME-E5                   PIC  X(6)  VALUE SPACES.
           05 algo                      PIC  9(2)  VALUE 0.
           05 COMMA-POSIT               PIC  9(2)  VALUE 0.
           05 LRM                       PIC  9(2)  VALUE 0.
           05 CRM                       PIC  9(2)  VALUE 0.
           05 SRM                       PIC  9(4)  VALUE 0.
           05 LEN-REV                   PIC  9(2)  VALUE 0.
           05 COL-REV                   PIC  9(2)  VALUE 0.
           05 REVERSEDS                            VALUE LOW-VALUES.
           10 REVERSEDS-LIN OCCURS 25.
              15 REVERSEDS-COL OCCURS 80 PIC 9(2) COMP-X.
           05 EXIBIR-FROM              PIC  9(001) VALUE 0.
           05 FROM-INSERT              PIC  9(001) VALUE 0.
           05 INSERT-CHAR              PIC  X(001) VALUE SPACE.
           05 DP-E                     PIC  X(001) VALUE SPACE.
           05 CWBEEP                   PIC  X(003) VALUE SPACES.
           05 CWENDK                   PIC  X(003) VALUE SPACES.
           05 DIGITOS                  PIC  9(002) VALUE 0.
           05 SX                       PIC  9(001) VALUE 0.
           05 nouser                   PIC  9(001) VALUE 0.
           05 USING-POSIT              PIC  9(004) VALUE 0.
           05 USING-LEN                PIC  9(004) VALUE 0.
           05 USING-Y                  PIC  9(004) VALUE 0.
           05 USING-MC.
              10 MC                    PIC  9(004) VALUE 0.
              10 MC-MAX                PIC  9(004) VALUE 0.
              10 MCS OCCURS 2000.
                 15 MC-POSIT           PIC  9(004) VALUE 0.
                 15 MC-LEN             PIC  9(004) VALUE 0.
                 15 MC-Y               PIC  9(004) VALUE 0.
           05 TIMEOUT-RETURN           PIC  9(002) VALUE 0.
           05 CWUPDATE                 PIC  X(005) VALUE SPACES.
           05 CWLOGT                   PIC  X(002) VALUE SPACES.
Frango*    05 cwboxw-attr                          VALUE LOW-VALUES.
Frango*       10 BOX-LINE              PIC  X(80) OCCURS 25.
           05 ZE                       PIC  X(001) VALUE "Z".
           05 IX                       PIC  9(002) VALUE 0.
           05 TS                       PIC  9(001) VALUE 0.
           05 yj                       PIC  9(002) VALUE 0.
           05 lenj                     PIC  9(002) VALUE 0.
           05 CWAUTO                   PIC  X(003) VALUE SPACES.
           05 ct                       PIC  X(001) VALUE SPACE.
           05 ct-lines                 PIC  9(002) VALUE zero.
           05 WS-TIMEOUT               PIC  9(018) VALUE 0.
           05 DGS               COMP-X PIC  9(002) VALUE 0.
           05 DGS-CHAR          COMP-X PIC  9(002) VALUE 0.
           05 EDITNUM                  PIC  X(003) VALUE SPACES.
           05 EDITCHAR                 PIC  X(003) VALUE SPACES.
           05 EDITJUST                 PIC  X(003) VALUE SPACES.
           05 TAMANHO-JUST      COMP-X PIC  9(008) VALUE 0.
           05 BARR                     PIC  X(003) VALUE SPACES.
           05 color-num         COMP-X PIC  9(002) VALUE 0.
           05 DF                       PIC  X(001) VALUE "D".
           05 CWENTRY                  PIC  X(003) VALUE SPACE.
           05 dif                      PIC  9(002) VALUE 0.
           05 BX                       PIC  9(002) VALUE 0.
           05 PZ                       PIC  9(002) VALUE 0.
           05 COL-ED-V                 PIC  9(002) VALUE 0.
           05 CURPOS-COL-V             PIC  9(002) VALUE 0.
           05 NEGATIVO                 PIC  9(001) VALUE 0.
           05 INPUT-NEGATIVO           PIC  9(001) VALUE 0.
           05 XN                       PIC  9(002) VALUE 0.
           05 N                        PIC  9(002) VALUE 0.
           05 NUMERO                   PIC  9(018) VALUE 0.
           05 SNUMERO REDEFINES NUMERO PIC S9(018).
           05 CWNUMERO                 PIC  X(018) VALUE SPACES.
           05 SINAL-ED                 PIC  9(001) VALUE 0.
           05 SAVE-NAME                PIC  X(030) VALUE SPACES.
           05 BUF                      PIC  X(080) VALUE SPACES.
           05 FLAG-CRITICA             PIC  9(003) VALUE 0.
           05 P                        PIC  9(002) VALUE 0.
           05 UNO                      PIC  9(002) VALUE 0.
           05 P2                       PIC  9(002) VALUE 0.
           05 CWCONTROL                PIC  9(001) VALUE 0.
           05 LIN-PLUS                 PIC  9(002) VALUE 0.
           05 COMBO-DISPLAY            PIC  9(001) VALUE 0.
           05 SAVE-FUNCTION            PIC  X(001) VALUE SPACE.
           05 SAVE-FUNCTION2           PIC  X(001) VALUE SPACE.
           05 SAVE-FUNCTION3           PIC  X(001) VALUE SPACE.
      *    05 DATANAME-F               PIC  X(030) VALUE SPACES.
           05 SCREEN-POSITION4         PIC  X(002) VALUE SPACES.
           05 ATTRIBUTE-BUFFER4        PIC  X(080) VALUE SPACES.
           05 CHARACTER-BUFFER4        PIC  X(080) VALUE SPACES.
           05 STRING-LENGTH4    COMP-X PIC  9(004) VALUE 0.
           05 SCREEN-POSITION5.
                 15                    PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER5     PIC  9(002) COMP-X VALUE 0.
           05 ATTRIBUTE-BUFFER5 COMP-X PIC  9(001) VALUE 0.
           05 ATTRIBUTE-BUFFER6 COMP-X PIC  9(001) VALUE 0.
           05 CARACTER-BOX             PIC  X(001) VALUE SPACE.
           05 BOXF-MOUSE               PIC  9(001) VALUE 0.
           05 FROM-BOXF-MOUSE          PIC  9(001) VALUE 0.
           05 BOXF-OFF                 PIC  9(001) VALUE 0.
           05 CWBOXF-TECLA             PIC  9(003) VALUE 0.
           05 CRITICOU                 PIC  9(002) VALUE 0.
           05 VEZES                    PIC  9(002) VALUE 0.
           05 TEMP-GUIA-REG            PIC  X(016) VALUE SPACES.
           05 TEMP-GUIA.
              10 TEMP-FIELD     COMP-X PIC  9(004) VALUE 0.
              10 TEMP-OBJECT    COMP-X PIC  9(004) VALUE 0.
              10 TEMP-POP              PIC  9(001) VALUE 0.
           05 NAO-EXIBE                PIC  9(001) VALUE 0.
           05 HIDE-GUIA                PIC  9(001) VALUE 0.
           05 SAVE-GUIA-SEQ            PIC  X(003) VALUE SPACES.
           05 LEN-CRITICA              PIC  9(004) VALUE 0.
           05 DEFOBJ                   PIC  9(001) VALUE 0.
           05 ERRO                     PIC  9(001) VALUE 0.
           05 PARAMETROS-CRITICA       PIC X(2000) VALUE SPACES.
           05 DOISMIL           COMP-X PIC  9(004) VALUE 2000.
      *    05 ATTR-MAP                 PIC X(2000) VALUE SPACES.
      *    05 TXT-MAP                  PIC X(2000) VALUE SPACES.
           05 CWMULTI                  PIC  X(002) VALUE SPACE.
           05 MULTI-USER               PIC  9(001) VALUE 0.
           05 FLAGS-SA                             VALUE ZEROS.
              10 FLAG-SA               PIC  9(001).
              10 COL-SA OCCURS 2000    PIC  9(002).
           05 POS-G.
              10 LIN-G                 PIC  9(002) VALUE 0.
              10 COL-G                 PIC  9(002) VALUE 0.
           05 POS-S.
              10 LIN-S                 PIC  9(002) VALUE 0.
              10 COL-S                 PIC  9(002) VALUE 0.
           05 POS-EX.
              10 LIN-EX                PIC  9(002) VALUE 0.
              10 COL-EX                PIC  9(002) VALUE 0.
           05 PLUS-FLAG                PIC  9(001) VALUE 0.
           05 DESCE                    PIC  X(001) VALUE X"1F".
           05 SKIPS                    PIC  9(002) VALUE 0.
           05 SIZE-ICON                PIC  9(002) VALUE 0.
           05 OBJECTS-CWOBJE    COMP-X PIC  9(004) VALUE 0.
           05 SET-GROUP                PIC  9(001) VALUE 1.
           05 TEXTOA                   PIC  9(002) COMP-X VALUE 0.
           05 FUNDO                    PIC  9(002) COMP-X VALUE 0.
           05 REVERSO                  PIC  9(002) COMP-X VALUE 0.
           05 ER-OBJETOS.
              10 FS-OBJETOS            PIC  X(002) VALUE "00".
              10 LB-OBJETOS            PIC  X(255) VALUE "cwuser1.".
           05 ER-HOTS.
              10 FS-HOTS               PIC  X(002) VALUE "00".
              10 LB-HOTS               PIC  X(255) VALUE "cwuser7.".
           05 ER-GUIA.
              10 FS-GUIA               PIC  X(002) VALUE "00".
              10 LB-GUIA               PIC  X(255) VALUE "cwuser2.".
           05 ER-GUIA-O.
              10 FS-GUIA-O             PIC  X(002) VALUE "00".
              10 LB-GUIA-O             PIC  X(255) VALUE "cwuser9.".
           05 ER-HIDE.
              10 FS-HIDE               PIC  X(002) VALUE "00".
              10 LB-HIDE               PIC  X(255) VALUE "cwuser3.".
           05 ER-CRITICA.
              10 FS-CRITICA            PIC  X(002) VALUE "00".
              10 LB-CRITICA            PIC  X(255) VALUE "cwuser4.".
           05 ER-USINGS.
              10 FS-USINGS             PIC  X(002) VALUE "00".
              10 LB-USINGS             PIC  X(255) VALUE "cwuser5.".
           05 ER-BUTTONS.
              10 FS-BUTTONS            PIC  X(002) VALUE "00".
              10 LB-BUTTONS            PIC  X(255) VALUE "cwuser6.".
           05 ER-SAVEATTR.
              10 FS-SAVEATTR           PIC  X(002) VALUE "00".
              10 LB-SAVEATTR           PIC  X(255) VALUE "cwuser8.".
           05 ER-WORKBOX.
              10 FS-WORKBOX            PIC  X(002) VALUE "00".
              10 LB-WORKBOX            PIC  X(255) VALUE "cwwbox.".
           05 ER-WINWORK.
              10 FS-WINWORK            PIC  X(002) VALUE "00".
              10 LB-WINWORK            PIC  X(255) VALUE "cwwin1".
           05 ER-THUMBS.
              10 FS-THUMBS             PIC  X(002) VALUE "00".
              10 LB-THUMBS             PIC  X(255) VALUE "cwwin3".
           05 SZ-CRITICA               PIC  9(004) VALUE 0.
           05 SAVE-CWOBJE       COMP-X PIC  9(004) VALUE 0.
           05 TESTE-M           COMP-X PIC  9(004) VALUE 0.
           05 TESTE-G           COMP-X PIC  9(004) VALUE 0.
           05 GROUP-W           COMP-X PIC  9(004) VALUE 0.
           05 SAVE-GUIA                PIC  9(001) VALUE 0.
           05 ACESO                    PIC  9(001) VALUE 0.
           05 ACENDER                  PIC  9(001) VALUE 0.
           05 ACENDER-MOUSE            PIC  9(001) VALUE 0.
           05 save-ACENDER-MOUSE       PIC  9(001) VALUE 0.
           05 SAVE-OBJECT              PIC  9(001) VALUE 0.
           05 ATT-0                    PIC  X(001) VALUE "0".
           05 ATT-P                    PIC  X(001) VALUE "p".
           05 ATT-X                    PIC  X(001) VALUE "p".
           05 INTENSO                  PIC  X(001) VALUE X"7F".
           05 BACK-NW                  PIC  9(002) VALUE 0.
           05 FORE-NW                  PIC  9(002) VALUE 0.
           05 ECHOCURSOR               PIC  X(003) VALUE SPACES.
           05 ECHODELETED              PIC  X(003) VALUE SPACES.
           05 CWCASE                   PIC  X(003) VALUE SPACES.
           05 CWACCENT                 PIC  X(003) VALUE SPACES.
           05 CWLITS                   PIC  X(003) VALUE SPACES.
           05 DATA-SAVE                PIC  X(080) VALUE SPACES.
           05 HEX-BUFFER               PIC  X(017) VALUE SPACES.
           05 DEC                      PIC  9(003) VALUE 0.
           05 CHAR                     PIC  X(001) VALUE SPACE.
           05 DEC-X REDEFINES CHAR     PIC  9(002) COMP-X.
           05 HORA-A                   PIC  X(006) VALUE SPACES.
           05 HORA-B                   PIC  X(006) VALUE SPACES.
           05 SEGUNDOS                 PIC  9(004) VALUE 0.
           05 KEYBOARD-STATUS          PIC  9(002) COMP-X VALUE 0.
           05 CONTEUDO                 PIC  X(050) VALUE SPACES.
           05 TEST-NUM                 PIC  X(001) VALUE SPACE.
              88 VIRGULA          VALUE "," ".".
              88 NUM              VALUE "0" THRU "9" "," ".".
              88 VALOR            VALUE "1" THRU "9".
              88 MASK             VALUE "9" "Z" "*" "-" "+".
              88 VALID            VALUE "0" THRU "9" "-" "+" "," ".".
              88 ZERO-OFF         VALUE "-" "+" "," "." "Z" "*".
              88 EDIT             VALUE "-" "+" "," "." "C" "R"
                                                        "D" "B" ":" "/".
              88 SINAL            VALUE "-" "+" "C" "R" "D" "B".
              88 SINAL-POS        VALUE "-" "+" "C" "D".
              88 SINAL-CD         VALUE "C" "R" "D" "B".
           05 SINAL-ATUAL              PIC  X(002) VALUE SPACES.
           05 SINAL-MASK               PIC  X(002) VALUE SPACES.
           05 SINAL-POSIT              PIC  9(002) VALUE 0.
           05 REMOVE-SINAL             PIC  9(001) VALUE 0.
           05 WORK-ED                  PIC  X(200) VALUE SPACES.
           05 DECIMAIS                 PIC  9(001) VALUE 0.
           05 SINAL-FLUTUANTE          PIC  9(001) VALUE 0.
           05 COL-ED-N                 PIC  9(002) VALUE 0.
           05 CURPOS-COL-N             PIC  9(002) VALUE 0.
           05 T-LIN                    PIC  9(002) VALUE 0.
           05 T-COL                    PIC  9(002) VALUE 0.
           05 S-LIN                    PIC  9(002) VALUE 0.
           05 S-COL                    PIC  9(002) VALUE 0.
           05 COL-SAVE                 PIC  9(004) VALUE 0.
           05 COL-END                  PIC  9(004) VALUE 0.
           05 COL-ED                   PIC  9(004) VALUE 0.
           05 COL-ED-1                 PIC  9(004) VALUE 0.
           05 CONTEXT-ID               PIC  X(038) VALUE SPACES.
           05 REPINS                   PIC  X(001) VALUE "?".
              88 INSERT-OFF                        VALUE "0".
              88 INSERT-ON                         VALUE "1".
           05 I                        PIC  9(004) VALUE 0.
           05 Z                        PIC  9(004) VALUE 0.
           05 II                       PIC  9(003) VALUE 0.
           05 E                        PIC  9(004) VALUE 0.
           05 objeto            COMP-X PIC  9(004) VALUE 0.
           05 OBJECT-GET-MOUSE  COMP-X PIC  9(004) VALUE 0.
           05 OBJECT-SAVE       COMP-X PIC  9(004) VALUE 0.
           05 OBJECT-BOXF       COMP-X PIC  9(004) VALUE 0.
           05 B                        PIC  9(004) VALUE 0.
           05 B1                       PIC  9(004) VALUE 0.
           05 B2                       PIC  9(004) VALUE 0.
           05 B3                       PIC  9(004) VALUE 0.
           05 J                        PIC  9(004) VALUE 0.
           05 T                        PIC  9(004) VALUE 0.
           05 X                        PIC  9(004) VALUE 0.
           05 Y                        PIC  9(004) VALUE 0.
           05 Y-MINUS                  PIC  9(001) VALUE 0.
           05 D                        PIC  9(004) VALUE 0.
           05 K                        PIC  9(004) VALUE 0.
           05 FROM-LEFT                PIC  9(001) VALUE 0.
           05 F                        PIC  9(004) VALUE 0.
           05 VERT-MOVE                PIC  9(004) VALUE 0.
           05 FIELD-CRITICA            PIC  9(004) VALUE 0.
           05 FIELD-GET-MOUSE          PIC  9(004) VALUE 0.
           05 SAVE-FIELD               PIC  9(004) VALUE 0.
           05 FIELD-BEEP               PIC  9(004) VALUE 0.
           05 FOCUS                    PIC  9(004) VALUE 0.
           05 FIELD                    PIC  9(004) VALUE 0.
           05 WORK-JUST                PIC  X(080) VALUE SPACES.
           05 ACCEPTs                  PIC  9(004) VALUE 0.
           05 CWBOXF-GUIA              PIC  9(004) VALUE 0.
           05 FIELDS                   PIC  9(004) VALUE 0.
           05 FIELD-LAST               PIC  9(004) VALUE 0.
           05 FIELD-ANT                PIC  9(004) VALUE 0.
           05 FIELD-CWBOXF             PIC  9(004) VALUE 0.
           05 FIELD-PRE                PIC  9(004) VALUE 0.
           05 FIELD-SAVE               PIC  9(004) VALUE 0.
           05 FIELD-USING              PIC  9(004) VALUE 0.
           05 GT                       PIC  9(001) VALUE 0.
           05 GTS                      PIC  9(004) VALUE 0.
           05 OBJECTS                  PIC  9(004) VALUE 0.
           05 X91-RESULT    COMP-X     PIC  9(002) VALUE 0.
           05 X91-FUNCTION  COMP-X     PIC  9(002) VALUE 16.
           05 X91-PARAMETER COMP-X     PIC  9(002) VALUE 0.
           05 ELEMENTO.
              10 POS-E.
                 15 LIN-E              PIC  9(002).
                 15 COL-E              PIC  9(002).
              10 REDEFINES POS-E.
                 15 PLUS-LIN           PIC  X(001).
                 15 LIN-X              PIC  9(002) COMP-X.
                 15 PLUS-COL           PIC  X(001).
                 15 COL-X              PIC  9(002) COMP-X.
              10 LEN-E                 PIC  9(002).
              10 ATTR-E.
                 15 MODE-E             PIC  X(001).
                    88 ACCEPT-E             VALUE "U" "u" "T" "t" "P".
                    88 VARIAVEL             VALUE "U" "u" "T" "t"
                                                  "F" "f" "P".
                    88 no-update            VALUE "T" "t".
                    88 ERASE-EOL            VALUE "l".
                    88 ERASE-EOS            VALUE "s".
                    88 BLANK-LINE           VALUE "L".
                    88 BLANK-SCREEN         VALUE "S".
                    88 TEXTO                VALUE "V" "v".
                    88 PROT                 VALUE "P".
                 15 FORE-E.
                    20 FORE-N              PIC  9(001).
                 15 BACK-E.
                    20 BACK-N          PIC  9(001).
                 15 SECURE-E           PIC  X(001).
                 15 BLINK-E            PIC  X(001).
                 15 BZERO-E            PIC  X(001).
                    88 FLOAT                VALUE "E".
                 15 EMPTY-E            PIC  X(001).
                    88 RM-EOL          VALUE 'L'.
                    88 RM-EOS          VALUE 'S'.
                 15 BEEP-E             PIC  X(001).
                 15 REVERSE-E          PIC  X(001).
                 15 AUTO-E             PIC  X(001).
                 15 HIGH-E             PIC  X(001).
                 15 UPPLOW-E           PIC  X(001).
                 15 ADVANCE-E          PIC  X(001).
                    88 AD-EOL          VALUE 'L'.
                    88 AD-EOS          VALUE 'S'.
              10 PLUS-E                PIC  X(004).
              10                       PIC  X(006).
              10 FIELD-GUIA-E          PIC  X(008).
              10 ATTR-INIT             PIC  X(001).
              10 COM-SINAL             PIC  9(001).
              10 MODO-ACCEPT           PIC  X(001).
                 88 INDEFINIDO                 VALUE "?".
                 88 VER-MODO-ACCEPT            VALUE SPACE.
                 88 NUMERICO                   VALUE "N" "b" "/".
                 88 NUMERICO-BARRA             VALUE "/".
                 88 ALFABETICO                 VALUE "A" "B" "?".
                 88 COM-BARRA                  VALUE "B" "b".
                 88 COM-BARRA1                 VALUE "B".
                 88 COM-BARRA2                 VALUE "b".
              10 PIC-E                 PIC  X(080).
              10 DATANAME-E            PIC  X(030).
              10 DATA-E                PIC  X(080).
              10 CTRL-E                PIC  X(080).
           05 ATTRIBUTE-X.
              10 ATTRIBUTE             PIC  9(002) COMP-X VALUE 0.
           05 ATTRIBUTE-NX.
              10 ATTRIBUTE-N           PIC  9(002) COMP-X VALUE 0.
           05 ATTRIBUTES-ACCEPT.
              10 ATA OCCURS 2000.
                 15 ATTRIBUTE-ACCEPT   PIC  9(002) COMP-X.
                 15 ACC-LIN            PIC  9(002) COMP-X.
                 15 ACC-COL            PIC  9(002) COMP-X.
                 15 ACC-LEN            PIC  9(002) COMP-X.
                 15 ACC-POP            PIC  9(002) COMP-X.
                 15 ACC-POP-CHAR       PIC  9(002) COMP-X.
           05 ATTRIBUTES-ACCEPT2.
              10 OCCURS 2000.
                 15 ACC-PIC            PIC  X(080).
                 15 ATTR-P             PIC  X(001).
           05 CARACTER                 PIC  X(001).
           05 CARACTER-X REDEFINES CARACTER
                                       PIC  9(002) COMP-X.
           05 SAVE-EDIT                PIC  9(003) VALUE 0.
           05 TECLA-EDIT               PIC  9(003) VALUE 0. COPY CWEDIT.
      *       88 ALTS VALUE
      *          250 241 242 243 244 245 246 247 248 249 265 266 267 268
      *          269 252 270 271 272 273 274 275 276 277 278 279 280 281
      *          282 283 284 251 285 286 287 288 289 290.

           05 KEY-STATUS               PIC  X(003) VALUE SPACES.
              88 TOOBJECT    VALUE X"320B09" X"320C00".
              88 UPDOWN      VALUE X"320B00" X"320C00" X"320B09"
                                   X"320400" X"320300" X"320900".
           05 MOUSE-HANDLE             PIC  9(008) COMP-X VALUE 1.
           05 MOUSE-BUTTONS            PIC  9(002) COMP-X VALUE 3.
           05 MOUSE-POSITION-S         PIC  X(004).
           05 MOUSE-POSITION-A         PIC  X(004).
           05 MOUSE-POSITION.
              10 ROW-MOUSE             PIC  9(004) COMP-X.
              10 COLUMN-MOUSE          PIC  9(004) COMP-X.
           05 MOUSE-DATA.
              10 MOUSE-EVENT-TYPE      PIC  9(004) COMP-X VALUE 0.
              10 MOUSE-EVENT-TIME      PIC  9(008) COMP-X VALUE 0.
              10 MOUSE-EVENT-ROW       PIC  9(004) COMP-X VALUE 0.
              10 MOUSE-EVENT-COL       PIC  9(004) COMP-X VALUE 0.
           05 MOUSE-READ-TYPE          PIC  9(002) COMP-X VALUE 0.
           05 MOUSE-MAP.
              10 FROM-MOUSE            PIC  9(001) VALUE ZERO.
              10 FROM-KEY              PIC  9(001) VALUE ZERO.
              10 SAVE-LIN              PIC  9(002) VALUE ZERO.
              10 SAVE-COL              PIC  9(002) VALUE ZERO.
              10 OCCURS 25.
                 15 MOUSE-OK           PIC  9(004) OCCURS 80.
                 15 MOUSE-GROUP COMP-X PIC  9(004) OCCURS 80.
                 15 MOUSE-EVENT COMP-X PIC  9(004) OCCURS 80.
           05 CURSOR-POSITION.
              10 CURSOR-ROW            PIC  9(002) COMP-X VALUE 0.
              10 CURSOR-COLUMN         PIC  9(002) COMP-X VALUE 0.
           05 DELETADOS-ATTR           PIC  X(043) VALUE ALL X"0C".
           05 DELETADOS                PIC X(9990) VALUE SPACES.
           05 DEL                      PIC  9(004) VALUE 0.
           05 AP                       PIC  9(002) COMP-X.
              88 AMARELO VALUE 014 030 046 062 078 094 110 126 142 158
                               174 190 206 222 238 254.
           05 TESTE                    PIC  X(001) VALUE SPACE.
           05 SL                       PIC  9(004) COMP-X VALUE 0.
           05 MODO                     PIC  9(001) VALUE ZERO.
           05 TP                       PIC  X(001) VALUE SPACE.
           05 CL                       PIC  9(003) VALUE 0.
           05 TABELA-CORES.
              10 COR                   PIC  X(001) OCCURS 256.
           05 TABELA-MOLDURA.
              10 BASE-MOLDURA          PIC  X(008) OCCURS 9.
           05 MOLDURA                                  VALUE SPACES.
              10 M-201                 PIC  X(001).
              10 M-205                 PIC  X(001).
              10 M-187                 PIC  X(001).
              10 M-186                 PIC  X(001).
              10 M-204                 PIC  X(001).
              10 M-185                 PIC  X(001).
              10 M-200                 PIC  X(001).
              10 M-188                 PIC  X(001).
           05 MM-205                   PIC  X(080) VALUE SPACES.
           05 ATTR-LOW                 PIC  X(080) VALUE LOW-VALUES.
           05 ATTR-HIGH                PIC  X(080) VALUE LOW-VALUES.
           05 ATTR-REV.
              10 AB OCCURS 80          PIC  9(002) COMP-X.
           05 BUTTON-TYPE              PIC  X(001) VALUE SPACES.
           05 TAMANHO                  PIC  9(002) VALUE 0.
           05 CBL-READ-WRITE-SCR-ATTRS2.
              10 SP.
                 15                    PIC  9(002) COMP-X VALUE 0.
                 15 CP                 PIC  9(002) COMP-X VALUE 0.
              10 SCREEN-POSITION2.
                 15 ROW-NUMBER2        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER2     PIC  9(002) COMP-X VALUE 0.
              10 ATTRIBUTE-BUFFER2     PIC  X(080) VALUE SPACES.
              10 REDEFINES ATTRIBUTE-BUFFER2.
                 25 MID-X2 OCCURS 80   PIC  9(002) COMP-X.
              10 STRING-LENGTH2        PIC  9(004) COMP-X VALUE 0.
              10 SCREEN-POSITION3.
                 15 ROW-NUMBER3        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER3     PIC  9(002) COMP-X VALUE 0.
           05 TEXTO-MID                PIC  X(080) VALUE SPACES.
           05 TEXTO-BOT                PIC  X(080) VALUE SPACES.
           05 TEXTO-TOP                PIC  X(080) VALUE SPACES.
           05 ATTRIB-n                 PIC  X(080) VALUE SPACES.
           05 ATTRIB-pb.
              06 ATTRIB-TOP            PIC  X(080) VALUE SPACES.
              06 ATTRIB-MID            PIC  X(080) VALUE SPACES.
              06 REDEFINES ATTRIB-MID.
                 15 MID-X OCCURS 80    PIC  9(002) COMP-X.
              06 ATTRIB-BOT            PIC  X(080) VALUE SPACES.
           05 REDEFINES ATTRIB-pb.
              06 dis-X OCCURS 240   PIC  9(002) COMP-X.
           05 BOXF-POSITION.
              10 BOXF-ROW              PIC  9(002) COMP-X VALUE 0.
              10 BOXF-COLUMN           PIC  9(002) COMP-X VALUE 0.

       01  TECLA          PIC 9(003) VALUE 0. COPY CWKEYS.
       01  MIL     COMP-X PIC 9(008) VALUE 1000.
       COPY CWGETF.
       COPY CWSEND.
       COPY CWSETK.
       COPY CWACOR.
       COPY CWSETF.
            88 NOBMS VALUE 'U' 'P'  'H' 'S' 'B' 'R' 'h' 'b' 'r' 's' 'X'.
       COPY CWGETL.
       COPY CWUNIX.
       COPY CWBOXF.
       COPY CWOBJE.
       01  CWOBJE-FUNCTION.
           05                             PIC  X(001).
              88 CWOBJE-GET                          VALUE "g" "G".
              88 CWOBJE-OCCURS                       VALUE "o" "O".
           05 CWOBJE-OCCURS-NUMBER COMP-X PIC  9(004).
       COPY CONSOLE.

       01  TABELA-HEX.
           05 FILLER                         PIC  X(055) VALUE
              "000102030405060708090A0B0C0D0E0F101112131415161718191A1".
           05 FILLER                         PIC  X(054) VALUE
              "B1C1D1E1F202122232425262728292A2B2C2D2E2F3031323334353".
           05 FILLER                         PIC  X(054) VALUE
              "63738393A3B3C3D3E3F404142434445464748494A4B4C4D4E4F505".
           05 FILLER                         PIC  X(054) VALUE
              "152535455565758595A5B5C5D5E5F606162636465666768696A6B6".
           05 FILLER                         PIC  X(039) VALUE
              "C6D6E6F707172737475767778797A7B7C7D7E7F".
           05 FILLER                         PIC  X(055) VALUE
              "808182838485868788898A8B8C8D8E8F909192939495969798999A9".
           05 FILLER                         PIC  X(054) VALUE
              "B9C9D9E9FA0A1A2A3A4A5A6A7A8A9AAABACADAEAFB0B1B2B3B4B5B".
           05 FILLER                         PIC  X(054) VALUE
              "6B7B8B9BABBBCBDBEBFC0C1C2C3C4C5C6C7C8C9CACBCCCDCECFD0D".
           05 FILLER                         PIC  X(054) VALUE
              "1D2D3D4D5D6D7D8D9DADBDCDDDEDFE0E1E2E3E4E5E6E7E8E9EAEBE".
           05 FILLER                         PIC  X(039) VALUE
              "CEDEEEFF0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF".
       01  REDEFINES TABELA-HEX.
           10 CHAR-HEX PIC XX OCCURS 255.

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01                          PIC  X(080) VALUE ALL "e".
       02  LINHA-02.
           05 FILLER                         PIC  X(055) VALUE
              "effffffffffffffffffffffffffffffffffffffpppppppppppppppp".
           05 FILLER                         PIC  X(025) VALUE
              "pppppppprgggshhhhhhhhhhte".
       02  LINHA-03.
           05 FILLER                         PIC  X(055) VALUE
              "eiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiqqqqqqqqqqqqqqqq".
           05 FILLER                         PIC  X(025) VALUE
              "qqqqqqqqqqqqqqujjjjjjjjve".
       02  LINHA-04                          PIC  X(080) VALUE ALL "e".
       02  LINHA-05.
           05 FILLER                         PIC  X(055) VALUE
              "ekkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkllllllllllllllll".
           05 FILLER                         PIC  X(025) VALUE
              "lllllllllllllllllllllllle".
       02  LINHA-06                          PIC  X(080) VALUE ALL "e".
       02  LINHA-07.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-08.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-09.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-10.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-11.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-12.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-13.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-14.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-15.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-16.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-17.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-18.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-19.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-20.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-21.
           05 FILLER                         PIC  X(055) VALUE
              "emmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm".
           05 FILLER                         PIC  X(025) VALUE
              "mmmmmmmmmmmmmmmmmmmmmmmme".
       02  LINHA-22                          PIC  X(080) VALUE ALL "e".
       02  LINHA-23.
           05 FILLER                         PIC  X(055) VALUE
              "ennnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn".
           05 FILLER                         PIC  X(025) VALUE
              "nnnnnnnnnnnnnnnnwooooooxe".
       02  LINHA-24                          PIC  X(080) VALUE ALL "e".

       01  REDEFINES LINHAS-DE-IMPRESSAO-CLIC.
           05 CLASSE-C OCCURS 1920 PIC 9(002) COMP-X.

       LINKAGE SECTION.

       01  FUNCAO.
           05 COMANDO  PIC X(001).
              88 FUN-DISPLAY     VALUE "D" "d".
              88 FUN-ACCEPT      VALUE "A" "a".
              88 FUN-POSIT       VALUE "P" "p".
              88 FUN-CLEAR       VALUE X"00".
              88 FUN-DIM         VALUE "W" "w".
           05 PROGRAMA PIC X(008).
           05 DP       PIC X(001).
       01  MATRIZ.
           05 MATRIZ-E OCCURS 2000.
              10 POS-LK.
                 15 LIN-LK      PIC 9(002).
                 15 COL-LK      PIC 9(002).
              10 LEN-LK         PIC 9(002).
              10 ATTR-LK.
                 15 MODE-LK     PIC X(001).
                    88 ACCEPT-LK        VALUE "U" "u" "T" "t" "P".
                 15 FB-LK       PIC X(002).
                 15 SECURE-LK   PIC X(001).
                 15 BLINK-LK    PIC X(001).
                 15 BZERO-LK    PIC X(001).
                    88 FLOAT-LK         VALUE "E".
                 15 EMPTY-LK    PIC X(001).
                 15 BEEP-LK     PIC X(001).
                 15 REVERSE-LK  PIC X(001).
                 15 AUTO-LK     PIC X(001).
                 15 HIGH-LK     PIC X(001).
                 15 UPPLOW-LK   PIC X(001).
                 15 ADVANCE-LK  PIC X(001).
                 15 LENF-LK     PIC 9(002).
                 15 FILLER      PIC X(003).
              10 FILLER         PIC X(013).
              10 ATTR-INIT-LK   PIC X(001).
              10 COM-SINAL-LK   PIC X(001).
              10 MODO-ACCEPT-LK PIC X(001).
                 88 VER-MODO-ACCEPT-LK      VALUE SPACES.
                 88 NUMERICO-LK             VALUE "N" "/".
                 88 NUMERICO-BARRA-LK       VALUE "/".
                 88 BARRA-LK                VALUE "B" "b".
              10 PIC-LK         PIC X(080).
              10 DATANAME-LK    PIC X(030).
              10 DATA-LK        PIC X(080).
              10 CTRL-LK        PIC X(080).
       01  TAMANHO-MATRIZ PIC 9(008) COMP-X.
       01  TIMEOUT-SEGUNDOS.
           05 TECLA-LK      PIC 9(003).
           05 FILLER        PIC X(015).
       01  TIMEOUT-TAMANHO  PIC 9(008) COMP-X.
       01  THUMB            PIC 9(005).

       PROCEDURE DIVISION USING FUNCAO
                                MATRIZ
                                TAMANHO-MATRIZ
                                TIMEOUT-SEGUNDOS
                                TIMEOUT-TAMANHO.
       000-INICIO.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           IF   X91-PARAMETER = 0
                GOBACK
           ELSE
                IF  FUN-CLEAR
                    PERFORM UNTIL scrolls < 1
                       MOVE WINWORK-WINDOW TO THUMBS-WINDOW
                       MOVE scrolls        TO THUMBS-ID
                       READ THUMBS
                       MOVE THUMBS-INITIAL TO THUMBS-POSITION
                       MOVE 0              TO T
                       IF THUMBS-TYPE = 'v'
                          ADD  2              TO THUMBS-FULL
                       ELSE
                          ADD  3              TO THUMBS-FULL
                       END-IF
                       PERFORM THUMBS-FULL TIMES
                               ADD 1 TO T
                               CALL "CBL_WRITE_SCR_CHATTRS"
                                                 USING THUMBS-POSITION
                                                       THUMBS-CHA(T:1)
                                                       THUMBS-ATT(T:1)
                                                       X"0001"
                               COMPUTE X = THUMBS-ROW + 1
                               COMPUTE Y = THUMBS-COL + 1
                               MOVE ZERO TO scroll-col(X Y)
                               IF THUMBS-TYPE = 'v'
                                  ADD  1  TO THUMBS-ROW
                               ELSE
                                  ADD  1  TO THUMBS-COL
                               END-IF
                       END-PERFORM
                       DELETE THUMBS RECORD
                       SUBTRACT 1 from scrolls
                    END-PERFORM
                    PERFORM 300-CLEAR-OBJECT THRU 300-99-FIM
                END-IF
                IF   FUN-POSIT
                     MOVE PROGRAMA (1: 4) TO DINAMICA(1:4)
                     SUBTRACT 1 FROM DINLIN
                                     DINCOL
                     MOVE 0 TO AVANCO-PENDENTE
                     GOBACK
                END-IF
                IF   FUN-DIM
frango*              CALL "CWOBJE" USING FUNCAO(1:11) *> vaza um mesmo
                     MOVE PROGRAMA(1:10) TO JANELA *> vaza um mesmo
                     IF JANLIN > 1
                        SUBTRACT 1  FROM JANLIN
                     END-IF
                     IF JANCOL > 1
                        SUBTRACT 1 FROM JANCOL
                     END-iF
                     IF POP-WINDOW = 7
                        MOVE 80 TO JANMAX
                        MOVE 25 TO JANLIMAX
CEDAE                   MOVE '00' TO JANPOS
                     ELSE
                        COMPUTE JANMAX   = JANCOL + JANWID - 1
                        COMPUTE JANLIMAX = JANLIN + JANHEI - 1
                     END-IF
      *              MOVE 0 TO AVANCO-PENDENTE
                     GOBACK
                END-IF
                IF  X91-PARAMETER < 2
                    GOBACK
                END-IF
                MOVE 01 TO TIMEOUT-RETURN
                IF  X91-PARAMETER = 5
                    MOVE 0  TO WS-TIMEOUT
                    MOVE 18 TO Y
                    PERFORM VARYING I FROM TIMEOUT-TAMANHO BY -1
                              UNTIL I = 0
                            IF TIMEOUT-SEGUNDOS (I: 1) NOT = LOW-VALUES
                               MOVE TIMEOUT-SEGUNDOS (I: 1)
                                 TO WS-TIMEOUT       (Y: 1)
                                 SUBTRACT 1 FROM Y
                            END-IF
                    END-PERFORM
                    IF   WS-TIMEOUT NOT = 0
                         MOVE 98 TO TIMEOUT-RETURN
                    ELSE
                         MOVE 3 TO X91-PARAMETER
                    END-IF
                END-IF
           END-IF
           CALL "CWGETL" USING PARAMETROS-CWGETL
           CALL "CWACOR" USING PARAMETROS-CWACOR
           ON 1
              DISPLAY "CONSOLE" UPON ENVIRONMENT-NAME
              ACCEPT  CWCONSOLE   FROM ENVIRONMENT-VALUE
              INSPECT CWCONSOLE
                   CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWFOREBACK" UPON ENVIRONMENT-NAME
              ACCEPT  FOREBACK   FROM ENVIRONMENT-VALUE
              INSPECT FOREBACK
                   CONVERTING MINUSCULAS TO MAIUSCULAS
              INSPECT CBLS CONVERTING '-' TO '_'
              DISPLAY "CWCRON" UPON ENVIRONMENT-NAME
              ACCEPT  CRON   FROM ENVIRONMENT-VALUE
              INSPECT CRON
                   CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWENTER-TERMINATE" UPON ENVIRONMENT-NAME
              ACCEPT  ENTER-TERMINATE   FROM ENVIRONMENT-VALUE
              INSPECT ENTER-TERMINATE
                   CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWBACKDEFAULT" UPON ENVIRONMENT-NAME
              ACCEPT  CWBACKDEFAULT   FROM ENVIRONMENT-VALUE
              INSPECT CWENDK    CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWENDK"        UPON ENVIRONMENT-NAME
              ACCEPT  CWENDK          FROM ENVIRONMENT-VALUE
              INSPECT CWENDK    CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWBEEP"        UPON ENVIRONMENT-NAME
              ACCEPT  CWBEEP          FROM ENVIRONMENT-VALUE
              INSPECT CWBEEP    CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWAUTO"        UPON ENVIRONMENT-NAME
              ACCEPT  CWAUTO          FROM ENVIRONMENT-VALUE
              INSPECT CWAUTO    CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWLOGT"        UPON ENVIRONMENT-NAME
              ACCEPT  CWLOGT          FROM ENVIRONMENT-VALUE
              INSPECT CWLOGT    CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWUPDATE"      UPON ENVIRONMENT-NAME
              ACCEPT  CWUPDATE        FROM ENVIRONMENT-VALUE
              INSPECT CWUPDATE  CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWZEROSUPRESS" UPON ENVIRONMENT-NAME
              ACCEPT  ZEROSUPRESS     FROM ENVIRONMENT-VALUE
              INSPECT ZEROSUPRESS CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWSECURECHAR"  UPON ENVIRONMENT-NAME
              ACCEPT  SECURECHAR      FROM ENVIRONMENT-VALUE
              MOVE SECURECHAR TO SECUREMODE
              INSPECT SECUREMODE  CONVERTING MINUSCULAS TO MAIUSCULAS
              IF      SECUREMODE  = 'NULL' OR 'OFF' OR 'NONE'
                      MOVE 'OFF'  TO SECUREMODE
                      MOVE SPACES TO SECURECHAR
              END-IF
              INSPECT SECURECHAR  CONVERTING SPACES TO SECURECHAR(1:1)
              OPEN I-O WINWORK
              INITIALIZE WINWORK-REG
              CALL "CBL_GET_CSR_POS"  USING CURSOR-POSITION
              COMPUTE CURPOS-LIN = CURSOR-ROW    + 1
              COMPUTE CURPOS-COL = CURSOR-COLUMN + 1
              OPEN I-O OBJETOS
              IF   FS-OBJETOS > "09"
                   CALL "CWISAM" USING ER-OBJETOS
              END-IF
              OPEN I-O HOTS
              IF   FS-HOTS > "09"
                   CALL "CWISAM" USING ER-HOTS
              END-IF
              DISPLAY "ECHOCURSOR" UPON ENVIRONMENT-NAME
              ACCEPT ECHOCURSOR FROM ENVIRONMENT-VALUE
              INSPECT ECHOCURSOR CONVERTING "ofn" TO "OFN"
              DISPLAY "ECHODELETED" UPON ENVIRONMENT-NAME
              ACCEPT ECHODELETED FROM ENVIRONMENT-VALUE
              INSPECT ECHODELETED CONVERTING "ofn" TO "OFN"
              DISPLAY "CWCASE" UPON ENVIRONMENT-NAME
              ACCEPT CWCASE FROM ENVIRONMENT-VALUE
              INSPECT CWCASE CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWLITS" UPON ENVIRONMENT-NAME
              ACCEPT CWLITS FROM ENVIRONMENT-VALUE
              INSPECT CWLITS CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWACCENT" UPON ENVIRONMENT-NAME
              ACCEPT CWACCENT FROM ENVIRONMENT-VALUE
              INSPECT CWACCENT CONVERTING MINUSCULAS TO MAIUSCULAS
              CALL "CWUNIX" USING PARAMETROS-CWUNIX
              IF   CWUNIX-ON
                   MOVE "+"    TO DESCE
                   MOVE 1      TO MULTI-USER
                   MOVE '^v><' TO ARROWS
              END-IF
              CALL "CWMOLD" USING TABELA-CORES TABELA-MOLDURA
              MOVE COR (049) TO ATT-0
              MOVE COR (113) TO ATT-P
              MOVE COR (128) TO INTENSO
              MOVE BASE-MOLDURA (2) TO MOLDURA
              INSPECT MM-205 CONVERTING SPACE TO M-205
              INSPECT ATTR-LOW  CONVERTING X"00" TO COR (113)
              INSPECT ATTR-HIGH CONVERTING X"00" TO COR (128)
              DISPLAY "CWMULTI"      UPON ENVIRONMENT-NAME
              ACCEPT  CWMULTI        FROM ENVIRONMENT-VALUE
              INSPECT CWMULTI (1: 2) CONVERTING MINUSCULAS TO MAIUSCULAS
              IF   CWMULTI (1: 2) = "ON"
                   MOVE 1 TO MULTI-USER
              END-IF
              DISPLAY "CWEDITNUM"    UPON ENVIRONMENT-NAME
              ACCEPT  EDITNUM        FROM ENVIRONMENT-VALUE
              INSPECT EDITNUM  CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWEDITCHAR"   UPON ENVIRONMENT-NAME
              ACCEPT  EDITCHAR       FROM ENVIRONMENT-VALUE
              INSPECT EDITCHAR CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWJUST"       UPON ENVIRONMENT-NAME
              ACCEPT  EDITJUST       FROM ENVIRONMENT-VALUE
              INSPECT EDITJUST CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWENTRY"      UPON ENVIRONMENT-NAME
              ACCEPT  CWENTRY       FROM ENVIRONMENT-VALUE
              INSPECT CWENTRY CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWDISPLAY-BARR" UPON ENVIRONMENT-NAME
              ACCEPT  BARR    FROM ENVIRONMENT-VALUE
              INSPECT BARR     CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWATTRIBUTE" UPON ENVIRONMENT-NAME
              PERFORM AJUSTA
              IF  NUMERO NOT = 0
                  MOVE NUMERO TO MB-ATTRIB-N.

           IF   X91-PARAMETER < 3 OR CRON = 'ON'
                GOBACK
           END-IF

           IF POP-WINDOW NOT = WINWORK-WINDOW
              IF POP-WINDOW > WINWORK-WINDOW
                 IF WINWORK-WINDOW NOT = 0
                    REWRITE WINWORK-REG
                 END-IF
                 MOVE POP-WINDOW TO WINWORK-WINDOW
                 INITIALIZE WINWORK-DATA
                 MOVE JANELA(1:4) TO CURPOS
                 IF POP-WINDOW > 7
                    ADD 1 TO CURPOS-LIN
                 END-IF
                 WRITE WINWORK-REG
              ELSE
                 DELETE WINWORK RECORD
                 MOVE POP-WINDOW TO WINWORK-WINDOW
                 READ WINWORK
              END-IF
              DISPLAY 'WINWORK'     UPON ENVIRONMENT-NAME
              DISPLAY WINWORK-CHAVE UPON ENVIRONMENT-VALUE
              DISPLAY 'WINPOS'      UPON ENVIRONMENT-NAME
              DISPLAY JANPOS        UPON ENVIRONMENT-VALUE
           END-IF

           IF   CWGETL-MOUSE = 1
                MOVE 5 TO CWGETL-MOUSE
                CALL "CBL_INIT_MOUSE" USING MOUSE-HANDLE
                                            MOUSE-BUTTONS
                     ON OVERFLOW
                        CONTINUE
                END-CALL
                CALL "CBL_GET_MOUSE_POSITION"
                      USING MOUSE-HANDLE
                            MOUSE-POSITION
                  ON   OVERFLOW
                       CONTINUE
                END-CALL
                MOVE MOUSE-POSITION   TO MOUSE-POSITION-A
                                         MOUSE-POSITION-S
           END-IF

           add 1 to d-num
           COMPUTE FIELDS = TAMANHO-MATRIZ / LENGTH OF MATRIZ-E
           COMPUTE TAMANHO-CHECK = LENGTH OF MATRIZ-E(1) * FIELDS
           IF  TAMANHO-CHECK NOT = TAMANHO-MATRIZ
               MOVE SPACES TO CWSEND-MSG CWSEND-SCREENS
               MOVE "Tela inconsistente" TO CWSEND-MSG
               CALL "CWSEND" USING PARAMETROS-CWSEND
               GOBACK
           END-IF
           IF  CWLOGT = "ON"
               CALL "CWLOGT" USING FUNCAO
                                   MATRIZ
                                   TAMANHO-MATRIZ
           END-IF

           IF   CWCONSOLE = 'ON'
           AND  POS-LK (1) = '0000'
                PERFORM 900-CONSOLE THRU 900-99-FIM
                GOBACK
           END-IF

           MOVE LOW-VALUES TO ATTRIBUTES-ACCEPT
                              ATTRIBUTES-ACCEPT2
           INITIALIZE MOUSE-MAP FIELD-ANT FLAGS-SA BOXF-OFF ACESO
                      FOCUS CRITICA-FIELD GUIA-REG HIDE-REG
           CALL "CWATCH"
           MOVE 0 TO FIELD-LAST FROM-LEFT SEGUNDOS
           CLOSE GUIA HIDE GUIA-O
           DELETE FILE GUIA-O
           DELETE FILE GUIA
           DELETE FILE HIDE
           OPEN I-O GUIA GUIA-O HIDE
           IF   FS-GUIA > "09"
                CALL "CWISAM" USING ER-GUIA
           END-IF
           IF   FS-GUIA-O > "09"
                CALL "CWISAM" USING ER-GUIA-O
           END-IF
           IF   FS-HIDE > "09"
                CALL "CWISAM" USING ER-HIDE
           END-IF
           INITIALIZE GUIA-REG
           MOVE 1 TO SAVE-GUIA
                     DEFOBJ
           IF  FUN-ACCEPT
               CALL "CWSETK" USING PARAMETROS-CWSETK "Get"
               CANCEL "CWSETK"
               open i-o buttons
               PERFORM 004-BMS-ATTR THRU 004-99-FIM
           END-IF

           IF PROGRAMA = 'CWBOXD'
              MOVE 'CWOBJD' TO CWOBJE
           ELSE
              MOVE 'CWOBJE' TO CWOBJE
           END-IF

           MOVE 0 TO OBJECTS
                     FIELD-BEEP
                     FIELD
           MOVE 2 TO GT
           MOVE 0 TO GTS

           IF  FUN-ACCEPT
               MOVE "0" TO COMBO-DISPLAY
               MOVE "D" TO DF
           ELSE
               MOVE "1" TO COMBO-DISPLAY
               MOVE "d" TO DF
           END-IF

           PERFORM 300-CLEAR-OBJECT THRU 300-99-FIM
           PERFORM 270-DEFINE-OBJECTS THRU 270-99-FIM
           INITIALIZE PARAMETROS-CWOBJE GUIA-POP
           MOVE 1 TO GT
           MOVE 0 TO GTS
           MOVE 1 TO CWCONTROL
           CALL "CWATTW" USING "G" cwboxw-attr WINWORK-CHAVE
           MOVE 0 TO MULTILINE FLOAT-FIND
           PERFORM VARYING FIELD FROM 1 BY 1 UNTIL FIELD > FIELDS
                   IF   DATANAME-LK (FIELD) NOT = SPACES
                        IF   LENF-LK (FIELD) NUMERIC
                        AND  LENF-LK (FIELD) > LEN-LK (FIELD)
                             MOVE LENF-LK (FIELD) TO LEN-E
                        ELSE
                             MOVE LEN-LK (FIELD) TO LEN-E
                        END-IF
                        ADD  1      TO LEN-E
                        MOVE SPACES TO DATA-LK (FIELD) (LEN-E:)
                        IF   VER-MODO-ACCEPT-LK (FIELD)
                             INSPECT DATANAME-LK (FIELD)
                                     CONVERTING MINUSCULAS TO MAIUSCULAS
                        END-IF
                   END-IF
                   IF  FLOAT-LK (FIELD)
                       EXEC COBOLware Float-Point
                            NUMERIC DATA-LK(FIELD)
                               INTO FLOAT-ED
                            DECIMAL-POINT DP
                       END-EXEC
                       MOVE 1                   TO FLOAT-FIND
                       MOVE 30                  TO LENF-LK(FIELD)
                       MOVE FLOAT-ED            TO DATA-LK(FIELD)(1:30)
                       MOVE PIC-LK(FIELD)(1:30) TO PIC-LK(FIELD)(51:30)
                       MOVE ALL 'X'             TO PIC-LK(FIELD)(1:30)
                   END-IF
                   IF (POS-LK (1) = "0101"
                   AND DATANAME-LK (FIELD)(1:5) = 'SPACE'
                   AND ADVANCE-LK(FIELD) =  'a')
                   OR (POS-LK (FIELD) = "0000"
                   AND DATANAME-LK (FIELD) = SPACES
                   AND ADVANCE-LK(FIELD) =  'A')
                   AND FIELD > 1
                       ADD 1 TO MULTILINE
                   END-IF
           END-PERFORM
           MOVE 0 TO ACCEPTs
           PERFORM VARYING FIELD FROM 1 BY 1 UNTIL FIELD > FIELDS
                   MOVE POS-LK(FIELD) TO JANSAVE(FIELD)
                   IF LIN-LK (FIELD) NUMERIC
                      ADD DINLIN TO LIN-LK (FIELD)
                      IF LIN-LK (FIELD) NOT = 0
                         ADD JANLIN TO LIN-LK (FIELD)
                      END-IF
                   END-IF
                   IF  COL-LK (FIELD) NUMERIC
                       ADD DINCOL TO COL-LK (FIELD)
                       IF COL-LK (FIELD) NOT = 0
                          ADD JANCOL TO COL-LK (FIELD)
                       END-IF
                   END-IF
                   IF  AVANCO-PENDENTE = 1
                       IF  JANSAVE(FIELD) (1:2) = '00'
                       AND (JANSAVE(FIELD) (3:2) NOT = '00')
                            PERFORM 013-AVANCO
                       ELSE
                            MOVE 0 TO AVANCO-PENDENTE
                       END-IF
                   END-IF
                   IF  PIC-LK  (FIELD) (1:1) = "Z"
                   AND DATA-LK (FIELD) (1:1) = "0"
                   AND (DATANAME-LK (FIELD) NOT = SPACE)
                        MOVE MATRIZ-E (FIELD) TO ELEMENTO
                        MOVE 0 TO SAVE-GUIA
                        PERFORM 070-EDIT   THRU 070-99-FIM
                        MOVE 1 TO SAVE-GUIA
                   END-IF
                   PERFORM 010-EXIBIR THRU 010-99-FIM
           END-PERFORM
           MOVE 0 TO CWCONTROL
           MOVE 0 TO DEFOBJ GT
           MOVE 0 TO SAVE-GUIA
           IF  FUN-ACCEPT
               CANCEL "CWCRTS"
               CANCEL "CWAKEY"
               MOVE    0              TO SAVE-EDIT ERRO
               PERFORM 350-BOT-GUIA THRU 350-99-FIM
               MOVE GUIA-FIELD        TO FIELD-LAST
      *        SET  EDIT-CURSOR-DOWN  TO TRUE
               SET  EDIT-enter        TO TRUE
               MOVE 0                 TO TECLA
               IF ACCEPTs = 0
                  MOVE 0 TO FOCUS
                  CLOSE GUIA
                  OPEN I-O GUIA
                  MOVE LOW-VALUES TO GUIA-O-REG
                  START GUIA-O KEY NOT LESS GUIA-O-CHAVE
                  PERFORM UNTIL FS-GUIA-O > '09'
                          READ GUIA-O NEXT RECORD
                          IF FS-GUIA-O < '10'
                             WRITE GUIA-REG FROM GUIA-O-REG
                          END-IF
                  END-PERFORM
                  MOVE LOW-VALUES TO GUIA-REG
                  START GUIA KEY NOT LESS GUIA-CHAVE
                  READ GUIA NEXT RECORD
               ELSE
                  PERFORM 340-TOP-GUIA THRU 340-99-FIM
               END-IF
               PERFORM 020-RECEBER  THRU 020-99-FIM
                       UNTIL FS-GUIA > "09" AND ERRO = 0
               IF   X91-PARAMETER = 4
               AND  objeto = 0
                    MOVE TECLA TO TECLA-LK
                    IF   TIMEOUT-RETURN = 77
                         MOVE 01 TO TIMEOUT-RETURN
                         MOVE 98 TO TECLA-LK TECLA
                    END-IF
               END-IF
           END-IF
           PERFORM VARYING FIELD FROM 1 BY 1 UNTIL FIELD > FIELDS
                   IF  (BZERO-LK (FIELD) (1: 1) = "R" OR "r")
                   AND  PIC-LK (FIELD) (1: 1) = "X"
                   AND (DATA-LK (FIELD) NOT = SPACES)
                        MOVE DATA-LK (FIELD) TO BUF
                        MOVE SPACES          TO DATA-LK (FIELD)
                        PERFORM VARYING I FROM LEN-LK (FIELD)
                                            BY -1
                                         UNTIL BUF (I: 1) NOT = SPACE
                                            OR I = 0
                                CONTINUE
                        END-PERFORM
                        IF I NOT = 0
                           COMPUTE P = LEN-LK (FIELD) - I + 1
                           MOVE BUF (1: I) TO DATA-LK (FIELD) (P: I)
                           IF ACC-PIC (FIELD) NOT = LOW-VALUES
                              MOVE ACC-PIC (FIELD) TO PIC-LK (FIELD)
                           END-IF
                        END-IF
                   ELSE
                        IF  PIC-LK  (FIELD) (1:1) = "Z"
                        AND DATA-LK (FIELD) (1:1) = "0"
                        AND (DATANAME-LK (FIELD) NOT = SPACE)
                             MOVE MATRIZ-E (FIELD) TO ELEMENTO
                             PERFORM 070-EDIT   THRU 070-99-FIM
                             PERFORM 010-EXIBIR THRU 010-99-FIM
                        END-IF
                   END-IF
               IF  ATA (FIELD) NOT = LOW-VALUES
                   MOVE ACC-LIN (FIELD) TO ROW-NUMBER
                   MOVE ACC-COL (FIELD) TO COLUMN-NUMBER
                   MOVE ACC-LEN (FIELD) TO STRING-LENGTH
                   MOVE DATANAME-LK (FIELD) TO HIDE-CHAVE
                   READ HIDE
                   IF  FS-HIDE > '09'
                   OR  FS-HIDE = '00' AND HIDE-COMBO = 1
                       IF FS-HIDE = '00' AND HIDE-COMBO = 1
                          MOVE HIDE-LENGTH TO STRING-LENGTH2
                                              STRING-LENGTH
                          ADD 1 TO STRING-LENGTH2 STRING-LENGTH
                          MOVE SCREEN-POSITION TO SCREEN-POSITION2
                          IF   NUMERICO-LK (FIELD)
                          AND  DATA-LK (FIELD) (1:1) = "0"
                               MOVE MATRIZ-E (FIELD) TO ELEMENTO
                               PERFORM 070-EDIT THRU 070-99-FIM
                          END-IF
                          CALL "CBL_WRITE_SCR_CHATTRS"
                               USING SCREEN-POSITION2
                                     DATA-LK(FIELD)
                                     ACC-PIC(FIELD)
                                     STRING-LENGTH2
                          MOVE STRING-LENGTH2 TO POP
                          MOVE 1              TO STRING-LENGTH2
                          ADD  POP            TO COLUMN-NUMBER2
                          SUBTRACT 1 FROM COLUMN-NUMBER2
                          CALL "CBL_WRITE_SCR_CHATTRS"
                               USING SCREEN-POSITION2
                                     ACC-POP-CHAR (FIELD) (1: 1)
                                     ACC-POP      (FIELD) (1: 1)
                                     STRING-LENGTH2
                          MOVE     POP   TO STRING-LENGTH2
                          SUBTRACT POP FROM COLUMN-NUMBER2
                       END-IF
                       CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION
                                                        ATTRIBUTE-ACCEPT
                                                        (FIELD)
                                                        STRING-LENGTH
      *                IF   ACC-POP-CHAR (FIELD) NOT = 0
      *                     MOVE SCREEN-POSITION TO SCREEN-POSITION3
      *                     ADD  STRING-LENGTH   TO COLUMN-NUMBER3
      *                     CALL "CBL_WRITE_SCR_CHATTRS"
      *                        USING SCREEN-POSITION3
      *                              ACC-POP-CHAR (FIELD) (1: 1)
      *                              ACC-POP      (FIELD) (1: 1)
      *                              X"0001"
      *                END-IF
                   END-IF
                   IF   LENF-LK (FIELD) NUMERIC
                   AND  LENF-LK (FIELD) > LEN-LK (FIELD)
                   AND  PIC-LK  (FIELD) (1: 1) = "9" OR "Z"
                        COMPUTE P = LENF-LK (FIELD) - LEN-LK (FIELD) + 1
                        MOVE LEN-LK  (FIELD) TO P2
                        MOVE DATA-LK (FIELD) TO BUF
                        MOVE SPACES          TO DATA-LK (FIELD)
                        MOVE BUF             TO DATA-LK (FIELD) (P: P2)
                   END-IF
                   MOVE 1 TO NAO-EXIBE-VERIFICA
                   IF   FUN-ACCEPT
                   AND  NUMERICO-LK (FIELD)
                        perform 080-VERIFICA THRU 080-99-FIM
                        MOVE SPACE TO TIROU
                        PERFORM VARYING I FROM 1 BY 1
                                UNTIL I > LEN-LK  (FIELD)
                           IF  (DATA-LK(FIELD) (I: 1) = SPACE
                           OR                           "-"
                           OR                           "+")
                           AND  PIC-LK (FIELD) (I: 1) = "9"
                                IF  DATA-LK(FIELD) (I: 1) NOT = SPACE
                                    MOVE DATA-LK(FIELD) (I: 1)
                                      TO TIROU
                                END-IF
                                MOVE "0" TO DATA-LK(FIELD) (I: 1)
                           END-IF
                        END-PERFORM
                        IF TIROU NOT = SPACE
                           PERFORM VARYING I FROM LEN-LK(FIELD) BY -1
                                   UNTIL I = 0
                                   IF DATA-LK(FIELD) (I: 1) = SPACE
                                   AND (PIC-E (I: 1) = '-' OR '+')
                                      MOVE TIROU TO DATA-LK(FIELD)(I: 1)
                                      MOVE SPACE TO TIROU
                                   END-IF
                           END-PERFORM
                        END-IF
                        IF DATA-LK(FIELD) NOT = DATA-E
                           PERFORM 010-EXIBIR THRU 010-99-FIM
                        END-IF
                   END-IF
                   MOVE 0 TO NAO-EXIBE-VERIFICA
               END-IF
           END-PERFORM
           IF (ATTRIBUTES-ACCEPT NOT = LOW-VALUES)
               PERFORM 350-BOT-GUIA THRU 350-99-FIM
               IF FS-GUIA < '10'
               AND GUIA-FIELD > 0
                   MOVE GUIA-FIELD       TO FIELD
                   MOVE MATRIZ-E (FIELD) TO ELEMENTO
                   PERFORM 410-CHECK-PLUS THRU 410-99-FIM
                   PERFORM 013-CHECK-AVANCO THRU 013-99-FIM
               END-IF
           END-IF
           IF FUN-ACCEPT
              MOVE LOW-VALUES TO SAVEATTR-REG
              START SAVEATTR KEY NOT LESS SAVEATTR-FIELD
              PERFORM TEST AFTER UNTIL FS-SAVEATTR > "09"
                 READ SAVEATTR NEXT RECORD
                 IF FS-SAVEATTR < "10"
                    MOVE SAVEATTR-ATRR
                      TO ATTR-LK(SAVEATTR-FIELD)
                    MOVE SAVEATTR-MODO-ACCEPT
                      TO MODO-ACCEPT-LK(SAVEATTR-FIELD)
                    MOVE SAVEATTR-PIC
                      TO PIC-LK(SAVEATTR-FIELD)
                    DELETE SAVEATTR RECORD
                 END-IF
              END-PERFORM
              CLOSE SAVEATTR
              MOVE LOW-VALUES TO BUTTONS-REG
              START BUTTONS KEY NOT LESS BUTTONS-CHAVE
              PERFORM TEST AFTER UNTIL FS-BUTTONS > "09"
                 READ BUTTONS NEXT RECORD
                 IF FS-BUTTONS < "10"
                    PERFORM VARYING e FROM 1 BY 1 UNTIL e > 80
                            add 16 to BUTTONS-ATT (E)
                    END-PERFORM
                    CALL "CBL_WRITE_SCR_CHATTRS" USING BUTTONS-CHAVE
                                                       BUTTONS-TEXTO
                                                       BUTTONS-ATTRIB
                                                       BUTTONS-LENGTH
                 END-IF
              END-PERFORM
              CLOSE BUTTONS
           END-IF
           CLOSE GUIA HIDE GUIA-O
           DELETE FILE GUIA-O
           DELETE FILE GUIA
           DELETE FILE HIDE
           IF FLOAT-FIND NOT = 0
              PERFORM VARYING FIELD FROM 1 BY 1 UNTIL FIELD > FIELDS
                   IF  FLOAT-LK (FIELD)
                       MOVE DATA-LK(FIELD)(1:30) TO FLOAT-ED
                       MOVE PIC-LK(FIELD)(51:30) TO PIC-LK(FIELD)(1:30)
                                                    DATA-LK(FIELD)
                       MOVE SPACES TO PIC-LK(FIELD)(51:30)
                       EXEC COBOLware Float-Point
                            NUMERIC (DATA-LK(FIELD))
                               FROM FLOAT-ED
                            DECIMAL-POINT DP
                       END-EXEC
                   END-IF
              END-PERFORM
           END-IF
           IF JANELA (1:4) NOT = '0000'
              PERFORM VARYING FIELD FROM 1 BY 1 UNTIL FIELD > FIELDS
                      MOVE JANSAVE (FIELD) TO POS-LK (FIELD)
              END-PERFORM
           END-IF
           IF DINAMICA NOT = ZEROS
              PERFORM VARYING FIELD FROM 1 BY 1 UNTIL FIELD > FIELDS
                      SUBTRACT DINLIN FROM LIN-LK (FIELD)
                      SUBTRACT DINCOL FROM COL-LK (FIELD)
              END-PERFORM
              INITIALIZE DINAMICA
           END-IF.

       000-99-FIM. GOBACK.

       004-BMS-ATTR.

           open i-o SAVEATTR
           PERFORM TEST AFTER UNTIL CWSETF-FIELD = SPACES
              INITIALIZE PARAMETROS-CWSETF
              CALL "CWSETF" USING PARAMETROS-CWSETF
              IF CWSETF-FIELD NOT = SPACES
                 IF CWSETF-SUBSCRIPT > 0
                    SUBTRACT 1 FROM CWSETF-SUBSCRIPT
                 END-IF
                 PERFORM VARYING FIELD FROM 1 BY 1
                                  UNTIL FIELD > FIELDS
                    IF CWSETF-FIELD = DATANAME-LK (FIELD)
                       IF CWSETF-SUBSCRIPT = 0
                          IF CWSETF-OPTION = LOW-VALUE
                             MOVE FIELD TO FOCUS
                          ELSE
                             MOVE FIELD TO SAVEATTR-FIELD
                             READ SAVEATTR
                             IF FS-SAVEATTR = '23'
                                MOVE ATTR-LK        (FIELD)
                                  TO SAVEATTR-ATRR
                                move MODO-ACCEPT-LK (FIELD)
                                  TO SAVEATTR-MODO-ACCEPT
                                move PIC-LK         (FIELD)
                                  TO SAVEATTR-PIC
                                WRITE SAVEATTR-REG
                             END-IF
                             IF   CWSETF-FIXED
                                  MOVE 'V' TO MODE-LK (FIELD)
                             ELSE
                                  IF   CWSETF-PROTECT
                                       MOVE 'P' TO MODE-LK (FIELD)
                                  ELSE
                                       IF   CWSETF-UNPROTECT
                                            MOVE 'U' TO MODE-LK (FIELD)
                                       END-IF
                                  END-IF
                             END-IF
                             IF   CWSETF-HIGHLIGHT
                                  MOVE 'H' TO HIGH-LK (FIELD)
                             ELSE
                                  IF NOT NOBMS
                                  OR CWSETF-LOWLIGHT
                                     MOVE 'h' TO HIGH-LK (FIELD)
                                  END-IF
                             END-IF
                             IF   CWSETF-BLINK
                                  MOVE 'B' TO BLINK-LK (FIELD)
                             ELSE
                                  IF NOT NOBMS
                                  OR CWSETF-NOBLINK
                                     MOVE 'b' TO BLINK-LK (FIELD)
                                  END-IF
                             END-IF
                             IF   CWSETF-REVERSED
                                  MOVE 'R' TO REVERSE-LK(FIELD)
                             ELSE
                                  IF NOT NOBMS
                                  OR CWSETF-NOREVERSED
                                     MOVE 'r' TO REVERSE-LK(FIELD)
                                  END-IF
                             END-IF
                             IF   CWSETF-SECURE
                                  MOVE 'S' TO SECURE-LK(FIELD)
                             ELSE
                                  IF NOT NOBMS
                                  OR CWSETF-NOSECURE
                                     MOVE 's' TO SECURE-LK(FIELD)
                                  END-IF
                             END-IF
                             IF   CWSETF-NUMERIC
                                  MOVE SPACES TO PIC-LK(FIELD)
                                  MOVE 'N' TO MODO-ACCEPT-LK(FIELD)
                                  MOVE ALL "Z"
                                    TO PIC-LK(FIELD) (1: LEN-LK(FIELD))
                             ELSE
                                  IF NOT NOBMS
                                  OR CWSETF-NONUMERIC
                                     MOVE SPACES TO PIC-LK(FIELD)
                                     MOVE '?' TO MODO-ACCEPT-LK(FIELD)
                                     MOVE ALL "X"
                                     TO PIC-LK(FIELD) (1: LEN-LK(FIELD))
                                  END-IF
                             END-IF
                             IF   CWSETF-OPTION NUMERIC
                                  MOVE CWSETF-OPTION
                                    TO FB-LK(FIELD)(1:1)
                             END-IF
                          END-IF
                          EXIT PERFORM
                       ELSE
                          SUBTRACT 1 FROM CWSETF-SUBSCRIPT
                       END-IF
                    END-IF
                 END-PERFORM
              END-IF
           END-PERFORM.

       004-99-FIM. EXIT.

       010-EXIBIR.

           IF DATANAME-LK (FIELD) (1: 6) = "CWFONT"
              GO TO 010-99-FIM
           END-IF

           PERFORM UNTIL COL-LK (FIELD) NOT NUMERIC
                      OR COL-LK (FIELD) < 81
                   SUBTRACT 80 FROM COL-LK (FIELD)
                   IF  LIN-LK (FIELD) NUMERIC
                       ADD 1 TO LIN-LK (FIELD)
                   END-IF
           END-PERFORM

           IF LIN-LK (FIELD) NUMERIC
           AND LIN-LK (FIELD) > JANLIMAX
               PERFORM UNTIL LIN-LK (FIELD) NOT > JANLIMAX
      *           MOVE "2501"             TO CURPOS
                  MOVE JANLIMAX           TO CURPOS(1:2)
                  MOVE '01'               TO CURPOS(3:2)
                  MOVE FIELDS             TO FI-S
                  MOVE FIELD              TO FIELDS
                  MOVE COL-LK (FIELD)     TO CO-S
                  MOVE LIN-LK (FIELD)     TO LI-S
                  MOVE 0                  TO COL-LK (FIELD)
                  MOVE 0                  TO LIN-LK (FIELD)
                  MOVE "A"                TO ADVANCE-E
                  MOVE 80                 TO STRING-LENGTH
                  PERFORM 013-CHECK-AVANCO THRU 013-99-FIM
                  MOVE FI-S               TO FIELDS
                  MOVE LI-S TO LIN-LK (FIELD)
                  SUBTRACT 1 FROM LIN-LK (FIELD)
                  MOVE CO-S                TO COL-LK (FIELD)
               END-PERFORM
           END-IF

           IF  DATA-LK (FIELD) (1: 1) = X"0B"
               MOVE DATA-LK (FIELD) (2: 1) TO ATTRIBUTE-X
               IF DATA-LK (FIELD) (3: 1) < X'20'
                  COMPUTE MB-ATTRIB-N = (ATTRIBUTE - 1) * 16
                  MOVE DATA-LK (FIELD) (3: 1) TO ATTRIBUTE-X
                  subtract 1 from ATTRIBUTE
                  ADD ATTRIBUTE    TO MB-ATTRIB-N
                  MOVE 3 TO MB-SHIFT
               ELSE
                  MOVE ATTRIBUTE-X TO MB-ATTRIB
                  MOVE 2 TO MB-SHIFT
               END-IF
               IF  LIN-LK(FIELD) NUMERIC
               AND LIN-LK(FIELD) > 0
                   MOVE LIN-LK(FIELD) TO CURPOS-LIN
               END-IF
               IF  COL-LK(FIELD) NUMERIC
               AND COL-LK(FIELD) > 0
                   MOVE COL-LK(FIELD) TO CURPOS-COL
               END-IF
               IF ADVANCE-LK (FIELD) = 'S' OR 'L'
                  MOVE MATRIZ-E (FIELD) TO ELEMENTO
                  IF ADVANCE-LK (FIELD) = 'L'
                     SET ERASE-EOL TO TRUE
                  ELSE
                     SET ERASE-EOS TO TRUE
                  END-IF
                  GO TO EVA
               END-IF
               IF  DATA-LK (FIELD) (MB-SHIFT + 1: ) = SPACES
                   MOVE 0 TO MB-LEN
                   GO TO 010-MB-FIM
               ELSE
                   IF MB-SAVE = SPACES
                      MOVE MATRIZ-E (FIELD) TO MB-SAVE
                   END-IF
                   MOVE DATA-LK (FIELD) (MB-SHIFT + 1:) TO MB-STRING
                   MOVE ALL '0' TO POS-LK (FIELD)
                   IF MB-LEN = 0
                      COMPUTE MB-LEN = LEN-LK (FIELD) - MB-SHIFT
                   ELSE
                      SUBTRACT MB-SHIFT FROM MB-LEN
                   END-IF
               END-IF
           END-IF

           IF  MB-LEN > 0
               PERFORM VARYING I
                          FROM 1 BY 1
                         UNTIL I > MB-LEN
                           OR (MB-STRING (I: 1) = X"0B"
                             AND I > 1)
                     CONTINUE
               END-PERFORM
               SUBTRACT 1 FROM I
               MOVE MB-STRING (1: I) TO DATA-LK (FIELD)
               MOVE I                TO LEN-LK (FIELD)
               ADD 1 TO I
               IF I < MB-LEN
                  MOVE MB-STRING (I: )  TO WORK-ED
                  MOVE WORK-ED          TO MB-STRING
                  SUBTRACT I          FROM MB-LEN
                  ADD 1 TO MB-LEN
               ELSE
                  MOVE 0 TO MB-LEN
               END-IF
           END-IF

           IF  DATANAME-LK (FIELD) = SPACES
           AND FUN-ACCEPT
               go to 010-99-FIM
           END-IF

           IF  DATANAME-LK (FIELD) = "ALL"
           AND LEN-LK      (FIELD) = 80
               COMPUTE LEN-LK (FIELD) = 80
                     - COL-LK (FIELD) + 1
           END-IF
           IF MODO-ACCEPT-LK (FIELD) = SPACE
              MOVE DP TO DP-E
           ELSE
              MOVE ',' TO DP-E
           END-IF

           IF   CWCONTROL = 1
                IF   CWAUTO = 'ON'
                     MOVE "A" TO AUTO-E
                                 AUTO-LK (FIELD)
                END-IF
                IF   CWAUTO = 'OFF'
                     MOVE "a" TO AUTO-E
                                 AUTO-LK (FIELD)
                END-IF
                IF  PIC-LK (FIELD) (1: 1) = "Z"
                AND ACC-PIC (FIELD) = LOW-VALUES
                    PERFORM VARYING PZ FROM 1 BY 1
                            UNTIL PIC-LK (FIELD) (PZ: 1) = DP-E OR SPACE
                                  CONTINUE
                    END-PERFORM
                    IF   PZ > 1
                    AND  PIC-LK (FIELD) (PZ: 1) = DP-E
                         IF PIC-LK (FIELD) (PZ - 1: 1) = "Z"
                         OR PIC-LK (FIELD) (PZ + 1: 1) = "Z"
                            SUBTRACT 1 FROM PZ
                            MOVE PIC-LK (FIELD) TO ACC-PIC (FIELD)
                            INSPECT PIC-LK (FIELD) (PZ: )
                                    CONVERTING "Z" TO "9"
                            ADD 1 TO PZ
                            MOVE "Z" TO ATTR-LK (FIELD) (6: 1)
                            PERFORM UNTIL PIC-LK (FIELD) (PZ: 1) = SPACE
                                    IF DATA-LK (FIELD) (PZ: 1) = SPACE
                                    AND PIC-LK (FIELD) (PZ: 1) = "9"
                                        MOVE "0"
                                          TO DATA-LK (FIELD) (PZ: 1)
                                    END-IF
                                    IF DATA-LK (FIELD) (PZ: 1) = SPACE
                                    AND PIC-LK (FIELD) (PZ: 1) = DP-E
                                        MOVE DP-E
                                          TO DATA-LK (FIELD) (PZ: 1)
                                    END-IF
                                    ADD 1 TO PZ
                            END-PERFORM
                         END-IF
                    END-IF
                END-IF
                CALL "CWCTRL" USING CTRL-LK (FIELD) ATTR-LK (FIELD)
                IF   LENF-LK (FIELD) NUMERIC
                AND  LENF-LK (FIELD) > LEN-LK (FIELD)
                AND  PIC-LK  (FIELD) (1: 1) = "9" OR "Z"
                     COMPUTE P = LENF-LK (FIELD) - LEN-LK (FIELD) + 1
                     MOVE LEN-LK (FIELD) TO P2
                     MOVE DATA-LK (FIELD) (P: P2) TO BUF
                     MOVE BUF                     TO DATA-LK (FIELD)
                END-IF
           END-IF
           MOVE MATRIZ-E (FIELD) TO ELEMENTO
           IF  POS-LK (FIELD) = '0000'
130419     AND CURPOS-LIN  = JANLIMAX
130419     AND CURPOS-COL  > JANMAX
130419*    AND CURPOS = '2580'
130419*        MOVE '0101' TO CURPOS
130419         PERFORM 013-AVANCO
           END-IF
           IF  FUN-ACCEPT
           AND ACCEPT-E
               MOVE "D" TO DF
           ELSE
               MOVE "d" TO DF
           END-IF
           if  cwcontrol = 1
           and (NO-UPDATE AND (CWUPDATE NOT = "ON"))
               MOVE SPACES TO DATA-LK (FIELD)
           end-if
           IF  LEN-E = 0
           AND ATTR-E = SPACES
               MOVE LIN-E TO CURPOS-LIN
               MOVE COL-E TO CURPOS-COL
               GO TO 010-99-FIM
           END-IF
           PERFORM 410-CHECK-PLUS THRU 410-99-FIM
           IF  (DATANAME-E NOT = SPACES)
           AND  VER-MODO-ACCEPT
                MOVE DATANAME-LK (FIELD) TO DATANAME-E
           END-IF
           IF   MODE-LK (FIELD) = "V"
                MOVE "v" TO MODE-LK (FIELD)
                INSPECT DATA-LK (FIELD) (1: LEN-E)
                        CONVERTING X"FB" TO '"'
                MOVE DATA-LK (FIELD) (1: LEN-E) TO DATA-E
           END-IF
           MOVE "23"             TO FS-HIDE
           IF   DATANAME-E NOT = SPACES
                MOVE DATANAME-E TO HIDE-CHAVE
                READ HIDE
                IF   FS-HIDE = "00"
                     MOVE FIELD TO HIDE-FIELD
                     REWRITE HIDE-REG
                END-IF
           END-IF

           MOVE POS-E            TO POS-EX
           IF   FUN-ACCEPT
           AND  POS-E = "0000" AND (POS-S NOT = "0000")
                MOVE POS-S TO POS-E
           ELSE
                MOVE "0000" TO POS-S
           END-IF
           MOVE 0 TO FLAG-SA
           IF   LIN-E = 0
                PERFORM 013-AVANCO
                MOVE CURPOS-LIN TO LIN-E
           END-IF
           IF   COL-E = 0
                IF   FUN-DISPLAY
                     MOVE CURPOS-COL TO COL-E
                     IF   ACCEPT-E
                          MOVE COL-E TO COL-SA (FIELD)
                     END-IF
                ELSE
                     IF  COL-SA (FIELD) = 0
                         MOVE CURPOS-COL TO COL-E
                         MOVE COL-E TO COL-SA (FIELD)
                         MOVE 1 TO FLAG-SA
                     ELSE
                         MOVE COL-SA (FIELD) TO COL-E
                     END-IF
                END-IF
           END-IF

           IF   DATANAME-E = SPACES
                MOVE 1 TO FLAG-SA
           END-IF

           IF  PIC-LK(FIELD)(1:1) = 'X'
           AND BZERO-LK (FIELD) = 'r'
           AND JUST-D = 0
           AND (DATA-LK(FIELD) (1:LEN-E) NOT = SPACES)
           AND (DATA-LK(FIELD) (LEN-E:1)     = SPACES)
               MOVE LEN-E       TO K
               PERFORM VARYING I FROM 1 BY 1
                  UNTIL DATA-LK(FIELD)(I:1) NOT = SPACE
                     CONTINUE
               END-PERFORM
               MOVE DATA-LK(FIELD) (I:LEN-E) TO WORK-JUST
               MOVE SPACES TO DATA-LK(FIELD)
               MOVE 0  TO I
               PERFORM VARYING I FROM LEN-E BY -1
                  UNTIL WORK-JUST(I:1) NOT = SPACE
                        SUBTRACT 1 FROM K
               END-PERFORM
               MOVE WORK-JUST TO DATA-LK(FIELD)(LEN-E - K + 1: K)
               MOVE DATA-LK(FIELD)(1:LEN-E) TO DATA-E (1:LEN-E)
           END-IF

           IF  (FUN-DISPLAY
           OR   FLAG-SA = 1 AND (POS-LK (FIELD) NOT = "0000"))
                MOVE POS-E TO CURPOS
                IF   FLAG-SA = 1
                AND  FUN-ACCEPT
                     MOVE LEN-E TO STRING-LENGTH
                     PERFORM 013-CHECK-AVANCO THRU 013-99-FIM
                END-IF
           END-IF.
       eva.
           move zeros to pos-c
           MOVE 0 TO FLAG-SA
           COMPUTE ROW-NUMBER     = LIN-E - 1
           COMPUTE COLUMN-NUMBER  = COL-E - 1
           EVALUATE TRUE
               WHEN ERASE-EOL
                    COMPUTE STRING-LENGTH = 80 - COL-E + 1
                    MOVE SPACES TO CHARACTER-BUFFER (1: STRING-LENGTH)
                    PERFORM 011-ERASE THRU 011-99-FIM
                    MOVE 0     TO STRING-LENGTH LEN-E
                    MOVE LIN-E TO CURPOS-LIN
                    MOVE COL-E TO CURPOS-COL
               WHEN (BLANK-SCREEN and pos-e < 0102)
                 OR (ERASE-EOS AND POS-E < 0102)
                    MOVE 2000 TO STRING-LENGTH
                    MOVE 0    TO ROW-NUMBER COLUMN-NUMBER
                    PERFORM 011-ERASE THRU 011-99-FIM
                    MOVE 1    TO CURPOS-LIN CURPOS-COL
                    MOVE 0    TO STRING-LENGTH LEN-E
               WHEN ERASE-EOS
                 or (blank-screen and pos-e > 0101)
                    COMPUTE STRING-LENGTH = 80 - COL-E + 1
                                          + (80 * (25 - LIN-E))
                    PERFORM 011-ERASE THRU 011-99-FIM
                    MOVE 0    TO STRING-LENGTH LEN-E
                    MOVE LIN-E TO CURPOS-LIN
                    MOVE COL-E TO CURPOS-COL
               WHEN BLANK-LINE
                    MOVE 80 TO STRING-LENGTH
                    MOVE 0  TO COLUMN-NUMBER
                    PERFORM 011-ERASE THRU 011-99-FIM
                    MOVE 1  TO CURPOS-COL
                    MOVE 0  TO STRING-LENGTH LEN-E
               WHEN FUN-DISPLAY
               OR  (FUN-ACCEPT AND (ACCEPT-E OR EXIBIR-FROM = 1))
                    If (lin-e = 01
                    and col-e = 01
                    and len-e = 80
                    and DATANAME-LK(FIELD) = 'SPACES' or 'SPACE')
                        set ad-eos to true
                    end-if
                    if ad-eos
                       COMPUTE STRING-LENGTH = 80 - COL-E + 1
                                            + (80 * (25 - LIN-E))
                       PERFORM 011-ERASE THRU 011-99-FIM
                    end-if
                    if ad-eol
                       COMPUTE STRING-LENGTH = 80 - COL-E + 1
                       PERFORM 011-ERASE THRU 011-99-FIM
                    end-if

                    if  col-c > 79
                        move lin-e to lin-c
                        move col-e to col-c
                        compute len-c = len-e - 1
                        if  len-c = 0
                            move 1 to len-c
                            if FUN-ACCEPT
                               subtract 1 from col-c
                            end-if
                        end-if
                        add len-c to col-c
                        perform until col-c < 81
                                if lin-c < 25
                                   add 1 to lin-c
                                end-if
                                subtract 80 from col-c
                        end-perform
                    end-if
                    MOVE LEN-E  TO STRING-LENGTH
                    MOVE DATA-E TO CHARACTER-BUFFER
                    PERFORM 012-SET-ATTRIBUTES THRU 012-99-FIM
                    IF   FUN-ACCEPT
                    AND  ACCEPT-E
                         IF   NOT PROT
                              add 1 to ACCEPTs
                         END-IF
                         IF   SAVE-GUIA = 1
                              MOVE FIELD TO GUIA-FIELD
                              MOVE 0     TO GUIA-OBJECT
                                            GUIA-POP
                              PERFORM 014-SAVE-GUIA THRU 014-99-FIM
                         END-IF
                         IF   ATTRIBUTE-ACCEPT (FIELD) = 0
                              MOVE ATTRIBUTE TO ATTRIBUTE-ACCEPT (FIELD)
                              MOVE ROW-NUMBER    TO ACC-LIN (FIELD)
                              MOVE COLUMN-NUMBER TO ACC-COL (FIELD)
                              MOVE LEN-E         TO ACC-LEN (FIELD)
                              IF (DATANAME-E (1: 7) = "CWCHECK"
                                                   OR "CWRADIO")
                              AND LEN-E = 1
                              AND PIC-E = "X"
                                  MOVE 3 TO ACC-LEN (FIELD)
                                  SUBTRACT 1 FROM ACC-COL (FIELD)
                              END-IF
                         ELSE
                              MOVE ATTRIBUTE-ACCEPT (FIELD)
                                TO ATTRIBUTE
                         END-IF
                         IF  (CWENTRY NOT = "OFF") OR PROT
                              MOVE ALL X"00" TO ATTRIBUTE-BUFFER
                                               (1: STRING-LENGTH)
                           IF NOT PROT
                              INSPECT ATTRIBUTE-BUFFER(1: STRING-LENGTH)
                                      CONVERTING X"00" TO ATT-P
                           ELSE
                           IF PROT AND(FB-LK (FIELD)(1:1) NUMERIC
                                    OR FB-LK (FIELD)(2:1) NUMERIC)
                              INSPECT ATTRIBUTE-BUFFER(1: STRING-LENGTH)
                                      CONVERTING X"00" TO ATTR-P(FIELD)
                           ELSE
                              INSPECT ATTRIBUTE-BUFFER(1: STRING-LENGTH)
      *                               CONVERTING X"00" TO X"71"
                                      CONVERTING X"00" TO X"80"
                           END-IF
                           END-IF
                           MOVE ATT-P TO ATT-X
                         END-IF
                         IF FB-LK (FIELD)(1:1) NUMERIC
                         AND (NOT PROT)
                            MOVE FIELD TO SAVEATTR-FIELD
                            READ SAVEATTR
                            IF FS-SAVEATTR = '00'
                               IF  CWENTRY NOT = "OFF"
                                   MOVE X"70" TO ATT-BMS
                               ELSE
                                   MOVE X"00" TO ATT-BMS
                               END-IF
                               MOVE FB-LK (FIELD)(1:1) TO FA
                               ADD FA TO ATT-BMS-N
                               MOVE ALL X"00" TO ATTRIBUTE-BUFFER
                                                (1: STRING-LENGTH)
                               INSPECT
                               ATTRIBUTE-BUFFER(1: STRING-LENGTH)
                                 CONVERTING X"00"
                                 TO ATT-BMS
                            END-IF
                         END-IF
                    END-IF
                    IF   FUN-ACCEPT
                    AND  ACCEPT-E
                    AND  VER-MODO-ACCEPT
                         SET INDEFINIDO TO TRUE
                         IF   PIC-E = "X" OR "A"
                              INSPECT PIC-E (1: STRING-LENGTH)
                                      CONVERTING " A" TO "XX"
                              SET ALFABETICO TO TRUE
                         END-IF
                         IF   PIC-E = "9"
                              INSPECT PIC-E (1: STRING-LENGTH)
                                      CONVERTING SPACE TO "9"
                              SET NUMERICO TO TRUE
                         END-IF
                         IF   PIC-E = "Z"
                              INSPECT PIC-E (1: STRING-LENGTH)
                                      CONVERTING SPACE TO "Z"
                              SET NUMERICO TO TRUE
                         END-IF
                         PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > STRING-LENGTH
                              OR NOT INDEFINIDO
                                 IF   PIC-E (I: 1) = "X" OR "A"
                                      SET ALFABETICO TO TRUE
                                 END-IF
                                 IF   PIC-E (I: 1) = "9" OR "Z" OR "*"
                                                  OR "-" OR "+" OR "."
                                                  OR "," OR "/"
                                      SET NUMERICO TO TRUE
                                 END-IF
                         END-PERFORM
                         IF   NUMERICO
                              PERFORM VARYING I FROM 1 BY 1
                                        UNTIL I > LEN-E
                                   IF   PIC-E (I: 1) = "/" OR ":"
                                        IF  I = LEN-E
                                        AND (NOT COM-BARRA)
                                            SET COM-BARRA2 TO TRUE
                                        ELSE
                                            SET COM-BARRA TO TRUE
                                        END-IF
                                   END-IF
                              END-PERFORM
                         END-IF
                         MOVE MODO-ACCEPT TO MODO-ACCEPT-LK (FIELD)
                         IF  DP = "."
                             INSPECT PIC-E CONVERTING ",." TO ".,"
                         END-IF
                         MOVE PIC-E       TO PIC-LK         (FIELD)
                    END-IF
                    IF  (NOT VER-MODO-ACCEPT)
                    AND  MOUSE-OK (LIN-E COL-E) = 0
                    AND (NOT PROT)
                         MOVE COL-E TO I
                         MOVE 1     TO Y
                         PERFORM LEN-E TIMES
                           IF   PIC-E (Y: 1) = "X" OR "9" OR "A" OR "Z"
                                            OR "-" OR "+" OR "*"
                                MOVE FIELD TO MOUSE-OK (LIN-E I)
                                IF   FS-HIDE < "10"
                                AND  HIDE-OBJECT > 0
                                     MOVE HIDE-OBJECT
                                       TO MOUSE-EVENT (LIN-E I)
                                END-IF
                                IF (DATANAME-E (1: 7) = "CWCHECK"
                                                     OR "CWRADIO")
                                AND LEN-E = 1
                                AND PIC-E = "X"
                                    ADD 1 TO I
                                    MOVE FIELD TO MOUSE-OK (LIN-E I)
                                    SUBTRACT 2 FROM I
                                    MOVE FIELD TO MOUSE-OK (LIN-E I)
                                    ADD  1     TO I
                                END-IF
                           END-IF
                           ADD 1 TO I Y
                         END-PERFORM
                         IF   FS-HIDE < "10"
                              MOVE FIELD       TO MOUSE-OK    (LIN-E I)
                              MOVE HIDE-OBJECT TO MOUSE-EVENT (LIN-E I)
                              IF HIDE-LENGTH > LEN-E
                                  COMPUTE DIF = HIDE-LENGTH - LEN-E
                                  PERFORM DIF TIMES
                                    ADD 1 TO I
                                    MOVE FIELD
                                          TO MOUSE-OK    (LIN-E I)
                                    MOVE HIDE-OBJECT
                                          TO MOUSE-EVENT (LIN-E I)
                                  END-PERFORM
                              END-IF
                         END-IF
                    END-IF
                    IF   BZERO-E = "Z"
                    AND (PIC-E (1: 1) NOT = "X")
                         PERFORM VARYING I FROM 1 BY 1
                                   UNTIL I > STRING-LENGTH
                              OR (CHARACTER-BUFFER (I: 1) NUMERIC
                             AND (CHARACTER-BUFFER (I: 1) NOT = "0"))
                              OR  CHARACTER-BUFFER (I: 1) = "-"
                              OR  CHARACTER-BUFFER (I: 1) = "+"
                              OR  CHARACTER-BUFFER (I: 1) = "C"
                              OR  CHARACTER-BUFFER (I: 1) = "D"
                                 CONTINUE
                         END-PERFORM
                         IF I > STRING-LENGTH
                            MOVE SPACES TO CHARACTER-BUFFER
                                           (1: STRING-LENGTH)
                                           DATA-E (1: LEN-E)
                         END-IF
                    END-IF
                    IF   NUMERICO
                    AND  DP = "."
                         INSPECT CHARACTER-BUFFER (1: LEN-E)
                                 CONVERTING ",." TO ".,"
                    END-IF
                    IF   SINAL-ATUAL = "- " OR "DB"
                         MOVE 1 TO INPUT-NEGATIVO
                    ELSE
                         MOVE 0 TO INPUT-NEGATIVO
                    END-IF
                    MOVE SPACES TO SINAL-ATUAL
                    IF   COM-SINAL NOT NUMERIC
                    AND  NUMERICO
                         MOVE 0 TO COM-SINAL
                         PERFORM VARYING I FROM 1 BY 1
                                  UNTIL I > STRING-LENGTH
                                 IF  PIC-E (I: 1) = "-" OR "+"
                                     MOVE 1 TO COM-SINAL
                                 END-IF
                                 IF  PIC-E (I: 2) = "DB" OR "CR"
                                     MOVE 2 TO COM-SINAL
                                 END-IF
                         END-PERFORM
                         MOVE COM-SINAL TO COM-SINAL-LK (FIELD)
                    END-IF
                    IF   NUMERICO
                    AND (COM-SINAL = 1 OR 2)
                         PERFORM VARYING I FROM 1 BY 1
                                  UNTIL I > STRING-LENGTH
                                 IF  DATA-LK(FIELD) (I: 1) = "-"
                                     MOVE "-" TO SINAL-ATUAL
                                 END-IF
                                 IF  DATA-LK(FIELD) (I: 2) = "DB"
                                     MOVE "DB" TO SINAL-ATUAL
                                 END-IF
                         END-PERFORM
                         IF  SINAL-ATUAL = SPACES
                             PERFORM VARYING I FROM 1 BY 1
                                      UNTIL I > STRING-LENGTH
                                     IF  PIC-E (I: 1) = "-" OR "+"
                                         MOVE "+" TO SINAL-ATUAL
                                         IF INPUT-NEGATIVO = 1
                                            MOVE "-" TO SINAL-ATUAL
                                         END-IF
                                     END-IF
                                     IF  PIC-E (I: 2) = "DB" OR "CR"
                                         MOVE "CR" TO SINAL-ATUAL
                                         IF INPUT-NEGATIVO = 1
                                            MOVE "DB" TO SINAL-ATUAL
                                         END-IF
                                     END-IF
                             END-PERFORM
                         END-IF
                         MOVE 0 TO INPUT-NEGATIVO
                         IF  SINAL-ATUAL = "-" OR "DB"
                         AND ATTRIBUTE-BUFFER (1: 1) = ATT-P OR ATT-0
                             IF  ATTRIBUTE-BUFFER = ATT-0
                                 MOVE 48  TO CL
                             ELSE
                                 MOVE 112 TO CL
                             END-IF
                             ADD  5 TO CL
                             MOVE COR (CL)  TO ATTRIBUTE-NX
                             MOVE ALL X"00" TO ATTRIBUTE-BUFFER
                                            (1: STRING-LENGTH)
                             INSPECT ATTRIBUTE-BUFFER (1: STRING-LENGTH)
                                     CONVERTING X"00" TO ATTRIBUTE-NX
                         END-IF
                    END-IF
                    PERFORM 260-CASE THRU 260-99-FIM
                    IF (DATANAME-E (1: 7) = "CWCHECK"
                                         OR "CWRADIO")
                    AND LEN-E = 1
                    AND PIC-E = "X"
                        MOVE 3 TO STRING-LENGTH
                        MOVE ATTRIBUTE-BUFFER (1: 1)
                          TO ATTRIBUTE-BUFFER (2: 1)
                             ATTRIBUTE-BUFFER (3: 1)
                        IF (CHARACTER-BUFFER (1: 1) NOT = "0")
                        AND(CHARACTER-BUFFER (1: 1) NOT = "1")
                            MOVE "0" TO CHARACTER-BUFFER (1: 1)
                        END-IF
                        IF  CHARACTER-BUFFER (1: 1) = "0"
                            IF  DATANAME-E (1: 7) = "CWRADIO"
                                MOVE "( )" TO CHARACTER-BUFFER
                            ELSE
                                MOVE "[ ]" TO CHARACTER-BUFFER
                            END-IF
                        ELSE
                            IF  DATANAME-E (1: 7) = "CWRADIO"
                                MOVE "( )" TO CHARACTER-BUFFER
                            ELSE
                                MOVE "[ ]" TO CHARACTER-BUFFER
                            END-IF
                            IF  CWUNIX-ON
                                MOVE X"FA" TO CHARACTER-BUFFER (2: 1)
                            ELSE
                                IF  DATANAME-E (1: 7) = "CWRADIO"
                                    MOVE X"09" TO CHARACTER-BUFFER(2: 1)
                                ELSE
                                    MOVE X"07" TO CHARACTER-BUFFER(2: 1)
                                END-IF
                            END-IF
                        END-IF
                        SUBTRACT 1 FROM COLUMN-NUMBER
                    END-IF
                    IF   SECURE-E = "S"
                         IF  NUMERICO OR MODO-ACCEPT = 'B'
                             INSPECT CHARACTER-BUFFER
                                        (1: STRING-LENGTH)
                             CONVERTING '1234567890' TO SECURECHAR
                                        (1: STRING-LENGTH)
                         ELSE
                             MOVE SECURECHAR TO CHARACTER-BUFFER
                                            (1: STRING-LENGTH)
                         END-IF
                    END-IF
                    IF   COM-BARRA
                    AND((CHARACTER-BUFFER(1:STRING-LENGTH) NOT = SPACES)
                    OR BARR = 'ON')
                         PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN-E
                                 IF  PIC-E (I: 1) = "/" OR ":"
                                 AND CHARACTER-BUFFER (I: 1) = SPACE
                                     MOVE PIC-E (I: 1)
                                       TO CHARACTER-BUFFER (I: 1)
                                 END-IF
                         END-PERFORM
                    END-IF
                    IF   ACCEPT-E
                    AND  FS-HIDE < "10"
                         IF   FUN-ACCEPT
                         AND  HIDE-LIST = 0
                              CALL "CBL_WRITE_SCR_ATTRS"
                                                USING SCREEN-POSITION
                                                      ATTRIBUTE-BUFFER
                                                      STRING-LENGTH
                         END-IF
                         SET CWOBJE-GET   TO TRUE
                         MOVE HIDE-OBJECT TO CWOBJE-OCCURS-NUMBER
                         CALL  CWOBJE  USING PARAMETROS-CWOBJE
                                             CWOBJE-FUNCTION
frango                   IF   CWOBJE-CURPOS NOT = '0000'
frango                        add JANLIN to CWOBJE-LINE
frango                        add JANCOL to CWOBJE-COLUMN
frango                   END-IF
                         PERFORM 015-AJUSTA-HORIZONTAL THRU 015-99-FIM
                         SET CWBOXF-VIEW TO TRUE
                         IF  HIDE-LIST = 0
                             MOVE DF               TO CWBOXF-FUNCTION
                             MOVE CHARACTER-BUFFER TO CWBOXF-OPTION
                         END-IF
                         IF  HIDE-LIST = 1
                         OR  CWBOXF-OFF = 1
                             CALL "CBL_WRITE_SCR_CHATTRS"
                                               USING SCREEN-POSITION
                                                     CHARACTER-BUFFER
                                                     ATTRIBUTE-BUFFER
                                                     STRING-LENGTH
                             IF  HIDE-LIST = 1
                                 MOVE SCREEN-POSITION
                                   TO OBJETOS-SCREEN-POSITION
                                 SUBTRACT 1 FROM OBJETOS-COL
                                 READ OBJETOS
                                 IF FS-OBJETOS = '00'
                                    MOVE CHARACTER-BUFFER
                                      TO OBJETOS-CHARACTER-BUFFER
                                         (1:STRING-LENGTH)
                                    REWRITE OBJETOS-REG
                                 END-IF
                             END-IF
                         END-IF
                         IF  CWBOXF-OFF = 0
                         AND (NOT PROT)
                             PERFORM 022-CWBOXF-CALL THRU 022-99-FIM
                         END-IF
                    ELSE
      *                  IF  (FS-HIDE = "23")
      *                  OR   FUN-DISPLAY
                              IF  BZERO-E = '9'
                              AND CHARACTER-BUFFER(1:STRING-LENGTH)
                                  = SPACES
                              AND NUMERICO
                                  PERFORM VARYING IX FROM 1 BY 1
                                       UNTIL IX = STRING-LENGTH
                                          OR PIC-E (IX: 1) = ","
                                       CONTINUE
                                  END-PERFORM
                                  IF IX < STRING-LENGTH
                                     SUBTRACT 1 FROM IX
                                  END-IF
                                  IF ZEROSUPRESS NOT = 'ON'
                                      MOVE '0'
                                        TO CHARACTER-BUFFER (IX: 1)
                                  END-IF
                              END-IF
                              IF  BZERO-E = 'z'
                              AND NUMERICO
                                  perform varying pi-z from 1 by 1
                                        until pi-z > STRING-LENGTH
                                    If CHARACTER-BUFFER (pi-z:1) = space
                                    and  pic-e (pi-z:1) = '9'
                                        move '0'
                                          to CHARACTER-BUFFER (pi-z:1)
                                    end-if
                                  end-perform
                              END-IF
                              if  NUMERICO-BARRA
                                  IF DP = ','
                                     inspect CHARACTER-BUFFER
                                          (1:STRING-LENGTH)
                                     converting "," TO "/"
                                  ELSE
                                     inspect CHARACTER-BUFFER
                                          (1:STRING-LENGTH)
                                     converting ".," TO "/."
                                  END-IF
                              end-if
                              IF  BZERO-E = 'Z'
                              AND NUMERICO
                              AND PIC-E (1:1) = '9'
                              AND CHARACTER-BUFFER (1:1) = space
                              AND (CHARACTER-BUFFER (1:STRING-LENGTH)
                                    not = spaces)
                                  inspect CHARACTER-BUFFER
                                          (1:STRING-LENGTH)
                                  converting SPACE TO "0"
                              END-IF
                              IF SECUREMODE = 'OFF'
                              AND SECURE-E = "S"
                                  CONTINUE
                              ELSE
                                  PERFORM 010-DISPLAY-STRING
                              END-IF
      *                  END-IF
                    END-IF
                    IF   FUN-ACCEPT
                    AND (NOT PROT)
                    AND  FS-HIDE < "10"
                    AND (ACC-POP-CHAR (FIELD) (1: 1) = X"00")
                         MOVE SCREEN-POSITION TO SCREEN-POSITION3
                         IF   HIDE-LENGTH = 0
                              ADD  STRING-LENGTH TO COLUMN-NUMBER3
                         ELSE
                              ADD  HIDE-LENGTH   TO COLUMN-NUMBER3
                         END-IF
                         CALL "CBL_READ_SCR_CHATTRS"
                               USING SCREEN-POSITION3
                                     ACC-POP-CHAR (FIELD) (1: 1)
                                     ACC-POP      (FIELD) (1: 1)
                                     X"0001"
                         IF   HIDE-LIST = 0
                              CALL "CBL_WRITE_SCR_CHATTRS"
                                    USING SCREEN-POSITION3
                                          DESCE
                                          ATTRIBUTE-BUFFER (1: 1)
                                          X"0001"
                         END-IF
                    END-IF
                    IF   CWBEEP NOT = "OFF"
                    AND ("B" = BEEP-E OR BEEP-LK (FIELD))
                    AND (FIELD NOT = FIELD-BEEP)
                         MOVE FIELD TO FIELD-BEEP
                         PERFORM XE5 THRU FIM-XE5
                    END-IF
                    IF  FUN-DISPLAY
                    OR  RM-EOL
                    OR  RM-EOS
                        PERFORM 013-CHECK-AVANCO THRU 013-99-FIM
                    END-IF
                    IF  RM-EOL
                    OR  RM-EOS
                        COMPUTE STRING-LENGTH = 80 - COL-E + 1
                                                   - LEN-E
                        COMPUTE ROW-NUMBER = LIN-E - 1
                        COMPUTE COLUMN-NUMBER = COL-E
                                              + LEN-E - 1
                        IF  RM-EOS
                            COMPUTE STRING-LENGTH =
                                    STRING-LENGTH +
                            ((24 - lin-e) * 80)
                        END-IF
                        PERFORM 011-ERASE THRU 011-99-FIM
                    END-IF
           END-EVALUATE

           if pos-c = zeros
              move CURPOS to pos-c
           end-if.

       010-mb-fim.

           IF  MB-LEN > 0
               MOVE MB-STRING TO DATA-lk (field)
               go to 010-exibir
           else
               if mb-save not = spaces
                  move mb-save to matriz-e (field)
                  move spaces  to mb-save
               end-if
           end-if.

       010-99-FIM. EXIT.

       010-DISPLAY-STRING.

           MOVE STRING-LENGTH TO BUFFER-LENGTH
           PERFORM UNTIL BUFFER-LENGTH = 0
              COMPUTE ESPACO = JANMAX - COLUMN-NUMBER
              IF ESPACO NOT < BUFFER-LENGTH
                 MOVE BUFFER-LENGTH TO ESPACO
              END-IF
              CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                 CHARACTER-BUFFER
                                                 ATTRIBUTE-BUFFER
                                                 ESPACO
              SUBTRACT ESPACO FROM BUFFER-LENGTH
              IF BUFFER-LENGTH > 0
                 MOVE CHARACTER-BUFFER(ESPACO + 1:) TO CHARACTER-BUFFER
                 ADD ESPACO TO COLUMN-NUMBER
                 IF COLUMN-NUMBER NOT < JANMAX
                    ADD  1      TO ROW-NUMBER
                    SUBTRACT JANMAX FROM COLUMN-NUMBER
                    ADD JANCOL TO COLUMN-NUMBER
                 END-IF
      *          PERFORM ESPACO TIMES
      *                  ADD 1 TO COLUMN-NUMBER
      *                  IF COLUMN-NUMBER NOT < JANMAX
      *                     MOVE JANCOL TO COLUMN-NUMBER
      *                     ADD  1      TO ROW-NUMBER
      *                  END-IF
      *          END-PERFORM
              END-IF
           END-PERFORM.

       010-99B-FIM. EXIT.

       011-ERASE.

           MOVE SPACES TO CHARACTER-BUFFER (1: STRING-LENGTH)

           IF  BLANK-SCREEN
           OR  BLANK-LINE
           OR (MB-ATTRIB NOT = LOW-VALUES)
               PERFORM 012-SET-ATTRIBUTES THRU 012-99-FIM
               CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                  CHARACTER-BUFFER
                                                  ATTRIBUTE-BUFFER
                                                  STRING-LENGTH
           ELSE
               CALL "CBL_READ_SCR_ATTRS"    USING SCREEN-POSITION
                                                  ATTRIBUTE-BUFFER
                                                  STRING-LENGTH
               IF RM-EOS OR RM-EOL
                  MOVE LOW-VALUES TO ATTRIBUTE-BUFFER(2:)
                  INSPECT ATTRIBUTE-BUFFER(2:)
                          CONVERTING X"00" TO ATTRIBUTE-X
               END-IF
               MOVE LIN-E TO LRM
               MOVE COL-E TO CRM
               MOVE 1     TO SRM
               PERFORM STRING-LENGTH TIMES
                       IF REVERSEDS-COL (LRM CRM) NOT = 0
                          MOVE REVERSEDS-COL (LRM CRM) (1: 1)
                            TO ATTRIBUTE-BUFFER    (SRM: 1)
                          MOVE 0 TO REVERSEDS-COL (LRM CRM)
                       END-IF
                       ADD 1 TO SRM CRM
                       IF CRM > 80
                          MOVE 1 TO CRM
                          ADD  1 TO LRM
                       END-IF
               END-PERFORM
               CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                  CHARACTER-BUFFER
                                                  ATTRIBUTE-BUFFER
                                                  STRING-LENGTH
           END-IF.

       011-99-FIM. EXIT.

       012-SET-ATTRIBUTES.

           IF SECUREMODE = 'OFF'
           AND SECURE-E = "S"
               EXIT PARAGRAPH
           END-IF

           IF   ATTR-INIT = SPACE
                IF   REVERSE-E = "R"
                     MOVE X"07" TO ATTRIBUTE-BUFFER (1: 1)
                ELSE
                     IF REVERSEDS-COL (LIN-E COL-E) NOT = 0
                        MOVE REVERSEDS-COL (LIN-E COL-E)(1:1)
                          TO ATTRIBUTE-BUFFER
                        MOVE LOW-VALUES TO REVERSEDS-LIN
                                           (LIN-E)(COL-E: LEN-E)
                     ELSE
                        CALL "CBL_READ_SCR_ATTRS" USING SCREEN-POSITION
                                                        ATTRIBUTE-BUFFER
                                                        X"0001"
                     END-IF
                END-IF
                MOVE ATTRIBUTE-BUFFER (1: 1) TO ATTRIBUTE-X
                                                ATTR-INIT
                                                ATTR-INIT-LK (FIELD)
           ELSE
                MOVE ATTR-INIT TO ATTRIBUTE-X
           END-IF

           IF   BACK-E = "b"
           AND  FORE-E = "f"
                IF MB-ATTRIB NOT = LOW-VALUES
                   MOVE MB-ATTRIB TO ATTRIBUTE-X
                   GO TO 012-ok
                END-IF
                move STRING-LENGTH to SL-BOX
                PERFORM UNTIL (COL-E + SL-BOX - 1) NOT > 80
                        SUBTRACT 1 FROM SL-BOX
                END-PERFORM
                IF  (BOX-LINE (LIN-E) (COL-E: SL-BOX)
                             NOT = ALL X"00")
                     MOVE BOX-LINE (LIN-E) (COL-E: SL-BOX)
                       TO ATTRIBUTE-BUFFER (1: SL-BOX)
                     GO TO 012-99-FIM
                ELSE
                     IF   HIGH-E = "H"
                          ADD 8 TO ATTRIBUTE
                     END-IF
                     IF   REVERSE-E = "R"
                          ADD 15 TO ATTRIBUTE
                     END-IF
                     go to 012-ok
                END-IF
           END-IF

           IF   ATTRIBUTE > 127
                SUBTRACT 128 FROM ATTRIBUTE
           END-IF
           MOVE 0 TO BACK-NW
           COMPUTE I = (LIN-E * 80) - 80 + COL-E
           COMPUTE Y = CLASSE-C (I) - 100
           PERFORM UNTIL ATTRIBUTE < 16
                   ADD 1 TO BACK-NW
                   SUBTRACT 16 FROM ATTRIBUTE
           END-PERFORM
           IF   ATTRIBUTE > 7
                SUBTRACT 8 FROM ATTRIBUTE
           END-IF
           MOVE ATTRIBUTE TO FORE-NW
           IF   FORE-E = "F"
                MOVE CWACOR-F (Y) TO FORE-NW
           END-IF
           IF   BACK-E = "B"
                MOVE CWACOR-B (Y) TO BACK-NW
           END-IF
           IF   FORE-E NUMERIC
                MOVE FORE-N TO FORE-NW
           END-IF
           IF   BACK-E NUMERIC
                MOVE BACK-N TO BACK-NW
           ELSE IF BACK-E = "b"
                AND CWBACKDEFAULT NUMERIC
                   MOVE 0             TO BACK-NW
                   MOVE CWBACKDEFAULT TO BACK-NW (2: 1)
                END-IF
           END-IF
           IF FORE-NW = BACK-NW
           AND (FOREBACK NOT = 'OFF')
              IF FORE-NW = 1
                 MOVE 7 TO FORE-NW
              ELSE
                 IF FORE-NW = 7
                    MOVE 0 TO FORE-NW
                 ELSE
                    ADD 1 TO FORE-NW
                 END-IF
              END-IF
           END-IF
           IF   REVERSE-E = "R"
                COMPUTE ATTRIBUTE = BACK-NW + (FORE-NW * 16)
                IF  REVERSEDS-COL (LIN-E COL-E) = 0
                    CALL "CBL_READ_SCR_ATTRS" USING SCREEN-POSITION
                                          REVERSEDS-COL (LIN-E COL-E)
                                                X"0001"
                    COMPUTE LEN-REV = LEN-E - 1
                    COMPUTE COL-REV = COL-E + 1
                    PERFORM LEN-REV TIMES
                            MOVE REVERSEDS-COL (LIN-E COL-E)
                              TO REVERSEDS-COL (LIN-E COL-REV)
                            ADD  1           TO       COL-REV
                    END-PERFORM
                END-IF
           ELSE
                COMPUTE ATTRIBUTE = FORE-NW + (BACK-NW * 16)
                MOVE ALL X"00" TO REVERSEDS-LIN (LIN-E) (COL-E: LEN-E)
           END-IF
           IF   BLINK-E = "B"
                ADD 128 TO ATTRIBUTE
           END-IF
           IF   HIGH-E = "H"
                ADD 8 TO ATTRIBUTE
           END-IF
           COMPUTE CL  = ATTRIBUTE + 1
           MOVE COR (CL) TO ATTRIBUTE-X.
       012-ok.
      *    IF FS-HIDE = '00'
      *       MOVE HIDE-LENGTH TO STRING-LENGTH2
      *       MOVE SCREEN-POSITION TO SCREEN-POSITION2
      *       CALL "CBL_WRITE_SCR_CHATTRS"
      *             USING SCREEN-POSITION2
      *                   DATA-LK(FIELD)
      *                   ATTRIBUTE-BUFFER (1: STRING-LENGTH2)
      *                   STRING-LENGTH2
      *    ELSE
              MOVE SPACES TO ATTRIBUTE-BUFFER
              MOVE ATTRIBUTE-X TO ATTR-P (FIELD)
221116        if   ATTRIBUTE-X = x'00'
221116             move x'07' to ATTRIBUTE-X
221116        end-if
              INSPECT ATTRIBUTE-BUFFER (1: STRING-LENGTH)
                      CONVERTING SPACE TO ATTRIBUTE-X.

       012-99-FIM. EXIT.

       013-CHECK-AVANCO.

           IF   RM-EOL
                MOVE CURPOS-LIN TO RM-LINE-ERASE
           ELSE
                IF   CURPOS-LIN NOT = RM-LINE-ERASE
                     MOVE 0 TO RM-LINE-ERASE
                END-IF
           END-IF
           IF   STRING-LENGTH = 0
           OR  (STRING-LENGTH = 80
           AND  POS-LK (FIELD) = "0101"
           AND  DATANAME-LK (FIELD) = 'SPACES' OR 'SPACE')
                IF   STRING-LENGTH = 80
                     MOVE SPACES TO CHARACTER-BUFFER
                END-IF
                GO 013-99-FIM
           END-IF
           IF   BZERO-E = "Z"
           AND  POS-LK (FIELD) = "0000"
                move "z" to BZERO-E
           END-IF
           IF  (ADVANCE-E = "A"
           AND  FLAG-SA = 0
      *    AND  POS-LK (FIELD) = "0000"
           AND  POS-LK (FIELD) (1:2) = "00"
           AND  (FIELD = FIELDS OR MULTILINE = FIELDS)
           AND (CURPOS-LIN NOT = RM-LINE-ERASE))
                MOVE 1 TO CURPOS-COL
                PERFORM 013-AVANCO
                ADD  1 TO CURPOS-LIN
           ELSE
                IF ((col-e + STRING-LENGTH) - 1 ) = JANMAX
                AND (POS-LK (FIELD) NOT = "0000")
                and lin-e = JANLIMAX
                    continue
                ELSE
                    ADD STRING-LENGTH TO CURPOS-COL
                    PERFORM UNTIL (CURPOS-COL NOT > JANMAX)
                            ADD  1 TO CURPOS-LIN
                            SUBTRACT JANMAX FROM CURPOS-COL
                    END-PERFORM
                END-IF
           END-IF.

       013-AVANCO.

           IF  CURPOS-LIN > JANLIMAX
           OR  AVANCO-PENDENTE = 1
               IF  AVANCO-PENDENTE = 0
               AND JANSAVE (FIELD) (1:2) = '00'
               AND (JANSAVE(FIELD) (3:2) NOT = '00')
                   MOVE 1 TO AVANCO-PENDENTE
                   NEXT SENTENCE
               END-IF
               MOVE 0 TO AVANCO-PENDENTE
               IF   OBJECTS-CWOBJE NOT = 0
                    PERFORM 300-CLEAR-OBJECT THRU 300-99-FIM
               END-IF
               IF  JANLIMAX < 25
                   CALL CBL-READ-SCR-CHATTRS USING X'0000'
                                                   CHARACTER-BUFFER
                                                   ATTRIBUTE-BUFFER
                                                   X"07D0"
                   COMPUTE JL = JANLIN + 2
                   COMPUTE JC = JANCOL + 1
                   COMPUTE JW = JANWID - 1
                   COMPUTE COLUMN-NUMBER = JC - 1
                   MOVE JW TO STRING-LENGTH
                   PERFORM VARYING JL FROM JL BY 1 UNTIL JL > JANHEI
                       COMPUTE ROW-NUMBER = JL - 2
                       if 3 > 2
                       COMPUTE JK = JL - 1
                       MOVE JW TO STRING-LENGTH
                       COMPUTE JJ = JC + JW - 1
                       PERFORM UNTIL JK = 0 OR JW = 0 OR JJ = JC
                        OR CHAR-LIN(JL)(JJ:1) NOT = CHAR-LIN(JK)(JJ:1)
                        OR ATTR-LIN(JL)(JJ:1) NOT = ATTR-LIN(JK)(JJ:1)
                           SUBTRACT 1 FROM JJ JW
                       END-PERFORM
                       ADD 1 TO JW
                       MOVE STRING-LENGTH TO JK
                       MOVE JW TO STRING-LENGTH
                       MOVE JK TO JW
                       end-if
                       CALL CBL-WRITE-SCR-CHATTRS USING
                                           SCREEN-POSITION
                                           CHAR-LIN (JL) (JC: JW)
                                           ATTR-LIN (JL) (JC: JW)
                                           STRING-LENGTH
                   END-PERFORM
                   MOVE JW TO STRING-LENGTH
                   SUBTRACT 1 FROM JL
                   ADD 1 TO ROW-NUMBER
                   MOVE SPACES TO CHAR-LIN (JL) (JC: JW)
                   CALL CBL-WRITE-SCR-CHATTRS USING SCREEN-POSITION
                                               CHAR-LIN (JL) (JC: JW)
                                               ATTR-LIN (JL) (JC: JW)
                                               STRING-LENGTH
               ELSE
                   MOVE 1920   TO STRING-LENGTH
                   MOVE 1      TO ROW-NUMBER
                   MOVE 0      TO COLUMN-NUMBER
                   MOVE SPACES TO CHARACTER-BUFFER
                                  ATTRIBUTE-BUFFER
                   CALL CBL-READ-SCR-CHATTRS USING SCREEN-POSITION
                                                   CHARACTER-BUFFER
                                                   ATTRIBUTE-BUFFER
                                                   STRING-LENGTH
                   MOVE 0    TO ROW-NUMBER
                   ADD 80    TO STRING-LENGTH
                   INSPECT ATTRIBUTE-BUFFER (1921: 80)
                           CONVERTING SPACE TO ATTRIBUTE-X
                   CALL CBL-WRITE-SCR-CHATTRS USING SCREEN-POSITION
                                                    CHARACTER-BUFFER
                                                    ATTRIBUTE-BUFFER
                                                    STRING-LENGTH
               END-IF
               MOVE JANLIMAX  TO CURPOS-LIN
               COMPUTE CURPOS-COL = JANCOL + 1
               IF   OBJECTS-CWOBJE NOT = 0
                    PERFORM 270-DEFINE-OBJECTS THRU 270-99-FIM
               END-IF
           END-IF.

       013-99-FIM. EXIT.

       014-SAVE-GUIA.

           MOVE 0 TO HIDE-GUIA
           IF   GUIA-FIELD NOT = 0
                MOVE LIN-E      TO LIN-G
                MOVE COL-E      TO COL-G
                MOVE GUIA-FIELD TO FIELD-LAST
                MOVE DATANAME-E TO HIDE-CHAVE
                                   SAVE-NAME
                READ HIDE
                IF  FS-HIDE = "00"
                    MOVE 1 TO HIDE-GUIA
                    IF HIDE-COMBO = 1 AND (HIDE-NOEDIT NOT = 1)
                       MOVE 0 TO HIDE-GUIA
                    END-IF
                END-IF
           ELSE
                MOVE CWOBJE-LINE   TO LIN-G
                MOVE CWOBJE-COLUMN TO COL-G
                IF   CWOBJE-TAB-OFF
                AND (CWOBJE-PUSH-BUTTON
                 OR  CWOBJE-PUSH-MOUSE
                 OR  CWOBJE-ICON)
                     WRITE GUIA-O-REG FROM GUIA-REG
                     IF   FS-GUIA-O > "09"
                          CALL "CWISAM" USING ER-GUIA-O
                     END-IF
                END-IF
                IF (NOT CWOBJE-PUSH-BUTTON)
                AND(NOT CWOBJE-PUSH-MOUSE)
                AND(NOT CWOBJE-ICON)
                AND(NOT CWOBJE-LIST-BOX)
                AND(NOT CWOBJE-COMBO-BOX)
                OR      CWOBJE-TAB-OFF
                OR      COMBO-DISPLAY = "1"
                   EXIT PARAGRAPH
                END-IF
                MOVE CWOBJE-OPTION TO SAVE-NAME
           END-IF

           IF  MOUSE-GROUP (LIN-G COL-G) NOT = 0
               MOVE CWOBJE-OCCURS-NUMBER      TO SAVE-CWOBJE
               MOVE MOUSE-GROUP (LIN-G COL-G) TO TESTE-G
                                                 CWOBJE-OCCURS-NUMBER
               CALL  CWOBJE  USING PARAMETROS-CWOBJE CWOBJE-FUNCTION
frango         IF   CWOBJE-CURPOS NOT = '0000'
frango              add JANLIN to CWOBJE-LINE
frango              add JANCOL to CWOBJE-COLUMN
frango         END-IF
               MOVE CWOBJE-CURPOS             TO GUIA-CURPOS
               MOVE POS-G                     TO GUIA-CURPOS2
               MOVE SAVE-CWOBJE               TO CWOBJE-OCCURS-NUMBER
               CALL  CWOBJE  USING PARAMETROS-CWOBJE CWOBJE-FUNCTION
frango         IF   CWOBJE-CURPOS NOT = '0000'
frango              add JANLIN to CWOBJE-LINE
frango              add JANCOL to CWOBJE-COLUMN
frango         END-IF
           ELSE
               MOVE POS-G  TO GUIA-CURPOS
               MOVE "0000" TO GUIA-CURPOS2
           END-IF

           IF   DEFOBJ = 1
                ADD 1 TO OBJECTS
           END-IF

           MOVE GT  TO GUIA-TYPE
           ADD  1   TO GTS
           MOVE GTS TO GUIA-OBJECTS
           IF   GT = 1
           AND  HIDE-GUIA = 1
                MOVE GUIA-REG TO TEMP-GUIA-REG
                READ GUIA
                IF  FS-GUIA = "00"
                    MOVE GUIA-FIELD    TO TEMP-FIELD
                    MOVE GUIA-OBJECT   TO TEMP-OBJECT
                    MOVE GUIA-POP      TO TEMP-POP
                    DELETE GUIA RECORD
                    MOVE TEMP-GUIA-REG TO GUIA-REG
                    MOVE TEMP-FIELD    TO GUIA-FIELD
                    MOVE TEMP-OBJECT   TO GUIA-OBJECT
                    MOVE TEMP-POP      TO GUIA-POP
                END-IF
           END-IF
           MOVE SAVE-NAME TO GUIA-DATANAME
           if CWOBJE-COMBO-BOX
           and (NOT CWOBJE-NOEDIT)
           and guia-field = 0
           and fs-hide = '00'
               if  hide-field > 0
                   move hide-field to guia-field
               else
                   if field > 0
                      move field to guia-field
                   end-if
               end-if
               move 0          to guia-object
           end-if
           If guia-field  not = 0
              move guia-reg to temp-guia-reg
              read guia key is guia-field
              if fs-guia = '00'
                 delete guia record
                 move temp-guia-reg to guia-reg
              end-if
           end-if
           IF GUIA-DATANAME = SPACES
           AND FIELD > 1
                move dataname-lk(Field) TO GUIA-DATANAME
           END-IF
           WRITE GUIA-REG
           IF   FS-GUIA > "09"
           AND (FS-GUIA NOT = "22")
                CALL "CWISAM" USING ER-GUIA
           END-IF.

       014-99-FIM. EXIT.

       015-AJUSTA-HORIZONTAL.

           IF CWOBJE-CURPOS NOT = '0000'
              ADD DINLIN TO CWOBJE-LINE
      *       ADD JANLIN TO CWOBJE-LINE
              ADD DINCOL TO CWOBJE-COLUMN
      *       ADD JANCOL TO CWOBJE-COLUMN
           END-IF
           IF   CWOBJE-COMBO-BOX
           AND  (CWOBJE-OPTION NOT = SPACES)
               PERFORM VARYING CWR FROM 1 BY 1 UNTIL CWR > FIELDS
                       IF CWOBJE-OPTION = DATANAME-LK (CWR)
                       AND CWOBJE-HORIZONTAL-LENGTH > LEN-LK (CWR)
                           MOVE LEN-LK (CWR) TO CWOBJE-HORIZONTAL-LENGTH
                           EXIT PERFORM
                       END-IF
               END-PERFORM
           END-IF.

       015-99-FIM. EXIT.

       020-RECEBER.

           MOVE 0 TO NAO-EXIBE
                     nouser
                     CWBOXF-KEY

           IF   FS-GUIA > "09"
           AND  ERRO = 0
                GO TO 020-JUST
           END-IF

           IF   GUIA-FIELD NOT = 0
                MOVE GUIA-FIELD  TO FIELD
                MOVE 0           TO objeto
           ELSE
                MOVE GUIA-OBJECT TO objeto
                MOVE 0           TO FIELD
           END-IF

           IF   FIELD NOT = 0
                INSPECT DATA-LK (FIELD) (1: LEN-LK(FIELD))
                       converting low-value
                               to space
                MOVE MATRIZ-E (FIELD) TO ELEMENTO
                DISPLAY "CICS-CURPOS" UPON ENVIRONMENT-NAME
                DISPLAY POS-E         UPON ENVIRONMENT-VALUE
                IF  ERRO = 0
                    PERFORM 410-CHECK-PLUS THRU 410-99-FIM
                END-IF
                IF   EDIT-ESC
                     MOVE 0 TO ERRO
                END-IF
                MOVE    GUIA-FIELD       TO FIELD-CRITICA
                PERFORM 029-FIELDS     THRU 029-99-FIM
                IF  (GUIA-FIELD NOT = FIELD)
                AND  GUIA-POP = 1
                AND  EDIT-CURSOR-DOWN
                     move 1 to nouser
                ELSE
                     IF   GUIA-FIELD NOT = FIELD
                     OR   ERRO = 1
                          GO TO 020-JUST
                     END-IF
                END-IF
           END-IF

           IF  (objeto NOT = 0)
           OR   GUIA-POP = 1
                MOVE GUIA-OBJECT TO CWOBJE-OCCURS-NUMBER
                CALL  CWOBJE  USING PARAMETROS-CWOBJE
                                    CWOBJE-FUNCTION
frango          IF   CWOBJE-CURPOS NOT = '0000'
frango               add JANLIN to CWOBJE-LINE
frango               add JANCOL to CWOBJE-COLUMN
frango          END-IF
                EVALUATE TRUE
                   WHEN FROM-TAB = 1
                        MOVE 0 TO FROM-TAB
                   WHEN no-arrow = 1
                        MOVE 0 TO no-arrow
                   WHEN CWOBJE-COMBO-BOX
                    AND (NOT EDIT-TAB)
                        IF CWOBJE-NOEDIT
                        OR EDIT-CURSOR-DOWN
                           SET CWBOXF-SHOW TO TRUE
                           PERFORM 021-CWBOXF THRU 021-99-FIM
                        END-IF
                   WHEN CWOBJE-LIST-BOX
                        SET CWBOXF-POP-UP TO TRUE
                        PERFORM 021-CWBOXF THRU 021-99-FIM
                   WHEN OTHER
                        PERFORM 031-OBJECTS THRU 031-99-FIM
                END-EVALUATE
           END-IF

           MOVE 0 TO BOXF-MOUSE.
       020-JUST.
           IF  JUST-FIELD > 0
           AND ((GUIA-FIELD NOT = JUST-FIELD) OR FS-GUIA > '00')
               MOVE FIELD      TO JUST-SAVE
               MOVE JUST-FIELD TO FIELD
               MOVE 0          TO JUST-D
               PERFORM 010-EXIBIR  THRU 010-99-FIM
               MOVE JUST-SAVE  TO FIELD
               MOVE 0          TO JUST-FIELD JUST-SAVE
           END-IF.

       020-99-FIM. EXIT.

       021-CWBOXF.

           IF  BOXF-MOUSE = 0
           AND CWOBJE-COMBO-BOX
               MOVE CWBOXF-FUNCTION TO SAVE-FUNCTION3
               MOVE DF         TO CWBOXF-FUNCTION
               PERFORM 022-CWBOXF-CALL THRU 022-99-FIM
               MOVE SAVE-FUNCTION3 TO CWBOXF-FUNCTION
               COMPUTE BOXF-ROW    = CWOBJE-LINE    - 1
               COMPUTE BOXF-COLUMN = CWOBJE-COLUMN  - 1
               MOVE CWOBJE-OPTION TO HIDE-CHAVE
               READ HIDE
               IF   FS-HIDE = "00"
               AND (HIDE-FIELD NOT = 0)
                    MOVE CWBOXF-FUNCTION TO SAVE-FUNCTION
                    MOVE DATA-LK (HIDE-FIELD) TO CWBOXF-OPTION
                    if MODO-ACCEPT-LK (HIDE-FIELD) = "N"
                    AND CWBOXF-ORDER = CWBOXF-RETURN
                       inspect CWBOXF-OPTION (1: LENF-LK(HIDE-FIELD))
                              converting space to "0"
                    end-if
                    MOVE DF                   TO CWBOXF-FUNCTION
                    PERFORM 022-CWBOXF-CALL THRU 022-99-FIM
                    MOVE SAVE-FUNCTION TO CWBOXF-FUNCTION
               END-IF
               MOVE LOW-VALUES    TO CARACTER-BOX
               MOVE 1             TO NAO-EXIBE
               MOVE objeto        TO OBJECT-BOXF
               MOVE 0             TO objeto GUIA-POP
               MOVE ACENDER-MOUSE TO SAVE-ACENDER-MOUSE
               MOVE 0             TO ACENDER-MOUSE
               MOVE FIELD         TO FIELD-CWBOXF
               CALL "CBL_SET_CSR_POS" USING BOXF-POSITION
               if nouser = 0
                  PERFORM 033-CHECK-USER THRU 033-99-FIM
               end-if
               IF MULTI-USER  = 1
                  MOVE CWOBJE-LINE   TO CURPOS-LIN
                  MOVE CWOBJE-COLUMN TO CURPOS-COL
               END-IF
               MOVE save-ACENDER-MOUSE  TO ACENDER-MOUSE
               IF  KEYBOARD-STATUS = 0
                   IF  (objeto NOT = 0)
                   AND  objeto = OBJECT-BOXF
                        GO TO 021-CWBOXF-SEGUE
                   END-IF
                   IF  (FIELD NOT = FIELD-CWBOXF)
                   AND (FIELD NOT = 0)
                   AND (FIELD-CWBOXF NOT = 0)
                        GO TO 021-99-FIM
                   END-IF
                   IF  (objeto NOT = 0)
                   AND (CWOBJE-KEY NOT = 0)
                        MOVE CWOBJE-KEY TO TECLA
                        CALL "CWAKEY" USING TECLA MIL
                        MOVE "10"       TO FS-GUIA
                        GO TO 021-99-FIM
                   END-IF
                   IF   GUIA-FIELD = 0
                        MOVE OBJECT-BOXF TO objeto
                   ELSE
                        GO TO 021-99-FIM
                   END-IF
               ELSE
                   if nouser = 0
                      CALL "CWKBDC" USING CURPOS CARACTER   TECLA-EDIT
                                          KEY-STATUS MODO-ACCEPT REPINS
                      PERFORM 150-CHECK-ALT THRU 150-99-FIM
                   end-if
                   move 0 to nouser
                   IF   EDIT-ESC
                        MOVE 0 TO ERRO
                   END-IF
               END-IF
               EVALUATE TRUE
                   WHEN EDIT-F1
                     OR EDIT-ALT-H
                        MOVE PROGRAMA            TO CONTEXT-ID
                        MOVE DATANAME-E          TO CONTEXT-ID (09: )
                        CALL "CWEHLP" USING CONTEXT-ID TECLA-EDIT
                                            POS-E "C"
                   WHEN TECLA-EDIT = 0
                        MOVE CARACTER TO CARACTER-BOX (1: 1)
                        GO TO 021-CWBOXF-SEGUE
                   WHEN EDIT-CURSOR-DOWN
                        MOVE 0 TO TECLA-EDIT
                        GO TO 021-CWBOXF-SEGUE
                   WHEN EDIT-CURSOR-UP
                     OR EDIT-SHIFT-TAB
                     OR EDIT-CURSOR-LEFT
                        MOVE 0 TO OBJECTS
                        PERFORM 330-UP-GUIA THRU 330-99-FIM
                        GO TO 021-99-FIM
                   WHEN EDIT-TAB
                     OR EDIT-CURSOR-RIGHT
                     OR EDIT-ENTER
                        MOVE 0 TO OBJECTS
                        IF EDIT-ENTER
                           MOVE 0 TO FIELD-CRITICA ERRO
                           MOVE CWOBJE-OPTION TO CRITICA-FIELD
                           PERFORM 032-CRITICA THRU 032-99-FIM
                           IF  ERRO = 1
                               GO TO 021-99-FIM
                           END-IF
                        END-IF
                        IF HOTS-KEY = 0
                           PERFORM 320-NEXT-GUIA THRU 320-99-FIM
                        END-IF
                        GO TO 021-99-FIM
                   WHEN OTHER
                        IF   TECLA-EDIT NOT = 0
                             IF   EDIT-ESC
                                  MOVE 0 TO ERRO
                             END-IF
                             MOVE "10" TO FS-GUIA
                             MOVE 0 TO objeto
                        END-IF
                        GO TO 021-99-FIM
               END-EVALUATE
               IF  FROM-MOUSE = 1 OR 2 OR 3
                   GO TO 021-99-FIM
               END-IF
           END-IF.

       021-CWBOXF-SEGUE.

           MOVE SPACES TO BUTTON-TYPE
           MOVE 1                        TO SKIPS
           PERFORM 022-CWBOXF-CALL THRU 022-99-FIM

           IF  SKIPS = 99
               MOVE 1 TO SKIPS
               GO TO 021-99-FIM
           END-IF

           if guia-field = 0
           AND guia-OBJECT = 0
              MOVE LOW-VALUES TO GUIA-REG
              START GUIA KEY NOT LESS GUIA-SEQ
              perform test after until fs-guia > '09'
                             or guia-field > 0
                      read guia next record
              end-perform
              if guia-field = 0
                 move fields to guia-field
              end-if
              move guia-field TO CWBOXF-GUIA
           end-if

           IF   CWBOXF-OPTION NOT = SPACES
                MOVE 3 TO FROM-MOUSE
                MOVE 0 TO MOUSE-EVENT-TYPE
                IF   CWBOXF-TECLA = 13
      *              IF   OBJECTS < 2
                     IF   GUIA-FIELD = FIELDS
                          SET EDIT-ENTER  TO TRUE
                          MOVE TECLA-EDIT TO SAVE-EDIT
                          MOVE HIGH-VALUES TO GUIA-REG
                          START GUIA KEY NOT GREATER GUIA-FIELD
                          READ GUIA NEXT RECORD
                          READ GUIA NEXT RECORD *> SÆo 2 READs mesmo
                          MOVE 1 TO SALTA
                     ELSE
                          IF   SAVE-EDIT = 0
      *                        SET EDIT-CURSOR-DOWN TO TRUE
                               SET EDIT-enter       TO TRUE
                               MOVE TECLA-EDIT      TO SAVE-EDIT
                          END-IF
                          MOVE 0 TO TECLA-EDIT
                     END-IF
                ELSE
                     IF CWBOXF-TECLA NOT = 0
                        MOVE CWBOXF-TECLA TO SAVE-EDIT TECLA-EDIT
                        MOVE 0 TO TECLA
                        PERFORM 150-CHECK-ALT THRU 150-99-FIM
                        IF TECLA = 0
                           PERFORM 100-CONVERTE-TECLA THRU 100-99-FIM
                        END-IF
                        MOVE '10' to fs-guia
                        move 1 to skip-next
                     END-IF
                END-IF
                IF   FS-HIDE = "00"
                     if (CWBOXF-OPTION (1: HIDE-LENRET)
                      NOT = DATA-LK (HIDE-FIELD) (1: HIDE-LENRET))
                     and  CWBOXF-TECLA = 13
                     and  FS-GUIA < '10'
                         set edit-tab to true
                         move tecla-edit to SAVE-EDIT
                         move 1 to from-combo
                         move 0 to from-mouse
                                   tecla-edit
                     end-if
                     MOVE    CWBOXF-OPTION TO DATA-LK (HIDE-FIELD)
                                              DATA-E
                     MOVE    HIDE-FIELD    TO FIELD
                     PERFORM 010-EXIBIR  THRU 010-99-FIM
                END-IF
           ELSE
                CALL "CBL_SET_CSR_POS"  USING CURSOR-POSITION
                MOVE 0 TO objeto
                INITIALIZE PARAMETROS-CWOBJE GUIA-POP
                IF   CWGETL-MOUSE = 5
                AND (MOUSE-EVENT-TYPE NOT = 0)
                     INITIALIZE MOUSE-POSITION-A
                     IF   MOUSE-EVENT-TYPE = 4
                          MOVE 2 TO MOUSE-EVENT-TYPE
                          MOVE ROW-MOUSE      TO T-LIN
                          MOVE COLUMN-MOUSE   TO T-COL
                          ADD 1 TO T-LIN T-COL
                     END-IF
                     move 1 to FROM-BOXF-MOUSE
                     move GUIA-FIELD  to FIELD-GET-MOUSE
                     move GUIA-OBJECT to OBJECT-GET-MOUSE
                     PERFORM 035-GET-MOUSE-2 THRU 035-99-FIM
                     move 0 to FROM-BOXF-MOUSE
                     IF  (GUIA-FIELD  NOT = FIELD-GET-MOUSE )
                     OR  (GUIA-OBJECT NOT = OBJECT-GET-MOUSE)
                          MOVE 3 TO FROM-MOUSE
                     END-IF
                END-IF
      *         IF  CWBOXF-TECLA = 27
      *             SET EDIT-ESC TO TRUE
      *             MOVE "10" TO FS-GUIA
      *             GO TO 021-99-FIM
      *         END-IF
                IF BUTTON-TYPE = 'P'
                   IF  CWBOXF-OPTION = SPACES
                       DISPLAY "CWBOXF"     UPON ENVIRONMENT-NAME
                       ACCEPT CWBOXF-OPTION FROM ENVIRONMENT-VALUE
                       DISPLAY SPACES       UPON ENVIRONMENT-NAME
                       IF CWBOXF-RETURN = 1
                          MOVE CWBOXF-OPTION TO DATA-LK (HIDE-FIELD)
                                                DATA-E
                       END-IF
                   END-IF
                   MOVE SPACES TO BUTTON-TYPE
                   MOVE CWOBJE-KEY TO TECLA
                   IF TECLA NOT = 0
                      CALL "CWAKEY" USING TECLA MIL
                      MOVE "10" TO FS-GUIA
                      GO TO 021-99-FIM
                   END-IF
               ELSE
                   MOVE CWBOXF-TECLA TO TECLA-EDIT
                   MOVE 0 TO TECLA
                   PERFORM 150-CHECK-ALT THRU 150-99-FIM
                   IF TECLA NOT = 0
                      GO TO 021-99-FIM
                   ELSE
                      PERFORM 100-CONVERTE-TECLA THRU 100-99-FIM
                      IF TECLA NOT = 0
                      AND (TECLA NOT = 1)
                      AND (TECLA NOT = 9)
                      AND (TECLA NOT = 99)
                      AND (TECLA NOT = 72)
                          CALL "CWAKEY" USING TECLA MIL
                          MOVE "10" TO FS-GUIA
                          GO TO 021-99-FIM
                      ELSE
                          move 1 to from-kbdc
                          IF TAB
                             SET EDIT-TAB       TO TRUE
                             MOVE 0 TO CWBOXF-GUIA
                          END-IF
                          IF SHIFT-TAB
                             SET EDIT-SHIFT-TAB TO TRUE
                             MOVE 0 TO CWBOXF-GUIA
                          END-IF
                      END-IF
                   END-IF
                END-IF
           END-IF

           PERFORM 023-CWBOX-SKIP THRU 023-99-FIM.

       021-99-FIM. EXIT.

       022-CWBOXF-CALL.

           IF FIELD = 0
              MOVE HIDE-FIELD TO FIELD
           END-IF
           MOVE CWOBJE-PROGRAM           TO CWBOXF-PROGRAM
           MOVE CWOBJE-WORK-AREA         TO CWBOXF-WORK-AREA
           MOVE CWOBJE-ORDER             TO CWBOXF-ORDER
           MOVE CWOBJE-RETURN            TO CWBOXF-RETURN
           MOVE CWOBJE-STRING-1-LENGTH   TO CWBOXF-STRING-1-LENGTH
           MOVE CWOBJE-STRING-2-LENGTH   TO CWBOXF-STRING-2-LENGTH
           IF NOT CWBOXF-VIEW
              SET CWBOXF-EDIT-ON TO TRUE
            ELSE
              MOVE "N" TO CWBOXF-KEY-ON
           END-IF

           IF  (CWOBJE-OPTION NOT = SPACES)
           AND (CWBOXF-FUNCTION NOT = DF)
                MOVE CWOBJE-OPTION TO HIDE-CHAVE
                READ HIDE
                IF   FS-HIDE = "00"
                AND (HIDE-FIELD NOT = 0)
                     MOVE DATA-LK (HIDE-FIELD) TO CWBOXF-OPTION
                     IF  (CWBOXF-ORDER  NOT = CWBOXF-RETURN)
                          MOVE CWBOXF-FUNCTION TO SAVE-FUNCTION2
                          MOVE "G"             TO CWBOXF-FUNCTION
                          CALL "CWBOXF" USING PARAMETROS-CWBOXF
                          MOVE SAVE-FUNCTION2  TO CWBOXF-FUNCTION
                     ELSE
                       if MODO-ACCEPT-LK (HIDE-FIELD) = "N"
                         inspect CWBOXF-OPTION (1: LENF-LK(HIDE-FIELD))
                                     converting space to "0"
                       end-if
                     END-IF
                ELSE
                     PERFORM 023-CWBOX-SKIP THRU 023-99-FIM
                     MOVE 99 TO SKIPS
                     GO TO 022-99-FIM
                END-IF
           END-IF

           MOVE CWOBJE-LINE  TO CWBOXF-LINE
           move spaces       to ct
           MOVE 0            TO ct-lines
           perform until cwboxf-line = 1
                      or (not cwobje-combo-box)
030817*               or (cwboxf-line + CWOBJE-VERTICAL-LENGTH) < 25
030817                or (cwboxf-line + CWOBJE-VERTICAL-LENGTH) < JANHEI
                   subtract 1 from cwboxf-line
                   add      1   to ct-lines
                   move "x"  to CT
           end-perform
           MOVE CWOBJE-COLUMN            TO CWBOXF-COLUMN
           SUBTRACT 2 FROM CWBOXF-COLUMN
           MOVE CWOBJE-VERTICAL-LENGTH   TO CWBOXF-VERTICAL-LENGTH
      *    IF CWBOXF-FUNCTION = DF
      *       MOVE LEN-E TO CWOBJE-HORIZONTAL-LENGTH
      *    END-IF
           MOVE CWOBJE-HORIZONTAL-LENGTH TO CWBOXF-HORIZONTAL-LENGTH

           MOVE ATTRIBUTE-ACCEPT (HIDE-FIELD)
                                         TO CWBOXF-COLOR-BARR-MENU
           MOVE 0                        TO CWBOXF-COLOR-SHADE
           MOVE "CWUSER"                 TO CWBOXF-TITLE
           MOVE ATT-X                    TO CWBOXF-COLOR-FRAME  (1: 1)
                                            CWBOXF-COLOR-BORDER (1: 1)
           IF   CWOBJE-COMBO-BOX
                ADD  1                   TO CWBOXF-HORIZONTAL-LENGTH
                IF   CWUNIX-OFF
                     ADD  128            TO CWBOXF-COLOR-BARR-MENU
                                            CWBOXF-COLOR-FRAME
                                            CWBOXF-COLOR-BORDER
                END-IF
           END-IF
           MOVE 8                        TO CWBOXF-TYPE
           MOVE 1                        TO CWBOXF-TYPE

           INITIALIZE MOUSE-POSITION MOUSE-EVENT-TYPE
           IF  (CARACTER-BOX NOT = LOW-VALUES)
           AND (CARACTER-BOX NOT = CWBOXF-OPTION (1: 1))
           AND (CWBOXF-FUNCTION NOT = DF)
                IF   CWBOXF-OPTION (2: 1) NOT = SPACE
                     MOVE 1  TO CWBOXF-START-CURSOR
                END-IF
                MOVE CARACTER-BOX TO CWBOXF-OPTION
                MOVE LOW-VALUES   TO CARACTER-BOX
           END-IF

           IF NOT CWBOXF-VIEW
              MOVE 0 TO CWBOXF-TECLA
           END-IF
           IF  DF         NOT = CWBOXF-FUNCTION
               compute CWBOXF-HORIZONTAL-LENGTH =
                       CWBOXF-STRING-1-LENGTH +
                       CWBOXF-STRING-2-LENGTH
           ELSE
               MOVE 1  TO CWBOXF-START-CURSOR
           END-IF
           MOVE PROGRAMA            TO CONTEXT-ID
           MOVE GUIA-DATANAME       TO CONTEXT-ID (09: )
           if ct = 'x'
              move ct       to CWOBJE-TYPE
           end-if
           move 0 to from-kbdc
           IF  CWBOXF-SHOW
           AND(FIELD NOT = GUIA-FIELD)
           AND(GUIA-FIELD > 0)
               MOVE LIN-LK(FIELD) TO CURPOS-LIN
               MOVE COL-LK(FIELD) TO CURPOS-COL
               COMPUTE CURSOR-ROW    = CURPOS-LIN - 1
               COMPUTE CURSOR-COLUMN = CURPOS-COL - 1
               CALL "CBL_SET_CSR_POS"  USING CURSOR-POSITION
               move 0     to from-mouse
               MOVE FIELD TO GUIA-FIELD CWBOXF-GUIA
               READ GUIA KEY IS GUIA-FIELD
               READ GUIA KEY IS GUIA-SEQ
           END-IF
           CALL "CWBOXF" USING PARAMETROS-CWBOXF
                               MOUSE-POSITION MOUSE-EVENT-TYPE
                               CWOBJE-TYPE CWBOXF-TECLA
                               CONTEXT-ID GUIA-CURPOS PIC-E
                               ct-lines

           If   MOUSE-EVENT-TYPE = 0
           AND  CWBOXF-OPTION = SPACES
           AND (CWBOXF-TECLA NOT = 13)
           AND (CWBOXF-TECLA NOT = 0)
           AND (CWBOXF-TECLA NOT = 9)
           AND (CWBOXF-TECLA NOT = 27)
           AND (CWBOXF-TECLA NOT = 72)
               DISPLAY "CWBOXF"     UPON ENVIRONMENT-NAME
               ACCEPT CWBOXF-OPTION FROM ENVIRONMENT-VALUE
               DISPLAY SPACES       UPON ENVIRONMENT-NAME
           END-IF
           if ct = "x"
              set CWOBJE-COMBO-BOX to true
           end-if.

       022-99-FIM. EXIT.

       023-CWBOX-SKIP.

           IF from-combo = 1
              move 0 to from-combo
              exit paragraph
           end-if
           MOVE SAVE-EDIT TO TECLA-EDIT
           IF   SKIPS = 0
           AND  GUIA-POP = 0
           AND  FIELD = 0
           AND  objeto = 0
                MOVE 1 TO SKIPS
           END-IF

           IF   EDIT-CURSOR-UP
           OR   EDIT-SHIFT-TAB
           OR   EDIT-CURSOR-LEFT
                PERFORM 330-UP-GUIA   THRU 330-99-FIM SKIPS TIMES
           ELSE
                PERFORM 320-NEXT-GUIA THRU 320-99-FIM SKIPS TIMES
           END-IF.

       023-99-FIM. EXIT.

       029-FIELDS.

           MOVE MATRIZ-E (FIELD) TO ELEMENTO
           PERFORM 410-CHECK-PLUS THRU 410-99-FIM

           IF   DATANAME-E = "CWSTOP"
                MOVE SPACES TO DATA-E (1: LEN-E)
                               DATA-LK (FIELD) (1: LEN-E)
           END-IF

           PERFORM 010-EXIBIR THRU 010-99-FIM
           MOVE 0     TO TECLA-EDIT

           IF  FROM-MOUSE NOT = 2
               MOVE LIN-E TO CURPOS-LIN
               MOVE COL-E TO CURPOS-COL
               MOVE 1     TO COL-ED
               move 0     to att
               IF  NUMERICO
               AND (NOT FS-HIDE = '00'
               AND (CWBOXF-ORDER NOT = CWBOXF-RETURN))
                   PERFORM UNTIL (COL-ED NOT < LEN-E)
                              OR PIC-E (COL-ED + 1: 1) = ","
                           ADD 1 TO COL-ED
                                    CURPOS-COL
                   END-PERFORM
                   PERFORM
                         UNTIL (PIC-E (COL-ED: 1) NOT = '+')
                           AND (PIC-E (COL-ED: 1) NOT = '-')
                           AND (PIC-E (COL-ED: 1) NOT = 'C')
                           AND (PIC-E (COL-ED: 1) NOT = 'R')
                           AND (PIC-E (COL-ED: 1) NOT = 'D')
                           AND (PIC-E (COL-ED: 1) NOT = 'B')
                           SUBTRACT 1 FROM COL-ED
                                           CURPOS-COL
                   END-PERFORM
                   COMPUTE COL-ED-V     = COL-ED     + 2
                   COMPUTE CURPOS-COL-V = CURPOS-COL + 2
               END-IF
           END-IF

           IF  ERRO = 1
               MOVE 2 TO ERRO
           END-IF.

       SETKEY.

           MOVE 0 TO CRITICOU

           MOVE 1 TO CWBOXF-OFF
           PERFORM 030-ACCEPT THRU 030-99-FIM
                   UNTIL ((TECLA-EDIT NOT = 0)
                   AND    (TECLA-EDIT NOT = 999))
                   OR    (objeto NOT = 0)
                   OR     ERRO = 1
           MOVE 0 TO CWBOXF-OFF

           IF   CWSETK-KEYS NOT = 0
                MOVE 0 TO TECLA
           END-IF
           PERFORM 100-CONVERTE-TECLA THRU 100-99-FIM

           IF   CWSETK-KEYS NOT = 0
           AND  objeto = 0
                MOVE 0 TO OK
                PERFORM VARYING I FROM 1
                            BY 1 UNTIL I > CWSETK-KEYS
                                    OR OK = 1
                        IF CWSETK-KEY (I) = 100
                           MOVE 100 TO TECLA
                        END-IF
                        IF CWSETK-KEY (I) = TECLA
                           MOVE 1 TO OK
                        END-IF
                END-PERFORM
                IF OK = 0
                   MOVE 0 TO TECLA-EDIT ERRO
                   GO TO SETKEY
                END-IF
           END-IF

           CALL "CWAKEY" USING TECLA MIL

Tcham      IF  (EDIT-SHIFT-TAB
Tcham      OR   EDIT-CURSOR-UP)
Tchan      AND  FS-GUIA < "10"
Tchan           EXIT PARAGRAPH
Tchan      END-IF
Tchan
           IF  (ERRO = 0 OR 2)
           AND (FS-GUIA > "09" OR TECLA > 1)
                PERFORM 032-CRITICA THRU 032-99-FIM
                        VARYING FIELD-CRITICA FROM 1 BY 1
                                  UNTIL FIELD-CRITICA > FIELDS
                                     OR ERRO = 1
                                     OR TECLA = 1
                MOVE SPACES TO CRITICA-FIELD
           END-IF

           IF   ERRO = 1
                MOVE FIELD-CRITICA TO FIELD
                GO TO 029-99-FIM
           ELSE
                IF   criticou = 0
                AND (FIELD-ANT NOT = 0)
                AND (FIELD > FIELD-ANT or (tecla > 0))
                AND (NOT EDIT-ESC)
Tcham           AND (NOT EDIT-SHIFT-TAB)
Tcham           AND (NOT EDIT-CURSOR-UP)
                     MOVE FIELD TO FIELD-CRITICA
                     PERFORM 032-CRITICA THRU 032-99-FIM
                     IF   ERRO = 1
                          MOVE FIELD-CRITICA TO GUIA-FIELD
                                                FIELD-ANT
                                                FIELD
                          READ GUIA KEY IS GUIA-FIELD
                          READ GUIA KEY IS GUIA-SEQ
                          MOVE 0 TO TECLA-EDIT objeto
                          GO TO 029-99-FIM
                     END-IF
                END-IF
           END-IF

           IF   EDIT-HOME
                PERFORM 340-TOP-GUIA THRU 340-99-FIM
           ELSE
                IF   TECLA-EDIT NOT = 0
                AND  (NOT EDIT-CURSOR-DOWN)
                AND  (NOT EDIT-CURSOR-UP)
                AND  (NOT EDIT-ENTER)
                AND  (NOT EDIT-TAB)
                     MOVE "10" TO FS-GUIA
                END-IF
           END-IF.

       029-99-FIM. EXIT.

       030-ACCEPT.

           PERFORM GETF THRU FIM-GETF
           MOVE 0 TO FROM-TAB FROM-STAB NO-ARROW HOTS-KEY
           IF   GUIA-DATANAME NOT = CRITICA-FIELD
                IF  (CRITICA-FIELD NOT = SPACES)
                AND  FIELD > FIELD-ANT
                     MOVE FIELD-ANT TO FIELD-CRITICA
                     MOVE 1         TO CRITICOU
                     PERFORM 032-CRITICA THRU 032-99-FIM
                     IF   ERRO = 1
                          MOVE FIELD-CRITICA TO GUIA-FIELD
                                                FIELD-ANT
                                                FIELD
                          READ GUIA KEY IS GUIA-FIELD
                          READ GUIA KEY IS GUIA-SEQ
                          GO TO 030-99-FIM
                     END-IF
                END-IF
                MOVE DATA-E TO DATA-SAVE
           ELSE
                MOVE 0 TO CRITICOU
           END-IF

           MOVE GUIA-DATANAME TO CRITICA-FIELD

           MOVE "Z" TO ZE

           IF   ALFABETICO
                IF PIC-E (1: 1) = "Z"
                   PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN-E
                                                    OR ZE = "9"
                           IF PIC-E (I: 1) = "9"
                              MOVE "9" TO ZE
                           END-IF
                   END-PERFORM
                END-IF
CEDAE?          PERFORM UNTIL PIC-E(COL-ED: 1) = "X" OR "9" OR ZE OR ' '
                           OR COL-ED > LEN-E
                        IF  TECLA-EDIT = 0
                        OR  EDIT-CURSOR-RIGHT
                            ADD 1 TO COL-ED CURPOS-COL
                            IF  COL-ED > LEN-E
                                perform test after
                                  until PIC-E (COL-ED: 1) NOT = "B"
                                        SUBTRACT 1 FROM COL-ED
                                                        CURPOS-COL
                                end-perform
                                MOVE 0 TO TECLA-EDIT
                                IF  AUTO-E = 'A'
                                    SET EDIT-enter TO TRUE
                                END-IF
                                exit perform
                            END-IF
                        ELSE
                            SUBTRACT 1 FROM COL-ED CURPOS-COL
                            IF  COL-ED = 0
                                ADD 1 TO COL-ED CURPOS-COL
                                SET EDIT-CURSOR-RIGHT TO TRUE
                            END-IF
                        END-IF
                END-PERFORM
           END-IF

           IF   FROM-LEFT = 1
           or   (numerico and col-ed = 0)
                MOVE LEN-E TO COL-ED
                COMPUTE CURPOS-COL = COL-E + LEN-E - 1
                MOVE 0 TO FROM-LEFT
           END-IF.
       030-accept-2.
           MOVE COL-ED     TO COL-ED-sf
           MOVE CURPOS-COL TO CURPOS-COL-sf
           if   FIELD NOT = FIELD-ANT
                move 0 to att
           end-if
           IF  (PIC-LK (FIELD)(1:1) = 'X')
           AND (FIELD NOT = FIELD-ANT)
               MOVE 0 TO DGS-CHAR
               IF  BZERO-E = 'r'
                   MOVE FIELD TO JUST-FIELD
                   IF  (DATA-LK(FIELD)(1:LEN-E) NOT = SPACES)
                   AND(EDITJUST NOT = 'OFF')
                       MOVE DATA-LK(FIELD)(1:LEN-E) TO DATA-E(1:LEN-E)
                       MOVE LEN-E TO K
                       PERFORM VARYING I FROM 1 BY 1
                         UNTIL DATA-LK(FIELD)(I:1) NOT = SPACE
                               SUBTRACT 1 FROM K
                       END-PERFORM
                       MOVE DATA-LK(FIELD)(I:K) TO DATA-E(1:LEN-E)
                       MOVE DATA-E(1:LEN-E)
                         TO DATA-LK(FIELD)(1:LEN-E)
                       MOVE 1 TO JUST-D
                       PERFORM 010-EXIBIR THRU 010-99-FIM
                   END-IF
               END-IF
           END-IF
           IF   NUMERICO
           AND (FIELD NOT = FIELD-ANT)
                MOVE 0      TO Y DECIMAIS SINAL-FLUTUANTE SINAL-POSIT
                               DGS DIGITOS
                MOVE SPACES TO SINAL-ATUAL
                IF  COL-ED = LEN-E
                    PERFORM VARYING BX FROM 1 BY 1
                             UNTIL BX > LEN-E
                                OR DECIMAIS = 1
                            IF PIC-E (BX: 1) = ","
                               MOVE 1 TO DECIMAIS
                            END-IF
                    END-PERFORM
                    IF  DECIMAIS = 1
                    AND BX < LEN-E
                        MOVE BX TO COL-ED-V
                        PERFORM UNTIL PIC-E (COL-ED: 1) = ","
                                SUBTRACT 1 FROM COL-ED
                                                CURPOS-COL
                        END-PERFORM
                        SUBTRACT 1 FROM COL-ED
                                        CURPOS-COL
                        COMPUTE CURPOS-COL-V = CURPOS-COL + 2
                    END-IF
                ELSE
                    PERFORM UNTIL PIC-E (COL-ED: 1) = ","
                            OR  COL-ED = LEN-E
                                IF   PIC-E (COL-ED: 1) = "B"
                                     MOVE 1 TO Y
                                END-IF
                                ADD 1 TO COL-ED CURPOS-COL
                    END-PERFORM
                END-IF
                IF   PIC-E (COL-ED: 1) = ","
                     MOVE 1 TO DECIMAIS
                END-IF
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN-E
                        MOVE PIC-E (I: 1) TO TEST-NUM
                        IF   SINAL-POS
                             MOVE I TO SINAL-POSIT
                        END-IF
                        IF   SINAL
                        AND  I < LEN-E
                        AND  I > 1
                             MOVE 1 TO SINAL-FLUTUANTE
                        END-IF
                        IF   SINAL-CD
                             MOVE 0 TO SINAL-FLUTUANTE
                        END-IF
                END-PERFORM
                IF  COM-SINAL = 1 OR 2
                AND SINAL-POSIT > 1
                    MOVE PIC-E (SINAL-POSIT: COM-SINAL) TO SINAL-MASK
                    IF  SINAL-FLUTUANTE = 0
                        MOVE DATA-LK(FIELD) (SINAL-POSIT: )
                          TO SINAL-ATUAL
                        IF  SINAL-ATUAL = SPACES
                            EVALUATE TRUE
                                WHEN PIC-E (SINAL-POSIT: 1) = "-"
                                                           OR "+"
                                     MOVE "+" TO SINAL-ATUAL
                                WHEN PIC-E (SINAL-POSIT: 2) = "DB"
                                                           OR "CR"
                                     MOVE "CR" TO SINAL-ATUAL
                            END-EVALUATE
                        END-IF
                    ELSE
                        PERFORM VARYING I FROM 1
                                            BY 1 UNTIL I > LEN-E
                                        OR (SINAL-ATUAL NOT = SPACES)
                                IF  DATA-LK(FIELD) (I: 1) = "-" OR "+"
                                    MOVE DATA-LK(FIELD) (I: 1)
                                      TO SINAL-ATUAL
                                END-IF
                        END-PERFORM
                        IF  SINAL-ATUAL = SPACES
                            MOVE "+" TO SINAL-ATUAL
                        END-IF
                    END-IF
                END-IF
                MOVE PIC-E (COL-ED: 1) TO TEST-NUM
                IF   COL-ED = LEN-E
                AND  SINAL
                     SUBTRACT 1 FROM COL-ED CURPOS-COL
                END-IF
                IF  DECIMAIS = 0
                    PERFORM UNTIL COL-ED NOT < LEN-E
                            ADD 1 TO COL-ED CURPOS-COL
                    END-PERFORM
                    MOVE LEN-E TO COL-ED
                END-IF
                MOVE COL-ED     TO COL-ED-N
                MOVE CURPOS-COL TO CURPOS-COL-N
           END-IF

           IF  FS-HIDE = '00'
           AND (CWBOXF-ORDER NOT = CWBOXF-RETURN)
               MOVE COL-ED-sf     TO COL-ED
               MOVE CURPOS-COL-sf TO CURPOS-COL
           END-IF

           IF  ZE = "9"
           AND COM-BARRA
           and not (NUMERICO-BARRA)
               SET NUMERICO-BARRA           TO TRUE
               SET NUMERICO-BARRA-LK(FIELD) TO TRUE
               go to 030-ACCEPT-2
           END-IF

           IF  NUMERICO-BARRA-LK(FIELD)
               IF DP = ','
                 INSPECT PIC-E         (1: LEN-E) CONVERTING "/" TO ','
                 INSPECT PIC-LK(FIELD) (1: LEN-E) CONVERTING "/" TO ','
                 INSPECT DATA-LK(FIELD)(1: LEN-E) CONVERTING "/" TO ','
                 INSPECT DATA-E        (1: LEN-E) CONVERTING "/" TO ','
               ELSE
                 INSPECT PIC-E         (1:LEN-E) CONVERTING "/." TO '.,'
                 INSPECT PIC-LK(FIELD) (1:LEN-E) CONVERTING "/." TO '.,'
                 INSPECT DATA-LK(FIELD)(1:LEN-E) CONVERTING "/." TO '.,'
                 INSPECT DATA-E        (1:LEN-E) CONVERTING "/." TO '.,'
               END-IF
           ELSE
               IF  FUN-ACCEPT
               AND MODO-ACCEPT-LK(FIELD) = 'B'
                   PERFORM VARYING BX FROM 1 BY 1 UNTIL BX > LEN-E
                   IF  DATA-LK(FIELD) (BX:1) = SPACE
                   AND PIC-LK(FIELD)  (BX:1) = "/"
                       MOVE "/" TO DATA-LK(FIELD) (BX: 1)
                   END-IF
                   END-PERFORM
               END-IF
           END-IF

           MOVE SPACE TO ZE

           MOVE FIELD  TO FIELD-ANT
           MOVE 0      TO TECLA-EDIT
           COMPUTE CURSOR-ROW    = CURPOS-LIN - 1
           COMPUTE CURSOR-COLUMN = CURPOS-COL - 1
           CALL "CBL_SET_CSR_POS"  USING CURSOR-POSITION
           IF   ECHOCURSOR = "ON"
                PERFORM 220-HEX-DISPLAY THRU 220-99-FIM
           END-IF
           PERFORM 033-CHECK-USER THRU 033-99-FIM
           IF  FS-GUIA = '10'
           AND SALTA = 1
               MOVE 0 TO SALTA
               READ GUIA PREVIOUS RECORD
               SET EDIT-ENTER TO TRUE
               GO TO 030-99-FIM
           END-IF

           IF ((objeto NOT = 0)
           AND  TECLA-EDIT = 0)
                IF  (CWOBJE-KEY NOT = 0)
                     MOVE CWOBJE-KEY TO TECLA
                     CALL "CWAKEY" USING TECLA MIL
                     MOVE "10"       TO FS-GUIA
                ELSE
                     SET EDIT-ENTER TO TRUE
                END-IF
                GO TO 030-99-FIM
           END-IF

           move 0 to KEYBOARD-STATUS

           IF   FROM-MOUSE = 2 OR 3
                MOVE 0 TO FROM-MOUSE
                          nouser
                GO TO 030-ACCEPT
           END-IF.

           IF  (NOT EDIT-ESC)
           AND  FROM-MOUSE = 0
                CALL "CBL_GET_KBD_STATUS" USING KEYBOARD-STATUS
                IF  KEYBOARD-STATUS = 0
                AND MULTI-USER = 0
                and (not prot)
                    GO TO 030-99-FIM
                END-IF
                IF  PROT
                    IF  KEYBOARD-STATUS = 1
                        CALL "CWKBDC" USING CURPOS CARACTER TECLA-EDIT
                                            KEY-STATUS " "
                                            REPINS
                        PERFORM 150-CHECK-ALT THRU 150-99-FIM
                        IF  EDIT-CONTROL-U
                        OR  EDIT-CONTROL-L
                        OR  EDIT-CONTROL-V
                        OR  EDIT-CONTROL-F
                        OR  EDIT-CONTROL-END
                        OR  EDIT-CONTROL-X
                        OR  EDIT-CONTROL-R
                        OR  EDIT-DEL
                        OR  EDIT-BACKSPACE
                        OR  TECLA-EDIT = 0
                            PERFORM XE5 THRU FIM-XE5
                            GO TO 030-ACCEPT
                        END-IF
                    ELSE
                        move save-prot to tecla-edit
                        if tecla-edit = 0
                           set edit-enter to true
                           move tecla-edit to save-prot
                        end-if
                    END-IF
                ELSE
                     CALL "CWKBDC" USING CURPOS CARACTER   TECLA-EDIT
                                   KEY-STATUS MODO-ACCEPT
                                   REPINS
                     PERFORM 150-CHECK-ALT THRU 150-99-FIM
                END-IF
                IF   EDIT-ESC
                     MOVE 0 TO ERRO
                END-IF
                EVALUATE TRUE
                    WHEN TECLA-EDIT = 0
                     AND CARACTER = X"01"
                         MOVE DATA-LK (FIELD) TO WORK-ED
                         MOVE DATA-SAVE       TO DATA-LK (FIELD)
                         MOVE WORK-ED         TO DATA-SAVE
                         PERFORM 010-EXIBIR THRU 010-99-FIM
                         GO TO 030-ACCEPT
                    WHEN EDIT-CONTROL-U
                         IF CWCASE = "MIX" OR "UPP"
                            INSPECT DATA-LK (FIELD) (1: LEN-E)
                                    CONVERTING MINUSCULAS TO MAIUSCULAS
                            PERFORM 010-EXIBIR THRU 010-99-FIM
                         END-IF
                         GO TO 030-ACCEPT
                    WHEN EDIT-CONTROL-L
                         IF CWCASE = "MIX" OR "LOW"
                            INSPECT DATA-LK (FIELD) (1: LEN-E)
                                    CONVERTING MAIUSCULAS TO MINUSCULAS
                            PERFORM 010-EXIBIR THRU 010-99-FIM
                         END-IF
                         GO TO 030-ACCEPT
                    WHEN EDIT-CONTROL-V
                         PERFORM 250-CASE-MIX THRU 250-99-FIM
                         PERFORM 010-EXIBIR   THRU 010-99-FIM
                         GO TO 030-ACCEPT
                    WHEN EDIT-CONTROL-F
                         PERFORM VARYING I
                                    FROM 1 BY 1
                                      UNTIL I > LENGTH OF MAIUSCULAS
                            EVALUATE TRUE
                                WHEN DATA-LK (FIELD) (COL-ED: 1)
                                  = MAIUSCULAS (I: 1)
                                    MOVE MINUSCULAS (I: 1)
                                      TO DATA-LK (FIELD) (COL-ED: 1)
                                WHEN DATA-LK (FIELD) (COL-ED: 1)
                                  = MINUSCULAS (I: 1)
                                    MOVE MAIUSCULAS (I: 1)
                                      TO DATA-LK (FIELD) (COL-ED: 1)
                            END-EVALUATE
                         END-PERFORM
                         PERFORM 010-EXIBIR THRU 010-99-FIM
                         SET EDIT-CURSOR-RIGHT TO TRUE
                    WHEN EDIT-TAB         SET EDIT-CURSOR-DOWN TO TRUE
                         MOVE 1 TO FROM-TAB
                    WHEN EDIT-SHIFT-TAB   SET EDIT-CURSOR-UP   TO TRUE
                         MOVE 1 TO FROM-STAB
                    WHEN EDIT-CONTROL-END SET EDIT-CONTROL-X   TO TRUE
                    WHEN EDIT-CONTROL-R
                         IF  DEL = 0
                         OR  NUMERICO
                             PERFORM XE5 THRU FIM-XE5
                             GO TO 030-ACCEPT
                         ELSE
                             SET EDIT-CONTROL-O TO TRUE
                             MOVE DELETADOS (DEL: 1) TO CARACTER
                             MOVE SPACES TO DELETADOS (DEL: 1)
                             SUBTRACT 1 FROM DEL
                             PERFORM 240-DISPLAY-DELETADOS
                                THRU 240-99-FIM
                         END-IF
                END-EVALUATE
           END-IF
           IF  (CWGETL-TIMEOUT NOT = 0)
           OR   X91-PARAMETER = 5
                ACCEPT HORA-A FROM TIME
                MOVE 0 TO SEGUNDOS
           END-IF
           MOVE DATANAME-LK(field) TO HIDE-CHAVE
           READ HIDE
           EVALUATE TRUE
               WHEN EDIT-CONTROL-HOME
                    PERFORM 340-TOP-GUIA THRU 340-99-FIM
                    MOVE GUIA-FIELD TO FIELD
                    PERFORM UNTIL FS-GUIA > "09"
                              MOVE MATRIZ-E (FIELD) TO ELEMENTO
                              PERFORM 410-CHECK-PLUS THRU 410-99-FIM
                              IF   ACCEPT-E
                                   EVALUATE TRUE
                                    WHEN DATANAME-E (1: 7) = "CWCHECK"
                                                          OR "CWRADIO"
                                         MOVE "0" TO DATA-LK(FIELD)
                                                     (1: 1)
                                         PERFORM 010-EXIBIR
                                            THRU 010-99-FIM
                                    WHEN NUMERICO
                                         INSPECT DATA-LK(FIELD)
                                                 CONVERTING "123456789"
                                                         TO "000000000"
                                         PERFORM 070-EDIT
                                         THRU 070-99-FIM
                                    WHEN OTHER
                                         MOVE SPACES TO DATA-LK(FIELD)
                                         PERFORM 010-EXIBIR
                                            THRU 010-99-FIM
                                   END-EVALUATE
                              END-IF
                              PERFORM TEST AFTER UNTIL FS-GUIA > "09"
                                                 OR(GUIA-FIELD NOT = 0)
                                                 or guia-pop = 1
                                      READ GUIA NEXT RECORD
                              END-PERFORM
                              move 0 to field objeto
                              if guia-field not = 0
                                 MOVE GUIA-FIELD TO FIELD
                              else
                                 IF GUIA-object = objeto
                                    MOVE '10' TO FS-GUIA
                                    SET EDIT-ENTER TO TRUE
                                 END-IF
                                 MOVE GUIA-object TO objeto
                              end-if
                    END-PERFORM
                    PERFORM 350-BOT-GUIA THRU 350-99-FIM
      *             SET EDIT-CURSOR-DOWN TO TRUE
                    SET EDIT-TAB         TO TRUE
               WHEN EDIT-F1
                 OR EDIT-ALT-H
                    MOVE PROGRAMA            TO CONTEXT-ID
                    MOVE DATANAME-E          TO CONTEXT-ID (09: )
                    CALL "CWEHLP" USING CONTEXT-ID TECLA-EDIT POS-E "C"
               WHEN EDIT-CURSOR-UP
                    MOVE TECLA-EDIT TO SAVE-EDIT
                                       save-prot
                    MOVE 0 TO VERT-MOVE
                    IF   FIELD > 1
                    AND  FROM-STAB = 0
      *                  MOVE DATANAME-LK (FIELD) TO DATANAME-F
                         PERFORM VARYING F FROM FIELD BY -1
                                                UNTIL F < 1
                                 IF  (F NOT = FIELD)
                                 AND  COL-E = COL-LK (F)
      *                          AND  DATANAME-LK (F) = DATANAME-F
                                 AND  ACCEPT-LK (F)
                                      MOVE F TO VERT-MOVE
                                      EXIT PERFORM
                                 END-IF
                         END-PERFORM
                    END-IF
                    IF   FIELD = 1
                    and (enter-terminate = 'ON' OR PROT) *> ROF
                         PERFORM 350-BOT-GUIA THRU 350-99-FIM
                    ELSE
                         IF   VERT-MOVE = 0
                              PERFORM 330-UP-GUIA THRU 330-99-FIM
                         ELSE
                              MOVE VERT-MOVE TO GUIA-FIELD
                              READ GUIA KEY IS GUIA-FIELD
                              IF FS-GUIA = '23'
                                 MOVE DATANAME-LK (F) TO GUIA-DATANAME
                                 READ GUIA KEY IS GUIA-DATANAME
                                 IF FS-GUIA = '23'
                                    PERFORM 330-UP-GUIA THRU 330-99-FIM
                                 END-IF
                              END-IF
                              READ GUIA KEY IS GUIA-SEQ
                         END-IF
                    END-IF
               WHEN EDIT-CURSOR-DOWN
                 OR EDIT-ENTER
                    MOVE TECLA-EDIT TO SAVE-EDIT
                                       save-prot
                    IF PROT
                       SET EDIT-CURSOR-DOWN TO TRUE
                    END-IF
      ******************* MD **************
                    IF (EDIT-CURSOR-DOWN AND FROM-TAB = 0)
                    AND FS-HIDE = '00'
                    AND HIDE-COMBO  = 1
                        MOVE "P"             TO BUTTON-TYPE
                        MOVE HIDE-OBJECT     TO objeto
                        MOVE 1 TO BOXF-MOUSE skip-next
                        MOVE 0 To TECLA-EDIT
                        PERFORM 271-EXIBE-OBJECTS THRU 271-99-FIM
Tcham               ELSE
Tcham                   IF  FS-HIDE = '00'
Tcham                   AND FROM-TAB = 1
Tcham                       SET EDIT-ENTER TO TRUE
Tcham                   END-IF
                    END-IF
                    MOVE 0 TO VERT-MOVE
                    IF   FIELD < FIELDS
                    AND  (EDIT-CURSOR-DOWN AND FROM-TAB = 0)
      *                  MOVE DATANAME-LK (FIELD) TO DATANAME-F
                         PERFORM VARYING F FROM FIELD BY 1
                                                UNTIL F > FIELDS
                                 IF  (F NOT = FIELD)
                                 AND  COL-E = COL-LK (F)
      *                          AND  DATANAME-LK (F) = DATANAME-F
                                 AND  ACCEPT-LK (F)
                                      MOVE F TO VERT-MOVE
                                      EXIT PERFORM
                                 END-IF
                         END-PERFORM
                    END-IF
                    IF   VERT-MOVE = 0
                         IF EDIT-ENTER
                         AND (ENTER-TERMINATE = 'ON' OR HOTS-KEY > 0)
                             MOVE '10' TO FS-GUIA
                         ELSE
                             move 1 to no-arrow
                             PERFORM 320-NEXT-GUIA THRU 320-99-FIM
                         END-IF
                    ELSE
                         MOVE VERT-MOVE TO GUIA-FIELD
                         READ GUIA KEY IS GUIA-FIELD
                         IF FS-GUIA = '23'
                            MOVE DATANAME-LK (F) TO GUIA-DATANAME
                            READ GUIA KEY IS GUIA-DATANAME
                            SET EDIT-TAB TO TRUE
                            IF FS-GUIA = '23'
                               move 1 to no-arrow
                               PERFORM 320-NEXT-GUIA THRU 320-99-FIM
                            END-IF
                         END-IF
                         READ GUIA KEY IS GUIA-SEQ
                    END-IF
               WHEN ALFABETICO
                    PERFORM 040-ALFABETICO THRU 040-99-FIM
               WHEN NUMERICO
                    PERFORM 050-NUMERICO   THRU 050-99-FIM
           END-EVALUATE

           IF  NUMERICO-BARRA-LK(FIELD)
               IF DP = ','
                  INSPECT PIC-E          (1: LEN-E)
                          CONVERTING ',' TO "/"
                  INSPECT PIC-LK(FIELD)  (1: LEN-E)
                          CONVERTING ',' TO "/"
                  INSPECT DATA-LK(FIELD) (1: LEN-E)
                          CONVERTING ',' TO "/"
                  INSPECT DATA-E
                          (1: LEN-E) CONVERTING ',' TO "/"
               ELSE
                  INSPECT PIC-E          (1: LEN-E)
                          CONVERTING '.,' TO "/."
                  INSPECT PIC-LK(FIELD)  (1: LEN-E)
                           CONVERTING '.,' TO "/."
                  INSPECT DATA-E
                          (1: LEN-E) CONVERTING ',.' TO "//"
                  PERFORM VARYING IB FROM 1 BY 1 UNTIL IB > LEN-E
                          IF DATA-E(IB:1) = '/'
                             MOVE PIC-E (IB:1) TO DATA-E(IB:1)
                          END-IF
                  END-PERFORM
                  INSPECT DATA-E
                          (1: LEN-E) CONVERTING '.' TO ","
                  MOVE DATA-E (1: LEN-E) TO DATA-LK (FIELD) (1:LEN-E)
               END-IF
           END-IF
           MOVE 0 TO FROM-MOUSE.

       030-99-FIM. EXIT.

       031-OBJECTS.

           MOVE 1 TO ACENDER
           PERFORM 271-EXIBE-OBJECTS THRU 271-99-FIM
           MOVE 0 TO ACENDER
                     TECLA-EDIT
                     FROM-KEY

           PERFORM UNTIL EDIT-ENTER
                      OR GUIA-OBJECT = 0
                      OR FS-GUIA > "09"
                   CALL "CBL_GET_KBD_STATUS" USING KEYBOARD-STATUS
                   IF  KEYBOARD-STATUS = 1
                   OR  CWUNIX-ON
                   OR  MULTI-USER = 1
                       CALL "CWKBDC" USING CURPOS CARACTER TECLA-EDIT
                                           KEY-STATUS "A" REPINS
                       PERFORM 150-CHECK-ALT THRU 150-99-FIM
                       IF   EDIT-ESC
                            MOVE 0 TO ERRO
                       END-IF
                       MOVE TECLA-EDIT TO SAVE-EDIT
                       IF TECLA = 0
                          PERFORM 100-CONVERTE-TECLA THRU 100-99-FIM
                          IF TECLA NOT = 0
                             IF  GUIA-OBJECT = 0
                                 SET EDIT-ENTER   TO TRUE
                             END-IF
                             MOVE "10" TO FS-GUIA
                             CALL "CWAKEY" USING TECLA MIL
      *                      EXIT PERFORM
                          END-IF
                       END-IF
                       EVALUATE TRUE
                           WHEN EDIT-TAB
                             OR EDIT-CURSOR-DOWN
                             OR EDIT-CURSOR-RIGHT
                                PERFORM 320-NEXT-GUIA THRU 320-99-FIM
                                MOVE 1 TO FROM-KEY
                                MOVE MOUSE-POSITION TO MOUSE-POSITION-S
                           WHEN EDIT-SHIFT-TAB
                             OR EDIT-CURSOR-UP
                             OR EDIT-CURSOR-LEFT
                                PERFORM 330-UP-GUIA THRU 330-99-FIM
                                MOVE 1 TO FROM-KEY
                           WHEN EDIT-ENTER
                                MOVE CWOBJE-KEY     TO TECLA
                                CALL "CWAKEY" USING TECLA MIL
                                MOVE MOUSE-POSITION TO MOUSE-POSITION-S
                                IF TECLA NOT = 0
                                   MOVE "10" TO FS-GUIA
                                END-IF
                           WHEN(CARACTER NOT = X'00')
                            AND(CARACTER NOT = SPACE)
                                MOVE CARACTER TO HOTS-HOT
                                INSPECT HOTS-HOT CONVERTING
                                       MINUSCULAS TO MAIUSCULAS
                                READ HOTS
                                IF FS-HOTS = '00'
                                   SET EDIT-ENTER   TO TRUE
                                   MOVE HOTS-KEY    TO TECLA
                                   MOVE HOTS-OBJECT TO GUIA-OBJECT
                                   CALL "CWAKEY" USING TECLA MIL
                                END-IF
                       END-EVALUATE
                       IF   GUIA-OBJECT NOT = objeto
                       OR   FROM-KBDC > 0
                            MOVE 0 TO FROM-KBDC
                            PERFORM 271-EXIBE-OBJECTS THRU 271-99-FIM
                            IF   GUIA-OBJECT NOT = 0
                                 MOVE GUIA-OBJECT TO objeto
                                 MOVE 1 TO ACENDER
                                 PERFORM 271-EXIBE-OBJECTS
                                    THRU 271-99-FIM
                                 MOVE 0 TO ACENDER
                            END-IF
                       END-IF
                   ELSE
                       IF   CWGETL-MOUSE = 5
                            MOVE 1 TO ACENDER-MOUSE
                            MOVE 0 TO FIELD
                            PERFORM 035-GET-MOUSE THRU 035-99-FIM
                            MOVE 0 TO ACENDER-MOUSE
                            IF  BUTTON-TYPE = "P"
                                SET EDIT-ENTER TO TRUE
                            END-IF
                       END-IF
                   END-IF
                   IF   GUIA-OBJECT NOT = 0
                        continue *> xato
      *                 CALL "CBL_SET_CSR_POS" USING X"FFFF"
                   ELSE
                        IF  objeto NOT = OBJECT-SAVE
                            PERFORM 271-EXIBE-OBJECTS
                               THRU 271-99-FIM
                            MOVE objeto TO OBJECT-SAVE
                        END-IF
                   END-IF
           END-PERFORM

           IF   GUIA-OBJECT = 0
                MOVE 0          TO TECLA-EDIT objeto
                MOVE GUIA-FIELD TO FIELD
           ELSE
                MOVE GUIA-OBJECT TO objeto
                IF  (GUIA-FIELD = FIELD
                AND  FIELD > 0)
                OR   (ACCEPTs = 0 AND TECLA = 0)
                     MOVE "10"        TO FS-GUIA
                     IF   CWOBJE-KEY NOT = 0
                          MOVE 0          TO TECLA-EDIT
                          MOVE CWOBJE-KEY TO TECLA
                          CALL "CWAKEY" USING TECLA MIL
                     END-IF
                END-IF
           END-IF

           MOVE 0 TO FROM-KEY.

       031-99-FIM. EXIT.

       032-CRITICA.

           MOVE 0 TO ERRO
           IF   EDIT-ESC
                GO TO 032-99-FIM
           END-IF

           IF   FIELD-CRITICA = 0
           OR   ACCEPT-LK (FIELD-CRITICA)
                IF   FIELD-CRITICA NOT = 0
                     MOVE DATANAME-LK (FIELD-CRITICA) TO CRITICA-FIELD
                END-IF
                MOVE CRITICA-FIELD TO SAVE-NAME
                READ CRITICA
                IF   FS-CRITICA = "23"
                     MOVE "ANY" TO CRITICA-FIELD
                     READ CRITICA
                END-IF
                MOVE SAVE-NAME TO CRITICA-FIELD
                IF   FS-CRITICA = "00"
                     MOVE SPACES  TO PARAMETROS-CRITICA
                     INITIALIZE USING-MC
                     MOVE 1       TO LEN-CRITICA
                     PERFORM VARYING I FROM 1 BY 1
                               UNTIL I > CRITICA-USINGS
                             MOVE 0 TO FLAG-CRITICA
                             MOVE 0 TO SUBSCRIPT
                             PERFORM VARYING Y FROM 1 BY 1
                                       UNTIL Y > FIELDS
                              IF  CRITICA-USING (I) = DATANAME-LK (Y)
                                  ADD 1 TO SUBSCRIPT
                                  IF SUBSCRIPT = CRITICA-SUBSCRIPT(I)
                                     PERFORM 032A-MOVER THRU 032A-99-FIM
                                     MOVE 1 TO FLAG-CRITICA
                                     EXIT PERFORM
                                  END-IF
                              END-IF
                             END-PERFORM
                             IF FLAG-CRITICA = 0
                                MOVE CRITICA-USING (I)
                                 TO PARAMETROS-CRITICA (LEN-CRITICA:)
                                PERFORM VARYING FLAG-CRITICA
                                        FROM LENGTH OF CRITICA-USING (I)
                                          BY -1
                                     UNTIL FLAG-CRITICA = 0
                                OR CRITICA-USING (I) (FLAG-CRITICA: 1)
                                           NOT = SPACE
                                        CONTINUE
                                END-PERFORM
                                ADD FLAG-CRITICA TO LEN-CRITICA
                             END-IF
                     END-PERFORM
                     MOVE 0 TO SUBSCRIPT
                     PERFORM VARYING Y FROM 1 BY 1
                               UNTIL Y > FIELD-CRITICA
                           IF  CRITICA-FIELD = DATANAME-LK (Y)
                               ADD 1 TO SUBSCRIPT
                           END-IF
                     END-PERFORM
                     DISPLAY 'CWUSER-MSG' UPON ENVIRONMENT-NAME
                     DISPLAY 'ON'         UPON ENVIRONMENT-VALUE
                     CALL CRITICA-PROGRAM USING ERRO
                                          PARAMETROS-CRITICA
                                          CRITICA-FIELD
                                          SUBSCRIPT
                     DISPLAY 'CWUSER-MSG' UPON ENVIRONMENT-NAME
                     DISPLAY 'OFF'        UPON ENVIRONMENT-VALUE
                     IF ERRO > 1
                        MOVE 1 TO ERRO
                     END-IF
                     PERFORM VARYING MC FROM 1 BY 1
                               UNTIL MC > MC-MAX
                             MOVE FIELD            TO SAVE-FIELD
                             MOVE MC-POSIT(MC)     TO USING-POSIT
                             MOVE MC-LEN  (MC)     TO USING-LEN
                             MOVE MC-Y    (MC)     TO FIELD
                             MOVE FIELD            TO FIELD-USING
                             MOVE MATRIZ-E (FIELD) TO ELEMENTO
                             MOVE PARAMETROS-CRITICA
                                  (USING-POSIT: USING-LEN)
                                              TO DATA-LK (FIELD)
                             IF   NUMERICO-LK (FIELD)
                             OR   BARRA-LK (FIELD)
                                  PERFORM 070-EDIT THRU 070-99-FIM
                             END-IF
                             MOVE 1 TO EXIBIR-FROM
                             move cwboxf-off to cwboxf-off-save
                             move 0 to cwboxf-off
                             PERFORM 010-EXIBIR  THRU 010-99-FIM
                             move cwboxf-off-save to cwboxf-off
                             MOVE 0 TO EXIBIR-FROM
                             MOVE FIELD-USING      TO FIELD
                             MOVE SAVE-FIELD TO FIELD
                             MOVE MATRIZ-E (FIELD) TO ELEMENTO
                     END-PERFORM
                     IF   ERRO = 1
                     AND (CRITICA-CURSOR NOT = SPACES)
                          MOVE 0 TO SUBSCRIPT
                          PERFORM VARYING Y FROM 1 BY 1
                                    UNTIL Y > FIELDS
                              IF  CRITICA-CURSOR = DATANAME-LK (Y)
                              AND ACCEPT-LK (Y)
                                 ADD 1 TO SUBSCRIPT
                                 IF SUBSCRIPT = CRITICA-CURSOR-SUBSCRIPT
                                    MOVE Y TO FIELD-CRITICA
                                 EXIT PERFORM
                                 END-IF
                              END-IF
                          END-PERFORM
                     END-IF
                END-IF
           END-IF.

       032-99-FIM. EXIT.

       032A-MOVER.

           IF  DATANAME-LK (Y) = CRITICA-FIELD
               MOVE LEN-CRITICA TO USING-POSIT
               MOVE LEN-LK (Y)  TO USING-LEN
               MOVE Y           TO USING-Y
           END-IF

           ADD  1           TO MC-MAX
           MOVE LEN-CRITICA TO MC-POSIT (MC-MAX)
           MOVE Y           TO MC-Y     (MC-MAX)

           IF   NUMERICO-LK (Y)
           OR   BARRA-LK    (Y)
                MOVE 0  TO NUMERO NEGATIVO SINAL-ED
                MOVE 18 TO N
                PERFORM VARYING XN FROM LEN-LK (Y)
                            BY -1 UNTIL XN = 0
                        IF  DATA-LK (Y) (XN: 1) NUMERIC
                        AND N > 1
                            MOVE DATA-LK (Y) (XN: 1) TO NUMERO (N: 1)
                            SUBTRACT 1 FROM N
                        ELSE
                            IF   PIC-LK (Y) (XN: 1) = "9"
                                                   OR "Z"
                                                   OR "*"
                                 SUBTRACT 1 FROM N
                            END-IF
                            IF   PIC-LK (Y) (XN: 1)  = "-"
                                                    OR "+"
                                 IF  SINAL-ED = 0
                                     MOVE 1 TO SINAL-ED
                                 ELSE
                                     SUBTRACT 1 FROM N
                                 END-IF
                            END-IF
                            IF  DATA-LK (Y) (XN: 1) = "-" OR "D"
                                MOVE 1 TO NEGATIVO
                            END-IF
                        END-IF
                END-PERFORM
                IF   NEGATIVO = 1
                     COMPUTE NUMERO = NUMERO * -1
                END-IF
                ADD  1            TO N
                MOVE NUMERO (N: ) TO PARAMETROS-CRITICA (LEN-CRITICA: )
                MOVE LENGTH OF NUMERO (N: ) TO MC-LEN   (MC-MAX)
                ADD  LENGTH OF NUMERO (N: ) TO LEN-CRITICA
           ELSE
                MOVE DATA-LK (Y) TO PARAMETROS-CRITICA (LEN-CRITICA: )
                MOVE LEN-LK (Y)  TO MC-LEN   (MC-MAX)
                ADD LEN-LK (Y)   TO LEN-CRITICA
           END-IF.

       032A-99-FIM. EXIT.

       033-CHECK-USER.
           move 0 to guia-pop
           PERFORM TEST AFTER UNTIL KEYBOARD-STATUS = 1
                                 OR CWUNIX-ON
                                 OR FROM-MOUSE = 1 OR 2
                                 OR (objeto NOT = 0)
                                 OR (GUIA-POP = 1)
                                 OR PROT
                                 OR FS-GUIA = '10'
                   CALL "CWATCH"
                   IF  POS-EX = "0000"
                       MOVE 0 TO SEGUNDOS
                   END-IF
                   IF  MULTI-USER = 0
                       CALL "CBL_GET_KBD_STATUS" USING KEYBOARD-STATUS
                   ELSE
                       MOVE 1 TO KEYBOARD-STATUS
                   END-IF
                   IF ((CWGETL-TIMEOUT NOT = 0)
                   OR   X91-PARAMETER = 5)
                   AND (KEYBOARD-STATUS NOT = 1)
                        ACCEPT HORA-B FROM TIME
                        IF   HORA-B NOT = HORA-A
                             MOVE HORA-B TO HORA-A
                             ADD 1 TO SEGUNDOS
                             IF   X91-PARAMETER = 5
                                  IF   SEGUNDOS NOT < WS-TIMEOUT
                                       MOVE 1 TO KEYBOARD-STATUS
                                       SET EDIT-F11 TO TRUE
                                       CALL "CWCRTS" USING "S" X"31008B"
                                       IF   TIMEOUT-RETURN = 98
                                            MOVE 77 TO TIMEOUT-RETURN
                                       END-IF
                                  END-IF
                             ELSE
                                  IF   SEGUNDOS NOT < CWGETL-TIMEOUT
                                       MOVE 1 TO KEYBOARD-STATUS
                                       SET EDIT-ESC TO TRUE
                                       CALL "CWCRTS" USING "S" X"31001B"
                                  END-IF
                             END-IF
                        END-IF
                   END-IF
                   IF   CWGETL-MOUSE = 5
                   AND  GUIA-POP = 0
                   AND (KEYBOARD-STATUS NOT = 1)
                        PERFORM 035-GET-MOUSE THRU 035-99-FIM
                   END-IF
           END-PERFORM.

       033-99-FIM. EXIT.

       035-GET-MOUSE.

           MOVE 0 TO MOUSE-READ-TYPE
           CALL "CBL_GET_MOUSE_POSITION" USING MOUSE-HANDLE
                                               MOUSE-POSITION
                ON   OVERFLOW
                     CONTINUE
           END-CALL
           IF   MOUSE-POSITION = MOUSE-POSITION-S
           AND  FROM-KEY = 1
                GO TO 035-99-FIM
           END-IF
           IF   MOUSE-POSITION-A NOT = MOUSE-POSITION
                MOVE 0 TO SEGUNDOS
           END-IF
           MOVE MOUSE-POSITION TO MOUSE-POSITION-A
           MOVE ROW-MOUSE      TO T-LIN
           MOVE COLUMN-MOUSE   TO T-COL
           ADD 1 TO T-LIN T-COL
           IF  MOUSE-EVENT (T-LIN T-COL) NOT = 0
           AND ((T-LIN NOT = S-LIN)
           OR (T-COL NOT = S-COL)
           OR  KEYBOARD-STATUS = 0)
               MOVE T-LIN TO S-LIN
               MOVE T-COL TO S-COL
               IF   ACENDER-MOUSE = 1
                    IF   MOUSE-EVENT (T-LIN T-COL) NOT = objeto
                         MOVE 0 TO ACENDER
                         PERFORM 271-EXIBE-OBJECTS THRU 271-99-FIM
                         MOVE MOUSE-EVENT (T-LIN T-COL) TO objeto
                         MOVE 1 TO ACENDER
                         PERFORM 271-EXIBE-OBJECTS THRU 271-99-FIM
                         MOVE 0 TO ACENDER
                    END-IF
               END-IF
               IF   ACENDER-MOUSE = 0
                    IF   MOUSE-EVENT (T-LIN T-COL) NOT = TESTE-M
                    AND  ACESO = 1
                         PERFORM 283-EXIBE-MOUSE  THRU 283-99-FIM
                    END-IF
                    MOVE CWOBJE-OCCURS-NUMBER      TO SAVE-CWOBJE
                    MOVE MOUSE-EVENT (T-LIN T-COL) TO TESTE-M
                                                CWOBJE-OCCURS-NUMBER
                    CALL  CWOBJE  USING PARAMETROS-CWOBJE
                                        CWOBJE-FUNCTION
frango              IF   CWOBJE-CURPOS NOT = '0000'
frango                   add JANLIN to CWOBJE-LINE
frango                   add JANCOL to CWOBJE-COLUMN
frango              END-IF
                    IF   CWOBJE-PUSH-MOUSE
                         IF  ACESO = 0
                             COMPUTE ROW-NUMBER2    = CWOBJE-LINE   - 1
                             COMPUTE COLUMN-NUMBER2 = CWOBJE-COLUMN - 1
                             PERFORM 283-EXIBE-MOUSE  THRU 283-99-FIM
                         END-IF
                    END-IF
                    MOVE SAVE-CWOBJE TO CWOBJE-OCCURS-NUMBER
                    CALL  CWOBJE  USING PARAMETROS-CWOBJE
                                        CWOBJE-FUNCTION
frango              IF   CWOBJE-CURPOS NOT = '0000'
frango                   add JANLIN to CWOBJE-LINE
frango                   add JANCOL to CWOBJE-COLUMN
frango              END-IF
               END-IF
           ELSE
               IF   ACESO = 1
                    PERFORM 283-EXIBE-MOUSE  THRU 283-99-FIM
               END-IF
               MOVE 0 TO TESTE-M
           END-IF
           CALL "CBL_READ_MOUSE_EVENT" USING
                                       MOUSE-HANDLE
                                       MOUSE-DATA
                                    MOUSE-READ-TYPE
                ON   OVERFLOW
                     CONTINUE
           END-CALL.

       035-GET-MOUSE-2.

           IF MOUSE-EVENT-TYPE = 2 OR 3
              IF scroll-col (T-LIN T-COL) NOT = 0
                 PERFORM 037-SCROLL THRU 037-99-FIM
                 GO TO 035-99-FIM
              END-IF
              IF (MOUSE-OK    (T-LIN T-COL) NOT = 0)
              OR (MOUSE-EVENT (T-LIN T-COL) NOT = 0)
                  MOVE 0 TO SKIPS
                  IF  MOUSE-EVENT (T-LIN T-COL) NOT = 0
                      IF  MOUSE-OK (T-LIN T-COL) NOT = 0
                          MOVE MOUSE-OK (T-LIN T-COL) TO FIELD
                          MOVE DATANAME-LK (FIELD) TO HIDE-CHAVE
                          READ HIDE
                          IF   FS-HIDE = "00"
                          AND (HIDE-OBJECT NOT = 0)
                              MOVE HIDE-OBJECT TO CWOBJE-OCCURS-NUMBER
                               CALL  CWOBJE  USING PARAMETROS-CWOBJE
                                                   CWOBJE-FUNCTION
frango                         IF   CWOBJE-CURPOS NOT = '0000'
frango                              add JANLIN to CWOBJE-LINE
frango                              add JANCOL to CWOBJE-COLUMN
frango                         END-IF
                               MOVE 1 TO BOXF-MOUSE
                          END-IF
                      END-IF
                      MOVE MOUSE-EVENT (T-LIN T-COL) TO objeto
                      MOVE "P"                       TO BUTTON-TYPE
                      PERFORM 271-EXIBE-OBJECTS THRU 271-99-FIM
                      PERFORM UNTIL MOUSE-EVENT-TYPE = 0
                         MOVE 1 TO MOUSE-READ-TYPE
                         CALL "CBL_READ_MOUSE_EVENT" USING
                                                     MOUSE-HANDLE
                                                     MOUSE-DATA
                                                     MOUSE-READ-TYPE
                         ON   OVERFLOW
                              CONTINUE
                         END-CALL
                         CALL "CBL_GET_MOUSE_POSITION"
                               USING MOUSE-HANDLE
                                     MOUSE-POSITION
                           ON   OVERFLOW
                                CONTINUE
                         END-CALL
                         IF   MOUSE-POSITION-A NOT = MOUSE-POSITION
                              MOVE 0 TO SEGUNDOS
                              MOVE ROW-MOUSE    TO T-LIN
                              MOVE COLUMN-MOUSE TO T-COL
                              ADD 1 TO T-LIN T-COL
                              IF  MOUSE-EVENT (T-LIN T-COL) = objeto
                              AND BUTTON-TYPE = "D"
                                  MOVE "P" TO BUTTON-TYPE
                                  PERFORM 271-EXIBE-OBJECTS
                                     THRU 271-99-FIM
                              ELSE
                                  IF  MOUSE-EVENT (T-LIN T-COL)
                                      NOT = objeto
                                  AND BUTTON-TYPE = "P"
                                      MOVE "D" TO BUTTON-TYPE
                                      PERFORM 271-EXIBE-OBJECTS
                                         THRU 271-99-FIM
                                  END-IF
                              END-IF
                         END-IF
                      END-PERFORM
                      IF   MOUSE-EVENT (T-LIN T-COL) NOT = objeto
                      and  CWBOXF-GUIA = 0
                           IF   FROM-BOXF-MOUSE = 1
                                IF   MOUSE-EVENT (T-LIN T-COL) NOT = 0
                                     move MOUSE-EVENT (T-LIN T-COL)
                                       to objeto
                                     move objeto   to guia-object
                                     read guia key is guia-object
                                     read guia key is guia-seq
                                END-IF
                                GO TO 035-99-FIM
                           ELSE
                                MOVE 0 TO objeto
                                GO TO 035-GET-MOUSE
                           END-IF
                      END-IF
                      if  (objeto not = 0)
                      and (objeto not = guia-object)
                      and MOUSE-EVENT (T-LIN T-COL) = objeto
                      and CWBOXF-GUIA = 0
                           move objeto   to guia-object
                           read guia key is guia-object
                           read guia key is guia-seq
                      END-IF
                      MOVE 0 TO SKIPS
                                CWBOXF-GUIA
                  ELSE
                      IF   FIELD NOT = MOUSE-OK (T-LIN T-COL)
                           IF  JUST-FIELD > 0
                               MOVE 0          TO JUST-D
                               PERFORM 010-EXIBIR  THRU 010-99-FIM
                               MOVE 0          TO JUST-FIELD
                           END-IF
                           MOVE MOUSE-OK (T-LIN T-COL) TO FIELD
                           MOVE MATRIZ-E (FIELD)       TO ELEMENTO
                           PERFORM 410-CHECK-PLUS THRU 410-99-FIM
                           MOVE FIELD TO GUIA-FIELD
                           READ GUIA KEY IS GUIA-FIELD
                           READ GUIA KEY IS GUIA-SEQ
                           PERFORM 010-EXIBIR THRU 010-99-FIM
                      END-IF
                      MOVE 1     TO COL-ED
                      MOVE LIN-E TO CURPOS-LIN
                      MOVE COL-E TO CURPOS-COL
                      PERFORM UNTIL CURPOS-COL = T-COL
                              ADD 1 TO CURPOS-COL
                                       COL-ED
                              IF FLOAT *>float
                              AND (DATA-LK(FIELD)(COL-ED:1) = SPACE
                               OR  DATA-LK(FIELD)(COL-ED:1) = '+'
                               OR  DATA-LK(FIELD)(COL-ED:1) = '-')
                               AND COL-ED > 1
                                   SUBTRACT 1 FROM COL-ED
                                                   CURPOS-COL
                                   EXIT PERFORM
                              END-IF
                      END-PERFORM
                      COMPUTE CURSOR-ROW    = CURPOS-LIN - 1
                      COMPUTE CURSOR-COLUMN = CURPOS-COL - 1
                      IF ((DATANAME-E (1: 7) = "CWCHECK"
                                            OR "CWRADIO")
                      AND LEN-E = 1
                      AND PIC-E = "X")
                          MOVE LIN-E           TO CURPOS-LIN
                          MOVE COL-E           TO CURPOS-COL
                          PERFORM 036-CHECK  THRU 036-99-FIM
                          COMPUTE CURSOR-ROW    = CURPOS-LIN - 1
                          COMPUTE CURSOR-COLUMN = CURPOS-COL - 1
                          MOVE 0 TO SAVE-LIN SAVE-COL
                          MOVE 1 TO COL-ED
                      END-IF
                      IF   CURPOS-LIN = SAVE-LIN
                      AND  CURPOS-COL = SAVE-COL
                           MOVE 1 TO FROM-MOUSE
                           SET EDIT-ENTER TO TRUE
                           MOVE 0 TO SAVE-LIN
                                     SAVE-COL
                      ELSE
                           MOVE 2          TO FROM-MOUSE
                           MOVE CURPOS-LIN TO SAVE-LIN
                           MOVE CURPOS-COL TO SAVE-COL
                           CALL "CBL_SET_CSR_POS"  USING CURSOR-POSITION
                           IF   ECHOCURSOR = "ON"
                                PERFORM 220-HEX-DISPLAY THRU 220-99-FIM
                           END-IF
                      END-IF
                  END-IF
              ELSE
                  SET EDIT-ENTER TO TRUE
                  MOVE 1 TO FROM-MOUSE
              END-IF
           END-IF.

       035-99-FIM. EXIT.

       036-CHECK.

           IF   DATANAME-E (1: 7) = "CWCHECK"
                IF   DATA-LK(FIELD) (1: 1) = "0"
                     MOVE "1" TO DATA-LK(FIELD) (1: 1)
                ELSE
                     MOVE "0" TO DATA-LK(FIELD) (1: 1)
                END-IF
           ELSE
                IF   DATA-LK(FIELD) (1: 1) = "1"
                     GO TO 036-99-FIM
                ELSE
                     MOVE "1" TO DATA-LK(FIELD) (1: 1)
                END-IF
           END-IF

           IF   DATANAME-E (1: 7) = "CWRADIO"
           AND (MOUSE-GROUP (LIN-E COL-E) NOT = 0)
                MOVE MOUSE-GROUP (LIN-E COL-E) TO GROUP-W
                MOVE FIELD                     TO FIELD-SAVE
                PERFORM VARYING FIELD FROM 1 BY 1 UNTIL FIELD > FIELDS
                    IF  DATANAME-LK (FIELD) (1: 7) = "CWRADIO"
                    AND FIELD NOT = FIELD-SAVE
                        MOVE MATRIZ-E (FIELD) TO ELEMENTO
                        PERFORM 410-CHECK-PLUS THRU 410-99-FIM
                        IF  MOUSE-GROUP (LIN-E COL-E) = GROUP-W
                            MOVE "0" TO DATA-LK(FIELD) (1: 1)
                            PERFORM 010-EXIBIR THRU 010-99-FIM
                        END-IF
                    END-IF
                END-PERFORM
                MOVE FIELD-SAVE TO FIELD
           END-IF

           MOVE 0 TO TECLA-EDIT
           PERFORM 010-EXIBIR THRU 010-99-FIM.

       036-99-FIM. EXIT.

       037-SCROLL.

           MOVE T-LIN                    TO X
           MOVE T-COL                    TO Y
           MOVE WINWORK-WINDOW           TO THUMBS-WINDOW
           MOVE scroll-col (T-LIN T-COL) TO THUMBS-ID
           READ THUMBS
           SET ADDRESS THUMB             TO THUMBS-THUMB
           MOVE THUMB                    TO THUMB-SAVE
           MOVE ROW-MOUSE                TO ROW-NUMBER2
           MOVE COLUMN-MOUSE             TO COLUMN-NUMBER2
           CALL "CBL_READ_SCR_CHARS"  USING SCREEN-POSITION2
                                            CHAR
                                            X"0001"
           EVALUATE TRUE
               WHEN CHAR = ARROW-UP OR ARROW-LEFT
                    IF THUMB > 1
                       SUBTRACT 1 FROM THUMB
                    END-IF
               WHEN CHAR = ARROW-DOWN OR ARROW-RIGHT
                    IF THUMB < 100
                       ADD 1 TO THUMB
                    END-IF
               WHEN CHAR = X'B2'
                    PERFORM 039-SCROLL-DRAG THRU 039-99-FIM
               WHEN CHAR = X'B0'
                    IF THUMBS-TYPE = 'v'
                       IF ROW-MOUSE <> THUMBS-ROW
                       AND (ROW-MOUSE NOT >
                           (THUMBS-INITROW + THUMBS-FULL))
                       AND ROW-MOUSE > THUMBS-INITROW
                           IF ROW-MOUSE > THUMBS-ROW
                           AND THUMB < 100
                               ADD 10 TO THUMB
                               IF THUMB > 100
                                  MOVE 100 TO THUMB
                               END-IF
                           END-IF
                           IF ROW-MOUSE < THUMBS-ROW
                           AND THUMB > 10
                               SUBTRACT 10 FROM THUMB
                               IF THUMB < 2
                                  MOVE 1 TO THUMB
                               END-IF
                           END-IF
                       END-IF
                    ELSE
                       IF COLUMN-MOUSE <> THUMBS-COL
                       AND COLUMN-MOUSE NOT >
                          (THUMBS-INITCOL + THUMBS-FULL)
                       AND COLUMN-MOUSE > THUMBS-INITCOL
                           IF COLUMN-MOUSE > THUMBS-COL
                           AND THUMB < 100
                               ADD 10 TO THUMB
                               IF THUMB > 100
                                  MOVE 100 TO THUMB
                               END-IF
                           END-IF
                           IF COLUMN-MOUSE < THUMBS-COL
                           AND THUMB > 10
                               SUBTRACT 10 FROM THUMB
                               IF THUMB < 2
                                  MOVE 1 TO THUMB
                               END-IF
                           END-IF
                       END-IF
                    END-IF
           END-EVALUATE

           IF   THUMB NOT = THUMB-SAVE
                MOVE    T-LIN              TO X
                MOVE    T-COL              TO Y
                PERFORM 038-SCROLL-POSIT THRU 038-99-FIM
                MOVE    thumbs-key         TO TECLA
                CALL    "CWAKEY"        USING TECLA MIL
                MOVE    "10"               TO FS-GUIA
                SET     EDIT-ENTER         TO TRUE
           END-IF.

       037-99-FIM. EXIT.

       038-SCROLL-POSIT.

           MOVE WINWORK-WINDOW   TO THUMBS-WINDOW
           MOVE scroll-col (X Y) TO THUMBS-ID
           READ THUMBS
           IF FS-THUMBS = '47'
              OPEN I-O THUMBS
              MOVE '23' TO FS-THUMBS
           END-IF
           IF FS-THUMBS < '09'
              CALL "CBL_WRITE_SCR_CHARS" USING THUMBS-POSITION
                                               X'B0'
                                               X"0001"
           END-IF
           SET ADDRESS THUMB     TO THUMBS-THUMB
           MOVE THUMBS-INITIAL   TO THUMBS-POSITION
           EVALUATE TRUE
               WHEN THUMB < 2
                    MOVE 1 TO POSITS
               WHEN THUMB > 98
                    MOVE THUMBS-FULL TO POSITS
               WHEN OTHER
                    COMPUTE POSITS = THUMB * THUMBS-FULL / 100
           END-EVALUATE
           IF POSITS < 2
              MOVE 1 TO POSITS
           END-IF
           IF   THUMBS-TYPE = 'v'
                ADD POSITS TO THUMBS-ROW
           ELSE
                ADD POSITS TO THUMBS-COL
           END-IF
           CALL "CBL_WRITE_SCR_CHARS" USING THUMBS-POSITION
                                            X'B2'
                                            X"0001"
           IF FS-THUMBS > '09'
              WRITE THUMBS-REG
           ELSE
              REWRITE THUMBS-REG
           END-IF.

       038-99-FIM. EXIT.

       039-SCROLL-DRAG.

           MOVE MOUSE-POSITION TO MOUSE-POSITION-A

           PERFORM UNTIL MOUSE-EVENT-TYPE = 0
              MOVE 1 TO MOUSE-READ-TYPE
              CALL "CBL_READ_MOUSE_EVENT" USING
                                          MOUSE-HANDLE
                                          MOUSE-DATA
                                          MOUSE-READ-TYPE
              ON   OVERFLOW
                   CONTINUE
              END-CALL
              IF  MOUSE-EVENT-TYPE NOT = 0
                  CALL "CBL_GET_MOUSE_POSITION"
                        USING MOUSE-HANDLE
                              MOUSE-POSITION
                    ON   OVERFLOW
                         CONTINUE
                  END-CALL
                  IF MOUSE-POSITION NOT = MOUSE-POSITION-A
                     MOVE MOUSE-POSITION TO MOUSE-POSITION-A
                     COMPUTE X = ROW-MOUSE    + 1
                     COMPUTE Y = COLUMN-MOUSE + 1
                     CALL "CBL_WRITE_SCR_CHARS" USING THUMBS-POSITION
                                                      X'B0'
                                                      X"0001"
                     IF THUMBS-TYPE = 'v'
                        IF ROW-MOUSE <> THUMBS-ROW
                        AND (ROW-MOUSE NOT >
                            (THUMBS-INITROW + THUMBS-FULL))
                        AND ROW-MOUSE > THUMBS-INITROW
                            COMPUTE THUMB = THUMB
                                + ((100 / THUMBS-FULL)
                                * (ROW-MOUSE - THUMBS-ROW))
                            MOVE ROW-MOUSE TO THUMBS-ROW
                        END-IF
                     ELSE
                        IF COLUMN-MOUSE <> THUMBS-COL
                        AND COLUMN-MOUSE NOT >
                           (THUMBS-INITCOL + THUMBS-FULL)
                        AND COLUMN-MOUSE > THUMBS-INITCOL
                            COMPUTE THUMB = THUMB
                                + ((100 / THUMBS-FULL)
                                * (COLUMN-MOUSE - THUMBS-COL))
                            MOVE COLUMN-MOUSE TO THUMBS-COL
                        END-IF
                     END-IF
                     CALL "CBL_WRITE_SCR_CHARS" USING THUMBS-POSITION
                                                      X'B2'
                                                      X"0001"
                     REWRITE THUMBS-REG
                  END-IF
              END-IF
           END-PERFORM.

       039-99-FIM. EXIT.

       040-ALFABETICO.

           MOVE 1 TO JUST-D
           IF FLOAT
              MOVE SPACE TO DP-F
              MOVE 30    TO DP-F-P
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > 30
                      IF DATA-LK(FIELD)(I:1) = DP
                         MOVE DP TO DP-F
                         MOVE I  TO DP-F-P
                      END-IF
              END-PERFORM
           END-IF
           EVALUATE TRUE
               WHEN EDIT-CONTROL-Z
                    IF  COM-BARRA
                        INSPECT DATA-LK(FIELD) (COL-ED: )
                           CONVERTING "123456789" TO "000000000"
                    ELSE
                        MOVE DATA-LK(FIELD) (COL-ED: ) TO WORK-ED
                        PERFORM 230-SAVE-DEL THRU 230-99-FIM
                        MOVE SPACES TO DATA-LK(FIELD) (COL-ED: )
                    END-IF
                    PERFORM 010-EXIBIR THRU 010-99-FIM
                    MOVE 0 TO TECLA-EDIT
               WHEN EDIT-CONTROL-X
                    IF  COM-BARRA
                        INSPECT DATA-LK(FIELD)
                           CONVERTING "123456789" TO "000000000"
                    ELSE
                        MOVE DATA-LK(FIELD) TO WORK-ED
                        PERFORM 230-SAVE-DEL THRU 230-99-FIM
                        MOVE SPACES TO DATA-LK(FIELD)
                    END-IF
                    IF   COL-ED > 1
                         PERFORM UNTIL COL-ED = 1
                                 SUBTRACT 1 FROM COL-ED CURPOS-COL
                         END-PERFORM
                    END-IF
                    PERFORM 010-EXIBIR THRU 010-99-FIM
                    MOVE 0 TO TECLA-EDIT
               WHEN TECLA-EDIT = 0
                AND COM-BARRA
                AND CARACTER NOT NUMERIC
                    PERFORM XE5 THRU FIM-XE5
               WHEN TECLA-EDIT = 0
                AND FLOAT
                AND (CARACTER = '+' OR '-')
                    PERFORM VARYING I FROM 10 BY -1
                            UNTIL I = 1
                            OR DATA-LK(FIELD)(I:1) NOT = SPACE
                          CONTINUE
                    END-PERFORM
                    IF DATA-LK(FIELD)(I:1) NUMERIC
                       ADD 1 TO I
                    END-IF
                    MOVE CARACTER TO DATA-LK(FIELD)(I:1)
                    PERFORM 010-EXIBIR THRU 010-99-FIM
               WHEN TECLA-EDIT = 0
                AND FLOAT
                AND(CARACTER = DP-F
                OR  (CARACTER NOT NUMERIC)
                AND (CARACTER NOT = ',')
                AND (CARACTER NOT = '.')
                OR (CARACTER NOT NUMERIC AND (COL-ED NOT < DP-F-P)))
                    PERFORM XE5 THRU FIM-XE5
               WHEN(TECLA-EDIT = 0
                 OR EDIT-CONTROL-O)
                AND((DATANAME-E (1: 7) = "CWCHECK" OR "CWRADIO")
                AND LEN-E = 1
                AND PIC-E = "X")
                    PERFORM 036-CHECK  THRU 036-99-FIM
               WHEN TECLA-EDIT = 0
                 OR EDIT-CONTROL-O
                    IF   INSERT-ON
                    OR   EDIT-CONTROL-O
                         MOVE DATA-LK(FIELD) (COL-ED: ) TO WORK-ED
                         IF   DATA-LK(FIELD) (LEN-E: 1) NOT = SPACE
                              PERFORM XE5 THRU FIM-XE5
                         END-IF
                         MOVE SPACES TO DATA-LK(FIELD) (COL-ED: )
                         COMPUTE I = COL-ED + 1
                         PERFORM UNTIL I > LEN-E
                                    OR PIC-E (I: 1) = "X"
                                 ADD 1 TO I
                         END-PERFORM
                         IF  I NOT > LEN-E
                             MOVE WORK-ED TO DATA-LK(FIELD) (I: )
                         ELSE
                             MOVE WORK-ED TO DATA-LK(FIELD) (COL-ED: )
                         END-IF
                    END-IF
                    IF   UPPLOW-E = "U"
                         INSPECT CARACTER CONVERTING
                                          MINUSCULAS TO MAIUSCULAS
                    ELSE
                         IF   UPPLOW-E = "L"
                              INSPECT CARACTER CONVERTING
                                               MAIUSCULAS TO MINUSCULAS
                         END-IF
                    END-IF
                    IF  DGS-CHAR = 0
                    AND((EDITCHAR = 'OFF')
                    AND(COL-ED = 1))
                    AND INSERT-OFF
                        MOVE 1 TO DGS-CHAR
                        MOVE CARACTER TO DATA-E (1:LEN-E)
                        MOVE DATA-E (1:LEN-E)
                          TO DATA-LK(FIELD)(1:LEN-E)
                    END-IF
                    IF  FLOAT
                    AND DATA-LK(FIELD) (COL-ED: 1) = '+' OR '-'
                        MOVE DATA-LK(FIELD) (COL-ED: 1)
                          TO DATA-LK(FIELD) (COL-ED + 1: 1)
                    END-IF
                    MOVE CARACTER TO  DATA-LK(FIELD) (COL-ED: 1)
                    IF   COL-ED < LEN-E
                         move 0 to att
                    end-if
                    IF  (COL-ED NOT > LEN-E)
                    AND  NOT EDIT-CONTROL-O
                         ADD 1 TO COL-ED CURPOS-COL
                    END-IF
                    MOVE 0 TO TECLA-EDIT
                    PERFORM 010-EXIBIR THRU 010-99-FIM
                    IF   COL-ED > LEN-E
                         IF   AUTO-E = "A"
      *                       SET EDIT-CURSOR-DOWN TO TRUE
                              SET EDIT-enter       TO TRUE
                              PERFORM 320-NEXT-GUIA THRU 320-99-FIM
                         ELSE
                              if att = 1
                                 PERFORM XE5 THRU FIM-XE5
                              else
                                 move 1 to att
                              end-if
                         END-IF
                    END-IF
               WHEN EDIT-BACKSPACE
                AND COL-ED < 2
                    MOVE 0 TO TECLA-EDIT
                    PERFORM XE5 THRU FIM-XE5
               WHEN (EDIT-DEL
                 OR EDIT-BACKSPACE)
                AND COM-BARRA
                    MOVE 0 TO TECLA-EDIT Y
                    MOVE SPACES TO WORK-ED
                    PERFORM VARYING I FROM 1 BY 1 UNTIL I = COL-ED
                            IF   DATA-LK(FIELD) (I: 1) NUMERIC
                            OR   DATA-LK(FIELD) (I: 1) = "+" OR "-"
                                 ADD 1 TO Y
                                 MOVE DATA-LK(FIELD) (I: 1)
                                   TO WORK-ED (Y: 1)
                            END-IF
                    END-PERFORM
                    INSPECT WORK-ED(Y: 1)
                            CONVERTING "123456789" TO "000000000"
                    perform test after
                            until not edit
                            SUBTRACT 1 FROM COL-ED CURPOS-COL
                            move DATA-LK(FIELD) (COL-ED:1)
                              to test-num
                    end-perform
                    INSPECT DATA-LK(FIELD) (1: COL-ED)
                            CONVERTING "123456789" TO "000000000"
                    MOVE COL-ED TO I
                    PERFORM VARYING I FROM I BY -1
                              UNTIL I = 0
                                    IF  NOT PIC-E (I: 1) = ("/" OR ":")
                                    AND Y > 0
                                        MOVE WORK-ED (Y: 1)
                                          TO DATA-LK(FIELD) (I: 1)
                                        SUBTRACT 1 FROM Y
                                    END-IF
                    END-PERFORM
                    PERFORM 070-EDIT THRU 070-99-FIM
               WHEN (EDIT-DEL
                 OR EDIT-BACKSPACE) *> pep
                AND NOT COM-BARRA
                    IF  EDIT-DEL
                    AND SECURE-E NOT = "S"
                        IF   DEL = LENGTH OF DELETADOS
                             PERFORM VARYING I FROM 2 BY 1 UNTIL I > DEL
                                     COMPUTE Y = I - 1
                                     MOVE DELETADOS (I: 1)
                                       TO DELETADOS (Y: 1)
                             END-PERFORM
                        ELSE
                             ADD 1 TO DEL
                        END-IF
                        MOVE DATA-LK(FIELD) (COL-ED: 1)
                          TO DELETADOS (DEL: 1)
                        PERFORM 240-DISPLAY-DELETADOS THRU 240-99-FIM
                    END-IF
                    IF  EDIT-BACKSPACE
                        PERFORM TEST AFTER
                               UNTIL COL-ED = 1
                                  OR PIC-E (COL-ED: 1) = "X"
                                     SUBTRACT 1 FROM COL-ED CURPOS-COL
                        END-PERFORM
                    END-IF
                    COMPUTE I = COL-ED + 1
                    PERFORM UNTIL I > LEN-E
                               OR PIC-E (I: 1) = "X"
                                  ADD 1 TO I
                    END-PERFORM
                    MOVE DATA-LK(FIELD) (I: ) TO WORK-ED
                    MOVE WORK-ED    TO DATA-LK(FIELD) (COL-ED: )
                    MOVE 0          TO TECLA-EDIT
                    PERFORM 010-EXIBIR THRU 010-99-FIM
               WHEN EDIT-CURSOR-LEFT
                    IF  COL-ED > 1
                        SUBTRACT 1 FROM COL-ED CURPOS-COL
                        MOVE 999 TO TECLA-EDIT
                    ELSE
                        SET EDIT-CURSOR-UP TO TRUE
                        PERFORM 330-UP-GUIA THRU 330-99-FIM
                        MOVE 1 TO FROM-LEFT
                    END-IF
               WHEN EDIT-CURSOR-RIGHT
                    COMPUTE COL-ED-1 = COL-ED + 1
                    IF  COL-ED < LEN-E
                    AND((DATA-LK(FIELD) (COL-ED-1: ) NOT = SPACES)
                    OR  (DATA-LK(FIELD) (COL-ED: 1) NOT = SPACE))
                        ADD  1 TO COL-ED CURPOS-COL
                        MOVE 0 TO TECLA-EDIT
                    ELSE
      *                 SET EDIT-CURSOR-DOWN TO TRUE
                        SET EDIT-enter       TO TRUE
                        PERFORM 320-NEXT-GUIA THRU 320-99-FIM
                    END-IF
               WHEN EDIT-HOME
                    IF   COL-ED > 1
                         PERFORM UNTIL COL-ED = 1
                                 SUBTRACT 1 FROM COL-ED CURPOS-COL
                         END-PERFORM
                         MOVE 0 TO TECLA-EDIT
                         PERFORM 010-EXIBIR THRU 010-99-FIM
                    END-IF
               WHEN EDIT-END AND CWENDK = "ON"
                    CONTINUE
               WHEN EDIT-END
                    COMPUTE COL-END = COL-E + LEN-E - 1
                    IF   CURPOS-COL < COL-END
                         MOVE COL-ED  TO COL-SAVE
                         MOVE COL-END TO CURPOS-COL
                         MOVE LEN-E   TO COL-ED
                         PERFORM UNTIL COL-ED = 1
                             OR (PIC-E (COL-ED: 1) = "X")
                            AND (DATA-LK(FIELD) (COL-ED: 1) NOT = SPACE)
                                 SUBTRACT 1 FROM COL-ED CURPOS-COL
                         END-PERFORM
                         IF (DATA-LK(FIELD) (COL-ED: 1) NOT = SPACE)
                         AND COL-ED < LEN-E
                             ADD 1 TO COL-ED CURPOS-COL
                         END-IF
                         MOVE 0 TO TECLA-EDIT
                         IF   COL-ED NOT = COL-SAVE
                              PERFORM 010-EXIBIR THRU 010-99-FIM
                         ELSE
                              IF  FIELD = FIELD-LAST
                                  PERFORM XE5 THRU FIM-XE5
                                  PERFORM 010-EXIBIR THRU 010-99-FIM
                              ELSE
                                  PERFORM 350-BOT-GUIA THRU 350-99-FIM
                                  SET EDIT-CURSOR-UP TO TRUE
                                  MOVE 1 TO FROM-LEFT
                              END-IF
                         END-IF
                    ELSE
                         IF   FIELD = FIELD-LAST
                              PERFORM XE5 THRU FIM-XE5
                              MOVE 0 TO TECLA-EDIT
                              PERFORM 010-EXIBIR THRU 010-99-FIM
                         ELSE
                              PERFORM 350-BOT-GUIA THRU 350-99-FIM
                              SET EDIT-CURSOR-UP TO TRUE
                              MOVE 1 TO FROM-LEFT
                         END-IF
                    END-IF
               WHEN EDIT-CONTROL-CURSOR-LEFT
                    PERFORM 200-CONTRL-LEFT THRU 200-99-FIM
               WHEN EDIT-CONTROL-CURSOR-RIGHT
                    PERFORM 210-CONTROL-RIGHT THRU 210-99-FIM
           END-EVALUATE.

       040-99-FIM. EXIT.

       050-NUMERICO.

           MOVE CARACTER TO TEST-NUM
           IF (TEST-NUM = '-' OR '+')
           OR((TECLA-EDIT NOT = 0) AND (NOT EDIT-CURSOR-RIGHT))
               MOVE 1 TO DGS
           END-IF
           IF   AUTO-E = "A"
           AND  CARACTER NUMERIC
                ADD 1 TO DIGITOS
           END-IF
           IF  DGS = 0
           AND((EDITNUM NOT = 'ON')
                         OR (DECIMAIS = 1 AND DATA-E = SPACES))
      *    AND (NUM OR EDIT-CURSOR-RIGHT)
           AND NUM
           AND ((CARACTER NOT = ',') AND (CARACTER NOT = '.'))
               MOVE 1 TO DGS
               INSPECT DATA-E
                       CONVERTING "123456789" TO "000000000"
               MOVE DATA-E TO DATA-LK(FIELD)
               PERFORM 070-EDIT THRU 070-99-FIM
               MOVE CARACTER TO TEST-NUM
           END-IF
           IF DECIMAIS = 1
              PERFORM VARYING I FROM 1 BY 1 UNTIL PIC-E(I:1) = SPACE
                      IF PIC-E(I:1) = '9'
                      AND DATA-LK(FIELD)(I:1) = SPACE
                          MOVE '0' TO DATA-LK(FIELD)(I:1)
                      END-IF
              END-PERFORM
              IF  DATA-LK(FIELD) NOT = DATA-E
                  PERFORM 010-EXIBIR THRU 010-99-FIM
              END-IF
           END-IF
           IF  TECLA-EDIT = 0
           AND (VIRGULA OR SINAL)
           AND DECIMAIS = 1
               MOVE    COL-ED-V     TO COL-ED
               MOVE    CURPOS-COL-V TO CURPOS-COL
               PERFORM VARYING I FROM 1 BY 1
                       UNTIL DATA-LK(FIELD)(I:1) = DP
                        CONTINUE
               END-PERFORM
               PERFORM VARYING I FROM I BY 1
                       UNTIL DATA-LK(FIELD)(I:1) = SPACE
                        CONTINUE
               END-PERFORM
               MOVE '0' TO DATA-LK(FIELD)(I:1)
           END-IF
           EVALUATE TRUE
               WHEN TECLA-EDIT = 0
                AND VIRGULA
                AND DECIMAIS = 1
                    MOVE    COL-ED-V     TO COL-ED
                    MOVE    CURPOS-COL-V TO CURPOS-COL
                    PERFORM 070-EDIT   THRU 070-99-FIM
               WHEN EDIT-CONTROL-Z
                    INSPECT DATA-LK(FIELD) (COL-ED: )
                            CONVERTING "123456789" TO "000000000"
                    PERFORM 070-EDIT THRU 070-99-FIM
                    MOVE 0 TO TECLA-EDIT
               WHEN EDIT-CONTROL-X
                    INSPECT DATA-LK(FIELD)
                            CONVERTING "123456789" TO "000000000"
                    MOVE COL-ED-N     TO COL-ED
                    IF   CURPOS-COL-N not = 0
                         MOVE CURPOS-COL-N TO CURPOS-COL
                    end-if
                    IF  SINAL-ATUAL = "-"
                        MOVE "+" TO SINAL-ATUAL
                    END-IF
                    IF  SINAL-ATUAL = "DB"
                        MOVE "CR" TO SINAL-ATUAL
                    END-IF
                    PERFORM 070-EDIT THRU 070-99-FIM
                    MOVE 0 TO TECLA-EDIT
               WHEN TECLA-EDIT = 0
                AND NOT VALID
                    PERFORM XE5 THRU FIM-XE5
               WHEN TECLA-EDIT = 0
                AND CARACTER = "+" OR "-"
                    IF  COM-SINAL = 0
                        PERFORM XE5 THRU FIM-XE5
                    ELSE
                        EVALUATE SINAL-ATUAL ALSO CARACTER
                            WHEN "+" ALSO "-"
                                  MOVE "-" TO SINAL-ATUAL
                            WHEN "-" ALSO "+"
                                  MOVE "+" TO SINAL-ATUAL
                            WHEN "CR" ALSO "-"
                                  MOVE "DB" TO SINAL-ATUAL
                            WHEN "DB" ALSO "+"
                                  MOVE "CR" TO SINAL-ATUAL
                        END-EVALUATE
                        PERFORM 070-EDIT THRU 070-99-FIM
                    END-IF
               WHEN TECLA-EDIT = 0
                AND VALID
                AND COL-ED = COL-ED-N
                AND DECIMAIS = 1
                AND ((DATA-LK(FIELD) (1: 1) = "1" OR "2" OR "3" OR "4"
                                   OR "5" OR "6" OR "7" OR "8" OR "9")
                 OR  (DATA-LK(FIELD) (2: 1) = "1" OR "2" OR "3" OR "4"
                                   OR "5" OR "6" OR "7" OR "8" OR "9")
                     AND SINAL-FLUTUANTE = 1)
                    IF  COL-ED < LEN-E
                        ADD 1 TO COL-ED CURPOS-COL
                        GO TO 050-NUMERICO
                    ELSE
                        PERFORM XE5 THRU FIM-XE5
                    END-IF
               WHEN TECLA-EDIT = 0
                    IF (PIC-E(1:1) NOT = "-")
                    AND (PIC-E(1:1) NOT = "+")
                        MOVE PIC-E (COL-ED: 1) TO TEST-NUM
                        PERFORM UNTIL NOT SINAL
                                SUBTRACT 1 FROM COL-ED
                                                CURPOS-COL
                                MOVE PIC-E (COL-ED: 1) TO TEST-NUM
                        END-PERFORM
                        MOVE PIC-E (LEN-E: 1) TO TEST-NUM
                        PERFORM UNTIL NOT SINAL
                                SUBTRACT 1 FROM LEN-E
                                MOVE PIC-E (LEN-E: 1) TO TEST-NUM
                        END-PERFORM
                    END-IF
                    MOVE DATA-LK(FIELD) (COL-ED: 1) TO TEST-NUM
                    EVALUATE TRUE
                        WHEN COL-ED = LEN-E
                         AND DATA-LK(FIELD) (1: 1) > "0"
                         AND DATA-LK(FIELD) (1: 1) < ":"
                             MOVE CARACTER TO DATA-LK(FIELD) (LEN-E: 1)
                             PERFORM 070-EDIT THRU 070-99-FIM
                             IF   AUTO-E = "A"
      *                           SET EDIT-CURSOR-DOWN TO TRUE
                                  SET EDIT-enter       TO TRUE
                                  PERFORM 320-NEXT-GUIA THRU 320-99-FIM
                             ELSE
                                  PERFORM XE5 THRU FIM-XE5
                             END-IF
                        WHEN COL-ED = LEN-E
                         AND DECIMAIS = 0
                             MOVE SPACES TO WORK-ED
                             MOVE 0      TO Y I
                             PERFORM VARYING I FROM 2 BY 1
                                       UNTIL I > COL-ED
                                     IF   DATA-LK(FIELD) (I: 1) NUMERIC
                                          ADD 1 TO Y
                                          MOVE DATA-LK(FIELD) (I: 1)
                                            TO WORK-ED (Y: 1)
                                     END-IF
                             END-PERFORM
                             if insert-off
                             and editnum = 'ON'
                             and y > 1
                             and digitos = 1
                                 subtract 1 from y
                             end-if
                             MOVE LEN-E TO I
                             PERFORM UNTIL Y = 0
                                     SUBTRACT 1 FROM I
                                     MOVE PIC-E (I: 1) TO TEST-NUM
                                     IF  MASK
                                         MOVE WORK-ED (Y: 1) TO
                                              DATA-LK(FIELD) (I: 1)
                                         SUBTRACT 1 FROM Y
                                     ELSE
                                         MOVE PIC-E (I: 1)
                                           TO DATA-LK(FIELD) (I: 1)
                                     END-IF
                             END-PERFORM
                             MOVE CARACTER TO DATA-LK(FIELD) (LEN-E: 1)
                             PERFORM 070-EDIT THRU 070-99-FIM
                             if digitos not = len-e
                                move 0 to att
                             end-if
                             IF (DATA-LK(FIELD) (1: 1) > "0"
                             AND DATA-LK(FIELD) (1: 1) < ":")
                             or digitos = len-e
                                 IF   AUTO-E = "A"
      *                               SET EDIT-CURSOR-DOWN TO TRUE
                                      SET EDIT-enter       TO TRUE
                                      PERFORM 320-NEXT-GUIA
                                         THRU 320-99-FIM
                                 ELSE
                                      if att = 1
                                         PERFORM XE5 THRU FIM-XE5
                                      else
                                         move 1 to att
                                      end-if
                                 END-IF
                             END-IF
                        WHEN COL-ED = COL-ED-N
                         AND DECIMAIS = 1
                             MOVE SPACES TO WORK-ED
                             MOVE 0      TO Y I
                             PERFORM VARYING I FROM 1 BY 1
                                       UNTIL I = COL-ED
                                     IF   DATA-LK(FIELD) (I: 1) = "0"
                                     AND  Y = 0
                                          MOVE SPACE
                                            TO DATA-LK(FIELD) (I: 1)
                                     END-IF
                                     IF   DATA-LK(FIELD) (I: 1) NUMERIC
                                          ADD 1 TO Y
                                          MOVE DATA-LK(FIELD) (I: 1)
                                            TO WORK-ED (Y: 1)
                                     END-IF
                             END-PERFORM
                             COMPUTE I = COL-ED - 1
                             if insert-off
                             and editnum = 'ON'
                             and y > 1
                             and digitos = 1
                                 subtract 1 from y
                             end-if
                             PERFORM UNTIL Y = 0
                                     SUBTRACT 1 FROM I
                                     MOVE PIC-E (I: 1) TO TEST-NUM
                                     IF  MASK
                                         MOVE WORK-ED (Y: 1) TO
                                              DATA-LK(FIELD) (I: 1)
                                         SUBTRACT 1 FROM Y
                                     ELSE
                                         MOVE PIC-E (I: 1)
                                           TO DATA-LK(FIELD) (I: 1)
                                     END-IF
                             END-PERFORM
                             COMPUTE Y = COL-ED - 1
                             MOVE CARACTER TO DATA-LK(FIELD) (Y: 1)
                             IF NOT (DGS = 0
                             AND EDITNUM = 'ON')
                                 compute yj = j + 1
                                 compute lenj = len-e - yj + 1
                                 inspect data-lk(field) (yj: lenj)
                                         converting space to zero
                             END-IF
                             PERFORM 070-EDIT THRU 070-99-FIM
                        WHEN OTHER
                             IF   NOT INSERT-ON
                                  COMPUTE COL-ED-1 = COL-ED + 1
                             END-IF
                             IF  COL-ED-1 = COL-ED-N
                             AND DECIMAIS = 1
                                 MOVE 2 TO DECIMAIS
                             END-IF
                             IF   INSERT-ON
                             AND((COL-ED NOT > COL-ED-N)
                             OR   DECIMAIS = 0)
                                  MOVE DATA-LK(FIELD) (COL-ED: 1)
                                    TO INSERT-CHAR
                             END-IF
                             MOVE CARACTER TO DATA-LK(FIELD) (COL-ED: 1)
                             If decimais not = 0
                                move col-ed to i
                                perform until i > len-e
                                        add 1 to i
                                        if i not > len-e
                                      and(DATA-LK(FIELD) (i: 1) = space)
                                           move "0"
                                             to DATA-LK(FIELD) (i: 1)
                                        end-if
                                end-perform
                             end-if
                             PERFORM 070-EDIT THRU 070-99-FIM
                             perform 080-VERIFICA THRU 080-99-FIM
                             IF    DECIMAIS = 2
                                   MOVE 1 TO DECIMAIS
                                   ADD 1  TO COL-ED CURPOS-COL
                             ELSE
                             IF  COL-ED < LEN-E
                                 IF FROM-INSERT = 0
                                 PERFORM TEST AFTER
                                   UNTIL COL-ED NOT < LEN-E
                                      OR TEST-NUM NUMERIC
                                         ADD 1 TO COL-ED CURPOS-COL
                                         MOVE DATA-LK(FIELD) (COL-ED: 1)
                                           TO TEST-NUM
                                 END-PERFORM
                                 END-IF
                                 MOVE 0 TO FROM-INSERT
                             ELSE
                                 IF   AUTO-E = "A"
      *                               SET EDIT-CURSOR-DOWN TO TRUE
                                      SET EDIT-enter       TO TRUE
                                      PERFORM 320-NEXT-GUIA
                                         THRU 320-99-FIM
                                 ELSE
                                      PERFORM XE5 THRU FIM-XE5
                                 END-IF
                             END-IF
                             END-IF
                    END-EVALUATE
               WHEN EDIT-BACKSPACE
                AND COL-ED < 2
                    MOVE 0 TO TECLA-EDIT
                    PERFORM XE5 THRU FIM-XE5
               WHEN (EDIT-BACKSPACE OR EDIT-DEL)
                AND(COL-ED NOT = COL-ED-N)
                    IF digitos not = 0
                       subtract 1 from digitos
                    end-if
                    ADD 1 COL-ED GIVING I
                    PERFORM TEST AFTER UNTIL MASK
                            SUBTRACT 1 FROM I
                            MOVE PIC-E (I: 1) TO TEST-NUM
                            IF  MASK AND (NOT SINAL)
                                MOVE "@" TO DATA-LK(FIELD) (I: 1)
                            ELSE
                                MOVE SPACE TO TEST-NUM
                            END-IF
                    END-PERFORM
                    MOVE 0 TO TECLA-EDIT
                    PERFORM 070-EDIT THRU 070-99-FIM
               WHEN EDIT-DEL
                AND COL-ED > COL-ED-N
                    COMPUTE I = COL-ED + 1
                    MOVE DATA-LK(FIELD) (I: ) TO WORK-ED
                    MOVE WORK-ED TO DATA-LK(FIELD) (COL-ED: )
                    MOVE "0"     TO DATA-LK(FIELD) (I: )
                    MOVE 0 TO TECLA-EDIT
                    PERFORM 070-EDIT THRU 070-99-FIM
               WHEN (EDIT-DEL OR EDIT-BACKSPACE)
                AND DECIMAIS = 0
                AND COL-ED = LEN-E
                    MOVE 0      TO TECLA-EDIT
                    if  digitos not = 0
                        subtract 1 from digitos
                    end-if
                    IF  LEN-E > 1
                        MOVE "0"    TO WORK-ED
                        MOVE 1      TO Y
                        PERFORM VARYING I FROM 1 BY 1
                                  UNTIL I = LEN-E
                            IF   DATA-LK(FIELD) (I: 1) = "0"
                            AND  Y = 0
                                 MOVE SPACE
                                   TO DATA-LK(FIELD) (I: 1)
                            END-IF
                                  IF   DATA-LK(FIELD) (I: 1) NUMERIC
                                       ADD 1 TO Y
                                       MOVE DATA-LK(FIELD) (I: 1)
                                         TO WORK-ED (Y: 1)
                                  END-IF
                        END-PERFORM
                        MOVE SPACES TO DATA-LK(FIELD)
                        PERFORM VARYING I FROM LEN-E BY -1
                                  UNTIL (I = 0 OR Y = 0)
                                MOVE PIC-E (I: 1) TO TEST-NUM
                                IF  MASK
                                    MOVE WORK-ED (Y: 1)
                                      TO DATA-LK(FIELD) (I: 1)
                                    SUBTRACT 1 FROM Y
                                ELSE
                                    MOVE TEST-NUM
                                      TO DATA-LK(FIELD) (I: 1)
                                END-IF
                        END-PERFORM
                    ELSE
                        MOVE "0" TO DATA-LK(FIELD) (1: 1)
                    END-IF
                    PERFORM 070-EDIT THRU 070-99-FIM
               WHEN (EDIT-DEL
                 OR EDIT-BACKSPACE)
                AND COL-ED > 1
                    MOVE 0      TO TECLA-EDIT
                    MOVE SPACES TO WORK-ED
                    MOVE 0      TO Y I
                    COMPUTE COL-ED-1 = COL-ED
                    PERFORM VARYING I FROM 1 BY 1
                              UNTIL I = COL-ED-1
                            IF   DATA-LK(FIELD) (I: 1) NUMERIC
                                 ADD 1 TO Y
                                 MOVE DATA-LK(FIELD) (I: 1)
                                   TO WORK-ED (Y: 1)
                            END-IF
                    END-PERFORM
                    If  DECIMAIS > 0
                        PERFORM VARYING COMMA-POSIT FROM LEN-E BY -1
                                    UNTIL COMMA-POSIT = 0
                                OR DATA-LK(FIELD) (COMMA-POSIT:1 ) = DP
                             CONTINUE
                        END-PERFORM
                        IF COMMA-POSIT > 0
                        AND COL-ED = COMMA-POSIT
                           MOVE SPACES TO WORK-ED (Y: 1)
                           SUBTRACT 1 FROM Y
                        END-IF
                    END-IF
                    COMPUTE I = COL-ED + 1
                    PERFORM UNTIL Y = 0
                            SUBTRACT 1 FROM I
                            MOVE PIC-E (I: 1) TO TEST-NUM
                            IF  MASK
                                MOVE WORK-ED (Y: 1) TO
                                     DATA-LK(FIELD) (I: 1)
                                SUBTRACT 1 FROM Y
                            ELSE
                                MOVE PIC-E (I: 1)
                                  TO DATA-LK(FIELD) (I: 1)
                                  IF DP = '.'
                                     INSPECT DATA-LK(FIELD) (I: 1)
                                             CONVERTING ".," TO ",."
                                  END-IF
                            END-IF
                    END-PERFORM
                    PERFORM TEST AFTER UNTIL MASK
                            SUBTRACT 1 FROM I
                            MOVE PIC-E (I: 1) TO TEST-NUM
                            IF  MASK
                                MOVE "0" TO DATA-LK(FIELD) (I: 1)
                            ELSE
                                MOVE SPACE TO TEST-NUM
                            END-IF
                    END-PERFORM
                    PERFORM 070-EDIT THRU 070-99-FIM
               WHEN EDIT-CURSOR-LEFT
                    IF  COL-ED > 1
                        MOVE COL-ED TO COL-ED-1
                        PERFORM TEST AFTER UNTIL COL-ED = 1
                                              OR TEST-NUM NUMERIC
                                              OR TEST-NUM = SPACE
                                SUBTRACT 1 FROM COL-ED CURPOS-COL
                                MOVE DATA-LK(FIELD) (COL-ED: 1)
                                  TO TEST-NUM
                        END-PERFORM
                        MOVE 999 TO TECLA-EDIT
                        IF   DATA-LK(FIELD) (COL-ED-1: 1) = SPACE
                        AND (EDITNUM NOT = 'ON')
                             SET EDIT-CURSOR-UP TO TRUE
                             PERFORM 330-UP-GUIA THRU 330-99-FIM
                             MOVE 1 TO FROM-LEFT
                        END-IF
                    ELSE
                        SET EDIT-CURSOR-UP TO TRUE
                        PERFORM 330-UP-GUIA THRU 330-99-FIM
                        MOVE 1 TO FROM-LEFT
                    END-IF
               WHEN EDIT-CURSOR-RIGHT
                    COMPUTE COL-ED-1 = COL-ED + 1
                    IF  COL-ED < LEN-E
                    AND((DATA-LK(FIELD) (COL-ED-1: ) NOT = SPACES)
                    OR  (DATA-LK(FIELD) (COL-ED: 1) NOT = SPACE))
                        ADD  1 TO COL-ED CURPOS-COL
                        MOVE 0 TO TECLA-EDIT
                    ELSE
      *                 SET EDIT-CURSOR-DOWN TO TRUE
                        SET EDIT-enter       TO TRUE
                        PERFORM 320-NEXT-GUIA THRU 320-99-FIM
                    END-IF
               WHEN EDIT-HOME
                    IF   COL-ED > 1
                         PERFORM UNTIL COL-ED = 1
                                 SUBTRACT 1 FROM COL-ED CURPOS-COL
                         END-PERFORM
                         MOVE 0 TO TECLA-EDIT
                         PERFORM 070-EDIT THRU 070-99-FIM
                    END-IF
               WHEN EDIT-END AND CWENDK = "ON"
                    CONTINUE
               WHEN EDIT-END
                    COMPUTE COL-END = COL-E + LEN-E - 1
                    IF   CURPOS-COL < COL-END
                         MOVE COL-ED  TO COL-SAVE
                         MOVE COL-END TO CURPOS-COL
                         MOVE LEN-E   TO COL-ED
                         PERFORM UNTIL COL-ED = 1
                             OR (PIC-E (COL-ED: 1) = "X")
                            AND (DATA-LK(FIELD) (COL-ED: 1) NOT = SPACE)
                                 SUBTRACT 1 FROM COL-ED CURPOS-COL
                         END-PERFORM
                         IF (DATA-LK(FIELD) (COL-ED: 1) NOT = SPACE)
                         AND COL-ED < LEN-E
                             ADD 1 TO COL-ED CURPOS-COL
                         END-IF
                         MOVE 0 TO TECLA-EDIT
                         IF   COL-ED NOT = COL-SAVE
                              PERFORM 070-EDIT THRU 070-99-FIM
                         ELSE
                              IF  FIELD = FIELD-LAST
                                  PERFORM XE5 THRU FIM-XE5
                                  PERFORM 070-EDIT THRU 070-99-FIM
                              ELSE
                                  MOVE FIELDS TO FIELD
                                  SET EDIT-CURSOR-UP TO TRUE
                              END-IF
                         END-IF
                    ELSE
                         IF   FIELD = FIELD-LAST
                              PERFORM XE5 THRU FIM-XE5
                              MOVE 0 TO TECLA-EDIT
                              PERFORM 070-EDIT THRU 070-99-FIM
                         ELSE
                              MOVE FIELDS TO FIELD
                              SET EDIT-CURSOR-UP TO TRUE
                         END-IF
                    END-IF
               WHEN EDIT-CONTROL-CURSOR-LEFT
                    PERFORM 200-CONTRL-LEFT THRU 200-99-FIM
               WHEN EDIT-CONTROL-CURSOR-RIGHT
                    PERFORM 210-CONTROL-RIGHT THRU 210-99-FIM
           END-EVALUATE.

       050-99-FIM. EXIT.

       070-EDIT.

           MOVE SPACES TO WORK-ED
           MOVE 0      TO Y ALGO P-SINAL
           MOVE 1      TO SX
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN-E
                   IF   DATA-LK(FIELD) (I: 1) = DP
                        MOVE I TO P-SINAL
                   END-IF
                   IF   DATA-LK(FIELD) (I: 1) = SPACE
                   AND  ALGO = 0
                        MOVE "0" TO DATA-LK(FIELD) (I: 1)
                   END-IF
                   IF   DATA-LK(FIELD) (I: 1) = "@"
                   OR  (DATA-LK(FIELD) (I: 1) = "0" AND Y = 0)
                        IF   DATA-LK(FIELD) (I: 1) = "@"
                             MOVE 1 TO Y-MINUS
                        END-IF
                        IF  NOT BARRA-LK (FIELD)
                            EXIT PERFORM CYCLE
                        END-IF
                   END-IF
                   move DATA-LK(FIELD) (I: 1) to test-num
                   if  (not sinal)
                   and (not virgula)
                      ADD 1 TO ALGO
                   end-if
                   IF   DATA-LK(FIELD) (I: 1) NUMERIC
                        ADD  1                     TO Y
                        MOVE DATA-LK(FIELD) (I: 1) TO WORK-ED (Y: 1)
                        IF INSERT-CHAR NOT = SPACE
                           IF I = COL-ED
                           AND I < LEN-E
                               ADD  1           TO Y
                               MOVE INSERT-CHAR TO WORK-ED (Y: 1)
                               MOVE SPACES      TO INSERT-CHAR
                               MOVE 1           TO FROM-INSERT
                           END-IF
                        END-IF
                   ELSE
                        IF   DATA-LK(FIELD) (I: 1) = "+" OR "-"
                                           OR "C" OR "D" OR "R" OR "B"
                             ADD  1        TO Y
                        ELSE
                             IF PIC-E (I: 1) = '9'
                             AND WORK-ED = SPACES
                                  ADD 1    TO Y
                                  MOVE '0' TO WORK-ED (Y: 1)
                            END-IF
                        END-IF
                   END-IF
           END-PERFORM
           IF  DECIMAIS = 1
           AND Y-MINUS = 1
           AND COM-SINAL = 0
               ADD 1    TO Y
               IF  P-SINAL < COL-ED
               AND WORK-ED (Y: 1) = SPACE
                   MOVE '0' TO WORK-ED (Y: 2)
               ELSE
                   move 0 to y-minus
                   SUBTRACT 1 FROM Y
               END-IF
           END-IF
           MOVE SPACES      TO INSERT-CHAR
           IF  Y = 0
               if caracter not = "-"
                  MOVE 1 TO REMOVE-SINAL
               end-if
               MOVE 0 TO Y-MINUS
           ELSE
               MOVE 0 TO REMOVE-SINAL
           END-IF
           SUBTRACT Y-MINUS FROM Y LEN-E
           MOVE SPACES TO DATA-LK(FIELD)
           PERFORM VARYING I FROM LEN-E BY -1 UNTIL I = 0
                               OR Y = 0
                   MOVE PIC-E (I: 1) TO TEST-NUM
                   IF  MASK
                   AND ((NOT SINAL) OR WORK-ED (Y: 1) NUMERIC)
                       MOVE WORK-ED (Y: 1) TO DATA-LK(FIELD) (I: 1)
                       SUBTRACT 1 FROM Y
                       if y = 0 and i > 1
                          subtract 1 from i
                          if pic-e(I:1) = DP
                             move dp to DATA-LK(FIELD) (I: 1)
                          end-if
                       end-if
                   ELSE
                       IF   PIC-E (I: 1) NOT = "B"
                       AND  (NOT SINAL)
                            MOVE PIC-E (I: 1) TO DATA-LK(FIELD) (I: 1)
                       END-IF
                       if sinal
                          subtract 1 from Y
                       end-if
                   END-IF
           END-PERFORM
           IF (SINAL-MASK = "-"  AND (SINAL-ATUAL = "+" OR SPACES))
           OR (SINAL-MASK = "DB" AND (SINAL-ATUAL = "CR" OR SPACES))
               MOVE 1 TO REMOVE-SINAL
           END-IF
           MOVE 0 TO Y
           MOVE 1 TO SX
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN-E
                   MOVE DATA-LK(FIELD) (I: 1) TO TEST-NUM
                   IF  VALOR
                       COMPUTE I = LEN-E + 1
                   ELSE
                       MOVE PIC-E (I: 1) TO TEST-NUM
                       IF  SINAL
                           MOVE SINAL-ATUAL (SX: 1) TO TEST-NUM
                           ADD  1                   TO SX
                       END-IF
                       IF  NOT ZERO-OFF
                           MOVE 1 TO Y
                       END-IF
                       IF  Y = 0
                           MOVE SPACE TO DATA-LK(FIELD) (I: 1)
                       ELSE
                           IF   EDIT
                                IF  TEST-NUM = "B"
                                    MOVE SPACE TO TEST-NUM
                                END-IF
                                MOVE TEST-NUM TO DATA-LK(FIELD) (I: 1)
                           END-IF
                       END-IF
                   END-IF
           END-PERFORM

           ADD  Y-MINUS TO LEN-E
           MOVE 0       TO Y-MINUS

           IF  COM-SINAL NOT = 0
               IF  SINAL-FLUTUANTE = 1
                   PERFORM VARYING I FROM 1 BY 1
                            UNTIL I = LEN-E
                               OR DATA-LK(FIELD) (I: 1) NUMERIC
                           CONTINUE
                   END-PERFORM
                   PERFORM TEST AFTER
                           UNTIL I = 1
                              OR DATA-LK(FIELD) (I: 1) NOT = "."
                       IF I > 1
                       AND(DATA-LK(FIELD) (I: 1) = '.'
                       OR  DATA-LK(FIELD) (I: 1) NUMERIC)
                           SUBTRACT 1 FROM I
                       END-IF
                   END-PERFORM
                   IF  REMOVE-SINAL = 0
                       MOVE SINAL-ATUAL TO DATA-LK(FIELD) (I: 1)
                   END-IF
                   IF SINAL-FLUTUANTE = 1
                      PERFORM VARYING I FROM 1 BY 1
                              UNTIL I > LEN-LK  (FIELD)
                               OR (DATA-LK(FIELD) (I: 1) NUMERIC
                              AND  DATA-LK(FIELD) (I: 1) > '0')
                         IF   DATA-LK(FIELD) (I: 1) = ZERO
                              OR  "-" OR "+" OR "."
                              MOVE SPACE TO DATA-LK(FIELD) (I: 1)
                         END-IF
                      END-PERFORM
                      SUBTRACT 1  FROM I
                      IF DATA-LK(FIELD) (I: 1) = ','
                         SUBTRACT 1 FROM I
                      END-IF
                      MOVE SINAL-ATUAL TO DATA-LK(FIELD) (I: 1)
                      perform 080-VERIFICA THRU 080-99-FIM
                   END-IF
               ELSE
                   if  SINAL-POSIT > 0
                       PERFORM VARYING TS FROM 2 BY -1 UNTIL TS = 1
                                   OR SINAL-ATUAL (TS: 1) NOT = SPACE
                               CONTINUE
                       END-PERFORM
                       MOVE SINAL-ATUAL (1: TS)
                         TO DATA-LK(FIELD) (SINAL-POSIT: TS)
                   END-IF
               END-IF
               IF   DP = "."
                    INSPECT DATA-LK (FIELD) (1: LEN-E)
                            CONVERTING ",." TO ".,"
               END-IF
               IF  REMOVE-SINAL = 1
                   INSPECT DATA-LK (FIELD) (1:)
                           CONVERTING SINAL-ATUAL TO SPACE
               END-IF
               PERFORM 010-EXIBIR  THRU 010-99-FIM
           ELSE
               IF   DP = "."
                    INSPECT DATA-LK (FIELD) (1: LEN-E)
                            CONVERTING ",." TO ".,"
               END-IF
               PERFORM 010-EXIBIR  THRU 010-99-FIM
           END-IF.

       070-99-FIM. EXIT.

       080-VERIFICA.

           MOVE SPACE TO TIROU

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > LEN-LK  (FIELD)
              IF  (DATA-LK(FIELD) (I: 1) = SPACE
              OR                           "-"
              OR                           "+")
              AND  PIC-LK (FIELD) (I: 1) = "9"
                   IF  DATA-LK(FIELD) (I: 1) NOT = SPACE
                       MOVE DATA-LK(FIELD) (I: 1)
                         TO TIROU
                   END-IF
                   MOVE "0" TO DATA-LK(FIELD) (I: 1)
              END-IF
           END-PERFORM

           IF TIROU NOT = SPACE
              PERFORM VARYING I FROM LEN-LK(FIELD) BY -1
                      UNTIL I = 0
                      IF DATA-LK(FIELD) (I: 1) = SPACE
                      AND (PIC-E (I: 1) = '-' OR '+')
                         MOVE TIROU TO DATA-LK(FIELD)(I: 1)
                         MOVE SPACE TO TIROU
                      END-IF
              END-PERFORM
           END-IF

           IF DATA-LK(FIELD) NOT = DATA-E
              IF NAO-EXIBE-VERIFICA = 1
                 MOVE DATA-LK(FIELD) TO DATA-E
              ELSE
                 PERFORM 010-EXIBIR THRU 010-99-FIM
              END-IF
           END-IF.

       080-99-FIM. EXIT.

       100-CONVERTE-TECLA.

           EVALUATE TRUE
            WHEN TECLA > 0
                 CONTINUE
            WHEN EDIT-CURSOR-DOWN       SET CURSOR-DOWN        TO TRUE
            WHEN EDIT-ENTER             SET ENTER-KEY          TO TRUE
            WHEN EDIT-TAB               SET TAB                TO TRUE
            WHEN EDIT-ESC               SET ESC                TO TRUE
            WHEN EDIT-F1                SET F1                 TO TRUE
            WHEN EDIT-F2                SET F2                 TO TRUE
            WHEN EDIT-F3                SET F3                 TO TRUE
            WHEN EDIT-F4                SET F4                 TO TRUE
            WHEN EDIT-F5                SET F5                 TO TRUE
            WHEN EDIT-F6                SET F6                 TO TRUE
            WHEN EDIT-F7                SET F7                 TO TRUE
            WHEN EDIT-F8                SET F8                 TO TRUE
            WHEN EDIT-F9                SET F9                 TO TRUE
            WHEN EDIT-F10               SET F10                TO TRUE
            WHEN EDIT-SHIFT-F1          SET SHIFT-F1           TO TRUE
            WHEN EDIT-SHIFT-F2          SET SHIFT-F2           TO TRUE
            WHEN EDIT-SHIFT-F3          SET SHIFT-F3           TO TRUE
            WHEN EDIT-SHIFT-F4          SET SHIFT-F4           TO TRUE
            WHEN EDIT-SHIFT-F5          SET SHIFT-F5           TO TRUE
            WHEN EDIT-SHIFT-F6          SET SHIFT-F6           TO TRUE
            WHEN EDIT-SHIFT-F7          SET SHIFT-F7           TO TRUE
            WHEN EDIT-SHIFT-F8          SET SHIFT-F8           TO TRUE
            WHEN EDIT-SHIFT-F9          SET SHIFT-F9           TO TRUE
            WHEN EDIT-SHIFT-F10         SET SHIFT-F10          TO TRUE
            WHEN EDIT-CONTROL-F1        SET CONTROL-F1         TO TRUE
            WHEN EDIT-CONTROL-F2        SET CONTROL-F2         TO TRUE
            WHEN EDIT-CONTROL-F3        SET CONTROL-F3         TO TRUE
            WHEN EDIT-CONTROL-F4        SET CONTROL-F4         TO TRUE
            WHEN EDIT-CONTROL-F5        SET CONTROL-F5         TO TRUE
            WHEN EDIT-CONTROL-F6        SET CONTROL-F6         TO TRUE
            WHEN EDIT-CONTROL-F7        SET CONTROL-F7         TO TRUE
            WHEN EDIT-CONTROL-F8        SET CONTROL-F8         TO TRUE
            WHEN EDIT-CONTROL-F9        SET CONTROL-F9         TO TRUE
            WHEN EDIT-CONTROL-F10       SET CONTROL-F10        TO TRUE
            WHEN EDIT-ALT-F1            SET ALT-F1             TO TRUE
            WHEN EDIT-ALT-F2            SET ALT-F2             TO TRUE
            WHEN EDIT-ALT-F3            SET ALT-F3             TO TRUE
            WHEN EDIT-ALT-F4            SET ALT-F4             TO TRUE
            WHEN EDIT-ALT-F5            SET ALT-F5             TO TRUE
            WHEN EDIT-ALT-F6            SET ALT-F6             TO TRUE
            WHEN EDIT-ALT-F7            SET ALT-F7             TO TRUE
            WHEN EDIT-ALT-F8            SET ALT-F8             TO TRUE
            WHEN EDIT-ALT-F9            SET ALT-F9             TO TRUE
            WHEN EDIT-ALT-F10           SET ALT-F10            TO TRUE
            WHEN EDIT-ALT-1             SET ALT-1              TO TRUE
            WHEN EDIT-ALT-2             SET ALT-2              TO TRUE
            WHEN EDIT-ALT-3             SET ALT-3              TO TRUE
            WHEN EDIT-ALT-4             SET ALT-4              TO TRUE
            WHEN EDIT-ALT-5             SET ALT-5              TO TRUE
            WHEN EDIT-ALT-6             SET ALT-6              TO TRUE
            WHEN EDIT-ALT-7             SET ALT-7              TO TRUE
            WHEN EDIT-ALT-8             SET ALT-8              TO TRUE
            WHEN EDIT-ALT-9             SET ALT-9              TO TRUE
            WHEN EDIT-ALT-0             SET ALT-0              TO TRUE
            WHEN EDIT-ALT-TRACE         SET ALT-TRACE          TO TRUE
            WHEN EDIT-ALT-EQUAL         SET ALT-EQUAL          TO TRUE
            WHEN EDIT-PAGE-UP           SET PAGE-UP            TO TRUE
            WHEN EDIT-PAGE-DOWN         SET PAGE-DOWN          TO TRUE
            WHEN EDIT-CONTROL-PAGE-UP   SET CONTROL-PAGE-UP    TO TRUE
            WHEN EDIT-CONTROL-PAGE-DOWN SET CONTROL-PAGE-DOWN  TO TRUE
            WHEN EDIT-ALT-A             SET ALT-A              TO TRUE
            WHEN EDIT-ALT-B             SET ALT-B              TO TRUE
            WHEN EDIT-ALT-C             SET ALT-C              TO TRUE
            WHEN EDIT-ALT-D             SET ALT-D              TO TRUE
            WHEN EDIT-ALT-E             SET ALT-E              TO TRUE
            WHEN EDIT-ALT-F             SET ALT-F              TO TRUE
            WHEN EDIT-ALT-G             SET ALT-G              TO TRUE
            WHEN EDIT-ALT-H             SET ALT-H              TO TRUE
            WHEN EDIT-ALT-I             SET ALT-I              TO TRUE
            WHEN EDIT-ALT-J             SET ALT-J              TO TRUE
            WHEN EDIT-ALT-K             SET ALT-K              TO TRUE
            WHEN EDIT-ALT-L             SET ALT-L              TO TRUE
            WHEN EDIT-ALT-M             SET ALT-M              TO TRUE
            WHEN EDIT-ALT-N             SET ALT-N              TO TRUE
            WHEN EDIT-ALT-O             SET ALT-O              TO TRUE
            WHEN EDIT-ALT-P             SET ALT-P              TO TRUE
            WHEN EDIT-ALT-Q             SET ALT-Q              TO TRUE
            WHEN EDIT-ALT-R             SET ALT-R              TO TRUE
            WHEN EDIT-ALT-S             SET ALT-S              TO TRUE
            WHEN EDIT-ALT-T             SET ALT-T              TO TRUE
            WHEN EDIT-ALT-U             SET ALT-U              TO TRUE
            WHEN EDIT-ALT-V             SET ALT-V              TO TRUE
            WHEN EDIT-ALT-W             SET ALT-W              TO TRUE
            WHEN EDIT-ALT-X             SET ALT-X              TO TRUE
            WHEN EDIT-ALT-Y             SET ALT-Y              TO TRUE
            WHEN EDIT-ALT-Z             SET ALT-Z              TO TRUE
            WHEN EDIT-F11               SET F11                TO TRUE
            WHEN EDIT-F12               SET F12                TO TRUE
            WHEN EDIT-SHIFT-F11         SET SHIFT-F11          TO TRUE
            WHEN EDIT-SHIFT-F12         SET SHIFT-F12          TO TRUE
            WHEN EDIT-CONTROL-F11       SET CONTROL-F11        TO TRUE
            WHEN EDIT-CONTROL-F12       SET CONTROL-F12        TO TRUE
            WHEN EDIT-ALT-F11           SET ALT-F11            TO TRUE
            WHEN EDIT-ALT-F12           SET ALT-F12            TO TRUE
            WHEN EDIT-SHIFT-TAB         SET SHIFT-TAB          TO TRUE
            WHEN EDIT-CURSOR-UP         SET CURSOR-UP          TO TRUE
           END-EVALUATE

           IF TIMEOUT-RETURN = 77
           AND F11
               MOVE 98 TO TIMEOUT-RETURN
               SET ALT-F11 TO TRUE
           END-IF.

       100-99-FIM. EXIT.

       200-CONTRL-LEFT.

           MOVE 999 TO TECLA-EDIT
           IF  COL-ED > 1
               SUBTRACT 1 FROM COL-ED CURPOS-COL
               IF   DATA-LK(FIELD) (COL-ED: 1) = SPACE
                    PERFORM UNTIL COL-ED = 1
                            OR (DATA-LK(FIELD) (COL-ED: 1) NOT = SPACE)
                               SUBTRACT 1 FROM COL-ED CURPOS-COL
                    END-PERFORM
                    IF  COL-ED > 1
                        PERFORM UNTIL COL-ED = 1
                           OR (DATA-LK(FIELD) (COL-ED: 1) = SPACE)
                              SUBTRACT 1 FROM COL-ED CURPOS-COL
                        END-PERFORM
                        PERFORM UNTIL COL-ED = LEN-E
                           OR (DATA-LK(FIELD) (COL-ED: 1) NOT = SPACE)
                              ADD 1 TO COL-ED CURPOS-COL
                        END-PERFORM
                    ELSE
                        IF   NUMERICO
                        AND  DATA-LK(FIELD) (COL-ED: 1) = SPACE
                             SET EDIT-CURSOR-UP TO TRUE
                             PERFORM 330-UP-GUIA THRU 330-99-FIM
                             MOVE 1 TO FROM-LEFT
                        END-IF
                    END-IF
               ELSE
                    PERFORM UNTIL COL-ED = 1
                            OR (DATA-LK(FIELD) (COL-ED: 1) = SPACE)
                               SUBTRACT 1 FROM COL-ED CURPOS-COL
                    END-PERFORM
                    PERFORM UNTIL COL-ED = LEN-E
                            OR (DATA-LK(FIELD) (COL-ED: 1) NOT = SPACE)
                               ADD 1 TO COL-ED CURPOS-COL
                     END-PERFORM
               END-IF
           ELSE
               SET EDIT-CURSOR-UP TO TRUE
               PERFORM 330-UP-GUIA THRU 330-99-FIM
               MOVE 1 TO FROM-LEFT
           END-IF.

       200-99-FIM. EXIT.

       210-CONTROL-RIGHT.

           MOVE 0 TO TECLA-EDIT
           IF   COL-ED NOT < LEN-E
      *         SET EDIT-CURSOR-DOWN TO TRUE
                SET EDIT-enter       TO TRUE
                PERFORM 320-NEXT-GUIA THRU 320-99-FIM
           ELSE
                IF   DATA-LK(FIELD) (COL-ED: 1) = SPACE
                     PERFORM UNTIL COL-ED = LEN-E
                             OR (DATA-LK(FIELD) (COL-ED: 1) NOT = SPACE)
                                ADD 1 TO COL-ED CURPOS-COL
                     END-PERFORM
                ELSE
                    PERFORM UNTIL COL-ED = LEN-E
                            OR (DATA-LK(FIELD) (COL-ED: 1) = SPACE)
                               ADD 1 TO COL-ED CURPOS-COL
                    END-PERFORM
                    PERFORM UNTIL COL-ED = LEN-E
                            OR (DATA-LK(FIELD) (COL-ED: 1) NOT = SPACE)
                               ADD 1 TO COL-ED CURPOS-COL
                    END-PERFORM
                END-IF
           END-IF.

       210-99-FIM. EXIT.

       220-HEX-DISPLAY.

           IF   FIELD = 0
                GO TO 220-99-FIM
           END-IF

           IF   SECURE-E = "S"
                MOVE "?"                        TO CHAR
           ELSE
                MOVE DATA-LK(FIELD) (COL-ED: 1) TO CHAR
           END-IF
           MOVE DEC-X                      TO DEC
           MOVE "00,000 ' '=XX/000"        TO HEX-BUFFER
           MOVE CURPOS-LIN                 TO HEX-BUFFER (01: 02)
           MOVE CURPOS-COL                 TO HEX-BUFFER (04: 03)
           MOVE CHAR                       TO HEX-BUFFER (09: 01)
           ADD  1                          TO DEC
           MOVE CHAR-HEX (DEC)             TO HEX-BUFFER (12: 02)
           SUBTRACT 1                    FROM DEC
           MOVE DEC                        TO HEX-BUFFER (15: 03)
           CALL "CBL_WRITE_SCR_CHATTRS" USING X"1837"
                                              HEX-BUFFER
                        X"0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A"
                                              X"0011".

       220-99-FIM. EXIT.

       230-SAVE-DEL.

           PERFORM VARYING J FROM LEN-E BY -1
                            UNTIL LEN-E = 0
                     OR WORK-ED (J: 1) NOT = SPACE
                   CONTINUE
           END-PERFORM
           IF  J > 0
               IF  (DEL + J) NOT < LENGTH OF DELETADOS
                   MOVE 0 TO DEL
                   PERFORM VARYING I FROM J BY 1
                             UNTIL I > LENGTH OF DELETADOS
                          ADD 1 TO DEL
                          MOVE DELETADOS (I: 1)
                            TO DELETADOS (DEL: 1)
                   END-PERFORM
                   PERFORM VARYING DEL
                                  FROM LENGTH OF DELETADOS
                                    BY -1
                                 UNTIL DELETADOS (DEL: 1) NOT = SPACE
                              SUBTRACT 1 FROM DEL
                   END-PERFORM
                   IF  DEL < LENGTH OF DELETADOS
                       ADD 1 TO DEL
                   END-IF
               END-IF
               ADD  1              TO DEL
               MOVE WORK-ED (1: J) TO DELETADOS (DEL: J)
               ADD  J              TO DEL
               PERFORM 240-DISPLAY-DELETADOS
                  THRU 240-99-FIM
           END-IF.

       230-99-FIM. EXIT.

       240-DISPLAY-DELETADOS.

           IF   ECHODELETED = "ON"
                IF  DEL > 43
                    COMPUTE D = DEL - 43
                ELSE
                    MOVE 1 TO D
                END-IF
                CALL "CBL_WRITE_SCR_CHATTRS" USING X"1800"
                                              DELETADOS (D: 43)
                                              DELETADOS-ATTR
                                              X"002B".

       240-99-FIM. EXIT.

       250-CASE-MIX.

           IF NUMERICO GO TO 250-99-FIM END-IF
           INSPECT DATA-LK (FIELD) (1: LEN-E)
                   CONVERTING MAIUSCULAS TO MINUSCULAS
           IF CWCASE = "LOW" GO TO 250-99-FIM END-IF
           IF CWCASE = "UPP"
              INSPECT DATA-LK (FIELD) (1: LEN-E)
                      CONVERTING MINUSCULAS TO MAIUSCULAS
              GO TO 250-99-FIM
           END-IF
           PERFORM 010-EXIBIR THRU 010-99-FIM
           MOVE SPACES TO WORK-ED
           MOVE 0      TO K
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN-E
                   IF DATA-LK(FIELD) (I: 1) NOT = SPACE
                      MOVE 0 TO Y
                      MOVE I TO J
                      PERFORM UNTIL I > LEN-E
                              OR DATA-LK(FIELD) (I: 1) = SPACE
                                 ADD  1 TO Y K
                                 MOVE DATA-LK(FIELD) (I: 1)
                                   TO WORK-ED (K: 1)
                                 IF Y = 1
                                    INSPECT WORK-ED (K: 1)
                                            CONVERTING MINUSCULAS
                                                    TO MAIUSCULAS
                                 END-IF
                              ADD 1 TO I
                      END-PERFORM
                      IF Y < 3
                         INSPECT WORK-ED (J: 1)
                                 CONVERTING MAIUSCULAS
                                         TO MINUSCULAS
                      END-IF
                      ADD 1 TO K
                   END-IF
           END-PERFORM
           MOVE WORK-ED TO DATA-LK (FIELD).

       250-99-FIM. EXIT.

       260-CASE.

           IF   VARIAVEL
           AND  (NOT NUMERICO)
                IF   CWCASE = "LOW"
                     INSPECT DATA-E (1: LEN-E)
                             CONVERTING MAIUSCULAS TO MINUSCULAS
                END-IF
                IF   CWCASE = "UPP"
                     INSPECT DATA-E (1: LEN-E)
                             CONVERTING MINUSCULAS TO MAIUSCULAS
                END-IF
                IF   CWACCENT = "OFF"
                     INSPECT DATA-E (1: LEN-E)
                             CONVERTING ACENTOS-850 TO ACENTOS-OFF
                END-IF
                MOVE DATA-E (1: LEN-E) TO CHARACTER-BUFFER
           ELSE
                IF   TEXTO
                     IF   CWLITS = "LOW"
                          INSPECT DATA-E (1: LEN-E)
                                  CONVERTING MAIUSCULAS TO MINUSCULAS
                     END-IF
                     IF   CWLITS = "UPP"
                          INSPECT DATA-E (1: LEN-E)
                                  CONVERTING MINUSCULAS TO MAIUSCULAS
                     END-IF
                     IF   CWACCENT = "OFF"
                          INSPECT DATA-E (1: LEN-E)
                                  CONVERTING ACENTOS-850 TO ACENTOS-OFF
                     END-IF
                     MOVE DATA-E (1: LEN-E) TO CHARACTER-BUFFER
                END-IF
           END-IF.

       260-99-FIM. EXIT.

       270-DEFINE-OBJECTS.

           OPEN I-O CRITICA
           IF   FS-CRITICA > "09"
                CALL "CWISAM" USING ER-CRITICA
           END-IF

           SET CWOBJE-OCCURS TO TRUE
           CALL  CWOBJE  USING PARAMETROS-CWOBJE CWOBJE-FUNCTION
frango     IF   CWOBJE-CURPOS NOT = '0000'
frango          add JANLIN to CWOBJE-LINE
frango          add JANCOL to CWOBJE-COLUMN
frango     END-IF
           MOVE CWOBJE-OCCURS-NUMBER TO OBJECTS-CWOBJE
           SET  CWOBJE-GET           TO TRUE
           MOVE "D"                  TO BUTTON-TYPE
           MOVE 1                    TO SAVE-OBJECT
           PERFORM VARYING objeto FROM 1 BY 1
                     UNTIL objeto > OBJECTS-CWOBJE
                   PERFORM 271-EXIBE-OBJECTS THRU 271-99-FIM
           END-PERFORM
           MOVE 0             TO objeto SAVE-OBJECT.

       270-99-FIM. EXIT.

       271-EXIBE-OBJECTS.

           MOVE objeto TO CWOBJE-OCCURS-NUMBER
           CALL  CWOBJE  USING PARAMETROS-CWOBJE CWOBJE-FUNCTION
frango     IF   CWOBJE-CURPOS NOT = '0000'
frango          add JANLIN to CWOBJE-LINE
frango          add JANCOL to CWOBJE-COLUMN
frango     END-IF
           MOVE 0 TO FIELD
           IF CWOBJE-LIST-BOX
           OR CWOBJE-COMBO-BOX
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > FIELDS
                   IF DATANAME-LK (I) = CWOBJE-OPTION
                      MOVE I TO FIELD
                      EXIT PERFORM
                   END-IF
              END-PERFORM
              IF I > FIELDS
                 GO TO 271-99-FIM
              END-IF
           END-IF
           PERFORM 015-AJUSTA-HORIZONTAL THRU 015-99-FIM
           IF   CWOBJE-VALIDATE
                IF  (CWOBJE-OPTION  NOT = SPACE)
                AND (CWOBJE-PROGRAM NOT = SPACE)
                     INITIALIZE CRITICA-REG
                     MOVE CWOBJE-OPTION TO CRITICA-FIELD
                     READ CRITICA
                     IF   FS-CRITICA = "00"
                          IF  CWOBJE-PROGRAM NOT = CRITICA-PROGRAM
                              CANCEL CRITICA-PROGRAM
                          END-IF
                          DELETE CRITICA RECORD
                     END-IF
                     MOVE 0                   TO CRITICA-USINGS
                     MOVE CWOBJE-PROGRAM      TO CRITICA-PROGRAM
                     MOVE 73                  TO SZ-CRITICA
                     MOVE CWOBJE-CURSOR       TO CRITICA-CURSOR
                     IF CWOBJE-SUBSCRIPT NUMERIC
                        MOVE CWOBJE-SUBSCRIPT
                          TO CRITICA-CURSOR-SUBSCRIPT
                     ELSE
                        MOVE 1
                          TO CRITICA-CURSOR-SUBSCRIPT
                     END-IF
                     IF  (CWOBJE-STRING-2 (1) (1: 3) NOT NUMERIC)
                     AND (CWOBJE-STRING-2 (1) (1: 3) NOT = SPACES)
                     AND CRITICA-CURSOR = SPACES
                         MOVE CWOBJE-STRING-2 (1) TO CRITICA-CURSOR
                         MOVE "001"               TO CWOBJE-STRING-2 (1)
                     END-IF
                     PERFORM VARYING I FROM 1 BY 1
                               UNTIL I > 100
                           IF CWOBJE-STRING-1 (I) NOT = SPACES
                              ADD 1  TO CRITICA-USINGS
                              ADD 33 TO SZ-CRITICA
                              MOVE CWOBJE-STRING-1 (I)
                                TO CRITICA-USING   (CRITICA-USINGS)
                              MOVE CWOBJE-STRING-2 (I) (1: 3)
                                TO CRITICA-SUBSCRIPT (CRITICA-USINGS)
                                   (1: 3)
                              IF CRITICA-SUBSCRIPT (CRITICA-USINGS)
                                 NOT NUMERIC
                                 MOVE 1
                                   TO CRITICA-SUBSCRIPT (CRITICA-USINGS)
                              END-IF
                           END-IF
                     END-PERFORM
                     WRITE CRITICA-REG
                     MOVE SPACES TO CRITICA-FIELD
                     IF   FS-CRITICA > "09"
                          CALL "CWISAM" USING ER-CRITICA
                     END-IF
                END-IF
                GO TO 271-99-FIM
           END-IF
           COMPUTE ROW-NUMBER2    = CWOBJE-LINE   - 1
           COMPUTE COLUMN-NUMBER2 = CWOBJE-COLUMN - 1
           IF  SAVE-GUIA = 1
           AND (FUN-ACCEPT OR COMBO-DISPLAY = "1")
                MOVE 0      TO GUIA-FIELD
                MOVE objeto TO GUIA-OBJECT
                IF   CWOBJE-LIST-BOX
                OR   CWOBJE-COMBO-BOX
                     MOVE 1 TO GUIA-POP
                ELSE
                     MOVE 0 TO GUIA-POP
                END-IF
                PERFORM 014-SAVE-GUIA THRU 014-99-FIM
           END-IF

           MOVE CWOBJE-LINE   TO X
           MOVE CWOBJE-COLUMN TO Y

           EVALUATE TRUE
               WHEN CWOBJE-PUSH-BUTTON
                 OR CWOBJE-ICON
                AND NOT CWOBJE-MICRO-ICON
                    PERFORM 280-EXIBE-BUTTON THRU 280-99-FIM
               WHEN CWOBJE-GROUP
                    PERFORM 281-EXIBE-GROUP  THRU 281-99-FIM
               WHEN CWOBJE-TEXT
                    PERFORM 282-EXIBE-TEXT   THRU 282-99-FIM
               WHEN CWOBJE-PUSH-MOUSE
                 OR CWOBJE-MICRO-ICON
                    MOVE objeto TO TESTE-M
                    PERFORM 283-EXIBE-MOUSE  THRU 283-99-FIM
                    MOVE 0 TO TESTE-M
               WHEN CWOBJE-COMBO-BOX
                 OR CWOBJE-LIST-BOX
                IF  SAVE-OBJECT = 1
                    IF   CWOBJE-OPTION NOT = SPACES
                         MOVE CWOBJE-OPTION      TO HIDE-CHAVE
                         MOVE 0                  TO HIDE-FIELD
                                                    HIDE-COMBO
                         MOVE objeto             TO HIDE-OBJECT
                         IF   CWOBJE-LIST-BOX
                              MOVE 1 TO HIDE-LIST
                         ELSE
                              MOVE 0 TO HIDE-LIST
                              MOVE 1 TO HIDE-COMBO
                              IF  CWOBJE-NOEDIT
                                  MOVE 1 TO HIDE-NOEDIT
                              END-IF
                         END-IF
                         MOVE CWOBJE-HORIZONTAL-LENGTH TO HIDE-LENGTH
                         IF   CWOBJE-RETURN > 1
                              MOVE CWOBJE-STRING-2-LENGTH TO HIDE-LENRET
                         ELSE
                              MOVE CWOBJE-STRING-1-LENGTH TO HIDE-LENRET
                         END-IF
                         IF HIDE-LENRET = 0
                            MOVE HIDE-LENGTH TO HIDE-LENRET
                         END-IF
                         WRITE HIDE-REG
                         IF   FS-HIDE = "22"
                              REWRITE HIDE-REG
                         END-IF
                         IF   FS-HIDE > "09"
                              CALL "CWISAM" USING ER-HIDE
                         END-IF
                    END-IF
                    IF   CWOBJE-LIST-BOX
                         COMPUTE ROW-NUMBER2    = CWOBJE-LINE   - 2
                         COMPUTE COLUMN-NUMBER2 = CWOBJE-COLUMN - 2
                         COMPUTE VEZES = CWOBJE-VERTICAL-LENGTH + 2
                         COMPUTE STRING-LENGTH2
                               = CWOBJE-HORIZONTAL-LENGTH + 1
                         PERFORM VEZES TIMES
                                 PERFORM 290-SAVE-PREV THRU 290-99-FIM
                                 ADD 1 TO ROW-NUMBER2
                         END-PERFORM
                         SET CWBOXF-VIEW TO TRUE
                         PERFORM 022-CWBOXF-CALL THRU 022-99-FIM
                         MOVE 1 TO NAO-EXIBE
                    END-IF
                ELSE
                    IF   NAO-EXIBE = 1
                         MOVE 0 TO NAO-EXIBE
                         GO TO 271-99-FIM
                    ELSE
                         move 1 to NAO-EXIBE
                         IF   PROT
                              MOVE 'd' TO CWBOXF-FUNCTION
                              PERFORM 021-CWBOXF THRU 021-99-FIM
                              move save-prot to tecla-edit
                              MOVE 1 TO FROM-TAB
                         ELSE
                              IF   CWOBJE-COMBO-BOX
                                   SET CWBOXF-SHOW TO TRUE
                                   PERFORM 021-CWBOXF THRU 021-99-FIM
                              ELSE
                                   SET CWBOXF-POP-UP TO TRUE
                                   PERFORM 021-CWBOXF  THRU 021-99-FIM
                              END-IF
                         END-IF
                    END-IF
                END-IF
               WHEN CWOBJE-SCROLL
                AND scroll-col (X Y) = 0
                    IF CWOBJE-COLOR (1:3) = SPACES
                       MOVE 112 TO CWOBJE-COLOR
                    END-IF
                    ADD  1             TO SCROLLS
                    MOVE 2000          TO STRING-LENGTH
                    CALL "CBL_READ_SCR_CHATTRS" USING X'0000'
                                                      CHARACTER-BUFFER
                                                      ATTRIBUTE-BUFFER
                                                      STRING-LENGTH
                    MOVE CWOBJE-COLOR       TO ATTRIBUTE
                    MOVE CHAR-LIN (X) (Y:1) TO THUMBS-CHA
                    MOVE ATTR-LIN (X) (Y:1) TO THUMBS-ATT
                    MOVE 1                  TO T
                    MOVE ATTRIBUTE-X        TO ATTR-LIN (X) (Y:1)
                    MOVE CWOBJE-OPTIONS     TO THUMBS-TYPE
                    MOVE CWOBJE-KEY         TO thumbs-key
                    MOVE SCROLLS            TO scroll-col (X Y)
                    COMPUTE THUMBS-INITROW = X - 1
                    COMPUTE THUMBS-INITCOL = Y - 1
                    IF CWOBJE-VERTICAL
                       MOVE ARROW-UP TO CHAR-LIN (X) (Y:1)
030817*                COMPUTE THUMBS-FULL = CWOBJE-HEIGHT - 2
030817                 COMPUTE THUMBS-FULL = CWOBJE-HEIGHT - 1
                       PERFORM THUMBS-FULL TIMES
                               ADD  1           TO X T
                               MOVE CHAR-LIN(X)(Y:1) TO THUMBS-CHA(T:1)
                               MOVE ATTR-LIN(X)(Y:1) TO THUMBS-ATT(T:1)
                               MOVE SCROLLS     TO scroll-col (X Y)
                               MOVE X'B0'       TO CHAR-LIN(X)(Y:1)
                               MOVE ATTRIBUTE-X TO ATTR-LIN(X)(Y:1)
                       END-PERFORM
                       ADD  1                TO X T
                       MOVE CHAR-LIN(X)(Y:1) TO THUMBS-CHA(T:1)
                       MOVE ATTR-LIN(X)(Y:1) TO THUMBS-ATT(T:1)
                       MOVE ARROW-DOWN       TO CHAR-LIN  (X) (Y:1)
                       MOVE SCROLLS          TO scroll-col(X Y)
                    ELSE
                       MOVE ARROW-LEFT TO CHAR-LIN (X) (Y:1)
                       COMPUTE THUMBS-FULL = CWOBJE-WIDTH - 2
                       PERFORM THUMBS-FULL TIMES
                               ADD  1           TO Y
                               MOVE SCROLLS     TO scroll-col (X Y)
                               MOVE X'B0'       TO CHAR-LIN (X) (Y:1)
                               MOVE ATTRIBUTE-X TO ATTR-LIN(X)(Y:1)
                       END-PERFORM
                       ADD  1                TO Y T
                       MOVE CHAR-LIN(X)(Y:1) TO THUMBS-CHA(T:1)
                       MOVE ATTR-LIN(X)(Y:1) TO THUMBS-ATT(T:1)
                       MOVE ARROW-RIGHT      TO CHAR-LIN  (X) (Y:1)
                       MOVE SCROLLS          TO scroll-col (X Y)
                    END-IF
                    MOVE ATTRIBUTE-X TO ATTR-LIN(X)(Y:1)
                    SET THUMBS-THUMB TO CWOBJE-THUMB
                    CALL "CBL_WRITE_SCR_CHATTRS" USING X'0000'
                                               CHARACTER-BUFFER
                                               ATTRIBUTE-BUFFER
                                               STRING-LENGTH
           END-EVALUATE

           IF  CWOBJE-SCROLL
               PERFORM 038-SCROLL-POSIT THRU 038-99-FIM
           END-IF

           MOVE 0 TO FIELD.

       271-99-FIM. EXIT.

       280-EXIBE-BUTTON.

           MOVE CWOBJE-HORIZONTAL-LENGTH TO TAMANHO STRING-LENGTH2
           MOVE M-201              TO TEXTO-TOP
           MOVE MM-205             TO TEXTO-TOP (2: )
           MOVE M-187              TO TEXTO-TOP (TAMANHO: )
           MOVE M-186              TO TEXTO-MID
           MOVE 2                  TO B2
           MOVE SPACE              TO TP
           MOVE 0                  TO B3
           PERFORM VARYING B FROM 1 BY 1 UNTIL B2 > TAMANHO
                   IF  CWOBJE-LABEL (B: 1) = "~"
                       COMPUTE B1 = B + 1
                       IF  CWOBJE-KEY NOT = 0
                           PERFORM 140-GRAVA-HOTS THRU 140-99-FIM
                       END-IF
                       MOVE CWOBJE-LABEL (B1: 1) TO TP
                   ELSE
                       ADD 1 TO B2
                       MOVE CWOBJE-LABEL (B: 1)
                         TO TEXTO-MID (B2: 1)
                   END-IF
           END-PERFORM
           MOVE M-186              TO TEXTO-MID (TAMANHO: )
           MOVE M-200              TO TEXTO-BOT
           MOVE MM-205             TO TEXTO-BOT (2: )
           MOVE M-188              TO TEXTO-BOT (TAMANHO: )

           SUBTRACT 1 FROM TAMANHO

           IF  BUTTON-TYPE = "D"
               MOVE ATTR-LOW              TO ATTRIB-TOP
                                             ATTRIB-MID
                                             ATTRIB-BOT
               MOVE ATTR-HIGH             TO ATTRIB-TOP (1: TAMANHO)
               MOVE ATTR-HIGH             TO ATTRIB-MID (1: 1)
                                             ATTRIB-BOT (1: 1)
           ELSE
               MOVE ATTR-HIGH            TO ATTRIB-TOP
                                            ATTRIB-MID
                                            ATTRIB-BOT
               MOVE ATTR-LOW             TO ATTRIB-TOP (1: TAMANHO)
               MOVE ATTR-LOW             TO ATTRIB-MID (1: 1)
           END-IF

           IF   FUN-DISPLAY
                PERFORM VARYING e FROM 1 BY 1 UNTIL e > 240
                        add 16 to dis-x (e)
                END-PERFORM
           END-IF

           PERFORM 290-SAVE-PREV THRU 290-99-FIM

           CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION2
                                              TEXTO-TOP
                                              ATTRIB-TOP
                                              STRING-LENGTH2
           IF   FUN-ACCEPT
                MOVE SCREEN-POSITION2 TO BUTTONS-CHAVE
                MOVE TEXTO-TOP        TO BUTTONS-TEXTO
                MOVE ATTRIB-TOP       TO BUTTONS-ATTRIB
                MOVE STRING-LENGTH2   TO BUTTONS-LENGTH
                WRITE BUTTONS-REG
           END-IF
           MOVE 0 TO SIZE-ICON
           PERFORM CWOBJE-VERTICAL-LENGTH TIMES
                   ADD 1 TO ROW-NUMBER2 SIZE-ICON
                   PERFORM 290-SAVE-PREV THRU 290-99-FIM
                   IF  ACENDER = 1
                   AND SIZE-ICON = 1
                       PERFORM VARYING E FROM 2 BY 1
                         UNTIL E = STRING-LENGTH2
                               MOVE INTENSO TO MID-X (E) (1: 1)
                       END-PERFORM
                   END-IF
                   IF  SIZE-ICON > 1
                       SUBTRACT 2 FROM STRING-LENGTH2
                       MOVE SPACES  TO TEXTO-MID (2: STRING-LENGTH2)
                       ADD  2       TO STRING-LENGTH2
                   END-IF
                   CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION2
                                                      TEXTO-MID
                                                      ATTRIB-MID
                                                      STRING-LENGTH2
                   IF   FUN-ACCEPT
                        MOVE SCREEN-POSITION2 TO BUTTONS-CHAVE
                        MOVE TEXTO-MID        TO BUTTONS-TEXTO
                        MOVE ATTRIB-MID       TO BUTTONS-ATTRIB
                        MOVE STRING-LENGTH2   TO BUTTONS-LENGTH
                        WRITE BUTTONS-REG
                   END-IF
                   IF  SIZE-ICON = 1
                   AND (NOT FUN-DISPLAY)
                       MOVE SCREEN-POSITION2        TO SP
                       MOVE STRING-LENGTH2          TO SL
                       PERFORM 310-DESTAQUE THRU 310-99-FIM
                   END-IF
           END-PERFORM

           ADD 1 TO ROW-NUMBER2

           PERFORM 290-SAVE-PREV THRU 290-99-FIM

           CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION2
                                              TEXTO-BOT
                                              ATTRIB-BOT
                                              STRING-LENGTH2
           IF   FUN-ACCEPT
                MOVE SCREEN-POSITION2 TO BUTTONS-CHAVE
                MOVE TEXTO-BOT        TO BUTTONS-TEXTO
                MOVE ATTRIB-BOT       TO BUTTONS-ATTRIB
                MOVE STRING-LENGTH2   TO BUTTONS-LENGTH
                WRITE BUTTONS-REG
           END-IF.

       280-99-FIM. EXIT.

       281-EXIBE-GROUP.

           MOVE CWOBJE-HORIZONTAL-LENGTH TO TAMANHO STRING-LENGTH2
           MOVE M-201              TO TEXTO-TOP
           MOVE MM-205             TO TEXTO-TOP (2: )
           PERFORM VARYING I FROM TAMANHO BY -1
                   UNTIL CWOBJE-LABEL (I: 1) NOT = SPACE
                   CONTINUE
           END-PERFORM
           MOVE 3 TO K
           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > I
                   MOVE CWOBJE-LABEL (Y: 1) TO TEXTO-TOP (K: 1)
                   ADD  1                   TO K
           END-PERFORM
           MOVE M-187              TO TEXTO-TOP (TAMANHO: )
           MOVE M-186              TO TEXTO-MID
           MOVE 2                  TO B2
           MOVE M-186              TO TEXTO-MID (TAMANHO: )
           MOVE M-200              TO TEXTO-BOT
           MOVE MM-205             TO TEXTO-BOT (2: )
           MOVE M-188              TO TEXTO-BOT (TAMANHO: )

           SUBTRACT 1 FROM TAMANHO

           PERFORM 290-SAVE-PREV THRU 290-99-FIM
           INSPECT TEXTO-TOP CONVERTING "_" TO SPACE
           CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION2
                                            TEXTO-TOP
                                            STRING-LENGTH2

           MOVE 1 TO STRING-LENGTH2
           PERFORM CWOBJE-VERTICAL-LENGTH TIMES
                   ADD  1 TO ROW-NUMBER2
                   PERFORM 290-SAVE-PREV THRU 290-99-FIM
                   CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION2
                                                    M-186
                                                    STRING-LENGTH2
                   ADD TAMANHO TO COLUMN-NUMBER2
                   MOVE 0 TO SET-GROUP
                   PERFORM 290-SAVE-PREV THRU 290-99-FIM
                   MOVE 1 TO SET-GROUP
                   CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION2
                                                    M-186
                                                    STRING-LENGTH2
                   SUBTRACT TAMANHO FROM COLUMN-NUMBER2
           END-PERFORM
           COMPUTE STRING-LENGTH2 = TAMANHO + 1
           ADD 1 TO ROW-NUMBER2

           PERFORM 290-SAVE-PREV THRU 290-99-FIM

           CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION2
                                            TEXTO-BOT
                                            STRING-LENGTH2.

       281-99-FIM. EXIT.

       282-EXIBE-TEXT.

           MOVE CWOBJE-LABEL TO TEXTO-MID
           PERFORM VARYING STRING-LENGTH2 FROM 80
                                          BY -1
                       UNTIL TEXTO-MID(STRING-LENGTH2:1) NOT = SPACE
                          OR STRING-LENGTH2 = CWOBJE-HORIZONTAL-LENGTH
                   CONTINUE
           END-PERFORM
           PERFORM 290-SAVE-PREV THRU 290-99-FIM
           CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION2
                                            TEXTO-MID
                                            STRING-LENGTH2.
           IF   CWOBJE-COLOR NUMERIC
                MOVE CWOBJE-COLOR TO color-num
                CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION2
                                                  color-num
                                                  STRING-LENGTH2
           END-IF.

       282-99-FIM. EXIT.

       283-EXIBE-MOUSE.

           IF  objeto NOT = 0
           AND SAVE-OBJECT = 0
               MOVE CWOBJE-HORIZONTAL-LENGTH TO TAMANHO STRING-LENGTH2
               MOVE SPACES                   TO TEXTO-MID
               MOVE 0                        TO Y B3
               PERFORM VARYING I FROM 1 BY 1
                       UNTIL I > LENGTH OF TEXTO-MID
                       IF  CWOBJE-LABEL (I: 1) NOT = "~"
                           ADD 1 TO Y
                           MOVE CWOBJE-LABEL (I: 1) TO TEXTO-MID (Y: 1)
                       ELSE
                           IF  CWOBJE-KEY NOT = 0
                               COMPUTE B1 = I + 1
                               PERFORM 140-GRAVA-HOTS THRU 140-99-FIM
                           END-IF
                       END-IF
               END-PERFORM
               MOVE SCREEN-POSITION2 TO OBJETOS-SCREEN-POSITION
               MOVE POP-WINDOW       TO OBJETOS-WINDOW
               READ OBJETOS
               MOVE OBJETOS-ATTRIBUTE-BUFFER TO ATTRIB-MID
               IF  ACENDER = 1
                   PERFORM 291-INVERTE-VIDEO THRU 291-99-FIM
               END-IF
               CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION2
                                                  TEXTO-MID
                                                  ATTRIB-MID
                                                  STRING-LENGTH2
               GO TO 283-99-FIM
           END-IF

           MOVE CWOBJE-OCCURS-NUMBER      TO SAVE-CWOBJE
           MOVE TESTE-M                   TO CWOBJE-OCCURS-NUMBER
           CALL  CWOBJE  USING PARAMETROS-CWOBJE CWOBJE-FUNCTION
frango     IF   CWOBJE-CURPOS NOT = '0000'
frango          add JANLIN to CWOBJE-LINE
frango          add JANCOL to CWOBJE-COLUMN
frango     END-IF
           MOVE CWOBJE-HORIZONTAL-LENGTH TO TAMANHO STRING-LENGTH2
           MOVE SPACES                   TO TEXTO-MID
           MOVE 0                        TO Y B3
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > LENGTH OF TEXTO-MID
                   IF  CWOBJE-LABEL (I: 1) NOT = "~"
                       ADD 1 TO Y
                       MOVE CWOBJE-LABEL (I: 1) TO TEXTO-MID (Y: 1)
                   ELSE
                       IF  CWOBJE-KEY NOT = 0
                           COMPUTE B1 = I + 1
                           PERFORM 140-GRAVA-HOTS THRU 140-99-FIM
                       END-IF
                   END-IF
           END-PERFORM

           PERFORM 290-SAVE-PREV THRU 290-99-FIM

           IF  ACESO = 0
               MOVE SCREEN-POSITION2 TO SCREEN-POSITION4
               MOVE TEXTO-MID        TO CHARACTER-BUFFER4
               MOVE STRING-LENGTH2   TO STRING-LENGTH4
               CALL "CBL_READ_SCR_ATTRS" USING SCREEN-POSITION4
                                               ATTRIBUTE-BUFFER4
                                               STRING-LENGTH4
               IF   SAVE-OBJECT = 0
                    MOVE ATTRIBUTE-BUFFER4 TO ATTRIB-MID
                    PERFORM 291-INVERTE-VIDEO THRU 291-99-FIM
                    CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION2
                                                       TEXTO-MID
                                                       ATTRIB-MID
                                                       STRING-LENGTH2
                    MOVE 1 TO ACESO
               ELSE
                    CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION2
                                                     TEXTO-MID
                                                     STRING-LENGTH2
               END-IF
           ELSE
               MOVE 0 TO ACESO
               CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION4
                                                  CHARACTER-BUFFER4
                                                  ATTRIBUTE-BUFFER4
                                                  STRING-LENGTH4
           END-IF

           IF B3 NOT = 0
              MOVE SCREEN-POSITION2 TO SCREEN-POSITION5
              ADD B3 TO COLUMN-NUMBER5
              CALL "CBL_READ_SCR_ATTRS" USING SCREEN-POSITION5
                                         ATTRIBUTE-BUFFER6
                                         X"0001"
              SUBTRACT 1 FROM COLUMN-NUMBER5
              CALL "CBL_READ_SCR_ATTRS" USING SCREEN-POSITION5
                                         ATTRIBUTE-BUFFER5
                                         X"0001"
      *       IF ATTRIBUTE-BUFFER5 = ATTRIBUTE-BUFFER6
SC               MOVE ATTRIBUTE-BUFFER6 TO AP
                 IF AMARELO
                    COMPUTE ATTRIBUTE-BUFFER5 = ATTRIBUTE-BUFFER6 + 1
                 ELSE
                    COMPUTE ATTRIBUTE-BUFFER5 = ATTRIBUTE-BUFFER6 + 8
                 END-IF
                 CALL "CBL_WRITE_SCR_ATTRS"
                      USING SCREEN-POSITION5
                            ATTRIBUTE-BUFFER5
                            X"0001"
      *       END-IF
           END-IF

           MOVE SAVE-CWOBJE               TO CWOBJE-OCCURS-NUMBER
           CALL  CWOBJE  USING PARAMETROS-CWOBJE CWOBJE-FUNCTION
frango     IF   CWOBJE-CURPOS NOT = '0000'
frango          add JANLIN to CWOBJE-LINE
frango          add JANCOL to CWOBJE-COLUMN
frango     END-IF.

       283-99-FIM. EXIT.

       290-SAVE-PREV.

           IF   SAVE-OBJECT = 0
                GO TO 290-99-FIM
           END-IF

           IF   CWOBJE-GROUP
           AND  SET-GROUP = 1
                COMPUTE T-LIN = ROW-NUMBER2    + 1
                COMPUTE T-COL = COLUMN-NUMBER2 + 1
                PERFORM TAMANHO TIMES
                        MOVE objeto TO MOUSE-GROUP (T-LIN T-COL)
                        ADD  1      TO T-COL
                END-PERFORM
           END-IF

           IF   CWOBJE-PUSH-BUTTON
           OR   CWOBJE-PUSH-MOUSE
           OR   CWOBJE-ICON
           OR   CWOBJE-LIST-BOX
           OR   CWOBJE-COMBO-BOX
                COMPUTE T-LIN = ROW-NUMBER2    + 1
                COMPUTE T-COL = COLUMN-NUMBER2 + 1
                PERFORM STRING-LENGTH2 TIMES
                        MOVE objeto TO MOUSE-EVENT (T-LIN T-COL)
                        IF  FUN-DISPLAY
                            MOVE 0 TO MOUSE-EVENT (T-LIN T-COL)
                                      MOUSE-OK    (T-LIN T-COL)
                        END-IF
                        ADD  1      TO T-COL
                END-PERFORM
           END-IF
           MOVE SCREEN-POSITION2 TO OBJETOS-SCREEN-POSITION
           MOVE POP-WINDOW       TO OBJETOS-WINDOW
           READ OBJETOS
           IF   FS-OBJETOS = "23"
                ADD  1                TO SEQUENCIA
                MOVE SEQUENCIA        TO OBJETOS-SEQUENCE
                MOVE STRING-LENGTH2   TO OBJETOS-STRING-LENGTH
                CALL "CBL_READ_SCR_CHATTRS"
                                   USING OBJETOS-SCREEN-POSITION
                                         OBJETOS-CHARACTER-BUFFER
                                         OBJETOS-ATTRIBUTE-BUFFER
                                         OBJETOS-STRING-LENGTH
                 WRITE OBJETOS-REG
                 IF   FS-OBJETOS = "22"
                      REWRITE OBJETOS-REG
                 END-IF
                 IF   FS-OBJETOS > "09"
                      CALL "CWISAM" USING ER-OBJETOS
                 END-IF
           END-IF.

       290-99-FIM. EXIT.

       291-INVERTE-VIDEO.

           PERFORM VARYING I FROM 1 BY 1  UNTIL I > STRING-LENGTH2
                   COMPUTE FUNDO   = MID-X (I) / 16
                   COMPUTE TEXTOA  = MID-X (I) - (FUNDO * 16)
                   COMPUTE REVERSO = FUNDO + (TEXTOA * 16)
      *            IF   I = B3
      *                 IF I > 1
      *                    MOVE MID-X (I - 1) TO MID-X (I)
      *                    ADD  8             TO MID-X (I)
      *                 ELSE
      *                    MOVE MID-X (I + 1) TO MID-X (I)
      *                    ADD  8             TO MID-X (I)
      *                 END-IF
      *            ELSE
                        IF   REVERSO = MID-X (I)
                             MOVE 112 TO MID-X (I)
                        ELSE
                             MOVE REVERSO TO MID-X (I)
                        END-IF
      *            END-IF
           END-PERFORM.

       291-99-FIM. EXIT.

       300-CLEAR-OBJECT.

           CLOSE HOTS
           OPEN I-O HOTS
           PERFORM UNTIL SEQUENCIA = 0
              MOVE POP-WINDOW TO OBJETOS-WINDOW
              MOVE SEQUENCIA  TO OBJETOS-SEQUENCE
              READ OBJETOS KEY IS OBJETOS-SEQ
              IF   FS-OBJETOS < "10"
                   CALL "CBL_WRITE_SCR_CHATTRS"
                                  USING OBJETOS-SCREEN-POSITION
                                        OBJETOS-CHARACTER-BUFFER
                                        OBJETOS-ATTRIBUTE-BUFFER
                                        OBJETOS-STRING-LENGTH
                   DELETE OBJETOS RECORD
              END-IF
              SUBTRACT 1 FROM SEQUENCIA
           END-PERFORM
           MOVE SPACES TO CRITICA-FIELD
           START CRITICA KEY NOT LESS CRITICA-FIELD
           PERFORM TEST AFTER UNTIL FS-CRITICA > "09"
                   READ CRITICA NEXT RECORD
                   IF  FS-CRITICA < "10"
                       CANCEL CRITICA-PROGRAM
                   END-IF
           END-PERFORM
           CLOSE CRITICA
           DELETE FILE CRITICA.

       300-99-FIM. EXIT.

       310-DESTAQUE.

           MOVE MOUSE-POSITION TO MOUSE-POSITION-S
           IF   TP = SPACE
           OR   MODO = 2
           OR  (CWGETL-HIGH NOT = 1)
                GO TO 310-99-FIM
           END-IF
           PERFORM SL TIMES
                   ADD 1 TO CP
                   CALL "CBL_READ_SCR_CHARS" USING SP TESTE X"0001"
                   IF   TESTE = TP
                        CALL "CBL_READ_SCR_ATTRS" USING SP AP X"0001"
                        IF   CWOBJE-PUSH-BUTTON
                        OR   CWOBJE-ICON
                             IF  AP < 127
                                 ADD 1 TO AP
                             ELSE
                                 SUBTRACT 1 FROM AP
                             END-IF
                        ELSE
SC    *                      ADD 8 TO AP
                             IF AMARELO
                                ADD 1 TO AP
                             ELSE
                                ADD 8 TO AP
                             END-IF
                        END-IF
                        CALL "CBL_WRITE_SCR_ATTRS" USING SP AP X"0001"
                        EXIT PERFORM
                   END-IF
           END-PERFORM.

       310-99-FIM. EXIT.

       320-NEXT-GUIA.

           if skip-next = 1
              move 0 to skip-next
              go to 320-99-FIM
           end-if

           INITIALIZE PARAMETROS-CWOBJE
           IF  (LIN-EX = 0
           OR   COL-EX = 0)
           AND  GUIA-FIELD > 0
           AND  NOT (TOOBJECT AND OBJECTS-CWOBJE > 0)
           AND  NOT EDIT-ENTER
                MOVE POS-E TO POS-S
                MOVE 0     TO SAVE-EDIT
                GO TO 320-99-FIM
           END-IF
           PERFORM 390-CHECK-NOSS THRU 390-99-FIM
           IF   FS-HIDE = '00'
           AND  HIDE-COMBO = 1
           AND (HIDE-NOEDIT NOT = 1)
                IF (NOT EDIT-CURSOR-DOWN)
                and (NOT EDIT-TAB)
                and (NOT EDIT-ENTER)
                OR  (FROM-KBDC = 1)
                   READ GUIA NEXT RECORD
                   IF FS-GUIA = '00'
                   AND GUIA-FIELD > 0
                   AND FROM-KBDC = 1
                   AND EDIT-ENTER
                       MOVE 0 TO FROM-KBDC
                       GO TO 320-99-FIM
                   END-IF
                   MOVE 0 TO FROM-KBDC
                else
                    move 1 to nouser
                end-if
           END-IF
           IF  FS-HIDE = '00'
      *    AND CWBOXF-GUIA = 0
           AND HIDE-COMBO = 1
           AND FROM-KBDC = 1
           AND EDIT-CURSOR-DOWN
               MOVE 2 TO FROM-KBDC
           END-IF
           IF  CWBOXF-GUIA = 0
           OR (FS-HIDE NOT = '00')
           OR (FROM-KBDC = 1)
               IF FROM-KBDC NOT = 2
                  PERFORM TEST AFTER UNTIL FS-GUIA > '09'
                                     OR GUIA-FIELD > 0
                                     OR GUIA-OBJECT > 0
                       READ GUIA NEXT RECORD
                  END-PERFORM
               END-IF
           END-IF

           IF  FROM-KBDC = 2
               MOVE 3 TO FROM-KBDC
                         MOVE 0 TO NAO-EXIBE
           END-IF
           IF   FS-GUIA = "10"
           AND  CWBOXF-TECLA = 13
           AND  SAVE-EDIT = 9
                MOVE CWBOXF-TECLA TO SAVE-EDIT
                                     TECLA-EDIT
           END-IF
           IF  (EDIT-CURSOR-DOWN OR EDIT-TAB)
           AND  FS-GUIA = "10"
                IF   FS-HIDE = '00'
                AND (HIDE-COMBO NOT = 1)
                     SET EDIT-ENTER TO TRUE
                else
                     IF   FS-HIDE = '00'
                          move 2 to nouser
                          READ GUIA PREVIOUS RECORD
                     END-IF
                END-IF
           END-IF
           PERFORM 400-CHECK-POP THRU 400-99-FIM
           IF   FS-GUIA = "10"
           AND (NOT EDIT-ENTER)
           AND (NOT EDIT-CURSOR-DOWN)
                PERFORM 340-TOP-GUIA THRU 340-99-FIM
           ELSE
                IF  (NOT TOOBJECT)
                AND  GUIA-FIELD = 0
                AND (FIELD NOT = 0)
                AND GUIA-POP    = 0
                AND  FS-GUIA < "10"
                     GO TO 320-NEXT-GUIA
                END-IF
                IF   FS-GUIA > "09"
                     if (not edit-cursor-down)
                     and (not edit-cursor-up)
                        SET EDIT-ENTER TO TRUE
                     else
                        if not enter-terminate = 'ON'
                           SET EDIT-ENTER TO TRUE
                        else
                          if edit-cursor-down
                             PERFORM 340-TOP-GUIA THRU 340-99-FIM
                          else
                             PERFORM 350-BOT-GUIA THRU 350-99-FIM
                          end-if
                          go to 320-99-FIM
                        end-if
                     end-if
                     PERFORM TEST AFTER
                             UNTIL (GUIA-FIELD NOT = 0
                                AND GUIA-FIELD > 0)
                                 OR FS-GUIA > "09"
                             READ GUIA PREVIOUS RECORD
                     END-PERFORM
                     MOVE "10" TO FS-GUIA
                END-IF
           END-IF.

       320-99-FIM. EXIT.

       330-UP-GUIA.

           INITIALIZE PARAMETROS-CWOBJE
           IF  (LIN-EX = 0
           OR   COL-EX = 0)
           AND  GUIA-FIELD > 0
           AND  NOT (TOOBJECT AND OBJECTS-CWOBJE > 0)
           AND  NOT EDIT-ENTER
                MOVE POS-E TO POS-S
                MOVE 0     TO SAVE-EDIT
                GO TO 330-99-FIM
           END-IF
           PERFORM 390-CHECK-NOSS THRU 390-99-FIM

           READ GUIA PREVIOUS RECORD
           PERFORM 400-CHECK-POP THRU 400-99-FIM
           IF   FS-GUIA = "10"
           AND (NOT EDIT-ENTER)
                IF  EDIT-CURSOR-UP
                AND GUIA-OBJECT = 0
                AND GUIA-POP    = 0
                    GO TO 330-99-FIM
                END-IF
                PERFORM 350-BOT-GUIA THRU 350-99-FIM
           ELSE
                IF  (NOT TOOBJECT)
                AND  GUIA-FIELD = 0
                AND GUIA-POP    = 0
                AND (FIELD NOT = 0)
                AND  FS-GUIA < "10"
                     GO TO 330-UP-GUIA
                END-IF
           END-IF.

       330-99-FIM. EXIT.

       335-GET-FIELD.

           IF   CWOBJE-LIST-BOX
           OR   CWOBJE-COMBO-BOX
                IF CWOBJE-CURPOS NUMERIC
                AND (CWOBJE-LINE NOT = 0)
                AND (CWOBJE-COLUMN NOT = 0)
                     MOVE CWOBJE-CURPOS TO CURPOS
                     compute CURSOR-ROW = CURPOS-LIN - 1
                     compute CURSOR-COLUMN = CURPOS-COL - 1
                     CALL "CBL_SET_CSR_POS" USING CURSOR-POSITION
                END-IF
                if nouser = 2
                   move 1 to nouser
                else
                   MOVE "23" TO FS-HIDE
                end-if
                IF   CWOBJE-OPTION NOT = SPACES
                     PERFORM VARYING F FROM 1 BY 1
                               UNTIL F > FIELDS
                             IF   DATANAME-LK (F) = CWOBJE-OPTION
                                  IF FIELD > FIELDS
                                  OR FIELD = 0
                                     MOVE F TO FIELD
                                  END-IF
                                  MOVE "00" TO FS-HIDE
                                  COMPUTE F =  FIELDS + 1
                             END-IF
                     END-PERFORM
                END-IF
           END-IF.

       335-99-FIM. EXIT.

       340-TOP-GUIA.

          IF FOCUS NOT = 0
             MOVE FOCUS TO GUIA-FIELD
             MOVE 0     TO FOCUS
             START GUIA KEY NOT LESS GUIA-FIELD
           ELSE
             MOVE LOW-VALUES TO GUIA-REG
             START GUIA KEY NOT LESS GUIA-SEQ
           END-IF
           PERFORM TEST AFTER UNTIL (GUIA-FIELD NOT = 0)
                                 OR TOOBJECT
                                 OR (GUIA-POP NOT = 0)
                                 OR FS-GUIA > "09"
                   READ GUIA NEXT RECORD
                   PERFORM 400-CHECK-POP THRU 400-99-FIM
           END-PERFORM.

       340-99-FIM. EXIT.

       350-BOT-GUIA.

           MOVE HIGH-VALUES TO GUIA-SEQ
           START GUIA KEY NOT GREATER GUIA-SEQ
           PERFORM TEST AFTER UNTIL (GUIA-FIELD NOT = 0)
                                 OR TOOBJECT
                                 OR (GUIA-POP NOT = 0)
                                 OR FS-GUIA > "09"
                   READ GUIA PREVIOUS RECORD
                   PERFORM 400-CHECK-POP THRU 400-99-FIM
           END-PERFORM.

       350-99-FIM. EXIT.

       390-CHECK-NOSS.

           IF   UPDOWN
           AND (GUIA-FIELD NOT = 0)
           AND (LIN-EX = 0
           OR   COL-EX = 0)
                MOVE POS-E TO POS-S
           ELSE
                IF  (GUIA-FIELD NOT = 0)
                     MOVE "0000" TO POS-S
                END-IF
           END-IF.

       390-99-FIM. EXIT.

       400-CHECK-POP.

           IF   GUIA-POP = 1
           AND  FS-GUIA < "10"
           AND (GUIA-OBJECT NOT = 0)
                MOVE GUIA-OBJECT TO CWOBJE-OCCURS-NUMBER
                CALL  CWOBJE  USING PARAMETROS-CWOBJE
                                    CWOBJE-FUNCTION
frango          IF   CWOBJE-CURPOS NOT = '0000'
frango               add JANLIN to CWOBJE-LINE
frango               add JANCOL to CWOBJE-COLUMN
frango          END-IF
                PERFORM 335-GET-FIELD THRU 335-99-FIM
                INITIALIZE PARAMETROS-CWOBJE
                IF   FS-HIDE = "23"
                     DELETE GUIA RECORD
                     MOVE 0 TO GUIA-POP
                END-IF
           END-IF.

       400-99-FIM. EXIT.

       410-CHECK-PLUS.

           IF  (PLUS-LIN = "+" OR PLUS-COL = "+"
           OR   PLUS-LIN = "-" OR PLUS-COL = "-")
           and  dataname-e not = spaces
                if  dataname-e = dataname-k
                and pos-e = pos-ss
                and (d-num not = d-num-a)
                    move lin-k to lin-e
                    move col-k to col-e
                    go to 410-99-FIM
                else
                    move pos-e      to pos-ss
                    move dataname-e to dataname-k
                    move d-num      to d-num-a
                    move "@@@@"     TO POS-K
                end-if
           END-IF

           IF   PLUS-LIN = "+"
                COMPUTE LIN-E = LIN-C + LIN-X + dinlin
           ELSE
                IF   PLUS-LIN = "-"
                     COMPUTE LIN-E = LIN-C - LIN-X + dinlin
                END-IF
           END-IF

           IF   PLUS-COL = "+"
                COMPUTE COL-E = COL-C + COL-X + dincol
           ELSE
                IF   PLUS-COL = "-"
                     COMPUTE COL-E = COL-C - COL-X + dincol
                END-IF
           END-IF

           IF POS-K = '@@@@'
              move POS-E to POS-K
           END-iF.

       410-99-FIM. EXIT.

       XE5.

           ACCEPT AGORA FROM TIME
           IF AGORA NOT = TIME-E5
              CALL X"E5"
              MOVE AGORA TO TIME-E5
           END-IF.

       FIM-XE5. EXIT.

       GETF.

           MOVE "S"                 TO CWGETF-FUNCTION
           MOVE DATANAME-LK (FIELD) TO CWGETF-FIELD
           MOVE 0                   TO CWGETF-LENGTH
                                       CWGETF-SUBSCRIPT RESTO
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > FIELDS
                   IF DATANAME-LK (I) = CWGETF-FIELD
                   AND (I NOT > FIELD)
                      ADD 1 TO CWGETF-SUBSCRIPT
                   END-IF
                   IF ACCEPT-LK (I)
                      MOVE LEN-LK (I) TO LENC
                      IF DATA-LK(I) (I:LENC) NOT = SPACES
                      OR (I NOT > FIELD)
                         ADD LEN-LK (I) TO CWGETF-LENGTH
                         ADD RESTO      TO CWGETF-LENGTH
                         MOVE 0         TO RESTO
                      ELSE
                         ADD LEN-LK (I) TO RESTO
                      END-IF
                   END-IF
           END-PERFORM
           CALL "CWGETF" USING PARAMETROS-CWGETF.

       FIM-GETF. EXIT.
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

       140-GRAVA-HOTS.

           MOVE CWOBJE-LABEL (B1: 1)   TO HOTS-HOT
           INSPECT HOTS-HOT
                   CONVERTING MINUSCULAS TO MAIUSCULAS

           READ HOTS
           MOVE objeto     TO HOTS-OBJECT
           MOVE CWOBJE-KEY TO HOTS-KEY
           MOVE 0          TO HOTS-ALT B3

           EVALUATE HOTS-HOT
               WHEN "0" MOVE 250 TO HOTS-ALT
               WHEN "1" MOVE 241 TO HOTS-ALT
               WHEN "2" MOVE 242 TO HOTS-ALT
               WHEN "3" MOVE 243 TO HOTS-ALT
               WHEN "4" MOVE 244 TO HOTS-ALT
               WHEN "5" MOVE 245 TO HOTS-ALT
               WHEN "6" MOVE 246 TO HOTS-ALT
               WHEN "7" MOVE 247 TO HOTS-ALT
               WHEN "8" MOVE 248 TO HOTS-ALT
               WHEN "9" MOVE 249 TO HOTS-ALT
               WHEN "A" MOVE 265 TO HOTS-ALT
               WHEN "B" MOVE 266 TO HOTS-ALT
               WHEN "C" MOVE 267 TO HOTS-ALT
               WHEN "D" MOVE 268 TO HOTS-ALT
               WHEN "E" MOVE 269 TO HOTS-ALT
               WHEN "=" MOVE 252 TO HOTS-ALT
               WHEN "F" MOVE 270 TO HOTS-ALT
               WHEN "G" MOVE 271 TO HOTS-ALT
               WHEN "H" MOVE 272 TO HOTS-ALT
               WHEN "I" MOVE 273 TO HOTS-ALT
               WHEN "J" MOVE 274 TO HOTS-ALT
               WHEN "K" MOVE 275 TO HOTS-ALT
               WHEN "L" MOVE 276 TO HOTS-ALT
               WHEN "M" MOVE 277 TO HOTS-ALT
               WHEN "N" MOVE 278 TO HOTS-ALT
               WHEN "O" MOVE 279 TO HOTS-ALT
               WHEN "P" MOVE 280 TO HOTS-ALT
               WHEN "Q" MOVE 281 TO HOTS-ALT
               WHEN "R" MOVE 282 TO HOTS-ALT
               WHEN "S" MOVE 283 TO HOTS-ALT
               WHEN "T" MOVE 284 TO HOTS-ALT
               WHEN "-" MOVE 251 TO HOTS-ALT
               WHEN "U" MOVE 285 TO HOTS-ALT
               WHEN "V" MOVE 286 TO HOTS-ALT
               WHEN "W" MOVE 287 TO HOTS-ALT
               WHEN "X" MOVE 288 TO HOTS-ALT
               WHEN "Y" MOVE 289 TO HOTS-ALT
               WHEN "Z" MOVE 290 TO HOTS-ALT
           END-EVALUATE

           IF HOTS-ALT NOT = 0
              COMPUTE B3 = B1 - 1
           END-IF

           IF FS-HOTS < '10'
              REWRITE HOTS-REG
           ELSE
              WRITE HOTS-REG
           END-IF.

       140-99-FIM. EXIT.

       150-CHECK-ALT.

           move 1 to from-kbdc
           MOVE 0 TO HOTS-KEY TECLA
           IF TECLA-EDIT NOT = 0
              INITIALIZE HOTS-REG
              MOVE TECLA-EDIT TO HOTS-ALT
              READ HOTS KEY IS HOTS-ALT
              IF FS-HOTS = '00'
                 MOVE X"00"       TO CARACTER
                 MOVE HOTS-KEY    TO TECLA
                 SET EDIT-ENTER   TO TRUE
                 MOVE 1           TO FROM-MOUSE
                 MOVE HOTS-OBJECT TO objeto
                 MOVE "P"                       TO BUTTON-TYPE
                 PERFORM 271-EXIBE-OBJECTS THRU 271-99-FIM
                 MOVE "10" TO FS-GUIA
                 CALL "CWAKEY" USING TECLA MIL
      *       ELSE
      *           IF  ALTS
      *           AND (NOT EDIT-ALT-H)
      *              CALL X"E5"
      *           END-IF
              END-IF
           END-IF.

       150-99-FIM. EXIT.

       900-CONSOLE.

           INITIALIZE PARAMETROS-CONSOLE
           MOVE 'CWUSER' TO CONSOLE-PROGRAM
           IF FUN-ACCEPT
              MOVE MATRIZ-E (1) TO ELEMENTO
              IF   DATANAME-E = 'CWSTOP'
                   CALL 'CONSOLE' USING PARAMETROS-CONSOLE 'STOP'
              ELSE
                   MOVE LEN-E TO CONSOLE-LENGTH
                   CALL 'CONSOLE' USING PARAMETROS-CONSOLE 'ACCT'
                   MOVE CONSOLE-MSG (1:LEN-E) TO DATA-LK(1)(1:LEN-E)
              END-IF
           ELSE
              MOVE 1 TO I
              PERFORM VARYING FIELD FROM 1 BY 1 UNTIL FIELD > FIELDS
                      MOVE MATRIZ-E (FIELD) TO ELEMENTO
                      MOVE DATA-E (1:LEN-E) TO CONSOLE-MSG (I:LEN-E)
                      ADD  LEN-E            TO I
              END-PERFORM
              CALL 'CONSOLE' USING PARAMETROS-CONSOLE
           END-IF.

       900-99-FIM. EXIT.

       END PROGRAM CWUSER.
