       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWUSER.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  28/11/2003 25/11/2013.
       SECURITY.      *************************************************
                      *                                               *
                      *  Tratamento de interface com o usu rio        *
                      *  modo grafico versÆo 2                        *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT TELA   ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS TELA-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-TELA
                  RESERVE NO ALTERNATE AREA.

           SELECT CAMPOS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CAMPOS-CHAVE = CAMPOS-JANELA
                                                  CAMPOS-CAMPO
                                                  pos-e
                  ALTERNATE KEY IS CAMPOS-JANELA-GUIA
                                                = CAMPOS-JANELA
                                                  CAMPOS-GUIA
                                                  WITH DUPLICATES
                  ALTERNATE KEY IS campos-janela-linha
                                                = CAMPOS-JANELA
                                                  LIN-E
                                                  WITH DUPLICATES
                  ALTERNATE KEY IS CAMPOS-ID    = CAMPOS-JANELA
                                                  CAMPOS-FD-ID
                                                  WITH DUPLICATES
                  ALTERNATE KEY IS CAMPOS-NAME  = CAMPOS-JANELA
                                                  DATANAME-E
                                                  campos-subscript
                                                  pos-e
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-CAMPOS
                  RESERVE NO ALTERNATE AREA.

           SELECT SAVES  ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS SAVES-CHAVE
                  ALTERNATE KEY IS SAVES-JANELA WITH DUPLICATES
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-SAVES
                  RESERVE NO ALTERNATE AREA.

           SELECT WINWORK ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS WINWORK-WINDOW
                  LOCK MODE     IS EXCLUSIVE
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-WINWORK.

           SELECT HIDE   ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS HIDE-CHAVE = HIDE-DATANAME
                                                HIDE-WINDOW
                  ALTERNATE KEY IS HIDE-WINDOW WITH DUPLICATES
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-HIDE
                  RESERVE NO ALTERNATE AREA.

           SELECT THUMBS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS THUMBS-ID
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-THUMBS
                  RESERVE NO ALTERNATE AREA.

           SELECT LISTBOX ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS LISTBOX-ID
                  ALTERNATE KEY IS LISTBOX-OBJECT WITH DUPLICATES
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-LISTBOX
                  RESERVE NO ALTERNATE AREA.

           SELECT WORKBOX ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS WORKBOX-OBJECT
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-WORKBOX
                  RESERVE NO ALTERNATE AREA.

           SELECT BUTTONS ASSIGN TO DISK
                  ORGANIZATION   IS INDEXED
                  ACCESS MODE    IS DYNAMIC
                  RECORD  KEY    IS BUTTONS-ID
                  ALTERNATE KEY  IS BUTTONS-LIST WITH DUPLICATES
                  LOCK MODE      IS EXCLUSIVE
                  FILE STATUS    IS FS-BUTTONS
                  RESERVE NO ALTERNATE AREA.

           SELECT OBJETOS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS OBJETOS-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-OBJETOS.

           SELECT DATANAMES ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS DATANAMES-ID
                  ALTERNATE KEY IS DATANAMES-FIELD
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-DATANAMES.

           SELECT FROMS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS FROMS-NAME
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-FROMS.

           SELECT OBJWORK ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS OBJECT-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-OBJWORK.

       COPY CRITICA.SEL.

       DATA DIVISION.
       FILE SECTION.

       FD  TELA
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-TELA.

       01  TELA-REG.
           05 TELA-CHAVE.
              10 TELA-JANELA           COMP-5 PIC S9(004).
           05 TELA-CHARS.
              10 TELA-LIN                                 OCCURS 25.
                 15 TELA-COL                  PIC  X(001) OCCURS 80.
           05 TELA-ATTRS.
              10 TELA-ATTR-LIN                            OCCURS 25.
                 15 TELA-ATTR-COL             PIC  X(001) OCCURS 80.
           05 TELA-CHARS-SAVE.
              10 TELA-LIN-SAVE                PIC  X(080) OCCURS 25.
           05 TELA-ATTRS-SAVE.
              10 TELA-ATTR-LIN-SAVE           PIC  X(080) OCCURS 25.
           05 TELA-FIELDS              COMP-5 PIC S9(004).
           05 TELA-FIELDS-MAP.
              10                                          OCCURS 25.
                 15 TELA-CAMPO         COMP-5 PIC S9(004) OCCURS 80.
           05 AVANCO-PENDENTE                 PIC  9(001).
           05 CURPOS.
              10 CURPOS-LIN                   PIC  9(002).
              10 CURPOS-COL                   PIC  9(002).
           05 REVERSEDS.
              10 REVERSEDS-LIN                            OCCURS 25.
                 15 REVERSEDS-COL      COMP-X PIC  9(002) OCCURS 80.

           05 SCREEN-CONTROL.
              10 LINHA OCCURS 25.
                 15 LIN-IDS            COMP-5 PIC S9(004).
                 15 IDS                                   OCCURS 80.
                    20 FONT-ID         COMP-5 PIC S9(004).
                    20 COL-ID          COMP-5 PIC S9(004).
                    20 GRID-ID         COMP-5 PIC S9(004).
150719*             20 REB-ID          COMP-5 PIC S9(004).
                    20 DEC-ID          COMP-5 PIC S9(004).
                    20 FONT-DX         COMP-5 PIC S9(004).
                 15 FDS.
                    20 ENTRY-ID        COMP-5 PIC S9(004) OCCURS 80.

       FD  CAMPOS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CAMPOS.

       01  CAMPOS-REG.
           05 CAMPOS-JANELA    COMP-5 PIC S9(004).
           05 CAMPOS-GUIA      COMP-5 PIC S9(005).
           05 CAMPOS-FIELD     COMP-5 PIC S9(004).
           05 CAMPOS-FD-ID     COMP-5 PIC S9(004).
           05 campos-subscript        pic  9(003).
           05 CAMPOS-off       COMP-5 PIC S9(004).
           05 CAMPOS-SHOW             PIC  X(080).
           05 CAMPOS-FONT-ID   COMP-5 PIC S9(004).
           05 CAMPOS-define. *>       PIC  X(310). (antigo ELEMENTO)
              10 POS-E                PIC  9(004).
              10 REDEFINES POS-E.
                 15 LIN-E             PIC  9(002).
                 15 COL-E             PIC  9(002).
              10 REDEFINES POS-E.
                 15 PLUS-LIN          PIC  X(001).
                 15 LIN-X             PIC  9(002) COMP-X.
                 15 PLUS-COL          PIC  X(001).
                 15 COL-X             PIC  9(002) COMP-X.
              10 LEN-E                PIC  9(002).
              10 ATTR-E.
                 15 MODE-E            PIC  X(001).
                    88 ACCEPT-E       VALUE "U" "u" "T" "t" "P".
                    88 VARIAVEL       VALUE "U" "u" "T" "t" "F" "f" "P".
                    88 no-update      VALUE "T" "t".
                    88 ERASE-EOL      VALUE "l".
                    88 ERASE-EOS      VALUE "s".
                    88 BLANK-LINE     VALUE "L".
                    88 BLANK-SCREEN   VALUE "S".
                    88 TEXTO          VALUE "V" "v".
                    88 PROT           VALUE "P".
                    88 from-e         VALUE "F" "f" "V" "v".
                 15 FORE-E.
                    20 FORE-N         PIC  9(001).
                 15 BACK-E.
                    20 BACK-N         PIC  9(001).
                 15 SECURE-E          PIC  X(001).
                 15 BLINK-E           PIC  X(001).
                 15 BZERO-E           PIC  X(001).
                    88 FLOAT                   VALUE "E".
                 15 EMPTY-E           PIC  X(001).
                    88 RM-EOL                  VALUE 'L'.
                    88 RM-EOS                  VALUE 'S'.
                 15 BEEP-E            PIC  X(001).
                 15 REVERSE-E         PIC  X(001).
                 15 AUTO-E            PIC  X(001).
                 15 HIGH-E            PIC  X(001).
                 15 UPPLOW-E          PIC  X(001).
                 15 ADVANCE-E         PIC  X(001).
                    88 AD-EOL                  VALUE 'L'.
                    88 AD-EOS                  VALUE 'S'.
              10 LENF-E               PIC  9(002).
              10 PLUS-E               PIC  9(004) COMP-X.
              10 hide-e               PIC  9(001).
              10 size-e               PIC  9(002).
              10 TIPO-E               PIC  X(001).
              10 ATTR-INIT            PIC  X(001).
              10 COM-SINAL            PIC  9(001).
              10 MODO-ACCEPT          PIC  X(001).
                 88 INDEFINIDO                 VALUE "?".
                 88 VER-MODO-ACCEPT            VALUE SPACE "n".
                 88 NUMERICO                   VALUE "N" "n" "-" "+".
                 88 ALFABETICO                 VALUE "A" "B" "?".
                 88 COM-BARRA                  VALUE "B".
              10 CAMPOS-CAMPO         PIC  9(005).
              10 FILLER               PIC  X(005).
              10 PIC-E                PIC  X(080).
              10 DATANAME-E           PIC  X(030).
              10 DATA-E               PIC  X(080).
              10 CTRL-E               PIC  X(080).
           05 CAMPOS-DATA-ED          PIC  X(080).

       FD  SAVES
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-SAVES.

       01  SAVES-REG.
           02 SAVES-CHAVE.
              05 SAVES-HASH            PIC  X(032).
              05 SAVES-JANELA   COMP-5 PIC S9(004).
              05 SAVES-CAMPO           PIC  9(005).
           02 SAVES-CAMPOS-REG         PIC  X(482).

       FD  WINWORK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-WINWORK.

       01  WINWORK-REG.
           02 WINWORK-CHAVE.
              05 WINWORK-WINDOW   COMP-5 PIC S9(004).
           02 WINWORK-DATA.
              05 GUIA                    PIC  9(004).
              05 GUIA-FIELDS             PIC  9(004).
              05 CURSOR-GUIA             PIC  9(004).
              05 SAVE-CURSOR-GUIA        PIC  9(004).
              05 POS-G.
                 10 LIN-G                PIC  9(002).
                 10 COL-G                PIC  9(002).
              05 POS-S.
                 10 LIN-S                PIC  9(002).
                 10 COL-S                PIC  9(002).
              05 POS-EX.
                 10 LIN-EX               PIC  9(002).
                 10 COL-EX               PIC  9(002).
              05 OFF-SETS.
                 10 INIT-SIZE-FIELDS   COMP-5 PIC S9(004).
                 10 SIZE-FIELDS        COMP-5 PIC S9(004).
                 10 PREOBJ-SIZE-FIELDS COMP-5 PIC S9(004).
                 10 OFF-W              COMP-5 PIC S9(004).
                 10 OFF-W2             COMP-5 PIC S9(004).
                 10 OFF-I              COMP-5 PIC S9(004).
                 10 RADIOS          PIC 9(4).
                 10 OCCURS 2000.
                    15 RADIO-ID     PIC S9(4) COMP-5.
                    15 RADIO-POS    PIC 9(4).
              05 FIELD-IDS.
                 10 FIELS-ARRAY.
                 15 FIELD-ID      COMP-5 PIC S9(004) OCCURS 2000.
                 10 COMBO-ID      COMP-5 PIC S9(004) OCCURS 2000.
                 10 SAVE-ID       COMP-5 PIC S9(004) OCCURS 2000.
                 10 ID-FIELD      COMP-5 PIC S9(004) OCCURS 2000.
                 10 GUIA-COL      COMP-X PIC  9(002) OCCURS 2000.
              05 FLAGS-SA.
                 10 FLAG-SA              PIC  9(001).
                 10 COL-SA               PIC  9(002) OCCURS 2000.
              05 COMBO-IDS.
                 10 COMBOS-FIELDS        PIC  9(004).
                 10 COMBO-FD-ID   COMP-5 PIC S9(004) OCCURS 2000.
              05 objects                 PIC  9(4).
150719        05 CONTROLE-BRANCOS.
150719           10 BRANCOS OCCURS 25.
150719              15 BRANCO            PIC  X(001) OCCURS 80.

       FD  HIDE
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-HIDE.

       01  HIDE-REG.
           05 HIDE-DATANAME             PIC  X(030).
           05 HIDE-WINDOW        COMP-5 PIC S9(004).
           05 HIDE-FIELD                PIC  9(004).
           05 HIDE-OBJECT        COMP-X PIC  9(004).
           05 HIDE-LIST                 PIC  9(001).
           05 HIDE-GUIA          COMP-5 PIC S9(004).
           05 HIDE-GUIA-IND             PIC  9(004).
           05 HIDE-lincol               PIC  9(004).
           05 HIDE-return               PIC  9(001).
           05 HIDE-order                PIC  9(001).
           05 HIDE-string-2-length      PIC  9(002).
           05 hide-HORIZONTAL-LENGTH    PIC  9(002).
           05 HIDE-POS                  PIC  9(004).
           05 HIDE-POS2                 PIC  9(004).
           05 HIDE-SIZE                 PIC  9(002).
           05 HIDE-alpha                PIC  9(001).
           05 HIDE-other                PIC  9(001).
           05 HIDE-combo-SIZE           PIC  9(002).
           05 HIDE-COMBO         COMP-5 PIC S9(004).
           05 HIDE-NOEDIT               PIC  9(001).
           05 HIDE-PROT                 PIC  9(001).
           05 HIDE-STRING               PIC  9(001).
           05 HIDE-STRING-DATA          PIC  X(080).
           05 HIDE-buffer.
           10 HIDE-STRING-SHOW          PIC  X(080).

       FD  THUMBS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-THUMBS.

       01  THUMBS-REG.
           05 THUMBS-ID          PIC S9(004) COMP-5.
           05 THUMBS-KEY         PIC  9(003).
           05 THUMBS-POSITION    PIC S9(004) COMP-5.
           05 THUMBS-THUMB    POINTER.

       FD  LISTBOX
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LISTBOX.

       01  LISTBOX-REG.
           05 LISTBOX-ID         COMP-5 PIC S9(004).
           05 LISTBOX-OBJECT            PIC  9(004).
           05 LISTBOX-KEY               PIC  9(001).
           05 LISTBOX-M                 PIC  9(002).

       FD  WORKBOX
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-WORKBOX.

       01  WORKBOX-REG.
           05 WORKBOX-OBJECT            PIC  9(004).
           05 WORKBOX-PROGRAM           PIC  X(008).
           05 WORKBOX-ORDER             PIC  9(001).
           05 WORKBOX-LINE              PIC  9(002).
           05 WORKBOX-COLUMN            PIC  9(002).
           05 WORKBOX-VERTICAL-LENGTH   PIC  9(002).
           05 WORKBOX-HORIZONTAL-LENGTH PIC  9(002).
           05 WORKBOX-STRING-1-LENGTH   PIC  9(002).
           05 WORKBOX-STRING-2-LENGTH   PIC  9(002).
           05 WORKBOX-WORK-AREA         PIC  X(050).
           05 WORKBOX-HIDE              PIC  X(030).
           05 WORKBOX-RETURN            PIC  9(001).
           05 WORKBOX-OPTION            PIC  X(076).
           05 WORKBOX-POSITS OCCURS 2.
              10 WORKBOX-POSIT          PIC  9(004) OCCURS 25.
           05 WORKBOX-GUIA              PIC  9(004).
           05 WORKBOX-FIELD             PIC  9(004).
           05 SEM-DADOS                 PIC  9(001).
           05 COMPOSIT.
              10 COL-1                  PIC  9(003).
              10 COL-2                  PIC  9(003).
              10 COL-B                  PIC  9(003).
              10 WID-1                  PIC  9(003).
              10 WID-2                  PIC  9(003).
              10 WID-G                  PIC  9(003).
              10 RESTO                  PIC  9(004).
              10 WD                     PIC  9(002)V99.
           05 MSGW-POSIT.
              10 L                      PIC  9(002).
              10 C                      PIC  9(002).
              10 ITEM-LEN               PIC  9(002).
           05 STRINGS-1-2.
              10 STRING-1               PIC  X(080).
              10 STRING-2               PIC  X(080).
           05 STRINGX.
              10 STRING-1X              PIC  X(080).
              10 STRING-2X              PIC  X(080).
           05 OK                        PIC  X(001).
           05 GAP2                      PIC  9(002).
           05 BARRA                     PIC  9(005).
           05 LAST-ID            COMP-5 PIC S9(004).
           05 LAST-TAB           COMP-5 PIC S9(004).
           05 BARRPOS                   PIC  9(005).
           05 GD-ID              COMP-5 PIC S9(004).
           05 BARR-ID            COMP-5 PIC S9(004).
           05 FD-ID              COMP-5 PIC S9(004) OCCURS 60.
           05 UM                        PIC  9(004).
           05 FIELDX                    PIC  9(002).
           05 RETURN-ID                 PIC  9(005).
           05 STR                       PIC  9(001).
           05 START-CURSOR              PIC  9(002).
           05 LISTAS.
              06 LISTA           COMP-5 PIC S9(004) OCCURS 25.
              06 LISTA2          COMP-5 PIC S9(004) OCCURS 25.
           05 VAZIO                     PIC  9(003).
           05 END-STRING-1              PIC  X(080).
           05 END-STRING-2              PIC  X(080).
           05 L-UP                      PIC  9(002) COMP-X.
           05 L-DN                      PIC  9(002) COMP-X.
           05 C-UPDN                    PIC  9(002) COMP-X.
           05 VOLTA                     PIC  9(002).
           05 STR-LEN                   PIC  9(002).
           05 ST1                       PIC  9(002).
           05 ST2                       PIC  9(002).
           05 XA                        PIC  9(002).
           05 X2                        PIC  9(002).
           05 ZZ                        PIC  9(002).
           05 SELECTED                  PIC  9(002).
           05 M                         PIC  9(002).
           05 M2                        PIC  9(002).
           05 FIM                       PIC  X(001).
           05 TEXTO-A.
              10 BYTE-A                 PIC  X(001) OCCURS 80.
           05 TEXTO-B.
              10 BYTE-B                 PIC  X(001) OCCURS 80.
           05 MATRIX.
              10 TEXTO-STRING OCCURS 25.
                 15 TEXTO-1             PIC  X(080).
                 15 TEXTO-2             PIC  X(080).
                 15 TEXTO-L             PIC  X(080).
           05 COLOR-ALERT        COMP-X PIC  9(002).
           05 inverte                   PIC  9(003).
           05 USER-IO                   PIC  X(001).
              88 TOP-BOT                             VALUE "B" "b"
                                                           "E" "e".
              88 BEGIN-FILE                          VALUE "B" "b".
              88 END-FILE                            VALUE "E" "e".
              88 AT-END                              VALUE "*".
              88 READ-NEXT                           VALUE "N" "n".
              88 READ-PREVIOUS                       VALUE "P" "p".
              88 NOT-LESS                            VALUE ">".
              88 NOT-GREATER                         VALUE "<".
              88 STARTED                             VALUE "<" ">".

       FD  BUTTONS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-BUTTONS.

       01  BUTTONS-REG.
           05 BUTTONS-ID      COMP-5 PIC S9(004).
           05 BUTTONS-LEN     COMP-5 PIC S9(004).
           05 BUTTONS-LIST           PIC  9(001).
           05 BUTTONS-DATA           PIC  X(080).

       FD  OBJETOS
           LABEL RECORD IS STANDARD
           RECORD VARYING FROM 371 TO 16403 DEPENDING ON SIZE-REG
           VALUE OF FILE-ID IS LB-OBJETOS.

       01  OBJETOS-REG.
           05 OBJETOS-CHAVE.
              10 OBJETOS-WINDOW     COMP-X PIC  9(004).
              10 OBJETOS-SEQUENCE   COMP-X PIC  9(004).
           05 OBJETOS-DATA.
              10 OBJETOS-FIXO              PIC  X(367).
              10 OBJETOS-MOVEL  OCCURS 0 TO 16000
                               DEPENDING ON SIZE-MOVEL PIC X.

       FD  DATANAMES
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-DATANAMES.

       01  DATANAMES-REG.
           05 DATANAMES-ID          COMP-5 PIC S9(004).
           05 DATANAMES-DATA.
           06 DATANAMES-FIELD.
              10 DATANAMES-NAME            PIC  X(030).
              10 DATANAMES-SUBSCRIPT       PIC  9(003).
           06 DATANAMES-POS.
              10 DATANAMES-LIN             PIC  9(002).
              10 DATANAMES-COL             PIC  9(002).
           06 DATANAMES-GUIA               PIC  9(004).
           06 DATANAMES-OFF         COMP-5 PIC S9(004).
           06 DATANAMES-LEN                PIC  9(002).
           06 DATANAMES-INITIAL-LEN COMP-5 PIC S9(004).
           06 DATANAMES-VAR-LEN     COMP-5 PIC S9(004).
           06 DATANAMES-FORMAT-LEN  COMP-5 PIC S9(004).
           06 DATANAMES-VAR-DATA           PIC  X(080).
           06 DATANAMES-HIDE               PIC  9(001).
           06 DATANAMES-COMANDO            PIC  X(001).
           06 DATANAMES-LEN-M              PIC  9(002).
           06 DATANAMES-CRITICA-POSIT      PIC  9(004).
           06 DATANAMES-NUMERIC            PIC  9(001).
           06 DATANAMES-screen-field       PIC  9(001).
           06 DATANAMES-colr               PIC  X(001).

       FD  FROMS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-FROMS.

       01  FROMS-REG.
           05 FROMS-NAME.
              10 FROMS-DATANAME            PIC  X(030).
              10 FROMS-SUBSCRIPT           PIC  9(003).
           05 FROMS-FIELD                  PIC  9(004).

       FD  OBJWORK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-OBJWORK.

       01  OBJWORJ-REG.
           05 OBJECT-CHAVE.
              10 OBJECT-WINDOW   PIC  9(004) COMP-X.
              10 OBJECT-SEQ      PIC  9(004) COMP-X.
           05 OBJECT-DATA.
              10 OBJECT-KEY      PIC  9(003).
              10 objeto-id       PIC S9(004) COMP-5.
              10 OBJECT-ID-S     PIC S9(004) COMP-5.
              10 OBJECT-CTRL     PIC  X(001).
              10 OBJECT-GUIA     PIC  X(030).

       COPY CRITICA.FD.

       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       01  DIGITAVEIS.
           05 PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 PIC X(10) VALUE "1234567890".
           05 PIC X(19) VALUE "!$%&*()-+<>[]'/?,.".
           05 PIC X(01) VALUE '"'.
           05 PIC X(05) VALUE "{}_\ ".
           05 PIC X(51) VALUE
              "ÁÉÍÓÚÝáéíóúýÂÊÎÔÛâêîôûÀÈÌÒÙàèìòùÃÕÑãõñÄÏÖÜäëïöüÿÇçË".

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 CWWBMP-INFILE           PIC  X(255) VALUE SPACES.
           05 SET-FONT                PIC  9(004) VALUE ZERO.
           05 W-L                     PIC  9(002) VALUE ZERO.
           05 W-C                     PIC  9(002) VALUE ZERO.
           05 test-CAMPO              PIC  9(005) VALUE ZERO.
           05 save-campos             PIC  X(484) VALUE SPACES.
           05 save-campos2            PIC  X(484) VALUE SPACES.
           05 DISPLAY-ACCEPT          PIC  9(001) VALUE 0.
           05 SAVE-objects            PIC  9(004) VALUE 0.
           05 save-workbox            pic  x(7346) value spaces.
           05 no-check                PIC  9(001) VALUE 0.
           05 sobrescreveu            PIC  9(001) VALUE 0.
           05 col-w                   PIC  9(002) VALUE 0.
           05 dif                     PIC  9(002) VALUE 0.
           05 dif-campo               PIC  9(005) VALUE 0.
           05 apagando-tela           PIC  9(001) VALUE 0.
           05 WITH-OBJECTS            PIC  9(001) VALUE 0.
           05 CAMPOS-FOUND            PIC  9(001) VALUE 0.
           05 SCREEN-HASH             PIC  X(032) VALUE SPACES.
           05 objects-on              PIC  9(001) VALUE 0.
           05 CORRENTE-ID      COMP-5 PIC S9(004) VALUE 0.
           05 CORRENTE                PIC  X(008) VALUE SPACES.
           05 save-size-fields COMP-5 PIC S9(004) VALUE 0.
           05 save-guia        COMP-5 PIC S9(004) VALUE 0.
           05 MSG-D                   PIC  X(076) VALUE SPACES.
           05 save-CAMPO              PIC  9(005) VALUE 0.
           05 save-CAMPO2             PIC  9(005) VALUE 0.
           05 var-data                PIC  X(076) VALUE SPACES.
           05 TEST-OPTION             PIC  X(076) VALUE SPACES.
           05 len-cri                 PIC  9(002) VALUE 0.
           05 DIGITADO                PIC  X(001) VALUE SPACE.
           05 T-PRE                   PIC  X(080) VALUE SPACE.
           05 case-char               PIC  X(001) VALUE space.
           05 CARACTER                PIC  X(001) VALUE SPACE.
           05 SKIP-LER                PIC  9(001) VALUE 0.
           05 XCUR                    PIC  X(001) VALUE X'1F'.  *> 02
           05 texto-t                 PIC  X(080) VALUE SPACES.
           05 FROM-COMBO              PIC  9(001) VALUE 0.
           05 TECLA-EDIT              PIC  9(003) VALUE 0. COPY CWEDIT.
           05 X91-RESULT       COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION     COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER    COMP-X PIC  9(002) VALUE 0.
           05 X91-parameter-CWOBJE    PIC  9(002) VALUE 0.
           05 ER-TELA.
              10 FS-TELA              PIC  X(002) VALUE "00".
              10 LB-TELA              PIC  X(255) VALUE "cwuser1".
           05 ER-CAMPOS.
              10 FS-CAMPOS            PIC  X(002) VALUE "00".
              10 LB-CAMPOS            PIC  X(255) VALUE "cwuser2".
           05 ER-HIDE.
              10 FS-HIDE              PIC  X(002) VALUE "00".
              10 LB-HIDE              PIC  X(255) VALUE "cwuser3".
           05 ER-LISTBOX.
              10 FS-LISTBOX           PIC  X(002) VALUE "00".
              10 LB-LISTBOX           PIC  X(255) VALUE "cwuser4".
           05 ER-WORKBOX.
              10 FS-WORKBOX           PIC  X(002) VALUE "00".
              10 LB-WORKBOX           PIC  X(255) VALUE "cwuser5".
           05 ER-BUTTONS.
              10 FS-BUTTONS           PIC  X(002) VALUE "00".
              10 LB-BUTTONS           PIC  X(255) VALUE "cwuser6".
           05 ER-CRITICA.
              10 FS-CRITICA           PIC  X(002) VALUE "00".
              10 LB-CRITICA           PIC  X(255) VALUE "cwuser7".
           05 ER-THUMBS.
              10 FS-THUMBS            PIC  X(002) VALUE "00".
              10 LB-THUMBS            PIC  X(255) VALUE "cwuser8".
           05 ER-WINWORK.
              10 FS-WINWORK           PIC  X(002) VALUE "00".
              10 LB-WINWORK           PIC  X(255) VALUE "cwuser9".
           05 ER-DATANAMES.
              10 FS-DATANAMES         PIC  X(002) VALUE "00".
              10 LB-DATANAMES         PIC  X(255) VALUE "cwuserA".
           05 ER-FROMS.
              10 FS-FROMS             PIC  X(002) VALUE "00".
              10 LB-FROMS             PIC  X(255) VALUE "cwuserB".
           05 ER-OBJWORK.
              10 FS-OBJWORK           PIC  X(002) VALUE "00".
              10 LB-OBJWORK           PIC  X(255) VALUE "cwuserC".
           05 ER-MAP.
              10 FS-MAP               PIC  X(002) VALUE "00".
              10 LB-MAP               PIC  X(255) VALUE "cwuserD".
           05 ER-SAVES.
              10 FS-SAVES             PIC  X(002) VALUE "00".
              10 LB-SAVES             PIC  X(255) VALUE "cwuserE".
           05 AGORA                   PIC  X(006) VALUE SPACES.
           05 TIME-E5                 PIC  X(006) VALUE SPACES.
           05 OK2                     PIC  X(001) VALUE X"02".
           05 CWNUMERO                PIC  X(018) VALUE SPACES.
           05 NUMERO                  PIC  9(018) VALUE 0.
           05 CWCONSOLE               PIC  X(003) VALUE SPACES.
           05 FOREBACK                PIC  X(003) VALUE SPACES.
           05 BOR-TYPE                PIC  X(001) VALUE "t".
           05 CWFIND                  PIC  X(003) VALUE SPACES.
           05 ENTER-TERMINATE         PIC  X(003) VALUE SPACES.
           05 CWBEEP                  PIC  X(003) VALUE SPACES.
           05 GUIECHO                 PIC  X(003) VALUE SPACES.
           05 CWPUSH-FONT             PIC  X(030) VALUE SPACES.
           05 CWPUSH-WIDTH            PIC  9(003) VALUE 0.
           05 CWPUSH-HEIGHT           PIC  9(003) VALUE 0.
           05 CWPUSH-FIXED            PIC  X(002) VALUE SPACES.
           05 CWPUSH-BOLD             PIC  X(002) VALUE SPACES.
           05 CWPUSH-ITALIC           PIC  X(002) VALUE SPACES.
           05 CWPUSH-STRIKE-OUT       PIC  X(002) VALUE SPACES.
           05 CWPUSH-UNDERLINE        PIC  X(002) VALUE SPACES.
           05 CWUPDATE                PIC  X(002) VALUE SPACES.
           05 CWUSER-FIELD            PIC  X(001) VALUE SPACE.
           05 GUICOLOR                PIC  X(003) VALUE SPACES.
           05 GUICOLOR-ACCEPT         PIC  X(003) VALUE SPACES.
           05 RESOLUTION              PIC  X(001) VALUE SPACES.
           05 WS-TIMEOUT              PIC  9(018) VALUE 0.
           05 MFF                     PIC  X(003) VALUE SPACES.
           05 MFS                     PIC  X(003) VALUE SPACES.
           05 FONT-MODE               PIC  X(005) VALUE SPACES.
           05 CWAUTO                  PIC  X(003) VALUE SPACES.
           05 CWCONTX                 PIC  X(003) VALUE SPACES.
           05 STATIC                  PIC  X(001) VALUE X"70".
           05 ENTRY-ATTR              PIC  X(001) VALUE X"F0".
           05 DISABLE-ATTR            PIC  X(001) VALUE X"70".
           05 CURCOLOR                PIC  X(001) VALUE X"00".
           05 MB-ATTRIB                           VALUE LOW-VALUES.
              10 MB-ATTRIB-N          PIC  9(002) COMP-X.
           05 CWWIND-LOCKED           PIC  9(001) VALUE 0.
           05 VEZ-COLOR               PIC  9(001) VALUE 0.
           05 COLOR-STATUS            PIC  X(001) VALUE '0'.
              88 COLOR-ON                         VALUE '1'.
              88 COLOR-OFF                        VALUE '0' '3'.
              88 COLOR-CLOSED                     VALUE '3'.
           05 TAMANHO-CHECK    COMP-X PIC  9(008).
           05 REALOC-AREA             PIC  9(002) VALUE 0.
           05 JANELA.
              10 JANLIN               PIC  9(002) VALUE 0.
              10 JANCOL               PIC  9(002) VALUE 0.
              10 JANWID               PIC  9(002) VALUE 80.
              10 JANHEI               PIC  9(002) VALUE 25.
              10 POP-WINDOW    COMP-X PIC  9(004) VALUE 7.
           05 DINAMICA.
              10 DINLIN               PIC S9(002) VALUE 0.
              10 DINCOL               PIC S9(002) VALUE 0.
           05 TIMEOUT-RETURN          PIC  9(002) VALUE 0.
           05 POSLC            COMP-5 PIC S9(004) VALUE 0.
           05 SAVE-CHAVE.
              10 SAVE-JANELA   COMP-5 PIC S9(004).
           05 FIELD-ant        COMP-5 PIC S9(004) VALUE 0.
           05 FIELD            COMP-5 PIC S9(004) VALUE 0.
           05 FIELDS           COMP-5 PIC S9(004) VALUE 0.
           05 FIELD-FONT       COMP-5 PIC S9(004) VALUE 0.
           05 Fm                      PIC  X(001) VALUE SPACES.
              88 FIX-MF                            VALUE "M".
           05 ECHOCURSOR              PIC  X(003) VALUE SPACES.
           05 ECHODELETED             PIC  X(003) VALUE SPACES.
           05 CWCASE                  PIC  X(003) VALUE SPACES.
           05 CWCASE-INPUT            PIC  X(003) VALUE SPACES.
           05 CWLITS                  PIC  X(003) VALUE SPACES.
           05 CWACCENT                PIC  X(003) VALUE SPACES.
           05 TABELA-CORES.
              10 COR                  PIC  X(001) OCCURS 256.
           05 TABELA-MOLDURA.
              10 BASE-MOLDURA         PIC  X(008) OCCURS 9.
           05 MOLDURA                                 VALUE SPACES.
              10 M-201                PIC  X(001).
              10 M-205                PIC  X(001).
              10 M-187                PIC  X(001).
              10 M-186                PIC  X(001).
              10 M-204                PIC  X(001).
              10 M-185                PIC  X(001).
              10 M-200                PIC  X(001).
              10 M-188                PIC  X(001).
           05 MM-205                  PIC  X(080) VALUE SPACES.
           05 ATTR-LOW                PIC  X(080) VALUE LOW-VALUES.
           05 ATTR-HIGH               PIC  X(080) VALUE LOW-VALUES.
           05 ATT-0                   PIC  X(001) VALUE "0".
           05 ATT-P                   PIC  X(001) VALUE "p".
           05 SAVE-OBJECT             PIC  9(001) VALUE 0.
           05 SAVED-SIZE              PIC  9(001) VALUE 0.
           05 M-1                     PIC  9(002) VALUE 0.
           05 END-W                   PIC  9(004) VALUE 0.
           05 INTENSO                 PIC  X(001) VALUE X"7F".
           05 PROGRAMA-A              PIC  X(008) VALUE SPACES.
           05 CWVERSION               PIC  X(010) VALUE SPACES.
           05 FIELDS-A                PIC  9(001) VALUE 0.
           05 FOCUS                   PIC  9(004) VALUE 0.
           05 MULTILINE               PIC  9(004) VALUE 0.
           05 ACCEPTS                 PIC  9(004) VALUE 0.
           05 FLOAT-ED                PIC  X(030) VALUE SPACES.
           05 erase-test              PIC  X(030) VALUE SPACES.
           05 WIN-ID                  PIC  9(004) VALUE 0.
           05 REDEFINES WIN-ID.
              10 X                    PIC  9(002).
              10 Y                    PIC  9(002).
           05 P                COMP-5 PIC s9(004) VALUE 0.
           05 P2               COMP-5 PIC s9(004) VALUE 0.
           05 I                COMP-5 PIC s9(004) VALUE 0.
           05 F                COMP-5 PIC s9(004) VALUE 0.
           05 II               COMP-5 PIC s9(004) VALUE 0.
           05 III              COMP-5 PIC s9(004) VALUE 0.
           05 Y-END            COMP-5 PIC s9(004) VALUE 0.
           05 S                COMP-5 PIC s9(004) VALUE 0.
           05 Z                COMP-5 PIC s9(004) VALUE 0.
           05 Z1               COMP-5 PIC s9(004) VALUE 0.
           05 Z2               COMP-5 PIC s9(004) VALUE 0.
           05 Z3               COMP-5 PIC s9(004) VALUE 0.
           05 T                COMP-5 PIC s9(004) VALUE 0.
           05 cwboxw-attr                         VALUE LOW-VALUES.
              10 BOX-LINE             PIC  X(080) OCCURS 25.
           05 BACK-NW                 PIC  9(002) VALUE 0.
           05 FORE-NW                 PIC  9(002) VALUE 0.
           05 ATTRIBUTE-X.
              10 ATTRIBUTE     COMP-X PIC  9(002) VALUE 0.
           05 ATTRIBUTE-NX.
              10 ATTRIBUTE-N          PIC  9(002) COMP-X VALUE 0.
           05 RM-LINE-ERASE           PIC  9(002) VALUE 0.
           05 WORK                    PIC  X(080) VALUE SPACES.
           05 POS              COMP-5 PIC S9(004) VALUE 0.
           05 SZ-CRITICA              PIC  9(004) VALUE 0.
           05 LEN-ID                  PIC  9(002) VALUE 0.
           05 TEST-EDIT               PIC  X(001) value space.
              88 EDIT-OK VALUE "X" "9" "Z" "*".
           05 TEST-NUM                PIC  X(001) VALUE SPACE.
              88 VIRGULA   VALUE "," ".".
              88 VALOR     VALUE "1" THRU "9".
              88 ALGARISMO VALUE "0" THRU "9".
              88 MASK      VALUE "9" "Z" "*" "-" "+".
              88 VALID     VALUE "0" THRU "9" "-" "+" "," ".".
              88 ZERO-OFF  VALUE "-" "+" "," "." "Z" "*".
              88 EDIT      VALUE "-" "+" "," "." "C" "R"
                                                 "D" "B" ":" "/".
              88 SINAL     VALUE "-" "+" "C" "R" "D" "B".
              88 SINAL-POS VALUE "-" "+" "C" "D".
              88 SINAL-CD  VALUE "C" "R" "D" "B".
           05 SUPERS.
              10 SUPER-TEXTO          PIC  X(080) VALUE SPACES.
              10 SUPER-POS            PIC  9(004) VALUE 0.
              10 SUPER-SIZE           PIC  9(004) VALUE 0.
           05 SIM                     PIC  X(001) VALUE "+".
           05 NAO                     PIC  X(001) VALUE "-".
           05 BUF                     PIC  X(080) VALUE SPACES.
           05 FUN              COMP-5 PIC S9(004) VALUE 0.
           05 FUNDO-EXT               PIC  X(003) VALUE SPACES.
           05 FONT-WIN                PIC S9(001) VALUE -1.
           05 CWPRTS                              VALUE SPACES.
              10 CWPRTS-LIN           PIC  9(002).
              10 CWPRTS-COL           PIC  9(002).
              10 CWPRTS-TXT           PIC  X(003).
           05 OFF-S                   PIC  9(004) VALUE 0.
           05 CRT-STATUS              PIC  X(003) VALUE SPACES.
           05 NUMEROS                 PIC S9(018) VALUE 0.
           05 UNSIGNED                PIC  9(018) VALUE 0.
           05 REDEFINES UNSIGNED.
              10 N PIC 9 OCCURS 18.
           05 CRITICAR-ID             PIC S9(004) COMP-5.
           05 SKIP-CRITICA            PIC  9(001) VALUE 0.
           05 ERRO                    PIC  9(001) VALUE 0.
           05 SKIP-HIDE               PIC  9(001) VALUE 0.
           05 FIELD-CRITICA           PIC  9(004) VALUE 0.
           05 KEY-EXIT                PIC  9(001) VALUE 0.
           05 LEN-CRITICA             PIC  9(004) VALUE 0.
           05 PARAMETROS-CRITICA      PIC X(2000) VALUE SPACES.
           05 USING-MC.
              10 MC                   PIC  9(004) VALUE 0.
              10 MC-MAX               PIC  9(004) VALUE 0.
              10 MC-OFF-L             PIC  9(004) VALUE 0.
              10 MC-LEN-L             PIC  9(004) VALUE 0.
              10 MC-LENC-L            PIC  9(004) VALUE 0.
              10 MCS OCCURS 300.
                 15 MC-FROM           PIC  9(004) VALUE 0.
                 15 MC-OFF            PIC  9(004) VALUE 0.
                 15 MC-LEN            PIC  9(004) VALUE 0.
                 15 MC-LENC           PIC  9(004) VALUE 0.
                 15 MC-FLAG           PIC  X(001) VALUE SPACE.
           05 DELETE-OBJECT           PIC  9(004) VALUE 0.
           05 OBJECTS-CWOBJE   COMP-5 PIC s9(004) VALUE 0.
           05 ALTURA                  PIC  9(002) VALUE 0.
           05 LARGURA                 PIC  9(002) VALUE 0.
           05 CO-S                    PIC  9(002) VALUE 0.
           05 LI-S                    PIC  9(002) VALUE 0.
           05 FI-S                    PIC  9(004) VALUE 0.
           05 MB-SAVE                 PIC  X(400) VALUE SPACES.
           05 MB-STRING               PIC  X(080) VALUE SPACES.
           05 MB-LEN                  PIC  9(002) VALUE 0.
           05 MB-SHIFT                PIC  9(002) VALUE 0.
           05 objeto           COMP-5 PIC S9(004) VALUE 0.
           05 LAST-ACENDER            PIC  9(001) VALUE 0.
           05 ACENDER                 PIC  9(001) VALUE 0.
           05 FLAG-SAVE               PIC  9(002) VALUE 0.
           05 pos-c.
               10 lin-c               PIC  9(002)  VALUE 0.
               10 col-c               PIC  9(002)  VALUE 0.
           05 len-c                   PIC  9(004)  VALUE 0.
           05 CL                      PIC  9(003) VALUE 0.
           05 lenc             COMP-5 PIC S9(004) VALUE 0.
           05 OBJDISA          COMP-5 PIC S9(004) VALUE 0.
           05 BUTTON           COMP-5 PIC S9(004) VALUE 0.
           05 VISUAL                  PIC  9(001) VALUE 0.
           05 BUTTON-TYPE             PIC  X(001) VALUE SPACES.
           05 BUTTON-ON        COMP-5 PIC S9(004) VALUE 387.
           05 color-num        COMP-X PIC  9(002) VALUE 0.
           05 pos-k.
               10 lin-k               PIC  9(002) VALUE 0.
               10 col-k               PIC  9(002) VALUE 0.
           05 dataname-k              pic  x(030) value spaces.
           05 pos-ss                  PIC  X(004) VALUE SPACES.
           05 IO                      PIC  X(002) VALUE SPACES.
           05 PLUS-W                  PIC  9(002) VALUE 0.
           05 d-num            comp-3 PIC  9(018) VALUE 0.
           05 d-num-a          comp-3 PIC  9(018) VALUE 0.
           05 GET-KEY          COMP-5 PIC S9(004) VALUE 0.
           05 KEY-CWBOXF              PIC  9(004) VALUE 0.
           05 ss-campo                pic  x(030) value spaces.
           05 SEGUNDOS         COMP-X PIC  9(002) VALUE 0.
           05 WAIT-SW                 PIC  X(001) VALUE SPACES.
           05 NEWOBJECTS              PIC  X(001) VALUE SPACE.
           05 FROM-DISPLAY            PIC  9(001) VALUE ZERO.
           05 POP-CHAR                PIC  9(004) VALUE 0.
           05 VEZFF                   PIC  9(003) VALUE 0.
           05 CWBOXF                  PIC  9(001) VALUE 0.
           05 PRINT-SCREEN            PIC  9(001) VALUE 0.
           05 M3                      PIC  9(002) VALUE 0.
           05 OFF-w4           COMP-5 PIC S9(004) VALUE 0.
           05     w4           COMP-5 PIC S9(004) VALUE 0.
           05 TEST-GUIA               PIC  9(004) VALUE 0.
           05 F-CICS                  PIC  9(004) VALUE 0.
           05 NEXT-OK                 PIC  9(004) VALUE 0.
           05 skip-keys               PIC  9(001) VALUE 0.
           05 D                       PIC  9(005) VALUE 0.
           05 OFF-C                   PIC  9(004) VALUE 0.
           05 HELP-KEY         COMP-5 PIC S9(004) VALUE 0.
           05 ENTER-BUTTON            PIC  9(001) VALUE 0.
           05 OK-KEY                  PIC  9(001) VALUE 0.
           05 CONTEXT-ID              PIC  X(038) VALUE SPACES.
           05 CD-KEY           COMP-5 PIC S9(004) VALUE 0.
           05 CD-KEY-VERT      COMP-5 PIC S9(004) VALUE 0.
           05 BT-KEY           COMP-5 PIC S9(004) VALUE 0.
           05 sv-datanames-id  COMP-5 PIC S9(004) VALUE 0.
           05 CG                      PIC  9(004) VALUE 0.
           05 guia-up                 PIC  9(004) VALUE 0.
           05 SAVE-OBJETOS            PIC  9(004) VALUE 0.

       01  cwscre.
           05 PS                      PIC  9(001) VALUE 0.
           05 PUT-LIN                             VALUE SPACES.
              10 PUT-COL OCCURS 80    PIC  X(001).
           05 Y1               COMP-5 PIC s9(004) VALUE 0.
           05 Y2               COMP-5 PIC s9(004) VALUE 0.
           05 Y3               COMP-5 PIC s9(004) VALUE 0.
           05 X3               COMP-5 PIC s9(004) VALUE 0.
           05 O                COMP-5 PIC s9(004) VALUE 0.
           05 K                COMP-5 PIC s9(004) VALUE 0.
           05 OK-static               PIC  9(001) VALUE 0.
           05 FONT-OK          COMP-5 PIC S9(004) VALUE 0.
           05 FONT-OFF         COMP-5 PIC S9(004) VALUE 0.
           05 TEST-TOP                PIC  X(001) VALUE SPACE.
              88 TOP-OK                        VALUE
                 X"AA" X"B4" X"B9" X"BB" X"C2" X"C3" X"C5" X"C9"
                 X"CB" X"CC" X"CE" X"D5" X"DA" X"BA" X"B3".
           05 TEST-BOT                PIC  X(001) VALUE SPACE.
              88 BOT-OK                        VALUE
                 X"B4" X"B9" X"BC" X"C0" X"C1" X"C3" X"C5" X"C8" X"CA"
                 X"CE" X"D9" X"BA" X"B3".
           05 TEST-NEXT               PIC  X(001) VALUE SPACE.
              88 NEXT-OK2                      VALUE
                 X"AA" X"C4" X"CD" X"C1" X"C2" X"C5" X"CA" X"CB" X"CD"
                 X"CE" X"D5" X"D9".
           05 TEST-GRID               PIC  X(001) VALUE SPACE.
              88 GRIDS                         VALUE
                 X"DA" X"C4" X"BF" X"B3" X"C3" X"B4" X"C0" X"D9"
                 X"C9" X"CD" X"BB" X"BA" X"CC" X"B9" X"C8" X"BC"
                 X"D5" X"B8" X"C6" X"B5" X"D4" X"BE"
                 X"D6" X"B7" X"C7" X"B6" X"D3" X"BD"
                 X"C1" X"C2" X"C5"
                 X"CA" X"CB" X"CE".
              88 DECORA VALUE X"02" THRU X"08"
                              X"0A" THRU X"1F"
                              X"F0" THRU X"FF"
                              X"B0" THRU X"B2".
              88 OPEN-BOT  VALUE X"C0" X"C8" X"D3" X"D4".
              88 CLOSE-BOT VALUE X"D9" X"BC" X"BD" X"BE".
              88 FIO-H     VALUE X"CD" X"C4".
           05 TEST-entre              PIC  X(001) VALUE SPACE.
              88 entre-ok  VALUE "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
             "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y"
             "z" "‡" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
             "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y"
             "Z" "€".
           05 BOT                     PIC  9(001) VALUE 0.
           05 SAV                     PIC  9(001) VALUE 0.
150719*    05 REBARBA                 PIC  9(002) VALUE 0.
150719*    05 BUFFER-rebarba          PIC  X(080) VALUE spaces.
           05 COR-static       COMP-X PIC  9(002) VALUE 0.
           05 XUSE                    PIC  X(001) VALUE X"01".
           05 ATTRIBUTE-CHOOSE        PIC  X(003) VALUE LOW-VALUES.
           05 ATT-CH REDEFINES ATTRIBUTE-CHOOSE
                                      PIC  9(003).
           05 CHOOSE           COMP-X PIC  9(002) VALUE 0.
           05 CHOOSED                 PIC  9(001) VALUE 0.
           05 sinais                  pic  x(01) value space.
           05 SINAL-ATUAL             PIC  X(002) VALUE SPACES.
           05 SINAL-MASK              PIC  X(002) VALUE SPACES.
           05 SINAL-POSIT             PIC  9(002) VALUE 0.
           05 WORK-ED                 PIC  X(200) VALUE SPACES.
           05 LX                      PIC  9(002) VALUE 0.
           05 SUBSCRIPT               PIC  9(003) VALUE 0.
           05 COMBO                   PIC  9(001) VALUE 0.
           05 DELETE-IMAGE            PIC  X(255) VALUE SPACES.
           05 FUNDO-ID         COMP-5 PIC S9(004) VALUE 0.
           05 STATUS-FUNDO            PIC  X(001) VALUE SPACE.
              88 FUNDO-LIVRE                   VALUE "f".
           05 PANO-COBOL              PIC  X(080) VALUE SPACES.
           05 PANO-COBWARE            PIC  X(080) VALUE SPACES.
           05 FUNDO                   PIC  X(255) VALUE SPACES.
           05 FULL-ON                 PIC  X(003) VALUE SPACES.
           05 TOP-ON                  PIC  X(003) VALUE SPACES.
           05 BMP-STATUS              PIC  X(001) VALUE "f".
              88 BMP-ON                           VALUE "f".
              88 BMP-OFF                          VALUE "F".
           05 LTOP             COMP-X PIC  9(004) VALUE 0.
           05 TOPHEIGHT        COMP-X PIC  9(004) VALUE 0.
           05 T480             COMP-X PIC  9(004) VALUE 480.
           05 TOPLINES                PIC  9(001) VALUE 6.
           05 OLD                     PIC  X(255) VALUE SPACES.
           05 NEW                     PIC  X(255) VALUE SPACES.
           05 OLD-SIZE                PIC  9(003) VALUE 0.
           05 CWLOGT                  PIC  X(002) VALUE SPACES.
           05 CWMENU-ACTIVE           PIC  X(003) VALUE SPACES.
           05 WRITEATTR               PIC  X(003) VALUE SPACES.
           05 CWREMOVE                PIC  X(255) VALUE SPACES.
           05 CWRSIZE                 PIC  9(003) VALUE 0.
           05 FRAME-CHARS             PIC  X(034) VALUE
               "ÚÄ¿³Ã´ÀÙÉÍ»ºÌ¹È¼Õ¸ÆµÔ¾Ö·Ç¶Ó½ÁÂÅÊËÎ".
           05 FRAME                   PIC  X(003) VALUE SPACES.
           05 CWLINES                 PIC  X(002) VALUE SPACES.
           05 MINLINE                 PIC  9(002) VALUE 0.
           05 CWSCRE-STATIC           PIC  X(001) VALUE SPACES.
           05 CTRL-WINDOW             PIC  9(001) VALUE 1.
           05 cwscre-FDS                          VALUE LOW-VALUES.
              10  FD-LIN    OCCURS 25 PIC  X(080).
           05 CWSCRE-FUNCTION         PIC  X(001).
              88 CWSCRE-INSERT-GROUP              VALUE "K".
              88 CWSCRE-DELETE-GROUP              VALUE "k".
              88 CWSCRE-INSERT-TEXT               VALUE "T".
              88 CWSCRE-DELETE-TEXT               VALUE "t".

       01  cwobje.
           05 DELETE-CWREAD            PIC  9(001) VALUE 0.
           05 STRINGS                  PIC  9(004) VALUE 0.
           05 G                        PIC  9(004) VALUE 0.
           05 SIZE-MOVEL        COMP-X PIC  9(004) VALUE 0.
           05 SIZE-REG          COMP-X PIC  9(004) VALUE 0.
           05 ER-OBJETOS.
              10 FS-OBJETOS            PIC  X(002) VALUE "00".
              10 LB-OBJETOS            PIC  X(255) VALUE "cwuserF".
       01  CWOBJE-FUNCTION.
           05                          PIC  X(001).
              88 CWOBJE-GET                        VALUE "g" "G".
              88 CWOBJE-OCCURS                     VALUE "o" "O".
              88 CWOBJE-CHECK-VALIDATE             VALUE "V".
           05 CWOBJE-oc-NUMBER  COMP-X PIC  9(004).

       COPY CWSPWS.
       COPY CWGETL.
       COPY CWSETK.
       COPY CWSETF.
            88 NOBMS VALUE 'U' 'P'  'H' 'S' 'B' 'R' 'h' 'b' 'r' 's' 'X'.
       COPY CWGETF.
       COPY CWFONT.
       COPY CWOBJE.
       COPY CONSOLE.
       01  PARAMETROS-CWUSER. COPY CWUSER.
       01  TECLA          PIC 9(003) VALUE 0. COPY CWKEYS.
       01  MIL     COMP-X PIC 9(008) VALUE 1000.
       01  buffer                  PIC X(32768) VALUE SPACES.
       01  buffer-ctrl             PIC X(32768) VALUE SPACES.
       01  save-buffer             PIC X(32768) VALUE SPACES.

       LINKAGE SECTION.

       01  FUNCAO.
           05 COMANDO  PIC X(001).
              88 FUN-ACCEPT          VALUE "A" "a".
              88 FUN-AREA            VALUE X"02".
              88 FUN-BMP-REFRESH     VALUE X"04".
              88 FUN-CLEAR-FIELDS    VALUE X"03".
              88 FUN-CLEAR-objects   VALUE X"00".
              88 FUN-COLOR-OFF       VALUE X"10".
              88 FUN-COLOR-ON        VALUE X"11".
              88 FUN-CWWIND          VALUE "%".
              88 FUN-DIM             VALUE "W" "w".
              88 FUN-DISPLAY         VALUE "D" "d".
              88 FUN-INITIAL         VALUE X"01".
              88 FUN-MF-CHECK        VALUE "M" "m".
              88 FUN-OBJECT          VALUE "O" "o".
              88 FUN-POSIT           VALUE "P" "p".
              88 FUN-READ            VALUE X"13".
              88 FUN-REFRESH         VALUE "R" "r".
              88 FUN-REFRESH-IMAGES  VALUE "H" "h".
              88 FUN-REFRESH-OBJECTS VALUE X"FF".
              88 FUN-WRITE           VALUE X"14".
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
                    88 UPDATE-LK        VALUE "U" "u".
                    88 FROM-LK          VALUE "F" "f".
                    88 VALUE-LK         VALUE "V".
                    88 PROT-LK          VALUE "P".
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
                 15 PLUS-E-LK   PIC 9(004) COMP-X.
              10 HIDE-LK        PIC 9(001).
              10 SIZE-LK        PIC 9(002).
              10 TIPO-LK        PIC X(001).
              10 ATTR-INIT-LK   PIC X(001).
              10 COM-SINAL-LK   PIC X(001).
              10 MODO-ACCEPT-LK PIC X(001).
                 88 VER-MODO-ACCEPT-LK VALUE SPACES.
                 88 NUMERICO-LK VALUE "n" "N" "+" "-".
              10 CAMPO-LK       PIC 9(005).
              10 FILLER         PIC X(005).
              10 PIC-LK         PIC X(080).
              10 DATANAME-LK    PIC X(030).
              10 DATA-LK        PIC X(080).
              10 CTRL-LK.
                 15             PIC X(050).
                 15 SCREEN-LK   PIC X(030).

       01  TAMANHO-MATRIZ PIC 9(008) COMP-X.
       01  TIMEOUT-SEGUNDOS.
           05 TECLA-LK      PIC 9(003).
           05 FILLER        PIC X(015).
       01  TIMEOUT-TAMANHO  PIC 9(008) COMP-X.
       01  THUMB            PIC 9(0005).

       PROCEDURE DIVISION USING FUNCAO
                                MATRIZ
                                TAMANHO-MATRIZ
                                TIMEOUT-SEGUNDOS
                                TIMEOUT-TAMANHO.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM.

       000-99-FIM. GOBACK.

       100-PROCESSAMENTO.

           IF  FUN-DISPLAY
           OR  FUN-ACCEPT
           OR  FUN-DIM
               IF FUN-DIM
                  MOVE     PROGRAMA TO JANELA
                  SUBTRACT 1      FROM JANLIN
                                       JANCOL
                  MOVE     DP(1:2)  TO POP-WINDOW(1:2)
               END-IF
               IF  POP-WINDOW > WINWORK-WINDOW
                   REWRITE WINWORK-REG
                   MOVE    SIZE-FIELDS      TO SAVE-SIZE-FIELDS
                   INITIALIZE                  WINWORK-DATA
                   MOVE    POP-WINDOW       TO WINWORK-WINDOW
                   MOVE    SAVE-SIZE-FIELDS TO SIZE-FIELDS
                                          INIT-SIZE-FIELDS
                   WRITE WINWORK-REG
               END-IF
               IF  POP-WINDOW > TELA-JANELA
                   INITIALIZE         TELA-REG
                   MOVE POP-WINDOW TO TELA-JANELA
                   MOVE 1          TO CURPOS-LIN
                                      CURPOS-COL
                   MOVE LOW-VALUES TO TELA-ATTRS
                                      TELA-ATTRS-SAVE
                                      SCREEN-CONTROL
                   WRITE TELA-REG
                   READ TELA
               END-IF
               IF  POP-WINDOW < WINWORK-WINDOW
                   MOVE POP-WINDOW TO WINWORK-WINDOW
                   START WINWORK KEY GREATER WINWORK-CHAVE
                   PERFORM UNTIL FS-WINWORK > '09'
                           READ WINWORK NEXT RECORD
                           IF FS-WINWORK < '10'
                              PERFORM TEST AFTER UNTIL FS-HIDE > '09'
                                      MOVE WINWORK-WINDOW
                                        TO HIDE-WINDOW
                                      READ HIDE KEY IS HIDE-WINDOW
                                      IF   FS-HIDE < '10'
                                           DELETE HIDE RECORD
                                      END-IF
                              END-PERFORM
                              DELETE WINWORK RECORD
                           END-IF
                   END-PERFORM
                   MOVE POP-WINDOW TO TELA-JANELA
                   START TELA KEY GREATER TELA-CHAVE
                   PERFORM UNTIL FS-TELA > '09'
                           READ TELA NEXT RECORD
                           IF  FS-TELA < '10'
                           AND TELA-JANELA > POP-WINDOW
                               MOVE tela-janela       TO CAMPOS-JANELA
                               MOVE 0                 TO CAMPOS-CAMPO
                                                         pos-e
                               START CAMPOS KEY NOT LESS CAMPOS-CHAVE
                               PERFORM UNTIL FS-CAMPOS > '09'
                                       READ CAMPOS NEXT RECORD
                                       IF  FS-CAMPOS < '10'
                                       AND CAMPOS-JANELA = tela-janela
                                           move spaces to buffer-ctrl
                                           (campos-off: len-e)
                                           perform drop-campo
                                              thru fim-drop-campo
                                           DELETE CAMPOS RECORD
                                       ELSE
                                           EXIT PERFORM
                                       END-IF
                               END-PERFORM
                               DELETE TELA RECORD
                           ELSE
                               EXIT PERFORM
                           END-IF
                   END-PERFORM
                   MOVE POP-WINDOW TO WINWORK-WINDOW
                                      TELA-JANELA
                   READ WINWORK
                   READ TELA
               END-IF
               IF POP-WINDOW NOT = tela-janela
                  MOVE POP-WINDOW TO TELA-JANELA
                  READ TELA
                  IF FS-TELA > '09'
                     INITIALIZE TELA-REG
                     MOVE POP-WINDOW TO TELA-JANELA
                     MOVE 1          TO CURPOS-LIN
                                        CURPOS-COL
                     MOVE LOW-VALUES TO TELA-ATTRS
                                        TELA-ATTRS-SAVE
                                        SCREEN-CONTROL
                     WRITE TELA-REG
                     READ TELA
                  END-IF
               END-IF
           END-IF

           EVALUATE TRUE
               WHEN FUN-BMP-REFRESH
                    IF POP-WINDOW = 7
                       PERFORM FUNDO-OFF     THRU FIM-FUNDO-OFF
                       DISPLAY "CWFULL"      UPON ENVIRONMENT-NAME
                       ACCEPT  PANO-COBOL    FROM ENVIRONMENT-VALUE
                       MOVE    PANO-COBOL      TO FULL-ON
                       DISPLAY "CWTOP-BMP"   UPON ENVIRONMENT-NAME
                       ACCEPT  PANO-COBWARE  FROM ENVIRONMENT-VALUE
                       MOVE    PANO-COBWARE    TO TOP-ON
                       PERFORM FUNDO-REFRESH THRU FIM-REFRESH-ON
                    END-IF
                    GOBACK
               WHEN FUN-OBJECT
                    MOVE MATRIZ(1: LENGTH PARAMETROS-CWOBJE)
                      TO PARAMETROS-CWOBJE
                    MOVE X91-PARAMETER TO X91-PARAMETER-CWOBJE
                    IF X91-PARAMETER-CWOBJE > 2
                       MOVE TAMANHO-MATRIZ(1:) TO CWOBJE-FUNCTION
                    END-IF
                    PERFORM 670-CWOBJE THRU 670-99-FIM
                    MOVE PARAMETROS-CWOBJE
                      TO MATRIZ(1: LENGTH PARAMETROS-CWOBJE)
               WHEN FUN-CLEAR-FIELDS
                    PERFORM 147-LIMPA-LINHA-FIELD  THRU 147-99-FIM
                            VARYING X FROM 1 BY 1 UNTIL X > janhei
               WHEN FUN-REFRESH-IMAGES
                    continue
               WHEN FUN-MF-CHECK
                    MOVE  COMANDO  TO Fm
               WHEN FUN-CWWIND
                    MOVE  1        TO CWWIND-LOCKED
               WHEN FUN-DIM
                    GOBACK
               WHEN FUN-COLOR-ON
                    SET  COLOR-ON  TO TRUE
                    MOVE 1         TO REALOC-AREA
               WHEN FUN-COLOR-OFF
                    SET  COLOR-OFF TO TRUE
               WHEN FUN-AREA
                    MOVE 1 TO REALOC-AREA
               WHEN FUN-INITIAL
                    PERFORM 300-CLEAR-OBJECTS THRU 300-99-FIM
                    SET     CWOBJE-DROP         TO TRUE
                    PERFORM 670-CWOBJE        THRU 670-99-FIM
                    PERFORM 130-APAGAR-TELA   THRU 130-99-FIM
                            VARYING X FROM 1 BY 1
                              UNTIL X > JANHEI
                    MOVE SPACES TO buffer
                                   buffer-ctrl
                                   save-buffer
                    CLOSE TELA
                    CLOSE CAMPOS
                    CLOSE SAVES
                    IF OBJECTS-ON = 1
                       CLOSE OBJETOS
                       CLOSE HIDE
                    END-IF
                    CLOSE OBJWORK
                    OPEN I-O TELA
                    OPEN I-O CAMPOS
                    OPEN I-O SAVES
                    IF OBJECTS-ON = 1
                       OPEN I-O OBJETOS
                       OPEN I-O HIDE
                    END-IF
                    OPEN I-O OBJWORK
                    INITIALIZE         TELA-REG
                                       CAMPOS-REG
                                       buffer
                                       objetos-reg
                                       PARAMETROS-CWUSER
                                       WINWORK-DATA
                    MOVE 80         TO JANWID
                    MOVE 25         TO JANHEI
                    MOVE 7          TO POP-WINDOW
                    MOVE 1          TO CURPOS-LIN
                                       CURPOS-COL
                    MOVE SPACES     TO cwuser-CHARACTERS
                    MOVE LOW-VALUES TO cwscre-FDS
                                       SCREEN-CONTROL
               WHEN FUN-POSIT
                    MOVE     PROGRAMA (1: 4) TO DINAMICA
                    SUBTRACT 1             FROM DINLIN
                                                DINCOL
               WHEN FUN-READ
                    MOVE MATRIZ(1:LENGTH PARAMETROS-CWUSER)
                      TO PARAMETROS-CWUSER
                    PERFORM 110-LER-TELA    THRU 110-99-FIM
                    MOVE PARAMETROS-CWUSER
                      TO MATRIZ(1:LENGTH PARAMETROS-CWUSER)
               WHEN FUN-REFRESH
                    PERFORM 140-EXIBIR-TELA THRU 140-99-FIM
               WHEN FUN-WRITE
                    MOVE MATRIZ(1:LENGTH PARAMETROS-CWUSER)
                      TO PARAMETROS-CWUSER
                    PERFORM 120-GRAVAR-TELA THRU 120-99-FIM
                    PERFORM 140-EXIBIR-TELA THRU 140-99-FIM
                    MOVE PARAMETROS-CWUSER
                      TO MATRIZ(1:LENGTH PARAMETROS-CWUSER)
                    MOVE ZERO   TO AVANCO-PENDENTE
               WHEN FUN-CLEAR-OBJECTS
                    PERFORM 300-CLEAR-OBJECTS  THRU 300-99-FIM
               WHEN CWCONSOLE = 'ON'
               AND  POS-LK (1) = '0000'
                    PERFORM 150-CONSOLE THRU 150-99-FIM
                    IF CWWIND-LOCKED = 0
                       CALL "CWWIND" USING "$0"
                    END-IF
               WHEN FUN-DISPLAY
                 OR FUN-ACCEPT
                    PERFORM 260-LOAD-HIDE THRU 260-99-FIM
                    OPEN I-O LISTBOX
                    OPEN I-O WORKBOX
                    OPEN I-O BUTTONS
                    OPEN I-O CRITICA
                    OPEN I-O DATANAMES
                    OPEN I-O FROMS
                    PERFORM 160-MONTAR-TELA THRU 160-99-FIM
                    IF FUN-DISPLAY
                       MOVE 1 TO FROM-DISPLAY
                       PERFORM 140-EXIBIR-TELA THRU 140-99-FIM
                       IF  WITH-OBJECTS = 1
                           MOVE ZERO TO WITH-OBJECTS
                           PERFORM 300-CLEAR-OBJECTS  THRU 300-99-FIM
                           PERFORM 270-DEFINE-OBJECTS THRU 270-99-FIM
                       END-IF
                    ELSE
                       PERFORM 140-EXIBIR-TELA THRU 140-99-FIM
                       MOVE ZERO TO PRINT-SCREEN
                       PERFORM 170-ACCEPT-TELA THRU 170-99-FIM
                    END-IF
                    INITIALIZE HIDE-REG
                    MOVE '23' TO FS-HIDE
                    CLOSE LISTBOX
                    PERFORM 220-CLOSE-COMBO THRU 220-99-FIM
                    MOVE    ZERO              TO WORKBOX-OBJECT
                    START WORKBOX KEY NOT LESS WORKBOX-OBJECT
                    PERFORM UNTIL FS-WORKBOX > "09"
                       READ WORKBOX NEXT RECORD
                        NOT AT END
                            PERFORM 700-DROP-LISTBOX THRU 700-990-FIM
                       END-READ
                    END-PERFORM
                    CLOSE WORKBOX
                    MOVE 1 TO BUTTONS-LIST
                    START BUTTONS KEY NOT LESS BUTTONS-LIST
                    PERFORM UNTIL FS-BUTTONS > "09"
                            READ BUTTONS NEXT RECORD
                            IF  FS-BUTTONS < "10"
                                MOVE BUTTONS-ID TO SP2-FD-ID
                                CALL SP2     USING SP2-DELETE-FIELD
                                                   SP2-FIELD-DEF
                            END-IF
                    END-PERFORM
                    CLOSE BUTTONS
                    CLOSE CRITICA
                    CLOSE DATANAMES
                    MOVE SPACES TO FROMS-DATANAME
                    MOVE ZERO   TO FROMS-SUBSCRIPT
                    START FROMS KEY NOT LESS FROMS-NAME
                    PERFORM UNTIL FS-FROMS > '09'
                            READ FROMS NEXT RECORD
                            IF FS-FROMS < '10'
                               MOVE FROMS-DATANAME
                                 TO DATANAME-LK (FROMS-FIELD)
                               SET FROM-LK (FROMS-FIELD) TO TRUE
                            END-IF
                    END-PERFORM
                    CLOSE FROMS
           END-EVALUATE.

       100-99-FIM. EXIT.

       110-LER-TELA.

           COMPUTE POSLC = ((cwuser-LINE - 1) * 80) + cwuser-COLUMN

           IF   cwuser-LENGTH-ATTR NOT = ZERO
                MOVE TELA-ATTRS        (POSLC: cwuser-LENGTH-ATTR)
                  TO cwuser-ATTRIBUTES (1:     cwuser-LENGTH-ATTR)
           END-IF

           IF   cwuser-LENGTH-CHAR = 2000
           AND  (TELA-CHARS (POSLC: cwuser-LENGTH-CHAR) NOT = SPACES)
                MOVE TELA-CHARS        (POSLC: cwuser-LENGTH-CHAR)
                  TO  cwuser-CHARACTERS (1:     cwuser-LENGTH-CHAR)
                MOVE SPACES           TO SCREEN-HASH
                MOVE pop-window       TO CAMPOS-JANELA
                MOVE 0                TO CAMPOS-CAMPO
                                         pos-e
                START CAMPOS KEY NOT LESS CAMPOS-CHAVE
                PERFORM UNTIL FS-CAMPOS > '09'
                        READ CAMPOS NEXT RECORD
                        IF  FS-CAMPOS < '10'
                        AND CAMPOS-JANELA = pop-window
                            MOVE DATA-E TO GUI-LIN (LIN-E)(COL-E: LEN-E)
                            MOVE DATANAME-E TO HIDE-DATANAME
                            MOVE pop-WINDOW TO HIDE-WINDOW
                            READ HIDE
                            IF   FS-HIDE < '10'
                            AND  LIN-E > ZERO
                            AND  COL-E > ZERO
                            AND  HIDE-SIZE > ZERO
                                 MOVE HIDE-STRING-SHOW
                                   TO GUI-LIN (LIN-E)(COL-E: HIDE-SIZE)
                            END-IF
                            IF SCREEN-HASH = SPACES
                               EXEC COBOLware MD5 HASH SCREEN-HASH
                                        FIELD
                                cwuser-CHARACTERS(1:cwuser-LENGTH-CHAR)
                               END-EXEC
                               MOVE pop-window TO SAVES-JANELA
                               START SAVES KEY NOT LESS SAVES-JANELA
                               PERFORM UNTIL FS-SAVES > '09'
                                  READ SAVES NEXT RECORD
                                  IF  FS-SAVES = '00'
                                  AND SAVES-JANELA = pop-window
                                      DELETE SAVES RECORD
                                  ELSE
                                      EXIT PERFORM
                                  END-IF
                               END-PERFORM
                            END-IF
                            MOVE  SCREEN-HASH  TO SAVES-HASH
                            MOVE  pop-window   TO SAVES-JANELA
                            MOVE  CAMPOS-CAMPO TO SAVES-CAMPO
                            MOVE  CAMPOS-REG   TO SAVES-CAMPOS-REG
                            WRITE SAVES-REG
                        ELSE
                            EXIT PERFORM
                        END-IF
                END-PERFORM
           END-IF.

       110-99-FIM. EXIT.

       120-GRAVAR-TELA.

           COMPUTE POSLC = ((cwuser-LINE - 1) * 80) + cwuser-COLUMN

           IF   cwuser-LENGTH-ATTR NOT = ZERO
                MOVE cwuser-ATTRIBUTES (1:     cwuser-LENGTH-ATTR)
                  TO TELA-ATTRS        (POSLC: cwuser-LENGTH-ATTR)
                move spaces to TELA-ATTRS-SAVE
                              (POSLC: cwuser-LENGTH-ATTR)
           END-IF

           IF   cwuser-LENGTH-CHAR NOT = ZERO
           AND  (cwuser-CHARACTERS(1:cwuser-LENGTH-CHAR) NOT = SPACES)
                MOVE cwuser-CHARACTERS (1:     cwuser-LENGTH-CHAR)
                  TO TELA-CHARS        (POSLC: cwuser-LENGTH-CHAR)
                IF   cwuser-LENGTH-CHAR = 2000
                     move low-values to TELA-CHARS-SAVE
                                            (POSLC: cwuser-LENGTH-CHAR)
                     EXEC COBOLware MD5
                          HASH SCREEN-HASH
                          FIELD cwuser-CHARACTERS(1:cwuser-LENGTH-CHAR)
                     END-EXEC
                     MOVE SCREEN-HASH TO SAVES-HASH
                     MOVE pop-window TO SAVES-JANELA
                     MOVE 0           TO SAVES-CAMPO CAMPOS-FOUND
                     START SAVES KEY NOT LESS SAVES-CHAVE
                     PERFORM UNTIL FS-SAVES > '09'
                        READ SAVES NEXT RECORD
                        IF FS-SAVES = '00'
                           IF  SAVES-HASH   = SCREEN-HASH
                           AND SAVES-JANELA = pop-window
                               IF CAMPOS-FOUND = 0
                                  MOVE 1          TO CAMPOS-FOUND
                                  MOVE pop-window TO CAMPOS-JANELA
                                  MOVE 0          TO CAMPOS-CAMPO
                                                     pos-e
                                  START CAMPOS KEY NOT LESS CAMPOS-CHAVE
                                  PERFORM UNTIL FS-CAMPOS > '09'
                                          READ CAMPOS NEXT RECORD
                                         IF FS-CAMPOS < '10'
                                         AND CAMPOS-JANELA = pop-window
                                           move spaces to buffer-ctrl
                                           (campos-off: len-e)
                                             DELETE CAMPOS RECORD
                                         ELSE
                                             EXIT PERFORM
                                         END-IF
                                  END-PERFORM
                               END-IF
                               MOVE SAVES-CAMPOS-REG TO CAMPOS-REG
                               WRITE CAMPOS-REG FROM SAVES-CAMPOS-REG
                               DELETE SAVES RECORD
                           ELSE
                               EXIT PERFORM
                           END-IF
                        END-IF
                     END-PERFORM
                END-IF
           END-IF.

       120-99-FIM. EXIT.

       130-APAGAR-TELA.

           PERFORM 146-LIMPA-LINHA-STATIC THRU 146-99-FIM
           move    1                        to apagando-tela
           PERFORM 147-LIMPA-LINHA-FIELD  THRU 147-99-FIM
           move    0                        to apagando-tela
           move    low-values               to tela-attr-lin(x)
           MOVE    SPACES                   TO TELA-LIN(X)
                                               TELA-LIN-save(X).

       130-99-FIM. EXIT.

       140-EXIBIR-TELA.

           PERFORM VARYING X FROM 1 BY 1 UNTIL X > JANHEI
                PERFORM 147-LIMPA-LINHA-FIELD THRU 147-99-FIM
                IF (TELA-LIN(X) NOT = TELA-LIN-SAVE(X))
                OR (TELA-ATTR-LIN(X) NOT = TELA-ATTR-LIN-SAVE (X))
                   PERFORM 145-LINHA THRU 145-99-FIM
                   MOVE TELA-LIN      (X) TO TELA-LIN-SAVE (X)
                   MOVE TELA-ATTR-LIN (X) TO TELA-ATTR-LIN-SAVE (X)
                END-IF
           END-PERFORM
           MOVE pop-window       TO CAMPOS-JANELA
           MOVE ZERO             TO CAMPOS-GUIA RADIOS
           MOVE INIT-SIZE-FIELDS TO SIZE-FIELDS
           START CAMPOS KEY NOT LESS CAMPOS-JANELA-GUIA
           PERFORM UNTIL FS-CAMPOS > '09'
                   READ CAMPOS NEXT RECORD
                   IF  FS-CAMPOS < '10'
                   AND CAMPOS-JANELA = pop-window
                       PERFORM 162-SP2-INTERFACE THRU 162-99-FIM
                       REWRITE CAMPOS-REG
                   ELSE
                       EXIT PERFORM
                   END-IF
           END-PERFORM
           move    TELA-CHARS   TO cwuser-CHARACTERS
           PERFORM PANO-FUNDO THRU FIM-PANO-FUNDO
           IF FUN-ACCEPT
           OR FUN-REFRESH
           OR FUN-WRITE
              PERFORM DD THRU FIM-DD
           END-IF
           REWRITE TELA-REG.

       140-99-FIM. EXIT.

       145-LINHA.

           MOVE LOW-VALUES  TO SP2-SD-DATA

           IF   RESOLUTION = 1
                MOVE 11     TO SP2-SD-HEIGHT
           ELSE
                MOVE 10     TO SP2-SD-HEIGHT
           END-IF

           IF  LINHA(X)(1:160)   = LOW-VALUES *> Somente FONT-IDs
           AND TELA-LIN      (X) = SPACES
           AND TELA-ATTR-LIN (X) = LOW-VALUES
               MOVE  0          TO Y-END
           ELSE
               MOVE janwid          TO Y-END
               PERFORM VARYING Y FROM janwid BY -1
                                UNTIL Y = 1
                                   OR (TELA-COL (X Y) NOT = SPACE)
                                   OR (TELA-ATTR-COL (X Y) NOT = X"00")
                                   OR (FONT-ID  (X Y) NOT = 0)
                      MOVE Y TO Y-END
               END-PERFORM
           END-IF
           PERFORM 146-LIMPA-LINHA-STATIC THRU 146-99-FIM

           IF  (TELA-LIN (X) = SPACES)
           AND (TELA-ATTR-LIN (X) = LOW-VALUES)
               EXIT PARAGRAPH
           END-IF

           MOVE ZERO TO PS
           PERFORM VARYING Y
                      FROM 1
                        BY 1
                   UNTIL Y > Y-END
              MOVE Y             TO Y3
              ADD  1             TO LIN-IDS (X)
              MOVE LIN-IDS (X)   TO O
              CALL "CWSPID"   USING SP2-SD-ID "SInsert"
              MOVE SP2-SD-ID     TO COL-ID (X O)
              COMPUTE SP2-SD-ROW  = X - 1
              COMPUTE SP2-SD-COL  = Y - 1
              MOVE 1             TO SP2-SD-TEXT-LEN
                                    SP2-SD-VAR-LEN
              MOVE 1             TO K
              MOVE X"00"         TO SP2-SD-TYPE
                                    SP2-SD-MISC-OPTIONS
              MOVE ZERO          TO OK-static
              PERFORM UNTIL OK-static = 1
                      IF   TELA-ATTR-COL (X Y) = X'06'
                           MOVE X'00' TO TELA-ATTR-COL (X Y)
                      END-IF
                      IF   FONT-ID (X Y) NOT = ZERO
                           MOVE FONT-ID (X Y) TO FONT-OK
                           MOVE ZERO          TO FONT-ID (X Y)
                           MOVE FONT-DX (X Y) TO FONT-OFF
                           MOVE ZERO          TO FONT-DX (X Y)
                      END-IF
                      MOVE TELA-ATTR-COL (X Y) TO SP2-SD-COLR
                      MOVE TELA-COL      (X Y) TO SP2-SD-TEXT (K: 1)
                                                  TEST-GRID
                      IF   Y > 1
                      AND  GRIDS
                           PERFORM CHECK-ENTRE THRU FIM-CHECK-ENTRE
                      END-IF
                      IF   GRIDS
                           PERFORM 149-GRID THRU 149-99-FIM
                      ELSE
                           MOVE ZERO TO BOT
                      END-IF
                      IF (MFF NOT = "MF")
                      AND (TELA-COL (X Y) = SPACE
                      AND Y > 71 AND SAV = 0)
                      AND (NOT FIX-MF)
                      and JANWID = 80
                           MOVE 1 TO PS
                      END-IF
                      IF   Y = Y-END
                      OR   GRIDS OR DECORA
                      OR  (TELA-COL (X Y) = SPACE
                           AND Y > 71 and sav = 0
                           AND (NOT FIX-MF)
                           and JANWID = 80)
                      OR  (TELA-ATTR-COL (X Y + 1) NOT = SP2-SD-COLR)
                      OR ((FONT-ID (X Y + 1) NOT = 0)
                      AND (FONT-ID (X Y + 1) NOT = FONT-OK))
                      OR  (Y < 78
                      AND (NOT GRIDS)
                      AND  TELA-COL (X Y + 1) = SPACE
                      AND  TELA-COL (X Y + 2) = SPACE
                      AND (TELA-COL (X Y) NOT = SPACE))
                      AND (NOT GRIDS)
                      OR  (Y < JANWID
                      AND  TELA-COL (X Y + 1) = X"00")
                      OR  (Y < janwid
                      AND  TELA-COL (X Y + 1) = ":" OR '['
                      AND (FONT-MODE NOT = "FIXED")
                      AND (NOT FIX-MF))
                      AND JANWID = 80
                           MOVE 1 TO OK-static
                      ELSE
                           ADD  1              TO Y
                           MOVE TELA-COL (X Y) TO TEST-GRID
                           IF   Y > 1
                           AND  GRIDS
                                perform check-entre thru fim-check-entre
                           END-IF
                           IF   GRIDS
                                SUBTRACT 1 FROM Y
                                MOVE 1 TO OK-static
                           ELSE
                                ADD  1  TO K
                                MOVE K  TO SP2-SD-TEXT-LEN
                                           SP2-SD-VAR-LEN
                           END-IF
                      END-IF
              END-PERFORM
              IF  DECORA
                  SUBTRACT 1 FROM K
                             SP2-SD-TEXT-LEN
                             SP2-SD-VAR-LEN
              END-IF
              IF K > 0
                 IF   SP2-SD-TYPE = X"00"
                      move 1 to OK-static
                 END-IF
                 COMPUTE SP2-SD-WIDTH = K * 10
                 IF TELA-COL (X Y + 1) = X"00"
                    SUBTRACT 1 FROM SP2-SD-WIDTH
                 END-IF
                 IF  SP2-SD-TYPE = "l"
                     MOVE 3     TO SP2-SD-FONT-ID
                     MOVE X"01" TO SP2-SD-MISC-OPTIONS
                     MOVE X"00" TO SP2-SD-MISC-OPTIONS
                 ELSE
                     IF   FONT-MODE = "FIXED"
                     OR   FIX-MF
                     or   JANWID NOT = 80
                          MOVE 1 TO SP2-SD-FONT-ID
                          IF   FIX-MF
                          or   JANWID NOT = 80
                               MOVE 4 TO SP2-SD-FONT-ID
                          END-IF
                     ELSE
                          MOVE -1 TO SP2-SD-FONT-ID
150719*                   PERFORM
150719*                    UNTIL (SP2-SD-TEXT (1:1) NOT = SPACE)
150719*                       OR (SP2-SD-TEXT (1:SP2-SD-TEXT-LEN)
150719*                          = SPACES)
150719*                           ADD 1 TO SP2-SD-COL
150719*                                    REBARBA
150719*                           SUBTRACT 10 FROM SP2-SD-WIDTH
150719*                           SUBTRACT 1
150719*                               FROM SP2-SD-TEXT-LEN
150719*                                    SP2-SD-VAR-LEN
150719*                           MOVE SP2-SD-TEXT (2: )
150719*                             TO BUFFER-rebarba
150719*                           MOVE BUFFER-rebarba
150719*                             TO SP2-SD-TEXT
150719*                   END-PERFORM
                     END-IF
                     IF  FONT-OK > 0
                         MOVE FONT-OK TO SP2-SD-FONT-ID
                         IF   FONT-ID (X Y + 1) = -1
                              MOVE ZERO TO FONT-OK
                              MOVE ZERO TO FONT-ID (X Y + 1)
                         END-IF
                         IF  FONT-OFF = 1
                             MOVE ZERO TO FONT-OK FONT-OFF
                         END-IF
                     END-IF
                 END-IF
                 IF  SP2-SD-COL < 71
                     MOVE ZERO TO PS
                 END-IF
                 IF   SP2-SD-TEXT-LEN NOT = ZERO
150719*          AND (SP2-SD-TEXT (1:SP2-SD-TEXT-LEN) NOT = SPACES)
                     ADD PS  TO SP2-SD-COL
                     ADD 1   TO SP2-SD-WIDTH
                     MOVE 11 TO SP2-SD-HEIGHT
                     IF SP2-SD-FONT-ID > 4
                        MOVE LOW-VALUES     TO SP2-FO-DATA
                        MOVE SP2-SD-FONT-ID TO SP2-FO-ID
                        CALL SP2 USING SP2-GET-FONT-DEF SP2-FONT-DEF
                        IF SP2-FO-WIDTH NOT = 0
                           COMPUTE SP2-SD-WIDTH = SP2-FO-WIDTH
                                                * (SP2-SD-TEXT-LEN + 1)
                        END-IF
                        IF SP2-FO-HEIGHT NOT = ZERO
                           MOVE SP2-FO-HEIGHT TO SP2-SD-HEIGHT
                        END-IF
                     END-IF
                     INSPECT SP2-SD-TEXT (1:SP2-SD-TEXT-LEN)
                             CONVERTING X'09' TO X'20'
                     PERFORM 148-ACENTOS THRU 148-99-FIM
                     CALL SP2   USING SP2-SET-STATIC-DEF
                                      SP2-STATIC-DEF
                 ELSE
                     MOVE ZERO        TO COL-ID (X O)
                     CALL "CWSPID" USING SP2-SD-ID "SDelete"
                 END-IF
150719*          IF REBARBA NOT = ZERO
150719*             COMPUTE SP2-SD-COL = SP2-SD-COL - REBARBA
150719*             MOVE SPACES  TO SP2-SD-TEXT
150719*             COMPUTE SP2-SD-WIDTH = REBARBA * 10
150719*             MOVE REBARBA TO SP2-SD-TEXT-LEN
150719*                             SP2-SD-VAR-LEN
150719*             IF   REB-ID (X O) = 0
150719*                  CALL "CWSPID" USING REB-ID (X O) "SInsert"
150719*             END-IF
150719*             MOVE REB-ID (X O) TO SP2-SD-ID
150719*             ADD PS TO SP2-SD-COL
150719*             IF SP2-SD-COLR NOT = SP2-COLR-DEFAULT
150719*                MOVE SP2-SD-COLR TO COR-STATIC(1:1)
150719*                COMPUTE Y1 = COR-STATIC / 16
150719*                COMPUTE Y2 = COR-STATIC - (Y1 * 16)
150719*                IF Y1 = Y2
150719*                   ADD 1 TO Y2
150719*                   COMPUTE COR-STATIC = (Y1 * 16) + Y2
150719*                   MOVE COR-STATIC(1:2) TO SP2-SD-COLR
150719*                END-IF
150719*             END-IF
150719*             CALL SP2 USING SP2-SET-STATIC-DEF
150719*                            SP2-STATIC-DEF
150719*             MOVE ZERO   TO REBARBA
150719*          END-IF
              END-IF
              MOVE 0 TO FONT-OK
              IF  DECORA
                  MOVE LOW-VALUES  TO SP2-SD-DATA
                  IF   RESOLUTION = 1
                       MOVE 11     TO SP2-SD-HEIGHT
                  ELSE
                       MOVE 10     TO SP2-SD-HEIGHT
                  END-IF
                  MOVE ZERO        TO SP2-SD-TEXT-LEN
                                      SP2-SD-VAR-LEN
                  CALL "CWSPID" USING SP2-SD-ID "SInsert"
                  MOVE SP2-SD-ID   TO DEC-ID (X Y)
                  COMPUTE SP2-SD-ROW = X - 1
                  COMPUTE SP2-SD-COL = Y - 1
                  MOVE 4             TO SP2-SD-FONT-ID
                  MOVE TELA-ATTR-COL (X Y) TO SP2-SD-COLR
                  PERFORM UNTIL (NOT DECORA)
                                 OR Y > Y-END
                                 OR (TELA-ATTR-COL (X Y)
                                    NOT = SP2-SD-COLR)
                      OR ((FONT-ID (X Y) NOT = 0)
                      AND (FONT-ID (X Y) NOT = FONT-OK))
                          ADD  1              TO SP2-SD-TEXT-LEN
                                                 SP2-SD-VAR-LEN
                          ADD  10             TO SP2-SD-WIDTH
                          MOVE TELA-COL (X Y) TO SP2-SD-TEXT
                                                (SP2-SD-VAR-LEN: 1)
                          ADD  1              TO Y
                          IF  Y < Y-END
                              MOVE TELA-COL (X Y) TO TEST-GRID
                          END-IF
                  END-PERFORM
                  SUBTRACT 1           FROM Y
                  PERFORM  148-ACENTOS THRU 148-99-FIM
                  INSPECT  SP2-SD-TEXT (1:SP2-SD-TEXT-LEN)
                        CONVERTING X'0A' TO X'20'
                  CALL SP2            USING SP2-SET-STATIC-DEF
                                            SP2-STATIC-DEF
              END-IF
           END-PERFORM.

       145-99-FIM. EXIT.

       CHECK-ENTRE.

           If  X > 1
               MOVE TELA-COL (X - 1 Y) TO TEST-TOP
               IF TEST-GRID = TEST-TOP
               OR TOP-OK
                  EXIT PARAGRAPH
               END-IF
               If  X < 25
                   MOVE TELA-COL (X + 1 Y) TO TEST-BOT
                   IF TEST-GRID = TEST-TOP
                   OR BOT-OK
                      EXIT PARAGRAPH
                   END-IF
               END-IF
           END-IF
           IF  Y < 80
               MOVE TELA-COL (X Y + 1) TO TEST-NEXT
               IF NEXT-OK2
                  EXIT PARAGRAPH
               END-IF
           END-IF
           MOVE TELA-COL (X Y - 1) TO TEST-ENTRE
           IF ENTRE-OK
              MOVE SPACE TO TEST-GRID
           END-IF
           IF Y < 80
              MOVE TELA-COL (X Y + 1) TO TEST-ENTRE
              IF ENTRE-OK
150719*          MOVE SPACE TO TEST-GRID
150719           IF   Y > 1
150719                MOVE TELA-COL (X Y - 1) TO TEST-ENTRE
150719                IF ENTRE-OK
150719                   MOVE SPACE TO TEST-GRID
150719                END-IF
150719           ELSE
150719                MOVE SPACE TO TEST-GRID
150719           END-IF
                 MOVE TELA-COL (X Y + 1) TO TEST-ENTRE
              END-IF
           END-IF.

       FIM-CHECK-ENTRE. EXIT.

       146-LIMPA-LINHA-STATIC.

           IF LINHA (X) NOT = LOW-VALUES
              PERFORM VARYING Y FROM JANWID BY -1
                        UNTIL Y = 0
                      MOVE TELA-COL (X Y) TO TEST-GRID
                      IF   GRID-ID (X Y) NOT = ZERO
                           MOVE GRID-ID (X Y) TO SP2-SD-ID
                           MOVE ZERO          TO GRID-ID (X Y)
                           CALL "CWSPID"   USING SP2-SD-ID "SDelete"
                           CALL SP2        USING SP2-DELETE-STATIC
                                                 SP2-STATIC-DEF
                      END-IF
                      IF COL-ID (X Y) NOT = ZERO
                         MOVE COL-ID (X Y) TO SP2-SD-ID
                         MOVE ZERO         TO COL-ID (X Y)
                         MOVE LOW-VALUES   TO SP2-SD-TEXT
                         CALL SP2       USING SP2-GET-STATIC-DEF
                                              SP2-STATIC-DEF
                         IF SP2-SD-FONT-ID > 4
                         AND FONT-ID (X SP2-SD-COL + 1) = 0
                            PERFORM VARYING I FROM 1 BY 1
                                      UNTIL I > (janwid - 1)
                                      OR (SP2-SD-TEXT(I: I) NOT = X"00")
                                 CONTINUE
                            END-PERFORM
                            IF  SP2-SD-TEXT(1: I) =
                                TELA-LIN (X) (SP2-SD-COL + 1: I)
                                MOVE SP2-SD-FONT-ID TO FONT-ID
                                                    (X SP2-SD-COL + 1)
                                MOVE 1              TO FONT-DX
                                                    (X SP2-SD-COL + 1)
                            END-IF
                         END-IF
                         CALL "CWSPID" USING SP2-SD-ID "SDelete"
                         CALL SP2      USING SP2-DELETE-STATIC
                                             SP2-STATIC-DEF
                      END-IF
150719*               IF  REB-ID (X Y) NOT = ZERO
150719*                   MOVE REB-ID (X Y) TO SP2-SD-ID
150719*                   MOVE ZERO         TO REB-ID (X Y)
150719*                   CALL "CWSPID"  USING SP2-SD-ID "SDelete"
150719*                   CALL SP2       USING SP2-DELETE-STATIC
150719*                                        SP2-STATIC-DEF
150719*               END-IF
                      IF  DEC-ID (X Y) NOT = ZERO
                          MOVE DEC-ID (X Y)  TO SP2-SD-ID
                          MOVE ZERO          TO DEC-ID (X Y)
                          CALL "CWSPID"   USING SP2-SD-ID "SDelete"
                          CALL SP2        USING SP2-DELETE-STATIC
                                                SP2-STATIC-DEF
                      END-IF
              END-PERFORM
           END-IF

           MOVE ZERO TO LIN-IDS (X).

       146-99-FIM. EXIT.

       147-LIMPA-LINHA-FIELD.

           IF FDS (X) NOT = LOW-VALUES
              move zero to SP2-FD-ID
              PERFORM VARYING Y FROM janwid BY -1
                        UNTIL Y = 0
                IF  ENTRY-ID (X Y) NOT = ZERO
                    if ENTRY-ID (X Y) not = SP2-FD-ID
                       MOVE ENTRY-ID (X Y) TO SP2-FD-ID
                       perform test after until fs-campos > '09'
                               MOVE pop-window TO CAMPOS-JANELA
                               move sp2-fd-id  to campos-fd-id
                               read campos key is campos-id
                               if  fs-campos < '10'
                                   IF FUN-CLEAR-FIELDS
                                   OR apagando-tela = 1
150719                             OR BRANCO(LIN-E COL-E) = SPACE
                                      move spaces to buffer-ctrl
                                                (campos-off: len-e)
                                                     buffer
                                                (campos-off: len-e)
                                      DELETE CAMPOS RECORD
                                   ELSE
                                      MOVE ZERO TO CAMPOS-FD-ID
                                      REWRITE CAMPOS-REG
                                   END-IF
                               end-if
                       end-perform
                       CALL    "CWSPID"      USING SP2-FD-ID "FDelete"
                       PERFORM 041-DROP-FIELD THRU 041-99-FIM
                    end-if
                    MOVE ZERO           TO ENTRY-ID (X Y)
                    IF  FUN-CLEAR-FIELDS
                    OR  apagando-tela = 1
                        MOVE ZERO       TO TELA-CAMPO (X Y)
                    END-IF
                END-IF
              END-PERFORM
           END-IF.

       147-99-FIM. EXIT.

       148-ACENTOS.

           IF CWREMOVE NOT = SPACES
              INSPECT SP2-SD-TEXT (1: SP2-SD-TEXT-LEN)
                      CONVERTING CWREMOVE (1: CWRSIZE)
                              TO SPACES
           END-IF

           IF SP2-SD-FONT-ID NOT = ZERO
              IF SP2-SD-FONT-ID NOT = SP2-FO-ID
                 MOVE SP2-SD-FONT-ID TO SP2-FO-ID
                 MOVE SPACES         TO SP2-FO-NAME
                 CALL SP2         USING SP2-GET-FONT-DEF SP2-FONT-DEF
              END-IF
           ELSE
              MOVE SPACES  TO SP2-FO-NAME
              IF FIX-MF
              OR JANWID NOT = 80
                 MOVE "MF" TO SP2-FO-NAME
              END-IF
           END-IF
           IF   CWLITS = "UPP"
                INSPECT SP2-SD-TEXT (1: SP2-SD-TEXT-LEN)
                        CONVERTING MINUSCULAS
                                TO MAIUSCULAS
           END-IF
           IF   CWLITS = "LOW"
                INSPECT SP2-SD-TEXT (1: SP2-SD-TEXT-LEN)
                        CONVERTING MAIUSCULAS
                                TO MINUSCULAS
           END-IF
           IF   CWACCENT = "OFF"
                INSPECT SP2-SD-TEXT (1: SP2-SD-TEXT-LEN)
                        CONVERTING ACENTOS-850
                                TO ACENTOS-OFF
           ELSE
                IF   SP2-FO-NAME (1:2) = "MF"
                     INSPECT SP2-SD-TEXT (1: SP2-SD-TEXT-LEN)
                             CONVERTING ACENTOS-850
                                     TO ACENTOS-437
                ELSE
                     INSPECT SP2-SD-TEXT (1: SP2-SD-TEXT-LEN)
                          CONVERTING ACENTOS-850
                                  TO ACENTOS-WINDOWS
                END-IF
           END-IF

           IF SP2-SD-COLR NOT = SP2-COLR-DEFAULT
              MOVE SP2-SD-COLR TO COR-static(1:1)
              COMPUTE Y1 = COR-static / 16
              COMPUTE Y2 = COR-static - (Y1 * 16)
              IF Y1 = Y2
                 ADD 1 TO Y2
                 COMPUTE COR-static = (Y1 * 16) + Y2
                 MOVE COR-static(1:2) TO SP2-SD-COLR
              END-IF
           END-IF.

       148-99-FIM. EXIT.

       149-GRID.

           MOVE 1 TO K
           PERFORM UNTIL Y > 79
                      OR (TELA-COL (X Y + 1) NOT = TEST-GRID)
                      OR (TELA-COL (X Y + 1)     = X"B3")
                      OR (TELA-COL (X Y + 1)     = X"BA")
                   ADD 10 TO SP2-SD-WIDTH
                   ADD 1  TO Y K
           END-PERFORM
           MOVE K             TO SP2-SD-TEXT-LEN
                                 SP2-SD-VAR-LEN
           MOVE SPACE         TO SP2-SD-TEXT
           CALL "CWSPID"   USING GRID-ID (X O) "SInsert"
           MOVE X"00"         TO SP2-SD-TYPE
                                 SP2-SD-MISC-OPTIONS
           COMPUTE SP2-SD-WIDTH = K * 10
           MOVE -1 TO SP2-SD-FONT-ID
           IF  OPEN-BOT
               MOVE 1 TO BOT
           END-IF
           IF  CLOSE-BOT
               IF BOT = 1
                  MOVE 9  TO SP2-SD-HEIGHT
                  MOVE 10 TO SP2-SD-HEIGHT
               END-IF
               MOVE ZERO TO BOT
           END-IF
           IF  BOT = 1
               MOVE 10 TO SP2-SD-HEIGHT
           END-IF
           IF SP2-SD-COLR NOT = SP2-COLR-DEFAULT
              MOVE SP2-SD-COLR TO COR-static(1:1)
              COMPUTE Y1 = COR-static / 16
              COMPUTE Y2 = COR-static - (Y1 * 16)
              IF Y1 = Y2
                 ADD     1                     TO Y2
                 COMPUTE COR-static = (Y1 * 16) + Y2
                 MOVE    COR-static(1:2) TO SP2-SD-COLR
              END-IF
           END-IF
           CALL SP2        USING SP2-SET-STATIC-DEF SP2-STATIC-DEF
           MOVE "l"           TO SP2-SD-TYPE
           MOVE X"01"         TO SP2-SD-MISC-OPTIONS
           MOVE GRID-ID (X O) TO SP2-SD-ID
           MOVE TEST-GRID     TO SP2-SD-TEXT.

       149-99-FIM. EXIT.

       150-CONSOLE.

           INITIALIZE PARAMETROS-CONSOLE
           MOVE 'CWUSER' TO CONSOLE-PROGRAM
           IF FUN-ACCEPT
              MOVE 1 TO FIELD
              CALL "CWLOCK" USING "X"
              MOVE MATRIZ-E (1) TO CAMPOS-define
              IF   DATANAME-E = 'CWSTOP'
                   CALL 'CONSOLE' USING PARAMETROS-CONSOLE 'STOP'
              ELSE
                   MOVE LEN-E TO CONSOLE-LENGTH
                   CALL 'CONSOLE' USING PARAMETROS-CONSOLE 'ACCT'
                   MOVE CONSOLE-MSG (1:LEN-E) TO DATA-LK(1)(1:LEN-E)
              END-IF
           ELSE
           COMPUTE FIELDS = TAMANHO-MATRIZ / LENGTH OF MATRIZ-E(1)
              MOVE 1 TO I
              PERFORM VARYING FIELD FROM 1 BY 1 UNTIL FIELD > FIELDS
                      MOVE MATRIZ-E (FIELD) TO CAMPOS-define
                      MOVE DATA-E (1:LEN-E) TO CONSOLE-MSG (I:LEN-E)
                      ADD  LEN-E            TO I
              END-PERFORM
              CALL 'CONSOLE' USING PARAMETROS-CONSOLE
           END-IF.

       150-99-FIM. EXIT.

       160-MONTAR-TELA.

           IF   NOT COLOR-ON
                IF VEZ-COLOR = 0
                   MOVE 1 TO VEZ-COLOR
                   MOVE
                  'Programa cont‚m comandos de tela e chamadas SP2'
                         TO MSG-D
                   PERFORM SEND-SP2
                END-IF
                EXIT PARAGRAPH
           END-IF

           CALL "CWCURS" USING "G" CURPOS
           CALL "CWATTW" USING "G" cwboxw-attr TELA-JANELA
           COMPUTE FIELDS = TAMANHO-MATRIZ / LENGTH OF MATRIZ-E(1)
           COMPUTE TAMANHO-CHECK = LENGTH OF MATRIZ-E(1) * FIELDS

           IF  TAMANHO-CHECK NOT = TAMANHO-MATRIZ
               MOVE "Tela inconsistente" TO MSG-D
               PERFORM SEND-SP2
               IF CWWIND-LOCKED = 0
                  CALL "CWWIND" USING "$0"
               END-IF
               EXIT PARAGRAPH
           END-IF

           IF   PROGRAMA NOT = PROGRAMA-A
                MOVE PROGRAMA TO PROGRAMA-A
                DISPLAY 'CWVERSION' UPON ENVIRONMENT-NAME
                ACCEPT CWVERSION    FROM ENVIRONMENT-VALUE
                IF CWVERSION NOT = SPACES
                   MOVE LOW-VALUES TO SP2-WD-DATA
                   CALL SP2     USING SP2-GET-WINDOW-DEF SP2-WINDOW-DEF
                   PERFORM VARYING I FROM LENGTH SP2-WD-TITLE BY -1
                             UNTIL I = 0
                                OR SP2-WD-TITLE (I:1) NOT = SPACE
                           CONTINUE
                   END-PERFORM
                   ADD  2         TO I
                   MOVE CWVERSION TO SP2-WD-TITLE (I:)
                   CALL SP2    USING SP2-SET-WINDOW-DEF SP2-WINDOW-DEF
                   MOVE SPACES    TO CWVERSION
                   DISPLAY CWVERSION UPON ENVIRONMENT-VALUE
                END-IF
           END-IF

           IF   FUN-ACCEPT
                CALL    "CWSETK"    USING PARAMETROS-CWSETK "Get"
                CANCEL  "CWSETK"
                CALL    "CWKBBF"    USING "C"
                CALL    "CWKBST"    USING "C"
                CANCEL  "CWCRTS"
                CANCEL  "CWAKEY"
                MOVE    ZERO           TO FIELDS-A
                PERFORM 200-BMS-ATTR THRU 200-99-FIM
           END-IF

           MOVE  pop-window       TO CAMPOS-JANELA
           MOVE  1                TO CAMPOS-GUIA
           START CAMPOS KEY NOT LESS CAMPOS-JANELA-GUIA
           PERFORM UNTIL FS-CAMPOS > '09'
                   READ CAMPOS NEXT RECORD
                   IF  FS-CAMPOS < '10'
                   AND CAMPOS-JANELA = pop-window
                       MOVE ZERO TO CAMPOS-GUIA
                                    CAMPOS-FIELD
                       REWRITE CAMPOS-REG
                   ELSE
                       EXIT PERFORM
                   END-IF
           END-PERFORM

      *> Ajustes

           MOVE ZERO TO MULTILINE
                        FIELDS-A
                        ACCEPTS
                        SET-FONT
150719     MOVE ALL '*' TO CONTROLE-BRANCOS
           PERFORM VARYING FIELD FROM 1 BY 1 UNTIL FIELD > FIELDS
                   IF  LIN-LK (FIELD) NUMERIC
                       ADD DINLIN TO LIN-LK (FIELD)
                   END-IF
                   IF  COL-LK (FIELD) NUMERIC
                       ADD DINCOL TO COL-LK (FIELD)
                   END-IF
                   PERFORM UNTIL COL-LK (FIELD) NOT NUMERIC
                              OR COL-LK (FIELD) < (JANWID + 1)
                           SUBTRACT JANWID FROM COL-LK (FIELD)
                           IF  LIN-LK (FIELD) NUMERIC
                               ADD 1 TO LIN-LK (FIELD)
                           END-IF
                   END-PERFORM
                   IF   LENF-LK (FIELD) NUMERIC
                   AND (LENF-LK (FIELD) NOT = ZERO)
                       IF NOT (LEN-LK(FIELD) NUMERIC
                       AND     LEN-LK(FIELD) NOT = ZERO)
                         MOVE LENF-LK (FIELD) TO LEN-LK(FIELD)
                       END-IF
                   END-IF
                   if len-lk(field) not = 0
                      inspect data-lk (field) (1: len-lk(field))
                              CONVERTING x"00FF98"
                                      to x"202020"
                   else
                      inspect data-lk (field)
                              CONVERTING x"00FF98"
                                      to x"202020"
                   end-if
                   move zero to subscript
                   IF  DATANAME-LK (FIELD) NOT = SPACES
                       perform varying i from 1 by 1 until i > field
                              if dataname-lk (i) = dataname-lk(field)
                                 add 1 to subscript
                              end-if
                       end-perform
                       IF   VER-MODO-ACCEPT-LK (FIELD)
                       AND  LEN-LK (FIELD) < 80
                            compute y = LEN-LK (FIELD) + 1
                            MOVE SPACES TO DATA-LK (FIELD) (Y:)
                       END-IF
                   END-IF
                   IF  FLOAT-LK (FIELD)
                       EXEC COBOLware Float-Point
                            NUMERIC       DATA-LK(FIELD)
                            INTO          FLOAT-ED
                            DECIMAL-POINT DP
                       END-EXEC
                       MOVE 30                 TO LENF-LK(FIELD)
                       MOVE FLOAT-ED           TO DATA-LK(FIELD)(1:30)
                       MOVE PIC-LK(FIELD)(1:30)TO PIC-LK(FIELD)(51:30)
                       MOVE ALL 'X'            TO PIC-LK(FIELD)(1:30)
                   END-IF
                   IF  DATANAME-LK (FIELD) = "ALL"
                   AND LEN-LK      (FIELD) = 80
                       COMPUTE LEN-LK (FIELD) = 80
                             - COL-LK (FIELD) + 1
                       MOVE SPACES TO DATANAME-LK (FIELD)
                   END-IF
                   IF DATANAME-LK (FIELD) (1: 6) = "CWFONT"
                      PERFORM 164-SET-FONT THRU 164-99-FIM
                      EXIT PERFORM CYCLE
                   END-IF
                   IF  FROM-LK (FIELD)
                   AND (DATANAME-LK (FIELD) (1: 7) NOT = "CWRADIO")
                   AND (DATANAME-LK (FIELD) (1: 7) NOT = "CWCHECK")
                       IF  DATANAME-LK (FIELD) NOT = SPACES
                           MOVE DATANAME-LK (FIELD)
                             TO FROMS-DATANAME
                           MOVE SUBSCRIPT TO FROMS-SUBSCRIPT
                           MOVE FIELD TO FROMS-FIELD
                           WRITE FROMS-REG
                       END-IF
                       MOVE SPACES TO DATANAME-LK (FIELD)
                       SET VALUE-LK (FIELD) TO TRUE
                   END-IF
                   IF  ACCEPT-LK (FIELD)
                       MOVE pop-window TO CAMPOS-JANELA
                       IF   FUN-ACCEPT
                            ADD 1 TO ACCEPTS
                       END-IF
                       IF CAMPO-LK (FIELD) NUMERIC
                          MOVE pop-window         TO CAMPOS-JANELA
                          move dataname-lk(field) to DATANAME-E
                          move SUBSCRIPT          to CAMPOS-SUBSCRIPT
                          move POS-LK (FIELD)     TO POS-E
                          READ CAMPOS KEY IS CAMPOS-NAME
                          IF   FS-CAMPOS < '10'
                               perform 161-compor-elemento
                                  thru 161-99-fim
                               if   fs-campos > '09'
                                    READ CAMPOS key is campos-name
                                    if   fs-campos > '09'
                                         WRITE CAMPOS-REG
                                    end-if
                               end-if
                               MOVE FIELD            TO CAMPOS-FIELD
                               MOVE ACCEPTS          TO CAMPOS-GUIA
                               move hide-lk (field)  to hide-e
                               move SET-FONT         to campos-font-id
                               REWRITE CAMPOS-REG
                          ELSE
                               MOVE SPACES TO CAMPO-LK(FIELD)(1:)
                          END-IF
                       END-IF
                       IF CAMPO-LK (FIELD) NOT NUMERIC
                          move dataname-lk(field) to ss-campo
                          MOVE LIN-LK     (FIELD) TO X
                          MOVE COL-LK     (FIELD) TO Y
                          MOVE LEN-LK     (FIELD) TO S
                          PERFORM 180-SOBRESCREVE-CAMPOS THRU 180-99-FIM
                          PERFORM 190-NOVO-CAMPO         THRU 190-99-FIM
                          MOVE FIELD            TO CAMPOS-FIELD
                          MOVE ACCEPTS          TO CAMPOS-GUIA
                          MOVE CAMPOS-CAMPO     TO CAMPO-LK (FIELD)
                          MOVE MATRIZ-E (FIELD) TO CAMPOS-define
                          move subscript        to campos-subscript
                          move 1                to sobrescreveu
                          perform 161-compor-elemento thru 161-99-fim
                          WRITE CAMPOS-REG
                          READ  CAMPOS KEY   IS CAMPOS-NAME
                       END-IF
                       MOVE 3                TO FIELDS-A
                   ELSE
                      IF  (FROM-LK  (FIELD)
                      OR   VALUE-LK (FIELD))
                      AND (PIC-LK   (FIELD) (1: 1) = 'X' OR SPACE)
                          move spaces to erase-test
                          if  len-lk(FIELD) = 80
                          and lin-lk(FIELD) = 01
                          and col-lk(FIELD) = 01
                               MOVE DATANAME-LK (FIELD) to erase-test
                               inspect erase-test
                                       converting minusculas
                                               to maiusculas
                          end-if
                          MOVE SPACES TO DATANAME-LK (FIELD)
                                         PIC-LK      (FIELD)
                          SET  VALUE-LK              (FIELD) TO TRUE
                          if   erase-test = 'SPACE' or 'SPACES'
                               move 'SPACES' TO DATANAME-LK (FIELD)
                          end-if
                      END-IF
                   END-IF
                   IF (POS-LK (1) = "0101"
                   AND DATANAME-LK (FIELD)(1:5) = 'SPACE'
                   AND ADVANCE-LK  (FIELD) =  'a')
                   OR (POS-LK      (FIELD) = "0000"
                   AND DATANAME-LK (FIELD) = SPACES
                   AND ADVANCE-LK  (FIELD) =  'A')
                   AND FIELD > 1
                       ADD 1 TO MULTILINE
                   END-IF
                   perform 161-compor-elemento thru 161-99-fim
           END-PERFORM.

       160-99-FIM. EXIT.

       161-compor-elemento.

           IF  POS-LK (FIELD) = '0000'
           AND CURPOS-LIN  = JANHEI
           AND CURPOS-COL  > JANWID
               PERFORM 211-AVANCO
           END-IF

           IF   CWCONTX NOT = 'OFF'
                IF  NO-UPDATE
                AND (FUN-ACCEPT AND (CWUPDATE NOT = "ON"))
                    MOVE SPACES TO DATA-LK (FIELD)
                END-IF
                CALL "CWCTRL" USING CTRL-LK (FIELD)
                                    ATTR-LK (FIELD)
                IF   LENF-LK (FIELD) NUMERIC
                AND  LENF-LK (FIELD) > LEN-LK (FIELD)
                AND (PIC-LK  (FIELD) (1: 1) = "9" OR "Z")
                     COMPUTE P = LENF-LK (FIELD)
                               - LEN-LK (FIELD) + 1
                     MOVE LEN-LK  (FIELD)         TO P2
                     MOVE DATA-LK (FIELD) (P: P2) TO BUF
                     MOVE BUF
                       TO DATA-LK (FIELD)
                END-IF
           END-IF

           IF  LIN-LK (FIELD) NUMERIC
           AND LIN-LK (FIELD) > JANHEI
               PERFORM UNTIL LIN-LK (FIELD) NOT > JANHEI
                  MOVE JANHEI                TO CURPOS(1:2)
                  MOVE '01'                  TO CURPOS(3:2)
                  MOVE FIELDS                TO FI-S
                  MOVE FIELD                 TO FIELDS
                  MOVE COL-LK (FIELD)        TO CO-S
                  MOVE LIN-LK (FIELD)        TO LI-S
                  MOVE 0                     TO COL-LK (FIELD)
                  MOVE 0                     TO LIN-LK (FIELD)
                  MOVE "A"                   TO ADVANCE-E
                  MOVE JANWID                TO s
                  PERFORM 211-CHECK-AVANCO THRU 211-99-FIM
                  MOVE FI-S                  TO FIELDS
                  MOVE LI-S                  TO LIN-LK (FIELD)
                  SUBTRACT 1               FROM LIN-LK (FIELD)
                  MOVE CO-S                  TO COL-LK (FIELD)
               END-PERFORM
           END-IF

           IF  DATA-LK (FIELD) (1: 1) = X"0B"
               MOVE DATA-LK (FIELD) (2: 1) TO ATTRIBUTE-X
               IF DATA-LK (FIELD) (3: 1) < X'20'
                  COMPUTE  MB-ATTRIB-N = (ATTRIBUTE - 1) * 16
                  MOVE     DATA-LK (FIELD) (3: 1) TO ATTRIBUTE-X
                  subtract 1                    from ATTRIBUTE
                  ADD      ATTRIBUTE              TO MB-ATTRIB-N
                  MOVE     3                      TO MB-SHIFT
               ELSE
                  MOVE ATTRIBUTE-X TO MB-ATTRIB
                  MOVE 2           TO MB-SHIFT
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
                  MOVE MATRIZ-E (FIELD) TO CAMPOS-define
                  IF ADVANCE-LK (FIELD) = 'L'
                     SET ERASE-EOL TO TRUE
                  ELSE
                     SET ERASE-EOS TO TRUE
                  END-IF
                  GO TO EVA
               END-IF
               IF  DATA-LK (FIELD) (MB-SHIFT + 1: ) = SPACES
                   MOVE 0 TO MB-LEN
                   GO     TO 161-MB-FIM
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
               go to 161-99-FIM
           END-IF

           IF  (PIC-LK (FIELD) (1: 1) = "9" OR "Z" OR "*" OR "-" OR "+")
                MOVE "n" TO MODO-ACCEPT-LK (FIELD)
                            MODO-ACCEPT
                IF   DP = "."
                     INSPECT PIC-LK  (FIELD) (1: LEN-LK(FIELD))
                             CONVERTING ",." TO ".,"
                     INSPECT DATA-LK (FIELD) (1: LEN-LK(FIELD))
                             CONVERTING ",." TO ".,"
                END-IF
           END-IF

           MOVE MATRIZ-E (FIELD) TO CAMPOS-define

           IF  LEN-E = 0
           AND STATIC = SPACES
               MOVE LIN-E TO CURPOS-LIN
               MOVE COL-E TO CURPOS-COL
               GO TO 161-99-FIM
           END-IF

           PERFORM 410-CHECK-PLUS THRU 410-99-FIM

           IF  (DATANAME-LK (FIELD) NOT = SPACES)
           AND  VER-MODO-ACCEPT
                MOVE DATANAME-LK (FIELD) TO DATANAME-E
                INSPECT CTRL-LK  (FIELD)
                        CONVERTING MINUSCULAS TO MAIUSCULAS
                MOVE CTRL-LK (FIELD) TO CTRL-E
           END-IF

           IF   MODE-LK (FIELD) = "V"
                MOVE "v" TO MODE-LK (FIELD)
                INSPECT DATA-LK (FIELD) (1: LEN-E)
                        CONVERTING X"FB" TO '"'
                MOVE DATA-LK (FIELD) (1: LEN-E) TO DATA-E
           END-IF
           MOVE "23"             TO FS-HIDE
           MOVE 0                TO HIDE-LK (FIELD)

           IF   DATANAME-E NOT = SPACES
                INITIALIZE HIDE-REG
                MOVE DATANAME-E TO HIDE-DATANAME
                MOVE pop-WINDOW TO HIDE-WINDOW
                READ HIDE
                IF   FS-HIDE < "10"
                     IF  (HIDE-FIELD = 0)
                     AND (HIDE-LIST = 1)
                     AND  FUN-ACCEPT
                     AND  ACCEPT-E
                          move ACCEPTS   TO GUIA
                          MOVE GUIA      TO HIDE-GUIA-IND
                          MOVE HIDE-GUIA TO FIELD-ID (GUIA)
                          MOVE GUIA      TO GUIA-FIELDS
                     END-IF
                     MOVE FIELD TO HIDE-FIELD
                     MOVE 0     TO FLAG-SAVE
                     IF   ACCEPT-E
                     AND  HIDE-POS = 0
                     AND  HIDE-LIST = 1
                          COMPUTE HIDE-POS = SIZE-FIELDS + 1
                          ADD  LEN-LK (FIELD) TO SIZE-FIELDS
                          MOVE LEN-LK (FIELD) TO SIZE-LK(FIELD) P
                          MOVE DATA-LK(FIELD) TO buffer
                                                (HIDE-POS: P)
                          MOVE 1 TO FLAG-SAVE
                     END-IF
                     PERFORM X11-DISPLAY-LIST THRU X11-99-FIM
                     REWRITE HIDE-REG
                     IF   HIDE-LIST = 1
                          continue
                     ELSE
                          MOVE 1 TO HIDE-LK (FIELD)
                     END-IF
                END-IF
           END-IF

           MOVE POS-E       TO POS-EX

           IF   FUN-ACCEPT
           AND  POS-E = "0000" AND (POS-S NOT = "0000")
                MOVE POS-S  TO POS-E
           ELSE
                MOVE "0000" TO POS-S
           END-IF
           MOVE 0 TO FLAG-SA
           IF   LIN-E = 0
           AND  COL-E = 0
           AND  ERASE-EOS
                 MOVE 1 TO LIN-E
                           COL-E
           END-IF
           IF   LIN-E = 0
                PERFORM 211-AVANCO
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

           IF  DATANAME-E = SPACES
               MOVE 1 TO FLAG-SA
           END-IF

           IF  (FUN-DISPLAY
           OR  (FLAG-SA = 1 AND (POS-LK (FIELD) NOT = "0000")))
                MOVE POS-E TO CURPOS
                IF   FLAG-SA = 1
                AND  FUN-ACCEPT
                     MOVE LEN-E TO s
                     PERFORM 211-CHECK-AVANCO THRU 211-99-FIM
                END-IF
           END-IF.
       eva.
           MOVE 0 TO FLAG-SA
           move zeros to pos-c
           EVALUATE TRUE
               WHEN ERASE-EOL
                    MOVE LIN-E TO X
                    MOVE COL-E TO Y
                    COMPUTE S = JANWID - COL-E + 1
                    PERFORM 180-SOBRESCREVE-CAMPOS THRU 180-99-FIM
                    MOVE SPACES TO TELA-LIN (X)(Y:S)
                    MOVE 0      TO LEN-E
                    MOVE LIN-E  TO CURPOS-LIN
                    MOVE COL-E  TO CURPOS-COL
               WHEN ERASE-EOS
                 or (blank-screen and pos-e > 0101)
                    IF  LIN-E = 1
                    AND COL-E = 1
                        PERFORM 130-APAGAR-TELA THRU 130-99-FIM
                                VARYING X FROM 1 BY 1 UNTIL X > JANHEI
                        CANCEL "CWBOXR"
                    ELSE
                        MOVE LIN-E TO X
                        MOVE COL-E TO Y
                        COMPUTE S = JANWID - COL-E + 1
                                 + (JANWID * (JANHEI - LIN-E))
                        PERFORM 180-SOBRESCREVE-CAMPOS THRU 180-99-FIM
                        MOVE    SPACES TO TELA-LIN (X)(Y:)
                        ADD     1      TO X
                        PERFORM 130-APAGAR-TELA THRU 130-99-FIM
                                VARYING X FROM X BY 1 UNTIL X > JANHEI
                    END-IF
                    MOVE    0           TO LEN-E
                    MOVE    LIN-E       TO CURPOS-LIN
                    MOVE    COL-E       TO CURPOS-COL
               WHEN (BLANK-SCREEN and pos-e < 0102)
                    PERFORM 130-APAGAR-TELA THRU 130-99-FIM
                            VARYING X FROM 1 BY 1 UNTIL X > JANHEI
               WHEN BLANK-LINE
                    MOVE LIN-E TO X
                    PERFORM 130-APAGAR-TELA THRU 130-99-FIM
                    MOVE 1  TO CURPOS-COL
                    MOVE 0  TO LEN-E
               WHEN FUN-DISPLAY
               OR   ACCEPT-E
                    If NOT (DATANAME-LK(FIELD) = 'SPACES' or 'SPACE')
                       move dataname-lk(field) to ss-campo
                    end-if
                    If (lin-e = 01
                    and col-e = 01
                    and len-e = 80
                    and DATANAME-LK(FIELD) = 'SPACES' or 'SPACE')
                        set ad-eos to true
                    end-if
                    if ad-eos
                       COMPUTE S = janwid - COL-E + 1
                                + (janwid * (janhei - LIN-E))
                       MOVE LIN-E TO X
                       MOVE COL-E TO Y
                       PERFORM 180-SOBRESCREVE-CAMPOS THRU 180-99-FIM
                       MOVE SPACES TO TELA-LIN (X)(Y:)
                       ADD  1      TO X
                       PERFORM 130-APAGAR-TELA THRU 130-99-FIM
                               VARYING X FROM X BY 1 UNTIL X > JANHEI
                    end-if
                    if ad-eol
                       COMPUTE S = JANWID - COL-E + 1
                       MOVE LIN-E TO X
                       MOVE COL-E TO Y
                       PERFORM 180-SOBRESCREVE-CAMPOS THRU 180-99-FIM
                       MOVE SPACES TO TELA-LIN (X)(Y:)
                    end-if
                    move lin-e to lin-c
                    move col-e to col-c
                    compute len-c = len-e - 1
                    if  len-c = 0 move 1 to len-c end-if
                    add len-c to col-c
                    perform until col-c < (JANWID + 1)
                            if lin-c < JANHEI
                               add 1 to lin-c
                            end-if
                            subtract JANWID from col-c
                    end-perform
                    MOVE LIN-E TO X
                    MOVE COL-E TO Y
                    MOVE LEN-E TO S
                    PERFORM 180-SOBRESCREVE-CAMPOS THRU 180-99-FIM
                    IF   DATANAME-E = 'SPACES' OR 'SPACE'
                         MOVE SPACES
                           TO DATA-E (1:LEN-E)
                    END-IF
                    if   NOT accept-e
                    OR  (FUN-DISPLAY AND DATANAME-E = SPACES
                       AND NOT ACCEPT-E)
                         IF   SECURE-E = "S"
                              MOVE all "*"
                                TO TELA-LIN(LIN-E)(COL-E:LEN-E)
                         ELSE
                              MOVE DATA-E
                                TO TELA-LIN(LIN-E)(COL-E:LEN-E)
                         END-IF
150719                   MOVE SPACES TO BRANCOS(LIN-E)(COL-E:LEN-E)
                    ELSE
                         MOVE SPACES
                           TO TELA-LIN(LIN-E)(COL-E:LEN-E)
150719                   MOVE ALL "-" TO BRANCOS(LIN-E)(COL-E:LEN-E)
                    end-if
                    IF  (GUICOLOR NOT = 'OFF')
                    OR  (CTRL-E(1:8) = 'STANDARD')
                         PERFORM 210-SET-ATTRIBUTES THRU 210-99-FIM
                    ELSE
                         IF CWCONTX = 'OFF'
                            MOVE LOW-VALUES
                              TO TELA-ATTR-LIN (LIN-E) (COL-E: LEN-E)
                            IF  (CTRL-LK (FIELD) (50: 13) =
                                "reverse-video" OR "REVERSE-VIDEO")
                                 INSPECT TELA-ATTR-LIN
                                         (LIN-E) (COL-E: LEN-E)
                                  CONVERTING LOW-VALUE TO X"0F"
                            END-IF
                         END-IF
                    END-IF
                    IF   FUN-ACCEPT
                    AND  ACCEPT-E
                    AND  VER-MODO-ACCEPT
                         SET INDEFINIDO TO TRUE
                         IF   PIC-E = "X" OR "A"
                              INSPECT PIC-E (1: s)
                                      CONVERTING " A" TO "XX"
                              SET ALFABETICO TO TRUE
                         END-IF
                         IF   PIC-E = "9"
                              INSPECT PIC-E (1: s)
                                      CONVERTING SPACE TO "9"
                              SET NUMERICO TO TRUE
                         END-IF
                         IF   PIC-E = "Z"
                              INSPECT PIC-E (1: s)
                                      CONVERTING SPACE TO "Z"
                              SET NUMERICO TO TRUE
                         END-IF
                         PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > s
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
                                        SET COM-BARRA TO TRUE
                                        EXIT PERFORM
                                   END-IF
                              END-PERFORM
                         END-IF
                         MOVE MODO-ACCEPT TO MODO-ACCEPT-LK (FIELD)
                         MOVE PIC-E       TO PIC-LK         (FIELD)
                    END-IF
                    IF  (NOT VER-MODO-ACCEPT)
                         MOVE COL-E TO I
                         MOVE 1     TO Y
                         PERFORM LEN-E TIMES
                           IF   PIC-E (Y: 1) = "X" OR "9" OR "A" OR "Z"
                                            OR "-" OR "+" OR "*"
                                IF (DATANAME-E (1: 7) = "CWCHECK"
                                                     OR "CWRADIO")
                                AND LEN-E = 1
                                AND PIC-E = "X"
                                    ADD 1 TO I
                                    SUBTRACT 2 FROM I
                                    ADD  1     TO I
                                END-IF
                           END-IF
                           ADD 1 TO I Y
                         END-PERFORM
                    END-IF
                    IF  (BZERO-E = "Z" OR "L" OR "R")
                    AND (PIC-E (1: 1) NOT = "X")
                         MOVE DATA-LK (FIELD)  TO BUF
                         PERFORM VARYING I FROM 1 BY 1
                                   UNTIL I > LEN-E
                              OR (BUF (I: 1) NUMERIC
                             AND (BUF (I: 1) NOT = "0"))
                                 CONTINUE
                         END-PERFORM
                         IF I > LEN-E
                            MOVE SPACES TO TELA-LIN(LIN-E)(COL-E:LEN-E)
                                           DATA-E (1: LEN-E)
                                           DATA-LK (FIELD)
                                                   (1: LEN-E)
                         END-IF
                    END-IF
                    IF   COM-BARRA
                         MOVE COL-E TO Y
                         PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN-E
                                 IF (PIC-E (I: 1) = "/" OR ":")
                                 AND TELA-LIN(LIN-E)(Y:1) = SPACE
                                     MOVE PIC-E (I: 1)
                                       TO TELA-LIN(LIN-E)(Y:1)
                                 END-IF
                                 ADD 1 TO Y
                         END-PERFORM
                    END-IF
                    MOVE SPACES TO SINAL-ATUAL
                    IF   COM-SINAL NOT NUMERIC
                         MOVE 0 TO COM-SINAL
                         PERFORM VARYING I FROM 1 BY 1
                                  UNTIL I > LEN-E
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
                                  UNTIL I > LEN-E
                                 IF  DATA-LK(FIELD) (I: 1) = "-"
                                     MOVE "-" TO SINAL-ATUAL
                                 END-IF
                                 IF  DATA-LK(FIELD) (I: 2) = "DB"
                                     MOVE "DB" TO SINAL-ATUAL
                                 END-IF
                         END-PERFORM
                         IF  SINAL-ATUAL = SPACES
                             PERFORM VARYING I FROM 1 BY 1
                                      UNTIL I > LEN-E
                                     IF  PIC-E (I: 1) = "-" OR "+"
                                         MOVE "+" TO SINAL-ATUAL
                                     END-IF
                                     IF  PIC-E (I: 2) = "DB" OR "CR"
                                         MOVE "CR" TO SINAL-ATUAL
                                     END-IF
                             END-PERFORM
                         END-IF
                         IF  SINAL-ATUAL = "-" OR "DB"
                         AND(TELA-ATTR-LIN(LIN-E)(COL-E: 1)
                           = ATT-P OR ATT-0)
                             IF  TELA-ATTR-LIN(LIN-E)(COL-E: 1) = ATT-0
                                 MOVE 48  TO CL
                             ELSE
                                 MOVE 112 TO CL
                             END-IF
                             ADD  5 TO CL
                             MOVE COR (CL)  TO ATTRIBUTE-NX
                             MOVE ALL X"00"
                               TO TELA-ATTR-LIN(LIN-E)(COL-E: LEN-E)
                             INSPECT TELA-ATTR-LIN(LIN-E)(COL-E: LEN-E)
                                     CONVERTING X"00" TO ATTRIBUTE-NX
                         END-IF
                    END-IF
                    IF   SECURE-E = "S"
                         MOVE SPACES TO TELA-LIN(LIN-E)(COL-E:LEN-E)
                    END-IF
                    IF (DATANAME-E (1: 7) = "CWCHECK" OR "CWRADIO")
                    AND LEN-E = 1
                    AND PIC-E = "X"
                        MOVE LIN-E TO X
                        MOVE COL-E TO Y
                        PERFORM 180-SOBRESCREVE-CAMPOS THRU 180-99-FIM
                        MOVE SPACES TO TELA-LIN (X)(Y:S)
                    END-IF
                    *> Duvida
                    IF   FUN-ACCEPT
                         PERFORM 162-SP2-INTERFACE THRU 162-99-FIM
                    END-IF
                    IF   CWBEEP NOT = "OFF"
                    AND ("B" = BEEP-E OR BEEP-LK (FIELD))
                         PERFORM 720-XE5 THRU 720-99-FIM
                    END-IF
                    IF  FUN-DISPLAY
                    OR  RM-EOL
                    OR  RM-EOS
                        PERFORM 211-CHECK-AVANCO THRU 211-99-FIM
                    END-IF
                    IF  RM-EOL
                    OR  RM-EOS
                        COMPUTE S = JANWID - COL-E + 1
                                           - LEN-E
                        MOVE LIN-E TO X
                        COMPUTE Y = COL-E + LEN-E + 1
                        PERFORM 180-SOBRESCREVE-CAMPOS THRU 180-99-FIM
                        MOVE SPACES TO TELA-LIN (X)(Y:S)
                        IF  RM-EOS
                            ADD 1 TO X
                            PERFORM 130-APAGAR-TELA THRU 130-99-FIM
                                  VARYING X FROM X BY 1 UNTIL X > JANHEI
                        END-IF
                    END-IF
                    move spaces to ss-campo
           END-EVALUATE
           if pos-c = zeros
              move CURPOS to pos-c
           end-if

           CALL "CWCURS" USING "S" CURPOS.

       161-mb-fim.

           IF  MB-LEN > 0
               MOVE MB-STRING TO DATA-lk (field)
               go to 161-compor-elemento
           else
               if mb-save not = spaces
                  move mb-save to matriz-e (field)
                  move spaces  to mb-save
               end-if
           end-if.

       161-99-FIM. EXIT.

       162-SP2-INTERFACE.

           INITIALIZE HIDE-REG
           IF   DATANAME-E NOT = SPACES
                MOVE DATANAME-E TO HIDE-DATANAME
                MOVE pop-WINDOW TO HIDE-WINDOW
                READ HIDE
                move 0 to len-cri
      ********************** OUTRA COLUNA
                IF  FS-HIDE < '10'
                AND HIDE-other > 0
                    move len-e           to len-cri
                    MOVE HIDE-COMBO-SIZE TO LEN-E
                    SET ALFABETICO       TO TRUE
                    MOVE ALL "X" TO PIC-E (1: LEN-E)
                END-IF
                IF   NOT ACCEPT-E
                     MOVE ALL "X" TO PIC-E (1: LEN-E)
                     MOVE "A"     TO MODO-ACCEPT
                END-IF
                MOVE LOW-VALUES      TO SP2-FD-DATA
                                        SP2-FD-VAR-LENS
                if campos-off = zero or low-values
                   perform varying campos-off from 1 by 1
                           until (campos-off + len-e - 1) > 32768
                              or buffer-ctrl (campos-off: len-e)
                              = spaces
                           continue
                   end-perform
                end-if
                compute SP2-FD-PROG-OFF = campos-off - 1
                if (campos-off + len-e - 1) > SIZE-FIELDS
                    compute SIZE-FIELDS = campos-off + len-e - 1
                end-if
                IF  SIZE-FIELDS > length buffer
                    EXEC COBOLware Send
                         Message "Limite de buffer atingido"
                    END-EXEC
                    STOP RUN
                else
                    move all '*' to buffer-ctrl (campos-off: len-e)
                END-IF
                MOVE X"00"           TO SP2-FD-CURS-SHOW
                COMPUTE SP2-FD-ROW    = LIN-E - 1
                COMPUTE SP2-FD-COL    = COL-E - 1
                IF   FUN-ACCEPT
                AND  ACCEPT-E
                and (not prot)
                and (CAMPOS-GUIA not = zero)
                    move CAMPOS-GUIA TO GUIA
                    MOVE GUIA        TO SP2-FD-FLD-NUM
                                        SP2-FD-TAB-NUM
                    MOVE GUIA        TO GUIA-FIELDS
                END-IF
                MOVE DATANAME-E      TO SP2-FD-NAME
                MOVE "e"             TO SP2-FD-CTRL-TYPE
                COMPUTE SP2-FD-WIDTH  = (LEN-E * 10) - 2
                IF   FONT-MODE = "FIXED"
                     ADD  2 TO SP2-FD-WIDTH
                END-IF
                IF   PIC-E (2: ) = SPACE
                AND  LEN-E > 1
                     INSPECT PIC-E (1: LEN-E) CONVERTING SPACE
                                              TO PIC-E (1: 1)
                END-IF
                MOVE 07   TO SP2-FD-HEIGHT
                MOVE ZERO TO LEN-ID
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN-E
                        MOVE PIC-E(I:1) TO TEST-EDIT
                        IF EDIT-OK
                           ADD 1 TO LEN-ID
                        END-IF
                END-PERFORM
                IF LEN-ID = 0
                   MOVE LEN-E    TO LEN-ID
                END-IF
                IF  (NUMERICO
                OR   COM-BARRA)
                     MOVE "n"   TO SP2-FD-TYPE
                                   SP2-FD-INIT-NUMS
                     MOVE X"00" TO SP2-FD-SPEC-FMT
                     MOVE LEN-E TO SP2-FD-FORMAT-LEN
                     IF   BZERO-E = "Z" OR "L" OR "R"
                          MOVE "y" TO SP2-FD-BLANK-ZERO
                     END-IF
                ELSE
                     MOVE ZERO TO SP2-FD-FORMAT-LEN
                     PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN-E
                             IF  (PIC-E (I: 1) NOT = "B")
                                  ADD 1 TO SP2-FD-FORMAT-LEN
                                  MOVE PIC-E (I: 1) TO SP2-FD-VAR-DATA
                                                (SP2-FD-FORMAT-LEN: 1)
                             END-IF
                     END-PERFORM
                END-IF
                MOVE SP2-FD-PROG-OFF TO OFF-W
                MOVE SP2-FD-TYPE     TO TIPO-E
                PERFORM 163-DE-EDIT THRU 163-99-FIM
                IF   SIZE-E = 0
                     MOVE LEN-E TO SIZE-E
                END-IF
                MOVE SIZE-E     TO SP2-FD-PROG-LEN
                                   SP2-FD-MAX-LEN
                MOVE LEN-E      TO SP2-FD-INITIAL-LEN
                MOVE bor-type   TO SP2-FD-BOR-TYPE
                IF   FUN-ACCEPT
                AND  ACCEPT-E
                     MOVE X"00"        TO SP2-FD-OUTPUT
                     IF   SECURE-E = "S"
                          MOVE "s"     TO SP2-FD-OUTPUT
                          MOVE "v"     TO SP2-FD-CURS-SHOW
                          MOVE X"00"   TO SP2-FD-CTRL-TYPE
                     END-IF
                     MOVE CURCOLOR     TO SP2-FD-CUR-COLR
                     IF   DATANAME-E (1: 7) = "CWCHECK" OR "CWRADIO"
                          MOVE STATIC TO SP2-FD-COLR
                          ADD  1      TO SP2-FD-HEIGHT
                     ELSE
                          IF   AUTO-E = "A"
                          OR   CWAUTO = 'ON'
                               MOVE "y" TO SP2-FD-CURS-SKIP
                               IF ENTER-TERMINATE NOT = 'ON'
                                  MOVE "a" TO SP2-FD-PROG-CTRL
                               END-IF
                          END-IF
                          IF   CWAUTO = 'OFF'
                               MOVE "n" TO SP2-FD-CURS-SKIP
                          END-IF
                     END-IF
                     if   prot
                          MOVE "g" TO SP2-FD-OUTPUT
                     else
                          if  CAMPOS-GUIA = 0
                          and SP2-FD-OUTPUT = X'00'
                              MOVE "y" TO SP2-FD-OUTPUT
                          end-if
                     end-if
                ELSE
                     MOVE DISABLE-ATTR TO SP2-FD-COLR
                     IF   DATANAME-E (1: 7) = "CWCHECK" OR "CWRADIO"
                          MOVE STATIC TO SP2-FD-COLR
                          MOVE "g"    TO SP2-FD-OUTPUT
                     ELSE
                          MOVE "y"    TO SP2-FD-OUTPUT
                          IF CHOOSED = 0
                             MOVE 1 TO CHOOSED
                             DISPLAY "CWCHOOSE" UPON
                                     ENVIRONMENT-NAME
                             ACCEPT  ATTRIBUTE-CHOOSE FROM
                                     ENVIRONMENT-VALUE
                             IF ATTRIBUTE-CHOOSE NOT = LOW-VALUES
                                MOVE ATT-CH TO CHOOSE
                             END-IF
                          END-IF
                          IF  ATTRIBUTE-X = CHOOSE
                          AND CHOOSE > 0
                              MOVE XUSE TO SP2-FD-COLR
                          END-IF
                     END-IF
                END-IF
                IF   DATANAME-E (1: 7) = "CWCHECK" OR "CWRADIO"
                     ADD 3      TO SP2-FD-WIDTH
                                   SP2-FD-HEIGHT
                     IF   TELA-ATTR-COL (LIN-E COL-E) NOT = X"00"
                          MOVE TELA-ATTR-COL (LIN-E COL-E)
                            TO SP2-FD-COLR
                     END-IF
                     move LIN-E TO cwuser-LINE
                     move COL-E TO cwuser-COLUMN
                     move data-e(1:1)
                       TO FD-LIN (cwuser-LINE) (cwuser-COLUMN: 1)
                     IF   FD-LIN (cwuser-LINE) (cwuser-COLUMN: 1) = "0"
                          MOVE SPACE TO
                             FD-LIN (cwuser-LINE) (cwuser-COLUMN: 1)
                     ELSE
                        MOVE "*"   TO
                             FD-LIN (cwuser-LINE) (cwuser-COLUMN: 1)
                     END-IF
                     ADD 1 TO cwuser-COLUMN
                     IF DATANAME-E(1: 7) = "CWRADIO"
                        MOVE ")"   TO
                             FD-LIN (cwuser-LINE) (cwuser-COLUMN: 1)
                        SUBTRACT 2 FROM cwuser-COLUMN
                        MOVE "("   TO
                             FD-LIN (cwuser-LINE) (cwuser-COLUMN: 1)
                     ELSE
                        MOVE "]"   TO
                             FD-LIN (cwuser-LINE) (cwuser-COLUMN: 1)
                        SUBTRACT 2 FROM cwuser-COLUMN
                        MOVE "["   TO
                             FD-LIN (cwuser-LINE) (cwuser-COLUMN: 1)
                     END-IF
                END-IF
                IF   NUMERICO
                OR   COM-BARRA
                     MOVE LOW-VALUES TO SP2-FD-VAR-DATA
                     MOVE PIC-E      TO WORK-ED
                     MOVE ZERO       TO SP2-FD-MAX-LEN
                     move space to sinais
                     perform varying x from 1 by 1 until x > len-e
                             IF (PIC-E (x: 1) = '-' OR '+')
                             and x < len-e
                                 MOVE PIC-E (x: 1) TO SINAIS
                             END-IF
                     end-perform
                     if sinais not = space
                        INSPECT WORK-ED (1: LEN-E)
                                CONVERTING ",.-+"
                                        TO ".,ZZ"
                        move sinais to SINAL-MASK(1: 1)
                                       MODO-ACCEPT
                     else
                        INSPECT WORK-ED (1: LEN-E)
                                CONVERTING "CRDB-+*,."
                                        TO "      Z.,"
                     end-if
                     PERFORM VARYING X FROM 1 BY 1 UNTIL X > LEN-E
                          IF   WORK-ED (X: 1) NOT = SPACE
                               ADD 1 TO SP2-FD-MAX-LEN
                               MOVE WORK-ED (X: 1)
                                 TO SP2-FD-VAR-DATA (SP2-FD-MAX-LEN: 1)
                          END-IF
                     END-PERFORM
                     if sinais not = space
                        move 1 to com-sinal
                        add  1 to SP2-FD-INITIAL-LEN
                                  SP2-FD-FORMAT-LEN
                        add 10 to SP2-FD-WIDTH
                        move space to sinais
                     end-if
                     IF   COM-SINAL = 1 OR 2
                          ADD 1 TO SP2-FD-MAX-LEN
                          IF   SINAL-MASK(1: 1) = "+" OR "C"
                               MOVE "+"
                                 TO SP2-FD-VAR-DATA (SP2-FD-MAX-LEN: 1)
                          ELSE
                               MOVE "-"
                                 TO SP2-FD-VAR-DATA (SP2-FD-MAX-LEN: 1)
                          END-IF
                          If sinais not = spaces
                             move "+"
                              TO SP2-FD-VAR-DATA (SP2-FD-MAX-LEN: 1)
                          end-if
                     END-IF
                END-IF
                IF CAMPOS-FD-ID = ZERO
                   CALL "CWSPID" USING SP2-FD-ID "FInsert" *> Obter FD-ID sem CWSCRE
                   MOVE SP2-FD-ID    tO CAMPOS-FD-ID
                ELSE
                   MOVE CAMPOS-FD-ID TO SP2-FD-ID
                END-IF
                move col-e to col-w
                perform len-e times
                        MOVE SP2-FD-ID    TO ENTRY-ID  (LIN-E col-w)
                        MOVE CAMPOS-CAMPO TO TELA-CAMPO(LIN-E col-w)
                        add  1            to col-w
                end-perform
                IF  FUN-ACCEPT
                AND ACCEPT-E
                and (not prot)
                    MOVE SP2-FD-ID     TO FIELD-ID (GUIA)
                                          SAVE-ID  (GUIA)
                    MOVE campos-FIELD  TO ID-FIELD (GUIA)
                    MOVE COL-E         TO GUIA-COL (GUIA)
                END-IF
                IF   HIDE-E = 1
                     IF   DATANAME-E NOT = HIDE-DATANAME
                          INITIALIZE HIDE-REG
                          MOVE DATANAME-E TO HIDE-DATANAME
                          MOVE pop-WINDOW TO HIDE-WINDOW
                          READ HIDE
                     END-IF
                     IF  FUN-ACCEPT
                     AND ACCEPT-E
                     AND HIDE-POS = 0
                         IF   PROT
                              MOVE 1 TO HIDE-PROT
                         END-IF
                         if   fs-hide < '10'
                              if  HIDE-NOEDIT = 1
                                  MOVE "p"     TO SP2-FD-OUTPUT
                                  MOVE OK2     TO SP2-FD-CUR-COLR
                                  move "o"     TO SP2-FD-PROG-CTRL
                              end-if
                              CALL "CWSPID" USING HIDE-COMBO "FInsert"
                              ADD  1           TO COMBOS-FIELDS
                              MOVE SP2-FD-ID   TO COMBO-FD-ID
                                                  (COMBOS-FIELDS)
                              move accepts     to guia
                              MOVE GUIA        TO HIDE-GUIA
                              MOVE HIDE-COMBO  TO COMBO-ID (GUIA)
                        end-if
                     END-IF
                     IF  LEN-E > HIDE-SIZE
                         MOVE LEN-E TO HIDE-SIZE
                     END-IF
                     COMPUTE HIDE-POS = SP2-FD-PROG-OFF + 1
                     IF   fs-hide < '10'
                          MOVE HIDE-buffer TO buffer (HIDE-POS: LEN-E)
                                              CAMPOS-SHOW
                          REWRITE HIDE-REG
                     ELSE
                          MOVE CAMPOS-SHOW TO buffer (HIDE-POS: LEN-E)
                     END-IF
                     IF  NUMERICO
                         INSPECT BUFFER (HIDE-POS: LEN-E)
                            CONVERTING SPACE TO ZERO
                     END-IF
                END-IF
                MOVE SP2-FD-INITIAL-LEN TO SP2-FD-VAR-LEN
                IF   DATANAME-E (1: 7) = "CWCHECK" OR "CWRADIO"
                     IF   DATANAME-E (1: 7) = "CWCHECK"
                          MOVE "c"           TO SP2-FD-CTRL-TYPE
                     ELSE
                          MOVE "r"           TO SP2-FD-CTRL-TYPE
                          ADD  1             TO RADIOS
                          MOVE SP2-FD-ID     TO RADIO-ID  (RADIOS)
                          MOVE POS-E         TO RADIO-POS (RADIOS)
                     END-IF
                     move X'10'              to sp2-FD-MISC-OPTIONS
                     MOVE ZERO               TO SP2-FD-INITIAL-LEN
                                                SP2-FD-FORMAT-LEN
                     MOVE LOW-VALUES         TO SP2-FD-VAR-DATA
                     MOVE ZERO               TO SP2-FD-CAPTION-LEN
                     MOVE SP2-FD-CAPTION-LEN TO SP2-FD-VAR-LEN
                     ADD  1                  TO SP2-FD-VAR-LEN
                     MOVE SIM                TO SP2-FD-VAR-DATA
                                               (SP2-FD-VAR-LEN: 1)
                     ADD  1                  TO SP2-FD-VAR-LEN
                     MOVE NAO                TO SP2-FD-VAR-DATA
                                               (SP2-FD-VAR-LEN: 1)
                     MOVE 2                  TO SP2-FD-DISCRETE-LEN
                END-IF
                MOVE SP2-FD-ID          TO DATANAMES-ID
                INITIALIZE DATANAMES-DATA
                READ DATANAMES
                IF   FUN-ACCEPT
                AND  ACCEPT-E
                     MOVE GUIA          TO DATANAMES-GUIA
                ELSE
                     MOVE ZERO          TO DATANAMES-GUIA
                END-IF
                MOVE SP2-FD-NAME        TO DATANAMES-FIELD
                move campos-subscript   to datanames-subscript
                move campos-off         TO DATANAMES-OFF
                MOVE LEN-E              TO DATANAMES-LEN
                MOVE SP2-FD-INITIAL-LEN TO DATANAMES-INITIAL-LEN
                MOVE SP2-FD-VAR-LEN     TO DATANAMES-VAR-LEN
                MOVE SP2-FD-VAR-DATA    TO DATANAMES-VAR-DATA
                MOVE SP2-FD-FORMAT-LEN  TO DATANAMES-FORMAT-LEN
                MOVE LIN-E              TO DATANAMES-LIN
                MOVE COL-E              TO DATANAMES-COL
                IF   HIDE-E NOT NUMERIC
                     MOVE ZERO          TO HIDE-E
                END-IF
                MOVE HIDE-E             TO DATANAMES-HIDE
                MOVE COMANDO            TO DATANAMES-COMANDO
                MOVE ZERO               TO DATANAMES-LEN-M
                IF   DATANAMES-HIDE = 0
                     MOVE 0                  TO DATANAMES-LEN-M
                     PERFORM VARYING LX FROM 1 BY 1 UNTIL LX > LEN-E
                             IF PIC-E (LX: 1) = '9' OR 'X'
                             OR 'A' OR 'Z' OR '*'
                                ADD 1 TO DATANAMES-LEN-M
                             END-IF
                     END-PERFORM
                     IF   DATANAMES-LEN-M = 0
                          MOVE LEN-E TO DATANAMES-LEN-M
                     END-IF
                ELSE
                     move len-cri   TO DATANAMES-LEN-M
                END-IF
                IF   DATANAMES-LEN-M = 0
                     MOVE LEN-E TO DATANAMES-LEN-M
                END-IF
                IF   NUMERICO
                     MOVE 1 TO DATANAMES-NUMERIC
                ELSE
                     MOVE ZERO TO DATANAMES-NUMERIC
                END-IF
                *> Adaptar controle de LK contra -E
                move CAMPOS-field to DATANAMES-screen-field
                IF  FS-DATANAMES < "10"
                    REWRITE DATANAMES-REG
                ELSE
                    MOVE ZERO TO DATANAMES-CRITICA-POSIT
                    WRITE DATANAMES-REG
                END-IF
                IF   campos-font-id NOT = ZERO
                     MOVE campos-font-id TO SP2-FD-FONT-ID
                ELSE
                     IF   FONT-MODE = "FIXED"
                          MOVE 1 TO SP2-FD-FONT-ID
                     ELSE
                          MOVE FONT-WIN TO SP2-FD-FONT-ID
                          IF  CWUSER-FIELD = "1"
                              MOVE 2 TO SP2-FD-FONT-ID
                          END-IF
                     END-IF
                END-IF
                IF   NUMERICO
                AND  COM-BARRA
                     MOVE "y"  TO SP2-FD-SPEC-FMT
                END-IF
                IF   NOT ACCEPT-E
                     ADD 2         TO SP2-FD-WIDTH
                     MOVE 10       TO SP2-FD-HEIGHT
                     MOVE X"00"    TO SP2-FD-BOR-TYPE
                     MOVE bor-type TO SP2-FD-BOR-TYPE
                     IF   SP2-FD-WIDTH < 22
                     and (SP2-FD-BOR-TYPE not = 't')
                          MOVE 't' TO SP2-FD-BOR-TYPE
                     END-IF
                     MOVE X"00" TO SP2-FD-CTRL-TYPE
                     MOVE TELA-ATTR-COL(LIN-E COL-E) TO SP2-FD-COLR
                END-IF
                IF   CWCASE-INPUT = "UPP"
                OR   UPPLOW-E = "U"
                     MOVE "u" TO SP2-FD-CASE
                ELSE
                     IF   CWCASE-INPUT = "UPP"
                     OR   UPPLOW-E = "L"
                          MOVE "l" TO SP2-FD-CASE
                     END-IF
                END-IF
                IF  BZERO-E = "R" OR "r"
                    MOVE "r" TO SP2-FD-JUSTIFY
                END-IF
                IF  BZERO-E = "L" OR "l"
                    MOVE "l" TO SP2-FD-JUSTIFY
                END-IF
                IF SP2-FD-BOR-TYPE = "l" OR 'f'
                   COMPUTE SP2-FD-HEIGHT = SP2-FD-HEIGHT
                                        + (SP2-FD-HEIGHT / 2)
                END-IF
                IF  SP2-FD-TYPE = "n"
                    MOVE "r" TO SP2-FD-JUSTIFY
                END-IF
      ********************** OUTRA COLUNA
                IF   FS-HIDE < "10"
                AND  hide-alpha > 0
                     MOVE DATA-E TO HIDE-STRING-DATA
                                    HIDE-STRING-SHOW
                     if hide-alpha = 2
                        MOVE string-2     TO HIDE-STRING-SHOW
                     end-if
                     REWRITE HIDE-REG
                END-IF
                IF CTRL-E(1:8) = 'STANDARD'
                   MOVE ATTRIBUTE-X TO SP2-FD-COLR
                END-IF
                IF PROT
                   MOVE X"08"  TO SP2-FD-COLR
                END-IF
                IF SP2-FD-FONT-ID > 4
                   MOVE LOW-VALUES     TO SP2-FO-DATA
                   MOVE SP2-FD-FONT-ID TO SP2-FO-ID
                   CALL SP2 USING SP2-GET-FONT-DEF SP2-FONT-DEF
                   IF SP2-FO-WIDTH NOT = 0
                      COMPUTE SP2-FD-WIDTH = SP2-FO-WIDTH
                                           * (SP2-FD-FORMAT-LEN * 1,5)
                   END-IF
                   IF SP2-FO-HEIGHT NOT = ZERO
                      COMPUTE SP2-FD-HEIGHT = SP2-FO-HEIGHT / 2
                   END-IF
                END-IF
                IF  (GUICOLOR NOT = 'OFF')
                AND (GUICOLOR-ACCEPT NOT = 'OFF')
                    COMPUTE ATTRIBUTE = BACK-NW + (FORE-NW * 16) + 8
                    IF SP2-FD-COLR NOT = DISABLE-ATTR
                       MOVE ATTRIBUTE-X TO SP2-FD-COLR
                    END-IF
                END-IF
                IF SP2-FD-BOR-TYPE = X'00'
                   ADD 2 TO SP2-FD-HEIGHT
                END-IF
                MOVE SP2-FD-COLR TO DATANAMES-COLR
                PERFORM 040-MAP-FIELD THRU 040-99-FIM
                REWRITE DATANAMES-REG
                MOVE campos-FD-ID TO DATANAMES-ID
                read datanames
                INSPECT TELA-ATTR-LIN(LIN-E)(COL-E:LEN-E)
                        converting low-value to SP2-FD-COLR
           END-IF.

       162-99-FIM. EXIT.

       163-DE-EDIT.

           IF   FS-HIDE < "10"
           AND (HIDE-E  = 1
            or campos-guia = zero)
                ADD  1        TO OFF-W
                MOVE LEN-E    TO SIZE-E
                IF   CWACCENT = "OFF"
                     INSPECT HIDE-buffer  (1: LEN-E)
                     CONVERTING ACENTOS-850
                             TO ACENTOS-OFF
                ELSE
                     IF   MFF = "MF"
                          INSPECT HIDE-buffer  (1: LEN-E)
                          CONVERTING ACENTOS-850
                                  TO ACENTOS-437
                     ELSE
                          INSPECT HIDE-buffer  (1: LEN-E)
                          CONVERTING ACENTOS-850
                                  TO ACENTOS-WINDOWS
                     END-IF
                END-IF
                MOVE HIDE-buffer  TO buffer (OFF-W: LEN-E)
                IF  NUMERICO
                    INSPECT BUFFER (OFF-W: LEN-E)
                       CONVERTING SPACE TO ZERO
                END-IF
                EXIT PARAGRAPH
           END-IF

           IF   DATANAME-E (1: 7) = "CWCHECK" OR "CWRADIO"
                MOVE 1 TO SIZE-E
                compute off-w = sp2-fd-prog-off + 1
                IF   DATA-E (1: 1) = "1"
                     MOVE SIM TO buffer (OFF-W: 1)
                ELSE
                     MOVE NAO TO buffer (OFF-W: 1)
                END-IF
                MOVE SPACES TO CWPRTS
                IF   DATANAME-E (1: 7) = "CWCHECK"
                     STRING LIN-E COL-E '['
                            buffer (OFF-W: 1) ']'
                            DELIMITED BY SIZE
                       INTO CWPRTS
                ELSE
                     STRING LIN-E COL-E '('
                            buffer (OFF-W: 1) ')'
                            DELIMITED BY SIZE
                       INTO CWPRTS
                END-IF
                CALL "CWPRTS" USING CWPRTS
                EXIT PARAGRAPH
           END-IF

           IF  (NOT ACCEPT-E)
           AND (SECURE-E NOT = "S")
               COMPUTE OFF-S = OFF-W + 1
               MOVE LEN-E  TO SIZE-E
               MOVE DATA-E (1: LEN-E)
                 TO buffer (OFF-S: LEN-E)
               IF  (NOT NUMERICO)
                    IF   CWACCENT = "OFF"
                         INSPECT buffer (OFF-S: LEN-E)
                                      CONVERTING ACENTOS-850
                                              TO ACENTOS-OFF
                    ELSE
                         IF   MFF = "MF"
                              INSPECT buffer(OFF-S: LEN-E)
                                      CONVERTING ACENTOS-850
                                              TO ACENTOS-437
                         ELSE
                              INSPECT buffer(OFF-S: LEN-E)
                                      CONVERTING ACENTOS-850
                                              TO ACENTOS-WINDOWS
                        END-IF
                    END-IF
                    EXIT PARAGRAPH
               END-IF
           END-IF

           IF   NUMERICO
           OR   COM-BARRA
                MOVE 18    TO Z
                MOVE ZERO  TO NUMEROS
                MOVE ZERO  TO SIZE-E
                ADD  1     TO OFF-W
                MOVE LEN-E TO X
                IF   PIC-E (X: 1) = "-" OR "+"
                     SUBTRACT 1 FROM X
                     IF   PIC-E (X: 1) = "-" OR "+"
                          ADD 1 TO X
                     END-IF
                END-IF
                PERFORM VARYING X FROM X BY -1 UNTIL X = 0
                                                  OR Z = 0
                        MOVE DATA-E (X: 1) TO TEST-NUM
                        IF   ALGARISMO
                             MOVE TEST-NUM TO NUMEROS (Z: 1)
                             SUBTRACT 1 FROM Z
                             ADD 1 TO SIZE-E
                        ELSE
                             MOVE PIC-E (X: 1) TO TEST-NUM
                             IF   MASK
                                  MOVE "0" TO NUMEROS (Z: 1)
                                  SUBTRACT 1 FROM Z
                                  ADD 1 TO SIZE-E
                             END-IF
                        END-IF
                END-PERFORM
                IF   SINAL-ATUAL = "-" OR "DB"
                     COMPUTE NUMEROS = NUMEROS * -1
                END-IF
                ADD  1                       TO Z
                IF   COM-SINAL = 0
                     MOVE NUMEROS            TO UNSIGNED
                     MOVE UNSIGNED(Z:SIZE-E) TO buffer
                                                (OFF-W:SIZE-E)
                ELSE
                     MOVE NUMEROS(Z:SIZE-E)  TO buffer
                                                (OFF-W:SIZE-E)
                END-IF
                IF   COM-SINAL = 1 OR 2
                     ADD 1 TO SIZE-E
                END-IF
           ELSE
                COMPUTE OFF-S = OFF-W + 1
                MOVE ZERO TO SIZE-E
                PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > LEN-E
                        IF   PIC-E (Y: 1) = "X" OR "A"
                             ADD  1 TO OFF-W
                                       SIZE-E
                             MOVE DATA-E (Y: 1)
                               TO buffer (OFF-W: 1)
                        END-IF
                END-PERFORM
                IF   CWACCENT = "OFF"
                     INSPECT buffer (OFF-S: LEN-E)
                                  CONVERTING ACENTOS-850
                                          TO ACENTOS-OFF
                ELSE
                    IF   MFF = "MF"
                         INSPECT buffer (OFF-S: LEN-E)
                                 CONVERTING ACENTOS-850
                                         TO ACENTOS-437
                    ELSE
                         INSPECT buffer (OFF-S: LEN-E)
                                 CONVERTING ACENTOS-850
                                         TO ACENTOS-WINDOWS
                    END-IF
                END-IF
                IF   SECURE-E = "S"
                AND  FUN-DISPLAY
                     MOVE LEN-E TO III
                     COMPUTE II = OFF-S + LEN-E - 1
                     PERFORM VARYING II
                                FROM II
                                  BY -1 UNTIL III = 0
                                  OR((buffer(II: 1)
                                  NOT = SPACE)
                                 AND (buffer (II: 1)
                                     NOT = X"00"))
                              SUBTRACT 1 FROM III
                     END-PERFORM
                     MOVE SPACES TO buffer (OFF-S: LEN-E)
                     IF   III NOT = ZERO
                          MOVE ALL "*" TO buffer
                                          (OFF-S: III)
                     END-IF
                END-IF
           END-IF.

       163-99-FIM. EXIT.

       164-SET-FONT.

           MOVE 0 TO SET-FONT
           MOVE 5 TO II
           PERFORM VARYING I FROM LENGTH OF DATA-LK(FIELD) BY -1
                   UNTIL I = 0
                      OR II = 1
                   IF DATA-LK(FIELD)(I:1) NUMERIC
                      SUBTRACT 1 FROM II
                      MOVE DATA-LK(FIELD)(I:1) TO SET-FONT(II:1)
                   END-IF
           END-PERFORM
           COMPUTE FIELD-FONT = FIELD + 1
           PERFORM VARYING FIELD-FONT
                      FROM FIELD-FONT
                        BY 1
                     UNTIL FIELD-FONT > FIELDS
                        OR DATANAME-LK (FIELD-FONT) (1: 6) = "CWFONT"
                   IF  DATANAME-LK (FIELD-FONT) = SPACES
                       MOVE LIN-LK(FIELD-FONT) TO W-L
                       MOVE COL-LK(FIELD-FONT) TO W-C
                       IF  W-L > 0
                       AND W-C > 0
                           MOVE SET-FONT TO FONT-ID (W-L W-C)
                       END-IF
                   END-IF
           END-PERFORM.

       164-99-FIM. EXIT.

       017-EDIT.

           IF  NOT ACCEPT-E
               GO TO 017-99-FIM
           END-IF

           IF   DATANAME-E (1: 7) = "CWCHECK" OR "CWRADIO"
                IF   buffer (OFF-W: 1) = SIM
                     MOVE "1" TO data-e (1: 1)
                ELSE
                     MOVE "0" TO data-e (1: 1)
                END-IF
                GO TO 017-99-FIM
           END-IF
           MOVE DATANAME-E TO HIDE-DATANAME
           MOVE pop-WINDOW TO HIDE-WINDOW
           READ HIDE
           IF   FS-HIDE < "10"
                IF   HIDE-LIST = 1
                AND (HIDE-STRING NOT = 0)
                     MOVE HIDE-STRING-DATA TO data-e (1: LEN-E)
                     MOVE HIDE-STRING-SHOW TO buffer (OFF-W: SIZE-E)
                END-IF
                IF  HIDE-NOEDIT = 0
                AND (HIDE-COMBO NOT = 0)
                AND HIDE-STRING = 0
                   CONTINUE
                ELSE
                   GO TO 017-99-FIM
                END-IF
           END-IF
           MOVE buffer (OFF-W: SIZE-E) TO WORK-ED
           MOVE SPACES                 TO data-e (1: LEN-E)
           IF   TIPO-E = "n" OR "d"
                IF   COM-SINAL = 1 OR 2
                     SUBTRACT 1 FROM SIZE-E
                END-IF
                MOVE ZERO   TO NUMEROS
                MOVE SPACES TO SINAL-ATUAL
                COMPUTE Z = 19 - SIZE-E
                INSPECT WORK-ED (1: SIZE-E) CONVERTING SPACE TO ZERO
                MOVE WORK-ED (1: SIZE-E) TO NUMEROS (Z: SIZE-E)
                IF   COM-SINAL = 1 OR 2
                     ADD 1 TO SIZE-E
                     IF   WORK-ED (SIZE-E: 1) = "-"
                          COMPUTE NUMEROS = NUMEROS * -1
                     END-IF
                     SUBTRACT 1 FROM SIZE-E
                END-IF
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN-E
                        MOVE PIC-E (I: 1) TO TEST-NUM
                        IF   SINAL-POS
                             MOVE PIC-E (I: 2) TO SINAL-MASK
                             MOVE I            TO SINAL-POSIT
                             IF   NUMEROS NEGATIVE
                                  EVALUATE TEST-NUM
                                      WHEN "+" MOVE "-"  TO SINAL-ATUAL
                                      WHEN "-" MOVE "-"  TO SINAL-ATUAL
                                      WHEN "C" MOVE "DB" TO SINAL-ATUAL
                                      WHEN "D" MOVE "DB" TO SINAL-ATUAL
                                  END-EVALUATE
                             ELSE
                                  EVALUATE TEST-NUM
                                      WHEN "+" MOVE "+"   TO SINAL-ATUAL
                                      WHEN "-" MOVE SPACE TO SINAL-ATUAL
                                      WHEN "C" MOVE "CR"  TO SINAL-ATUAL
                                      WHEN "D" MOVE SPACE TO SINAL-ATUAL
                                  END-EVALUATE
                             END-IF
                             COMPUTE I = LEN-E + 1
                        END-IF
                END-PERFORM
                MOVE LEN-E   TO Z
                MOVE NUMEROS TO UNSIGNED
                IF   COM-SINAL = 1 OR 2
                     IF   SINAL-MASK = "CR" OR "DB"
                          SUBTRACT 1 FROM Z
                          IF   SINAL-ATUAL = "CR"
                               IF  SINAL-MASK = "DB"
                                   MOVE "CR" TO data-e (Z: 2)
                               ELSE
                                   MOVE "  " TO data-e (Z: 2)
                               END-IF
                          END-IF
                          IF   SINAL-ATUAL = "DB"
                               MOVE "DB" TO data-e (Z: 2)
                          END-IF
                          SUBTRACT 1 FROM Z
                     ELSE
                          IF  SINAL-POSIT = LEN-E
                              MOVE SINAL-ATUAL TO data-e (Z: 1)
                              IF SINAL-ATUAL(1:1) = '+'
                              AND PIC-E (Z: 1) = '-'
                                  MOVE SPACE TO data-e (Z: 1)
                              END-IF
                              IF SINAL-ATUAL(1:2) = 'CR'
                              AND PIC-E (Z: 2) = 'DB'
                                  MOVE SPACE TO data-e (Z: 2)
                              END-IF
                              SUBTRACT 1 FROM Z
                          END-IF
                     END-IF
                END-IF
                IF  (BZERO-E = "Z" OR "L" OR "R")
                AND  NUMEROS = 0
                     MOVE SPACES TO data-e (1: LEN-E)
                ELSE
                     MOVE 18 TO X
                     PERFORM UNTIL X = 0
                                OR Z = 0
                             MOVE PIC-E (Z: 1) TO TEST-NUM
                             IF  MASK
                                 MOVE N (X)    TO data-e (Z: 1)
                                 SUBTRACT 1 FROM X
                             ELSE
                                 MOVE TEST-NUM TO data-e (Z: 1)
                             END-IF
                             SUBTRACT 1 FROM Z
                     END-PERFORM
                     MOVE ZERO TO Z
                     PERFORM VARYING X FROM 1 BY 1 UNTIL X > LEN-E
                                                      OR Z NOT = ZERO
                             MOVE data-e (X: 1) TO TEST-NUM
                             IF   VALOR
                                  MOVE X TO Z
                             ELSE
                                  IF   PIC-E (X: 1) = "9"
                                       MOVE X TO Z
                                  END-IF
                             END-IF
                     END-PERFORM
                     IF   Z NOT = ZERO
                          SUBTRACT 1 FROM Z
                          IF  Z NOT = ZERO
                              MOVE ZERO TO Z3
                              PERFORM VARYING Z2 FROM LEN-E BY -1
                                      UNTIL Z2 = 0
                                      OR data-e (Z2: 1) = ','
                                         ADD 1 TO Z3
                              END-PERFORM
                              MOVE SPACES TO data-e (1: Z)
                              IF Z2 NOT = ZERO
                                 MOVE DP TO data-e (Z2: 1)
                                 INSPECT data-e (Z2: Z3)
                                   CONVERTING SPACE TO ZERO
                              END-IF
                              IF   SINAL-POSIT < LEN-E
                              AND (SINAL-ATUAL = "-" OR "+")
                                   MOVE SINAL-ATUAL
                                     TO data-e (Z: 1)
                              END-IF
                              IF DP = '.'
                                 INSPECT data-e (1: LEN-E)
                                         CONVERTING ',.' TO '.,'
                              END-IF
                          END-IF
                     END-IF
                END-IF
                IF  COM-SINAL = 1 OR 2
                    ADD 1 TO SIZE-E
                END-IF
                INSPECT data-e (X: SIZE-E)
                        CONVERTING "B" TO SPACE
           ELSE
                MOVE 1 TO Y
                PERFORM VARYING X FROM 1 BY 1 UNTIL X > SIZE-E
                        PERFORM UNTIL Y > LEN-E
                                   OR PIC-E (Y: 1) = "A" OR "X"
                                ADD 1 TO Y
                        END-PERFORM
                        MOVE WORK-ED (X: 1) TO data-e (Y: 1)
                        ADD  1              TO Y
                END-PERFORM
           END-IF.

       017-99-FIM. EXIT.

       032-CRITICA.

           IF   SP2-CD-KEY = SP2-KEY-F1
           OR                SP2-KEY-ALT-H
           OR                SP2-KEY-UP
           OR                SP2-KEY-BACKTAB
           OR   SKIP-CRITICA = 1
           OR  (SP2-CD-KEY = 0
           AND   SP2-CD-LAST-FLD-ID = SP2-CD-NEXT-FLD-ID)
                 GO TO 032-99-FIM
           END-IF

           MOVE ZERO TO ERRO
           IF   SP2-CD-KEY NOT = SP2-KEY-ESC
                MOVE CRITICAR-ID        TO DATANAMES-ID
                READ DATANAMES
                IF   FS-DATANAMES = "00"
                     IF  (SP2-CD-KEY = SP2-KEY-TAB
                     OR                SP2-KEY-BACKTAB)
                         AND DATANAMES-HIDE = 1
                         AND SKIP-HIDE      = 1
                             EXIT PARAGRAPH
                     END-IF
                     MOVE DATANAMES-FIELD     TO CRITICA-FIELD
                     READ CRITICA
                     IF   FS-CRITICA = "23"
                          MOVE "ANY" TO CRITICA-FIELD
                          READ CRITICA
                          MOVE DATANAMES-FIELD TO CRITICA-FIELD
                     END-IF
                     IF   FS-CRITICA = "00"
                          PERFORM 035-CRITICA-FIELD THRU 035-99-FIM
                     END-IF
                END-IF
           END-IF.

       032-99-FIM. EXIT.

       034-CRITICA-GERAL.

           IF   ACCEPT-LK (FIELD-CRITICA)
                MOVE DATANAME-LK (FIELD-CRITICA) TO CRITICA-FIELD
                READ CRITICA
                IF   FS-CRITICA = "23"
                     MOVE "ANY" TO CRITICA-FIELD
                     READ CRITICA
                END-IF
                IF   FS-CRITICA < "10"
                     MOVE DATANAME-LK (FIELD-CRITICA) TO CRITICA-FIELD
                     PERFORM 035-CRITICA-FIELD THRU 035-99-FIM
                END-IF

                IF   ERRO = 1
                     MOVE ZERO TO KEY-EXIT
                END-IF
           END-IF.

       034-99-FIM. EXIT.

       035-CRITICA-FIELD.

           MOVE SPACES  TO PARAMETROS-CRITICA
           INITIALIZE      USING-MC
           MOVE 1       TO LEN-CRITICA
           PERFORM VARYING I FROM 1 BY 1
                     UNTIL I > CRITICA-USINGS
                   MOVE CRITICA-USING (I)     TO DATANAMES-FIELD
                                                 FROMS-DATANAME
                   MOVE CRITICA-SUBSCRIPT (I) TO DATANAMES-SUBSCRIPT
                                                 FROMS-SUBSCRIPT
                   READ DATANAMES KEY DATANAMES-FIELD
                   IF   FS-DATANAMES < "10"
                        MOVE DATANAMES-LEN-M TO DATANAMES-LEN
                        IF  DATANAMES-NUMERIC = 1
                        AND buffer (DATANAMES-OFF: DATANAMES-LEN)
                            = SPACES
                            MOVE ALL "0"
                            TO buffer (DATANAMES-OFF: DATANAMES-LEN)
                        END-IF
                        MOVE buffer
                             (DATANAMES-OFF: DATANAMES-LEN)
                          TO PARAMETROS-CRITICA
                              (LEN-CRITICA: DATANAMES-LEN)
                        ADD  1             TO MC-MAX
                        IF DATANAMES-NAME(1:7) = 'CWRADIO' OR 'CWCHECK'
                           IF PARAMETROS-CRITICA (LEN-CRITICA: 1) = '+'
                              MOVE '1' TO
                              PARAMETROS-CRITICA (LEN-CRITICA: 1)
                           ELSE
                              MOVE '0' TO
                              PARAMETROS-CRITICA (LEN-CRITICA: 1)
                           END-IF
                           MOVE '*' TO MC-FLAG (MC-MAX)
                        END-IF
                        MOVE ZERO          TO MC-FROM (MC-MAX)
                        MOVE DATANAMES-OFF TO MC-OFF  (MC-MAX)
                        MOVE DATANAMES-LEN TO MC-LEN  (MC-MAX)
                        MOVE LEN-CRITICA   TO MC-LENC (MC-MAX)
                        IF DATANAMES-CRITICA-POSIT = 0
                           MOVE LEN-CRITICA TO DATANAMES-CRITICA-POSIT
                           REWRITE DATANAMES-REG
                        END-IF
                   ELSE
                        READ FROMS
                        IF   FS-FROMS < "10"
                             MOVE DATA-LK (FROMS-FIELD)
                               TO PARAMETROS-CRITICA (LEN-CRITICA:)
                             MOVE LEN-LK (FROMS-FIELD)
                               TO DATANAMES-LEN
                             ADD 1 TO MC-MAX
                             MOVE FROMS-FIELD   TO MC-FROM (MC-MAX)
                             MOVE DATANAMES-LEN TO MC-LEN  (MC-MAX)
                             MOVE LEN-CRITICA   TO MC-LENC (MC-MAX)
                        ELSE
                             MOVE CRITICA-USING (I)
                               TO PARAMETROS-CRITICA (LEN-CRITICA:)
                             PERFORM VARYING DATANAMES-LEN
                                        FROM LENGTH OF CRITICA-USING (I)
                                          BY -1
                                          UNTIL DATANAMES-LEN = 0
                                 OR CRITICA-USING (I) (DATANAMES-LEN: 1)
                                        NOT = SPACE
                                     CONTINUE
                             END-PERFORM
                        END-IF
                   END-IF
                   ADD DATANAMES-LEN TO LEN-CRITICA
           END-PERFORM
           MOVE ZERO TO SUBSCRIPT
           PERFORM VARYING Y FROM 1 BY 1
                     UNTIL Y > FIELD-CRITICA
                 IF  CRITICA-FIELD = DATANAME-LK (Y)
                     ADD 1 TO SUBSCRIPT
                 END-IF
           END-PERFORM
           DISPLAY 'CWUSER-MSG' UPON ENVIRONMENT-NAME
           DISPLAY 'ON'         UPON ENVIRONMENT-VALUE
           CALL CRITICA-PROGRAM USING ERRO
                                PARAMETROS-CRITICA CRITICA-FIELD
                                SUBSCRIPT
           DISPLAY 'CWUSER-MSG' UPON ENVIRONMENT-NAME
           DISPLAY 'OFF'        UPON ENVIRONMENT-VALUE
           IF ERRO > 1
              MOVE 1 TO ERRO
           END-IF
           PERFORM VARYING MC FROM 1 BY 1
                     UNTIL MC > MC-MAX
                   MOVE MC-OFF  (MC) TO MC-OFF-l
                   MOVE MC-LEN  (MC) TO MC-LEN-L
                   MOVE MC-LENC (MC) TO MC-LENC-L
                   IF  MC-FLAG (MC) = '*'
                       IF PARAMETROS-CRITICA (MC-LENC-L: 1) = '1'
                          MOVE '+' TO PARAMETROS-CRITICA(MC-LENC-L:1)
                       ELSE
                          MOVE '-' TO PARAMETROS-CRITICA(MC-LENC-L:1)
                       END-IF
                   END-IF
                   IF MC-FROM (MC)  = 0
                      MOVE PARAMETROS-CRITICA (MC-LENC-L: MC-LEN-L)
                        TO buffer (MC-OFF-l: MC-LEN-L)
                   ELSE
                      MOVE MC-FROM (MC) TO FROMS-FIELD
                      IF PARAMETROS-CRITICA (MC-LENC-L: MC-LEN-L)
                          NOT = DATA-LK (FROMS-FIELD)
                         MOVE LIN-LK (FROMS-FIELD) TO L
                         MOVE COL-LK (FROMS-FIELD) TO C
                         MOVE PARAMETROS-CRITICA (MC-LENC-L: MC-LEN-L)
                           TO DATA-LK (FROMS-FIELD)
                      END-IF
                   END-IF
           END-PERFORM
           IF   ERRO = 1
                MOVE 1000 TO SP2-CD-KEY
                IF   CRITICA-CURSOR NOT = SPACES
                     MOVE CRITICA-CURSOR TO DATANAMES-FIELD
                     READ DATANAMES KEY DATANAMES-FIELD
                     IF   FS-DATANAMES < "10"
                          MOVE DATANAMES-GUIA TO CURSOR-GUIA
                     END-IF
                END-IF
           ELSE
                MOVE CRITICA-FIELD TO DATANAMES-FIELD
                READ DATANAMES KEY DATANAMES-FIELD
                IF   FS-DATANAMES < "10"
                AND (DATANAMES-CRITICA-POSIT NOT = 0)
                     MOVE PARAMETROS-CRITICA
                           (DATANAMES-CRITICA-POSIT: DATANAMES-LEN)
                       TO buffer
                          (DATANAMES-OFF: DATANAMES-LEN)
                END-IF
           END-IF.

       035-99-FIM. EXIT.

       170-ACCEPT-TELA.

           MOVE 0 TO CWBOXF
           EXEC COBOLware PROCESS (CLOSE) END-EXEC
           IF   CWGETL-TIMEOUT NOT = 0
                IF   CWGETL-TIMEOUT > 255
                     MOVE 255 TO SEGUNDOS
                ELSE
                     MOVE CWGETL-TIMEOUT TO SEGUNDOS
                END-IF
           ELSE
                MOVE 0              TO SEGUNDOS
           END-IF
           IF  X91-PARAMETER = 5
               MOVE WS-TIMEOUT TO SEGUNDOS
           END-IF
           MOVE SEGUNDOS (1: 1) TO SP2-CD-TIMEOUT
           IF   SEGUNDOS = 0
                IF  X91-PARAMETER = 5
                    MOVE "k" TO SP2-CD-WAIT-SW
                ELSE
                    MOVE X"00" TO SP2-CD-WAIT-SW
                END-IF
           ELSE
                MOVE "a"   TO SP2-CD-WAIT-SW
           END-IF
           MOVE    SP2-CD-WAIT-SW TO WAIT-SW
           DISPLAY "NEWOBJECTS" UPON ENVIRONMENT-NAME
           MOVE    SPACES         TO NEWOBJECTS
           ACCEPT  NEWOBJECTS   FROM ENVIRONMENT-VALUE
           IF NEWOBJECTS NOT = SPACE
           OR FROM-DISPLAY = 1
              MOVE 0 TO FROM-DISPLAY
              PERFORM 300-CLEAR-OBJECTS  THRU 300-99-FIM
              PERFORM 270-DEFINE-OBJECTS THRU 270-99-FIM
              MOVE    SPACE                TO NEWOBJECTS
              DISPLAY NEWOBJECTS         UPON ENVIRONMENT-VALUE
           END-IF
           INITIALIZE PARAMETROS-CWOBJE
           MOVE 0         TO SP2-CD-KEY
           MOVE 0         TO SP2-CD-KEY KEY-EXIT
           MOVE 1         TO CURSOR-GUIA
           IF FOCUS NOT = 0
              PERFORM VARYING CURSOR-GUIA
                        FROM 1 BY 1
                        UNTIL ID-FIELD (CURSOR-GUIA) = FOCUS
                           OR ID-FIELD (CURSOR-GUIA) = 0
                     CONTINUE
               END-PERFORM
               IF ID-FIELD (CURSOR-GUIA) = 0
                  MOVE 1  TO CURSOR-GUIA
               END-IF
           END-IF
           CALL "CWCRTS" USING "G" CRT-STATUS
           IF  CRT-STATUS = X"FFFFFF"
               MOVE 1           TO KEY-EXIT
               MOVE SP2-KEY-ESC TO SP2-CD-KEY
               ADD  1           TO VEZFF
               IF  VEZFF > 3
                   STOP RUN
               END-IF
           END-IF
           MOVE 0 TO ERRO
           PERFORM UNTIL KEY-EXIT = 1
                     AND ERRO = 0
              IF  (SP2-CD-KEY = SP2-KEY-ENTER)
              AND (CWBOXF = 2)
                   MOVE 0 TO CWBOXF
              END-IF
              IF   CWBOXF = 0
                   MOVE FIELD-ID(CURSOR-GUIA) TO SP2-CD-NEXT-FLD-ID
                   move wait-sw to sp2-cd-wait-sw
                   move 0       to sP2-CD-row-col-sw t
              else
                   move "1" to sp2-cd-wait-sw
                   move 2   to sP2-CD-row-col-sw
                   if SP2-CD-NEXT-FLD-ID not = sp2-fd-id
                      move SP2-CD-NEXT-FLD-ID to sp2-fd-id
                      MOVE 2000               TO SP2-FD-VAR-LEN
                      CALL SP2   USING SP2-GET-FIELD-DEF
                                       SP2-FIELD-DEF
                   end-if
                   MOVE 0 TO SP2-FD-ROW
                   MOVE T TO SP2-CD-CURSOR-COL
              END-IF
              MOVE 0 TO SP2-CD-LAST-FLD-ID BUTTON
              IF   CWBOXF = 2
                   MOVE 0 TO CWBOXF
              END-IF
              CALL "CWCRTS" USING "G" CRT-STATUS
              IF   CRT-STATUS = HIGH-VALUES
                   MOVE SP2-KEY-CLOSE TO SP2-CD-KEY
              ELSE
                   MOVE buffer TO save-buffer
                   PERFORM DD THRU FIM-DD
                   IF   PRINT-SCREEN = 1
                        MOVE 1 to cwuser-LINE cwuser-COLUMN
                        move 2000 to cwuser-LENGTH-CHAR
                        PERFORM 110-LER-TELA    THRU 110-99-FIM
                        CALL "CWPRTS"
                              using cwprts cwuser-characters
                        CANCEL "CWPRTS"
                        MOVE 0 TO PRINT-SCREEN
                   END-IF
                   PERFORM 820-EVITA-BUG-SP2 THRU 820-99-FIM
                   MOVE SP2-CD-LAST-FLD-ID TO LAST-TAB
                   CALL SP2 USING SP2-SET-PANEL-FIELDS BUFFER
                   CALL SP2   USING SP2-GET-INPUT SP2-CONVERSE-DATA
                   IF  COMBO = 1
                       IF   SP2-CD-NEXT-PANEL NOT = LOW-VALUES
                            move low-values to SP2-CD-NEXT-PANEL
                            MOVE 'CWBOXF'   TO SP2-ND-NAME
                            CALL SP2 USING SP2-ACTIVATE-WINDOW
                                           SP2-NAME-DEF
                       END-IF
                       IF  SP2-CD-KEY = SP2-KEY-SWITCH
                           MOVE ZERO              TO SP2-CD-KEY
                           PERFORM 220-CLOSE-COMBO THRU 220-99-FIM
                           MOVE 0 TO CWBOXF
                           CALL SP2
                           USING SP2-GET-INPUT SP2-CONVERSE-DATA
                           EXIT PERFORM CYCLE
                       END-IF
                       move SP2-CD-LAST-FLD-ID to last-id
                   END-IF
                   PERFORM 830-GETF THRU 830-99-FIM
                   PERFORM VARYING M3
                           FROM WORKBOX-VERTICAL-LENGTH BY -1
                           UNTIL M3 = 0
                              OR LISTA  (M3) = SP2-CD-NEXT-FLD-ID
                              OR LISTA2 (M3) = SP2-CD-NEXT-FLD-ID
                           CONTINUE
                   END-PERFORM
                   IF  CWBOXF = 1
                       IF   SP2-CD-KEY = SP2-KEY-UP
                       AND (FIM NOT = 'N')
                       AND (FIM NOT = '<')
                       AND  M3  = 1
                            MOVE SP2-KEY-BACKTAB TO SP2-CD-KEY
                       ELSE
                           IF  SP2-CD-KEY = SP2-KEY-DOWN
                           AND FIM = '<'
                           AND M3 NOT < WORKBOX-VERTICAL-LENGTH
                               MOVE SP2-KEY-TAB TO SP2-CD-KEY
                           END-IF
                       END-IF
                   END-IF
                   IF  CWBOXF = 1
                   AND M3 = 0
                   AND SP2-CD-KEY = SP2-KEY-DOWN
                       IF (LISTA2(1) NOT = 0)
                       AND (WORKBOX-ORDER NOT = 1)
                          MOVE 1 TO M3
                          MOVE LISTA2 (M3) TO SP2-CD-NEXT-FLD-ID
                       END-IF
                       IF (LISTA(1) NOT = 0)
                       AND WORKBOX-ORDER = 1
                          MOVE 1 TO M3
                          MOVE LISTA (M3) TO SP2-CD-NEXT-FLD-ID
                       END-IF
                   END-IF
                   move 0 to no-check
                   IF (SP2-CD-KEY = BUTTON-ON OR SP2-KEY-ENTER)
                   AND M3 > 0
                       MOVE WORKBOX-HIDE  TO HIDE-DATANAME
                       MOVE pop-WINDOW    TO HIDE-WINDOW
                       READ HIDE
                       IF  FS-HIDE < '10'
                       AND HIDE-FIELD > 0
                       and HIDE-combo-SIZE > 0
                           MOVE pop-WINDOW    TO campos-janela
                           move hide-dataname to dataname-e
                           move 1             to campos-subscript
                           move hide-lincol   to pos-e
                           READ CAMPOS KEY IS CAMPOS-name
                           MOVE SPACES TO
                                HIDE-STRING-DATA
                                HIDE-STRING-SHOW
                                CAMPOS-SHOW
                           MOVE CAMPOS-OFF TO OFF-W2
                           MOVE LEN-LK(HIDE-FIELD) TO P2
                           IF  WORKBOX-RETURN < 2
                               MOVE TEXTO-1 (M3)
                                 TO buffer(off-w2: p2)
                                    DATA-LK(HIDE-FIELD)
                                    HIDE-STRING-DATA
                                    DATA-E
                                    CAMPOS-SHOW
                               IF WORKBOX-STRING-1-LENGTH = 0
                                  MOVE TEXTO-2 (M3)
                                    TO buffer(off-w2: p2)
                                       HIDE-STRING-SHOW
                                       BUFFER(CAMPOS-OFF:P2)
                                       CAMPOS-SHOW
                               ELSE
                                  move off-w2 to off-w4
                                  move HIDE-combo-SIZE to w4
                                  ADD WORKBOX-STRING-1-LENGTH
                                   to off-w2
                                  subtract WORKBOX-STRING-1-LENGTH
                                      from HIDE-combo-SIZE
                                  MOVE TEXTO-2 (M3)
                                    TO buffer
                                       (off-w2: HIDE-combo-SIZE)
                                  add WORKBOX-STRING-1-LENGTH
                                   to HIDE-combo-SIZE
                                  move buffer
                                       (off-w4: w4)
                                    to HIDE-STRING-SHOW
                                       CAMPOS-SHOW
                                       DATA-E
                               END-IF
                           ELSE
                               MOVE TEXTO-2 (M3)
                                 TO buffer(off-w2: p2)
                                    DATA-LK(HIDE-FIELD)
                                    HIDE-STRING-DATA
                                    HIDE-STRING-SHOW
                                    DATA-E
                               IF HIDE-ALPHA = 1
                                  MOVE TEXTO-1 (M3)
                                    TO buffer
                                       (off-w2: HIDE-combo-SIZE)
                                    HIDE-STRING-SHOW
                               END-IF
                           END-IF
                           REWRITE CAMPOS-REG
                           REWRITE HIDE-REG
                           IF HIDE-FIELD NOT < FIELDS
                              MOVE 1 TO KEY-EXIT
                           END-IF
                       END-IF
                   END-IF
                   IF  LAST-TAB > 0
                   AND SP2-CD-LAST-FLD-ID NOT = LAST-TAB
                       PERFORM VARYING TEST-GUIA FROM 1 BY 1
                                 UNTIL TEST-GUIA > GUIA
                            IF SP2-CD-LAST-FLD-ID = FIELD-ID (TEST-GUIA)
                               MOVE TEST-GUIA            TO CURSOR-GUIA
                               MOVE ID-FIELD (TEST-GUIA) TO F-CICS
                               DISPLAY "CICS-CURPOS"
                                       UPON ENVIRONMENT-NAME
                               DISPLAY POS-LK(F-CICS)
                                  UPON ENVIRONMENT-VALUE
                            END-IF
                       END-PERFORM
                   END-IF
                   IF (SP2-CD-KEY = SP2-KEY-TAB OR SP2-KEY-ENTER)
                   AND M3 = ZERO
                   AND CWBOXF = 0
                   AND COMBO  = 0
                       MOVE DATANAME-E TO HIDE-DATANAME
                       MOVE pop-WINDOW TO HIDE-WINDOW
                       READ HIDE
                       IF  FS-HIDE < '10'
                       AND HIDE-FIELD > 0
                       and HIDE-combo-SIZE > 0
                       and HIDE-LIST = 0
                       and hide-ORDER = 1
                       and hide-RETURN = 1
                       and hide-STRING-2-LENGTH > ZERO
                       and hide-HORIZONTAL-LENGTH > hide-STRING-2-LENGTH
                           move 1 to no-check
                           MOVE HIDE-OBJECT TO WORKBOX-OBJECT
                           move workbox-reg to save-workbox
                           READ WORKBOX
                           IF  FS-WORKBOX > '10'
                               MOVE 1           TO DISPLAY-ACCEPT
                               MOVE OBJECTS     TO SAVE-OBJECTS
                               MOVE HIDE-OBJECT TO OBJETO
                                                   OBJECTS
                               PERFORM 271-EXIBE-OBJECTS
                                  THRU 271-99-FIM
                               PERFORM 500-OPEN-LISTBOX  THRU 500-99-FIM
                               MOVE 0            TO DISPLAY-ACCEPT
                               MOVE SAVE-OBJECTS TO OBJECTS
                               PERFORM 510-CALL-PROVIDER
                                  THRU 510-99-FIM
                               MOVE BUFFER
                               (HIDE-POS:WORKBOX-STRING-1-LENGTH)
                                TO STRING-1
                               MOVE SPACES TO STRING-2
                               IF  PIC-LK (HIDE-FIELD)(1:1) = "Z"
                                   INSPECT STRING-1
                                   (1: LEN-LK(HIDE-FIELD))
                                           CONVERTING SPACE TO "0"
                               END-IF
                               MOVE STRING-1 TO TEST-OPTION
                               SET NOT-LESS TO TRUE
                               PERFORM 650-LER THRU 650-99-FIM
                               IF NOT  AT-END
                                  SET READ-NEXT TO TRUE
                                  PERFORM 650-LER THRU 650-99-FIM
                               END-IF
                               IF   STRING-1 = TEST-OPTION
                                   COMPUTE P = HIDE-POS
                                         + WORKBOX-STRING-1-LENGTH
                                   MOVE STRING-2 TO BUFFER
                                     (P: WORKBOX-STRING-2-LENGTH)
                               END-IF
                               PERFORM 710-CANCEL-PROVIDER
                                  THRU 710-99-FIM
                           END-IF
                           move save-workbox to workbox-reg
                       END-IF
                   END-IF
                   MOVE FIELD-ID (CURSOR-GUIA) TO CRITICAR-ID
                   IF  SP2-CD-KEY NOT = SP2-KEY-CTRL-FIELD
                       PERFORM 820-EVITA-BUG-SP2 THRU 820-99-FIM
                   END-IF
                   IF   SP2-CD-KEY = SP2-KEY-CTRL-P
                        MOVE 1 TO PRINT-SCREEN
                                  KEY-EXIT
                        MOVE 0 TO ERRO
                        GO TO 170-ACCEPT-TELA
                   END-IF
                   IF (SP2-CD-KEY = SP2-KEY-CTRL-FIELD)
                   AND(SP2-CD-CTRL-FIELD-KEY = SP2-KEY-CTRL-MINUS)
                       MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                       MOVE 0             TO SP2-CD-CTRL-FIELD-KEY
                   ELSE
                       IF  SP2-CD-KEY = SP2-KEY-CTRL-FIELD
                           MOVE SP2-CD-CTRL-FIELD-KEY TO SP2-CD-KEY
                           MOVE 0 TO SP2-CD-CTRL-FIELD-KEY
                           MOVE 7 TO NEXT-OK
                       END-IF
                   END-IF
                   IF SP2-CD-KEY = SP2-KEY-ENTER
                   AND (ENTER-TERMINATE = 'ON' OR COMANDO = 'a')
                       MOVE 1 TO KEY-EXIT
                   ELSE
                       IF ACCEPTS = 1
                       AND (SP2-CD-KEY = SP2-KEY-TAB
                                      OR SP2-KEY-BACKTAB)
                           MOVE 1 TO KEY-EXIT
                       END-IF
                   END-IF
                   IF (SP2-CD-KEY = SP2-KEY-MOUSE)
                   AND(SP2-CD-NEXT-FLD-ID = BARR-ID)
                       move 1 to skip-keys
                   ELSE
                       move 0 to skip-keys
                   END-IF
                   IF (SP2-CD-KEY = SP2-KEY-MOUSE)
                   AND (SP2-CD-NEXT-FLD-ID NOT = BARR-ID)
                    IF SP2-CD-NEXT-FLD-ID = FIELD-ID(CURSOR-GUIA)
                       MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                       MOVE 1             TO KEY-EXIT
                    ELSE
                       IF (SP2-CD-BUTTON-ID = 0)
                       AND (SP2-CD-NEXT-FLD-ID NOT = 0)
                           MOVE SP2-CD-NEXT-FLD-ID TO BUTTONS-ID
                           READ BUTTONS
                           IF  FS-BUTTONS = "00"
                               MOVE BUTTONS-ID TO SP2-CD-BUTTON-ID
                               MOVE BUTTON-ON     TO SP2-CD-KEY
                           ELSE
                               MOVE 0 TO SP2-CD-KEY
                               move CURSOR-GUIA
                                 to SAVE-CURSOR-GUIA
                               PERFORM VARYING CURSOR-GUIA
                                     FROM 1 BY 1
                                   UNTIL FIELD-ID(CURSOR-GUIA) = 0
                                      OR FIELD-ID(CURSOR-GUIA)
                                      =  SP2-CD-NEXT-FLD-ID
                                   CONTINUE
                               END-PERFORM
                               IF (CWBOXF = 0)
                               AND(CURSOR-GUIA = SAVE-CURSOR-GUIA)
                                   MOVE SP2-KEY-ENTER
                                     TO SP2-CD-KEY
                               ELSE
                                   IF CURSOR-GUIA > GUIA
                                      MOVE SAVE-CURSOR-GUIA
                                        TO CURSOR-GUIA
                                   END-IF
                                   EXIT PERFORM CYCLE
                               END-IF
                           END-IF
                       END-IF
                    END-IF
                   END-IF
                   if sp2-cd-wait-sw = "1"
                      if  skip-keys = 0
                          move 0 to k
                          if SP2-CD-KEY (2: 1) = X"00"
                             perform varying d from 1 by 1
                                        until d > length digitaveis
                                           or k = 1
                              if SP2-CD-KEY(1:1) = digitaveis(D:1)
                                 move 1 to k
                              end-if
                             end-perform
                           end-if
                           if k = 1
                              compute off-c = SP2-FD-PROG-OFF
                                            + T + 1
                              move SP2-CD-KEY (1: 1)
                                to buffer (off-c: 1)
                              move 0 to SP2-CD-KEY
                              add 1  to t
                              CALL SP2 USING SP2-DISPLAY-WINDOW
                                             SP2-NULL-PARM
                           else
                              evaluate SP2-CD-KEY
                                  when SP2-KEY-RIGHT
                                       if t < SP2-FD-PROG-LEN
                                          add 1 to t
                                       else
                                          move 0 to t
                                       end-if
                                  when SP2-KEY-LEFT
                                       if t > 0
                                          subtract 1 from t
                                       end-if
                              end-evaluate
                          end-if
                      else
                          move WAIT-SW to sp2-cd-wait-sw
                          PERFORM 820-EVITA-BUG-SP2
                             THRU 820-99-FIM
                          move "1" to sp2-cd-wait-sw
                      end-if
                   end-if              *> FODA
                   MOVE SP2-CD-KEY TO HELP-KEY
                   IF   SP2-CD-KEY  = SP2-KEY-TIMEOUT
                   OR                 SP2-KEY-MENU
                        IF   SP2-CD-KEY  = SP2-KEY-TIMEOUT
                        AND  TIMEOUT-RETURN = 98
                             MOVE 77 TO TIMEOUT-RETURN
                        END-IF
                        MOVE SP2-KEY-ESC TO SP2-CD-KEY
                   ELSE
                        IF  (SP2-CD-KEY = SP2-KEY-CTRL-FIELD)
                        AND (SP2-CD-BUTTON-ID = 90)
                             MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                             MOVE 0             TO SP2-CD-BUTTON-ID
                        END-IF
                   END-IF
                   MOVE SP2-CD-KEY TO GET-KEY
                   MOVE 0 TO SKIP-CRITICA
                   IF   SP2-CD-KEY = BUTTON-ON
                        IF  SP2-CD-BUTTON-ID = 0
                        AND (SP2-CD-NEXT-FLD-ID NOT = 0)
                            MOVE SP2-CD-NEXT-FLD-ID
                              TO SP2-CD-BUTTON-ID
                        END-IF
                        MOVE 0 TO objeto
                        PERFORM VARYING objeto
                                   FROM 1 BY 1
                                  UNTIL objeto > objects
                                     OR SKIP-CRITICA = 1
                          MOVE pop-WINDOW TO OBJECT-WINDOW
                          MOVE objeto     TO OBJECT-SEQ
                          READ OBJWORK
                          IF  SP2-CD-BUTTON-ID = objeto-id
                          AND(OBJECT-GUIA NOT = SPACES)
                              MOVE OBJECT-GUIA TO HIDE-DATANAME
                              MOVE pop-WINDOW  TO HIDE-WINDOW
                              READ HIDE
                              IF FS-HIDE < '10'
                              AND HIDE-GUIA > 0
                                  MOVE HIDE-GUIA TO CURSOR-GUIA
                              END-IF
                          END-IF
                          IF  SP2-CD-BUTTON-ID = objeto-id
                          AND (OBJECT-CTRL = "F" OR "s")
                              IF  OBJECT-KEY = 1
                                  MOVE 1           TO SKIP-CRITICA
                                  MOVE 0           TO SP2-CD-BUTTON-ID
                                  MOVE SP2-KEY-ESC TO SP2-CD-KEY
                                  EXIT PERFORM
                              ELSE
                                  IF  OBJECT-KEY NOT = 0
                                      MOVE OBJECT-KEY  TO TECLA
                                      PERFORM 216-CONVERTE-TECLA-SP2
                                         THRU 216-99-FIM
                                      CALL "CWAKEY" USING TECLA MIL
                                      PERFORM 034-CRITICA-GERAL
                                         THRU 034-99-FIM
                                      VARYING FIELD-CRITICA
                                         FROM 1 BY 1
                                        UNTIL FIELD-CRITICA > FIELDS
                                            OR SKIP-CRITICA = 1
                                            OR ERRO > 0
                                      if erro = 0
                                         MOVE 1 TO KEY-EXIT
                                         EXIT PERFORM
                                      end-if
                                  END-IF
                              END-IF
                          END-IF
                        END-PERFORM
                        IF   SP2-CD-KEY = BUTTON-ON
                             MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                        END-IF
                   END-IF
                   IF  TECLA > 0
                   AND KEY-EXIT = 1
                       EXIT PERFORM CYCLE
                   END-IF
                   IF   SP2-CD-KEY = BUTTON-ON
                   AND  SP2-CD-BUTTON-ID = 0
                        MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                        MOVE 1             TO ENTER-BUTTON
                                              KEY-EXIT
                   ELSE
                        MOVE 0             TO ENTER-BUTTON
                   END-IF
                   IF NEXT-OK = 7
                   OR NOT (SP2-CD-KEY = SP2-KEY-CTRL-FIELD
                   AND SP2-CD-BUTTON-ID = SP2-KEY-MOUSE)
                      MOVE 1 TO NEXT-OK
                   ELSE
                      MOVE 0 TO NEXT-OK
                   END-IF
                   IF  SP2-CD-LAST-FLD-ID > 0
                   AND((SP2-CD-LAST-FLD-ID
                       NOT = FIELD-ID(CURSOR-GUIA))
                     OR (SP2-CD-KEY = SP2-KEY-CTRL-FIELD
                     AND SP2-CD-BUTTON-ID = SP2-KEY-MOUSE))
                        PERFORM VARYING I FROM 1 BY 1
                                UNTIL I > GUIA
                                     OR (NEXT-OK NOT = 0)
                            IF   SP2-CD-NEXT-FLD-ID = FIELD-ID(I)
                                 MOVE I TO CURSOR-GUIA
                                           NEXT-OK
                                 MOVE 0 TO SP2-CD-BUTTON-ID
                                           SP2-CD-KEY
                            END-IF
                        END-PERFORM
                        MOVE pop-window  TO CAMPOS-JANELA
                        move cursor-guia to CAMPOS-GUIA
                        read campos key is CAMPOS-JANELA-GUIA
                   END-IF
                   IF  NEXT-OK = 0
                   AND SP2-CD-LAST-FLD-ID > 0
                   AND(SP2-CD-LAST-FLD-ID NOT =
                        FIELD-ID(CURSOR-GUIA))
                     OR (SP2-CD-KEY = SP2-KEY-CTRL-FIELD
                     AND SP2-CD-BUTTON-ID = SP2-KEY-MOUSE)
                        PERFORM VARYING I FROM 1 BY 1
                                UNTIL I > GUIA
                            IF   SP2-CD-LAST-FLD-ID = FIELD-ID(I)
                                 MOVE I TO CURSOR-GUIA
                                           NEXT-OK
                                 MOVE 0 TO SP2-CD-BUTTON-ID
                                           SP2-CD-KEY
                            END-IF
                        END-PERFORM
                   END-IF
                   IF  NEXT-OK = 0
                   AND ENTER-BUTTON = 1
                   AND SP2-CD-NEXT-FLD-ID > 0
                   AND(SP2-CD-NEXT-FLD-ID NOT =
                        FIELD-ID(CURSOR-GUIA))
                       PERFORM VARYING I FROM 1 BY 1
                               UNTIL I > GUIA
                           IF   SP2-CD-NEXT-FLD-ID = FIELD-ID(I)
                                MOVE I TO NEXT-OK
                                MOVE FIELD-ID(I) TO BUTTON
                           END-IF
                       END-PERFORM
                       IF   NEXT-OK > GUIA-FIELDS
                            MOVE 1 TO KEY-EXIT
                       END-IF
                   END-IF
                   IF  (CWSETK-KEYS NOT = 0)
                        MOVE LOW-VALUES TO TECLA(1:)
                        PERFORM 215-CONVERTE-TECLA
                           THRU 215-99-FIM
                        MOVE 0 TO OK-KEY
                        IF TECLA(1:) NOT = LOW-VALUES
                        AND NOT ALT-F11
                           PERFORM VARYING I FROM 1
                                       BY 1 UNTIL I > CWSETK-KEYS
                                               OR OK-KEY = 1
                                   IF CWSETK-KEY (I) = 100
                                      MOVE 100 TO TECLA
                                   END-IF
                                   IF CWSETK-KEY (I) = TECLA
                                      MOVE 1 TO OK-KEY
                                   END-IF
                           END-PERFORM
                           IF OK-KEY = 0
                              MOVE 0 TO SP2-CD-KEY
                           END-IF
                        END-IF
                   END-IF
              END-IF
              if sp2-cd-key = SP2-KEY-ESC
              AND CWBOXF = 1
                  move SP2-KEY-TAB to sp2-cd-key
              end-if
              IF   SP2-CD-KEY             = SP2-KEY-CTRL-FIELD
              AND  SP2-CD-CTRL-FIELD-KEY  = SP2-KEY-UP
              AND (CURSOR-GUIA = 1
               OR (COMBO = 1 AND M3 = 1))
                   MOVE ZERO            TO SP2-CD-CTRL-FIELD-KEY
                   MOVE SP2-KEY-BACKTAB TO SP2-CD-KEY
              END-IF
              IF   SP2-CD-KEY = SP2-KEY-DOWN
              AND (CURSOR-GUIA = GUIA and guia > 1)
              AND  CWBOXF = 0
              AND  FS-HIDE = '23'
                   MOVE SP2-KEY-TAB TO SP2-CD-KEY
              END-IF
              PERFORM 215-CONVERTE-TECLA THRU 215-99-FIM
              CALL "CWAKEY" USING TECLA MIL
              MOVE 0 TO TECLA
              IF   SP2-CD-KEY = SP2-KEY-SYS-SHUTDOWN
              OR                SP2-KEY-APP-CLOSE
              OR                SP2-KEY-CLOSE
                   MOVE SP2-KEY-CLOSE TO SP2-CD-KEY
              ELSE
                   MOVE 1 TO SKIP-HIDE
                   IF   SP2-CD-KEY = SP2-KEY-TAB
                   OR                SP2-KEY-BACKTAB
                   OR                SP2-KEY-ESC
                        PERFORM 220-CLOSE-COMBO THRU 220-99-FIM
                        MOVE 0 TO CWBOXF
                        PERFORM 032-CRITICA      THRU 032-99-FIM
                   ELSE
                        INITIALIZE SUPERS
                        if no-check = 0
                           PERFORM 205-CHECK-CWBOXF THRU 205-99-FIM
                        end-if
                        IF  SUPER-POS NOT = 0
                            IF   SUPER-TEXTO (1: SUPER-SIZE) =
                                 buffer
                                        (SUPER-POS: SUPER-SIZE)
                                 MOVE SP2-KEY-DOWN  TO SP2-CD-KEY
                                 MOVE 0 TO CWBOXF
                            END-IF
                        END-IF
                        IF  CWBOXF = 0
                            PERFORM 032-CRITICA  THRU 032-99-FIM
                        ELSE
                             IF   ENTER-BUTTON = 1
                             AND  SP2-CD-KEY = 1000
                                  MOVE 0             TO CWBOXF
                                  MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                             END-IF
                        END-IF
                   END-IF
                   MOVE 0 TO SKIP-HIDE
              END-IF
              IF  SP2-CD-KEY = SP2-KEY-CTRL-FIELD
              AND SP2-CD-BUTTON-ID > 0
              AND ENTER-BUTTON = 0
                  MOVE SP2-CD-BUTTON-ID TO SP2-CD-KEY
              END-IF
              IF    (HELP-KEY = SP2-KEY-F1
                              OR SP2-KEY-ALT-H)
              AND    SP2-CD-KEY = 1000
                     IF  HELP-KEY = SP2-KEY-F1
                         SET EDIT-F1   TO TRUE
                     ELSE
                         SET EDIT-ALT-H TO TRUE
                     END-IF
                     MOVE HIDE-DATANAME   TO DATANAMES-FIELD
                     MOVE 1               TO DATANAMES-SUBSCRIPT
                     READ DATANAMES KEY DATANAMES-FIELD
                     MOVE PROGRAMA        TO CONTEXT-ID
                     MOVE DATANAMES-FIELD TO CONTEXT-ID(09:)
                     CALL "CWEHLP" USING CONTEXT-ID
                                         TECLA-EDIT
                                         DATANAMES-POS
                                         "G"
              END-IF
              EVALUATE SP2-CD-KEY
                  WHEN 1000
                       CONTINUE
                  WHEN SP2-KEY-ENTER
                       if   guia = cursor-guia
                            move 0 to from-combo
                       end-if
                       IF   from-combo = 1
                            move 0 to from-combo
                       else
                            IF   CURSOR-GUIA NOT < GUIA
                                 MOVE 1 TO KEY-EXIT
                            ELSE
                                 ADD 1 TO CURSOR-GUIA
                                 IF  CURSOR-GUIA > GUIA
                                     MOVE 1 TO CURSOR-GUIA
                                 END-IF
                            END-IF
                       END-IF
                  WHEN SP2-KEY-CLOSE
                       MOVE SP2-KEY-ESC   TO SP2-CD-KEY
                       MOVE 1             TO KEY-EXIT
                       CALL "CWCRTS" USING "S" X"FFFFFF"
                  WHEN SP2-KEY-ESC
                       MOVE 1 TO KEY-EXIT
                  WHEN SP2-KEY-HOME
                       MOVE 1 TO CURSOR-GUIA
                  WHEN SP2-KEY-END
                       MOVE GUIA-FIELDS TO CURSOR-GUIA
                  WHEN SP2-KEY-DOWN
                       PERFORM VARYING I FROM CURSOR-GUIA BY 1
                               UNTIL I > GUIA
                                     OR(GUIA-COL (I)
                                      = GUIA-COL (CURSOR-GUIA)
                                     AND I > CURSOR-GUIA)
                               CONTINUE
                       END-PERFORM
                       IF I < GUIA
                          MOVE I TO CURSOR-GUIA
                       ELSE
                          ADD 1 TO CURSOR-GUIA
                       END-IF
                       IF  CURSOR-GUIA > GUIA
                           IF ENTER-TERMINATE = 'ON'
                           OR COMANDO = 'a'
                              move 1 to cursor-guia
                           else
                              MOVE GUIA-FIELDS TO CURSOR-GUIA
                              MOVE 1    TO KEY-EXIT
                           end-if
                       END-IF
                  WHEN SP2-KEY-TAB
                       ADD 1 TO CURSOR-GUIA
                       IF  CURSOR-GUIA > GUIA
                           MOVE 1 TO CURSOR-GUIA
                       END-IF
                  WHEN SP2-KEY-UP
                       move 0 to I
                       IF CWBOXF = 1
                          PERFORM VARYING I FROM CURSOR-GUIA BY -1
                                  UNTIL I = 0
                                        OR (GUIA-COL (I)
                                         = GUIA-COL (CURSOR-GUIA)
                                        AND I < CURSOR-GUIA)
                                  CONTINUE
                          END-PERFORM
                       END-IF
                       IF I > 0
                          MOVE I TO CURSOR-GUIA
                       ELSE
                          SUBTRACT 1 FROM CURSOR-GUIA
                       END-IF
                       IF  CURSOR-GUIA = 0
                           IF ENTER-TERMINATE NOT = 'ON'
                           OR COMANDO = 'a'
                              MOVE 1 TO CURSOR-GUIA
                              MOVE 1 TO KEY-EXIT
                           ELSE
                              MOVE GUIA-FIELDS TO CURSOR-GUIA
                           END-IF
                       END-IF
                  WHEN SP2-KEY-BACKTAB
                       SUBTRACT 1 FROM CURSOR-GUIA
                       IF  CURSOR-GUIA = 0
                           IF ENTER-TERMINATE = 'ON'
                           OR COMANDO = 'a'
                              MOVE GUIA-FIELDS TO CURSOR-GUIA
                           else
                              MOVE GUIA TO CURSOR-GUIA
                           end-if
                       END-IF
                  WHEN SP2-KEY-PGUP
                       SET EDIT-PAGE-UP           TO TRUE
                       MOVE 1 TO KEY-EXIT
                  WHEN SP2-KEY-PGDN
                       SET EDIT-PAGE-DOWN         TO TRUE
                       MOVE 1 TO KEY-EXIT
                  WHEN OTHER
                       IF   SP2-CD-KEY = SP2-KEY-F1
                       OR   SP2-CD-KEY = SP2-KEY-ALT-H
                            IF  SP2-CD-KEY = SP2-KEY-F1
                                SET EDIT-F1   TO TRUE
                            ELSE
                                SET EDIT-ALT-H TO TRUE
                            END-IF
                            MOVE FIELD-ID (CURSOR-GUIA)
                              TO DATANAMES-ID
                            READ DATANAMES
                            MOVE PROGRAMA        TO CONTEXT-ID
                            MOVE DATANAMES-FIELD TO CONTEXT-ID(09:)
                            CALL "CWEHLP" USING CONTEXT-ID
                                                TECLA-EDIT
                                                DATANAMES-POS
                                                "G"
                            IF  TECLA-EDIT = 0
                                MOVE 0 TO SP2-CD-KEY
                            END-IF
                       END-IF
                       IF (SP2-CD-KEY > 0
                       AND (SP2-CD-KEY NOT = BUTTON-ON))
                       OR  (SP2-CD-BUTTON-ID > 0)
                           MOVE 1 TO KEY-EXIT
                       END-IF
              END-EVALUATE
              IF  SP2-CD-KEY = SP2-KEY-ESC
              OR               SP2-KEY-CLOSE
              OR               ESC
                  MOVE 0 TO ERRO
              ELSE
                  IF  KEY-EXIT = 1
                      PERFORM 034-CRITICA-GERAL THRU 034-99-FIM
                         VARYING FIELD-CRITICA FROM 1 BY 1
                           UNTIL FIELD-CRITICA > FIELDS
                              OR SKIP-CRITICA = 1
                              OR ERRO > 0
                  END-IF
              END-IF
           END-PERFORM
           PERFORM 350-DISABLE-OBJECT THRU 350-99-FIM
           INSPECT buffer (1: SIZE-FIELDS)
                             CONVERTING X"00FF"
                                     TO X"2020"
           IF   CWACCENT = "OFF"
                INSPECT buffer (1: SIZE-FIELDS)
                             CONVERTING ACENTOS-WINDOWS
                                     TO ACENTOS-OFF
           ELSE
                IF   MFF = "MF"
                     INSPECT buffer (1: SIZE-FIELDS)
                                  CONVERTING ACENTOS-437
                                          TO ACENTOS-850
                ELSE
                     INSPECT buffer (1: SIZE-FIELDS)
                                  CONVERTING ACENTOS-WINDOWS
                                          TO ACENTOS-850
                END-IF
           END-IF
           IF FUN-ACCEPT
           PERFORM VARYING FIELD FROM 1 BY 1 UNTIL FIELD > FIELDS
              MOVE pop-window TO CAMPOS-JANELA
              IF   CAMPO-LK (FIELD) = ZERO
              OR   DATANAME-LK (FIELD) = SPACES
                   exit perform cycle
              ELSE
                   MOVE CAMPO-LK (FIELD) TO CAMPOS-CAMPO
                   MOVE POS-LK   (FIELD) TO POS-E
                   READ CAMPOS
                   if   fs-campos > '09'
                   or prot
                   or (not accept-e)
                        exit perform cycle
                   end-if
              end-if
      ***************** OUTRA COLUNA
              MOVE DATANAME-E TO HIDE-DATANAME
              MOVE pop-WINDOW TO HIDE-WINDOW
              READ HIDE
              IF  FS-HIDE < '10'
              AND HIDE-ALPHA > 0
              AND (HIDE-STRING-DATA NOT = SPACES)
                  IF   HIDE-POS NOT = ZERO
                  AND  HIDE-SIZE = LEN-E
                       IF  buffer(campos-off:LEN-E)
                         NOT = save-buffer(campos-off:LEN-E)
                           MOVE buffer(campos-off:LEN-E)
                             TO HIDE-STRING-DATA
                       END-IF
                  END-IF
                  MOVE HIDE-STRING-DATA TO DATA-LK(FIELD)
                  DELETE HIDE RECORD
                  MOVE '23' TO FS-HIDE
                  MOVE FIELD-ID (FIELD) TO SP2-FD-ID
                  exit perform cycle
              END-IF
              MOVE CAMPOS-off TO OFF-W
              PERFORM 017-EDIT THRU 017-99-FIM
              MOVE DATA-E TO DATA-LK(FIELD)
              REWRITE CAMPOS-REG
              IF NUMERICO OR COM-BARRA
                 INSPECT DATA-LK(FIELD) (1: LEN-E)
                      CONVERTING
                      LOW-VALUE TO ZERO
              ELSE
                 INSPECT DATA-LK(FIELD) (1: LEN-E)
                      CONVERTING
                      LOW-VALUE TO SPACE
              END-IF
              IF   LENF-LK (FIELD) NUMERIC
              AND  LENF-LK (FIELD) > LEN-LK (FIELD)
              AND (PIC-LK  (FIELD) (1: 1) = "9" OR "Z")
                   COMPUTE P = LENF-LK (FIELD) - LEN-LK (FIELD) + 1
                   MOVE LEN-LK  (FIELD) TO P2
                   MOVE DATA-LK (FIELD) TO BUF
                   MOVE ALL "0"         TO DATA-LK (FIELD)
                   MOVE BUF             TO DATA-LK (FIELD) (P: P2)
              END-IF
           END-PERFORM
           END-IF
           IF   CWACCENT = "OFF"
                INSPECT buffer (1: SIZE-FIELDS)
                             CONVERTING ACENTOS-850
                                     TO ACENTOS-OFF
           ELSE
                IF   MFF = "MF"
                     INSPECT buffer (1: SIZE-FIELDS)
                             CONVERTING ACENTOS-850
                                     TO ACENTOS-437
                ELSE
                     INSPECT buffer (1: SIZE-FIELDS)
                             CONVERTING ACENTOS-850
                                     TO ACENTOS-WINDOWS
                END-IF
           END-IF
           IF  KEY-CWBOXF NOT = 0
               MOVE KEY-CWBOXF TO SP2-CD-KEY
               MOVE 0          To KEY-CWBOXF
               MOVE 0          TO TECLA
           END-IF
           IF  TECLA = 0
               PERFORM 215-CONVERTE-TECLA THRU 215-99-FIM
           END-IF
           CALL "CWAKEY" USING TECLA MIL
           IF   X91-PARAMETER = 4
                MOVE TECLA TO TECLA-LK
           END-IF
           PERFORM 420-CRT-STATUS THRU 420-99-FIM
           MOVE LOW-VALUES TO SP2-FD-DATA
                              SP2-FD-VAR-LENS
           MOVE 0 TO DATANAMES-ID
           PERFORM UNTIL GUIA = 0
              IF   SAVE-ID (GUIA) NOT = 0
                   MOVE    SAVE-ID (GUIA)      TO SP2-FD-ID
                   PERFORM 042-DISABLE-FIELD THRU 042-99-FIM
              END-IF
              MOVE     0   TO FIELD-ID (GUIA)
                              COMBO-ID (GUIA)
                              SAVE-ID  (GUIA)
              SUBTRACT 1 FROM GUIA
           END-PERFORM
           PERFORM UNTIL COMBOS-FIELDS = 0
              IF   COMBO-FD-ID (COMBOS-FIELDS) NOT = 0
                   MOVE    COMBO-FD-ID (COMBOS-FIELDS) TO SP2-FD-ID
                   PERFORM 042-DISABLE-FIELD         THRU 042-99-FIM
                   MOVE    0             TO COMBO-FD-ID (COMBOS-FIELDS)
              END-IF
              SUBTRACT 1 FROM COMBOS-FIELDS
           END-PERFORM.

       170-99-FIM. EXIT.

       180-SOBRESCREVE-CAMPOS.

           if   sobrescreveu = 1
                move zero to sobrescreveu
                exit paragraph
           end-if
           move campos-reg   to save-campos
           move campos-campo to test-campo

           perform test after until dif = 0
             move zero to dif
             move y    to y2
             PERFORM S TIMES
                IF (TELA-CAMPO (X Y) NOT = ZERO)
                AND((TELA-CAMPO (X Y) NOT = test-campo)
                 or (from-e and TELA-CAMPO (X Y) = test-campo))
                   CONTINUE *> Verificar fields das posi‡äes
                   MOVE pop-window       TO CAMPOS-JANELA
                   MOVE TELA-CAMPO (X Y) TO CAMPOS-CAMPO
                   MOVE WIN-ID           TO POS-E
                   READ CAMPOS
                   IF FS-CAMPOS < '10'
                      if  dataname-e = ss-campo
                      and len-e = s
                      and (not from-e)
                          exit perform
                      end-if
                      evaluate true
                      when Y > 1
                       and TELA-CAMPO (X Y - 1) = CAMPOS-CAMPO
                           move campos-reg   to save-campos2
                           move campos-campo to dif-campo
                           compute dif = (y - col-e)
                           move dif    to len-e
                           move spaces to CAMPOS-DATA-ED(dif + 1:)
                                                 DATA-E (dif + 1:)
                           rewrite campos-reg
                           PERFORM 190-NOVO-CAMPO THRU 190-99-FIM
                           move campos-campo to save-CAMPO2
                           move save-campos2 to campos-reg
                           move save-CAMPO2  to campos-campo
                           perform varying pos from 1 by 1
                                   until pos = 30
                                      or dataname-e(pos:1) = SPACE
                           end-perform
                           move '!'    to dataname-e(pos:)
                           move y      to pos
                                          col-e
                           perform until (tela-campo(x pos)
                                          not = dif-campo)
                                   and   (tela-campo(x pos)
                                          not = campos-campo)
                                   move campos-campo
                                     to tela-campo(x pos)
                                   add 1 to pos
                           end-perform
                           MOVE  CAMPOS-DATA-ED TO WORK
                           MOVE  WORK(DIF + 1:) TO CAMPOS-DATA-ED
                           MOVE         DATA-E  TO WORK
                           MOVE  WORK(DIF + 1:) TO        DATA-E
                           subtract dif  from len-e
                           write campos-reg
                      when TELA-CAMPO (X Y + 1) = CAMPOS-CAMPO
                           add      1   to col-e
                           subtract 1 from len-e
                           move CAMPOS-DATA-ED(2:) TO WORK-ED
                           move WORK-ED            TO CAMPOS-DATA-ED
                           move DATA-E (2:)        TO WORK-ED
                           move WORK-ED            to DATA-E
                           rewrite campos-reg
                           MOVE ZERO               TO TELA-CAMPO (X Y)
                           move 1                  to dif
                      when other
                           move spaces to buffer-ctrl
                                (campos-off: len-e)
                           perform drop-campo thru fim-drop-campo
                           delete campos record
                           MOVE ZERO TO TELA-CAMPO (X Y)
                      END-EVALUATE
                   ELSE
                      MOVE ZERO TO TELA-CAMPO (X Y)
                   END-IF
                END-IF
                if  dif not = 0
                    move y2 to y
                    exit perform
                end-if
                ADD 1 TO Y
                IF Y > JANWID
                   MOVE 1 TO Y
                   IF X < JANHEI
                      ADD 1 TO X
                   END-IF
                END-IF
             end-perform
           END-PERFORM

           move save-campos TO campos-reg
           read campos
           move save-campos TO campos-reg.

       180-99-FIM. EXIT.

       190-NOVO-CAMPO.

           PERFORM TEST AFTER UNTIL FS-CAMPOS > '09'
                   MOVE pop-window         TO CAMPOS-JANELA
                   move dataname-lk(field) to dataname-e
                   move subscript          to campos-subscript
                   move POS-LK (FIELD)     TO POS-E
                   read campos key is campos-name
                   if fs-campos < '10'
                      move spaces to buffer-ctrl
                           (campos-off: len-e)
                      perform drop-campo
                         thru fim-drop-campo
                      delete campos record
                   end-if
           END-PERFORM
           MOVE HIGH-VALUES  TO CAMPOS-CAMPO(1:)
                                pos-e(1:)
           START CAMPOS KEY NOT GREATER CAMPOS-CHAVE
           IF  FS-CAMPOS < '10'
               READ CAMPOS PREVIOUS RECORD
           END-IF
           IF  FS-CAMPOS > '09'
           OR (CAMPOS-JANELA NOT = pop-window)
               MOVE pop-window   TO CAMPOS-JANELA
               MOVE 1            TO CAMPOS-CAMPO
           ELSE
               ADD  1            TO CAMPOS-CAMPO
           END-IF

           MOVE ZERO TO CAMPOS-FD-ID campos-off.

       190-99-FIM. EXIT.

       200-BMS-ATTR.

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

       200-99-FIM. EXIT.

       205-CHECK-CWBOXF.

           MOVE SP2-CD-KEY TO CD-KEY-VERT

           IF    COMBO = 0
           AND   CWBOXF = 0
           AND   SP2-CD-KEY = SP2-KEY-DOWN
                 COMPUTE CG = CURSOR-GUIA + 1
                 MOVE pop-WINDOW                TO HIDE-WINDOW
                 MOVE DATANAME-E                TO HIDE-DATANAME
                 READ HIDE
                 IF  FS-HIDE < "10"
                 AND HIDE-LIST = 1
                     move '77' to FS-HIDE
                 END-IF
                 IF  FS-HIDE = "23"
                 AND CURSOR-GUIA > 0
                 AND COMBO-ID (CURSOR-GUIA - 1) > 0
                 AND(sp2-cd-key not = sp2-key-down)
                     compute guia-up = cursor-guia - 1
                     perform until guia-up = 0
                             or (DATANAME-LK (guia-up) not = spaces)
                             subtract 1 from guia-up
                     end-perform
                     if guia-up not = zero
                        MOVE pop-WINDOW                TO HIDE-WINDOW
                        READ HIDE
                     end-if
                 END-IF
                 IF  FS-HIDE < "10"
                     MOVE FIELD-ID(CURSOR-GUIA) TO SP2-CD-NEXT-FLD-ID
                     MOVE SP2-KEY-ENTER         TO SP2-CD-KEY
                     IF  COMBO-ID (CURSOR-GUIA) = 0
                     AND COMBO-ID (CG) > 0
                         MOVE COMBO-ID (CG) TO COMBO-ID (CURSOR-GUIA)
                     END-IF
                 END-IF
                 IF  FS-HIDE = "77"
                     MOVE '00' TO FS-HIDE
                 END-IF
           END-IF

           IF  CWBOXF = 0
           AND (CURSOR-GUIA NOT = 0)
           AND  SP2-CD-KEY = SP2-KEY-ENTER
           AND (COMBO-ID (CURSOR-GUIA) NOT = 0)
               MOVE COMBO-ID (CURSOR-GUIA) TO SP2-CD-BUTTON-ID
               MOVE BUTTON-ON              TO SP2-CD-KEY
               MOVE 0                      TO SP2-CD-NEXT-FLD-ID
           ELSE
               MOVE CD-KEY-VERT   TO SP2-CD-KEY
           END-IF

           MOVE 0 TO CWBOXF

           IF   SP2-CD-BUTTON-ID = 0
                MOVE SP2-CD-NEXT-FLD-ID TO LISTBOX-ID
                READ LISTBOX
                IF  FS-LISTBOX = "00"
                    IF   LISTBOX-OBJECT NOT = WORKBOX-OBJECT
                         move 0 to t
                         PERFORM 220-CLOSE-COMBO THRU 220-99-FIM
                         MOVE SP2-CD-NEXT-FLD-ID TO LISTBOX-ID
                         READ LISTBOX
                         MOVE LISTBOX-OBJECT TO WORKBOX-OBJECT
                         READ WORKBOX
                    ELSE
                         IF   CWBOXF = 1
                         AND  (WORKBOX-GUIA NOT = 0)
                              MOVE LISTBOX-ID TO FIELD-ID (WORKBOX-GUIA)
                         END-IF
                    END-IF
                    IF (WORKBOX-GUIA NOT = 0)
                        MOVE LISTBOX-ID TO FIELD-ID (WORKBOX-GUIA)
                    END-IF
                    PERFORM 600-SELECT-LISTBOX THRU 600-99-FIM
                    REWRITE WORKBOX-REG
                    IF   HIDE-LIST = 0
                    AND  SP2-CD-KEY = SP2-KEY-ENTER
                         MOVE 2 TO CWBOXF
                    ELSE
                         IF   SP2-CD-KEY NOT = BUTTON-ON
                         AND (SP2-CD-KEY NOT = 1000)
                              MOVE 1    TO CWBOXF
                              MOVE 1000 TO SP2-CD-KEY
                         END-IF
                    END-IF
                ELSE
                    PERFORM 220-CLOSE-COMBO THRU 220-99-FIM
                END-IF
           ELSE
                MOVE SP2-CD-BUTTON-ID TO LISTBOX-ID
                READ LISTBOX
                IF  FS-LISTBOX = "00"
                    IF   LISTBOX-OBJECT NOT = WORKBOX-OBJECT
                         IF   COMBO = 0
                         AND  CWBOXF = 0
                         AND  GET-KEY = SP2-KEY-ENTER
                              MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                              GO TO 205-99-FIM
                         END-IF
                         PERFORM 220-CLOSE-COMBO THRU 220-99-FIM
                         MOVE SP2-CD-BUTTON-ID TO LISTBOX-ID
                         READ LISTBOX
                         MOVE LISTBOX-OBJECT TO WORKBOX-OBJECT
                         READ WORKBOX
                         IF FS-WORKBOX > '09'
                            INITIALIZE WORKBOX-REG
                            MOVE LISTBOX-OBJECT TO WORKBOX-OBJECT
                            WRITE WORKBOX-REG
                            READ WORKBOX
                         END-IF
                         COMPUTE OFF-W = SIZE-FIELDS + 1
                         MOVE SPACES TO buffer (OFF-W: )
                         MOVE objects     TO SAVE-OBJETOS
                         MOVE LISTBOX-OBJECT TO CWOBJE-oc-NUMBER
                                                objects
                         SET CWOBJE-GET   TO TRUE
                         MOVE 3 TO X91-parameter-CWOBJE
                         PERFORM 670-CWOBJE THRU 670-99-FIM
                         MOVE 2 TO X91-parameter-CWOBJE
                         ADD  1                TO CWOBJE-LINE
                         MOVE 1                TO COMBO
                         PERFORM 500-OPEN-LISTBOX THRU 500-99-FIM
                         MOVE SAVE-OBJETOS TO objects
                         MOVE 1                TO CWBOXF
                         MOVE 1000             TO SP2-CD-KEY
                    ELSE
                         MOVE SP2-CD-BUTTON-ID TO SP2-CD-NEXT-FLD-ID
                         MOVE 0                TO SP2-CD-BUTTON-ID
                                                  SP2-CD-KEY
                         IF   HIDE-GUIA = 0
                              MOVE 2 TO CWBOXF
                         ELSE
                              MOVE HIDE-GUIA TO CURSOR-GUIA
                         END-IF
                         PERFORM 220-CLOSE-COMBO THRU 220-99-FIM
                         MOVE 0                TO WORKBOX-OBJECT
                    END-IF
                ELSE
                    PERFORM 220-CLOSE-COMBO THRU 220-99-FIM
                END-IF
           END-IF.

       205-99-FIM. EXIT.

       210-SET-ATTRIBUTES.

           IF   BACK-E = "B"
           AND  FORE-E = "F"
           AND  TELA-ATTR-LIN (LIN-E) (COL-E: 1) = X"00"
                MOVE LOW-VALUES TO TELA-ATTR-LIN (LIN-E) (COL-E: LEN-E)
           END-IF

           IF   BACK-E = "b"
           AND  FORE-E = "f"
           AND (MB-ATTRIB NOT = LOW-VALUES)
              MOVE LOW-VALUES TO TELA-ATTR-LIN (LIN-E) (COL-E: LEN-E)
              INSPECT TELA-ATTR-LIN (LIN-E) (COL-E: LEN-E)
                       CONVERTING LOW-VALUES TO MB-ATTRIB
              EXIT PARAGRAPH
           ELSE
              IF   BACK-E = "b"
              AND  FORE-E = "f"
              AND (BOX-LINE (LIN-E) (COL-E: LEN-E) NOT = ALL X"00")
                   MOVE BOX-LINE (LIN-E) (COL-E: LEN-E)
                     TO TELA-ATTR-LIN (LIN-E) (COL-E: LEN-E)
              END-IF
           END-IF

           IF   FORE-E NUMERIC
           OR   BACK-E NUMERIC
                MOVE ZERO TO BACK-NW
                MOVE 7 TO FORE-NW
                IF   FORE-E NUMERIC
                     MOVE FORE-N TO FORE-NW
                END-IF
                IF   BACK-E NUMERIC
                     MOVE BACK-N TO BACK-NW
                END-IF
                IF FORE-NW = BACK-NW
                AND (FOREBACK NOT = 'OFF')
                   IF FORE-NW = 1
                      MOVE 7 TO FORE-NW
                   ELSE
                      IF FORE-NW = 7
                         MOVE ZERO TO FORE-NW
                      ELSE
                         ADD 1 TO FORE-NW
                      END-IF
                   END-IF
                END-IF
                IF   REVERSE-E = "R"
                     COMPUTE ATTRIBUTE = FORE-NW + (BACK-NW * 16)
                ELSE
                     COMPUTE ATTRIBUTE = BACK-NW + (FORE-NW * 16)
                END-IF
                IF   BLINK-E = "B"
                     ADD 128 TO ATTRIBUTE
                END-IF
                IF   HIGH-E = "H"
                     ADD 8 TO ATTRIBUTE
                END-IF
                MOVE LOW-VALUES TO TELA-ATTR-LIN (LIN-E) (COL-E: LEN-E)
                INSPECT TELA-ATTR-LIN (LIN-E) (COL-E: LEN-E)
                        CONVERTING LOW-VALUE TO ATTRIBUTE-X
           ELSE
                IF   REVERSE-E = "R"
                     MOVE ALL X"1F" TO TELA-ATTR-LIN
                          (LIN-E) (COL-E: LEN-E)
                                  REVERSEDS-LIN (LIN-E) (COL-E: LEN-E)
                ELSE
                     IF REVERSEDS-LIN (LIN-E) (COL-E: LEN-E) = ALL X"1F"
                        MOVE ALL X"00"
                          TO TELA-ATTR-LIN (LIN-E) (COL-E: LEN-E)
                             REVERSEDS-LIN (LIN-E) (COL-E: LEN-E)
                     END-IF
                END-IF
           END-IF.

       210-99-FIM. EXIT.

       211-CHECK-AVANCO.

           IF   RM-EOL
                MOVE CURPOS-LIN TO RM-LINE-ERASE
           ELSE
                IF   CURPOS-LIN NOT = RM-LINE-ERASE
                     MOVE 0 TO RM-LINE-ERASE
                END-IF
           END-IF
           IF   S             = 0
           OR  (S             = 80
           AND  POS-LK (FIELD) = "0101"
           AND  DATANAME-LK (FIELD) = 'SPACES')
                IF   S             = 80
                     MOVE SPACES TO TELA-CHARS
                END-IF
                GO 211-99-FIM
           END-IF
           IF  (BZERO-E = "Z" OR "L" OR "R")
           AND  POS-LK (FIELD) = "0000"
                move "z" to BZERO-E
           END-IF
           IF   ADVANCE-E = "A"
           AND  FLAG-SA = 0
           AND  POS-LK (FIELD)(1:2) = "00"
           AND  (FIELD = FIELDS OR MULTILINE = FIELDS)
           AND (CURPOS-LIN NOT = RM-LINE-ERASE)
                MOVE 1 TO CURPOS-COL
                PERFORM 211-AVANCO
                ADD  1 TO CURPOS-LIN
           ELSE
                IF ((col-e + s) - 1 ) = JANWID
                AND (POS-LK (FIELD) NOT = "0000")
                and lin-e = JANHEI
                    continue
                ELSE
                    ADD s TO CURPOS-COL
                    PERFORM UNTIL (CURPOS-COL NOT > JANWID)
                            ADD  1 TO CURPOS-LIN
                            SUBTRACT JANWID FROM CURPOS-COL
                            MOVE 1 TO CURPOS-COL
                    END-PERFORM
                    IF   CURPOS-LIN > JANHEI
                         GO TO 211-99-FIM
                    END-IF
                END-IF
           END-IF.

       211-AVANCO.

           IF  CURPOS-LIN > JANHEI
           OR  AVANCO-PENDENTE = 1
               IF  AVANCO-PENDENTE = 0
               AND POS-LK (FIELD) (1:2) = '00'
               AND(POS-LK (FIELD) (3:2) NOT = '00')
                   MOVE 1 TO AVANCO-PENDENTE
                   NEXT SENTENCE
               END-IF
               MOVE 0        TO AVANCO-PENDENTE
               MOVE JANHEI   TO CURPOS-LIN
               MOVE 1        TO CURPOS-COL
               MOVE 0        TO AVANCO-PENDENTE
               PERFORM VARYING X FROM 2 BY 1 UNTIL X > JANHEI
                       MOVE TELA-LIN (X)     TO TELA-LIN (X - 1)
                       MOVE TELA-ATTR-LIN(X) TO TELA-ATTR-LIN(X - 1)
               END-PERFORM
               MOVE SPACES TO TELA-LIN (JANHEI)
           END-IF.

       211-99-FIM. EXIT.

       215-CONVERTE-TECLA.

           COPY CWSPKY.
           IF ESC
           AND TIMEOUT-RETURN = 77
               MOVE SP2-KEY-ALT-F11 TO SP2-CD-KEY
               SET EDIT-ALT-F11 TO TRUE
               SET ALT-F11 TO TRUE
           END-IF.

       215-99-FIM. EXIT.

       216-CONVERTE-TECLA-SP2.

           EVALUATE TRUE
               WHEN ALT-F12
                    MOVE SP2-KEY-ALT-F12     to sp2-cd-key
               WHEN ALT-F11
                    MOVE SP2-KEY-ALT-F11     to sp2-cd-key
               WHEN CONTROL-F12
                    MOVE SP2-KEY-CTRL-F12    to sp2-cd-key
               WHEN CONTROL-F11
                    MOVE SP2-KEY-CTRL-F11    to sp2-cd-key
               WHEN SHIFT-F12
                    MOVE SP2-KEY-SHIFT-F12   to sp2-cd-key
               WHEN SHIFT-F11
                    MOVE SP2-KEY-SHIFT-F11   to sp2-cd-key
               WHEN F12
                    MOVE SP2-KEY-F12         to sp2-cd-key
               WHEN F11
                    MOVE SP2-KEY-F11         to sp2-cd-key
               WHEN CONTROL-PAGE-UP
                    MOVE SP2-KEY-CTRL-PGUP   to sp2-cd-key
               WHEN ALT-EQUAL
                    MOVE SP2-KEY-ALT-EQUAL   to sp2-cd-key
               WHEN ALT-TRACE
                    MOVE SP2-KEY-ALT-MINUS   to sp2-cd-key
               WHEN ALT-0
                    MOVE SP2-KEY-ALT-0       to sp2-cd-key
               WHEN ALT-9
                    MOVE SP2-KEY-ALT-9       to sp2-cd-key
               WHEN ALT-8
                    MOVE SP2-KEY-ALT-8       to sp2-cd-key
               WHEN ALT-7
                    MOVE SP2-KEY-ALT-7       to sp2-cd-key
               WHEN ALT-6
                    MOVE SP2-KEY-ALT-6       to sp2-cd-key
               WHEN ALT-5
                    MOVE SP2-KEY-ALT-5       to sp2-cd-key
               WHEN ALT-4
                    MOVE SP2-KEY-ALT-4       to sp2-cd-key
               WHEN ALT-3
                    MOVE SP2-KEY-ALT-3       to sp2-cd-key
               WHEN ALT-2
                    MOVE SP2-KEY-ALT-2       to sp2-cd-key
               WHEN ALT-1
                    MOVE SP2-KEY-ALT-1       to sp2-cd-key
               WHEN CONTROL-PAGE-DOWN
                    MOVE SP2-KEY-CTRL-PGDN   to sp2-cd-key
               WHEN ALT-F10
                    MOVE SP2-KEY-ALT-F10     to sp2-cd-key
               WHEN ALT-F9
                    MOVE SP2-KEY-ALT-F9      to sp2-cd-key
               WHEN ALT-F8
                    MOVE SP2-KEY-ALT-F8      to sp2-cd-key
               WHEN ALT-F7
                    MOVE SP2-KEY-ALT-F7      to sp2-cd-key
               WHEN ALT-F6
                    MOVE SP2-KEY-ALT-F6      to sp2-cd-key
               WHEN ALT-F5
                    MOVE SP2-KEY-ALT-F5      to sp2-cd-key
               WHEN ALT-F4
                    MOVE SP2-KEY-ALT-F4      to sp2-cd-key
               WHEN ALT-F3
                    MOVE SP2-KEY-ALT-F3      to sp2-cd-key
               WHEN ALT-F2
                    MOVE SP2-KEY-ALT-F2      to sp2-cd-key
               WHEN ALT-F1
                    MOVE SP2-KEY-ALT-F1      to sp2-cd-key
               WHEN SHIFT-F10
                    MOVE SP2-KEY-SHIFT-F10   to sp2-cd-key
               WHEN SHIFT-F9
                    MOVE SP2-KEY-SHIFT-F9    to sp2-cd-key
               WHEN SHIFT-F8
                    MOVE SP2-KEY-SHIFT-F8    to sp2-cd-key
               WHEN SHIFT-F7
                    MOVE SP2-KEY-SHIFT-F7    to sp2-cd-key
               WHEN SHIFT-F6
                    MOVE SP2-KEY-SHIFT-F6    to sp2-cd-key
               WHEN SHIFT-F5
                    MOVE SP2-KEY-SHIFT-F5    to sp2-cd-key
               WHEN SHIFT-F4
                    MOVE SP2-KEY-SHIFT-F4    to sp2-cd-key
               WHEN SHIFT-F3
                    MOVE SP2-KEY-SHIFT-F3    to sp2-cd-key
               WHEN SHIFT-F2
                    MOVE SP2-KEY-SHIFT-F2    to sp2-cd-key
               WHEN SHIFT-F1
                    MOVE SP2-KEY-SHIFT-F1    to sp2-cd-key
               WHEN PAGE-DOWN
                    MOVE SP2-KEY-PGDN        to sp2-cd-key
               WHEN CURSOR-DOWN
                    MOVE SP2-KEY-DOWN        to sp2-cd-key
               WHEN PAGE-UP
                    MOVE SP2-KEY-PGUP        to sp2-cd-key
               WHEN CURSOR-UP
                    MOVE SP2-KEY-UP          to sp2-cd-key
               WHEN F10
                    MOVE SP2-KEY-F10         to sp2-cd-key
               WHEN F9
                    MOVE SP2-KEY-F9          to sp2-cd-key
               WHEN F8
                    MOVE SP2-KEY-F8          to sp2-cd-key
               WHEN F7
                    MOVE SP2-KEY-F7          to sp2-cd-key
               WHEN F6
                    MOVE SP2-KEY-F6          to sp2-cd-key
               WHEN F5
                    MOVE SP2-KEY-F5          to sp2-cd-key
               WHEN F4
                    MOVE SP2-KEY-F4          to sp2-cd-key
               WHEN F3
                    MOVE SP2-KEY-F3          to sp2-cd-key
               WHEN F2
                    MOVE SP2-KEY-F2          to sp2-cd-key
               WHEN F1
                    MOVE SP2-KEY-F1          to sp2-cd-key
               WHEN ALT-M
                    MOVE SP2-KEY-ALT-M       to sp2-cd-key
               WHEN ALT-N
                    MOVE SP2-KEY-ALT-N       to sp2-cd-key
               WHEN ALT-B
                    MOVE SP2-KEY-ALT-B       to sp2-cd-key
               WHEN ALT-V
                    MOVE SP2-KEY-ALT-V       to sp2-cd-key
               WHEN ALT-C
                    MOVE SP2-KEY-ALT-C       to sp2-cd-key
               WHEN ALT-X
                    MOVE SP2-KEY-ALT-X       to sp2-cd-key
               WHEN ALT-Z
                    MOVE SP2-KEY-ALT-Z       to sp2-cd-key
               WHEN ALT-L
                    MOVE SP2-KEY-ALT-L       to sp2-cd-key
               WHEN ALT-K
                    MOVE SP2-KEY-ALT-K       to sp2-cd-key
               WHEN ALT-J
                    MOVE SP2-KEY-ALT-J       to sp2-cd-key
               WHEN ALT-H
                    MOVE SP2-KEY-ALT-H       to sp2-cd-key
               WHEN ALT-G
                    MOVE SP2-KEY-ALT-G       to sp2-cd-key
               WHEN ALT-F
                    MOVE SP2-KEY-ALT-F       to sp2-cd-key
               WHEN ALT-D
                    MOVE SP2-KEY-ALT-D       to sp2-cd-key
               WHEN ALT-S
                    MOVE SP2-KEY-ALT-S       to sp2-cd-key
               WHEN ALT-A
                    MOVE SP2-KEY-ALT-A       to sp2-cd-key
               WHEN ALT-P
                    MOVE SP2-KEY-ALT-P       to sp2-cd-key
               WHEN ALT-O
                    MOVE SP2-KEY-ALT-O       to sp2-cd-key
               WHEN ALT-I
                    MOVE SP2-KEY-ALT-I       to sp2-cd-key
               WHEN ALT-U
                    MOVE SP2-KEY-ALT-U       to sp2-cd-key
               WHEN ALT-Y
                    MOVE SP2-KEY-ALT-Y       to sp2-cd-key
               WHEN ALT-T
                    MOVE SP2-KEY-ALT-T       to sp2-cd-key
               WHEN ALT-R
                    MOVE SP2-KEY-ALT-R       to sp2-cd-key
               WHEN ALT-E
                    MOVE SP2-KEY-ALT-E       to sp2-cd-key
               WHEN ALT-W
                    MOVE SP2-KEY-ALT-W       to sp2-cd-key
               WHEN ALT-Q
                    MOVE SP2-KEY-ALT-Q       to sp2-cd-key
               WHEN ESC
                    MOVE SP2-KEY-ESC         to sp2-cd-key
               WHEN ENTER-KEY
                    MOVE SP2-KEY-ENTER       to sp2-cd-key
               WHEN TAB
                    MOVE SP2-KEY-TAB         to sp2-cd-key
               WHEN CURSOR-UP
                    MOVE SP2-KEY-BACKTAB     to sp2-cd-key
           END-EVALUATE.
       216-99-FIM. EXIT.

       220-CLOSE-COMBO.

           IF   COMBO = 1
                MOVE WORKBOX-OBJECT TO DELETE-OBJECT
                PERFORM 700-DROP-LISTBOX THRU 700-990-FIM
                MOVE ZERO TO COMBO
                MOVE DELETE-OBJECT TO LISTBOX-OBJECT
                START LISTBOX KEY NOT LESS LISTBOX-OBJECT
                PERFORM UNTIL FS-LISTBOX > "09"
                        READ LISTBOX NEXT RECORD
                        IF   LISTBOX-OBJECT NOT = DELETE-OBJECT
                             MOVE "10" TO FS-LISTBOX
                        END-IF
                        IF   FS-LISTBOX < "10"
                        AND  LISTBOX-KEY < 2
                             DELETE LISTBOX RECORD
                        END-IF
                END-PERFORM
                DELETE WORKBOX RECORD
                INITIALIZE WORKBOX-REG
                COMPUTE P = SIZE-FIELDS + 1
                MOVE SPACES TO buffer (P: )
                if sp2-cd-key = SP2-KEY-BACKTAB
                   move 1000 to sp2-cd-key
                end-if
           END-IF.

       220-99-FIM. EXIT.

       040-MAP-FIELD.

           IF   (SP2-FD-NAME(1:7) NOT = 'CWRADIO')
           AND  (SP2-FD-NAME(1:7) NOT = 'CWCHECK')
                MOVE SP2-FD-INITIAL-LEN TO SP2-FD-VAR-LEN
           END-IF

           IF RESOLUTION = 1
              ADD 1 TO SP2-FD-HEIGHT
           END-IF

           CALL SP2   USING SP2-DELETE-FIELD  SP2-FIELD-DEF
           CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF

           IF   DELETE-IMAGE NOT = SPACES
                CALL SP2   USING SP2-DISPLAY-FIELD SP2-FIELD-DEF
                CALL "CBL_DELETE_FILE" USING DELETE-IMAGE
                MOVE SPACES TO DELETE-IMAGE
           END-IF.

       040-99-FIM. EXIT.

       041-DROP-FIELD.

           If SP2-FD-CTRL-TYPE = 'c' OR 'r'
              COMPUTE CWPRTS-LIN = SP2-FD-ROW - 1
              COMPUTE CWPRTS-COL = SP2-FD-COL - 1
              MOVE SPACES TO CWPRTS-TXT
              CALL "CWPRTS" USING CWPRTS
           end-if

           CALL SP2 USING SP2-DELETE-FIELD SP2-FIELD-DEF.

       041-99-FIM. EXIT.

       042-DISABLE-FIELD.

           MOVE 2000     TO SP2-FD-VAR-LEN
           CALL SP2   USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
           CALL SP2   USING SP2-DELETE-FIELD  SP2-FIELD-DEF *> Importante BUG SP2
           PERFORM 430-GET-DATANAMES THRU 430-99-FIM
           move datanames-colr to sp2-fd-colr
           COMPUTE OFF-W = SP2-FD-PROG-OFF + 1
           IF    SP2-FD-OUTPUT = "s"
                 COMPUTE II = OFF-W + SP2-FD-VAR-LEN - 1
                 MOVE SP2-FD-VAR-LEN TO III
                 PERFORM VARYING II
                            FROM II
                              BY -1 UNTIL III = 0
                          OR (buffer (II: 1) NOT = SPACE)
                          SUBTRACT 1 FROM III
                 END-PERFORM
                 MOVE SPACES TO buffer
                                 (OFF-W: SP2-FD-VAR-LEN)
                 IF III NOT = 0
                    MOVE ALL "*" TO buffer (OFF-W: III)
                 END-IF
           END-IF
           IF    SP2-FD-COLR = X"00" OR ENTRY-ATTR
                 MOVE DISABLE-ATTR  TO SP2-FD-COLR
           END-IF
           IF   DATANAMES-FIELD (1: 7) = "CWCHECK" OR "CWRADIO"
                MOVE STATIC       TO SP2-FD-COLR
                MOVE "g"          TO SP2-FD-OUTPUT
                IF   TELA-ATTR-COL (LIN-E COL-E) NOT = X"00"
                     MOVE TELA-ATTR-COL (LIN-E COL-E) TO SP2-FD-COLR
                END-IF
           ELSE
                MOVE "y"          TO SP2-FD-OUTPUT
                IF  ATTRIBUTE-X = CHOOSE
                AND CHOOSE > 0
                    MOVE XUSE TO SP2-FD-COLR
                END-IF
           END-IF
           CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF.

       042-99-FIM. EXIT.

       260-LOAD-HIDE.

           SET CWOBJE-OCCURS TO TRUE
           MOVE 3 TO X91-parameter-CWOBJE
           PERFORM 670-CWOBJE THRU 670-99-FIM
           MOVE CWOBJE-oc-NUMBER TO OBJECTS-CWOBJE
           SET CWOBJE-GET TO TRUE
           PERFORM VARYING objeto FROM 1 BY 1
                     UNTIL objeto > OBJECTS-CWOBJE
                   MOVE objeto  TO CWOBJE-oc-NUMBER
                   PERFORM 670-CWOBJE THRU 670-99-FIM
                   IF  CWOBJE-LIST-BOX
                   OR  CWOBJE-COMBO-BOX
                       IF   CWOBJE-OPTION NOT = SPACES
                            PERFORM 261-PUT-HIDE THRU 261-99-FIM
                       END-IF
                   END-IF
           END-PERFORM
           MOVE 2 TO X91-parameter-CWOBJE.

       260-99-FIM. EXIT.

       261-PUT-HIDE.

           MOVE CWOBJE-OPTION TO HIDE-DATANAME
           MOVE pop-WINDOW    TO HIDE-WINDOW
           READ HIDE
           IF   FS-HIDE < "10"
                IF  objeto = HIDE-OBJECT
                and cwobje-at = hide-lincol(1:4)
                    move pop-WINDOW    TO CAMPOS-JANELA
                    MOVE CWOBJE-OPTION TO DATANAME-E
                    MOVE 1             TO campos-subscript
                    move cwobje-at     TO pos-e
                    READ CAMPOS KEY IS CAMPOS-NAME
                    IF FS-CAMPOS < '10'
                       EXIT PARAGRAPH
                    END-IF
                END-IF
                DELETE HIDE RECORD
           END-IF
           INITIALIZE HIDE-REG
           MOVE CWOBJE-OPTION            TO HIDE-DATANAME
           MOVE pop-WINDOW               TO HIDE-WINDOW
           MOVE objeto                   TO HIDE-OBJECT
           MOVE CWOBJE-HORIZONTAL-LENGTH TO HIDE-SIZE
           IF   CWOBJE-LIST-BOX
                MOVE 1                   TO HIDE-LIST
           ELSE
                MOVE 0 TO HIDE-LIST
                move CWOBJE-RETURN            to HIDE-return
                move CWOBJE-ORDER             to HIDE-order
                move CWOBJE-STRING-2-LENGTH   to HIDE-string-2-length
                move CWOBJE-HORIZONTAL-LENGTH to HIDE-HORIZONTAL-LENGTH
                IF  CWOBJE-NOEDIT
                    MOVE 1 TO HIDE-NOEDIT
                END-IF
           END-IF
      ********************** OUTRA COLUNA
           IF   CWOBJE-COMBO-BOX
               IF CWOBJE-ORDER = 1
               AND (CWOBJE-STRING-1-LENGTH NOT = 0)
                    MOVE CWOBJE-STRING-1-LENGTH TO HIDE-combo-SIZE
                    MOVE 1                      TO HIDE-ALPHA
               ELSE
                    IF   CWOBJE-STRING-2-LENGTH NOT = 0
                         MOVE CWOBJE-STRING-2-LENGTH TO HIDE-combo-SIZE
                         MOVE 1 TO HIDE-other
                         IF   CWOBJE-STRING-1-LENGTH NOT = 0
                              MOVE 2 TO HIDE-ALPHA
                         END-IF
                    END-IF
               END-IF
               IF CWOBJE-WIDTH > HIDE-combo-SIZE
                  MOVE CWOBJE-WIDTH TO HIDE-combo-SIZE
               END-IF
           END-IF
           move cwobje-at                to hide-lincol(1:4)
           WRITE HIDE-REG
           IF   FS-HIDE > "09"
                CALL "CWISAM" USING ER-HIDE
           END-IF.

       261-99-FIM. EXIT.

       270-DEFINE-OBJECTS.

           SET CWOBJE-OCCURS TO TRUE
           MOVE 3 TO X91-parameter-CWOBJE
           PERFORM 670-CWOBJE THRU 670-99-FIM
           MOVE SIZE-FIELDS TO PREOBJ-SIZE-FIELDS
           move CWOBJE-oc-NUMBER to OBJECTS-CWOBJE
           SET CWOBJE-GET   TO TRUE
           MOVE "D"         TO BUTTON-TYPE
           MOVE 1           TO SAVE-OBJECT SAVED-SIZE
           MOVE 0           TO VISUAL
           PERFORM VARYING objeto FROM 1 BY 1
                     UNTIL objeto > OBJECTS-CWOBJE
                   PERFORM 271-EXIBE-OBJECTS THRU 271-99-FIM
           END-PERFORM
           MOVE 0             TO objeto SAVE-OBJECT
           IF   VISUAL = 1
           AND  FUN-DISPLAY
                PERFORM DD THRU FIM-DD
           END-IF
           REWRITE WINWORK-REG.

       270-99-FIM. EXIT.

       271-EXIBE-OBJECTS.

           MOVE objeto      TO CWOBJE-oc-NUMBER

           IF  objeto = 1
               PERFORM DD THRU FIM-DD
           END-IF

           SET     CWOBJE-GET   TO TRUE
           MOVE    3            TO X91-parameter-CWOBJE
           PERFORM 670-CWOBJE THRU 670-99-FIM *> 1808
           MOVE    2            TO X91-parameter-CWOBJE

           IF   DISPLAY-ACCEPT = 1
                EXIT PARAGRAPH
           END-IF

           COMPUTE SIZE-MOVEL = SIZE-REG
                              - LENGTH OF OBJETOS-FIXO

           IF CWOBJE-LIST-BOX
           OR CWOBJE-COMBO-BOX
              MOVE pop-window       TO CAMPOS-JANELA
              MOVE CWOBJE-OPTION    TO DATANAME-E
              MOVE 1                TO CAMPOS-SUBSCRIPT
              MOVE CWOBJE-CURPOS    TO POS-E
              READ CAMPOS KEY IS CAMPOS-NAME
              if  fs-campos > '09'
                 EXIT PARAGRAPH
              END-IF
           END-IF

           IF   (NOT FUN-ACCEPT)
           AND  (NOT CWOBJE-GROUP)
           AND  (NOT CWOBJE-TEXT)
                EXIT PARAGRAPH
           END-IF

           IF   CWOBJE-VALIDATE
                ADD  1 TO objects
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
                EXIT PARAGRAPH
           END-IF

           MOVE ACENDER TO LAST-ACENDER

           MOVE CWOBJE-LINE   TO X
           MOVE CWOBJE-COLUMN TO Y
           INITIALIZE            OBJWORJ-REG
           ADD  1             TO objects
           MOVE pop-WINDOW    TO OBJECT-WINDOW
           MOVE objects       TO OBJECT-SEQ
           READ OBJWORK
           MOVE CWOBJE-KEY    TO OBJECT-KEY

           EVALUATE TRUE
               WHEN CWOBJE-PUSH-BUTTON
                 OR CWOBJE-ICON
                 OR CWOBJE-PUSH-MOUSE
                    MOVE 1             TO VISUAL
                    MOVE LOW-VALUES    TO SP2-FD-DATA
                                          SP2-FD-VAR-LENS
                    string 'CWOBJE-' CWOBJE-LINE CWOBJE-COLUMN
                         delimited by size
                        into sp2-fd-name
                    MOVE STATIC        TO SP2-FD-COLR
                    MOVE X"8F"         TO SP2-FD-COLR
                    MOVE 'w'           TO SP2-FD-CURS-SKIP
                    COMPUTE SP2-FD-WIDTH
                       = (CWOBJE-HORIZONTAL-LENGTH * 10) + 3
                    IF    CWOBJE-HORIZONTAL-LENGTH < 5
                          ADD 7 TO SP2-FD-WIDTH
                    END-IF
                    IF    CWOBJE-PUSH-MOUSE
                          SUBTRACT 10 FROM SP2-FD-WIDTH
                    END-IF
                    CALL "CWSPID" USING SP2-FD-ID "FInsert"
                    MOVE  SP2-FD-ID TO objeto-id
                    MOVE "F"        TO OBJECT-CTRL
                    MOVE "y"        TO SP2-FD-OUTPUT
                    COMPUTE SP2-FD-ROW = X - 1
                    COMPUTE SP2-FD-COL = Y - 1
                    MOVE CWOBJE-LABEL TO SP2-FD-VAR-DATA
                    PERFORM VARYING I FROM 1 BY 1
                              UNTIL I > LENGTH OF CWOBJE-LABEL
                                 OR (SP2-FD-MNEMONIC NOT = LOW-VALUE)
                            IF   CWOBJE-LABEL (I: 1) = X"7E"
                                 ADD 1 TO I
                                  MOVE CWOBJE-LABEL (I: 1)
                                    TO SP2-FD-MNEMONIC
                            END-IF
                    END-PERFORM
                    IF   CWOBJE-PUSH-MOUSE
                         MOVE X"00"                 TO SP2-FD-TYPE
                         COMPUTE SP2-FD-HEIGHT
                            = CWOBJE-VERTICAL-LENGTH * 10
                    ELSE
                         MOVE X"01"                 TO SP2-FD-TYPE
                         COMPUTE SP2-FD-HEIGHT
                                 = (CWOBJE-VERTICAL-LENGTH + 2) * 9
                    END-IF
                    MOVE CWOBJE-HORIZONTAL-LENGTH TO SP2-FD-MAX-LEN
                    MOVE BUTTON-ON                TO SP2-FD-HELP-KEY
                    IF   CWOBJE-ICON
                    AND (CWOBJE-IMAGE NOT = SPACES)
                         MOVE "i"    TO SP2-FD-CTRL-TYPE
                         IF  (CWOBJE-OLDFILE NOT = SPACES)
                         AND (CWOBJE-FILE        = SPACES)
                             MOVE CWOBJE-OLDFILE TO CWOBJE-FILE
                         END-IF
                         IF  (NOT CWOBJE-MICRO-ICON)
                         AND (NOT CWOBJE-RECORD = SPACES)
                         AND (NOT CWOBJE-FILE   = SPACES)
                             EXEC COBOLware Picture (Extract)
                                  FILE   CWOBJE-FILE
                                  RECORD CWOBJE-RECORD
                                  IMAGE;CWOBJE-IMAGE
                             END-EXEC
                             MOVE CWOBJE-IMAGE TO DELETE-IMAGE
                         END-IF
                         MOVE SPACES TO FUNDO-EXT
                         PERFORM VARYING FUN FROM 1 BY 1
                               UNTIL FUN > LENGTH CWOBJE-IMAGE
                                 IF CWOBJE-IMAGE (FUN: 1) = '.'
                                    ADD 1 TO FUN
                                    MOVE CWOBJE-IMAGE (FUN: )
                                      TO FUNDO-EXT
                                    INSPECT FUNDO-EXT CONVERTING
                                                         MAIUSCULAS
                                                      TO MINUSCULAS
                                    EXIT PERFORM
                                 END-IF
                         END-PERFORM
                         IF   FUNDO-EXT = 'bmp'
                              MOVE CWOBJE-IMAGE
                                TO SP2-FD-VAR-DATA (9: )
                         ELSE
                              MOVE CWOBJE-IMAGE TO CWWBMP-INFILE
                              CALL "CWWBMP" USING CWWBMP-INFILE
                                            SP2-FD-VAR-DATA (9: )
                              CALL "CBL_DELETE_FILE" USING DELETE-IMAGE
                              MOVE SP2-FD-VAR-DATA (9: )
                                TO DELETE-IMAGE
                         END-IF
                         MOVE 58           TO SP2-FD-INITIAL-LEN
                         MOVE "f"          to SP2-FD-SPEC-FMT
                         IF   CWOBJE-MICRO-ICON
                              MOVE 18 TO SP2-FD-WIDTH
                              MOVE 10 TO SP2-FD-HEIGHT
                         END-IF
                    ELSE
                         MOVE "p" TO SP2-FD-CTRL-TYPE
                         MOVE CWOBJE-HORIZONTAL-LENGTH
                           TO SP2-FD-INITIAL-LEN
                         IF  SP2-FD-MNEMONIC NOT = LOW-VALUE
                             ADD      1   TO SP2-FD-INITIAL-LEN
                                             SP2-FD-VAR-LEN
                                             SP2-FD-MAX-LEN
                         END-IF
                         IF   CWLITS = "UPP"
                              INSPECT SP2-FD-VAR-DATA (1:SP2-FD-MAX-LEN)
                                  CONVERTING MINUSCULAS TO MAIUSCULAS
                         END-IF
                         IF   CWLITS = "LOW"
                              INSPECT SP2-FD-VAR-DATA (1:SP2-FD-MAX-LEN)
                                  CONVERTING MAIUSCULAS TO MINUSCULAS
                         END-IF
                         IF   CWACCENT = "OFF"
                              INSPECT SP2-FD-VAR-DATA(1:SP2-FD-MAX-LEN)
                                      CONVERTING ACENTOS-850
                                              TO ACENTOS-OFF
                         ELSE
                             IF SP2-FD-FONT-ID NOT = SP2-FO-ID
                                MOVE SP2-FD-FONT-ID TO SP2-FO-ID
                                MOVE 2000           TO SP2-FD-VAR-LEN
                                MOVE SPACES         TO SP2-FO-NAME
                                CALL SP2   USING SP2-GET-FONT-DEF
                                                 SP2-FONT-DEF
                             END-IF
                             IF   SP2-FO-NAME (1: 2) = "MF"
                                  INSPECT SP2-FD-VAR-DATA
                                          (1:SP2-FD-MAX-LEN)
                                      CONVERTING ACENTOS-850
                                              TO ACENTOS-437
                             ELSE
                                  INSPECT SP2-FD-VAR-DATA
                                          (1:SP2-FD-MAX-LEN)
                                      CONVERTING ACENTOS-850
                                              TO ACENTOS-WINDOWS
                             END-IF
                         END-IF
                    END-IF
                    MOVE SP2-FD-ID            TO BUTTONS-ID
                    MOVE SP2-FD-INITIAL-LEN   TO BUTTONS-LEN
                    MOVE SP2-FD-VAR-DATA      TO BUTTONS-DATA
                    MOVE 0                    TO BUTTONS-LIST
                    WRITE BUTTONS-REG
                    COMPUTE SP2-FD-FLD-NUM = FIELDS + objects
                    MOVE  SP2-FD-FLD-NUM TO SP2-FD-TAB-NUM
                    IF  NOT CWOBJE-TAB-OFF
                        ADD  1         TO ACCEPTS
                        MOVE ACCEPTS   TO GUIA
                        MOVE SP2-FD-ID TO FIELD-ID (GUIA)
                        MOVE X"00"     TO SP2-FD-OUTPUT
                        IF   NOT CWOBJE-PUSH-MOUSE
                             MOVE "d"  TO SP2-FD-BOR-TYPE
                        END-IF
                    END-IF
                    MOVE FONT-WIN TO SP2-FD-FONT-ID
                    IF  CWOBJE-NO3D
                        MOVE "l" TO SP2-FD-BOR-TYPE
                    END-IF
                    IF NOT CWOBJE-ICON
                       IF  (CWOBJE-OLDFILE = SPACES)
                       AND (CWPUSH-FONT NOT = SPACES)
                       AND (NOT CWOBJE-PUSH-MOUSE)
                            MOVE CWPUSH-FONT TO CWOBJE-OLDFILE
                            IF CWPUSH-WIDTH      NOT = 0
                               MOVE CWPUSH-WIDTH TO CWOBJE-FONT-WIDTH
                            END-IF
                            IF CWPUSH-HEIGHT     NOT = 0
                               MOVE CWPUSH-HEIGHT TO CWOBJE-FONT-HEIGHT
                            END-IF
                            IF CWPUSH-FIXED      = "ON"
                               SET  CWOBJE-FIXED TO TRUE
                            END-IF
                            IF CWPUSH-BOLD       = "ON"
                               SET  CWOBJE-BOLD TO TRUE
                            END-IF
                            IF CWPUSH-ITALIC     = "ON"
                               SET  CWOBJE-ITALIC TO TRUE
                            END-IF
                            IF CWPUSH-STRIKE-OUT = "ON"
                               SET  CWOBJE-STRIKE-OUT TO TRUE
                            END-IF
                            IF CWPUSH-UNDERLINE  = "ON"
                               SET  CWOBJE-UNDERLINE TO TRUE
                            END-IF
                       END-IF
                       PERFORM 272-FONT-COLOR THRU 272-99-FIM
                    END-IF
                    PERFORM 040-MAP-FIELD THRU 040-99-FIM
               WHEN CWOBJE-GROUP
                    MOVE X                        TO cwuser-LINE
                    MOVE Y                        TO cwuser-COLUMN
                    MOVE CWOBJE-HORIZONTAL-LENGTH TO cwuser-WIDTH
                    MOVE CWOBJE-VERTICAL-LENGTH   TO cwuser-HEIGHT
                    MOVE CWOBJE-LABEL             TO cwuser-CAPTION
                    SET cwscre-INSERT-GROUP       TO TRUE
                    PERFORM call-CWSCRE     THRU FIM-call-CWSCRE
                    MOVE 1                    TO VISUAL
                    MOVE LOW-VALUES           TO SP2-GD-DATA
                    move spaces to sp2-gd-name
                    string 'GROUP-' WIN-ID DELIMITED BY SIZE
                         into sp2-gd-name
                    COMPUTE SP2-GD-WIDTH
                            = CWOBJE-HORIZONTAL-LENGTH * 10
                    CALL "CWSPID" USING SP2-GD-ID "GInsert"
                    MOVE SP2-GD-ID TO objeto-id
                    MOVE "G"       TO OBJECT-CTRL
                    COMPUTE SP2-GD-ROW = X
                    COMPUTE SP2-GD-COL = Y - 1
                    compute SP2-GD-HEIGHT
                                 = CWOBJE-VERTICAL-LENGTH * 10
                    MOVE "t" TO SP2-GD-BOR-TYPE
                    MOVE "s" TO SP2-GD-SELECT-TYPE
                    MOVE  0  TO SP2-GD-FLD-CNT
                    PERFORM VARYING F FROM 1 BY 1 UNTIL F > RADIOS
                                                     OR (NOT FUN-ACCEPT)
                            MOVE RADIO-POS (F) TO WIN-ID
                            IF (X NOT < CWOBJE-LINE)
                            AND (X NOT > (CWOBJE-LINE
                                + CWOBJE-VERTICAL-LENGTH))
                            AND (Y NOT < CWOBJE-COLUMN)
                            AND (Y NOT > (CWOBJE-COLUMN
                                + CWOBJE-HORIZONTAL-LENGTH))
                                ADD  1            TO SP2-GD-FLD-CNT
                                MOVE RADIO-ID (F) TO SP2-GD-FLD-ID
                                                     (SP2-GD-FLD-CNT)
                            END-IF
                    END-PERFORM
                    IF  CWOBJE-NO3D
                        MOVE "l" TO SP2-GD-BOR-TYPE
                    END-IF
                    PERFORM 272-FONT-COLOR THRU 272-99-FIM
                    IF   CWOBJE-LABEL NOT = SPACES
                         PERFORM VARYING SP2-GD-TITLE-LEN FROM 30 BY -1
                                    UNTIL SP2-GD-TITLE-LEN = 0
                                   OR (CWOBJE-LABEL
                                   (SP2-GD-TITLE-LEN: 1) NOT = SPACE)
                                   CONTINUE
                         END-PERFORM
                         MOVE CWOBJE-LABEL TO SP2-GD-TITLE
                         IF   CWLITS = "UPP"
                              INSPECT SP2-GD-TITLE (1: SP2-GD-TITLE-LEN)
                                  CONVERTING MINUSCULAS TO MAIUSCULAS
                         END-IF
                         IF   CWLITS = "LOW"
                              INSPECT SP2-GD-TITLE (1: SP2-GD-TITLE-LEN)
                                  CONVERTING MAIUSCULAS TO MINUSCULAS
                         END-IF
                         IF   CWACCENT = "OFF"
                              INSPECT SP2-GD-TITLE (1: SP2-GD-TITLE-LEN)
                                      CONVERTING ACENTOS-850
                                              TO ACENTOS-OFF
                         ELSE
                             IF  MFS = "MF"
                                 INSPECT SP2-GD-TITLE
                                         (1: SP2-GD-TITLE-LEN)
                                     CONVERTING ACENTOS-850
                                             TO ACENTOS-437
                             ELSE
                                INSPECT SP2-GD-TITLE
                                        (1: SP2-GD-TITLE-LEN)
                                     CONVERTING ACENTOS-850
                                             TO ACENTOS-WINDOWS
                             END-IF
                         END-IF
                    END-IF
                    CALL SP2   USING SP2-SET-GROUP-DEF SP2-GROUP-DEF
               WHEN CWOBJE-TEXT
                    MOVE X                 TO cwuser-LINE
                    MOVE Y                 TO cwuser-COLUMN
                    MOVE CWOBJE-LABEL      TO cwuser-CAPTION
                    SET cwscre-INSERT-TEXT TO TRUE
                    PERFORM call-CWSCRE THRU FIM-call-CWSCRE
                    PERFORM 120-GRAVAR-TELA THRU 120-99-FIM
                    MOVE 1            TO VISUAL
                    MOVE LOW-VALUES   TO SP2-SD-DATA
                    COMPUTE SP2-SD-ROW = X - 1
                    COMPUTE SP2-SD-COL = Y - 1
                    MOVE CWOBJE-LABEL TO SP2-SD-TEXT
                                        (1: CWOBJE-HORIZONTAL-LENGTH)
                    IF   CWLITS = "UPP"
                         INSPECT SP2-SD-TEXT
                                (1: CWOBJE-HORIZONTAL-LENGTH)
                             CONVERTING MINUSCULAS TO MAIUSCULAS
                    END-IF
                    IF   CWLITS = "LOW"
                         INSPECT SP2-SD-TEXT
                                (1: CWOBJE-HORIZONTAL-LENGTH)
                             CONVERTING MAIUSCULAS TO MINUSCULAS
                    END-IF
                    IF   CWACCENT = "OFF"
                         INSPECT SP2-SD-TEXT
                                 (1: CWOBJE-HORIZONTAL-LENGTH)
                                  CONVERTING ACENTOS-850
                                          TO ACENTOS-OFF
                    ELSE
                        IF   MFS = "MF"
                             INSPECT SP2-SD-TEXT
                                     (1: CWOBJE-HORIZONTAL-LENGTH)
                                      CONVERTING ACENTOS-850
                                              TO ACENTOS-437
                        ELSE
                             INSPECT SP2-SD-TEXT
                                     (1: CWOBJE-HORIZONTAL-LENGTH)
                                      CONVERTING ACENTOS-850
                                              TO ACENTOS-WINDOWS
                        END-IF
                    END-IF
                    CALL "CWSPID" USING SP2-SD-ID "SInsert"
                    MOVE SP2-SD-ID   TO objeto-id
                    MOVE "S"         TO OBJECT-CTRL
                    MOVE 10          TO SP2-SD-HEIGHT
                    MOVE 1           TO SP2-SD-FONT-ID
                    MOVE CWOBJE-HORIZONTAL-LENGTH TO SP2-SD-TEXT-LEN
                                                     SP2-SD-VAR-LEN
                    ADD 1 TO SP2-SD-VAR-LEN
                    COMPUTE SP2-SD-WIDTH = (SP2-SD-VAR-LEN * 10)
                    PERFORM 272-FONT-COLOR THRU 272-99-FIM
                    IF CWOBJE-PIXEL-WIDTH NUMERIC
                       MOVE CWOBJE-PIXEL-WIDTH TO SP2-SD-WIDTH
                    ELSE
                       IF CWOBJE-FONT-WIDTH > 0
                          COMPUTE SP2-SD-WIDTH  =
                                    SP2-SD-VAR-LEN
                                  * (CWOBJE-FONT-WIDTH * 1,25)
                       END-IF
                    END-IF
                    IF CWOBJE-FONT-HEIGHT > 0
                       MOVE CWOBJE-FONT-HEIGHT TO SP2-SD-HEIGHT
                       IF CWOBJE-FONT-HEIGHT > 19
                          COMPUTE SP2-SD-HEIGHT = CWOBJE-FONT-HEIGHT
                                          / 2
                       END-IF
                    END-IF
                    CALL SP2   USING SP2-SET-STATIC-DEF SP2-STATIC-DEF
               WHEN CWOBJE-LIST-BOX
                    PERFORM 500-OPEN-LISTBOX THRU 500-99-FIM
               WHEN CWOBJE-COMBO-BOX
                    INITIALIZE LISTBOX-REG
                    MOVE 1              TO VISUAL
                    MOVE LOW-VALUES     TO SP2-FD-DATA
                                           SP2-FD-VAR-LENS
                    MOVE CWOBJE-OPTION  TO HIDE-DATANAME
                                           OBJECT-GUIA
                    MOVE pop-WINDOW     TO HIDE-WINDOW
                    READ HIDE
                    IF   FS-HIDE < "10"
                    AND  HIDE-LIST = 0
                         if  hide-combo = 0
                             go to 271-exit-evaluate
                         end-if
                         MOVE HIDE-COMBO    TO SP2-FD-ID
                    ELSE
                         CALL "CWSPID" USING SP2-FD-ID "FInsert"
                    END-IF
                    COMPUTE SP2-FD-ROW = X - 1
                    COMPUTE SP2-FD-COL = Y +
                                           CWOBJE-HORIZONTAL-LENGTH - 1
                    MOVE SP2-FD-ID       TO objeto-id
                                            LISTBOX-ID
                    MOVE objects         TO LISTBOX-OBJECT
                    MOVE 2               TO LISTBOX-KEY
                    WRITE LISTBOX-REG
                    MOVE "F"             TO OBJECT-CTRL
                    MOVE 18              TO SP2-FD-WIDTH
                    MOVE 08              TO SP2-FD-HEIGHT
                    MOVE "i"             TO SP2-FD-CTRL-TYPE
                    MOVE 18              TO SP2-FD-INITIAL-LEN
                    MOVE SPACES          TO SP2-FD-VAR-DATA
                    MOVE "cwdown.bmp"    TO SP2-FD-VAR-DATA (9: )
                    MOVE "f"             to SP2-FD-SPEC-FMT
                    MOVE "y"             TO SP2-FD-OUTPUT
                    MOVE X"01"           TO SP2-FD-TYPE
                    MOVE 1               TO SP2-FD-MAX-LEN
                    MOVE "y"             TO SP2-FD-CURS-SKIP
                    MOVE BUTTON-ON       TO SP2-FD-HELP-KEY
                    MOVE SP2-FD-ID       TO BUTTONS-ID
                    if  HIDE-PROT = 1
                        MOVE "g"         TO SP2-FD-OUTPUT
                    end-if
                    MOVE SP2-FD-INITIAL-LEN TO BUTTONS-LEN
                    MOVE SP2-FD-VAR-DATA TO BUTTONS-DATA
                    MOVE 1               TO BUTTONS-LIST
                    WRITE BUTTONS-REG
                    COMPUTE SP2-FD-FLD-NUM = FIELDS + objects
                    MOVE  SP2-FD-FLD-NUM TO SP2-FD-TAB-NUM
                    PERFORM 040-MAP-FIELD THRU 040-99-FIM
               WHEN CWOBJE-SCROLL
                    MOVE LOW-VALUES   TO SP2-FD-DATA
                                         SP2-FD-VAR-LENS
                    string 'CWOBJE-' CWOBJE-LINE CWOBJE-COLUMN
                         delimited by size
                        into sp2-fd-name
                    MOVE 1            TO VISUAL
                    MOVE ENTRY-ATTR   TO SP2-FD-COLR
                    CALL "CWSPID"  USING SP2-FD-ID "FInsert"
                    MOVE  SP2-FD-ID   TO objeto-id
                                         THUMBS-ID
                    COMPUTE SP2-FD-ROW = X - 1
                    COMPUTE SP2-FD-COL = Y - 1
                    MOVE CWOBJE-TYPE    TO OBJECT-CTRL
                    MOVE CWOBJE-OPTIONS TO SP2-FD-CTRL-TYPE
                    IF  CWOBJE-VERTICAL
                        COMPUTE SP2-FD-HEIGHT = CWOBJE-HEIGHT * 11
                        move     20  to SP2-FD-WIDTH
                        subtract 1 from SP2-FD-COL
                        ADD 2  to SP2-FD-HEIGHT
                    ELSE
                        COMPUTE SP2-FD-WIDTH = CWOBJE-WIDTH * 10
                        move 10 to SP2-FD-HEIGHT
                        add 1 to SP2-FD-ROW
                    END-IF
                    MOVE CURCOLOR         TO SP2-FD-CUR-COLR
                    MOVE "s"              TO SP2-FD-PROG-CTRL
                    MOVE SIZE-FIELDS      TO SP2-FD-PROG-OFF
                    ADD  5                TO SIZE-FIELDS
                    MOVE 5                TO SP2-FD-MAX-LEN
                                             SP2-FD-PROG-LEN
                                             SP2-FD-VAR-LEN
                                             SP2-FD-INITIAL-LEN
                    COMPUTE SP2-FD-FLD-NUM = FIELDS + objects
                    MOVE  SP2-FD-FLD-NUM  TO SP2-FD-TAB-NUM
                    SET ADDRESS THUMB     TO CWOBJE-THUMB
                    SET THUMBS-THUMB      TO CWOBJE-THUMB
                    COMPUTE THUMBS-POSITION = SP2-FD-PROG-OFF + 1
                    MOVE THUMB TO Buffer
                                  (THUMBS-POSITION:5)
                    MOVE CWOBJE-KEY  TO thumbs-key
                    WRITE THUMBS-REG
                    IF   CWOBJE-COLOR NUMERIC
                         MOVE CWOBJE-COLOR     TO color-num
                         MOVE color-num (1: 1) TO SP2-FD-COLR
                    END-IF
                    CALL SP2 USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           END-EVALUATE.

       271-EXIT-EVALUATE.

           IF FS-OBJWORK > '09'
              WRITE OBJWORJ-REG
           ELSE
              REWRITE OBJWORJ-REG
           END-IF.

       271-99-FIM. EXIT.

       272-FONT-COLOR.

           IF   CWOBJE-OLDFILE (1: 47) NOT = SPACES
                MOVE CWOBJE-OLDFILE(1: 47) TO PARAMETROS-CWFONT
                MOVE 0 TO CWFONT-REFERENCE
                CALL "CWFONT" USING PARAMETROS-CWFONT
                MOVE CWFONT-REFERENCE TO SP2-SD-FONT-ID
                                         SP2-FD-FONT-ID
                                         SP2-GD-FONT-ID
                IF   CWOBJE-FONT-WIDTH NUMERIC
                     COMPUTE SP2-SD-WIDTH =
                           ( SP2-SD-VAR-LEN + 2 )
                           * CWOBJE-FONT-WIDTH
                END-IF
                IF   CWOBJE-FONT-HEIGHT NUMERIC
                     COMPUTE SP2-SD-HEIGHT =
                          CWOBJE-FONT-HEIGHT / 2
                END-IF
           END-IF

           IF   CWOBJE-COLOR NUMERIC
                MOVE CWOBJE-COLOR TO color-num
                MOVE color-num (1: 1) TO SP2-SD-COLR
                                         SP2-FD-COLR
                                         SP2-GD-COLR
           END-IF.

       272-99-FIM. EXIT.

       300-CLEAR-OBJECTS.

           MOVE 0 TO objeto
           PERFORM objects  TIMES
                 ADD  1          TO objeto
                 MOVE pop-WINDOW TO OBJECT-WINDOW
                 MOVE objeto     TO OBJECT-SEQ
                 READ OBJWORK
                 IF  FS-OBJWORK < '09'
                 AND objeto-id NOT = 0
                   EVALUATE TRUE
                       WHEN OBJECT-CTRL = "s"
                            MOVE objeto-id TO thumbs-ID SP2-FD-ID
                            READ THUMBS
                            IF   FS-THUMBS < '10'
                                 DELETE THUMBS RECORD
                                 CALL "CWSPID" USING SP2-FD-ID "FDelete"
                                 CALL SP2 USING SP2-DELETE-FIELD
                                                SP2-FIELD-DEF
                            END-IF
                       WHEN OBJECT-CTRL = "F" or "f"
                            CALL SP2   USING SP2-DISPLAY-WINDOW
                                             SP2-NULL-PARM
                            MOVE LOW-VALUES         TO SP2-FD-DATA
                                                       SP2-FD-VAR-LENS
                            MOVE 2000               TO SP2-FD-VAR-LEN
                            MOVE objeto-id TO SP2-FD-ID
                            CALL SP2   USING SP2-GET-FIELD-DEF
                                             SP2-FIELD-DEF
                            CALL "CWSPID" USING SP2-FD-ID "FDelete"
                            IF SP2-FD-RET-CODE = 0
                               PERFORM 041-DROP-FIELD THRU 041-99-FIM
                            END-IF
                            CALL SP2   USING SP2-DISPLAY-WINDOW
                                             SP2-NULL-PARM
                       WHEN OBJECT-CTRL = "G"
                            MOVE objeto-id TO SP2-GD-ID
                            CALL "CWSPID" USING SP2-GD-ID "GDelete"
                            CALL SP2   USING SP2-GET-GROUP-DEF
                                             SP2-GROUP-DEF
                           *> compor grupo somente para imprimir tela
                            COMPUTE cwuser-LINE   = SP2-GD-ROW + 1
                            COMPUTE cwuser-COLUMN = SP2-GD-COL + 1
                            COMPUTE cwuser-WIDTH  = SP2-GD-WIDTH  / 10
                            COMPUTE cwuser-HEIGHT = SP2-GD-HEIGHT / 10
                            SET CWSCRE-DELETE-GROUP TO TRUE
                            PERFORM call-CWSCRE THRU FIM-call-CWSCRE
                            CALL SP2   USING SP2-DELETE-GROUP
                                             SP2-GROUP-DEF
                       WHEN OBJECT-CTRL = "S"
                            MOVE objeto-id TO SP2-SD-ID
                            CALL "CWSPID" USING SP2-SD-ID "SDelete"
                            CALL SP2   USING SP2-GET-STATIC-DEF
                                             SP2-STATIC-DEF
                            CALL SP2   USING SP2-DELETE-STATIC
                                             SP2-STATIC-DEF
                           *> compor texto somente para imprimir tela
                            COMPUTE cwuser-LINE   = SP2-SD-ROW + 1
                            COMPUTE cwuser-COLUMN = SP2-SD-COL + 1
                            MOVE SP2-SD-TEXT TO cwuser-CAPTION
                            INSPECT cwuser-CAPTION CONVERTING X"00"
                                                          TO SPACE
                            SET CWSCRE-DELETE-TEXT TO TRUE
                            PERFORM call-CWSCRE THRU FIM-call-CWSCRE
                   END-EVALUATE
                 END-IF
                 DELETE OBJWORK RECORD
           END-PERFORM
           MOVE 0 TO objects.

       300-99-FIM. EXIT.

       350-DISABLE-OBJECT.

           MOVE pop-WINDOW TO OBJECT-WINDOW
           PERFORM VARYING OBJDISA FROM 1 BY 1
                     UNTIL OBJDISA > objects
                   MOVE OBJDISA TO OBJECT-SEQ
                   READ OBJWORK
                   IF   FS-OBJWORK < '10'
                   AND  OBJECT-CTRL = "F"
                        MOVE "f"  TO OBJECT-CTRL
                        IF   (BUTTON = objeto-id
                        OR    SP2-CD-BUTTON-ID = objeto-id)
                        AND  TECLA = 0
                        AND  OBJECT-KEY > 0
                              MOVE OBJECT-KEY TO TECLA
                              PERFORM 216-CONVERTE-TECLA-SP2
                                 THRU 216-99-FIM
                              CALL "CWAKEY" USING TECLA MIL
                        END-IF
                        MOVE LOW-VALUES          TO SP2-FD-DATA
                                                    SP2-FD-VAR-LENS
                        MOVE objeto-id TO SP2-FD-ID
                        MOVE 2000                TO SP2-FD-VAR-LEN
                        CALL SP2   USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                        MOVE "g"                TO SP2-FD-OUTPUT
                        MOVE SP2-FD-INITIAL-LEN TO SP2-FD-VAR-LEN
                        CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
                   END-IF
           END-PERFORM

           IF   SAVED-SIZE = 1
                MOVE 0                  TO SAVED-SIZE
                MOVE PREOBJ-SIZE-FIELDS TO SIZE-FIELDS
                add  1                  to preobj-size-fields
                move spaces             to buffer (preobj-size-fields:)
                move 0                  to preobj-size-fields
           END-IF.

       350-99-FIM. EXIT.

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
                    move "@@@@" TO POS-K
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

       420-CRT-STATUS.

           MOVE X"32FFFF" TO CRT-STATUS
           EVALUATE SP2-CD-KEY
             WHEN SP2-KEY-CTRL-A         MOVE X"321901" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-B         MOVE X"320C02" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-C         MOVE X"330303" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-D         MOVE X"330404" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-E         MOVE X"330505" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-F         MOVE X"320D06" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-G         MOVE X"330707" TO CRT-STATUS
             WHEN SP2-KEY-BACKSPAC       MOVE X"320E08" TO CRT-STATUS
             WHEN SP2-KEY-TAB            MOVE X"320B09" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-J         MOVE X"330A0A" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-K         MOVE X"32010B" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-L         MOVE X"330C0C" TO CRT-STATUS
             WHEN SP2-KEY-ENTER          MOVE X"320B0D" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-N         MOVE X"330E0E" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-O         MOVE X"32100F" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-P         MOVE X"331010" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-Q         MOVE X"331111" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-R         MOVE X"321212" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-S         MOVE X"331313" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-T         MOVE X"331414" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-U         MOVE X"331515" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-V         MOVE X"331616" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-W         MOVE X"331717" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-X         MOVE X"321418" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-Y         MOVE X"320F19" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-Z         MOVE X"32131A" TO CRT-STATUS
             WHEN SP2-KEY-ESC            MOVE X"31001B" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-BACKSLASH MOVE X"331C1C" TO CRT-STATUS
             WHEN BUTTON-ON              MOVE X"331D1D" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-MINUS     MOVE X"390900" TO CRT-STATUS
             WHEN SP2-KEY-SPACEBAR       MOVE X"332020" TO CRT-STATUS
             WHEN SP2-KEY-BACKTAB        MOVE X"320C00" TO CRT-STATUS
             WHEN SP2-KEY-ALT-Q          MOVE X"315100" TO CRT-STATUS
             WHEN SP2-KEY-ALT-W          MOVE X"315700" TO CRT-STATUS
             WHEN SP2-KEY-ALT-E          MOVE X"314500" TO CRT-STATUS
             WHEN SP2-KEY-ALT-R          MOVE X"315200" TO CRT-STATUS
             WHEN SP2-KEY-ALT-T          MOVE X"315400" TO CRT-STATUS
             WHEN SP2-KEY-ALT-Y          MOVE X"315900" TO CRT-STATUS
             WHEN SP2-KEY-ALT-U          MOVE X"315500" TO CRT-STATUS
             WHEN SP2-KEY-ALT-I          MOVE X"314900" TO CRT-STATUS
             WHEN SP2-KEY-ALT-O          MOVE X"314F00" TO CRT-STATUS
             WHEN SP2-KEY-ALT-P          MOVE X"315000" TO CRT-STATUS
             WHEN SP2-KEY-ALT-A          MOVE X"314100" TO CRT-STATUS
             WHEN SP2-KEY-ALT-S          MOVE X"315300" TO CRT-STATUS
             WHEN SP2-KEY-ALT-D          MOVE X"314400" TO CRT-STATUS
             WHEN SP2-KEY-ALT-F          MOVE X"314600" TO CRT-STATUS
             WHEN SP2-KEY-ALT-G          MOVE X"314700" TO CRT-STATUS
             WHEN SP2-KEY-ALT-H          MOVE X"314800" TO CRT-STATUS
             WHEN SP2-KEY-ALT-J          MOVE X"314A00" TO CRT-STATUS
             WHEN SP2-KEY-ALT-K          MOVE X"314B00" TO CRT-STATUS
             WHEN SP2-KEY-ALT-L          MOVE X"314C00" TO CRT-STATUS
             WHEN SP2-KEY-ALT-Z          MOVE X"315A00" TO CRT-STATUS
             WHEN SP2-KEY-ALT-X          MOVE X"315800" TO CRT-STATUS
             WHEN SP2-KEY-ALT-C          MOVE X"314300" TO CRT-STATUS
             WHEN SP2-KEY-ALT-V          MOVE X"315600" TO CRT-STATUS
             WHEN SP2-KEY-ALT-B          MOVE X"314200" TO CRT-STATUS
             WHEN SP2-KEY-ALT-N          MOVE X"314E00" TO CRT-STATUS
             WHEN SP2-KEY-ALT-M          MOVE X"314D00" TO CRT-STATUS
             WHEN SP2-KEY-F1             MOVE X"310100" TO CRT-STATUS
             WHEN SP2-KEY-F2             MOVE X"310200" TO CRT-STATUS
             WHEN SP2-KEY-F3             MOVE X"310300" TO CRT-STATUS
             WHEN SP2-KEY-F4             MOVE X"310400" TO CRT-STATUS
             WHEN SP2-KEY-F5             MOVE X"310500" TO CRT-STATUS
             WHEN SP2-KEY-F6             MOVE X"310600" TO CRT-STATUS
             WHEN SP2-KEY-F7             MOVE X"310700" TO CRT-STATUS
             WHEN SP2-KEY-F8             MOVE X"310800" TO CRT-STATUS
             WHEN SP2-KEY-F9             MOVE X"310900" TO CRT-STATUS
             WHEN SP2-KEY-F10            MOVE X"310A00" TO CRT-STATUS
             WHEN SP2-KEY-HOME           MOVE X"320700" TO CRT-STATUS
             WHEN SP2-KEY-UP             MOVE X"320C00" TO CRT-STATUS
             WHEN SP2-KEY-PGUP           MOVE X"313500" TO CRT-STATUS
             WHEN SP2-KEY-LEFT           MOVE X"320300" TO CRT-STATUS
             WHEN SP2-KEY-RIGHT          MOVE X"320400" TO CRT-STATUS
             WHEN SP2-KEY-END            MOVE X"320A00" TO CRT-STATUS
             WHEN SP2-KEY-DOWN           MOVE X"320B00" TO CRT-STATUS
             WHEN SP2-KEY-PGDN           MOVE X"313600" TO CRT-STATUS
             WHEN SP2-KEY-INSERT         MOVE X"320700" TO CRT-STATUS
             WHEN SP2-KEY-DELETE         MOVE X"321100" TO CRT-STATUS
             WHEN SP2-KEY-SHIFT-F1       MOVE X"310B00" TO CRT-STATUS
             WHEN SP2-KEY-SHIFT-F2       MOVE X"310C00" TO CRT-STATUS
             WHEN SP2-KEY-SHIFT-F3       MOVE X"310D00" TO CRT-STATUS
             WHEN SP2-KEY-SHIFT-F4       MOVE X"310E00" TO CRT-STATUS
             WHEN SP2-KEY-SHIFT-F5       MOVE X"310F00" TO CRT-STATUS
             WHEN SP2-KEY-SHIFT-F6       MOVE X"311000" TO CRT-STATUS
             WHEN SP2-KEY-SHIFT-F7       MOVE X"311100" TO CRT-STATUS
             WHEN SP2-KEY-SHIFT-F8       MOVE X"311200" TO CRT-STATUS
             WHEN SP2-KEY-SHIFT-F9       MOVE X"311300" TO CRT-STATUS
             WHEN SP2-KEY-SHIFT-F10      MOVE X"311400" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-F1        MOVE X"311500" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-F2        MOVE X"311600" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-F3        MOVE X"311700" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-F4        MOVE X"311800" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-F5        MOVE X"311900" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-F6        MOVE X"311A00" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-F7        MOVE X"311B00" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-F8        MOVE X"311C00" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-F9        MOVE X"311D00" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-F10       MOVE X"311E00" TO CRT-STATUS
             WHEN SP2-KEY-ALT-F1         MOVE X"311F00" TO CRT-STATUS
             WHEN SP2-KEY-ALT-F2         MOVE X"312000" TO CRT-STATUS
             WHEN SP2-KEY-ALT-F3         MOVE X"312100" TO CRT-STATUS
             WHEN SP2-KEY-ALT-F4         MOVE X"312200" TO CRT-STATUS
             WHEN SP2-KEY-ALT-F5         MOVE X"312300" TO CRT-STATUS
             WHEN SP2-KEY-ALT-F6         MOVE X"312400" TO CRT-STATUS
             WHEN SP2-KEY-ALT-F7         MOVE X"312500" TO CRT-STATUS
             WHEN SP2-KEY-ALT-F8         MOVE X"312600" TO CRT-STATUS
             WHEN SP2-KEY-ALT-F9         MOVE X"312700" TO CRT-STATUS
             WHEN SP2-KEY-ALT-F10        MOVE X"312800" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-PRTSC     MOVE X"390900" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-LEFT      MOVE X"320900" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-RIGHT     MOVE X"320800" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-END       MOVE X"321400" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-PGDN      MOVE X"313800" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-HOME      MOVE X"321600" TO CRT-STATUS
             WHEN SP2-KEY-ALT-1          MOVE X"312900" TO CRT-STATUS
             WHEN SP2-KEY-ALT-2          MOVE X"312A00" TO CRT-STATUS
             WHEN SP2-KEY-ALT-3          MOVE X"312B00" TO CRT-STATUS
             WHEN SP2-KEY-ALT-4          MOVE X"312C00" TO CRT-STATUS
             WHEN SP2-KEY-ALT-5          MOVE X"312D00" TO CRT-STATUS
             WHEN SP2-KEY-ALT-6          MOVE X"312E00" TO CRT-STATUS
             WHEN SP2-KEY-ALT-7          MOVE X"312F00" TO CRT-STATUS
             WHEN SP2-KEY-ALT-8          MOVE X"313000" TO CRT-STATUS
             WHEN SP2-KEY-ALT-9          MOVE X"313100" TO CRT-STATUS
             WHEN SP2-KEY-ALT-0          MOVE X"313200" TO CRT-STATUS
             WHEN SP2-KEY-ALT-MINUS      MOVE X"313300" TO CRT-STATUS
             WHEN SP2-KEY-ALT-EQUAL      MOVE X"313400" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-PGUP      MOVE X"313700" TO CRT-STATUS
             WHEN SP2-KEY-F11            MOVE X"315B00" TO CRT-STATUS
             WHEN SP2-KEY-F12            MOVE X"315C00" TO CRT-STATUS
             WHEN SP2-KEY-SHIFT-F11      MOVE X"315D00" TO CRT-STATUS
             WHEN SP2-KEY-SHIFT-F12      MOVE X"315E00" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-F11       MOVE X"315F00" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-F12       MOVE X"316000" TO CRT-STATUS
             WHEN SP2-KEY-ALT-F11        MOVE X"316100" TO CRT-STATUS
             WHEN SP2-KEY-ALT-F12        MOVE X"316200" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-UP        MOVE X"390900" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-DOWN      MOVE X"390900" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-INSERT    MOVE X"390900" TO CRT-STATUS
             WHEN SP2-KEY-CTRL-DELETE    MOVE X"390900" TO CRT-STATUS
             WHEN BUTTON-ON              MOVE X"390900" TO CRT-STATUS
           END-EVALUATE
           CALL "CWCRTS" USING "S" CRT-STATUS.

       420-99-FIM. EXIT.

       430-GET-DATANAMES.

           MOVE SP2-FD-ID TO DATANAMES-ID
           READ DATANAMES
           IF  FS-DATANAMES < "10"
               MOVE DATANAMES-VAR-DATA    TO SP2-FD-VAR-DATA
               MOVE DATANAMES-INITIAL-LEN TO SP2-FD-INITIAL-LEN
               MOVE DATANAMES-VAR-LEN     TO SP2-FD-VAR-LEN
               MOVE DATANAMES-FORMAT-LEN  TO SP2-FD-FORMAT-LEN
           END-IF.

       430-99-FIM. EXIT.

       500-OPEN-LISTBOX.

           MOVE objects               TO WORKBOX-OBJECT
           READ WORKBOX
                IF   FS-WORKBOX < "10"
                     DELETE WORKBOX RECORD
                END-IF
           INITIALIZE WORKBOX-REG
           MOVE objects                  TO WORKBOX-OBJECT
           MOVE CWOBJE-PROGRAM           TO WORKBOX-PROGRAM
           MOVE CWOBJE-ORDER             TO WORKBOX-ORDER
           MOVE CWOBJE-VERTICAL-LENGTH   TO WORKBOX-VERTICAL-LENGTH
           MOVE CWOBJE-HORIZONTAL-LENGTH TO WORKBOX-HORIZONTAL-LENGTH
           MOVE CWOBJE-STRING-1-LENGTH   TO WORKBOX-STRING-1-LENGTH
           MOVE CWOBJE-STRING-2-LENGTH   TO WORKBOX-STRING-2-LENGTH
           MOVE CWOBJE-LINE              TO WORKBOX-LINE
           MOVE CWOBJE-COLUMN            TO WORKBOX-COLUMN
           MOVE CWOBJE-WORK-AREA         TO WORKBOX-WORK-AREA
           MOVE CWOBJE-RETURN            TO WORKBOX-RETURN
           perform until WORKBOX-LINE  < 2
             or (WORKBOX-LINE  + WORKBOX-VERTICAL-LENGTH) < (janhei + 2)
                       subtract 1 from WORKBOX-LINE
           end-perform

           IF   DISPLAY-ACCEPT = 1
                GO TO 500-99-FIM
           END-IF

           INITIALIZE HIDE-REG
           MOVE CWOBJE-OPTION            TO HIDE-DATANAME
                                            WORKBOX-HIDE
           MOVE pop-WINDOW               TO HIDE-WINDOW
           READ HIDE
           IF   FS-HIDE < "10"
           AND (HIDE-FIELD NOT = 0)
                MOVE DATA-LK (HIDE-FIELD) TO WORKBOX-OPTION
                IF  PIC-LK (HIDE-FIELD)(1:1) = "Z"
                    INSPECT WORKBOX-OPTION  (1: LEN-LK(HIDE-FIELD))
                            CONVERTING SPACE TO "0"
                END-IF
                MOVE HIDE-GUIA-IND       TO WORKBOX-GUIA
                MOVE HIDE-FIELD          TO WORKBOX-FIELD
           ELSE
                MOVE SPACES              TO WORKBOX-OPTION
           END-IF
           MOVE ">"                      TO FIM
           COMPUTE UM = SIZE-FIELDS + 1

           IF   WORKBOX-LINE = 0
           OR   WORKBOX-LINE NOT NUMERIC
                MOVE 1 TO WORKBOX-LINE
           END-IF

           IF   WORKBOX-COLUMN = 0
           OR   WORKBOX-COLUMN NOT NUMERIC
                MOVE 1 TO WORKBOX-COLUMN
           END-IF

           IF   WORKBOX-VERTICAL-LENGTH = 0
           OR   WORKBOX-VERTICAL-LENGTH NOT NUMERIC
                MOVE 1 TO WORKBOX-VERTICAL-LENGTH
           END-IF

           IF   WORKBOX-HORIZONTAL-LENGTH = 0
           OR   WORKBOX-HORIZONTAL-LENGTH NOT NUMERIC
                MOVE 10 TO WORKBOX-HORIZONTAL-LENGTH
           END-IF
           MOVE 1 TO SEM-DADOS

           COMPUTE ITEM-LEN = WORKBOX-STRING-1-LENGTH
                            + WORKBOX-STRING-2-LENGTH
           IF   ITEM-LEN > WORKBOX-HORIZONTAL-LENGTH
                MOVE ITEM-LEN TO WORKBOX-HORIZONTAL-LENGTH
           END-IF

           IF  WORKBOX-VERTICAL-LENGTH > (JANHEI + 2)
           OR  (WORKBOX-COLUMN + WORKBOX-HORIZONTAL-LENGTH) > JANWID
               MOVE "Erro de limites no uso do box" TO MSG-D
               PERFORM SEND-SP2
           END-IF

           IF   WORKBOX-STRING-1-LENGTH = 0
                MOVE WORKBOX-HORIZONTAL-LENGTH
                  TO WORKBOX-STRING-2-LENGTH
           ELSE
                IF   WORKBOX-STRING-2-LENGTH = 0
                     MOVE WORKBOX-HORIZONTAL-LENGTH
                       TO WORKBOX-STRING-1-LENGTH
                END-IF
           END-IF

           COMPUTE COL-1 = WORKBOX-COLUMN - 1
           MOVE 10 TO WD
           MOVE 0  TO PLUS-W
           IF   WORKBOX-STRING-2-LENGTH = 0
                MOVE 1 TO RESTO
           ELSE
                MOVE WORKBOX-STRING-2-LENGTH TO RESTO
                IF   CWOBJE-STRING-1-LENGTH > 0
                     MOVE 30 TO PLUS-W
                END-IF
           END-IF
           COMPUTE COL-2 = COL-1 + WORKBOX-STRING-1-LENGTH
           COMPUTE COL-B = COL-2 + RESTO - 1
           COMPUTE WID-1 = (COL-2 - COL-1 - 1) * WD
           COMPUTE WID-2 = (COL-B - COL-2 - 1) * WD
           COMPUTE WID-G = WID-1 + WID-2 + 15 + PLUS-W
           IF   WORKBOX-STRING-2-LENGTH = 0
                ADD WD TO WID-1
           END-IF
           IF   CWOBJE-STRING-1-LENGTH > 0
           AND  CWOBJE-STRING-2-LENGTH > 0
                SUBTRACT 1 FROM col-2
                SUBTRACT 2 FROM col-b
                add plus-w   to wid-2
           ELSE
                IF   WORKBOX-STRING-2-LENGTH > 0
                     ADD 1      TO col-b
                     ADD 20     TO wid-2
                     ADD 10     TO WID-G
                END-IF
           END-IF

           PERFORM 510-CALL-PROVIDER THRU 510-99-FIM

           PERFORM 605-INICIO-BOXWRK THRU 605-99-FIM

           MOVE ">" TO FIM
           ADD  1   TO C
           INITIALIZE LISTAS

           MOVE 1 TO GAP2
           IF  WORKBOX-STRING-1-LENGTH NOT = 0
               ADD WORKBOX-STRING-1-LENGTH TO GAP2
               IF  WORKBOX-STRING-2-LENGTH NOT = 0
                   ADD 1 TO GAP2
               END-IF
           END-IF

           IF   WORKBOX-ORDER = 0
                MOVE 1 TO WORKBOX-ORDER
           END-IF
           MOVE WORKBOX-COLUMN  TO C

           IF   HIDE-LIST = 1
                MOVE CAMPOS-GUIA         TO SAVE-GUIA
                MOVE pop-WINDOW          TO CAMPOS-JANELA
                MOVE HIDE-DATANAME       TO DATANAME-E
                MOVE 1                   TO campos-subscript
                move HIDE-POS            TO POS-E
                READ CAMPOS KEY IS CAMPOS-NAME
                IF   FS-CAMPOS < '10'
                     MOVE LOW-VALUES         TO SP2-FD-DATA
                     MOVE CAMPOS-FD-ID       TO SP2-FD-ID
                     MOVE 2000               TO SP2-FD-VAR-LEN
                     CALL SP2             USING SP2-GET-FIELD-DEF
                                                SP2-FIELD-DEF
                     MOVE "h"                TO SP2-FD-OUTPUT
                     MOVE SP2-FD-INITIAL-LEN TO SP2-FD-VAR-LEN
                     CALL SP2             USING SP2-SET-FIELD-DEF
                                                SP2-FIELD-DEF
                END-IF
                MOVE SAVE-GUIA      TO CAMPOS-GUIA
                READ CAMPOS KEY IS CAMPOS-JANELA-GUIA
                MOVE LOW-VALUES     TO SP2-GD-DATA
                move CWOBJE-PROGRAM to sp2-gd-name
                MOVE X"01"          TO SP2-GD-COLR
                CALL "CWSPID"    USING SP2-GD-ID "GInsert"
                MOVE SP2-GD-ID      TO GD-ID
                COMPUTE SP2-GD-ROW = WORKBOX-LINE - 1
                MOVE COL-1          TO SP2-GD-COL
                MOVE WID-G          TO SP2-GD-WIDTH
                MOVE "3"            TO SP2-GD-BOR-TYPE
                COMPUTE SP2-GD-HEIGHT = WORKBOX-VERTICAL-LENGTH * 10
                CALL SP2   USING SP2-SET-GROUP-DEF SP2-GROUP-DEF
           ELSE
                MOVE WINWORK-WINDOW            TO CAMPOS-JANELA
                MOVE HIDE-DATANAME             TO DATANAME-E
                MOVE 1                         TO campos-subscript
                move HIDE-POS                  TO POS-E
                READ CAMPOS KEY IS CAMPOS-NAME
                MOVE CAMPOS-GUIA TO CURSOR-GUIA
                READ CAMPOS KEY IS CAMPOS-JANELA-GUIA
                MOVE LOW-VALUES                TO SP2-WD-DATA
                CALL SP2                    USING SP2-GET-WINDOW-DEF
                                                  SP2-WINDOW-DEF
                MOVE SP2-WD-WINDOW-ID          TO CORRENTE-ID
                MOVE SP2-WD-NAME               TO CORRENTE
                MOVE LOW-VALUES                TO SP2-WD-DATA
                MOVE 's'                       TO SP2-WD-BOR-TYPE
                MOVE "CWBOXF"                  TO SP2-WD-NAME
                                                  SP2-WD-PANEL-NAME
                MOVE WORKBOX-LINE              TO SP2-WD-ROW
                ADD  1                         TO SP2-WD-ROW
                MOVE WORKBOX-COLUMN            TO SP2-WD-COL
                SUBTRACT 2                   FROM SP2-WD-ROW
                SUBTRACT 1                   FROM SP2-WD-COL
                MULTIPLY 5                     BY SP2-WD-ROW
                MULTIPLY 5                     BY SP2-WD-COL
                MOVE 10                        to sp2-WD-CELL-WIDTH
                MOVE 16                        to SP2-WD-CELL-HEIGHT
                MOVE WORKBOX-HORIZONTAL-LENGTH TO SP2-WD-WIDTH
                ADD  2                         TO SP2-WD-WIDTH
                MOVE WORKBOX-VERTICAL-LENGTH   TO SP2-WD-HEIGHT
                MOVE CORRENTE-ID               TO SP2-WD-OWNR-ID
                CALL SP2            USING SP2-OPEN-WINDOW
                                          SP2-WINDOW-DEF
                MOVE LOW-VALUES        TO SP2-PD-DATA
                MOVE X'80'             TO SP2-PD-MORE-OPTIONS
                MOVE X'20'             TO SP2-PD-OPTIONS-3
                MOVE SP2-WD-WIDTH      TO SP2-PD-WIDTH
                MOVE SP2-WD-HEIGHT     TO SP2-PD-HEIGHT
                MOVE 0                 TO SP2-PD-ROW
                MOVE 0                 TO SP2-PD-COL
                MOVE "CWBOXF"          TO SP2-PD-NAME
                MOVE "n"               TO SP2-PD-CURS-SHOW
                MOVE SP2-KEY-HOME      TO SP2-PD-CTRL-KEY (01)
                MOVE SP2-KEY-END       TO SP2-PD-CTRL-KEY (02)
                MOVE SP2-KEY-DOWN      TO SP2-PD-CTRL-KEY (03)
                MOVE SP2-KEY-UP        TO SP2-PD-CTRL-KEY (04)
                MOVE SP2-KEY-CTRL-PGUP TO SP2-PD-CTRL-KEY (05)
                MOVE SP2-KEY-CTRL-PGDN TO SP2-PD-CTRL-KEY (06)
                MOVE SP2-KEY-PGUP      TO SP2-PD-CTRL-KEY (07)
                MOVE SP2-KEY-PGDN      TO SP2-PD-CTRL-KEY (08)
                MOVE SP2-KEY-TAB       TO SP2-PD-CTRL-KEY (09)
                MOVE SP2-KEY-BACKTAB   TO SP2-PD-CTRL-KEY (10)
                MOVE SP2-KEY-ENTER     TO SP2-PD-CTRL-KEY (11)
                MOVE SP2-KEY-ESC       TO SP2-PD-CTRL-KEY (12)
                MOVE SP2-KEY-TIMEOUT   TO SP2-PD-CTRL-KEY (13)
                MOVE SP2-KEY-RIGHT     TO SP2-PD-CTRL-KEY (14)
                MOVE SP2-KEY-BACKSPAC  TO SP2-PD-CTRL-KEY (15)
                MOVE SP2-KEY-DELETE    TO SP2-PD-CTRL-KEY (16)
                MOVE SP2-KEY-INSERT    TO SP2-PD-CTRL-KEY (17)
                MOVE SP2-KEY-CTRL-HOME TO SP2-PD-CTRL-KEY (18)
                MOVE SP2-KEY-CTRL-END  TO SP2-PD-CTRL-KEY (19)
                MOVE SP2-KEY-LEFT      TO SP2-PD-CTRL-KEY (20)
                CALL SP2            USING SP2-SET-PANEL-DEF
                                          SP2-PANEL-DEF
                move 'y'               to sp2-cd-switch-sw
                MOVE 'CWBOXF'          TO SP2-ND-NAME
                CALL SP2 USING SP2-ACTIVATE-WINDOW SP2-NAME-DEF
           END-IF

           IF   WORKBOX-STRING-1-LENGTH NOT = 0
                MOVE 1                       TO STR
                MOVE 0                       TO L
                MOVE WORKBOX-STRING-1-LENGTH TO STR-LEN
                                                ST1
                PERFORM 651-CREATE-FIELD WORKBOX-VERTICAL-LENGTH TIMES
           END-IF

           IF   WORKBOX-STRING-2-LENGTH NOT = 0
                MOVE 2                       TO STR
                MOVE 0                       TO L
                MOVE WORKBOX-STRING-2-LENGTH TO STR-LEN
                MOVE STR-LEN                 TO ST2
                PERFORM 651-CREATE-FIELD WORKBOX-VERTICAL-LENGTH TIMES
           END-IF

           MOVE LOW-VALUES       TO SP2-FD-DATA
                                    SP2-FD-VAR-LENS
           MOVE 'CWBOXF-BARR'    TO SP2-FD-NAME
           MOVE 0                TO SP2-FD-VAR-LEN
           ADD  1                TO FIELDX
           MOVE FIELDX           TO SP2-FD-FLD-NUM
                                    SP2-FD-TAB-NUM
           CALL "CWSPID"      USING SP2-FD-ID "FInsert"
           INITIALIZE LISTBOX-REG
           MOVE SP2-FD-ID        TO FD-ID (FIELDX)
                                    BARR-ID
                                    LISTBOX-ID
           MOVE objects          TO LISTBOX-OBJECT
           MOVE 0                TO LISTBOX-M
           MOVE 0                TO LISTBOX-KEY
           WRITE LISTBOX-REG
           COMPUTE SP2-FD-HEIGHT  = WORKBOX-VERTICAL-LENGTH * 10
           IF   HIDE-LIST = 1
                COMPUTE SP2-FD-ROW = WORKBOX-LINE - 1
                MOVE COL-B                         TO SP2-FD-COL
           ELSE
                MOVE WORKBOX-HORIZONTAL-LENGTH     TO SP2-FD-COL
                IF  (WORKBOX-STRING-1-LENGTH NOT = 0)
                AND (WORKBOX-STRING-2-LENGTH NOT = 0)
                    SUBTRACT 3 FROM SP2-FD-COL
                END-IF
           END-IF
           IF   PLUS-W = 30
                ADD 3 TO SP2-FD-COL
           END-IF
           MOVE 20                    TO SP2-FD-WIDTH
           MOVE "v"                   TO SP2-FD-CTRL-TYPE
           MOVE "s"                   TO SP2-FD-PROG-CTRL
           MOVE SIZE-FIELDS           TO SP2-FD-PROG-OFF
           COMPUTE BARRPOS = SIZE-FIELDS + 1
           ADD  5                     TO SIZE-FIELDS
           MOVE 5                     TO SP2-FD-MAX-LEN
                                         SP2-FD-PROG-LEN
           PERFORM 652-FIELD-BOX    THRU 652-99-FIM
           PERFORM 610-MONTA-cwboxf THRU 610-99-FIM
           MOVE    1                  TO M
           IF  WORKBOX-ORDER < 2
               MOVE LISTA (M) TO SP2-FD-ID
           ELSE
               MOVE LISTA2(M) TO SP2-FD-ID
           END-IF
           MOVE 2000     TO SP2-FD-VAR-LEN
           CALL SP2   USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
           COMPUTE OFF-W = SP2-FD-PROG-OFF + 1
           IF   WORKBOX-ORDER = 1
                MOVE WORKBOX-STRING-1-LENGTH TO END-W
           ELSE
                MOVE WORKBOX-STRING-2-LENGTH TO END-W
           END-IF
           IF   WORKBOX-OPTION NOT = SPACES
                IF   WORKBOX-ORDER NOT = WORKBOX-RETURN
                     MOVE WORKBOX-OPTION TO STRING-1X
                                            STRING-2X
                     MOVE SPACES TO WORKBOX-OPTION
                     MOVE ">" TO IO
                     CALL WORKBOX-PROGRAM USING IO
                                                WORKBOX-RETURN
                                                STRING-1X
                                                STRING-2X
                                                WORKBOX-VERTICAL-LENGTH
                                                WORKBOX-WORK-AREA
                     IF IO NOT = '*'
                        MOVE "N" TO IO
                        CALL WORKBOX-PROGRAM USING
                                             IO
                                             WORKBOX-RETURN
                                             STRING-1X
                                             STRING-2X
                                             WORKBOX-VERTICAL-LENGTH
                                             WORKBOX-WORK-AREA
                        IF IO <> '*'
                           IF   WORKBOX-RETURN = 1
                                MOVE STRING-1X TO WORKBOX-OPTION
                           ELSE
                                MOVE STRING-2X TO WORKBOX-OPTION
                           END-IF
                        END-IF
                     END-IF
                END-IF
                MOVE WORKBOX-OPTION  TO buffer
                                        (OFF-W: END-W)
           END-IF
           PERFORM 660-FIND-CURSOR THRU 660-99-FIM
           IF   WORKBOX-OPTION = SPACES
                MOVE 0 TO SELECTED
           ELSE
                MOVE 1 TO SELECTED
           END-IF
           PERFORM 600-BARRA       THRU 600-99-FIM
           WRITE LISTBOX-REG
           WRITE WORKBOX-REG.

       500-99-FIM. EXIT.

       510-CALL-PROVIDER.

           CALL WORKBOX-PROGRAM USING "O" WORKBOX-ORDER
                                          STRING-1
                                          STRING-2
                                          WORKBOX-VERTICAL-LENGTH
                                          WORKBOX-WORK-AREA
                ON EXCEPTION
                   MOVE SPACES TO console-msg
                   STRING "Imposs¡vel executar " WORKBOX-PROGRAM
                                                 DELIMITED BY SIZE
                               INTO console-msg
                   MOVE console-msg TO MSG-D
                   PERFORM SEND-SP2
           END-CALL.

       510-99-FIM. EXIT.

       600-SELECT-LISTBOX.

           IF  WORKBOX-HIDE NOT = HIDE-DATANAME
               MOVE WORKBOX-HIDE TO HIDE-DATANAME
               MOVE pop-WINDOW   TO HIDE-WINDOW
               READ HIDE
           END-IF

           MOVE X"01"     TO OK
           PERFORM 655-CURSOR-BARRA THRU 655-99-FIM
           MOVE    0 TO SELECTED

           IF  SEM-DADOS = 1
               GO TO 600-FIM-SELECT
           END-IF

           IF   LISTBOX-KEY = 1
                MOVE WORKBOX-POSIT (WORKBOX-ORDER LISTBOX-M) TO OFF-W
                IF   WORKBOX-ORDER = 1
                     MOVE WORKBOX-STRING-1-LENGTH TO END-W
                ELSE
                     MOVE WORKBOX-STRING-2-LENGTH TO END-W
                END-IF
                IF  buffer(OFF-W:END-W)
                  NOT = save-buffer(OFF-W:END-W)
                    move 1         to m
                    MOVE buffer(OFF-W:END-W) TO TEXTO-A
                    MOVE SPACES TO TEXTO-A (T + 1: )
                    MOVE TEXTO-A TO buffer(OFF-W:END-W)
                    PERFORM 660-FIND-CURSOR THRU 660-99-FIM
                    GO TO 600-BARRA
                END-IF
           ELSE
                PERFORM 602-VERIFICA-BARRA THRU 602-99-FIM
           END-IF

           MOVE 0 TO TECLA-EDIT
           EVALUATE SP2-CD-KEY
               WHEN SP2-KEY-CLOSE
                    MOVE SP2-KEY-ESC           TO SP2-CD-KEY
               WHEN SP2-KEY-HOME
                    SET EDIT-HOME              TO TRUE
               WHEN SP2-KEY-END
                    SET EDIT-END               TO TRUE
               WHEN SP2-KEY-DOWN
                    SET EDIT-CURSOR-DOWN       TO TRUE
               WHEN SP2-KEY-TAB
                    SET EDIT-CURSOR-DOWN       TO TRUE
               WHEN SP2-KEY-UP
                    SET EDIT-CURSOR-UP         TO TRUE
               WHEN SP2-KEY-BACKTAB
                    SET EDIT-CURSOR-UP         TO TRUE
               WHEN SP2-KEY-CTRL-PGUP
                    SET EDIT-CONTROL-PAGE-UP   TO TRUE
               WHEN SP2-KEY-CTRL-PGDN
                    SET EDIT-CONTROL-PAGE-DOWN TO TRUE
               WHEN SP2-KEY-PGUP
                    SET EDIT-PAGE-UP           TO TRUE
               WHEN SP2-KEY-PGDN
                    SET EDIT-PAGE-DOWN         TO TRUE
               WHEN SP2-KEY-ENTER
                    CONTINUE
               WHEN OTHER
                    IF  SP2-CD-KEY NOT = 0
                    AND (SP2-CD-KEY NOT = SP2-KEY-LEFT)
                    AND (SP2-CD-KEY NOT = SP2-KEY-RIGHT)
                        MOVE SP2-CD-KEY    TO KEY-CWBOXF
                        MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                        MOVE 1             TO KEY-EXIT
                    END-IF
           END-EVALUATE

           IF  TECLA-EDIT NOT = 0
               PERFORM 653-ROLAGEM THRU 653-99-FIM
           END-IF.

       600-FIM-SELECT.

           MOVE 0 TO HIDE-STRING
           IF ( SP2-CD-KEY = SP2-KEY-ENTER
           OR   GET-KEY = SP2-KEY-ENTER )
           AND (WORKBOX-FIELD NOT = 0)
                PERFORM VARYING M
                             FROM WORKBOX-VERTICAL-LENGTH BY -1
                            UNTIL M = 0
                               OR LISTA  (M) = SP2-CD-NEXT-FLD-ID
                               OR LISTA2 (M) = SP2-CD-NEXT-FLD-ID
                        CONTINUE
                END-PERFORM
                IF  M > 0
                    IF  SP2-CD-NEXT-FLD-ID = LISTA (M)
                                          OR LISTA2(M)
                        MOVE LEN-LK (HIDE-FIELD) TO P
                        EVALUATE TRUE
                            WHEN WORKBOX-RETURN < 2
                                 MOVE 1 TO HIDE-STRING
                                 if   TEXTO-1 (M)           (1: P)
                                 not = WORKBOX-OPTION (1: P)
                                    move 0 to key-exit
                                 end-if
                                 MOVE TEXTO-1 (M)          (1: P)
                                   TO DATA-LK (HIDE-FIELD) (1: P)
                                                    WORKBOX-OPTION
                                 IF  HIDE-ALPHA = 2
                                     MOVE TEXTO-2 (M)
                                       TO DATA-LK (HIDE-FIELD)
                                 END-IF
                                 IF   CWACCENT = "OFF"
                                      INSPECT DATA-LK (HIDE-FIELD)
                                          (1: P)
                                          CONVERTING ACENTOS-WINDOWS
                                                  TO ACENTOS-OFF
                                 ELSE
                                     IF   MFF NOT = "MF"
                                          INSPECT DATA-LK (HIDE-FIELD)
                                          (1: P)
                                          CONVERTING ACENTOS-WINDOWS
                                                  TO ACENTOS-850
                                     ELSE
                                          INSPECT DATA-LK (HIDE-FIELD)
                                          (1: P)
                                          CONVERTING ACENTOS-437
                                                  TO ACENTOS-850
                                     END-IF
                                 END-IF
                                 MOVE DATA-LK (HIDE-FIELD) (1: P)
                                   TO               DATA-E (1: P)
                                      HIDE-STRING-DATA
                                      HIDE-STRING-SHOW
                                 IF  (HIDE-POS NOT = 0)
                                 AND  (SP2-CD-KEY NOT = SP2-KEY-ENTER)
                                      MOVE TEXTO-L (M) TO buffer
                                                          (HIDE-POS: P)
                                                       HIDE-STRING-SHOW
                                 ELSE
                                      IF  (HIDE-POS2 NOT = 0)
                                      AND  GET-KEY = SP2-KEY-ENTER
                                           MOVE TEXTO-L (M)
                                             TO SUPER-TEXTO
                                           MOVE HIDE-POS2 TO SUPER-POS
                                           MOVE P         TO SUPER-SIZE
                                      END-IF
                                 END-IF
                            WHEN WORKBOX-RETURN > 1
                                 MOVE 2 TO HIDE-STRING
                                 if   TEXTO-2 (M)           (1: P)
                                 not = WORKBOX-OPTION (1: P)
                                    move 0 to key-exit
                                 end-if
                                 MOVE TEXTO-2 (M)          (1: P)
                                   TO DATA-LK (HIDE-FIELD) (1: P)
                                                    WORKBOX-OPTION
                                                    HIDE-STRING-DATA
                                 IF  HIDE-ALPHA = 1
                                     MOVE TEXTO-1 (M)
                                       TO DATA-LK (HIDE-FIELD)
                                 END-IF
                                 IF   CWACCENT = "OFF"
                                      INSPECT DATA-LK (HIDE-FIELD)
                                          (1: P)
                                          CONVERTING ACENTOS-WINDOWS
                                                  TO ACENTOS-OFF
                                 ELSE
                                     IF   MFF = "MF"
                                          INSPECT DATA-LK (HIDE-FIELD)
                                          (1: P)
                                          CONVERTING ACENTOS-437
                                                  TO ACENTOS-850
                                     ELSE
                                          INSPECT DATA-LK (HIDE-FIELD)
                                          (1: P)
                                          CONVERTING ACENTOS-WINDOWS
                                                  TO ACENTOS-850
                                     END-IF
                                 END-IF
                                 MOVE DATA-LK (HIDE-FIELD) (1: P)
                                   TO               DATA-E (1: P)
                                 IF   (HIDE-POS NOT = 0)
                                 AND  (SP2-CD-KEY NOT = SP2-KEY-ENTER)
                                      MOVE TEXTO-L (M) TO buffer
                                                          (HIDE-POS: P)
                                 ELSE
                                      IF  (HIDE-POS2 NOT = 0)
                                      AND  GET-KEY = SP2-KEY-ENTER
                                           MOVE TEXTO-L (M)
                                             TO SUPER-TEXTO
                                           MOVE HIDE-POS2 TO SUPER-POS
                                           MOVE P         TO SUPER-SIZE
                                      END-IF
                                 END-IF
                        END-EVALUATE
                        MOVE WORKBOX-HORIZONTAL-LENGTH TO P
                        MOVE FIELD                     TO FIELD-ANT
                        MOVE HIDE-FIELD                TO FIELD
                        MOVE LIN-E                     TO X
                        MOVE COL-E                     TO Y
                        MOVE LEN-E                     TO Z
                        MOVE LIN-LK (HIDE-FIELD)       TO LIN-E
                        MOVE COL-LK (HIDE-FIELD)       TO COL-E
                        MOVE WORKBOX-HORIZONTAL-LENGTH TO LEN-E
                        MOVE X                         TO LIN-E
                        MOVE Y                         TO COL-E
                        MOVE Z                         TO LEN-E
                        MOVE FIELD-ANT                 TO FIELD
                    END-IF
                END-IF
                PERFORM 220-CLOSE-COMBO THRU 220-99-FIM
           END-IF
           REWRITE HIDE-REG.

       600-BARRA.

           IF   WORKBOX-OBJECT = 0
                GO TO 600-END
           END-IF

           MOVE LEN-LK (HIDE-FIELD) TO P

           IF   DATA-LK (HIDE-FIELD) (1: P) NOT = SPACES
                PERFORM VARYING M
                             FROM WORKBOX-VERTICAL-LENGTH BY -1
                           UNTIL M = 0
                EVALUATE TRUE
                    WHEN WORKBOX-RETURN < 2
                         MOVE TEXTO-1 (M) (1:P) TO TEXTO-T
                    WHEN WORKBOX-RETURN > 1
                         MOVE TEXTO-2 (M) (1:P) TO TEXTO-T
                END-EVALUATE
                IF   CWACCENT = "OFF"
                     INSPECT TEXTO-T (1: P)
                                  CONVERTING ACENTOS-WINDOWS
                                          TO ACENTOS-OFF
                ELSE
                     IF   MFF = "MF"
                          INSPECT TEXTO-T (1: P)
                                       CONVERTING ACENTOS-437
                                               TO ACENTOS-850
                     ELSE
                          INSPECT TEXTO-T (1: P)
                                       CONVERTING ACENTOS-WINDOWS
                                               TO ACENTOS-850
                     END-IF
                END-IF
                IF TEXTO-T (1:P) = DATA-LK (HIDE-FIELD) (1:P)
                   MOVE M TO SELECTED
                   EXIT PERFORM
                END-IF
                END-PERFORM
           END-IF

           IF   COMBO = 1
           AND  SP2-CD-NEXT-FLD-ID = 0
                IF   M = 0
                     MOVE 1 TO M
                END-IF
                IF   WORKBOX-ORDER  < 2
                     MOVE LISTA  (M) TO SP2-CD-NEXT-FLD-ID LAST-ID
                ELSE
                     MOVE LISTA2 (M) TO SP2-CD-NEXT-FLD-ID LAST-ID
                END-IF
           END-IF

           MOVE X"97"     TO OK
           PERFORM 655-CURSOR-BARRA THRU 655-99-FIM.

       600-END.

           PERFORM DD THRU FIM-DD.

       600-99-FIM. EXIT.

       602-VERIFICA-BARRA.

           IF ((buffer (BARRPOS: 5) NOT = "00050")
           OR   SP2-CD-NEXT-FLD-ID = BARR-ID)
           AND (SP2-CD-KEY NOT = SP2-KEY-ESC)
                IF   buffer (BARRPOS: 5) = "00050"
                AND  BARRA = 51
                     MOVE "00049" TO buffer (BARRPOS: 5)
                ELSE
                     IF  buffer (BARRPOS: 5) = "00050"
                     AND BARRA = 49
                         MOVE "00051" TO buffer(BARRPOS:5)
                    END-IF
                END-IF
                IF  SP2-CD-NEXT-FLD-ID = BARR-ID
                    MOVE LAST-ID TO SP2-CD-NEXT-FLD-ID
                END-IF
                MOVE 0                       TO SP2-CD-KEY
                MOVE buffer (BARRPOS: 5) TO BARRA
                IF   BARRA > 50
                     IF  BARRA = 51
                         MOVE SP2-KEY-DOWN TO SP2-CD-KEY
                     ELSE
                         MOVE SP2-KEY-PGDN TO SP2-CD-KEY
                         IF  BARRA > 98
                             MOVE SP2-KEY-CTRL-PGDN
                               TO SP2-CD-KEY
                         END-IF
                     END-IF
                ELSE
                     IF  BARRA = 49
                         MOVE SP2-KEY-UP   TO SP2-CD-KEY
                     ELSE
                         MOVE SP2-KEY-PGUP TO SP2-CD-KEY
                         IF  BARRA < 2
                             MOVE SP2-KEY-CTRL-PGUP
                               TO SP2-CD-KEY
                         END-IF
                     END-IF
                END-IF
           END-IF

           MOVE "00050" TO buffer (BARRPOS: 5).

       602-99-FIM. EXIT.

       605-INICIO-BOXWRK.

           MOVE ">" TO FIM
           MOVE "B" TO USER-IO
           PERFORM 650-LER THRU 650-99-FIM.

       605-99-FIM. EXIT.

       606-FIM-BOXWRK.

           MOVE "E" TO USER-IO
           PERFORM 650-LER THRU 650-99-FIM

           COMPUTE VOLTA = WORKBOX-VERTICAL-LENGTH + 1
           PERFORM VOLTA TIMES
             IF  FIM NOT = ">"
                 SET READ-PREVIOUS TO TRUE
                 PERFORM 650-LER THRU 650-99-FIM
                 IF   AT-END
                      MOVE ">" TO FIM
                 END-IF
             END-IF
           END-PERFORM

           MOVE    "N"                     TO FIM
           PERFORM 610-MONTA-cwboxf      THRU 610-99-FIM
           MOVE    WORKBOX-VERTICAL-LENGTH TO M
           COMPUTE L = WORKBOX-LINE
                     + WORKBOX-VERTICAL-LENGTH
           MOVE "<" TO                     FIM.

       606-99-FIM. EXIT.

       610-MONTA-cwboxf.

           MOVE WORKBOX-LINE TO L
           MOVE 0            TO M
                               VOLTA
           MOVE SPACES      TO MATRIX
           PERFORM WORKBOX-VERTICAL-LENGTH TIMES
             ADD 1 TO M L
             IF  FIM NOT = "<"
                 SET READ-NEXT TO TRUE
                 PERFORM 650-LER THRU 650-99-FIM
                 IF   AT-END
                      MOVE "<" TO FIM
                      SET READ-PREVIOUS TO TRUE
                      PERFORM 650-LER THRU 650-99-FIM
                 ELSE
                     ADD 1 TO VOLTA
                     PERFORM 615-DISCO-MEMORIA THRU 615-99-FIM
                END-IF
             END-IF
           END-PERFORM
           COMPUTE L = WORKBOX-LINE + 1
           MOVE    1 TO M
           PERFORM 620-exibe-CWBOXF THRU 620-99-FIM.

       610-99-FIM. EXIT.

       615-DISCO-MEMORIA.

           MOVE STRING-1 TO TEXTO-1 (M)
           MOVE STRING-2 TO TEXTO-2 (M)

           IF  WORKBOX-STRING-1-LENGTH NOT = 0
               MOVE STRING-1 TO TEXTO-L (M) (1: )
           END-IF

           IF  WORKBOX-STRING-2-LENGTH NOT = 0
               MOVE STRING-2 TO TEXTO-L (M) (GAP2: )
           END-IF

           IF   CWACCENT = "OFF"
                INSPECT TEXTO-STRING (M)
                             CONVERTING ACENTOS-WINDOWS
                                     TO ACENTOS-OFF
           ELSE
                     IF   MFF = "MF"
                          INSPECT TEXTO-STRING (M)
                                       CONVERTING ACENTOS-850
                                               TO ACENTOS-437
                     ELSE
                          INSPECT TEXTO-STRING (M)
                                       CONVERTING ACENTOS-850
                                               TO ACENTOS-WINDOWS
                     END-IF
           END-IF.

       615-99-FIM. EXIT.

       620-exibe-CWBOXF.

           MOVE 0 TO M2
           PERFORM WORKBOX-VERTICAL-LENGTH TIMES
               ADD  1 TO M2
               IF   WORKBOX-STRING-1-LENGTH NOT = 0
                    MOVE WORKBOX-POSIT (1 M2) TO P
                    MOVE TEXTO-1 (M2) TO buffer
                                         (P: WORKBOX-STRING-1-LENGTH)
                    IF  WORKBOX-ORDER < 2
                        MOVE LISTA (M2) TO SP2-FD-ID
                        MOVE 2000       TO SP2-FD-VAR-LEN
                        CALL SP2     USING SP2-GET-FIELD-DEF
                                           SP2-FIELD-DEF
                        IF  TEXTO-1 (M2) = SPACES
                            MOVE "y"   TO SP2-FD-OUTPUT
                        ELSE
                            MOVE X"00" TO SP2-FD-OUTPUT
                        END-IF
                        move XCUR  to sp2-fd-cur-colr
<pop>                 if   texto-2 (m2) (79:1) = x'01'
                           move texto-2 (m2) (80:1) to color-alert(1:1)
                           move color-alert(1:1)    to SP2-FD-COLR
                           add inverte              to color-alert
                           move color-alert(1:1)    to sp2-fd-cur-colr
                      end-if
                        CALL SP2   USING SP2-SET-FIELD-DEF
                                         SP2-FIELD-DEF
                    END-IF
               END-IF
               IF   WORKBOX-STRING-2-LENGTH NOT = 0
                    MOVE WORKBOX-POSIT (2 M2) TO P
                    MOVE TEXTO-2 (M2) TO buffer
                                         (P: WORKBOX-STRING-2-LENGTH)
                    IF  WORKBOX-ORDER > 1
                        MOVE LISTA2(M2) TO SP2-FD-ID
                        MOVE 2000       TO SP2-FD-VAR-LEN
                        CALL SP2     USING SP2-GET-FIELD-DEF
                                           SP2-FIELD-DEF
                        IF  TEXTO-2 (M2) = SPACES
                            MOVE "y"   TO SP2-FD-OUTPUT
                        ELSE
                            MOVE X"00" TO SP2-FD-OUTPUT
                        END-IF
                        move x'00' to SP2-FD-COLR
                        move XCUR  to sp2-fd-cur-colr
<pop>                 if   texto-2 (m2) (79:1) = x'01'
                           move texto-2 (m2) (80:1) to color-alert(1:1)
                           move color-alert(1:1)    to SP2-FD-COLR
                           add inverte              to color-alert
                           move color-alert(1:1)    to sp2-fd-cur-colr
                      end-if
                        CALL SP2   USING SP2-SET-FIELD-DEF
                                         SP2-FIELD-DEF
                    END-IF
               END-IF
           END-PERFORM
           MOVE "00050" TO buffer (BARRPOS: 5)
           call sp2   using sp2-display-window sp2-null-parm.

       620-99-FIM. EXIT.

       650-LER.

           IF SKIP-LER = 1 AND READ-NEXT
              MOVE 0 TO SKIP-LER
              EXIT PARAGRAPH
           END-IF


           MOVE 0               TO SKIP-LER
           MOVE 0               TO VAZIO
           MOVE        STRING-1 TO END-STRING-1
           MOVE        STRING-2 TO END-STRING-2

           PERFORM TEST AFTER
                        UNTIL AT-END
                        OR ((NOT READ-NEXT) AND (NOT READ-PREVIOUS))
                OR (WORKBOX-RETURN = 1 AND (STRING-1X NOT = SPACE))
                OR (WORKBOX-RETURN = 2 AND (STRING-2X NOT = SPACE))
                    CALL WORKBOX-PROGRAM USING USER-IO
                                               WORKBOX-ORDER
                                               STRING-1
                                               STRING-2
                                               WORKBOX-VERTICAL-LENGTH
                                               WORKBOX-WORK-AREA
                    MOVE STRING-1 TO STRING-1X
                    MOVE STRING-2 TO STRING-2X
                    IF   SPACES = STRING-1X AND STRING-2X
                    AND (READ-NEXT OR READ-PREVIOUS)
                         ADD 1 TO VAZIO
                         IF   VAZIO = 50
                              SET AT-END TO TRUE
                         END-IF
                    END-IF
           END-PERFORM

           IF   AT-END
                MOVE END-STRING-1 TO STRING-1 STRING-1X
                MOVE END-STRING-2 TO STRING-2 STRING-1X
           END-IF

           IF   SEM-DADOS = 1
           AND (NOT AT-END)
                IF  (STRING-1X NOT = SPACES)
                AND  WORKBOX-RETURN = 1
                     MOVE 0 TO SEM-DADOS
                END-IF
                IF  (STRING-2X NOT = SPACES)
                AND  WORKBOX-RETURN = 2
                     MOVE 0 TO SEM-DADOS
                END-IF
           ELSE
                continue
           END-IF.

       650-99-FIM. EXIT.

       651-CREATE-FIELD.

           MOVE LOW-VALUES      TO SP2-FD-DATA
                                   SP2-FD-VAR-LENS
           ADD  1               TO FIELDX
           IF   HIDE-LIST = 1
                COMPUTE SP2-FD-FLD-NUM = HIDE-FIELD + FIELDX
           ELSE
                move fieldx to SP2-FD-FLD-NUM
           END-IF
           MOVE    SP2-FD-FLD-NUM TO SP2-FD-TAB-NUM
           CALL "CWSPID" USING SP2-FD-ID "FInsert"
           MOVE SP2-FD-ID       TO FD-ID (FIELDX)

           IF   STR = 2
                MOVE WID-2      TO SP2-FD-WIDTH
           ELSE
                MOVE WID-1      TO SP2-FD-WIDTH
           END-IF
           IF   HIDE-LIST = 1
                COMPUTE SP2-FD-ROW = WORKBOX-LINE + L - 1
                IF   STR = 2
                     MOVE COL-2      TO SP2-FD-COL
                ELSE
                     MOVE COL-1      TO SP2-FD-COL
                END-IF
           ELSE
                MOVE L TO SP2-FD-ROW
                IF   STR = 2
                     MOVE WORKBOX-STRING-1-LENGTH TO SP2-FD-COL
                     IF   WORKBOX-STRING-1-LENGTH > ZERO
                          SUBTRACT 1 FROM SP2-FD-COL
                     END-IF
                END-IF
           END-IF

           ADD  1            TO L
           MOVE SPACES       TO SP2-FD-NAME
           STRING 'CWBOXF-' STR '-' L DELIMITED BY SIZE
             INTO SP2-FD-NAME

           IF  STR = 1
               MOVE SP2-FD-ID   TO LISTA  (L)
           ELSE
               MOVE SP2-FD-ID   TO LISTA2 (L)
           END-IF

           IF   WORKBOX-ORDER = STR
                MOVE OK2         TO SP2-FD-CUR-COLR
                INITIALIZE LISTBOX-REG
                MOVE SP2-FD-ID   TO LISTBOX-ID
                MOVE objects  TO LISTBOX-OBJECT
                MOVE 1           TO LISTBOX-KEY
                MOVE L           TO LISTBOX-M
                WRITE LISTBOX-REG
                MOVE SP2-FD-COL TO START-CURSOR
                IF   L = 1
                AND  COMBO = 1
                     MOVE SP2-FD-ID TO SP2-CD-NEXT-FLD-ID
                END-IF
                IF   L = 1
                AND (CWOBJE-OPTION NOT = SPACES)
                    INITIALIZE HIDE-REG
                    MOVE CWOBJE-OPTION TO HIDE-DATANAME
                    MOVE pop-WINDOW    TO HIDE-WINDOW
                    READ HIDE
                    IF  FS-HIDE < "10"
                    AND HIDE-GUIA = 0
                        MOVE SP2-FD-ID TO HIDE-GUIA
                        IF  (HIDE-GUIA-IND NOT = 0)
                             MOVE SP2-FD-ID TO FIELD-ID (HIDE-GUIA-IND)
                        END-IF
                        REWRITE HIDE-REG
                    END-IF
                END-IF
           ELSE
                MOVE "y"        TO SP2-FD-OUTPUT
           END-IF
           MOVE 10              TO SP2-FD-HEIGHT
           MOVE STR-LEN         TO SP2-FD-MAX-LEN
                                   SP2-FD-PROG-LEN
                                   sp2-fd-var-len
           MOVE SIZE-FIELDS     TO SP2-FD-PROG-OFF
           COMPUTE WORKBOX-POSIT (STR L) = SIZE-FIELDS + 1
           ADD  STR-LEN         TO SIZE-FIELDS
           MOVE FONT-WIN        TO SP2-FD-FONT-ID
           IF  CWUSER-FIELD = "1"
               MOVE 2 TO SP2-FD-FONT-ID
           END-IF
           MOVE X"01"           TO SP2-FD-COLR
<pop>      if   texto-2 (l) (79:1) = x'01'
                move texto-2 (l) (80:1) to color-alert(1:1)
                move color-alert(1:1)    to SP2-FD-COLR
                add inverte              to color-alert
                move color-alert(1:1)    to sp2-fd-cur-colr
           end-if
           PERFORM 652-FIELD-BOX THRU 652-99-FIM.

       651-99-FIM. EXIT.

       652-FIELD-BOX.

           IF   COMBO = 0
                MOVE HIDE-FIELD TO SP2-FD-FLD-NUM
                                   SP2-FD-TAB-NUM
                PERFORM 040-MAP-FIELD THRU 040-99-FIM
           ELSE
                IF RESOLUTION = 1
                   ADD 1 TO SP2-FD-HEIGHT
                END-IF
                move sp2-fd-prog-len to sp2-fd-initial-len
                                        sp2-fd-format-len
                                        sp2-fd-max-len
                MOVE FIELDX          TO SP2-FD-FLD-NUM
                                        SP2-FD-TAB-NUM
                CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           END-IF.

       652-99-FIM. EXIT.

       653-ROLAGEM.

           PERFORM VARYING M
                       FROM WORKBOX-VERTICAL-LENGTH BY -1
                      UNTIL M = 1
                         OR LISTA  (M) = SP2-CD-NEXT-FLD-ID
                         OR LISTA2 (M) = SP2-CD-NEXT-FLD-ID
                   CONTINUE
           END-PERFORM
           EVALUATE TRUE
              WHEN (M > 1 OR EDIT-CURSOR-DOWN)
               AND (M < WORKBOX-VERTICAL-LENGTH OR EDIT-CURSOR-UP)
               AND (EDIT-CURSOR-UP OR EDIT-CURSOR-DOWN)
                   IF   EDIT-CURSOR-UP
                        SUBTRACT 1 FROM M
                   ELSE
                        ADD 1 TO M
                        IF   TEXTO-L (M) = SPACE
                             SUBTRACT 1 FROM M
                        END-IF
                   END-IF
                   IF   WORKBOX-ORDER > 1
                        MOVE LISTA2 (M) TO SP2-CD-NEXT-FLD-ID LAST-ID
                   ELSE
                        MOVE LISTA  (M) TO SP2-CD-NEXT-FLD-ID LAST-ID
                   END-IF
              WHEN EDIT-HOME
                   MOVE    1 TO M
                   IF   WORKBOX-ORDER > 1
                        MOVE LISTA2 (M) TO SP2-CD-NEXT-FLD-ID LAST-ID
                   ELSE
                        MOVE LISTA  (M) TO SP2-CD-NEXT-FLD-ID LAST-ID
                   END-IF
              WHEN EDIT-END
                   MOVE WORKBOX-VERTICAL-LENGTH TO M
                   PERFORM UNTIL M = 1
                              OR (TEXTO-L (M) NOT = SPACE)
                           SUBTRACT 1 FROM M
                   END-PERFORM
                   IF   WORKBOX-ORDER > 1
                        MOVE LISTA2 (M) TO SP2-CD-NEXT-FLD-ID LAST-ID
                   ELSE
                        MOVE LISTA  (M) TO SP2-CD-NEXT-FLD-ID LAST-ID
                   END-IF
              WHEN EDIT-CURSOR-DOWN
               AND FIM NOT = "<"
               AND M = WORKBOX-VERTICAL-LENGTH
                   MOVE "N"  TO FIM
                   SET READ-NEXT TO TRUE
                   PERFORM 650-LER THRU 650-99-FIM
                   IF   AT-END
                        MOVE "<" TO FIM
                        SET READ-PREVIOUS TO TRUE
                        PERFORM 650-LER THRU 650-99-FIM
                   ELSE
                        MOVE "N" TO FIM
                        PERFORM VARYING M FROM 2 BY 1
                           UNTIL M > WORKBOX-VERTICAL-LENGTH
                           COMPUTE M-1 = M - 1
                           MOVE TEXTO-1 (M) TO TEXTO-1 (M-1)
                           MOVE TEXTO-2 (M) TO TEXTO-2 (M-1)
                           MOVE TEXTO-L (M) TO TEXTO-L (M-1)
                        END-PERFORM
                        MOVE WORKBOX-VERTICAL-LENGTH TO M
                        PERFORM 615-DISCO-MEMORIA
                        THRU    615-99-FIM
                        PERFORM 620-exibe-CWBOXF
                        THRU    620-99-FIM
                   END-IF
              WHEN EDIT-CONTROL-PAGE-UP
                   PERFORM 605-INICIO-BOXWRK THRU 605-99-FIM
                   PERFORM 610-MONTA-cwboxf  THRU 610-99-FIM
                   MOVE ">" TO FIM
              WHEN EDIT-CONTROL-PAGE-DOWN
                   MOVE    "N"              TO FIM
                   PERFORM 606-FIM-BOXWRK THRU 606-99-FIM
              WHEN ((EDIT-CURSOR-UP AND M = 1)
                OR EDIT-PAGE-UP)
               AND FIM NOT = ">"
                   MOVE "N"  TO FIM
                   IF   EDIT-PAGE-UP
                        ADD WORKBOX-VERTICAL-LENGTH TO VOLTA
                   ELSE
                        ADD 1 TO VOLTA
                   END-IF
                   PERFORM VOLTA TIMES
                           IF   FIM NOT = ">"
                                SET READ-PREVIOUS TO TRUE
                                PERFORM 650-LER THRU 650-99-FIM
                                IF AT-END
                                   PERFORM 605-INICIO-BOXWRK
                                   THRU 605-99-FIM
                                END-IF
                           END-IF
                   END-PERFORM
                   PERFORM 610-MONTA-cwboxf THRU 610-99-FIM
              WHEN EDIT-PAGE-DOWN
               AND FIM NOT = "<"
                   MOVE "N"                     TO FIM
                   PERFORM 610-MONTA-cwboxf   THRU 610-99-FIM
                   MOVE WORKBOX-VERTICAL-LENGTH TO M
                   COMPUTE L = WORKBOX-LINE
                             + WORKBOX-VERTICAL-LENGTH
                   IF   TEXTO-L (1) = SPACE
                        MOVE    "N"              TO FIM
                        PERFORM 606-FIM-BOXWRK THRU 606-99-FIM
                   END-IF
           END-EVALUATE.

       653-99-FIM. EXIT.

       655-CURSOR-BARRA.

           IF   SELECTED  = 0
                GO TO 655-99-FIM
           END-IF

           IF OK NOT = X'01'
              MOVE 0      TO INVERTE
           ELSE
              MOVE 128    TO INVERTE
           END-IF

           IF   LISTA (SELECTED ) NOT = 0
                MOVE LOW-VALUES TO SP2-FD-DATA
                                   SP2-FD-VAR-LENS
                MOVE LISTA (SELECTED )  TO SP2-FD-ID
                MOVE 2000       TO SP2-FD-VAR-LEN
                CALL SP2     USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                MOVE OK         TO SP2-FD-COLR
                MOVE SP2-FD-INITIAL-LEN TO SP2-FD-VAR-LEN
<pop>           if   texto-2 (m) (79:1) = x'01'
                     move texto-2 (m) (80:1) to color-alert(1:1)
                     add inverte             to color-alert
                     move color-alert(1:1)   to SP2-FD-COLR
                     add inverte             to color-alert
                     move color-alert(1:1)   to sp2-fd-cur-colr
                end-if
                CALL SP2   USING SP2-SET-FIELD-DEF  SP2-FIELD-DEF
           END-IF

           IF   LISTA2 (SELECTED ) NOT = 0
                MOVE LOW-VALUES TO SP2-FD-DATA
                                   SP2-FD-VAR-LENS
                MOVE LISTA2 (SELECTED ) TO SP2-FD-ID
                MOVE 2000       TO SP2-FD-VAR-LEN
                CALL SP2     USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                MOVE OK         TO SP2-FD-COLR
                MOVE SP2-FD-INITIAL-LEN TO SP2-FD-VAR-LEN
<pop>           if   texto-2 (m) (79:1) = x'01'
                     move texto-2 (m) (80:1) to color-alert(1:1)
                     add inverte             to color-alert
                     move color-alert(1:1)   to SP2-FD-COLR
                     add inverte             to color-alert
                     move color-alert(1:1)   to sp2-fd-cur-colr
                end-if
                CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           END-IF.

       655-99-FIM. EXIT.

       656-REVERSE-CASE.

           IF   CASE-CHAR ALPHABETIC-LOWER
                INSPECT CASE-CHAR CONVERTING MINUSCULAS
                                          TO MAIUSCULAS
           ELSE
                INSPECT CASE-CHAR CONVERTING MAIUSCULAS
                                          TO MINUSCULAS
           END-IF
           IF   WORKBOX-ORDER = 1
                MOVE TEXTO-A (1: T) TO STRING-1
                MOVE CASE-CHAR      TO STRING-1 (T:) DIGITADO
           ELSE
                MOVE TEXTO-A (1: T) TO STRING-2
                MOVE CASE-CHAR      TO STRING-2 (T:) DIGITADO
           END-IF.

       656-99-FIM. EXIT.

       660-FIND-CURSOR.

           IF   M = 0
                MOVE 1 TO M
           END-IF

           IF   WORKBOX-ORDER < 2
                MOVE LISTA  (M) TO SP2-CD-NEXT-FLD-ID LAST-ID
           ELSE
                MOVE LISTA2 (M) TO SP2-CD-NEXT-FLD-ID LAST-ID
           END-IF

           IF   WORKBOX-ORDER < 2
                MOVE buffer (OFF-W: END-W) TO STRING-1
                IF  PIC-LK (FIELD)(1:1) = "Z"
                    INSPECT STRING-1 (1: LEN-LK(FIELD))
                            CONVERTING SPACE TO "0"
                END-IF
                MOVE SPACES TO STRING-2
           ELSE
                MOVE buffer (OFF-W: END-W) TO STRING-2
                IF  PIC-LK (FIELD)(1:1) = "Z"
                    INSPECT STRING-2 (1: LEN-LK(FIELD))
                            CONVERTING SPACE TO "0"
                END-IF
                MOVE SPACES TO STRING-1
           END-IF


           MOVE "N" TO FIM
           IF SP2-CD-KEY = 0
             AND((WORKBOX-ORDER = 1
              AND WORKBOX-STRING-1-LENGTH NOT = 0)
               OR WORKBOX-ORDER > 1
              AND(WORKBOX-STRING-2-LENGTH NOT = 0))
                  IF WORKBOX-ORDER = 1
                     MOVE TEXTO-1 (1)     TO T-PRE
                                             TEXTO-A TEXTO-B
                     MOVE STRING-1 (T:1)  TO CARACTER
                  ELSE
                     MOVE TEXTO-2 (1)     TO T-PRE
                                             TEXTO-A TEXTO-B
                     MOVE STRING-2 (T:1)  TO CARACTER
                  END-IF
                  IF   CARACTER = X"08"
                       IF   T > 2
                            SUBTRACT 1 FROM T
                            MOVE SPACE TO TEXTO-A (T: 1) CARACTER
                            SUBTRACT 1 FROM T
                       END-IF
                  ELSE
                       MOVE CARACTER TO TEXTO-A (T: 1)
                  END-IF
                  IF  CWFIND = 'OFF'
                      MOVE TEXTO-A TO TEXTO-B
                  END-IF
                  IF  TEXTO-A NOT = TEXTO-B
                      MOVE "N"            TO FIM
                      IF WORKBOX-ORDER = 1
                         MOVE TEXTO-A (1: T) TO STRING-1
                      ELSE
                         MOVE TEXTO-A (1: T) TO STRING-2
                      END-IF
                      SET NOT-LESS TO TRUE
                      PERFORM 650-LER THRU 650-99-FIM
                      IF NOT  AT-END
                         SET READ-NEXT TO TRUE
                         PERFORM 650-LER THRU 650-99-FIM
                         MOVE 1 TO SKIP-LER
                      END-IF
                      COMPUTE P = T - 1
                      IF P = 0
                         MOVE 1 TO P
                      END-IF
                      IF  (WORKBOX-ORDER = 1 AND T > 1
                      AND  STRING-1(1: P) > T-PRE(1: P))
                      OR ((WORKBOX-ORDER NOT = 1) AND T > 1
                      AND (STRING-2(1: P) > T-PRE(1: P)))
                      OR  AT-END
                      OR  (WORKBOX-ORDER = 1
                      AND  STRING-1(T: 1) <> TEXTO-A (T: 1))
                      OR ((WORKBOX-ORDER NOT = 1)
                      AND (STRING-2(T: 1) <> TEXTO-A (T: 1)))
                          MOVE TEXTO-A (T: 1) TO CASE-CHAR DIGITADO
                          IF  CASE-CHAR ALPHABETIC-LOWER
                          OR  CASE-CHAR ALPHABETIC-UPPER
                              PERFORM 656-REVERSE-CASE THRU 656-99-FIM
                              SET NOT-LESS TO TRUE
                              PERFORM 650-LER THRU 650-99-FIM
                              IF NOT  AT-END
                                 SET READ-NEXT TO TRUE
                                 PERFORM 650-LER THRU 650-99-FIM
                                 MOVE 1 TO SKIP-LER
                              END-IF
                          END-IF
                          IF  (WORKBOX-ORDER = 1 AND T > 1
                          AND  STRING-1(1: P) > T-PRE(1: P))
                          OR ((WORKBOX-ORDER NOT = 1) AND T > 1
                          AND (STRING-2(1: P) > T-PRE(1: P)))
                          OR  AT-END
                          OR  (WORKBOX-ORDER = 1
                          AND  STRING-1(T: 1) <> DIGITADO)
                          OR ((WORKBOX-ORDER NOT = 1)
                          AND (STRING-2(T: 1) <> DIGITADO))
                              MOVE T-PRE     TO STRING-1
                                                STRING-2
                              SET NOT-LESS TO TRUE
                              PERFORM 650-LER THRU 650-99-FIM
                              IF T > 0
                              AND (NOT AT-END)
                                 SUBTRACT 1 FROM T
                              END-IF
                          END-IF
                      END-IF
                      IF   AT-END
                           PERFORM 606-FIM-BOXWRK
                           THRU    606-99-FIM
                      ELSE
                           PERFORM 610-MONTA-cwboxf
                           THRU    610-99-FIM
                           MOVE    buffer (OFF-W: END-W)
                             TO    save-buffer (OFF-W: END-W)
                      END-IF
                  END-IF
           ELSE
                SET NOT-LESS TO TRUE
                PERFORM 650-LER THRU 650-99-FIM
                IF NOT AT-END
                   SET READ-NEXT TO TRUE
                   PERFORM 610-MONTA-cwboxf THRU 610-99-FIM
                   MOVE buffer (OFF-W: END-W)
                     TO save-buffer (OFF-W: END-W)
                END-IF
           END-IF.

       660-99-FIM. EXIT.

       670-CWOBJE.

           ON 1
              MOVE 1 TO OBJECTs-ON
              OPEN I-O OBJETOS
              OPEN I-O HIDE.

           IF  PARAMETROS-CWOBJE (1:1) = 'W' OR 'w'
               GO TO 670-99-FIM
           END-IF

           EVALUATE TRUE
                    WHEN CWOBJE-DROP
                         MOVE POP-WINDOW TO OBJETOS-WINDOW
                         MOVE ZERO       TO OBJETOS-SEQUENCE
                         START OBJETOS KEY NOT < OBJETOS-CHAVE
                         PERFORM UNTIL FS-OBJETOS > "00"
                                  READ OBJETOS NEXT RECORD
                                  IF  FS-OBJETOS < "10"
                                      IF OBJETOS-WINDOW NOT = POP-WINDOW
                                         EXIT PERFORM
                                      END-IF
                                      DELETE OBJETOS RECORD
                                  END-IF
                         END-PERFORM
                         IF   DELETE-CWREAD = 1
                              MOVE ZERO TO DELETE-CWREAD
                              CALL "CWREAD" USING "D"
                                                  CWOBJE-ORDER
                                                  CWOBJE-STRING-1(1)
                                                  CWOBJE-STRING-2(1)
                                                  CWOBJE-VERTICAL-LENGTH
                                                  CWOBJE-WORK-AREA
                         END-IF
                         INITIALIZE PARAMETROS-CWOBJE
                         MOVE ZERO       TO OBJETOS-SEQUENCE
                         PERFORM TEST AFTER UNTIL FS-HIDE > '09'
                                 MOVE POP-WINDOW
                                   TO HIDE-WINDOW
                                 READ HIDE KEY IS HIDE-WINDOW
                                 IF   FS-HIDE < '10'
                                      MOVE pop-WINDOW  TO campos-janela
                                      move hide-dataname to dataname-e
                                      move 1        to campos-subscript
                                      move hide-lincol   to pos-e
                                      READ CAMPOS KEY IS CAMPOS-name
                                      DELETE HIDE RECORD
                                 END-IF
                         END-PERFORM
                         INITIALIZE HIDE-REG
                    WHEN CWOBJE-SCROLL
                     AND (CWOBJE-PIXEL-WIDTH (1:8) = SPACES
                       OR ((not CWOBJE-VERTICAL)
                      AND  (not CWOBJE-HORIZONTAL)))
                         CONTINUE
                    WHEN X91-parameter-CWOBJE > 2
                     AND CWOBJE-OCCURS
                         MOVE ZERO        TO CWOBJE-oc-NUMBER
                         MOVE POP-WINDOW  TO OBJETOS-WINDOW
                         MOVE HIGH-VALUES TO OBJETOS-SEQUENCE(1:)
                         START OBJETOS KEY NOT > OBJETOS-CHAVE
                         IF FS-OBJETOS = '00'
                            READ OBJETOS PREVIOUS RECORD
                            IF  FS-OBJETOS = '00'
                            AND OBJETOS-WINDOW = POP-WINDOW
                                MOVE OBJETOS-SEQUENCE
                                  TO CWOBJE-oc-NUMBER
                            END-IF
                         END-IF
                    WHEN X91-parameter-CWOBJE > 2
                     AND CWOBJE-GET
                         MOVE CWOBJE-oc-NUMBER TO OBJETOS-SEQUENCE
                         MOVE POP-WINDOW           TO OBJETOS-WINDOW
                         READ OBJETOS
                         COMPUTE SIZE-MOVEL = SIZE-REG
                                            - LENGTH OF OBJETOS-FIXO
                         MOVE OBJETOS-DATA TO PARAMETROS-CWOBJE
                    WHEN CWOBJE-SCROLL
                     AND (CWOBJE-THUMB-LENGTH NOT = 5)
                         MOVE
                         "Scroll bar requer vari vel THUMB com 5 bytes"
                         TO MSG-D
                         PERFORM SEND-SP2
                    WHEN OTHER
                         MOVE 1 TO WITH-OBJECTS
                         IF   CWOBJE-PUSH-BUTTON
                         AND (CWOBJE-IMAGE NOT = SPACES)
                             SET CWOBJE-ICON TO TRUE
                         END-IF
                         IF   CWOBJE-SMALL
                              IF   CWOBJE-PUSH-BUTTON
                                   SET CWOBJE-PUSH-MOUSE TO TRUE
                              ELSE
                                   IF CWOBJE-ICON
                                      SET CWOBJE-MICRO-ICON TO TRUE
                                   END-IF
                              END-IF
                              MOVE SPACE TO CWOBJE-FLAG
                         END-IF
                         PERFORM 680-LIMITS THRU 680-99-FIM
                         IF  CWOBJE-LIST-BOX
                         AND CWOBJE-STRING-2-LENGTH = 0
                         AND CWOBJE-STRING-1-LENGTH =
                             CWOBJE-HORIZONTAL-LENGTH
                                 ADD 2 TO CWOBJE-STRING-1-LENGTH
                                          CWOBJE-HORIZONTAL-LENGTH
                         END-IF
                         IF  CWOBJE-LIST-BOX
                         AND CWOBJE-STRING-1-LENGTH = 0
                         AND CWOBJE-STRING-2-LENGTH =
                             CWOBJE-HORIZONTAL-LENGTH
                                 ADD 2 TO CWOBJE-STRING-2-LENGTH
                                          CWOBJE-HORIZONTAL-LENGTH
                         END-IF
                         IF  (CWOBJE-LIST-BOX
                         OR   CWOBJE-COMBO-BOX)
                         AND  CWOBJE-PROGRAM = SPACES
                              MOVE OBJETOS-SEQUENCE (1: 2)
                                TO CWOBJE-WORK-AREA (1: 2)
                              MOVE CWOBJE-LABEL TO CWOBJE-WORK-AREA (4:)
                              PERFORM 540-LOAD-CWREAD THRU 540-99-FIM
                              MOVE ZERO TO SIZE-MOVEL
                         ELSE
                              MOVE ZERO TO STRINGS
                              PERFORM VARYING G FROM 1 BY 1
                                        UNTIL G > 100
                                      IF CWOBJE-STRING-1(G) NOT = SPACES
                                         ADD 1 TO STRINGS
                                      END-IF
                              END-PERFORM
                              COMPUTE SIZE-MOVEL = STRINGS * 160
                         END-IF
                         COMPUTE SIZE-REG = LENGTH OF OBJETOS-FIXO + 4
                                          + SIZE-MOVEL
                         INSPECT CWOBJE-OPTION
                                 CONVERTING MINUSCULAS TO MAIUSCULAS
                         MOVE PARAMETROS-CWOBJE TO OBJETOS-DATA
                         MOVE POP-WINDOW        TO OBJETOS-WINDOW
                         WRITE OBJETOS-REG
                         INITIALIZE PARAMETROS-CWOBJE
           END-EVALUATE.

       670-99-FIM. EXIT.

       680-LIMITS.

           MOVE POP-WINDOW  TO OBJETOS-WINDOW
           MOVE HIGH-VALUES TO OBJETOS-SEQUENCE(1:)
           START OBJETOS KEY NOT > OBJETOS-CHAVE
           IF FS-OBJETOS = '00'
              READ OBJETOS PREVIOUS RECORD
              IF  FS-OBJETOS = '00'
              AND OBJETOS-WINDOW = POP-WINDOW
                  ADD 1 TO OBJETOS-SEQUENCE
              ELSE
                  MOVE '23' TO FS-OBJETOS
              END-IF
           END-IF
           IF FS-OBJETOS > '09'
              MOVE POP-WINDOW TO OBJETOS-WINDOW
              MOVE 1          TO OBJETOS-SEQUENCE
           END-IF
           INITIALIZE OBJETOS-DATA

           IF   CWOBJE-LIST-BOX
                INSPECT CWOBJE-OPTION
                        CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF

           IF   CWOBJE-PUSH-MOUSE
                MOVE 1 TO CWOBJE-VERTICAL-LENGTH
           END-IF

           IF   CWOBJE-LINE = 0
                MOVE 1 TO CWOBJE-LINE
           END-IF

           IF   CWOBJE-COLUMN = 0
                MOVE 1 TO CWOBJE-COLUMN
           END-IF

           IF   CWOBJE-VERTICAL-LENGTH = 0
                MOVE 1 TO CWOBJE-VERTICAL-LENGTH
           END-IF

           IF   CWOBJE-HORIZONTAL-LENGTH = 0
           AND (CWOBJE-PUSH-BUTTON
             OR CWOBJE-PUSH-MOUSE
             OR CWOBJE-TEXT)
                MOVE 1 TO CWOBJE-HORIZONTAL-LENGTH
                IF CWOBJE-LABEL NOT = SPACES
                   PERFORM VARYING CWOBJE-HORIZONTAL-LENGTH
                                   FROM JANWID BY -1
                                   UNTIL CWOBJE-LABEL
                           (CWOBJE-HORIZONTAL-LENGTH: 1) NOT = SPACES
                           CONTINUE
                   END-PERFORM
                   PERFORM VARYING I FROM 1 BY 1
                                     UNTIL I > CWOBJE-HORIZONTAL-LENGTH
                           IF CWOBJE-LABEL (I: 1) = "~"
                              SUBTRACT 1 FROM CWOBJE-HORIZONTAL-LENGTH
                           END-IF
                   END-PERFORM
                   IF   CWOBJE-PUSH-BUTTON
                        ADD 4 TO CWOBJE-HORIZONTAL-LENGTH
                   END-IF
                END-IF
           END-IF

           PERFORM UNTIL CWOBJE-VERTICAL-LENGTH < (JANHEI - 1)
                     OR  CWOBJE-VERTICAL-LENGTH < 2
                   SUBTRACT 1 FROM CWOBJE-VERTICAL-LENGTH
           END-PERFORM

           PERFORM UNTIL CWOBJE-HORIZONTAL-LENGTH < (JANWID - 1)
                      OR CWOBJE-HORIZONTAL-LENGTH < 2
                   SUBTRACT 1 FROM CWOBJE-HORIZONTAL-LENGTH
           END-PERFORM

           PERFORM UNTIL (CWOBJE-LINE + CWOBJE-VERTICAL-LENGTH)
                                      < (JANHEI + 1)
                      or (CWOBJE-COMBO-BOX)
                    OR (CWOBJE-PUSH-MOUSE
                        AND CWOBJE-LINE < (JANHEI + 1))
                   OR CWOBJE-LINE < 2
                   SUBTRACT 1 FROM CWOBJE-LINE
           END-PERFORM

           PERFORM UNTIL (CWOBJE-COLUMN + CWOBJE-HORIZONTAL-LENGTH)
                       < (JANWID + 1)
                   OR CWOBJE-COLUMN < 2
                   SUBTRACT 1 FROM CWOBJE-COLUMN
           END-PERFORM.

       680-99-FIM. EXIT.

       540-LOAD-CWREAD.

           MOVE 1        TO DELETE-CWREAD
           MOVE "CWREAD" TO CWOBJE-PROGRAM
           IF   STRINGS = 0
                MOVE 99 TO STRINGS
           END-IF
           IF   CWOBJE-ORDER = 0
           AND (CWOBJE-STRING-1 (1) NOT = SPACES)
                MOVE 1 TO CWOBJE-ORDER
           END-IF
           IF   CWOBJE-RETURN = 0
                MOVE CWOBJE-ORDER TO CWOBJE-RETURN
           END-IF
           MOVE ZERO TO STRINGS
           PERFORM VARYING G FROM 1 BY 1 UNTIL G > 100
                   IF ((CWOBJE-STRING-1 (G) NOT = SPACES)
                   AND  CWOBJE-ORDER = 1)
                   OR ((CWOBJE-STRING-2 (G) NOT = SPACES)
                   AND  CWOBJE-ORDER = 2)
                        ADD 1 TO STRINGS
                        CALL "CWREAD" USING "L" CWOBJE-ORDER
                                             CWOBJE-STRING-1 (G)
                                             CWOBJE-STRING-2 (G)
                                             CWOBJE-VERTICAL-LENGTH
                                             CWOBJE-WORK-AREA
                   END-IF
           END-PERFORM.

       540-99-FIM. EXIT.

       700-DROP-LISTBOX.

           PERFORM 710-CANCEL-PROVIDER THRU 710-99-FIM
           MOVE LOW-VALUES TO SP2-FD-DATA
           PERFORM UNTIL FIELDX = 0
                   MOVE FD-ID (FIELDX) TO SP2-FD-ID
                   CALL "CWSPID" USING SP2-FD-ID "FDelete"
                   IF   COMBO NOT = 1
                        PERFORM 041-DROP-FIELD THRU 041-99-FIM
                   END-IF
                   SUBTRACT 1 FROM FIELDX
           END-PERFORM
           IF   COMBO = 1
                CALL SP2 USING SP2-CLOSE-WINDOW SP2-WINDOW-DEF
                MOVE CORRENTE  TO SP2-ND-NAME
                move X'00'     to sp2-cd-switch-sw
                CALL SP2 USING SP2-ACTIVATE-WINDOW SP2-NAME-DEF
                PERFORM DD THRU FIM-DD
                if sp2-cd-key = SP2-KEY-ENTER
                and (guia not = 1)
                   move 1000 to sp2-cd-key
                   MOVE HIDE-GUIA TO CURSOR-GUIA
                end-if
           ELSE
                MOVE GD-ID       TO SP2-GD-ID
                CALL SP2   USING SP2-DELETE-GROUP SP2-GROUP-DEF
                CALL "CWSPID" USING SP2-GD-ID "GDelete"
           END-IF.

       700-990-FIM. EXIT.

       710-CANCEL-PROVIDER.

           IF  (WORKBOX-PROGRAM NOT = "CWREAD")
           AND (WORKBOX-PROGRAM NOT = SPACES)
                CALL WORKBOX-PROGRAM USING "C" WORKBOX-ORDER
                                           STRING-1
                                           STRING-2
                                           WORKBOX-VERTICAL-LENGTH
                                           WORKBOX-WORK-AREA
                CANCEL WORKBOX-PROGRAM
           END-IF.

       710-99-FIM. EXIT.

       720-XE5.

           ACCEPT AGORA FROM TIME
           IF AGORA NOT = TIME-E5
              CALL X"E5"
              MOVE AGORA TO TIME-E5
           END-IF.

       720-99-FIM. EXIT.

       800-INICIAIS.

           ON 1
              open i-o WINWORK
              initialize WINWORK-REG
              write WINWORK-REG
              CALL "CWGETL"               USING PARAMETROS-CWGETL
              OPEN I-O TELA
              OPEN I-O CAMPOS
              OPEN I-O SAVES
              OPEN I-O THUMBS
              OPEN I-O OBJWORK
              INITIALIZE TELA-REG CAMPOS-REG
              move    low-values             to SCREEN-CONTROL
              MOVE    1                      TO CURPOS-LIN
                                                CURPOS-COL
              DISPLAY "CONSOLE"            UPON ENVIRONMENT-NAME
              ACCEPT  CWCONSOLE            FROM ENVIRONMENT-VALUE
              INSPECT CWCONSOLE
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWFOREBACK"         UPON ENVIRONMENT-NAME
              ACCEPT  FOREBACK             FROM ENVIRONMENT-VALUE
              INSPECT FOREBACK
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWBOR-TYPE"         UPON ENVIRONMENT-NAME
              ACCEPT  BOR-TYPE             FROM ENVIRONMENT-VALUE
              IF BOR-TYPE = '0'
                 MOVE X'00'                  TO BOR-TYPE
              END-IF
              DISPLAY "CWFIND"             UPON ENVIRONMENT-NAME
              ACCEPT  CWFIND               FROM ENVIRONMENT-VALUE
              INSPECT CWFIND
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              MOVE    HIGH-VALUES            TO SP2-FO-ID(1:1)
              DISPLAY "CWENTER-TERMINATE"  UPON ENVIRONMENT-NAME
              ACCEPT  ENTER-TERMINATE      FROM ENVIRONMENT-VALUE
              INSPECT ENTER-TERMINATE
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWBEEP"             UPON ENVIRONMENT-NAME
              ACCEPT  CWBEEP               FROM ENVIRONMENT-VALUE
              INSPECT CWBEEP
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWGUIECHO"          UPON ENVIRONMENT-NAME
              ACCEPT  GUIECHO              FROM ENVIRONMENT-VALUE
              INSPECT GUIECHO
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWPUSH-FONT"        UPON ENVIRONMENT-NAME
              ACCEPT  CWPUSH-FONT          FROM ENVIRONMENT-VALUE
              DISPLAY "CWPUSH-WIDTH"       UPON ENVIRONMENT-NAME
              PERFORM 810-AJUSTA           THRU 810-99-FIM
              MOVE    NUMERO                 TO CWPUSH-WIDTH
              DISPLAY "CWPUSH-HEIGHT "     UPON ENVIRONMENT-NAME
              PERFORM 810-AJUSTA           THRU 810-99-FIM
              MOVE    NUMERO                 TO CWPUSH-HEIGHT
              DISPLAY "CWPUSH-FIXED "      UPON ENVIRONMENT-NAME
              ACCEPT  CWPUSH-FIXED         FROM ENVIRONMENT-VALUE
              INSPECT CWPUSH-FIXED
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWPUSH-BOLD "       UPON ENVIRONMENT-NAME
              ACCEPT  CWPUSH-BOLD          FROM ENVIRONMENT-VALUE
              INSPECT CWPUSH-BOLD
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWPUSH-ITALIC "     UPON ENVIRONMENT-NAME
              ACCEPT  CWPUSH-ITALIC        FROM ENVIRONMENT-VALUE
              INSPECT CWPUSH-ITALIC
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWPUSH-STRIKE-OUT " UPON ENVIRONMENT-NAME
              ACCEPT  CWPUSH-STRIKE-OUT    FROM ENVIRONMENT-VALUE
              INSPECT CWPUSH-STRIKE-OUT
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWPUSH-UNDERLINE "  UPON ENVIRONMENT-NAME
              ACCEPT  CWPUSH-UNDERLINE     FROM ENVIRONMENT-VALUE
              INSPECT CWPUSH-UNDERLINE
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWUPDATE"           UPON ENVIRONMENT-NAME
              ACCEPT  CWUPDATE             FROM ENVIRONMENT-VALUE
              INSPECT CWUPDATE
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWUSER-FIELD"       UPON ENVIRONMENT-NAME
              ACCEPT  CWUSER-FIELD         FROM ENVIRONMENT-VALUE
              DISPLAY "CWGUICOLOR"         UPON ENVIRONMENT-NAME
              ACCEPT  GUICOLOR             FROM ENVIRONMENT-VALUE
              INSPECT GUICOLOR
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWGUICOLOR-ACCEPT"  UPON ENVIRONMENT-NAME
              ACCEPT GUICOLOR-ACCEPT       FROM ENVIRONMENT-VALUE
              INSPECT GUICOLOR-ACCEPT
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWRESOLUTION"       UPON ENVIRONMENT-NAME
              ACCEPT RESOLUTION            FROM ENVIRONMENT-VALUE
              DISPLAY "CWFONT-FIELD-NAME"  UPON ENVIRONMENT-NAME
              ACCEPT  MFF                  FROM ENVIRONMENT-VALUE
              DISPLAY "CWFONT-STATIC-NAME" UPON ENVIRONMENT-NAME
              ACCEPT  MFS                  FROM ENVIRONMENT-VALUE
              DISPLAY "CWFONT-MODE"        UPON ENVIRONMENT-NAME
              ACCEPT  FONT-MODE            FROM ENVIRONMENT-VALUE
              DISPLAY "CWFONT-STATIC-MODE" UPON ENVIRONMENT-NAME
              ACCEPT  FONT-MODE            FROM ENVIRONMENT-VALUE
              INSPECT FONT-MODE
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWAUTO"             UPON ENVIRONMENT-NAME
              ACCEPT  CWAUTO               FROM ENVIRONMENT-VALUE
              INSPECT CWAUTO
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWCONTROL"          UPON ENVIRONMENT-NAME
              ACCEPT  CWCONTX              FROM ENVIRONMENT-VALUE
              INSPECT CWCONTX
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              COPY CWSPPD.
              CALL "CWATTR" USING STATIC ENTRY-ATTR
                                  DISABLE-ATTR CURCOLOR
              IF    CURCOLOR > X"01"
                    MOVE CURCOLOR            TO OK2
              END-IF
              DISPLAY "CWATTRIBUTE"        UPON ENVIRONMENT-NAME
              PERFORM 810-AJUSTA
              IF  NUMERO NOT = ZERO
                  MOVE NUMERO                TO MB-ATTRIB-N
              END-IF
              DISPLAY "ECHOCURSOR"         UPON ENVIRONMENT-NAME
              ACCEPT  ECHOCURSOR           FROM ENVIRONMENT-VALUE
              INSPECT ECHOCURSOR
                      CONVERTING "ofn"       TO "OFN"
              DISPLAY "ECHODELETED"        UPON ENVIRONMENT-NAME
              ACCEPT  ECHODELETED          FROM ENVIRONMENT-VALUE
              INSPECT ECHODELETED
                      CONVERTING "ofn"       TO "OFN"
              DISPLAY "CWCASE"             UPON ENVIRONMENT-NAME
              ACCEPT  CWCASE               FROM ENVIRONMENT-VALUE
              INSPECT CWCASE
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWCASE-INPUT"       UPON ENVIRONMENT-NAME
              ACCEPT  CWCASE-INPUT         FROM ENVIRONMENT-VALUE
              IF  CWCASE-INPUT = SPACES
                  DISPLAY "CWCASE-INPUT"   UPON ENVIRONMENT-NAME
                  ACCEPT CWCASE-INPUT      FROM ENVIRONMENT-VALUE
              END-IF
              INSPECT CWCASE-INPUT
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWLITS"             UPON ENVIRONMENT-NAME
              ACCEPT  CWLITS               FROM ENVIRONMENT-VALUE
              INSPECT CWLITS
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWACCENT"           UPON ENVIRONMENT-NAME
              ACCEPT  CWACCENT             FROM ENVIRONMENT-VALUE
              INSPECT CWACCENT
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              CALL "CWMOLD" USING TABELA-CORES TABELA-MOLDURA
              MOVE COR (049) TO ATT-0
              MOVE COR (113) TO ATT-P
              MOVE COR (128) TO INTENSO
              MOVE BASE-MOLDURA (2) TO MOLDURA
              INSPECT MM-205    CONVERTING SPACE TO M-205
              INSPECT ATTR-LOW  CONVERTING X"00" TO COR (113)
              INSPECT ATTR-HIGH CONVERTING X"00" TO COR (128)
              DISPLAY "CWFULL"             UPON ENVIRONMENT-NAME
              ACCEPT  PANO-COBOL           FROM ENVIRONMENT-VALUE
              MOVE    PANO-COBOL             TO FULL-ON
              INSPECT FULL-ON
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              IF  PANO-COBOL = SPACES OR FULL-ON = 'OFF'
                  MOVE "Full.bmp" TO PANO-COBOL
                                     FUNDO
              END-IF
              DISPLAY "CWTOP-BMP"          UPON ENVIRONMENT-NAME
              ACCEPT  PANO-COBWARE         FROM ENVIRONMENT-VALUE
              MOVE    PANO-COBWARE           TO TOP-ON
              INSPECT TOP-ON
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              IF     PANO-COBWARE = SPACES OR TOP-ON = 'OFF'
                     MOVE "top.bmp" TO PANO-COBWARE
              END-IF
              DISPLAY "CWGUITOPLINES"      UPON ENVIRONMENT-NAME
              ACCEPT  TOPLINES(1:1)        FROM ENVIRONMENT-VALUE
              IF TOPLINES NUMERIC
                 COMPUTE T480 = TOPLINES * janwid
              ELSE
                 MOVE 6 TO TOPLINES
              END-IF
              DISPLAY "CWGUIOLD"          UPON ENVIRONMENT-NAME
              ACCEPT  OLD                 FROM ENVIRONMENT-VALUE
              IF OLD NOT = SPACES
                 DISPLAY "CWGUINEW"       UPON ENVIRONMENT-NAME
                 ACCEPT  NEW              FROM ENVIRONMENT-VALUE
                 PERFORM VARYING OLD-SIZE FROM LENGTH OF OLD
                              BY -1 UNTIL OLD(OLD-SIZE:1) NOT = SPACE
                         CONTINUE
                 END-PERFORM
              END-IF
              DISPLAY "CWLOGT"            UPON ENVIRONMENT-NAME
              ACCEPT  CWLOGT              FROM ENVIRONMENT-VALUE
              INSPECT CWLOGT
                      CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWRITEATTR"        UPON ENVIRONMENT-NAME
              ACCEPT  WRITEATTR           FROM ENVIRONMENT-VALUE
              INSPECT WRITEATTR
                      CONVERTING MINUSCULAS TO MAIUSCULAS
              DISPLAY "CWREMOVE"          UPON ENVIRONMENT-NAME
              ACCEPT  CWREMOVE            FROM ENVIRONMENT-VALUE
              IF   CWREMOVE NOT = SPACES
                   PERFORM VARYING CWRSIZE FROM LENGTH OF CWREMOVE
                              BY -1
                              UNTIL CWREMOVE (CWRSIZE: 1) NOT = SPACE
                   END-PERFORM
              END-IF
              DISPLAY "CWLINES"            UPON ENVIRONMENT-NAME
              ACCEPT  CWLINES              FROM ENVIRONMENT-VALUE
              DISPLAY "CWSHIFT71"          UPON ENVIRONMENT-NAME
              ACCEPT  FRAME                FROM ENVIRONMENT-VALUE
              INSPECT FRAME
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              IF  FRAME = "ON"
                  MOVE 0 TO SAV
              ELSE
                  MOVE 1 TO SAV
              END-IF
              MOVE    SPACES                 TO FRAME
              DISPLAY "CWFRAME"            UPON ENVIRONMENT-NAME
              ACCEPT  FRAME                FROM ENVIRONMENT-VALUE
              INSPECT FRAME
                      CONVERTING MINUSCULAS  TO MAIUSCULAS
              DISPLAY "CWMINLINE"          UPON ENVIRONMENT-NAME
              ACCEPT  MINLINE(1:2)         FROM ENVIRONMENT-VALUE.

           MOVE 0 TO X91-parameter-CWOBJE
           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER

           IF   X91-PARAMETER = 0
                GOBACK
           END-IF

           MOVE 01 TO TIMEOUT-RETURN

           IF   X91-PARAMETER = 5
                MOVE ZERO  TO WS-TIMEOUT
                MOVE 18    TO T
                PERFORM VARYING I FROM TIMEOUT-TAMANHO BY -1
                          UNTIL I = 0
                        IF TIMEOUT-SEGUNDOS (I: 1) NOT = LOW-VALUES
                           MOVE TIMEOUT-SEGUNDOS (I: 1)
                             TO WS-TIMEOUT       (T: 1)
                             SUBTRACT 1 FROM T
                        END-IF
                END-PERFORM
                IF   WS-TIMEOUT NOT = ZERO
                     MOVE 98 TO TIMEOUT-RETURN
                ELSE
                     MOVE 3  TO X91-PARAMETER
                END-IF
           END-IF.

       800-99-FIM. EXIT.

       810-AJUSTA.

           MOVE SPACES TO CWNUMERO
           MOVE ZERO   TO NUMERO
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

       810-99-FIM. EXIT.

       820-EVITA-BUG-SP2.

           IF  SP2-CD-WAIT-SW NOT = "1"
               MOVE SP2-CD-WAIT-SW   TO WAIT-SW
               MOVE SP2-CD-KEY       TO CD-KEY
               MOVE SP2-CD-BUTTON-ID TO BT-KEY
               MOVE "k"              TO SP2-CD-WAIT-SW
               CALL SP2           USING SP2-GET-INPUT SP2-CONVERSE-DATA
               MOVE WAIT-SW          TO SP2-CD-WAIT-SW
               MOVE CD-KEY           TO SP2-CD-KEY
               MOVE BT-KEY           TO SP2-CD-BUTTON-ID
           END-IF.

       820-99-FIM. EXIT.

       830-GETF.

           IF  SP2-CD-KEY = SP2-KEY-SELECT
               MOVE SP2-CD-NEXT-FLD-ID TO THUMBS-ID
               READ THUMBS
               IF   FS-THUMBS < '10'
                    SET ADDRESS THUMB TO THUMBS-THUMB
                    MOVE Buffer(THUMBS-POSITION:5)
                      TO THUMB
                    MOVE BUTTON-ON TO SP2-CD-KEY
                    GO TO 830-99-FIM
               END-IF
           END-IF
           move DATANAMES-ID to sv-datanames-id
           IF  SP2-CD-KEY NOT = BUTTON-ON
               MOVE SP2-CD-NEXT-FLD-ID TO DATANAMES-ID
           ELSE
               MOVE SP2-CD-LAST-FLD-ID TO DATANAMES-ID
           END-IF
           READ DATANAMES
           if fs-datanames not = '00'
              move sv-datanames-id to DATANAMES-ID
              read datanames
           end-if
           MOVE DATANAMES-ID        TO CAMPOS-FD-ID
           READ CAMPOS KEY IS CAMPOS-ID
           MOVE "S"                 TO CWGETF-FUNCTION
           MOVE DATANAMES-FIELD     TO CWGETF-FIELD
           MOVE 0                   TO CWGETF-LENGTH
                                       CWGETF-SUBSCRIPT TEST-GUIA
           PERFORM VARYING I FROM 1 BY 1
                             UNTIL TEST-GUIA = DATANAMES-GUIA
                   IF ACCEPT-LK (I)
                      ADD 1          TO TEST-GUIA
                      ADD LEN-LK (I) TO CWGETF-LENGTH
                      IF DATANAME-LK (I) = CWGETF-FIELD
                         ADD 1 TO CWGETF-SUBSCRIPT
                      END-IF
                   END-IF
           END-PERFORM
           MOVE 0 TO RESTO
           PERFORM VARYING I FROM I BY 1 UNTIL I > FIELDS
                   IF ACCEPT-LK (I)
                      MOVE LEN-LK (I) TO LENC
                      IF DATA-LK(I) (I:LENC) NOT = SPACES
                         ADD LEN-LK (I) TO CWGETF-LENGTH
                         ADD RESTO      TO CWGETF-LENGTH
                         MOVE 0         TO RESTO
                      ELSE
                         ADD LEN-LK (I) TO RESTO
                      END-IF
                   END-IF
           END-PERFORM
           CALL "CWGETF" USING PARAMETROS-CWGETF.

       830-99-FIM. EXIT.

       X11-DISPLAY-LIST.

           SET CWOBJE-GET              TO TRUE
           MOVE HIDE-OBJECT            TO CWOBJE-oc-NUMBER
           MOVE 3                      TO X91-parameter-CWOBJE
           PERFORM 670-CWOBJE        THRU 670-99-FIM
           MOVE 2                      TO X91-parameter-CWOBJE
           MOVE CWOBJE-PROGRAM         TO WORKBOX-PROGRAM
           MOVE CWOBJE-RETURN          TO WORKBOX-RETURN
                                          WORKBOX-ORDER
           MOVE CWOBJE-VERTICAL-LENGTH TO WORKBOX-VERTICAL-LENGTH
           MOVE CWOBJE-WORK-AREA       TO WORKBOX-WORK-AREA
           PERFORM 510-CALL-PROVIDER THRU 510-99-FIM
           IF   CWOBJE-RETURN < 2
                MOVE DATA-LK (FIELD) TO STRING-1
                IF  PIC-LK (FIELD)(1:1) = "Z"
                    INSPECT STRING-1 (1: LEN-LK(FIELD))
                            CONVERTING SPACE TO "0"
                END-IF
                MOVE STRING-1 TO TEST-OPTION
           ELSE
                MOVE DATA-LK (FIELD) TO STRING-2
                IF  PIC-LK (FIELD)(1:1) = "Z"
                    INSPECT STRING-2 (1: LEN-LK(FIELD))
                            CONVERTING SPACE TO "0"
                END-IF
                MOVE STRING-2 TO TEST-OPTION
           END-IF
           INSPECT TEST-OPTION CONVERTING X'0030' TO X'2020'
           MOVE "N" TO FIM
           SET NOT-LESS TO TRUE
           PERFORM 650-LER THRU 650-99-FIM
           IF NOT AT-END
              SET READ-NEXT TO TRUE
              PERFORM 650-LER THRU 650-99-FIM
           END-IF
           IF   CWOBJE-RETURN < 2
                move CWOBJE-STRING-1-LENGTH to lenc
                IF   lenc > 0
                and  STRING-1(1:lenc) NOT = DATA-LK (FIELD)(1:lenc)
                     MOVE DATA-LK (FIELD)(1:LENC) TO STRING-1
                     IF  PIC-LK (FIELD)(1:1) = "Z"
                         INSPECT STRING-1 (1: lenc)
                                 CONVERTING SPACE TO "0"
                     END-IF
                     MOVE SPACES          TO STRING-2
                END-IF
           ELSE
                move CWOBJE-STRING-2-LENGTH to lenc
                IF   lenc > 0
                and  STRING-2(1:lenc) NOT = DATA-LK (FIELD)(1:lenc)
                     MOVE DATA-LK (FIELD)(1:LENC) TO STRING-2
                     IF  PIC-LK (FIELD)(1:1) = "Z"
                         INSPECT STRING-2 (1: lenc)
                                 CONVERTING SPACE TO "0"
                     END-IF
                     MOVE SPACES          TO STRING-1
                END-IF
           END-IF

           MOVE SPACES TO HIDE-buffer
           MOVE 1      TO P

           IF   TEST-OPTION = SPACES
                IF   CWOBJE-RETURN < 2
                     IF (STRING-1 NOT = DATA-LK (FIELD))
                     OR STRING-1 = SPACES
                     OR STRING-2 = SPACES
                        MOVE SPACES TO HIDE-buffer  STRINGS-1-2
                     END-IF
                ELSE
                     IF (STRING-2 NOT = DATA-LK (FIELD))
                     OR STRING-1 = SPACES
                     OR STRING-2 = SPACES
                        MOVE SPACES TO HIDE-buffer  STRINGS-1-2
                     END-IF
                END-IF
           END-IF

           IF  CWOBJE-STRING-1-LENGTH NOT = 0
               MOVE STRING-1 TO HIDE-buffer  (P: )
               ADD  CWOBJE-STRING-1-LENGTH TO P
           END-IF

           IF  CWOBJE-STRING-2-LENGTH NOT = 0
               MOVE STRING-2 TO HIDE-buffer  (P: )
               ADD  CWOBJE-STRING-2-LENGTH TO P
           END-IF

           IF  HIDE-POS NOT = 0
               MOVE HIDE-buffer  TO buffer (HIDE-POS: P)
               PERFORM DD THRU FIM-DD
           END-IF

           MOVE LIN-LK (FIELD) TO X
           MOVE COL-LK (FIELD) TO Y
           PERFORM 710-CANCEL-PROVIDER THRU 710-99-FIM

           IF   FLAG-SAVE  = 1
                MOVE HIDE-POS         TO HIDE-POS2
                MOVE 0                TO HIDE-POS
           END-IF.

       X11-99-FIM. EXIT.

       PANO-FUNDO.

            EVALUATE TRUE
                WHEN POP-WINDOW NOT = 7
                     CONTINUE
                WHEN cwuser-CHARACTERS (1: T480) NOT = SPACES
                  OR(cwscre-FDS        (1: T480) NOT = LOW-VALUES)
                  OR(TELA-FIELDS-MAP   (1: T480 * 2) NOT = LOW-VALUES)
                     PERFORM FUNDO-OFF THRU FIM-FUNDO-OFF
                WHEN cwuser-CHARACTERS = SPACES
                AND  cwscre-FDS        = LOW-VALUES
                AND (TELA-FIELDS-MAP   = LOW-VALUES)
                     IF   FUNDO NOT = PANO-COBOL
                          PERFORM FUNDO-OFF THRU FIM-FUNDO-OFF
                          MOVE PANO-COBOL TO FUNDO
                          PERFORM CHECK-FUNDO THRU FIM-CHECk-FUNDO
                     END-IF
                     DISPLAY "CWMENU-ACTIVE" UPON ENVIRONMENT-NAME
                     ACCEPT   CWMENU-ACTIVE  FROM ENVIRONMENT-VALUE
                     IF  CWMENU-ACTIVE NOT = "NO"
                         PERFORM FUNDO-ON  THRU FIM-FUNDO-ON
                     END-IF
                WHEN cwuser-CHARACTERS (1: T480) = SPACES
                 AND(cwscre-FDS        (1: T480) = LOW-VALUES)
                 AND(TELA-FIELDS-MAP   (1: T480 * 2) = LOW-VALUES)
                     IF   FUNDO NOT = PANO-COBWARE
                          PERFORM FUNDO-OFF THRU FIM-FUNDO-OFF
                          MOVE PANO-COBWARE TO FUNDO
                          PERFORM CHECK-FUNDO THRU FIM-CHECk-FUNDO
                     END-IF
                     PERFORM FUNDO-ON  THRU FIM-FUNDO-ON
            END-EVALUATE.

       FIM-PANO-FUNDO. EXIT.

       FUNDO-ON.

           IF   FUNDO = SPACES
           OR  (FUNDO-ID NOT = 0)
           OR   BMP-OFF
           OR ( POP-WINDOW NOT = 7)
                GO TO FIM-FUNDO-ON
           END-IF

           MOVE LOW-VALUES  TO SP2-FD-DATA
                               SP2-FD-VAR-LENS
           IF  FUNDO = PANO-COBWARE
               MOVE 0 TO TOPHEIGHT
               MOVE 1 TO LTOP
               PERFORM UNTIL LTOP > TOPLINES
                         OR (GUI-LIN (LTOP) NOT = SPACES)
                       ADD 10 TO TOPHEIGHT
                       ADD 1  TO LTOP
               END-PERFORM
               MOVE TOPHEIGHT TO SP2-FD-HEIGHT
           ELSE
               MOVE 250     TO SP2-FD-HEIGHT
               IF  CWLINES = "25"
                   MOVE 260  TO SP2-FD-HEIGHT
               END-IF
           END-IF
           IF  SP2-FD-HEIGHT = TOPHEIGHT
               MOVE PANO-COBWARE TO FUNDO
           ELSE
               MOVE PANO-COBOL   TO FUNDO
           END-IF
           PERFORM CHECK-FUNDO THRU FIM-CHECk-FUNDO
           IF (FULL-ON = "OFF" AND SP2-FD-HEIGHT > 60)
           OR ( TOP-ON = "OFF" AND SP2-FD-HEIGHT = 60)
               GO TO FIM-FUNDO-ON
           END-IF
           CALL "CWSPID"   USING SP2-FD-ID "FInsert"
           MOVE SP2-FD-ID     TO FUNDO-ID
           COMPUTE SP2-FD-WIDTH = janwid * 10
           MOVE "o"           TO SP2-FD-OUTPUT
           MOVE "n"           TO SP2-FD-CURS-SHOW
           MOVE "i"           TO SP2-FD-CTRL-TYPE
           MOVE SP2-KEY-ENTER TO SP2-FD-HELP-KEY
           MOVE SPACES        TO SP2-FD-VAR-DATA
           IF   FUNDO-EXT = 'bmp'
                MOVE FUNDO          TO SP2-FD-VAR-DATA (9: )
           ELSE
                CALL "CWWBMP" USING FUNDO SP2-FD-VAR-DATA (9: )
           END-IF
           MOVE 58    TO SP2-FD-INITIAL-LEN
           MOVE "f"   to SP2-FD-SPEC-FMT
           MOVE SP2-FD-INITIAL-LEN TO SP2-FD-VAR-LEN
           CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           PERFORM DD THRU FIM-DD
           IF   FUNDO-EXT NOT = 'bmp'
                CALL "CBL_DELETE_FILE"
                USING SP2-FD-VAR-DATA (9: )
           END-IF.

       FIM-FUNDO-ON. EXIT.

       FUNDO-OFF.

           IF  (FUNDO-ID NOT = 0)
           AND  POP-WINDOW = 7
                MOVE FUNDO-ID    TO SP2-FD-ID
                MOVE 0           TO FUNDO-ID
                CALL "CWSPID" USING SP2-FD-ID "FDelete"
                CALL SP2      USING SP2-DELETE-FIELD SP2-FIELD-DEF
                PERFORM DD THRU FIM-DD
           END-IF.

       FIM-FUNDO-OFF. EXIT.

       FUNDO-REFRESH.

           IF   BMP-ON
           AND  FUNDO-ID > 0
           AND  POP-WINDOW = 7
                DISPLAY "CWFULL"  UPON ENVIRONMENT-NAME
                ACCEPT PANO-COBOL FROM ENVIRONMENT-VALUE
                MOVE   PANO-COBOL   TO FUNDO
                PERFORM CHECK-FUNDO THRU FIM-CHECk-FUNDO
                MOVE   LOW-VALUES   TO SP2-FD-DATA
                                       SP2-FD-VAR-LENS
                MOVE janwid             TO SP2-FD-VAR-LEN
                MOVE FUNDO-ID       TO SP2-FD-ID
                CALL SP2         USING SP2-GET-FIELD-DEF
                                       SP2-FIELD-DEF
                IF   FUNDO-EXT = 'bmp'
                     MOVE FUNDO          TO SP2-FD-VAR-DATA (9: )
                ELSE
                     CALL "CWWBMP" USING FUNDO SP2-FD-VAR-DATA (9: )
                END-IF
                MOVE 58             TO SP2-FD-INITIAL-LEN
                CALL SP2         USING SP2-SET-FIELD-DEF
                                       SP2-FIELD-DEF
                IF   FUNDO-EXT NOT = 'bmp'
                     CALL "CBL_DELETE_FILE"
                     USING SP2-FD-VAR-DATA (9: )
                END-IF
           END-IF.

       FIM-REFRESH-ON. EXIT.

       CHECK-FUNDO.

           MOVE SPACES TO FUNDO-EXT
           PERFORM VARYING FUN FROM 1 BY 1 UNTIL FUN > LENGTH FUNDO
                   IF FUNDO (FUN: 1) = '.'
                      ADD 1 TO FUN
                      MOVE FUNDO (FUN: ) TO FUNDO-EXT
                      INSPECT FUNDO-EXT CONVERTING MAIUSCULAS
                                        TO MINUSCULAS
                   END-IF
           END-PERFORM.

       FIM-CHECk-FUNDO. EXIT.

       call-CWSCRE.

           EVALUATE TRUE
               WHEN CWSCRE-INSERT-GROUP
                    MOVE cwuser-LINE   TO X
                    MOVE cwuser-COLUMN TO Y
                    COMPUTE K = cwuser-WIDTH - 2
                    MOVE "Ú" TO FD-LIN (X) (Y: 1)
                    PERFORM VARYING Y2 FROM janwid BY -1 UNTIL Y2 = 0
                                 OR (cwuser-CAPTION(Y2: 1) NOT = SPACE)
                            CONTINUE
                    END-PERFORM
                    MOVE 0 TO Y3
                    PERFORM K TIMES
                              ADD 1 TO Y
                              IF Y2 > 0
                              AND Y3 < Y2
                                 ADD 1 TO Y3
                                 MOVE cwuser-CAPTION(Y3: 1)
                                  TO FD-LIN (X) (Y: 1)
                              ELSE
                                  MOVE "Ä" TO FD-LIN (X) (Y: 1)
                              END-IF
                    END-PERFORM
                    ADD 1 TO Y
                    MOVE "¿" TO FD-LIN (X) (Y: 1)
                    MOVE cwuser-LINE   TO X
                    MOVE cwuser-COLUMN TO Y
                    IF  cwuser-HEIGHT < 3
                        MOVE 1 TO K
                    ELSE
                        COMPUTE K = cwuser-HEIGHT - 2
                    END-IF
                    COMPUTE Y2 = Y + cwuser-WIDTH - 1
                    PERFORM K TIMES
                              ADD 1 TO X
                              MOVE "³" TO FD-LIN (X) (Y: 1)
                                          FD-LIN (X) (Y2: 1)
                    END-PERFORM
                    ADD 1              TO X
                    MOVE cwuser-COLUMN TO Y
                    COMPUTE K = cwuser-WIDTH - 2
                    MOVE "À" TO FD-LIN (X) (Y: 1)
                    PERFORM K TIMES
                              ADD 1 TO Y
                              MOVE "Ä" TO FD-LIN (X) (Y: 1)
                    END-PERFORM
                    ADD 1 TO Y
                    MOVE "Ù" TO FD-LIN (X) (Y: 1)
               WHEN CWSCRE-DELETE-GROUP
                    MOVE cwuser-LINE   TO X
                    MOVE cwuser-COLUMN TO Y
                    COMPUTE K = cwuser-WIDTH - 4
                    MOVE X"00" TO FD-LIN (X) (Y: 1)
                    PERFORM K TIMES
                              ADD 1 TO Y
                              MOVE X"00" TO FD-LIN (X) (Y: 1)
                    END-PERFORM
                    ADD 1 TO Y
                    MOVE X"00" TO FD-LIN (X) (Y: 1)
                    MOVE cwuser-LINE   TO X
                    MOVE cwuser-COLUMN TO Y
                    IF  cwuser-HEIGHT < 3
                        MOVE 1 TO K
                    ELSE
                        COMPUTE K = cwuser-HEIGHT - 2
                    END-IF
                    COMPUTE Y2 = Y + cwuser-WIDTH - 1
                    PERFORM K TIMES
                              ADD 1 TO X
                              MOVE ALL X"00" TO FD-LIN (X)
                                                (Y: cwuser-WIDTH)
                                                FD-LIN (X) (Y2: 1)
                    END-PERFORM
                    ADD 1              TO X
                    MOVE cwuser-COLUMN TO Y
                    COMPUTE K = cwuser-WIDTH - 4
                    MOVE X"00" TO FD-LIN (X) (Y: 1)
                    PERFORM K TIMES
                              ADD 1 TO Y
                              MOVE X"00" TO FD-LIN (X) (Y: 1)
                    END-PERFORM
                    ADD 1 TO Y
                    MOVE X"00" TO FD-LIN (X) (Y: 1)
               WHEN CWSCRE-INSERT-TEXT
                    MOVE cwuser-LINE   TO X
                    MOVE cwuser-COLUMN TO Y
                    PERFORM VARYING Y2 FROM janwid BY -1 UNTIL Y2 = 0
                                 OR (cwuser-CAPTION(Y2: 1) NOT = SPACE)
                            CONTINUE
                    END-PERFORM
                    MOVE cwuser-CAPTION TO FD-LIN (X) (Y: Y2)
               WHEN CWSCRE-DELETE-TEXT
                    MOVE cwuser-LINE   TO X
                    MOVE cwuser-COLUMN TO Y
                    PERFORM VARYING Y2 FROM janwid BY -1 UNTIL Y2 = 0
                                 OR (cwuser-CAPTION(Y2: 1) NOT = SPACE)
                            CONTINUE
                    END-PERFORM
                    MOVE ALL X"00" TO FD-LIN (X) (Y: Y2)
           END-EVALUATE

           MOVE cwuser-LINE   TO X
           MOVE cwuser-COLUMN TO Y.

       FIM-call-CWSCRE. EXIT.

       SEND-SP2.

           MOVE "b"      TO SP2-MS-ICON
           MOVE "o"      TO SP2-MS-BUTTON
           MOVE MSG-D    TO SP2-MS-TEXT
           MOVE "Aviso:" TO SP2-MS-TITLE
           MOVE 1        TO SP2-MS-LINE-CNT
           PERFORM ACENTOS-MS THRU END-ACENTOS-MS
           CALL SP2   USING SP2-DISPLAY-MESSAGE
                            SP2-MESSAGE-DATA.

       END-SEND-SP2.

       ACENTOS-MS.

           IF   CWLITS = "LOW"
                INSPECT SP2-MS-TEXT
                        CONVERTING MAIUSCULAS TO MINUSCULAS
                INSPECT SP2-MS-TITLE
                        CONVERTING MAIUSCULAS TO MINUSCULAS
           END-IF
           IF   CWLITS = "UPP"
                INSPECT SP2-MS-TEXT
                        CONVERTING MINUSCULAS TO MAIUSCULAS
                INSPECT SP2-MS-TITLE
                        CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF
           IF   CWACCENT = "OFF"
                INSPECT SP2-MS-TEXT
                        CONVERTING ACENTOS-850 TO ACENTOS-OFF
                INSPECT SP2-MS-TITLE
                        CONVERTING ACENTOS-850 TO ACENTOS-OFF
           ELSE
                INSPECT SP2-MS-TEXT
                        CONVERTING ACENTOS-850 TO ACENTOS-WINDOWS
                INSPECT SP2-MS-TITLE
                        CONVERTING ACENTOS-850 TO ACENTOS-WINDOWS
           END-IF.

       END-ACENTOS-MS.
       drop-campo.

           if   campos-fd-id not = zero
                MOVE CAMPOS-FD-ID TO SP2-FD-ID
                CALL SP2 USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                CALL "CWSPID" USING SP2-FD-ID "FDelete"
                PERFORM 041-DROP-FIELD THRU 041-99-FIM
           end-if.

       fim-drop-campo. exit.
       dd.
           CALL SP2 USING SP2-SET-PANEL-FIELDS BUFFER
           CALL SP2 USING SP2-DISPLAY-WINDOW SP2-NULL-PARM.
       fim-dd. exit.
