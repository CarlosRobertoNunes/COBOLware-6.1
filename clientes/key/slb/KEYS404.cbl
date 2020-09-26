* Micro Focus Object COBOL           V4.0 revision 038 19-May-10 08:25 Page   1
*                      f:\cobol\slb\KEYS404.CBL
* Options: NOLIST NOASMLIST OMF(OBJ) OBJ(.\) CopyExt"CPY" NoBell
*          gnt(C:\CWTMP\28\KEYS404) obj(C:\CWTMP\28\KEYS404)
*          list(f:\cobol\slb\KEYS404.lst) NOASM
     1 IDENTIFICATION DIVISION.
     2 PROGRAM-ID.    KEYS404.
     3 AUTHOR.        KATIA.
     4 DATE-WRITTEN.  27/06/2002.
     5 SECURITY.      *************************************************
     6                *                                               *
     7                *  Padrao de programas de manutencao            *
     8                *  Fornece ocorrencia por    programa           *
     9                *                                               *
    10                *************************************************
    11 ENVIRONMENT DIVISION.
    12 CONFIGURATION SECTION.
    13 SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
    14 INPUT-OUTPUT SECTION.
    15 FILE-CONTROL.
    16
*   17 COPY KEYPMPRG.SL.
    18*---------------------------------------------------------------
    19* Label Fixo
    20* Arquivo de Parametros Especiais de Clientes KEY
    21*---------------------------------------------------------------
    22     SELECT KEYPMPRG
    23          ASSIGN TO DISK
    24          ORGANIZATION INDEXED
    25          ACCESS MODE IS DYNAMIC
    26          RECORD KEY IS KEYPMPRG-CHAVE
    27          FILE STATUS FS-KEYPMPRG.
*   28 COPY FATDSOCB.SL.
    29     SELECT FATDSOCB
    30          ASSIGN TO DISK
    31          ORGANIZATION INDEXED
    32          ACCESS MODE DYNAMIC
    33          LOCK MODE AUTOMATIC
    34          RECORD KEY FATDSOCB-CHAVE
    35          FILE STATUS FS-FATDSOCB.
    36
    37 DATA DIVISION.
    38 FILE SECTION.
    39
*   40 COPY KEYPMPRG.FD.
    41*---------------------------------------------------------------
    42* Label Fixo
    43* Arquivo de Parametros Especiais de Clientes KEY
    44*---------------------------------------------------------------
    45*    SELECT KEYPMPRG
    46*         ASSIGN TO DISK
    47*         ORGANIZATION INDEXED
    48*         ACCESS MODE IS DYNAMIC
    49*         RECORD KEY IS KEYPMPRG-CHAVE
    50*         FILE STATUS FS-KEYPMPRG.
    51
    52*  KATIA - 24/02/2010
    53*  CRIADA TERCEIRA LINHA DE OCORRENCIAS
    54*  PROGRAMAS ENVOLVIDOS: KEYS003, KEYS203, KEYS203X, KEYS404
    55
* Micro Focus Object COBOL           V4.0 revision 038 19-May-10 08:25 Page   2
*                      f:\cobol\slb\KEYS404.CBL (f:\cobol\SLBNF\BOOKN)
    56 FD  KEYPMPRG
    57     LABEL RECORD IS STANDARD
    58     VALUE OF FILE-ID LB-KEYPMPRG.
    59
    60 01  KEYPMPRG-REG.
    61     03 KEYPMPRG-CHAVE.
    62        05 KEYPMPRG-NOME-PADRAO       PIC X(008).
    63        05 KEYPMPRG-R-NOME-PADRAO  REDEFINES KEYPMPRG-NOME-PADRAO.
    64           07 KEYPMPRG-PREFIXO-PADRAO PIC X(005).
    65           07 KEYPMPRG-CODIGO-PADRAO  PIC X(003).
    66     03 KEYPMPRG-NOME-ESPECIAL        PIC X(008).
    67     03 KEYPMPRG-R-NOME-ESPECIAL REDEFINES KEYPMPRG-NOME-ESPECIAL.
    68        05 KEYPMPRG-PREFIXO-ESPECIAL  PIC X(005).
    69        05 KEYPMPRG-CODIGO-ESPECIAL   PIC X(003).
    70     03 KEYPMPRG-OCORRENCIAS          PIC 9(001) OCCURS 20 TIMES.
    71     03 KEYPMPRG-R-OCORRENCIAS    REDEFINES KEYPMPRG-OCORRENCIAS.
    72        05 KEYPMPRG-OCO1              PIC 9(010).
    73        05 KEYPMPRG-OCO2              PIC 9(010).
    74     03 KEYPMPRG-SISTEMA              PIC X(003).
    75     03 KEYPMPRG-OCORRENCIASX         PIC 9(001) OCCURS 20 TIMES.
    76     03 KEYPMPRG-R-OCORRENCIASX   REDEFINES KEYPMPRG-OCORRENCIASX.
    77        05 KEYPMPRG-OCO1X             PIC 9(010).
    78        05 KEYPMPRG-OCO2X             PIC 9(010).
    79     03 KEYPMPRG-ENVIA-EMAIL          PIC X(001).
    80        88 KEYPMPRG-SIM-EMAIL         VALUE "S" "s".
    81        88 KEYPMPRG-NAO-EMAIL         VALUE "N" "n" " ".
    82        88 KEYPMPRG-PERG-EMAIL        VALUE "P" "p" " ".
    83     03 KEYPMPRG-OCORRENCIASY         PIC 9(001) OCCURS 20 TIMES.
    84     03 KEYPMPRG-R-OCORRENCIASY   REDEFINES KEYPMPRG-OCORRENCIASY.
    85        05 KEYPMPRG-OCO1Y             PIC 9(010).
    86        05 KEYPMPRG-OCO2Y             PIC 9(010).
    87     03 KEYPMPRG-FILLER               PIC X(036).
    88
*   89 COPY FATDSOCB.FD.
    90*************************************************
    91*    Descricao das Ocorrencias por Programas    *
    92*************************************************
    93 FD  FATDSOCB
    94     LABEL RECORD IS STANDARD
    95     VALUE OF FILE-ID LB-FATDSOCB.
    96 01  FATDSOCB-REG.
    97     03 FATDSOCB-CHAVE.
    98        05 FATDSOCB-PROG-PDR    PIC X(08).
    99        05 FATDSOCB-PROG-ESP    PIC X(08).
   100        05 FATDSOCB-CODIGO      PIC 9(02).
   101     03 FATDSOCB-DESCRICAO-1    PIC X(60).
   102     03 FATDSOCB-DESCRICAO-2    PIC X(60).
   103     03 FATDSOCB-FILLER         PIC X(15).
   104
   105 WORKING-STORAGE SECTION.
   106
   107 01  AREAS-DE-TRABALHO-1.
   108     05 AUX-CODIGO          PIC 9(002) VALUE ZEROS.
   109     05 TRAVADO             PIC X(002) VALUE "9D".
   110     05 ER-KEYPMPRG.
   111        10 FS-KEYPMPRG      PIC  X(002) VALUE "00".
   112        10 LB-KEYPMPRG      PIC  X(050) VALUE "KEYPMPRG".
   113     05 ER-FATDSOCB.
* Micro Focus Object COBOL           V4.0 revision 038 19-May-10 08:25 Page   3
*                      f:\cobol\slb\KEYS404.CBL                      )
   114        10 FS-FATDSOCB      PIC  X(002) VALUE "00".
   115        10 LB-FATDSOCB      PIC  X(050) VALUE "FATDSOCB".
   116
*  117 COPY CPACOR.                                                     MS
   118
   119 01   PARAMETROS-GRACOR VALUE ALL "0701".
   120      05 GRACOR-CORES OCCURS 11.
   121         10 GRACOR-F                   PIC  9(002).
   122         10 GRACOR-B                   PIC  9(002).
   123      05 GRACOR-FUNCAO                 PIC  X(001).
   124
   125
   126 LINKAGE SECTION.
*  127 COPY KEYS404.LNK.
   128 01 LINK-KEYS404.
   129    03 LINK-KEYS404-PROGRAMA   PIC X(08).
   130    03 LINK-KEYS404-OCO1       PIC 9(10).
   131    03 LINK-KEYS404-OCO2       PIC 9(10).
   132    03 LINK-KEYS404-DESCR1     PIC X(60) OCCURS 20 TIMES.
   133    03 LINK-KEYS404-DESCR2     PIC X(60) OCCURS 20 TIMES.
   134    03 LINK-KEYS404-OCO1X      PIC 9(10).
   135    03 LINK-KEYS404-OCO2X      PIC 9(10).
   136    03 LINK-KEYS404-DESCR1X    PIC X(60) OCCURS 20 TIMES.
   137    03 LINK-KEYS404-DESCR2X    PIC X(60) OCCURS 20 TIMES.
   138    03 LINK-KEYS404-OCO1Y      PIC 9(10).
   139    03 LINK-KEYS404-OCO2Y      PIC 9(10).
   140    03 LINK-KEYS404-DESCR1Y    PIC X(60) OCCURS 20 TIMES.
   141    03 LINK-KEYS404-DESCR2Y    PIC X(60) OCCURS 20 TIMES.
   142
   143 PROCEDURE DIVISION USING LINK-KEYS404.
   144 DECLARATIVES.
   145
   146 DECLARATIVES-KEYPMPRG SECTION.
   147     USE AFTER ERROR PROCEDURE ON KEYPMPRG.
   148 CHECK-KEYPMPRG-TRAVADO.
   149     IF   FS-KEYPMPRG = TRAVADO
   150          CALL "GRISAM" USING ER-KEYPMPRG
   151     END-IF.
   152
   153 END DECLARATIVES.
   154
   155 000 SECTION.
   156 000-INICIO.
   157
   158     PERFORM 800-INICIAIS      THRU 800-99-FIM
   159     PERFORM 100-PROCESSAMENTO THRU 100-99-FIM.
   160     PERFORM 900-FINAIS        THRU 900-99-FIM.
   161
   162 000-99-FIM. GOBACK.
   163
   164 100-PROCESSAMENTO.
   165
   166     INITIALIZE KEYPMPRG-REG.
   167     MOVE LINK-KEYS404-PROGRAMA TO KEYPMPRG-NOME-PADRAO.
   168     PERFORM WITH TEST AFTER UNTIL FS-KEYPMPRG NOT = TRAVADO
   169        READ KEYPMPRG IGNORE LOCK
   170     END-PERFORM
   171     IF FS-KEYPMPRG < "10"
* Micro Focus Object COBOL           V4.0 revision 038 19-May-10 08:25 Page   4
*                      f:\cobol\slb\KEYS404.CBL                      )
   172        IF FS-FATDSOCB < "10"
   173           INITIALIZE FATDSOCB-REG
   174           MOVE LINK-KEYS404-PROGRAMA  TO FATDSOCB-PROG-PDR
   175           MOVE KEYPMPRG-NOME-ESPECIAL TO FATDSOCB-PROG-ESP
   176           PERFORM WITH TEST AFTER UNTIL FS-FATDSOCB NOT = TRAVADO
   177             START FATDSOCB KEY IS NOT LESS FATDSOCB-CHAVE
   178           END-PERFORM
   179           PERFORM TEST AFTER UNTIL FS-FATDSOCB > "09"
   180              PERFORM WITH TEST AFTER
   181                                   UNTIL FS-FATDSOCB NOT = TRAVADO
   182                 READ FATDSOCB IGNORE LOCK
   183              END-PERFORM
   184              IF (FS-FATDSOCB           > "10")
   185              OR (FATDSOCB-PROG-PDR NOT = LINK-KEYS404-PROGRAMA)
   186              OR (FATDSOCB-PROG-ESP NOT = KEYPMPRG-NOME-ESPECIAL)
   187                 MOVE "10" TO FS-FATDSOCB
   188              ELSE
   189                 IF FATDSOCB-CODIGO < 21
   190                    MOVE FATDSOCB-CODIGO TO AUX-CODIGO
   191                    MOVE FATDSOCB-DESCRICAO-1 TO
   192                              LINK-KEYS404-DESCR1(AUX-CODIGO)
   193                    MOVE FATDSOCB-DESCRICAO-2 TO
   194                              LINK-KEYS404-DESCR2(AUX-CODIGO)
   195                 ELSE
   196                    IF FATDSOCB-CODIGO < 41
   197                       COMPUTE AUX-CODIGO = FATDSOCB-CODIGO - 20
   198                       MOVE FATDSOCB-DESCRICAO-1 TO
   199                                 LINK-KEYS404-DESCR1X(AUX-CODIGO)
   200                       MOVE FATDSOCB-DESCRICAO-2 TO
   201                                 LINK-KEYS404-DESCR2X(AUX-CODIGO)
   202                    ELSE
   203                       COMPUTE AUX-CODIGO = FATDSOCB-CODIGO - 40
   204                       MOVE FATDSOCB-DESCRICAO-1 TO
   205                                 LINK-KEYS404-DESCR1X(AUX-CODIGO)
   206                       MOVE FATDSOCB-DESCRICAO-2 TO
   207                                 LINK-KEYS404-DESCR2X(AUX-CODIGO)
   208                    END-IF
   209                 END-IF
   210              END-IF
   211           END-PERFORM
   212        END-IF
   213        MOVE KEYPMPRG-OCO1 TO LINK-KEYS404-OCO1
   214        MOVE KEYPMPRG-OCO2 TO LINK-KEYS404-OCO2
   215        MOVE KEYPMPRG-OCO1X TO LINK-KEYS404-OCO1X
   216        MOVE KEYPMPRG-OCO2X TO LINK-KEYS404-OCO2X
   217        MOVE KEYPMPRG-OCO1Y TO LINK-KEYS404-OCO1Y
   218        MOVE KEYPMPRG-OCO2Y TO LINK-KEYS404-OCO2Y
   219     END-IF.
   220
   221 100-99-FIM. EXIT.
   222
   223 800-INICIAIS.
   224
   225     MOVE ZEROS TO LINK-KEYS404-OCO1
   226     MOVE ZEROS TO LINK-KEYS404-OCO2
   227     MOVE "9D"        TO TRAVADO                                  MS
   228     CALL "GRACOR" USING PARAMETROS-GRACOR                        MS
   229     CALL "GRFILE" USING LB-KEYPMPRG
* Micro Focus Object COBOL           V4.0 revision 038 19-May-10 08:25 Page   5
*                      f:\cobol\slb\KEYS404.CBL                      )
   230
   231     OPEN INPUT KEYPMPRG
   232     IF   FS-KEYPMPRG > "09"
   233          PERFORM 900-FINAIS THRU 900-99-FIM                      *> FALTO
   234          GOBACK.
   235
   236     INITIALIZE KEYPMPRG-REG.
   237
   238     CALL "GRFILE" USING LB-FATDSOCB
   239     OPEN INPUT FATDSOCB
   240     INITIALIZE FATDSOCB-REG.
   241
   242 800-99-FIM. EXIT.
   243
   244 900-FINAIS.
   245     CANCEL "GRFILE"
   246     CANCEL "GRISAM"
   247     CANCEL "GRACOR"
   248     CLOSE KEYPMPRG.
   249     CLOSE FATDSOCB.
   250
   251 900-99-FIM. EXIT.
   252
   253 END PROGRAM KEYS404.
   254
   255
* Micro Focus Object COBOL           V4.0 revision 038
* Copyright (C) 1984-1998 Micro Focus Ltd.     URN OXCNM/000047913  
*                                              REF GNB-166056000AA
*
* Total Messages:     0
* Data:        1456     Code:        1348
