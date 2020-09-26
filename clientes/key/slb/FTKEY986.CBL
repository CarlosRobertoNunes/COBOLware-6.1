     1 IDENTIFICATION DIVISION.
     2 PROGRAM-ID.    FTKEY986.
     3 AUTHOR.        Thiago de Jesus.
     4 DATE-WRITTEN.  31/05/2010.
     5 SECURITY.      **************************************************
     6                *  LER CADASTROS DE PRODUTOS FTPROAUX            *
     7                *  Padrao de programas de apoio a CWBOXF         *
     8                *                                                *
     9                *  Solicitacao : 5763                            *
    10                *------------------------------------------------*
    11                *  Dt.Alteracao:                                 *
    12                *  Programador :                                 *
    13                *  Descricao   :                                 *
    14                *  Configuracao:                                 *
    15                *  ID.Modif.   :                                 *
    16                *  Solicitacao :                                 *
    17                *  Versao Ant. :                                 *
    18                **************************************************
    19 ENVIRONMENT    DIVISION.
    20 CONFIGURATION  SECTION.
    21 SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
    22 INPUT-OUTPUT   SECTION.
    23 FILE-CONTROL.
    24
    26** LABEL VALIAREL FATTAXnn
    27**
    28** AO ALTERAR ESTA SELECT, TAMBEM ALTERAR O QUE ESTA INTERNO NO
    29** PROGRAMA FATU057 AOTM333 FATU066 FATU853 FTKEY066 LCEX011
    30**
    31     SELECT FTPROAUX ASSIGN TO DISK
    32          ORGANIZATION INDEXED
    33          ACCESS MODE DYNAMIC
    34          LOCK MODE AUTOMATIC
    35          RECORD KEY FTPROAUX-CHAVE
    36          ALTERNATE RECORD KEY FTPROAUX-CHAVE1 =
    37                               FTPROAUX-DESCRICAO
    38                               WITH DUPLICATES
    39          ALTERNATE RECORD KEY FTPROAUX-CHAVE2 =
    40                               FTPROAUX-COD-ALMOX
    41                               WITH DUPLICATES
    42          ALTERNATE RECORD KEY FTPROAUX-CHAVE3 =
    43                               FTPROAUX-GRUPO
    44                               FTPROAUX-COD-ALMOX
    45                               WITH DUPLICATES
    46          ALTERNATE RECORD KEY FTPROAUX-CHAVE4 =
    47                               FTPROAUX-COD-EAN
    48                               WITH DUPLICATES
    49          ALTERNATE RECORD KEY FTPROAUX-CHAVE5 =
    50                               FTPROAUX-CODMARCA
    51                               FTPROAUX-NOME-BUSCA
    52                               WITH DUPLICATES
    53          ALTERNATE RECORD KEY FTPROAUX-CHAVE6 =
    54                               FTPROAUX-CODMARCA
    55                               FTPROAUX-DESCRICAO
    56                               WITH DUPLICATES
    57          FILE STATUS FS-FTPROAUX.
    58
    59
    60 DATA DIVISION.
    61 FILE SECTION.
    62
    64** ATENCAO
    65** AO ALTERAR ESTE BOOK, TAMBEM ALTERAR O QUE ESTA INTERNO NO
    66** PROGRAMA FATU057 E FATU066 AOTM333 FATU853 FTKEY066 LCEX011
    67**
    68** LABEL VALIAREL FATTAXnn
    69**
    70**
    71**   SELECT FTPROAUX ASSIGN TO DISK
    72**        ORGANIZATION INDEXED
    73**        ACCESS MODE DYNAMIC
    74**        LOCK MODE AUTOMATIC
    75**        RECORD KEY FTPROAUX-CHAVE
    76**        ALTERNATE RECORD KEY FTPROAUX-CHAVE1 =
    77**                             FTPROAUX-DESCRICAO
    78**                             WITH DUPLICATES
    79**        ALTERNATE RECORD KEY FTPROAUX-CHAVE2 =
    80**                             FTPROAUX-COD-ALMOX
    81**                             WITH DUPLICATES
    82**        ALTERNATE RECORD KEY FTPROAUX-CHAVE3 =
    83**                             FTPROAUX-GRUPO
    84**                             FTPROAUX-COD-ALMOX
    85**                             WITH DUPLICATES
    86**        ALTERNATE RECORD KEY FTPROAUX-CHAVE4 =
    87**                             FTPROAUX-COD-EAN
    88**                             WITH DUPLICATES
    89**        ALTERNATE RECORD KEY FTPROAUX-CHAVE5 =
    90**                             FTPROAUX-CODMARCA
    91**                             FTPROAUX-NOME-BUSCA
    92**                             WITH DUPLICATES
    93**        ALTERNATE RECORD KEY FTPROAUX-CHAVE6 =
    94**                             FTPROAUX-CODMARCA
    95**                             FTPROAUX-DESCRICAO
    96**                             WITH DUPLICATES
    97**        FILE STATUS FS-FTPROAUX.
    98*----------------------------------------------------------------*
    99*  Dt.Alteracao: 22/07/2009                                      *
   100*  Programador : Marco Guerra                                    *
   101*  Descricao   : Campos novos de peso                            *
   102*                FTPROAUX-PESO-ACESS   - Produto com acessorios  *
   103*                FTPROAUX-PESO-EMB-COL - Embalagem coletiva      *
   104*  ID.Modif.   : mg0001                                          *
   105*  O.Servico   : 4166                                            *
   106*----------------------------------------------------------------*
   107 FD  FTPROAUX
   108     LABEL RECORD IS STANDARD
   109     VALUE OF FILE-ID ID-FTPROAUX.
   110 01  FTPROAUX-REG.
   111     03 FTPROAUX-CHAVE.
   112        05 FTPROAUX-CODMARCA       PIC 9(001).
   113        05 FTPROAUX-CODIGO         PIC 9(007).
   114        05 FTPROAUX-DIGITO         PIC 9(001).
   115     03 FTPROAUX-DESCRICAO         PIC X(040).
   116     03 FTPROAUX-GRUPO             PIC 9(002).
   117     03 FTPROAUX-COD-ALMOX         PIC 9(009).
   118     03 FTPROAUX-COD-SERVICO       PIC 9(010).
   119     03 FTPROAUX-ALIQ-ISS          PIC 9(002)V99.
   120     03 FTPROAUX-APELIDO           PIC X(015).
   121     *>----------
   122     03 FTPROAUX-FLAG-ENVIO        PIC X(001).
   123     *> FLAG-ENVIO, USADA NA MARCOLAR PARA RESTRINGIR O REENVIO
   124     *> DO PRODUTO PARA AS TABELAS DO SITE - POWER DESIGN
   125     *>----------
   126     03 FTPROAUX-COD-EAN           PIC 9(013).
   127     03 FTPROAUX-CUSTO-REAIS       PIC 9(007)V9999.
   128     03 FTPROAUX-CUSTO-EURO        PIC 9(007)V9999.
   129     03 FTPROAUX-NOME-BUSCA        PIC X(015).
   130     03 FTPROAUX-FLAG-PROD         PIC X(001).
   131        88 LK-FTPROAUX-ATIVO       VALUE "A" " ".
   132        88 LK-FTPROAUX-INATIVO     VALUE "I".
   133        88 LK-FTPROAUX-COM-ERRO    VALUE "C".
   134     03 FTPROAUX-DIG-COD-EAN       PIC 9(001).
   135     03 FTPROAUX-PESO-ACESS        PIC 9(005)V9999.
   136     03 FTPROAUX-PESO-EMB-COL      PIC 9(005)V9999.
   137     03 FTPROAUX-DESCRICAO2        PIC X(040).
   138     03 FTPROAUX-DESCRICAO3        PIC X(040).
   139     03 FTPROAUX-DATACAD           PIC 9(008).
   140     03 R-FTPROAUX-DATACAD REDEFINES FTPROAUX-DATACAD.
   141        05 FTPROAUX-ANOCAD         PIC 9(004).
   142        05 FTPROAUX-MESCAD         PIC 9(002).
   143        05 FTPROAUX-DIACAD         PIC 9(002).
   144     03 FTPROAUX-DATAALT           PIC 9(008).
   145     03 R-FTPROAUX-DATAALT REDEFINES FTPROAUX-DATAALT.
   146        05 FTPROAUX-ANOALT         PIC 9(004).
   147        05 FTPROAUX-MESALT         PIC 9(002).
   148        05 FTPROAUX-DIAALT         PIC 9(002).
   149     03 FTPROAUX-FILLER            PIC X(184).
   150
   151 WORKING-STORAGE SECTION.
   152
   153 01  W-COD-PRODUTO.
   154     03 W-CODMARCA          PIC  9(01).
   155     03 FILLER-1            PIC  X(01).
   156     03 W-COD-PROD          PIC  9(07).
   157     03 FILLER-2            PIC  X(01).
   158     03 W-DIG-PROD          PIC  9(01).
   159     03 FILLER              PIC  X(01).
   160
   162 01 ID-FTPROAUX.
   163    03 ID-LITERALX        PIC X(09).
   164    03 ID-NUM-TABELAX     PIC 9(02).
   166 01  W-FATU908-OCO1                   PIC 9(10).
   167 01  W-R-FATU908-OCO1                 REDEFINES W-FATU908-OCO1.
   168     03 W-FATU908-OC01                PIC 9(01).
   169        88 W-FATU908-TODOS-PROD       VALUE 0.
   170        88 W-FATU908-PROD-CHAVE-YAM   VALUE 1.
   171     03 W-FATU908-OC02                PIC 9(01).
   172        88 W-FATU908-NOME-BUSCA       VALUE 0.
   173        88 W-FATU908-DESCRICAO        VALUE 1.
   174     03 W-FATU908-OC03                PIC 9(01).
   175        88 W-FATU908-ACC-PALAVRA      VALUE 1.
   176     03 W-FATU908-OC04                PIC 9(01).
   177        88 W-FATU908-N-MOSTRA-INATIVO VALUE 1.
   178     03 W-FATU908-OC05                PIC 9(01).
   179     03 W-FATU908-OC06                PIC 9(01).
   180     03 W-FATU908-OC07                PIC 9(01).
   181     03 W-FATU908-OC08                PIC 9(01).
   182     03 W-FATU908-OC09                PIC 9(01).
   183     03 W-FATU908-OC10                PIC 9(01).
   184
   185 01  W-FATU908-OCO2                   PIC 9(10).
   186 01  W-R-FATU908-OCO2                 REDEFINES W-FATU908-OCO2.
   187     03 W-FATU908-OC11                PIC 9(01).
   188     03 W-FATU908-OC12                PIC 9(01).
   189     03 W-FATU908-OC13                PIC 9(01).
   190     03 W-FATU908-OC14                PIC 9(01).
   191     03 W-FATU908-OC15                PIC 9(01).
   192     03 W-FATU908-OC16                PIC 9(01).
   193     03 W-FATU908-OC17                PIC 9(01).
   194     03 W-FATU908-OC18                PIC 9(01).
   195     03 W-FATU908-OC19                PIC 9(01).
   196     03 W-FATU908-OC20                PIC 9(01).
   198 01 LINK-KEYS404.
   199    03 LINK-KEYS404-PROGRAMA   PIC X(08).
   200    03 LINK-KEYS404-OCO1       PIC 9(10).
   201    03 LINK-KEYS404-OCO2       PIC 9(10).
   202    03 LINK-KEYS404-DESCR1     PIC X(60) OCCURS 20 TIMES.
   203    03 LINK-KEYS404-DESCR2     PIC X(60) OCCURS 20 TIMES.
   204    03 LINK-KEYS404-OCO1X      PIC 9(10).
   205    03 LINK-KEYS404-OCO2X      PIC 9(10).
   206    03 LINK-KEYS404-DESCR1X    PIC X(60) OCCURS 20 TIMES.
   207    03 LINK-KEYS404-DESCR2X    PIC X(60) OCCURS 20 TIMES.
   208    03 LINK-KEYS404-OCO1Y      PIC 9(10).
   209    03 LINK-KEYS404-OCO2Y      PIC 9(10).
   210    03 LINK-KEYS404-DESCR1Y    PIC X(60) OCCURS 20 TIMES.
   211    03 LINK-KEYS404-DESCR2Y    PIC X(60) OCCURS 20 TIMES.
   212
   213 01  W-VALIDO               PIC  9(001).
   214     88 VALIDO                   VALUE 1.
   215     88 NAO-VALIDO               VALUE 2.
   216
   217 01  W-LIBEROU              PIC  9(01).
   218     88 LIBEROU                  VALUE 1.
   219     88 NAO-LIBEROU              VALUE 2.
   220
   221 01  AREAS-DE-TRABALHO-1.
   222     05 REGISTROS           PIC  9(002) VALUE 0.
   223     05 CONTADOR            PIC  9(002) VALUE 0.
   224     05 VEZ                 PIC  9(001) VALUE 1.
   225     05 ORD-LEIT            PIC  9(001) VALUE 1.
   226        88 ORD-DESCRICAO         VALUE 1.
   227        88 ORD-CODIGO            VALUE 2.
   228*       88 ORD-COD-EAN           VALUE 3.
   229     05 ER-FTPROAUX.
   230        10 FS-FTPROAUX      PIC  X(002) VALUE "00".
   231        10 LB-FTPROAUX      PIC  X(050) VALUE "FTPROAUX".
   232
   233 LINKAGE SECTION.
   234
   235 01  USER-IO                PIC  X(001) VALUE SPACE.
   236     88 OPEN-FILE                       VALUE "O" "o".
   237     88 CLOSE-FILE                      VALUE "C" "c".
   238     88 BEGIN-FILE                      VALUE "B" "b".
   239     88 END-FILE                        VALUE "E" "e".
   240     88 AT-END                          VALUE "*".
   241     88 READ-NEXT                       VALUE "N" "n".
   242     88 READ-PREVIOUS                   VALUE "P" "p".
   243     88 NOT-LESS                        VALUE ">".
   244     88 NOT-GREATER                     VALUE "<".
   245 01  ORDER-X                PIC  9(001) VALUE 0.
   246 01  STRING-1               PIC  X(040) VALUE SPACE.
   247 01  STRING-2               PIC  X(120) VALUE SPACE.
   248 01  VERTICAL-LENGTH        PIC  9(002) VALUE 0.
   249 01  WORK-AREA              PIC  X(050) VALUE SPACE.
   250 01  WORK-R-AREA            REDEFINES WORK-AREA.
   251     03 WORK-NUM-TABELA     PIC  9(002).
   252     03 WORK-CODMARCA       PIC  9(001).
   253     03 WORK-COD-PRD        PIC  9(007).
   254     03 FILLER              PIC  X(040).
   255
   256 PROCEDURE DIVISION USING USER-IO ORDER-X
   257                                  STRING-1
   258                                  STRING-2
   259                                  VERTICAL-LENGTH
   260                                  WORK-AREA.
   261 DECLARATIVES.
   262
   263 DECLARATIVES-FTPROAUX SECTION.
   264
   265     USE AFTER ERROR PROCEDURE ON FTPROAUX.
   266
   267*CHECK-FTPROAUX-TRAVADO.
   268*
   269*    IF   FS-FTPROAUX = "99" OR "9D"
   270*         CALL "GRISAM" USING ER-FTPROAUX
   271*    END-IF.
   272*
   273 END DECLARATIVES.
   274
   275 000 SECTION.
   276 000-INICIO.
   277
   278     IF   VEZ = 1
   279          MOVE "FATU908 "        TO LINK-KEYS404-PROGRAMA
   280          CALL "KEYS404"      USING LINK-KEYS404
   281          MOVE LINK-KEYS404-OCO1 TO W-FATU908-OCO1
   282          MOVE LINK-KEYS404-OCO2 TO W-FATU908-OCO2
   283          CANCEL "KEYS404"
   284
   285          MOVE 2                 TO VEZ
   286          CALL "GRFILE"       USING LB-FTPROAUX
   287          MOVE LB-FTPROAUX       TO ID-FTPROAUX
   288          MOVE WORK-NUM-TABELA   TO ID-NUM-TABELAX
   289          IF WORK-CODMARCA NOT NUMERIC
   290             MOVE ZEROS          TO WORK-CODMARCA
   291          END-IF
   292          IF WORK-COD-PRD  NOT NUMERIC
   293             MOVE ZEROS          TO WORK-COD-PRD
   294          END-IF
   295          MOVE ID-FTPROAUX       TO LB-FTPROAUX
   296     END-IF
   297
   298*   PERFORM TEST AFTER UNTIL (FS-FTPROAUX NOT = "99")
   299*                        AND (FS-FTPROAUX NOT = "9D")
   300     EVALUATE TRUE
   301         WHEN OPEN-FILE
   302              OPEN INPUT FTPROAUX
   303              IF   FS-FTPROAUX > "09"
   304                   CALL "GRISAM" USING ER-FTPROAUX
   305              END-IF
   306              EVALUATE ORDER-X
   307                 WHEN 1
   308                    SET ORD-DESCRICAO TO TRUE
   309                 WHEN 2
   310                    SET ORD-CODIGO    TO TRUE
   311*                WHEN 3
   312*                   MOVE 1 TO ORDER-X
   313*                   SET ORD-COD-EAN TO TRUE
   314              END-EVALUATE
   315              MOVE  WORK-CODMARCA TO FTPROAUX-CODMARCA
   316              MOVE  WORK-COD-PRD  TO FTPROAUX-CODIGO
   317              START FTPROAUX KEY  IS NOT < FTPROAUX-CHAVE
   318              MOVE  ZEROS TO REGISTROS
   319              PERFORM TEST AFTER UNTIL FS-FTPROAUX > "09"
   320                                    OR REGISTROS = VERTICAL-LENGTH
   321                      PERFORM 700-LER-FTPROAUX-NEXT
   322                      IF  FS-FTPROAUX < "10"  AND LIBEROU
   323                          ADD 1 TO REGISTROS
   324                      END-IF
   325              END-PERFORM
   326              IF   REGISTROS = 0
   327                   MOVE 1 TO REGISTROS
   328              END-IF
   329              IF   REGISTROS < VERTICAL-LENGTH
   330                   MOVE REGISTROS TO VERTICAL-LENGTH
   331              END-IF
   332         WHEN CLOSE-FILE
   333              CLOSE FTPROAUX
   334         WHEN BEGIN-FILE
   335              INITIALIZE FTPROAUX-REG
   336              EVALUATE TRUE
   337                  WHEN ORD-DESCRICAO
   338                       IF WORK-CODMARCA = ZEROS
   339                          START FTPROAUX KEY NOT < FTPROAUX-CHAVE1
   340                       ELSE
   341                          MOVE WORK-CODMARCA TO FTPROAUX-CODMARCA
   342                          START FTPROAUX KEY NOT < FTPROAUX-CHAVE6
   343                       END-IF
   344*                 WHEN ORD-COD-EAN
   345*                      START FTPROAUX KEY NOT < FTPROAUX-CHAVE4
   346                  WHEN ORD-CODIGO
   347                       MOVE WORK-CODMARCA TO FTPROAUX-CODMARCA
   348                       MOVE WORK-COD-PRD  TO FTPROAUX-CODIGO
   349                       START FTPROAUX KEY NOT < FTPROAUX-CHAVE
   350              END-EVALUATE
   351         WHEN END-FILE
   352              MOVE HIGH-VALUE TO FTPROAUX-REG
   353              EVALUATE TRUE
   354                  WHEN ORD-DESCRICAO
   355                       IF WORK-CODMARCA = ZEROS
   356                          START FTPROAUX KEY NOT > FTPROAUX-CHAVE1
   357                       ELSE
   358                          MOVE WORK-CODMARCA TO FTPROAUX-CODMARCA
   359                          START FTPROAUX KEY NOT > FTPROAUX-CHAVE6
   360                       END-IF
   361*                 WHEN ORD-COD-EAN
   362*                      START FTPROAUX KEY NOT > FTPROAUX-CHAVE4
   363                  WHEN ORD-CODIGO
   364                       MOVE WORK-CODMARCA TO FTPROAUX-CODMARCA
   365                       MOVE WORK-COD-PRD  TO FTPROAUX-CODIGO
   366                       START FTPROAUX KEY NOT > FTPROAUX-CHAVE
   367              END-EVALUATE
   368         WHEN READ-NEXT
   369              PERFORM 700-LER-FTPROAUX-NEXT
   370              IF (FS-FTPROAUX > "09") AND NAO-LIBEROU
   371                   SET AT-END TO TRUE
   372              END-IF
   373         WHEN READ-PREVIOUS
   374              PERFORM 700-LER-FTPROAUX-PREVIOUS
   375              IF (FS-FTPROAUX > "09") AND NAO-LIBEROU
   376                   SET AT-END TO TRUE
   377              END-IF
   378         WHEN NOT-LESS
   379              EVALUATE TRUE
   380                  WHEN ORD-DESCRICAO
   381                       IF WORK-CODMARCA = ZEROS
   382                          MOVE STRING-2(01:40) TO
   383                                               FTPROAUX-DESCRICAO
   384                          START FTPROAUX KEY NOT < FTPROAUX-CHAVE1
   385                             INVALID KEY   SET AT-END TO TRUE
   386                          END-START
   387                       ELSE
   388                          MOVE STRING-1   TO W-COD-PRODUTO
   389                          MOVE W-CODMARCA TO FTPROAUX-CODMARCA
   390                          MOVE STRING-2(01:40) TO
   391                                               FTPROAUX-DESCRICAO
   392                          START FTPROAUX KEY NOT < FTPROAUX-CHAVE6
   393                             INVALID KEY   SET AT-END TO TRUE
   394                          END-START
   395                       END-IF
   396*                 WHEN ORD-COD-EAN
   397*                      MOVE STRING-1(1: 13) TO FTPROAUX-COD-EAN
   398*                      START FTPROAUX KEY NOT < FTPROAUX-CHAVE4
   399*                         INVALID KEY   SET AT-END TO TRUE
   400*                       END-START
   401                  WHEN ORD-CODIGO
   402                       MOVE STRING-1   TO W-COD-PRODUTO
   403                       MOVE W-CODMARCA TO FTPROAUX-CODMARCA
   404                       MOVE W-COD-PROD TO FTPROAUX-CODIGO
   405                       MOVE W-DIG-PROD TO FTPROAUX-DIGITO
   406                       START FTPROAUX KEY NOT < FTPROAUX-CHAVE
   407                          INVALID KEY   SET AT-END TO TRUE
   408                        END-START
   409              END-EVALUATE
   410         WHEN NOT-GREATER
   411              EVALUATE TRUE
   412                  WHEN ORD-DESCRICAO
   413                       IF WORK-CODMARCA = ZEROS
   414                          MOVE STRING-2(01:40) TO
   415                                               FTPROAUX-DESCRICAO
   416                          START FTPROAUX KEY NOT > FTPROAUX-CHAVE1
   417                             INVALID KEY   SET AT-END TO TRUE
   418                          END-START
   419                       ELSE
   420                          MOVE STRING-1   TO W-COD-PRODUTO
   421                          MOVE W-CODMARCA TO FTPROAUX-CODMARCA
   422                          MOVE STRING-2(01:40) TO
   423                                               FTPROAUX-DESCRICAO
   424                          START FTPROAUX KEY NOT > FTPROAUX-CHAVE6
   425                             INVALID KEY   SET AT-END TO TRUE
   426                          END-START
   427                       END-IF
   428*                 WHEN ORD-COD-EAN
   429*                      MOVE STRING-1(1: 13) TO FTPROAUX-COD-EAN
   430*                      START FTPROAUX KEY NOT > FTPROAUX-CHAVE4
   431*                         INVALID KEY   SET AT-END TO TRUE
   432*                       END-START
   433                  WHEN ORD-CODIGO
   434                       MOVE STRING-1   TO W-COD-PRODUTO
   435                       MOVE W-CODMARCA TO FTPROAUX-CODMARCA
   436                       MOVE W-COD-PROD TO FTPROAUX-CODIGO
   437                       MOVE W-DIG-PROD TO FTPROAUX-DIGITO
   438                       START FTPROAUX KEY NOT > FTPROAUX-CHAVE
   439                          INVALID KEY   SET AT-END TO TRUE
   440                        END-START
   441              END-EVALUATE
   442     END-EVALUATE
   443*    END-PERFORM
   444
   445     MOVE SPACES                    TO STRING-1 STRING-2
   446     MOVE FTPROAUX-CODMARCA         TO W-CODMARCA
   447     MOVE "/"                       TO FILLER-1
   448     MOVE FTPROAUX-CODIGO           TO W-COD-PROD
   449     MOVE "-"                       TO FILLER-2
   450     MOVE FTPROAUX-DIGITO           TO W-DIG-PROD
   451     MOVE W-COD-PRODUTO             TO STRING-1
   452
   453     MOVE FTPROAUX-DESCRICAO        TO STRING-2(01: 40)
   454     MOVE FTPROAUX-DESCRICAO2       TO STRING-2(41: 40)
   455     MOVE FTPROAUX-DESCRICAO3       TO STRING-2(81: 40)
   456     GOBACK.
   457
   458 700-LER-FTPROAUX-NEXT.
   459     MOVE ZEROS TO CONTADOR.
   460     PERFORM WITH TEST AFTER UNTIL LIBEROU OR FS-FTPROAUX > "09"
   461        READ FTPROAUX NEXT RECORD ignore lock
   462        IF FS-FTPROAUX < "10"
   463           IF (WORK-CODMARCA = ZEROS)
   464              SET LIBEROU TO TRUE
   465           ELSE
   466              EVALUATE TRUE
   467                 WHEN ORD-DESCRICAO
   468                    IF (WORK-CODMARCA NOT = FTPROAUX-CODMARCA)
   469                       MOVE "10" TO FS-FTPROAUX
   470                       SET NAO-LIBEROU TO TRUE
   471                    ELSE
   472                       SET LIBEROU TO TRUE
   473                    END-IF
   474*                WHEN ORD-COD-EAN
   475*                   IF (WORK-CODMARCA NOT = FTPROAUX-CODMARCA)
   476*                      SET NAO-LIBEROU TO TRUE
   477*                   ELSE
   478*                      SET LIBEROU TO TRUE
   479*                   END-IF
   480                 WHEN ORD-CODIGO
   481                    IF (WORK-CODMARCA NOT = FTPROAUX-CODMARCA)
   482                       MOVE "10" TO FS-FTPROAUX
   483                       SET NAO-LIBEROU TO TRUE
   484                    ELSE
   485                       SET LIBEROU TO TRUE
   486                    END-IF
   487              END-EVALUATE
   488           END-IF
   489           IF W-FATU908-N-MOSTRA-INATIVO AND LK-FTPROAUX-INATIVO
   490              SET NAO-LIBEROU TO TRUE
   491           END-IF
   492        END-IF
   493     END-PERFORM.
   494
   495 700-LER-FTPROAUX-PREVIOUS.
   496     MOVE ZEROS TO CONTADOR.
   497     PERFORM WITH TEST AFTER UNTIL LIBEROU OR FS-FTPROAUX > "09"
   498        READ FTPROAUX PREVIOUS RECORD ignore lock
   499        IF FS-FTPROAUX < "10"
   500           IF (WORK-CODMARCA = ZEROS)
   501              SET LIBEROU TO TRUE
   502           ELSE
   503              EVALUATE TRUE
   504                 WHEN ORD-DESCRICAO
   505                    IF (WORK-CODMARCA NOT = FTPROAUX-CODMARCA)
   506                       MOVE "10" TO FS-FTPROAUX
   507                       SET NAO-LIBEROU TO TRUE
   508                    ELSE
   509                       SET LIBEROU TO TRUE
   510                    END-IF
   511*                WHEN ORD-COD-EAN
   512*                   IF (WORK-CODMARCA NOT = FTPROAUX-CODMARCA)
   513*                      SET NAO-LIBEROU TO TRUE
   514*                   ELSE
   515*                      SET LIBEROU TO TRUE
   516*                   END-IF
   517                 WHEN ORD-CODIGO
   518                    IF (WORK-CODMARCA NOT = FTPROAUX-CODMARCA)
   519                       MOVE "10" TO FS-FTPROAUX
   520                       SET NAO-LIBEROU TO TRUE
   521                    ELSE
   522                       SET LIBEROU TO TRUE
   523                    END-IF
   524              END-EVALUATE
   525           END-IF
   526           IF W-FATU908-N-MOSTRA-INATIVO AND LK-FTPROAUX-INATIVO
   527              SET NAO-LIBEROU TO TRUE
   528           END-IF
   529        END-IF
   530     END-PERFORM.
   531
   532 END PROGRAM FTKEY986.
