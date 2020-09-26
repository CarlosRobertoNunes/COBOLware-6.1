     1 IDENTIFICATION DIVISION.
     2 PROGRAM-ID.    WSTU908.
     3 AUTHOR.        KATIA.
     4 DATE-WRITTEN.  13/01/97.
     5 SECURITY.      *************************************************
     6                *  LER CADASTROS DE PRODUTOS                    *
     7                *  Padrao de programas de apoio a GRBOXF        *
     8                *                                               *
     9                *-----------------------------------------------*
    10                *  Dt.Alteracao: 11/12/2009                     *
    11                *  Programador : Marco Guerra                   *
    12                *  Descricao   : Configurado para nao mostrar os*
    13                *                produtos inativos              *
    14                *  Configuracao: FATU908 - OCOR 4 = 1           *
    15                *  ID.Modif.   : mg0001                         *
    16                *  Solicitacao : 1675                           *
    17                *-----------------------------------------------*
    18                *  Dt.Alteracao: 13/01/2010                     *
    19                *  Programador : Thiago de Jesus                *
    20                *  Descricao   : Nao permitir para ser digitado *
    21                *                com letra minuscula            *
    22                *  Configuracao:                                *
    23                *  ID.Modif.   : THG001                         *
    24                *  Solicitacao : 1889                           *
    25                *************************************************
    26 ENVIRONMENT    DIVISION.
    27 CONFIGURATION  SECTION.
    28 SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
    29 INPUT-OUTPUT   SECTION.
    30 FILE-CONTROL.
    31
    33** AO ALTERAR ESTA SELECT, TAMBEM ALTERAR O QUE ESTA INTERNO NO
    34** PROGRAMA FATU057 AOTM333 FATU066 FATU853 FTKEY066 LCEX011
    35**
    36
    37     SELECT FATPRODU
    38          ASSIGN TO DISK
    39          ORGANIZATION INDEXED
    40          ACCESS MODE DYNAMIC
    41          LOCK MODE AUTOMATIC
    42          RECORD KEY FATPRODU-CHAVE
    43          ALTERNATE RECORD KEY FATPRODU-CHAVE1 =
    44                               FATPRODU-NOME-BUSCA WITH DUPLICATES
    45          FILE STATUS FS-FATPRODU.
    47** LABEL VALIAREL FATTAXnn
    48**
    49** AO ALTERAR ESTA SELECT, TAMBEM ALTERAR O QUE ESTA INTERNO NO
    50** PROGRAMA FATU057 AOTM333 FATU066 FATU853 FTKEY066 LCEX011
    51**
    52     SELECT FTPROAUX ASSIGN TO DISK
    53          ORGANIZATION INDEXED
    54          ACCESS MODE DYNAMIC
    55          LOCK MODE AUTOMATIC
    56          RECORD KEY FTPROAUX-CHAVE
    57          ALTERNATE RECORD KEY FTPROAUX-CHAVE1 =
    58                               FTPROAUX-DESCRICAO
    59                               WITH DUPLICATES
    60          ALTERNATE RECORD KEY FTPROAUX-CHAVE2 =
    61                               FTPROAUX-COD-ALMOX
    62                               WITH DUPLICATES
    63          ALTERNATE RECORD KEY FTPROAUX-CHAVE3 =
    64                               FTPROAUX-GRUPO
    65                               FTPROAUX-COD-ALMOX
    66                               WITH DUPLICATES
    67          ALTERNATE RECORD KEY FTPROAUX-CHAVE4 =
    68                               FTPROAUX-COD-EAN
    69                               WITH DUPLICATES
    70          ALTERNATE RECORD KEY FTPROAUX-CHAVE5 =
    71                               FTPROAUX-CODMARCA
    72                               FTPROAUX-NOME-BUSCA
    73                               WITH DUPLICATES
    74          ALTERNATE RECORD KEY FTPROAUX-CHAVE6 =
    75                               FTPROAUX-CODMARCA
    76                               FTPROAUX-DESCRICAO
    77                               WITH DUPLICATES
    78          FILE STATUS FS-FTPROAUX.
    79
    80
    81 DATA DIVISION.
    82 FILE SECTION.
    83
    85** LABEL FATTABnn
    86** AO ALTERAR ESTE BOOK, TAMBEM ALTERAR O QUE ESTA INTERNO NO
    87** PROGRAMA FATU057 E FATU066 AOTM333 FATU853 FTKEY066 LCEX011
    88**          FTWOG014(FATPROD2.FD)
    89**   SELECT FATPRODU
    90**        ASSIGN TO DISK
    91**        ORGANIZATION INDEXED
    92**        ACCESS MODE DYNAMIC
    93**        LOCK MODE AUTOMATIC
    94**        RECORD KEY FATPRODU-CHAVE
    95**        ALTERNATE RECORD KEY FATPRODU-CHAVE1 =
    96**                             FATPRODU-NOME-BUSCA WITH DUPLICATES
    97**        FILE STATUS FS-FATPRODU.
    98
    99 FD  FATPRODU
   100     LABEL RECORD IS STANDARD
   101     VALUE OF FILE-ID ID-FATPRODU.
   102 01  FATPRODU-REG.
   103     03 FATPRODU-CHAVE.
   104        05 FATPRODU-CODMARCA      PIC 9(001).
   105        05 FATPRODU-CODIGO        PIC 9(007).
   106        05 FATPRODU-DIGITO        PIC 9(001).
   107     03 FATPRODU-DESCRICAO        PIC X(040).
   108     03 FATPRODU-NOME-BUSCA       PIC X(015).
   109     03 FATPRODU-UNIDADE          PIC X(003).
   110     03 FATPRODU-PRECO-UNIT       PIC 9(012)V9999.
   111     03 FATPRODU-PESO-UNIT        PIC 9(005)V9999.
   112     03 FATPRODU-CLAS-FISCAL      PIC 9(008).
   113     03 FATPRODU-ALIQ-IPI-FILLER  PIC 9(002).
   114     03 FATPRODU-QTD-EMBAL        PIC 9(004).
   115     03 FATPRODU-QTD-EMB-EMBARQ   PIC 9(004).
   116     03 FATPRODU-PESO-CX-EMBARQ   PIC 9(005)V9999.
   117     03 FATPRODU-COMISSAO         PIC 9(002)V99.
   118     03 FATPRODU-GRUPO            PIC 9(002).
   119     03 FATPRODU-SUBGRUPO         PIC 9(004).
   120     03 FATPRODU-COD-ALMOX        PIC 9(009).
   121     03 FATPRODU-COD-BARRA        PIC 9(005).
   122     03 FATPRODU-ICM-REG-LOCAL    PIC 9(002)V99.
   123     03 FATPRODU-ICM-REG-SUL      PIC 9(002)V99.
   124     03 FATPRODU-ICM-REG-NE       PIC 9(002)V99.
   125     03 FATPRODU-BASE-RED-LOCAL   PIC 9(002)V9999.
   126     03 FATPRODU-BASE-RED-SUL     PIC 9(002)V9999.
   127     03 FATPRODU-BASE-RED-NE      PIC 9(002)V9999.
   128     03 FATPRODU-ISENT-OUTR-ICM   PIC 9(001).
   129* ISENT-OUTR-ICM: 1 - Isenta ICM     (ISENTO)
   130*                 2 - Nao Tributada  (ISENTO)
   131*                 3 - Suspensa       (OUTRAS)
   132*                 4 - Diferida       (OUTRAS)
   133*                 5 - OUTRAS         (OUTRAS)
   134     03 FATPRODU-ISENT-OUTR-IPI   PIC 9(001).
   135* ISENT-OUTR-IPI: 1-Isenta IPI  ver Monpar existem outros codigos
   136*                 2-Outras IPI
   137     03 FATPRODU-ORIGEM-MERC      PIC 9(001).
   138* ORIGEM-MERC   : 0-Nacional
   139*                 1-Estrangeira - Imp.Direta
   140*                 2-Estrangeira - Adquirida Merc.Intern.
   141*                 3-Producao Propria
   142*                 4-Servicos
   143*                 5-Servicos com retencao de IR (Em estudo)
   144     03 FATPRODU-ICM-SUBST-TRIB   PIC 9(001).
   145* ICM-SUBST-TRIB: 0-Nao
   146*                 1-Sim
   147*                 2-Sim  icms retido antecipado (ex:COMBUSTRAM)
   148     03 FATPRODU-ALIQ-IPI         PIC 9(002)V99.
   149*   - MENSAGENS DA NF DE IPI E ICMS SOBRE PRODUTOS - FTMSGNFS
   150*   - PROG. DE MANUTENCAO : FTKEY108.CBL
   151     03 FATPRODU-COD-MSG1         PIC 9(002).
   152     03 FATPRODU-COD-MSG2         PIC 9(002).
   153*   - FLAG PARA SABER SE O PRODUTO TEM VARIACOES - YAMAPLAS
   154*
   155* 0 - NAO TEM
   156* 1 - VARIA TAMANHO
   157* 2 - VARIA COR
   158* 3 - VARIA TAMANHO E COR
   159*
   160     03 FATPRODU-VARIA-PROD       PIC 9(001).
   161     03 FATPRODU-FILLER           PIC X(002).
   163** ATENCAO
   164** AO ALTERAR ESTE BOOK, TAMBEM ALTERAR O QUE ESTA INTERNO NO
   165** PROGRAMA FATU057 E FATU066 AOTM333 FATU853 FTKEY066 LCEX011
   166**
   167** LABEL VALIAREL FATTAXnn
   168**
   169**
   170**   SELECT FTPROAUX ASSIGN TO DISK
   171**        ORGANIZATION INDEXED
   172**        ACCESS MODE DYNAMIC
   173**        LOCK MODE AUTOMATIC
   174**        RECORD KEY FTPROAUX-CHAVE
   175**        ALTERNATE RECORD KEY FTPROAUX-CHAVE1 =
   176**                             FTPROAUX-DESCRICAO
   177**                             WITH DUPLICATES
   178**        ALTERNATE RECORD KEY FTPROAUX-CHAVE2 =
   179**                             FTPROAUX-COD-ALMOX
   180**                             WITH DUPLICATES
   181**        ALTERNATE RECORD KEY FTPROAUX-CHAVE3 =
   182**                             FTPROAUX-GRUPO
   183**                             FTPROAUX-COD-ALMOX
   184**                             WITH DUPLICATES
   185**        ALTERNATE RECORD KEY FTPROAUX-CHAVE4 =
   186**                             FTPROAUX-COD-EAN
   187**                             WITH DUPLICATES
   188**        ALTERNATE RECORD KEY FTPROAUX-CHAVE5 =
   189**                             FTPROAUX-CODMARCA
   190**                             FTPROAUX-NOME-BUSCA
   191**                             WITH DUPLICATES
   192**        ALTERNATE RECORD KEY FTPROAUX-CHAVE6 =
   193**                             FTPROAUX-CODMARCA
   194**                             FTPROAUX-DESCRICAO
   195**                             WITH DUPLICATES
   196**        FILE STATUS FS-FTPROAUX.
   197*----------------------------------------------------------------*
   198*  Dt.Alteracao: 22/07/2009                                      *
   199*  Programador : Marco Guerra                                    *
   200*  Descricao   : Campos novos de peso                            *
   201*                FTPROAUX-PESO-ACESS   - Produto com acessorios  *
   202*                FTPROAUX-PESO-EMB-COL - Embalagem coletiva      *
   203*  ID.Modif.   : mg0001                                          *
   204*  O.Servico   : 4166                                            *
   205*----------------------------------------------------------------*
   206 FD  FTPROAUX
   207     LABEL RECORD IS STANDARD
   208     VALUE OF FILE-ID ID-FTPROAUX.
   209 01  FTPROAUX-REG.
   210     03 FTPROAUX-CHAVE.
   211        05 FTPROAUX-CODMARCA       PIC 9(001).
   212        05 FTPROAUX-CODIGO         PIC 9(007).
   213        05 FTPROAUX-DIGITO         PIC 9(001).
   214     03 FTPROAUX-DESCRICAO         PIC X(040).
   215     03 FTPROAUX-GRUPO             PIC 9(002).
   216     03 FTPROAUX-COD-ALMOX         PIC 9(009).
   217     03 FTPROAUX-COD-SERVICO       PIC 9(010).
   218     03 FTPROAUX-ALIQ-ISS          PIC 9(002)V99.
   219     03 FTPROAUX-APELIDO           PIC X(015).
   220     *>----------
   221     03 FTPROAUX-FLAG-ENVIO        PIC X(001).
   222     *> FLAG-ENVIO, USADA NA MARCOLAR PARA RESTRINGIR O REENVIO
   223     *> DO PRODUTO PARA AS TABELAS DO SITE - POWER DESIGN
   224     *>----------
   225     03 FTPROAUX-COD-EAN           PIC 9(013).
   226     03 FTPROAUX-CUSTO-REAIS       PIC 9(007)V9999.
   227     03 FTPROAUX-CUSTO-EURO        PIC 9(007)V9999.
   228     03 FTPROAUX-NOME-BUSCA        PIC X(015).
   229     03 FTPROAUX-FLAG-PROD         PIC X(001).
   230        88 LK-FTPROAUX-ATIVO       VALUE "A" " ".
   231        88 LK-FTPROAUX-INATIVO     VALUE "I".
   232        88 LK-FTPROAUX-COM-ERRO    VALUE "C".
   233     03 FTPROAUX-DIG-COD-EAN       PIC 9(001).
   234     03 FTPROAUX-PESO-ACESS        PIC 9(005)V9999.
   235     03 FTPROAUX-PESO-EMB-COL      PIC 9(005)V9999.
   236     03 FTPROAUX-DESCRICAO2        PIC X(040).
   237     03 FTPROAUX-DESCRICAO3        PIC X(040).
   238     03 FTPROAUX-DATACAD           PIC 9(008).
   239     03 R-FTPROAUX-DATACAD REDEFINES FTPROAUX-DATACAD.
   240        05 FTPROAUX-ANOCAD         PIC 9(004).
   241        05 FTPROAUX-MESCAD         PIC 9(002).
   242        05 FTPROAUX-DIACAD         PIC 9(002).
   243     03 FTPROAUX-DATAALT           PIC 9(008).
   244     03 R-FTPROAUX-DATAALT REDEFINES FTPROAUX-DATAALT.
   245        05 FTPROAUX-ANOALT         PIC 9(004).
   246        05 FTPROAUX-MESALT         PIC 9(002).
   247        05 FTPROAUX-DIAALT         PIC 9(002).
   248     03 FTPROAUX-FILLER            PIC X(184).
   249
   250 WORKING-STORAGE SECTION.
   251
   253 01  W-FATU908-OCO1                   PIC 9(10).
   254 01  W-R-FATU908-OCO1                 REDEFINES W-FATU908-OCO1.
   255     03 W-FATU908-OC01                PIC 9(01).
   256        88 W-FATU908-TODOS-PROD       VALUE 0.
   257        88 W-FATU908-PROD-CHAVE-YAM   VALUE 1.
   258     03 W-FATU908-OC02                PIC 9(01).
   259        88 W-FATU908-NOME-BUSCA       VALUE 0.
   260        88 W-FATU908-DESCRICAO        VALUE 1.
   261     03 W-FATU908-OC03                PIC 9(01).
   262        88 W-FATU908-ACC-PALAVRA      VALUE 1.
   263     03 W-FATU908-OC04                PIC 9(01).
   264        88 W-FATU908-N-MOSTRA-INATIVO VALUE 1.
   265     03 W-FATU908-OC05                PIC 9(01).
   266     03 W-FATU908-OC06                PIC 9(01).
   267     03 W-FATU908-OC07                PIC 9(01).
   268     03 W-FATU908-OC08                PIC 9(01).
   269     03 W-FATU908-OC09                PIC 9(01).
   270     03 W-FATU908-OC10                PIC 9(01).
   271
   272 01  W-FATU908-OCO2                   PIC 9(10).
   273 01  W-R-FATU908-OCO2                 REDEFINES W-FATU908-OCO2.
   274     03 W-FATU908-OC11                PIC 9(01).
   275     03 W-FATU908-OC12                PIC 9(01).
   276     03 W-FATU908-OC13                PIC 9(01).
   277     03 W-FATU908-OC14                PIC 9(01).
   278     03 W-FATU908-OC15                PIC 9(01).
   279     03 W-FATU908-OC16                PIC 9(01).
   280     03 W-FATU908-OC17                PIC 9(01).
   281     03 W-FATU908-OC18                PIC 9(01).
   282     03 W-FATU908-OC19                PIC 9(01).
   283     03 W-FATU908-OC20                PIC 9(01).
   285 01 LINK-KEYS404.
   286    03 LINK-KEYS404-PROGRAMA   PIC X(08).
   287    03 LINK-KEYS404-OCO1       PIC 9(10).
   288    03 LINK-KEYS404-OCO2       PIC 9(10).
   289    03 LINK-KEYS404-DESCR1     PIC X(60) OCCURS 20 TIMES.
   290    03 LINK-KEYS404-DESCR2     PIC X(60) OCCURS 20 TIMES.
   291    03 LINK-KEYS404-OCO1X      PIC 9(10).
   292    03 LINK-KEYS404-OCO2X      PIC 9(10).
   293    03 LINK-KEYS404-DESCR1X    PIC X(60) OCCURS 20 TIMES.
   294    03 LINK-KEYS404-DESCR2X    PIC X(60) OCCURS 20 TIMES.
   295    03 LINK-KEYS404-OCO1Y      PIC 9(10).
   296    03 LINK-KEYS404-OCO2Y      PIC 9(10).
   297    03 LINK-KEYS404-DESCR1Y    PIC X(60) OCCURS 20 TIMES.
   298    03 LINK-KEYS404-DESCR2Y    PIC X(60) OCCURS 20 TIMES.
   299
   300 01  W-COD-PRODUTO.
   301     03 W-CODMARCA          PIC  9(01).
   302     03 FILLER-1            PIC  X(01).
   303     03 W-COD-PROD          PIC  9(07).
   304     03 FILLER-2            PIC  X(01).
   305     03 W-DIG-PROD          PIC  9(01).
   306
   308 01 ID-FATPRODU.
   309    03 ID-LITERAL         PIC X(09).
   310    03 ID-NUM-TABELA      PIC 9(02).
   311    03 FILLER             PIC X(39).
   313 01 ID-FTPROAUX.
   314    03 ID-LITERALX        PIC X(09).
   315    03 ID-NUM-TABELAX     PIC 9(02).
   317 01  W-CAMPOS-ROT020.
   318     05 WP-P                PIC  9(002) VALUE ZEROS.
   319     05 WP-RETORNO          PIC  9(002) VALUE ZEROS.
   320        88 NAO-ACHOU-PALAVRA            VALUE ZEROS.
   321     05 WP-TAM              PIC  9(002) VALUE ZEROS.
   322     05 WP-PALAVRA          PIC  X(015) VALUE SPACES.
   323        88 NAO-TEM-PALAVRA              VALUE SPACES.
   324     05 WP-NOME             PIC  X(060) VALUE SPACES.
   326 01 W-CAMPOS-MSG23.
   327    03 W-MSG                    PIC X(70) VALUE SPACES.
   328    03 W-RESP-MSG               PIC X(01) VALUE SPACES.
   329       88 W-RESP-MSG-SIM VALUE "S" "s".
   330       88 W-RESP-MSG-NAO VALUE "N" "n".
   331    03 COR-F-MSG                PIC 9(03) VALUE ZEROS.
   332    03 COR-B-MSG                PIC 9(03) VALUE ZEROS.
   334
   335 01  COLOR-ATRIBUTES.
   336     05 BLACK-BLACK-LOW      PIC 9(3) VALUE 000.
   337     05 BLACK-BLUE-LOW       PIC 9(3) VALUE 001.
   338     05 BLACK-GREEN-LOW      PIC 9(3) VALUE 002.
   339     05 BLACK-CYAN-LOW       PIC 9(3) VALUE 003.
   340     05 BLACK-RED-LOW        PIC 9(3) VALUE 004.
   341     05 BLACK-MAGENTA-LOW    PIC 9(3) VALUE 005.
   342     05 BLACK-BROWN-LOW      PIC 9(3) VALUE 006.
   343     05 BLACK-WHITE-LOW      PIC 9(3) VALUE 007.
   344     05 BLACK-BLACK-HIGH     PIC 9(3) VALUE 008.
   345     05 BLACK-BLUE-HIGH      PIC 9(3) VALUE 009.
   346     05 BLACK-GREEN-HIGH     PIC 9(3) VALUE 010.
   347     05 BLACK-CYAN-HIGH      PIC 9(3) VALUE 011.
   348     05 BLACK-RED-HIGH       PIC 9(3) VALUE 012.
   349     05 BLACK-MAGENTA-HIGH   PIC 9(3) VALUE 013.
   350     05 BLACK-BROWN-HIGH     PIC 9(3) VALUE 014.
   351     05 BLACK-WHITE-HIGH     PIC 9(3) VALUE 015.
   352     05 BLUE-BLACK-LOW       PIC 9(3) VALUE 016.
   353     05 BLUE-BLUE-LOW        PIC 9(3) VALUE 017.
   354     05 BLUE-GREEN-LOW       PIC 9(3) VALUE 018.
   355     05 BLUE-CYAN-LOW        PIC 9(3) VALUE 019.
   356     05 BLUE-RED-LOW         PIC 9(3) VALUE 020.
   357     05 BLUE-MAGENTA-LOW     PIC 9(3) VALUE 021.
   358     05 BLUE-BROWN-LOW       PIC 9(3) VALUE 022.
   359     05 BLUE-WHITE-LOW       PIC 9(3) VALUE 023.
   360     05 BLUE-BLACK-HIGH      PIC 9(3) VALUE 024.
   361     05 BLUE-BLUE-HIGH       PIC 9(3) VALUE 025.
   362     05 BLUE-GREEN-HIGH      PIC 9(3) VALUE 026.
   363     05 BLUE-CYAN-HIGH       PIC 9(3) VALUE 027.
   364     05 BLUE-RED-HIGH        PIC 9(3) VALUE 028.
   365     05 BLUE-MAGENTA-HIGH    PIC 9(3) VALUE 029.
   366     05 BLUE-BROWN-HIGH      PIC 9(3) VALUE 030.
   367     05 BLUE-WHITE-HIGH      PIC 9(3) VALUE 031.
   368     05 GREEN-BLACK-LOW      PIC 9(3) VALUE 032.
   369     05 GREEN-BLUE-LOW       PIC 9(3) VALUE 033.
   370     05 GREEN-GREEN-LOW      PIC 9(3) VALUE 034.
   371     05 GREEN-CYAN-LOW       PIC 9(3) VALUE 035.
   372     05 GREEN-RED-LOW        PIC 9(3) VALUE 036.
   373     05 GREEN-MAGENTA-LOW    PIC 9(3) VALUE 037.
   374     05 GREEN-BROWN-LOW      PIC 9(3) VALUE 038.
   375     05 GREEN-WHITE-LOW      PIC 9(3) VALUE 039.
   376     05 GREEN-BLACK-HIGH     PIC 9(3) VALUE 040.
   377     05 GREEN-BLUE-HIGH      PIC 9(3) VALUE 041.
   378     05 GREEN-GREEN-HIGH     PIC 9(3) VALUE 042.
   379     05 GREEN-CYAN-HIGH      PIC 9(3) VALUE 043.
   380     05 GREEN-RED-HIGH       PIC 9(3) VALUE 044.
   381     05 GREEN-MAGENTA-HIGH   PIC 9(3) VALUE 045.
   382     05 GREEN-BROWN-HIGH     PIC 9(3) VALUE 046.
   383     05 GREEN-WHITE-HIGH     PIC 9(3) VALUE 047.
   384     05 CYAN-BLACK-LOW       PIC 9(3) VALUE 048.
   385     05 CYAN-BLUE-LOW        PIC 9(3) VALUE 049.
   386     05 CYAN-GREEN-LOW       PIC 9(3) VALUE 050.
   387     05 CYAN-CYAN-LOW        PIC 9(3) VALUE 051.
   388     05 CYAN-RED-LOW         PIC 9(3) VALUE 052.
   389     05 CYAN-MAGENTA-LOW     PIC 9(3) VALUE 053.
   390     05 CYAN-BROWN-LOW       PIC 9(3) VALUE 054.
   391     05 CYAN-WHITE-LOW       PIC 9(3) VALUE 055.
   392     05 CYAN-BLACK-HIGH      PIC 9(3) VALUE 056.
   393     05 CYAN-BLUE-HIGH       PIC 9(3) VALUE 057.
   394     05 CYAN-GREEN-HIGH      PIC 9(3) VALUE 058.
   395     05 CYAN-CYAN-HIGH       PIC 9(3) VALUE 059.
   396     05 CYAN-RED-HIGH        PIC 9(3) VALUE 060.
   397     05 CYAN-MAGENTA-HIGH    PIC 9(3) VALUE 061.
   398     05 CYAN-BROWN-HIGH      PIC 9(3) VALUE 062.
   399     05 CYAN-WHITE-HIGH      PIC 9(3) VALUE 063.
   400     05 RED-BLACK-LOW        PIC 9(3) VALUE 064.
   401     05 RED-BLUE-LOW         PIC 9(3) VALUE 065.
   402     05 RED-GREEN-LOW        PIC 9(3) VALUE 066.
   403     05 RED-CYAN-LOW         PIC 9(3) VALUE 067.
   404     05 RED-RED-LOW          PIC 9(3) VALUE 068.
   405     05 RED-MAGENTA-LOW      PIC 9(3) VALUE 069.
   406     05 RED-BROWN-LOW        PIC 9(3) VALUE 070.
   407     05 RED-WHITE-LOW        PIC 9(3) VALUE 071.
   408     05 RED-BLACK-HIGH       PIC 9(3) VALUE 072.
   409     05 RED-BLUE-HIGH        PIC 9(3) VALUE 073.
   410     05 RED-GREEN-HIGH       PIC 9(3) VALUE 074.
   411     05 RED-CYAN-HIGH        PIC 9(3) VALUE 075.
   412     05 RED-RED-HIGH         PIC 9(3) VALUE 076.
   413     05 RED-MAGENTA-HIGH     PIC 9(3) VALUE 077.
   414     05 RED-BROWN-HIGH       PIC 9(3) VALUE 078.
   415     05 RED-WHITE-HIGH       PIC 9(3) VALUE 079.
   416     05 MAGENTA-BLACK-LOW    PIC 9(3) VALUE 080.
   417     05 MAGENTA-BLUE-LOW     PIC 9(3) VALUE 081.
   418     05 MAGENTA-GREEN-LOW    PIC 9(3) VALUE 082.
   419     05 MAGENTA-CYAN-LOW     PIC 9(3) VALUE 083.
   420     05 MAGENTA-RED-LOW      PIC 9(3) VALUE 084.
   421     05 MAGENTA-MAGENTA-LOW  PIC 9(3) VALUE 085.
   422     05 MAGENTA-BROWN-LOW    PIC 9(3) VALUE 086.
   423     05 MAGENTA-WHITE-LOW    PIC 9(3) VALUE 087.
   424     05 MAGENTA-BLACK-HIGH   PIC 9(3) VALUE 088.
   425     05 MAGENTA-BLUE-HIGH    PIC 9(3) VALUE 089.
   426     05 MAGENTA-GREEN-HIGH   PIC 9(3) VALUE 090.
   427     05 MAGENTA-CYAN-HIGH    PIC 9(3) VALUE 091.
   428     05 MAGENTA-RED-HIGH     PIC 9(3) VALUE 092.
   429     05 MAGENTA-MAGENTA-HIGH PIC 9(3) VALUE 093.
   430     05 MAGENTA-BROWN-HIGH   PIC 9(3) VALUE 094.
   431     05 MAGENTA-WHITE-HIGH   PIC 9(3) VALUE 095.
   432     05 BROWN-BLACK-LOW      PIC 9(3) VALUE 096.
   433     05 BROWN-BLUE-LOW       PIC 9(3) VALUE 097.
   434     05 BROWN-GREEN-LOW      PIC 9(3) VALUE 098.
   435     05 BROWN-CYAN-LOW       PIC 9(3) VALUE 099.
   436     05 BROWN-RED-LOW        PIC 9(3) VALUE 100.
   437     05 BROWN-MAGENTA-LOW    PIC 9(3) VALUE 101.
   438     05 BROWN-BROWN-LOW      PIC 9(3) VALUE 102.
   439     05 BROWN-WHITE-LOW      PIC 9(3) VALUE 103.
   440     05 BROWN-BLACK-HIGH     PIC 9(3) VALUE 104.
   441     05 BROWN-BLUE-HIGH      PIC 9(3) VALUE 105.
   442     05 BROWN-GREEN-HIGH     PIC 9(3) VALUE 106.
   443     05 BROWN-CYAN-HIGH      PIC 9(3) VALUE 107.
   444     05 BROWN-RED-HIGH       PIC 9(3) VALUE 108.
   445     05 BROWN-MAGENTA-HIGH   PIC 9(3) VALUE 109.
   446     05 BROWN-BROWN-HIGH     PIC 9(3) VALUE 110.
   447     05 BROWN-WHITE-HIGH     PIC 9(3) VALUE 111.
   448     05 WHITE-BLACK-LOW      PIC 9(3) VALUE 112.
   449     05 WHITE-BLUE-LOW       PIC 9(3) VALUE 113.
   450     05 WHITE-GREEN-LOW      PIC 9(3) VALUE 114.
   451     05 WHITE-CYAN-LOW       PIC 9(3) VALUE 115.
   452     05 WHITE-RED-LOW        PIC 9(3) VALUE 116.
   453     05 WHITE-MAGENTA-LOW    PIC 9(3) VALUE 117.
   454     05 WHITE-BROWN-LOW      PIC 9(3) VALUE 118.
   455     05 WHITE-WHITE-LOW      PIC 9(3) VALUE 119.
   456     05 WHITE-BLACK-HIGH     PIC 9(3) VALUE 120.
   457     05 WHITE-BLUE-HIGH      PIC 9(3) VALUE 121.
   458     05 WHITE-GREEN-HIGH     PIC 9(3) VALUE 122.
   459     05 WHITE-CYAN-HIGH      PIC 9(3) VALUE 123.
   460     05 WHITE-RED-HIGH       PIC 9(3) VALUE 124.
   461     05 WHITE-MAGENTA-HIGH   PIC 9(3) VALUE 125.
   462     05 WHITE-BROWN-HIGH     PIC 9(3) VALUE 126.
   463     05 WHITE-WHITE-HIGH     PIC 9(3) VALUE 127.
   464     05 BLACK                PIC 9(1) VALUE 0.
   465     05 BLUE                 PIC 9(1) VALUE 1.
   466     05 GREEN                PIC 9(1) VALUE 2.
   467     05 CYAN                 PIC 9(1) VALUE 3.
   468     05 RED                  PIC 9(1) VALUE 4.
   469     05 MAGENTA              PIC 9(1) VALUE 5.
   470     05 BROWN                PIC 9(1) VALUE 6.
   471     05 WHITE                PIC 9(1) VALUE 7.
   472
   474
   475 01   PARAMETROS-GRACOR VALUE ALL "0701".
   476      05 GRACOR-CORES OCCURS 11.
   477         10 GRACOR-F                   PIC  9(002).
   478         10 GRACOR-B                   PIC  9(002).
   479      05 GRACOR-FUNCAO                 PIC  X(001).
   480
   482
   483 01 AREAS-DE-TELA.
   484    05 SCREEN-POSITION.
   485       10 ROW-NUMBER      PIC 9(0001) COMP-X.
   486       10 COL-NUMBER      PIC 9(0001) COMP-X.
   487    05 SCREEN-BUFFER      PIC X(2000) VALUE SPACES.
   488    05 ATTRIBUTE-BUFFER   PIC X(2000) VALUE SPACES.
   489    05 LENGHT-BUFFER      PIC 9(0004) COMP-X VALUE 2000.
   490
   491
   492 01  W-VALIDO               PIC  9(001).
   493     88 VALIDO                   VALUE 1.
   494     88 NAO-VALIDO               VALUE 2.
   495
   496 01  W-LIBEROU              PIC  9(01).
   497     88 LIBEROU                  VALUE 1.
   498     88 NAO-LIBEROU              VALUE 2.
   499
   500 01  W-LEITURA              PIC  9(01).
   501     88 LE-FTPROAUX              VALUE 1.
   502     88 LE-FATPRODU              VALUE 2.
   503
   504 01  AREAS-DE-TRABALHO-1.
   505     05 MINUSCULAS          PIC  X(026)    VALUE
   506        "abcdefghijklmnopqrstuvwxyz".
   507     05 MAIUSCULAS          PIC  X(026)    VALUE
   508        "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
   509     05 REGISTROS           PIC  9(002) VALUE 0.
   510     05 CONTADOR            PIC  9(002) VALUE 0.
   511     05 VEZ                 PIC  9(001) VALUE 1.
   512     05 ER-FATPRODU.
   513        10 FS-FATPRODU      PIC  X(002) VALUE "00".
   514        10 LB-FATPRODU      PIC  X(050) VALUE "fattab01".
   515     05 ER-FTPROAUX.
   516        10 FS-FTPROAUX      PIC  X(002) VALUE "00".
   517        10 LB-FTPROAUX      PIC  X(050) VALUE "fattax01".
   518
   519 LINKAGE SECTION.
   520
   521 01  USER-IO                PIC  X(001) VALUE SPACE.
   522     88 OPEN-FILE                   VALUE "O" "o".
   523     88 CLOSE-FILE                  VALUE "C" "c".
   524     88 BEGIN-FILE                  VALUE "B" "b".
   525     88 END-FILE                    VALUE "E" "e".
   526     88 AT-END                      VALUE "*".
   527     88 READ-NEXT                   VALUE "N" "n".
   528     88 READ-PREVIOUS               VALUE "P" "p".
   529     88 NOT-LESS                    VALUE ">".
   530     88 NOT-GREATER                 VALUE "<".
   531 01  ORDER-X                PIC  9(001) VALUE 0.
   532 01  STRING-1               PIC  X(120) VALUE SPACE.
   533 01  STRING-2               PIC  X(120) VALUE SPACE.
   534*01  STRING-2               PIC  X(040) VALUE SPACE.
   535 01  VERTICAL-LENGTH        PIC  9(002) VALUE 0.
   536 01  WORK-AREA              PIC  X(050) VALUE SPACE.
   537 01  WORK-R-AREA            REDEFINES WORK-AREA.
   538     03 WORK-NUM-TABELA     PIC  9(002).
   539     03 WORK-CODMARCA       PIC  9(001).
   540     03 WORK-COD-PRD        PIC  9(007).
   541     03 FILLER              PIC  X(040).
   542
   543 SCREEN SECTION.
   545 01  CTAC-LIT-ROT020.
   546 02  HIGHLIGHT FOREGROUND-COLOR BLUE-CYAN-HIGH
   547     BACKGROUND-COLOR BLUE-BLUE-LOW.
   548     05 LINE 19 COLUMN 03 VALUE "ษอออออออออออออออออออ".
   549     05 LINE 19 COLUMN 23 VALUE "ออออออออออออออออออป".
   550     05 LINE 20 COLUMN 03 VALUE "บ Palavra Chave:    ".
   551     05 LINE 20 COLUMN 23 VALUE "                  บ".
   552     05 LINE 21 COLUMN 03 VALUE "ศอออออออออออออออออออ".
   553     05 LINE 21 COLUMN 23 VALUE "ออออออออออออออออออผ".
   554
   555 01  CTAC-VAR-ROT020.
   556 02  HIGHLIGHT FOREGROUND-COLOR BLUE-WHITE-HIGH
   557     BACKGROUND-COLOR BLUE-BLUE-LOW.
   558     05 T-WP-PALAVRA.
   559        10 LINE 20 COLUMN 20 PIC X(020) USING WP-PALAVRA.
   561 01  CTAC-MSG23.
   562 02  FOREGROUND-COLOR GRACOR-F (10)                               MS
   563     BACKGROUND-COLOR GRACOR-B (10).                              MS
   564     05 T-W-MSG
   565        LINE 23 COLUMN 03 PIC X(076) FROM W-MSG.
   566     05 T-W-RESP-MSG
   567        LINE 23 COLUMN 74 PIC X(001) USING W-RESP-MSG.
   568
   569
   570 PROCEDURE DIVISION USING USER-IO ORDER-X
   571                                  STRING-1
   572                                  STRING-2
   573                                  VERTICAL-LENGTH
   574                                  WORK-AREA.
   575 DECLARATIVES.
   576
   577 DECLARATIVES-FATPRODU SECTION.
   578
   579     USE AFTER ERROR PROCEDURE ON FATPRODU.
   580
   581*CHECK-FATPRODU-TRAVADO.
   582*
   583*    IF   FS-FATPRODU = "99" OR "9D"
   584*         CALL "GRISAM" USING ER-FATPRODU
   585*    END-IF.
   586*
   587 DECLARATIVES-FTPROAUX SECTION.
   588
   589     USE AFTER ERROR PROCEDURE ON FTPROAUX.
   590
   591*CHECK-FTPROAUX-TRAVADO.
   592*
   593*    IF   FS-FTPROAUX = "99" OR "9D"
   594*         CALL "GRISAM" USING ER-FTPROAUX
   595*    END-IF.
   596*
   597 END DECLARATIVES.
   598
   599 000 SECTION.
   600 000-INICIO.
   601
   602     IF   VEZ = 1
   603          MOVE "FATU908 "        TO LINK-KEYS404-PROGRAMA
   604          CALL "KEYS404"      USING LINK-KEYS404
   605          MOVE LINK-KEYS404-OCO1 TO W-FATU908-OCO1
   606          MOVE LINK-KEYS404-OCO2 TO W-FATU908-OCO2
   607          CANCEL "KEYS404"
   608
   609          CALL "GRACOR" USING PARAMETROS-GRACOR
   610
   611*         IF W-FATU908-ACC-PALAVRA
   612*            PERFORM 700-ACC-PALAVRA-CHAVE
   613*         ELSE
   614*            MOVE SPACES TO WP-PALAVRA
   615*         END-IF
   616
   617          MOVE 2               TO VEZ
   618          CALL "GRFILE"     USING LB-FATPRODU
   619          MOVE LB-FATPRODU     TO ID-FATPRODU
   620          MOVE WORK-NUM-TABELA TO ID-NUM-TABELA
   621          CALL "GRFILE"     USING LB-FTPROAUX
   622          MOVE LB-FTPROAUX     TO ID-FTPROAUX
   623          MOVE WORK-NUM-TABELA TO ID-NUM-TABELAX
   624
   625          IF WORK-CODMARCA NOT NUMERIC
   626             MOVE ZEROS    TO  WORK-CODMARCA
   627          END-IF
   628          IF WORK-COD-PRD  NOT NUMERIC
   629             MOVE ZEROS TO WORK-COD-PRD
   630          END-IF
   631     END-IF
   632
   633*   PERFORM TEST AFTER UNTIL (FS-FATPRODU NOT = "99")
   634*                        AND (FS-FATPRODU NOT = "9D")
   635     EVALUATE TRUE
   636         WHEN OPEN-FILE
   637              OPEN INPUT FATPRODU
   638              IF   FS-FATPRODU > "09"
   639                   CALL "GRISAM" USING ER-FATPRODU
   640              END-IF
   641              OPEN INPUT FTPROAUX
   642              IF   FS-FTPROAUX > "09"
   643                   CALL "GRISAM" USING ER-FTPROAUX
   644              END-IF
   645
   646              IF ORDER-X = 2 AND WORK-CODMARCA NOT = ZEROS
   647                 SET LE-FTPROAUX TO TRUE
   648                 MOVE  WORK-CODMARCA TO FTPROAUX-CODMARCA
   649                 START FTPROAUX KEY  IS NOT < FTPROAUX-CHAVE1
   650                 MOVE  ZEROS TO REGISTROS
   651                 PERFORM TEST AFTER UNTIL FS-FTPROAUX > "09"
   652                                   OR REGISTROS = VERTICAL-LENGTH
   653                         PERFORM 700-LER-FTPROAUX-NEXT
   654                         IF  FS-FTPROAUX < "10"  AND LIBEROU
   655                             ADD 1 TO REGISTROS
   656                         END-IF
   657                 END-PERFORM
   658              ELSE
   659                 SET LE-FATPRODU TO TRUE
   660                 MOVE  WORK-CODMARCA TO FATPRODU-CODMARCA
   661                 MOVE  WORK-COD-PRD  TO FATPRODU-CODIGO
   662                 START FATPRODU KEY  IS NOT < FATPRODU-CHAVE
   663                 MOVE  ZEROS TO REGISTROS
   664                 PERFORM TEST AFTER UNTIL FS-FATPRODU > "09"
   665                                   OR REGISTROS = VERTICAL-LENGTH
   666                         PERFORM 700-LER-FATPRODU-NEXT
   667                         IF  FS-FATPRODU < "10"  AND LIBEROU
   668                             ADD 1 TO REGISTROS
   669                         END-IF
   670                 END-PERFORM
   671              END-IF
   672              IF   REGISTROS = 0
   673                   MOVE 1 TO REGISTROS
   674              END-IF
   675              IF   REGISTROS < VERTICAL-LENGTH
   676                   MOVE REGISTROS TO VERTICAL-LENGTH
   677              END-IF
   678         WHEN CLOSE-FILE
   679              CLOSE FATPRODU FTPROAUX
   680              CANCEL "KEYS404"
   681              CANCEL "GRFILE"
   682              CANCEL "GRISAM"
   683         WHEN BEGIN-FILE
   684              INITIALIZE FATPRODU-REG FTPROAUX-REG
   685              EVALUATE ORDER-X
   686                  WHEN 1
   687                       MOVE WORK-CODMARCA TO FATPRODU-CODMARCA
   688                       MOVE WORK-COD-PRD  TO FATPRODU-CODIGO
   689                       START FATPRODU KEY NOT < FATPRODU-CHAVE
   690                  WHEN 2
   691                       IF LE-FATPRODU
   692                          START FATPRODU KEY NOT < FATPRODU-CHAVE1
   693                       ELSE
   694                          MOVE WORK-CODMARCA TO FTPROAUX-CODMARCA
   695                          START FTPROAUX KEY NOT < FTPROAUX-CHAVE1
   696                       END-IF
   697              END-EVALUATE
   698         WHEN END-FILE
   699              MOVE HIGH-VALUE TO FATPRODU-REG FTPROAUX-REG
   700              EVALUATE ORDER-X
   701                  WHEN 1
   702                       MOVE WORK-CODMARCA TO FATPRODU-CODMARCA
   703                       MOVE WORK-COD-PRD  TO FATPRODU-CODIGO
   704                       START FATPRODU KEY NOT > FATPRODU-CHAVE
   705                  WHEN 2
   706                       IF LE-FATPRODU
   707                          START FATPRODU KEY NOT > FATPRODU-CHAVE1
   708                       ELSE
   709                          MOVE WORK-CODMARCA TO FTPROAUX-CODMARCA
   710                          START FTPROAUX KEY NOT > FTPROAUX-CHAVE1
   711                       END-IF
   712              END-EVALUATE
   713         WHEN READ-NEXT
   714              IF LE-FTPROAUX
   715                 PERFORM 700-LER-FTPROAUX-NEXT
   716                 IF (FS-FTPROAUX > "09") AND NAO-LIBEROU
   717                    SET AT-END TO TRUE
   718                 END-IF
   719              ELSE
   720                 PERFORM 700-LER-FATPRODU-NEXT
   721                 IF (FS-FATPRODU > "09") AND NAO-LIBEROU
   722                    SET AT-END TO TRUE
   723                 END-IF
   724              END-IF
   725         WHEN READ-PREVIOUS
   726              IF LE-FTPROAUX
   727                 PERFORM 700-LER-FTPROAUX-PREVIOUS
   728                 IF (FS-FTPROAUX > "09") AND NAO-LIBEROU
   729                    SET AT-END TO TRUE
   730                 END-IF
   731              ELSE
   732                 PERFORM 700-LER-FATPRODU-PREVIOUS
   733                 IF (FS-FATPRODU > "09") AND NAO-LIBEROU
   734                    SET AT-END TO TRUE
   735                 END-IF
   736              END-IF
   737         WHEN NOT-LESS
   738              EVALUATE ORDER-X
   739                 WHEN 1
   740                    MOVE STRING-1 TO W-COD-PRODUTO
   741                    MOVE W-CODMARCA TO FATPRODU-CODMARCA
   742                    MOVE W-COD-PROD TO FATPRODU-CODIGO
   743                    MOVE W-DIG-PROD TO FATPRODU-DIGITO
   744                    START FATPRODU KEY NOT < FATPRODU-CHAVE
   745                          INVALID KEY
   746                                  SET AT-END TO TRUE
   747                     END-START
   748                 WHEN 2
   749                    IF LE-FATPRODU
   750                       MOVE STRING-2(1: 15) TO FATPRODU-NOME-BUSCA
   751                       INSPECT FATPRODU-NOME-BUSCA CONVERTING
   752                               MINUSCULAS TO MAIUSCULAS
   753                       START FATPRODU KEY NOT < FATPRODU-CHAVE1
   754                          INVALID KEY
   755                                    SET AT-END TO TRUE
   756                       END-START
   757                    ELSE
   758                       MOVE STRING-1 TO W-COD-PRODUTO
   759                       MOVE W-CODMARCA TO FTPROAUX-CODMARCA
   760                       MOVE STRING-2(1: 40) TO FTPROAUX-DESCRICAO
   761                       INSPECT FTPROAUX-DESCRICAO  CONVERTING
   762                               MINUSCULAS TO MAIUSCULAS
   763                       START FTPROAUX KEY NOT < FTPROAUX-CHAVE1
   764                          INVALID KEY
   765                                    SET AT-END TO TRUE
   766                       END-START
   767
   768                    END-IF
   769              END-EVALUATE
   770         WHEN NOT-GREATER
   771              EVALUATE ORDER-X
   772                 WHEN 1
   773                    MOVE STRING-1 TO W-COD-PRODUTO
   774                    MOVE W-CODMARCA TO FATPRODU-CODMARCA
   775                    MOVE W-COD-PROD TO FATPRODU-CODIGO
   776                    MOVE W-DIG-PROD TO FATPRODU-DIGITO
   777                    START FATPRODU KEY NOT > FATPRODU-CHAVE
   778                          INVALID KEY
   779                                  SET AT-END TO TRUE
   780                     END-START
   781                 WHEN 2
   782                    IF LE-FATPRODU
   783                       MOVE STRING-2(1: 15) TO FATPRODU-NOME-BUSCA
   784                       INSPECT FATPRODU-NOME-BUSCA CONVERTING
   785                               MINUSCULAS TO MAIUSCULAS
   786                       START FATPRODU KEY NOT > FATPRODU-CHAVE1
   787                             INVALID KEY
   788                                  SET AT-END TO TRUE
   789                       END-START
   790                    ELSE
   791                       MOVE STRING-1 TO W-COD-PRODUTO
   792                       MOVE W-CODMARCA TO FTPROAUX-CODMARCA
   793                       MOVE STRING-2(1: 40) TO FTPROAUX-DESCRICAO
   794                       INSPECT FTPROAUX-DESCRICAO  CONVERTING
   795                               MINUSCULAS TO MAIUSCULAS
   796                       START FTPROAUX KEY NOT > FTPROAUX-CHAVE1
   797                             INVALID KEY
   798                                  SET AT-END TO TRUE
   799                       END-START
   800                    END-IF
   801              END-EVALUATE
   802     END-EVALUATE
   803*    END-PERFORM
   804
   805     MOVE SPACES TO STRING-1 STRING-2
   806     IF (LE-FATPRODU AND FS-FATPRODU < "10" AND LIBEROU) OR
   807        (LE-FTPROAUX AND FS-FTPROAUX < "10" AND LIBEROU)
   808        MOVE FATPRODU-CODMARCA   TO W-CODMARCA
   809        MOVE "/" TO FILLER-1
   810        MOVE FATPRODU-CODIGO     TO W-COD-PROD
   811        MOVE "-" TO FILLER-2
   812        MOVE FATPRODU-DIGITO     TO W-DIG-PROD
   813        MOVE W-COD-PRODUTO       TO STRING-1
   814*       MOVE FATPRODU-NOME-BUSCA TO STRING-2(01: 15)
   815        MOVE FATPRODU-DESCRICAO  TO STRING-2(01:40)
   816        MOVE FTPROAUX-DESCRICAO2 TO STRING-2(41:40)
   817        MOVE FTPROAUX-DESCRICAO3 TO STRING-2(81:40)
   818     END-IF
   819     GOBACK.
   820
   821 700-LER-FATPRODU-NEXT.
   822     SET NAO-LIBEROU TO TRUE
   823     MOVE ZEROS TO CONTADOR.
   824     PERFORM WITH TEST AFTER UNTIL LIBEROU OR FS-FATPRODU > "09"
   825        READ FATPRODU NEXT RECORD ignore lock
   826        IF FS-FATPRODU < "10"
   827           PERFORM 700-CONSISTIR THRU 700-99-FIM
   828        END-IF
   829     END-PERFORM.
   830
   831 700-LER-FATPRODU-PREVIOUS.
   832     SET NAO-LIBEROU TO TRUE
   833     MOVE ZEROS TO CONTADOR.
   834     PERFORM WITH TEST AFTER UNTIL LIBEROU OR FS-FATPRODU > "09"
   835        READ FATPRODU PREVIOUS RECORD ignore lock
   836        IF FS-FATPRODU < "10"
   837           PERFORM 700-CONSISTIR THRU 700-99-FIM
   838        END-IF
   839     END-PERFORM.
   840
   841 700-LER-FTPROAUX-NEXT.
   842     SET NAO-LIBEROU TO TRUE
   843     MOVE ZEROS TO CONTADOR.
   844     PERFORM WITH TEST AFTER UNTIL LIBEROU OR FS-FTPROAUX > "09"
   845        READ FTPROAUX NEXT RECORD ignore lock
   846        IF FS-FTPROAUX < "10"
   847           PERFORM 700-CONSISTIR THRU 700-99-FIM
   848        END-IF
   849     END-PERFORM.
   850
   851 700-LER-FTPROAUX-PREVIOUS.
   852     SET NAO-LIBEROU TO TRUE
   853     MOVE ZEROS TO CONTADOR.
   854     PERFORM WITH TEST AFTER UNTIL LIBEROU OR FS-FTPROAUX > "09"
   855        READ FTPROAUX PREVIOUS RECORD ignore lock
   856        IF FS-FTPROAUX < "10"
   857           PERFORM 700-CONSISTIR THRU 700-99-FIM
   858        END-IF
   859     END-PERFORM.
   860
   861 700-CONSISTIR.
   862     MOVE SPACES TO W-MSG
   863     STRING "Pesquisando ... " FATPRODU-CHAVE
   864            DELIMITED BY SIZE INTO W-MSG
   865     DISPLAY T-W-MSG
   866
   867     IF LE-FTPROAUX
   868        INITIALIZE FATPRODU-REG
   869        MOVE FTPROAUX-CHAVE TO FATPRODU-CHAVE
   870        READ FATPRODU IGNORE LOCK
   871        IF FS-FATPRODU > "09"
   872           SET NAO-LIBEROU TO TRUE
   873           GO 700-99-FIM
   874        END-IF
   875     ELSE
   876        INITIALIZE FTPROAUX-REG
   877        MOVE FATPRODU-CHAVE TO FTPROAUX-CHAVE
   878        READ FTPROAUX IGNORE LOCK
   879        IF FS-FTPROAUX > "09"
   880           SET NAO-LIBEROU TO TRUE
   881           GO 700-99-FIM
   882        END-IF
   883     END-IF
   884
   885*    IF W-FATU908-N-MOSTRA-INATIVO AND LK-FTPROAUX-INATIVO
   886*       SET NAO-LIBEROU TO TRUE
   887*       GO 700-99-FIM
   888*    END-IF
   889
   890     EVALUATE ORDER-X
   891        WHEN 1
   892           IF (WORK-CODMARCA NOT = ZEROS) AND
   893              (WORK-CODMARCA NOT = FATPRODU-CODMARCA)
   894              MOVE "10" TO FS-FATPRODU
   895              SET NAO-LIBEROU TO TRUE
   896              GO 700-99-FIM
   897           END-IF
   898        WHEN 2
   899           IF (WORK-CODMARCA NOT = ZEROS) AND
   900              (WORK-CODMARCA NOT = FTPROAUX-CODMARCA)
   901              MOVE "10" TO FS-FTPROAUX
   902              SET NAO-LIBEROU TO TRUE
   903              GO 700-99-FIM
   904           END-IF
   905     END-EVALUATE
   906
   907     IF (W-FATU908-TODOS-PROD         OR
   908        (W-FATU908-PROD-CHAVE-YAM    AND
   909        FATPRODU-CODIGO(04:04) = ZEROS))
   910        CONTINUE
   911     ELSE
   912        SET NAO-LIBEROU TO TRUE
   913        GO 700-99-FIM
   914     END-IF
   915
   916     IF WP-PALAVRA NOT = SPACES
   917        MOVE FATPRODU-DESCRICAO TO WP-NOME
   918        INITIALIZE WP-RETORNO
   919        INSPECT WP-NOME CONVERTING
   920                MINUSCULAS TO MAIUSCULAS
   921        INSPECT WP-NOME TALLYING WP-RETORNO
   922                FOR ALL WP-PALAVRA(1:WP-TAM)
   923        IF NAO-ACHOU-PALAVRA
   924           SET NAO-LIBEROU TO TRUE
   925           GO 700-99-FIM
   926        END-IF
   927     END-IF
   928
   929
   930     SET LIBEROU TO TRUE
   931     MOVE SPACES TO W-MSG
   932     DISPLAY T-W-MSG.
   933
   934 700-99-FIM. EXIT.
   935
   936*700-ACC-PALAVRA-CHAVE.
   937*    COPY ROT020.ROT.
   938
   939 700-SALVA-TELA.
   940     MOVE ZEROS TO ROW-NUMBER COL-NUMBER.
   941     CALL "CBL_READ_SCR_CHATTRS" USING SCREEN-POSITION
   942                                       SCREEN-BUFFER
   943                                       ATTRIBUTE-BUFFER
   944                                       LENGHT-BUFFER.
   945
   946 700-RESTAURA-TELA.
   947     CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
   948                                        SCREEN-BUFFER
   949                                        ATTRIBUTE-BUFFER
   950                                        LENGHT-BUFFER.
   951
   952
   953 END PROGRAM WSTU908.
