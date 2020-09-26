       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWISAM.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  05/06/1988.
       SECURITY.      *************************************************
                      *                                               *
                      *   Tratamento de file status                   *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                                       CALL-CONVENTION 74 IS WINAPI.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT OPTIONAL HELP ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-HELP
                  LOCK MODE     IS EXCLUSIVE.

           SELECT OPTIONAL MSG ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-MSG.

           SELECT fhneterr ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-fhneterr.

       DATA DIVISION.
       FILE SECTION.

       FD  HELP
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-HELP.

       01  HELP-REG              PIC X(080).

       FD  MSG
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-MSG.

       01  MSG-REG.
           05 MSG-KEY                   PIC  X(005).
           05 FILLER                    PIC  X(001).
           05 MSG-TEXT                  PIC  X(046).

       FD  fhneterr
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-fhneterr.

       01  fhneterr-REG.
           05 FILLER                    PIC  X(050).
           05 fhneterr-TEXT             PIC  X(074).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05  CmdShow                 PIC 9(4) COMP-5.
           05  CmdStatus               PIC 9(4) COMP-5.
           05 cobware                  PIC  X(255) VALUE SPACES.
           05 CmdLine                  PIC  X(255) VALUE SPACES.
           05 SKIPUSER                 PIC  X(003) VALUE SPACES.
           05 CWLOGW                   PIC  X(003) VALUE SPACES.
           05 NOLOG                    PIC  9(001) VALUE 0.
           05 USUARIO                  PIC  X(030) VALUE SPACES.
           05 TASK                     PIC  9(006) VALUE 0.
           05 TESTE-LB                 PIC  X(004) VALUE SPACES.
           05 ER-ANTERIOR              PIC  X(050) VALUE SPACES.
           05 CWSQLC                   PIC  X(002) VALUE SPACES.
           05 LB-ARQUIVO               PIC  X(255) VALUE SPACES.
           05 WS-ARQUIVO               PIC  X(255) VALUE SPACES.
           05 AUTOREBUILD              PIC  X(003) VALUE SPACES.
           05 FATOR-W           COMP-X PIC  9(002) VALUE 0.
           05 ERRO-CHAVE               PIC X(005) VALUE SPACES.
           05 N3                       PIC 9(003) VALUE 0.
           05 E                        PIC 9(005) VALUE 0.
           05 FSSERVER                 PIC X(050) VALUE SPACES.
           05 CCITCP2                  PIC X(050) VALUE SPACES.
           05 FHREDIR                  PIC X(050) VALUE SPACES.
           05 STRING-PARAMETER         PIC X(600).
           05 STATUS-PARAMETER         PIC 9(004) COMP-X.
           05 TEMPO-3.
              10 HH-3                  PIC  9(002).
              10 MM-3                  PIC  9(002).
              10 SS-3                  PIC  9(002).
              10 CC-3                  PIC  9(002).
           05 SEGUNDOS-1               PIC  9(008) VALUE 0.
           05 SEGUNDOS-2               PIC  9(008) VALUE 0.
           05 SEGUNDOS                 PIC  9(002) VALUE 0.
           05 CURSOR-POSITION.
              10                       PIC  9(004) COMP-X VALUE 00.
              10                       PIC  9(004) COMP-X VALUE 00.
           05 CWMENU                   PIC  X(001) VALUE SPACE.
           05 OPERADOR                 PIC  X(030) VALUE SPACES.
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 HOJE.
              10 AA-H-1                PIC  9(002) VALUE ZEROS.
              10 MM-H-1                PIC  9(002) VALUE ZEROS.
              10 DD-H-1                PIC  9(002) VALUE ZEROS.
           05 TEMPO.
              10 HH-1                  PIC  9(002) VALUE ZEROS.
              10 MM-1                  PIC  9(002) VALUE ZEROS.
              10 SS-1                  PIC  9(002) VALUE ZEROS.
              10 DD-1                  PIC  9(002) VALUE ZEROS.
           05 I                        PIC  9(003) VALUE ZERO.
           05 Y                        PIC  9(002) VALUE ZERO.
           05 CX                       PIC  9(002) VALUE ZERO.
           05 NADA                     PIC  X(001) VALUE SPACE.
           05 IGNORA                   PIC  X(065) VALUE "Ignora...".
           05 SV-CWLOGF                PIC  X(052) VALUE SPACES.
           05 ER-HELP.
              10 FS-HELP               PIC  X(002) VALUE "00".
              10 LB-HELP               PIC  X(255) VALUE SPACES.
           05 ER-MSG.
              10 FS-MSG                PIC  X(002) VALUE "00".
              10 LB-MSG                PIC  X(255) VALUE
                 "$COBOLWARE/cwisam.txt".
           05 MENSAGEM                 PIC  X(80) VALUE SPACES.
           05 ER-fhneterr.
              10 FS-fhneterr           PIC  X(002) VALUE "00".
              10 LB-fhneterr           PIC  X(255) VALUE SPACES.

       01  MENSAGENS.
           05 PIC X(52) VALUE "FS02  Existe duplicidade de chave".
           05 PIC X(52) VALUE
              "FS04  Tamanho incompat¡vel c/atributos do arquivo".
           05 PIC X(52) VALUE "FS05  Arquivo criado na opera‡Æo".
           05 PIC X(52) VALUE
              "FS07  REEL/UNIT nÆo suportadas pelo dispositivo".
           05 PIC X(52) VALUE "FS10  Fim de arquivo".
           05 PIC X(52) VALUE
              "FS14  Chave relativa fora dos limites do arquivo".
           05 PIC X(52) VALUE
              "FS21  InclusÆo fora de ordem em acesso rand“mico".
           05 PIC X(52)
              VALUE "FS22  Duplicidade de chave nÆo permitida".
           05 PIC X(52) VALUE "FS23  Registro nÆo encontrado".
           05 PIC X(52) VALUE "FS24  Viola‡Æo dos limites do arquivo".
           05 PIC X(52) VALUE "FS24  Chave relativa inconcistente".
           05 PIC X(52) VALUE "FS30  Arquivo nÆo encontrado".
           05 PIC X(52) VALUE "FS34  Grava‡Æo fora dos limites".
           05 PIC X(52) VALUE "FS35  Arquivo nÆo encontrado".
           05 PIC X(52) VALUE
              "FS37  Opera‡Æo impr¢pria para o modo de abertura".
           05 PIC X(52) VALUE
              "FS38  Arquivo bloqueado pelo pr¢prio programa".
           05 PIC X(52) VALUE
              "FS39  Estrutura do arquivo diverge da declarada".
           05 PIC X(52) VALUE "FS41  Abertura redundante".
           05 PIC X(52) VALUE "FS42  Fechamento redundante".
           05 PIC X(52) VALUE
              "FS43  Opera‡Æo para registro nÆo posicionado".
           05 PIC X(52) VALUE
              "FS44  Tamanho do registro fora do limite declarado".
           05 PIC X(52) VALUE "FS46  Leitura j  encerrada".
           05 PIC X(52) VALUE
              "FS47  Leitura nÆo dispon¡vel".
           05 PIC X(52) VALUE
              "FS48  Modo de abertura s¢ permite leitura".
           05 PIC X(52) VALUE
              "FS49  Modo de abertura nÆo permite altera‡äes".
           05 PIC X(52) VALUE
              "RT001 Falha de manipula‡Æo do cache".
           05 PIC X(52) VALUE
              "RT002 Opera‡Æo para arquivo fechado".
           05 PIC X(52) VALUE
              "RT003 Dispositivo s¢ suporta acesso sequencial".
           05 PIC X(52) VALUE "RT004 Nome de arquivo inv lido".
           05 PIC X(52) VALUE "RT005 Disposit¡vo nÆo supotado".
           05 PIC X(52) VALUE
              "RT006 Grava‡Æo em arquivo aberto para leitura".
           05 PIC X(52) VALUE
              "RT007 Falta espa‡o em disco para realizar a opera‡Æo".
           05 PIC X(52) VALUE
              "RT008 Leitura de um arquivo aberto para grava‡Æo".
           05 PIC X(52) VALUE "RT009 Pasta inexistente ou inacess¡vel".
           05 PIC X(52) VALUE "RT010 Nome de arquivo inv lido".
           05 PIC X(52) VALUE "RT012 Abertura redundante".
           05 PIC X(52) VALUE "RT013 Arquivo nÆo encontrado".
           05 PIC X(52) VALUE
              "RT014 Excesso de arquivos abertos simultƒneamente".
           05 PIC X(52) VALUE
              "RT015 Excesso de arquivos indexados abertos".
           05 PIC X(52) VALUE "RT016 Excesso de dispositivos abertos".
           05 PIC X(52) VALUE "RT017 Erro de registro".
           05 PIC X(52) VALUE
              "RT018 Fim de arquivo antes do fim de registro".
           05 PIC X(52) VALUE
              "RT019 Regrava‡Æo impropria para o modo de abertura".
           05 PIC X(52) VALUE "RT020 Dispositivo ou recurso ocupado.".
           05 PIC X(52) VALUE "RT021 Nome especificado ‚ uma pasta".
           05 PIC X(52) VALUE "RT022 Falha de acesso na abertura".
           05 PIC X(52) VALUE "RT023 Falha de acesso no fechamento".
           05 PIC X(52) VALUE "RT024 Error f¡sico no disco".
           05 PIC X(52) VALUE
              "RT025 Erro de dados no sistema operacional".
           05 PIC X(52) VALUE
              "RT026 Erro de acesso, o disco pode estar danificado".
           05 PIC X(52) VALUE "RT027 Dispositivo nÆo dispon¡vel".
           05 PIC X(52) VALUE "RT028 Dispositivo sem espa‡o".
           05 PIC X(52) VALUE
              "RT029 Para deletar um arquivo ele deve estar fechado".
           05 PIC X(52) VALUE
              "RT030 Acesso permitido apenas para leitura".
           05 PIC X(52) VALUE
              "RT031 Proriet rio do arquivo nÆo permite o acesso".
           05 PIC X(52) VALUE
              "RT032 Excesso de arquivos ou falha de reconhecimento".
           05 PIC X(52) VALUE "RT033 Erro f¡sico no dispositivo".
           05 PIC X(52) VALUE
              "RT034 Opera‡Æo impr¢pria para o modo de leitura".
           05 PIC X(52) VALUE "RT035 PermissÆo incorreta".
           05 PIC X(52) VALUE "RT036 Arquivo j  existe".
           05 PIC X(52) VALUE "RT037 Acesso negado".
           05 PIC X(52) VALUE "RT038 Disco incompat¡vel".
           05 PIC X(52) VALUE "RT039 Arquivo incompat¡vel".
           05 PIC X(52) VALUE
              "RT040 Suporte a caracteres estrangeiros indispon¡vel".
           05 PIC X(52) VALUE "RT041 Volume de ¡ndices corrompido".
           05 PIC X(52) VALUE "RT042 Acesso interrompido".
           05 PIC X(52) VALUE "RT043 Volume de indices indisponivel".
           05 PIC X(52) VALUE "RT045 Dados corrompidos".
           05 PIC X(52) VALUE
              "RT047 Excedida a capacidade de duplica‡Æo de chaves".
           05 PIC X(52) VALUE "RT065 Viola‡Æo de compartilhamento".
           05 PIC X(52) VALUE
              "RT066 Excedido o limite de duplicidade de chaves".
           05 PIC X(52) VALUE "RT067 Arquivo indexado fechado".
           05 PIC X(52) VALUE
              "RT068 Registro em uso por outro processo".
           05 PIC X(52) VALUE "RT069 Falha interna no m¢dulo ISAM".
           05 PIC X(52) VALUE
              "RT070 Limite de abertura de indexados excedido".
           05 PIC X(52) VALUE
              "RT071 Formato do volume de ¡ndices inv lido".
           05 PIC X(52) VALUE "RT072 Fim de arquivo indexado".
           05 PIC X(52) VALUE
              "RT073 Registro nÆo encontrado no arquivo indexado".
           05 PIC X(52) VALUE "RT074 Falha de acesso dados perdidos".
           05 PIC X(52) VALUE "RT075 Volume de ¡ndices muito longo".
           05 PIC X(52) VALUE "RT077 Falha interna no m¢dulo ISAM".
           05 PIC X(52) VALUE "RT078 Descri‡Æo de chave inv lida".
           05 PIC X(52) VALUE "RT081 Chave j  existe no arquivo".
           05 PIC X(52) VALUE "RT100 Opera‡Æo inv lida".
           05 PIC X(52) VALUE "RT124 Falha de acesso ao FileShare".
           05 PIC X(52) VALUE
              "RT101 Opera‡Æo inv lida para um arquivo indexado".
           05 PIC X(52) VALUE
              "RT102 Estrutura nÆo comporta o tamanho lido".
           05 PIC X(52) VALUE "RT104 Nome de arquivo nulo".
           05 PIC X(52) VALUE "RT105 Erro de aloca‡Æo de mem¢ria".
           05 PIC X(52) VALUE
              "RT129 Tentativa de acessar ocorrˆncia relativa zero".
           05 PIC X(52) VALUE "RT135 O Arquivo precisa existir".
           05 PIC X(52) VALUE "RT138 Arquivo travado".
           05 PIC X(52) VALUE
              "RT139 Tamanho inv lido ou chave inconsistentes".
           05 PIC X(52) VALUE "RT141 Abertura redundante".
           05 PIC X(52) VALUE "RT142 Arquivo j  estava fechado".
           05 PIC X(52) VALUE
              "RT143 Altera‡Æo de dados sem o devido acesso".
           05 PIC X(52) VALUE
              "RT146 Leitura sequencial nÆo posicionada".
           05 PIC X(52) VALUE
              "RT147 Modo de abertura s¢ permite grava‡Æo".
           05 PIC X(52) VALUE
              "RT148 Modo de abertura s¢ permite leitura".
           05 PIC X(52) VALUE
              "RT149 Modo de abertura nÆo permite altera‡äes".
           05 PIC X(52) VALUE
              "RT151 Leitura rand“mica para acesso sequencial".
           05 PIC X(52) VALUE
              "RT152 Modo de abertura nÆo permite altera‡äes".
           05 PIC X(52) VALUE
              "RT158 Regrava‡Æo de arquivo texto nÆo permitida".
           05 PIC X(52) VALUE
              "RT159 Erro de formata‡Æo no arquvo texto".
           05 PIC X(52) VALUE
              "RT161 Cabe‡alho do arquivo nÆo encontrado".
           05 PIC X(52) VALUE "RT173 Program nÆo encontrado na chamada".
           05 PIC X(52) VALUE
              "RT180 Marca fim-de-arquivo em localiza‡Æo indevida".
           05 PIC X(52) VALUE
              "RT182 Redirecionamento de console inconsistente".
           05 PIC X(52) VALUE
              "RT183 Altera‡Æo de arquivo texto nÆo permitida".
           05 PIC X(52) VALUE "RT188 Nome de arquivo muito longo".
           05 PIC X(52) VALUE
              "RT193 Falha na identifica‡Æo do tamanho do registro".
           05 PIC X(52) VALUE
              "RT194 Tamanho de arquivo excede limites do sistema".
           05 PIC X(52) VALUE "RT195 Sequˆncia de opera‡Æo il¢gica".
           05 PIC X(52) VALUE "RT196 N£mero do registro muito grande".
           05 PIC X(52) VALUE "RT210 Acesso temporariamente negado".
           05 PIC X(52) VALUE
              "RT213 Execedido o limite de travamento de registros".
           05 PIC X(52) VALUE
              "RT218 MULTIPLE REEL/UNIT inconsistente".
           05 PIC X(52) VALUE
              "RT219 Capacidade de compartilhamento exaurida".
       01  REDEFINES MENSAGENS.
           05 FSTABLE OCCURS 118 INDEXED BY M.
              10 FSCODE PIC X(05).
              10        PIC X(01).
              10 FSTEXT PIC X(46).

       COPY CWTIME.
       COPY CWSEND.
       COPY CWHELP.
       COPY CWUNIX.

       LINKAGE SECTION.

       01  ER-ARQUIVO.
           05 FS-ARQUIVO.
              10 RT-NUMBER-1     COMP-X PIC  9(002).
              10 RT-NUMBER-2     COMP-X PIC  9(002).
           05 LK-ARQUIVO                PIC  X(001).

       PROCEDURE DIVISION USING ER-ARQUIVO.

       000-INICIO.

           MOVE SPACES TO WS-ARQUIVO
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH WS-ARQUIVO
                         OR LK-ARQUIVO(I:1) = SPACES OR LOW-VALUES
                   MOVE LK-ARQUIVO(I:1) TO WS-ARQUIVO(I:1)
           END-PERFORM
           IF   FS-ARQUIVO = "00"
           OR  (ER-ARQUIVO = ER-ANTERIOR
           AND (ER-ARQUIVO NOT = "9D"))
                MOVE SPACES TO ER-ANTERIOR
                EXIT PROGRAM
           END-IF
           MOVE ER-ARQUIVO TO ER-ANTERIOR
           MOVE 0          TO NOLOG
           MOVE SPACES     TO CWSEND-SCREENS
           ON   1
                DISPLAY "CWISAM"  UPON ENVIRONMENT-NAME
                ACCEPT  LB-HELP   FROM ENVIRONMENT-VALUE
                MOVE LB-HELP TO TESTE-LB
                INSPECT TESTE-LB CONVERTING "ofn" TO "OFN"
                DISPLAY "FHREDIR" UPON ENVIRONMENT-NAME
                ACCEPT   FHREDIR  FROM ENVIRONMENT-VALUE
                DISPLAY "CWREBUILD" UPON ENVIRONMENT-NAME
                ACCEPT AUTOREBUILD FROM ENVIRONMENT-VALUE
                INSPECT AUTOREBUILD CONVERTING "ofn" TO "OFN"
                CALL "CWUNIX" USING PARAMETROS-CWUNIX.

           IF   FS-ARQUIVO = "9|"
      *    AND  WS-ARQUIVO = "cwused"
                MOVE 'fhneterr.log' TO LB-fhneterr
                OPEN INPUT fhneterr
                IF FS-fhneterr NOT = '00'
                   MOVE 'fhredir.msg' TO LB-fhneterr
                   OPEN INPUT fhneterr
                END-IF
                MOVE 2             TO CWSEND-OPTION
                MOVE " ~Ajuda "    TO CWSEND-SCREEN (1)
                MOVE " ~Fechar "   TO CWSEND-SCREEN (2)
                IF FS-fhneterr = '00'
                   MOVE " ~Detalhes"   TO CWSEND-SCREEN (3)
                   CLOSE fhneterr
                END-IF
                MOVE SPACES        TO CWSEND-MSG
                DISPLAY "CWGETL"   UPON ENVIRONMENT-NAME
                DISPLAY "OFF"      UPON ENVIRONMENT-VALUE
                DISPLAY "FSSERVER" UPON ENVIRONMENT-NAME
                ACCEPT FSSERVER FROM ENVIRONMENT-VALUE
                DISPLAY "CCITCP2" UPON ENVIRONMENT-NAME
                ACCEPT CCITCP2 FROM ENVIRONMENT-VALUE
                IF (FSSERVER NOT = SPACES)
                OR (CCITCP2 NOT = SPACES)
                STRING "Falha de acesso ao FileShare ("
                                    DELIMITED BY SIZE
                       WS-ARQUIVO   DELIMITED BY SPACE
                       ","
                       FSSERVER     DELIMITED BY SPACE
                       ","
                       CCITCP2      DELIMITED BY SPACE
                       ")"          DELIMITED BY SIZE
                       INTO CWSEND-MSG
                ELSE
                STRING "Falha de acesso ao FileShare ("
                                    DELIMITED BY SIZE
                       WS-ARQUIVO   DELIMITED BY SPACE
                       ")"          DELIMITED BY SIZE
                       INTO CWSEND-MSG
                END-IF
KS              DISPLAY 'SKIPUSER' UPON ENVIRONMENT-NAME
KS              ACCEPT SKIPUSER FROM ENVIRONMENT-VALUE
                PERFORM TEST AFTER UNTIL CWSEND-OPTION > 3
                        PERFORM 060-SEND THRU 060-99-FIM
                        EVALUATE CWSEND-OPTION
                            WHEN 1
                                 IF SKIPUSER = 'ON'
                                    MOVE SPACES TO CmdLine LB-HELP
                                    display 'cobolware'
                                    upon environment-name
                                    accept cobware
                                    from environment-value
                                    string cobware delimited by space
                                           '\fs.txt' delimited by size
                                                inTO LB-HELP
                                    move 'c:\cwtmp\fs.txt'
                                       to LB-fhneterr
                                    open input  help
                                         output fhneterr
                                    perform test after
                                      until fs-help > '09'
                                        read help into fhneterr-REG
                                        if fs-help < '10'
                                           IF   CWUNIX-OFF
                                           inspect  fhneterr-REG
                                             converting ACENTOS-437
                                                    to ACENTOS-WINDOWS
                                            End-if
                                            write fhneterr-REG
                                        end-if
                                    end-perform
                                    close help
                                          fhneterr
                                    move spaces to CmdLine
                                    IF   CWUNIX-OFF
                                    MOVE 5      TO CmdShow
                                    STRING 'notepad ' delimited by size
                                          LB-fhneterr delimited by space
                                          x'00' delimited by size
                                          into CmdLine
                                    MOVE SPACE TO LB-HELP
                                    CALL WINAPI "WinExec"
                                           USING BY REFERENCE CmdLine
                                                 BY VALUE     CmdShow
                                                 RETURNING    CmdStatus
                                    ELSE
                                    MOVE SPACE TO LB-HELP
                                    STRING 'vi ' delimited by size
                                          LB-fhneterr delimited by space
                                          x'00' delimited by size
                                          into CmdLine
                                    MOVE SPACE TO LB-HELP
                                    CALL 'system' using CmdLine
                                    END-IF
                                 ELSE
                                    MOVE "$COBOLWARE/fs.txt"
                                      TO CWHELP-FILE
                                    MOVE 05 TO CWHELP-LINE
                                    MOVE 10 TO CWHELP-COLUMN
                                    MOVE 62 TO CWHELP-HORIZONTAL-LENGTH
                                    MOVE 17 TO CWHELP-VERTICAL-LENGTH
                                    CALL "CWHELP"
                                    USING PARAMETROS-CWHELP
                                 END-IF
                            WHEN 2 STOP RUN
                            WHEN 3
                                 MOVE LB-fhneterr
                                   TO CWHELP-FILE
                                 IF SKIPUSER = 'ON'
                                    MOVE SPACES TO CmdLine
                                    IF   CWUNIX-OFF
                                    MOVE 5      TO CmdShow
                                    STRING 'notepad ' delimited by size
                                          cwhelp-file delimited by space
                                          x'00' delimited by size
                                          into CmdLine
                                    CALL WINAPI "WinExec"
                                           USING BY REFERENCE CmdLine
                                                 BY VALUE     CmdShow
                                                 RETURNING    CmdStatus
                                    ELSE
                                    STRING 'vi ' delimited by size
                                          cwhelp-file delimited by space
                                          x'00' delimited by size
                                          into CmdLine
                                     call 'system' using CmdLine
                                    END-IF
                                 ELSE
                                    MOVE 05 TO CWHELP-LINE
                                    MOVE 10 TO CWHELP-COLUMN
                                    MOVE 62 TO CWHELP-HORIZONTAL-LENGTH
                                    MOVE 17 TO CWHELP-VERTICAL-LENGTH
                                    CALL "CWHELP"
                                    USING PARAMETROS-CWHELP
                                END-IF
                        END-EVALUATE
                END-PERFORM
                EXIT PROGRAM
           END-IF

           IF   WS-ARQUIVO (1: 1) = "$"
           AND (WS-ARQUIVO (2: 1) NOT = "$")
                MOVE SPACES     TO LB-ARQUIVO
                PERFORM VARYING I FROM LENGTH WS-ARQUIVO
                                    BY -1 UNTIL I = 1
                                  OR WS-ARQUIVO (I: 1) = "\" OR "/"
                         CONTINUE
                END-PERFORM
                IF  I = 1
                    MOVE WS-ARQUIVO TO LB-ARQUIVO
                ELSE
                    SUBTRACT 2 FROM I
                    MOVE WS-ARQUIVO (2: I) TO LB-ARQUIVO
                    ADD 2 TO I
                END-IF
                DISPLAY LB-ARQUIVO UPON ENVIRONMENT-NAME
                MOVE SPACES TO LB-ARQUIVO
                ACCEPT LB-ARQUIVO FROM ENVIRONMENT-VALUE
                IF  LB-ARQUIVO = SPACES
                    MOVE WS-ARQUIVO TO LB-ARQUIVO
                ELSE
                    IF I NOT = 1
                         PERFORM VARYING Y FROM 1 BY 1
                                   UNTIL Y = 49
                                      OR LB-ARQUIVO (Y: 1) = SPACE
                                  CONTINUE
                         END-PERFORM
                         MOVE WS-ARQUIVO (I: ) TO LB-ARQUIVO (Y: )
                    END-IF
                END-IF
           ELSE
                MOVE WS-ARQUIVO TO LB-ARQUIVO
           END-IF

           PERFORM 010-MSG THRU 010-99-FIM

           IF   FS-ARQUIVO = "9+"
           OR ( FS-ARQUIVO = "9|"
           AND (FHREDIR NOT = SPACES))
                STOP RUN
           END-IF.

       000-FIM. EXIT PROGRAM.

       010-MSG.

           SET CWSEND-TIMEOUT-ENABLE TO TRUE
           OPEN INPUT MSG
           IF FS-ARQUIVO (1: 1) = "9"
              MOVE "RT" TO ERRO-CHAVE
              MOVE RT-NUMBER-2 TO N3
              MOVE N3          TO ERRO-CHAVE (3: 3)
           ELSE
              MOVE "FS"       TO ERRO-CHAVE
              MOVE FS-ARQUIVO TO ERRO-CHAVE (3: 2)
           END-IF
           SET M TO 1
           SEARCH FSTABLE AT END
                             MOVE ERRO-CHAVE TO CWSEND-MSG
                  WHEN FSCODE (M) = ERRO-CHAVE
                   MOVE FSTEXT (M) TO CWSEND-MSG
           END-SEARCH
           PERFORM TEST AFTER UNTIL FS-MSG > "09"
                                 OR ERRO-CHAVE = MSG-KEY
                   READ MSG
                     NOT AT END
                         IF MSG-KEY = ERRO-CHAVE
                            MOVE MSG-TEXT TO CWSEND-MSG
                         END-IF
                     END-READ
           END-PERFORM
           CLOSE MSG
           IF (FS-ARQUIVO NOT = "9D")
      *        SET CWSEND-TIMEOUT-DISABLE TO TRUE
               IF  FS-ARQUIVO < "11"
                   SET CWSEND-TIMEOUT-RETRY TO TRUE
               END-IF
               MOVE SPACES TO MENSAGEM
               STRING ERRO-CHAVE DELIMITED BY SIZE
                      " "        DELIMITED BY SIZE
                      LB-ARQUIVO DELIMITED BY SPACE
                            INTO MENSAGEM
               IF  FS-ARQUIVO > "10"
               OR  FS-ARQUIVO = "05"
                   PERFORM 020-GRAVAR-LOG THRU 020-99-FIM
                   MOVE CWSEND-MSG TO MENSAGEM
                   PERFORM 020-GRAVAR-LOG THRU 020-99-FIM
               END-IF
               IF  FS-ARQUIVO = "9)"
               AND (AUTOREBUILD NOT = "OFF")
                   MOVE 3              TO CWSEND-OPTION
                   MOVE "  ~Ajuda "    TO CWSEND-SCREEN (1)
                   MOVE "  ~Fechar "   TO CWSEND-SCREEN (2)
                   MOVE " ~Restaurar_" TO CWSEND-SCREEN (3)
                   PERFORM VARYING I FROM LENGTH OF CWSEND-MSG
                                 BY -1
                                 UNTIL CWSEND-MSG (I: 1) NOT = SPACE
                            CONTINUE
                   END-PERFORM
                   IF CWSEND-MSG (I: 1) NOT = "."
                       ADD  1   TO I
                       MOVE "." TO CWSEND-MSG (I: 1)
                   END-IF
                   PERFORM UNTIL CWSEND-OPTION > 3
                           PERFORM 060-SEND THRU 060-99-FIM
                           EVALUATE CWSEND-OPTION
                               WHEN 1
                                    MOVE "$COBOLWARE/rebuild.txt"
                                      TO CWHELP-FILE
                                    MOVE 07 TO CWHELP-LINE
                                    MOVE 10 TO CWHELP-COLUMN
                                    MOVE 62 TO CWHELP-HORIZONTAL-LENGTH
                                    MOVE 15 TO CWHELP-VERTICAL-LENGTH
                                    CALL "CWHELP"
                                   USING PARAMETROS-CWHELP
                               WHEN 2 STOP RUN
                               WHEN 3 PERFORM 040-REBUILD
                                      THRU    040-99-FIM
                           END-EVALUATE
                   END-PERFORM
               ELSE
                   IF  LB-HELP = SPACES
                       PERFORM 060-SEND THRU 060-99-FIM
                   END-IF
               END-IF
           ELSE
               SET CWSEND-TIMEOUT-RETRY TO TRUE
               MOVE "~Repetir" TO CWSEND-SCREEN (1)
               PERFORM 060-SEND THRU 060-99-FIM
           END-IF.

       010-99-FIM. EXIT.

       020-GRAVAR-LOG.

           INSPECT MENSAGEM (4: )
                   CONVERTING ACENTOS-850 TO ACENTOS-OFF
           IF  LB-HELP NOT = SPACES
               PERFORM 060-SEND THRU 060-99-FIM
           END-IF
           IF   MENSAGEM (66: 6) = SPACES
                CALL "CWGETU" USING USUARIO TASK PROGRAMA "?"
                MOVE TASK TO MENSAGEM (66: 6)
           END-IF

           IF FS-ARQUIVO NOT = X'390E'
              DISPLAY "CWLOGW"     UPON ENVIRONMENT-NAME
              ACCEPT  CWLOGW       FROM ENVIRONMENT-VALUE
              IF CWLOGW NOT = 'ON'
              AND NOLOG = 0
                 DISPLAY "CWLOGI"  UPON ENVIRONMENT-NAME
                 DISPLAY 'ON'      UPON ENVIRONMENT-VALUE
                 CALL "CWLOGW" USING "^" MENSAGEM
                 DISPLAY "CWLOGI"  UPON ENVIRONMENT-NAME
                 DISPLAY 'OFF'     UPON ENVIRONMENT-VALUE
              END-IF
           END-IF.

       020-99-FIM. EXIT.

       040-REBUILD.

           CALL "CWMSGW" USING "230339"
                         "Reconstru‡Æo de ¡ndices em andamento..."
           MOVE SPACES TO CWSEND-MSG STRING-PARAMETER
           MOVE LB-ARQUIVO TO STRING-PARAMETER
           IF   CWUNIX-OFF
                CALL "CALLRB" USING STRING-PARAMETER
                                    STATUS-PARAMETER
           ELSE
                CALL "callrb" USING STRING-PARAMETER
                                    STATUS-PARAMETER
           END-IF
           EVALUATE RETURN-CODE
               WHEN 0 MOVE "Reconstru‡Æo de ¡ndices bem sucedida."
                        TO CWSEND-MSG
                      DISPLAY "CWSQLC" UPON ENVIRONMENT-NAME
                      ACCEPT  CWSQLC   FROM ENVIRONMENT-VALUE
                      IF CWSQLC = "ON"
                         DISPLAY "OK"     UPON ENVIRONMENT-VALUE
                      END-IF
               WHEN 1 STRING "Arquivo " DELIMITED BY SIZE
                             LB-ARQUIVO DELIMITED BY SPACE
                      "nÆo encontrado." DELIMITED BY SIZE
                        INTO CWSEND-MSG
               WHEN 2 MOVE STATUS-PARAMETER TO E
                      STRING "Erro " E "." DELIMITED BY SIZE
                                INTO CWSEND-MSG
               WHEN 3 MOVE "Parƒmetros inv lidos." TO CWSEND-MSG
           END-EVALUATE
           CALL "CWMSGW" USING "230339"
                         "                                       "

           IF   CWSEND-MSG NOT = SPACES
                MOVE CWSEND-MSG    TO MENSAGEM
                MOVE E (4: 2)      TO FS-ARQUIVO
                PERFORM 020-GRAVAR-LOG THRU 020-99-FIM
                MOVE SPACES        TO CWSEND-SCREENS
                PERFORM 060-SEND THRU 060-99-FIM
                IF CWSQLC = "OK"
                   GOBACK
                END-IF
           END-IF

           STOP RUN.

       040-99-FIM. EXIT.

       060-SEND.

           IF  LB-HELP = SPACES
               IF LB-ARQUIVO NOT = SPACES
                  IF LB-ARQUIVO (1:6) = 'cwlog-'
                     MOVE 1 TO NOLOG
                  END-IF
                  CALL "CWSEND" USING PARAMETROS-CWSEND LB-ARQUIVO
               ELSE
                  CALL "CWSEND" USING PARAMETROS-CWSEND
               END-IF
           ELSE
               IF  CWSEND-SCREENS = SPACES
                   IF  TESTE-LB NOT = "OFF"
                       PERFORM TEST AFTER
                               UNTIL (FS-HELP NOT = "9D")
                                 AND (FS-HELP NOT = "9A")
                                 AND (FS-HELP NOT = "9#")
                              OPEN EXTEND HELP
                       END-PERFORM
                       WRITE HELP-REG FROM CWSEND-MSG
                       CLOSE HELP
                   ELSE
                       DISPLAY 'CWISAM-MSG' UPON ENVIRONMENT-NAME
                       DISPLAY CWSEND-MSG   UPON ENVIRONMENT-VALUE
                   END-IF
                   MOVE 3 TO CWSEND-OPTION
               END-IF
           END-IF
           MOVE SPACES TO LB-ARQUIVO.

       060-99-FIM. EXIT.

       END PROGRAM CWISAM.
