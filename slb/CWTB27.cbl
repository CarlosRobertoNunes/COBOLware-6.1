       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWTB27.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  16/02/1997.
       SECURITY.      *************************************************
                      *                                               *
                      *  Emulador de terminais UNISYS TB-27           *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 UPPER-OPT            PIC X(003)      VALUE SPACES.
              88 UPPER-ON                          VALUE "ON".
              88 UPPER-OFF                         VALUE "OFF".
           05 TB27POSIT            PIC X(006)      VALUE "EBCDIC".
              88 TB27-POSIT-ASCII                  VALUE "ASCII".
              88 TB27-POSIT-EBCDIC                 VALUE "EBCDIC".
           05 TB27INTENSO2         PIC X(003)      VALUE "ON".
           05 SKIPS                PIC 9(003)      VALUE ZEROS.
           05 apagar-ws            PIC 9(001)      VALUE 1.
           05 MANTER               PIC 9(001)      VALUE 0.
              88 MANTER-BUFFER                     VALUE 1.
              88 NAO-MANTER-BUFFER                 VALUE 0.
           05 FORM-MODE            PIC 9(001)      VALUE ZEROS.
              88 FORM-MODE-ON                      VALUE 1.
              88 FORM-MODE-OFF                     VALUE ZEROS.
           05 F12-MODE             PIC X(006)      VALUE SPACES.
              88 XMIT-CURSOR                       VALUE "CURSOR".
              88 XMIT-ALL                          VALUE "ALL".
           05 UP-MODE              PIC X(006)      VALUE SPACES.
              88 UP-MODE-ON                        VALUE "PROT".
              88 UP-MODE-OFF                       VALUE "NOPROT".
           05  WS-CLEAR-FIELD      PIC X(003)      VALUE "NAO".
               88  CLEAR-OFF                       VALUE "NAO".
               88  CLEAR-ON                        VALUE "SIM".

           05 COLX                 PIC 9(002)      VALUE ZEROS.
           05 LINX                 PIC 9(002)      VALUE ZEROS.
           05 WS-ATTRIBUTE         PIC X(001)      VALUE SPACE.
           05 WS-ATTRIBUTE2        PIC X(001)      VALUE SPACE.
           05 WS-BLINK             PIC X(001)      VALUE SPACE.
           05 WS-CONTROL           PIC X(001)      VALUE SPACE.
           05 WS-CONTROL-NUM       REDEFINES       WS-CONTROL
                                   PIC 9(002)      COMP-X.
           05 MASCARA              VALUE X"1F202020202020202020201E".
              10 MASK              PIC X(001)      OCCURS 5000.
           05 REDEFINES MASCARA.
              10 MASKN             PIC 9(002)      OCCURS 5000 COMP-X.
           05 SCREEN-BUFFER        PIC X(2000)     VALUE SPACES.
           05 FRONTEIRA            PIC X(4000)     VALUE SPACES.
           05 ATTRIBUTE-BUFFER     PIC X(2000)     VALUE SPACES.
           05 CONTROL-BUFFER       PIC X(2000)     VALUE SPACES.
           05 IND-MASK             PIC 9(004)      VALUE ZEROS.
           05 TABELA-POSICOES-EBCDIC.
              10 PIC X VALUE X"40".
              10 PIC X VALUE X"4F".
              10 PIC X VALUE X"7F".
              10 PIC X VALUE X"7B".
              10 PIC X VALUE X"5B".
              10 PIC X VALUE X"6C".
              10 PIC X VALUE X"50".
              10 PIC X VALUE X"7D".
              10 PIC X VALUE X"4D".
              10 PIC X VALUE X"5D".
              10 PIC X VALUE X"5C".
              10 PIC X VALUE X"4E".
              10 PIC X VALUE X"6B".
              10 PIC X VALUE X"60".
              10 PIC X VALUE X"4B".
              10 PIC X VALUE X"61".
              10 PIC X VALUE X"F0".
              10 PIC X VALUE X"F1".
              10 PIC X VALUE X"F2".
              10 PIC X VALUE X"F3".
              10 PIC X VALUE X"F4".
              10 PIC X VALUE X"F5".
              10 PIC X VALUE X"F6".
              10 PIC X VALUE X"F7".
              10 PIC X VALUE X"F8".
              10 PIC X VALUE X"F9".
              10 PIC X VALUE X"7A".
              10 PIC X VALUE X"5E".
              10 PIC X VALUE X"4C".
              10 PIC X VALUE X"7E".
              10 PIC X VALUE X"6E".
              10 PIC X VALUE X"6F".
              10 PIC X VALUE X"7C".
              10 PIC X VALUE X"C1".
              10 PIC X VALUE X"C2".
              10 PIC X VALUE X"C3".
              10 PIC X VALUE X"C4".
              10 PIC X VALUE X"C5".
              10 PIC X VALUE X"C6".
              10 PIC X VALUE X"C7".
              10 PIC X VALUE X"C8".
              10 PIC X VALUE X"C9".
              10 PIC X VALUE X"D1".
              10 PIC X VALUE X"D2".
              10 PIC X VALUE X"D3".
              10 PIC X VALUE X"D4".
              10 PIC X VALUE X"D5".
              10 PIC X VALUE X"D6".
              10 PIC X VALUE X"D7".
              10 PIC X VALUE X"D8".
              10 PIC X VALUE X"D9".
              10 PIC X VALUE X"E2".
              10 PIC X VALUE X"E3".
              10 PIC X VALUE X"E4".
              10 PIC X VALUE X"E5".
              10 PIC X VALUE X"E6".
              10 PIC X VALUE X"E7".
              10 PIC X VALUE X"E8".
              10 PIC X VALUE X"E9".
              10 PIC X VALUE X"4A".
              10 PIC X VALUE X"E0".
              10 PIC X VALUE X"5A".
              10 PIC X VALUE X"5F".
              10 PIC X VALUE X"6D".
              10 PIC X VALUE X"79".
              10 PIC X VALUE X"81".
              10 PIC X VALUE X"82".
              10 PIC X VALUE X"83".
              10 PIC X VALUE X"84".
              10 PIC X VALUE X"85".
              10 PIC X VALUE X"86".
              10 PIC X VALUE X"87".
              10 PIC X VALUE X"88".
              10 PIC X VALUE X"89".
              10 PIC X VALUE X"91".
              10 PIC X VALUE X"92".
              10 PIC X VALUE X"93".
              10 PIC X VALUE X"94".
              10 PIC X VALUE X"95".
              10 PIC X VALUE X"96".
              10 PIC X VALUE X"97".
              10 PIC X VALUE X"98".
              10 PIC X VALUE X"99".
              10 PIC X VALUE X"A2".
              10 PIC X VALUE X"A3".
              10 PIC X VALUE X"A4".
              10 PIC X VALUE X"A5".
              10 PIC X VALUE X"A6".
              10 PIC X VALUE X"A7".
              10 PIC X VALUE X"A8".
              10 PIC X VALUE X"A9".
              10 PIC X VALUE X"C0".
              10 PIC X VALUE X"6A".
              10 PIC X VALUE X"D0".
              10 PIC X VALUE X"A1".
              10 PIC X VALUE X"D7".
           05 REDEFINES TABELA-POSICOES-EBCDIC.
              10 EBCDIC-TB27 OCCURS 96 PIC 9(2) COMP-X.
           05 TABELA-POSICOES-ASCII.
              10 PIC X VALUE X"20".
              10 PIC X VALUE X"21".
              10 PIC X VALUE X"22".
              10 PIC X VALUE X"23".
              10 PIC X VALUE X"24".
              10 PIC X VALUE X"25".
              10 PIC X VALUE X"26".
              10 PIC X VALUE X"27".
              10 PIC X VALUE X"28".
              10 PIC X VALUE X"29".
              10 PIC X VALUE X"2A".
              10 PIC X VALUE X"2B".
              10 PIC X VALUE X"2C".
              10 PIC X VALUE X"2D".
              10 PIC X VALUE X"2E".
              10 PIC X VALUE X"2F".
              10 PIC X VALUE X"30".
              10 PIC X VALUE X"31".
              10 PIC X VALUE X"32".
              10 PIC X VALUE X"33".
              10 PIC X VALUE X"34".
              10 PIC X VALUE X"35".
              10 PIC X VALUE X"36".
              10 PIC X VALUE X"37".
              10 PIC X VALUE X"38".
              10 PIC X VALUE X"39".
              10 PIC X VALUE X"3A".
              10 PIC X VALUE X"3B".
              10 PIC X VALUE X"3C".
              10 PIC X VALUE X"3D".
              10 PIC X VALUE X"3E".
              10 PIC X VALUE X"3F".
              10 PIC X VALUE X"40".
              10 PIC X VALUE X"41".
              10 PIC X VALUE X"42".
              10 PIC X VALUE X"43".
              10 PIC X VALUE X"44".
              10 PIC X VALUE X"45".
              10 PIC X VALUE X"46".
              10 PIC X VALUE X"47".
              10 PIC X VALUE X"48".
              10 PIC X VALUE X"49".
              10 PIC X VALUE X"4A".
              10 PIC X VALUE X"4B".
              10 PIC X VALUE X"4C".
              10 PIC X VALUE X"4D".
              10 PIC X VALUE X"4E".
              10 PIC X VALUE X"4F".
              10 PIC X VALUE X"50".
              10 PIC X VALUE X"51".
              10 PIC X VALUE X"52".
              10 PIC X VALUE X"53".
              10 PIC X VALUE X"54".
              10 PIC X VALUE X"55".
              10 PIC X VALUE X"56".
              10 PIC X VALUE X"57".
              10 PIC X VALUE X"58".
              10 PIC X VALUE X"59".
              10 PIC X VALUE X"5A".
              10 PIC X VALUE X"5B".
              10 PIC X VALUE X"5C".
              10 PIC X VALUE X"5D".
              10 PIC X VALUE X"5E".
              10 PIC X VALUE X"5F".
              10 PIC X VALUE X"60".
              10 PIC X VALUE X"61".
              10 PIC X VALUE X"62".
              10 PIC X VALUE X"63".
              10 PIC X VALUE X"64".
              10 PIC X VALUE X"65".
              10 PIC X VALUE X"66".
              10 PIC X VALUE X"67".
              10 PIC X VALUE X"68".
              10 PIC X VALUE X"69".
              10 PIC X VALUE X"6A".
              10 PIC X VALUE X"6B".
              10 PIC X VALUE X"6C".
              10 PIC X VALUE X"6D".
              10 PIC X VALUE X"6E".
              10 PIC X VALUE X"6F".
              10 PIC X VALUE X"70".
              10 PIC X VALUE X"71".
              10 PIC X VALUE X"72".
              10 PIC X VALUE X"73".
              10 PIC X VALUE X"74".
              10 PIC X VALUE X"75".
              10 PIC X VALUE X"76".
              10 PIC X VALUE X"77".
              10 PIC X VALUE X"78".
              10 PIC X VALUE X"79".
              10 PIC X VALUE X"7A".
              10 PIC X VALUE X"7B".
              10 PIC X VALUE X"7C".
              10 PIC X VALUE X"7D".
              10 PIC X VALUE X"7E".
              10 PIC X VALUE X"7F".
           05 REDEFINES TABELA-POSICOES-ASCII.
              10 ASCII-TB27 OCCURS 96 PIC 9(2) COMP-X.
           05 CONVERTE-POSICOES VALUE LOW-VALUES.
              10 POS-TELA OCCURS 255 PIC 9(2) COMP-X.

      *01  AREAS-DE-TRABALHO. COPY G:\cobware\cpy\CWCASE.
       01  AREAS-DE-TRABALHO.
           05 MINUSCULAS             PIC  X(049) VALUE
              "abcdefghijklmnopqrstuvwxyz ‚¡¢£ì…Š•—„‰”ƒˆŒ“–Æä‡".
           05 MAIUSCULAS             PIC  X(049) VALUE
              "ABCDEFGHIJKLMNOPQRSTUVWXYZµÖàéí·ÔëãëŽÓ™š¶Ò×âêÇå€".
           05 ACENTOS-850            PIC  X(058) VALUE
           "µÖàéí ‚¡¢£ì¶Ò×âêƒˆŒ“–·ÔÞãë…Š•—Çå¥Æä¤Ž™š„‰‹”€‡Ó§¦«ó¸©¨¯®".
           05 ACENTOS-437            PIC  X(058) VALUE
           "µÖàéí ‚¡¢£y¶Ò×âêƒˆŒ“–·ÔÞãë…Š•—Ž™¥„”¤Ž™š„‰‹”€‡Ó§¦«ó¸©¨¯®".
      *    "AIOUY ‚¡¢£yAEIOUƒˆŒ“–AEIOU…Š•—Ž™¥„”¤Ž™š„‰‹”€‡E".
           05 ACENTOS-WINDOWS        PIC  X(058) VALUE
           "ÁÉÍÓÚÝáéíóúýÂÊÎÔÛâêîôûÀÈÌÒÙàèìòùÃÕÑãõñÄÖÜäëïöüÇçËºª½¼©®¿»«".
           05 ACENTOS-OFF            PIC  X(058) VALUE
           "AEIOUYaeiouyAEUIUaeuiuAEIOUaeiouAONaonAOUaeiouCcE§¦«ó¸©¨¯®".

           05 ALINHADOR            PIC X(018)      VALUE SPACES.
           05 TERM                 PIC X(010)      VALUE SPACES.
           05 STATION              PIC X(003)      VALUE SPACES.
           05 DOS-ATTR.
              10 PC-VERDE          PIC X(001)      VALUE X"02".
              10 PC-VERMELHO       PIC X(001)      VALUE X"4E".
              10 PC-VERDE-PISCA    PIC X(001)      VALUE X"82".
              10 PC-VERDE-HIGH     PIC X(001)      VALUE X"0A".
              10 PC-VERDE-REVERSO  PIC X(001)      VALUE X"20".
              10 PC-APAGADO        PIC X(001)      VALUE X"00".
           05 WS-UNIX-X.
              10 WS-UNIX           PIC 9(003)      VALUE ZEROS.
           05 VERDE-UNIX           PIC X(2000)     VALUE ALL X"00".
           05 UNIX-ATTR.
              10 UNIX-VERDE        PIC X(001)      VALUE X"05".
              10 UNIX-VERMELHO     PIC X(001)      VALUE X"05".
              10 UNIX-VERDE-PISCA  PIC X(001)      VALUE X"05".
              10 UNIX-VERDE-HIGH   PIC X(001)      VALUE X"05".
              10 UNIX-VERDE-REVERS PIC X(001)      VALUE X"05".
              10 UNIX-APAGADO      PIC X(001)      VALUE X"00".
           05 REDEFINES UNIX-ATTR.
              10 UNIX-VERDE-X      PIC 9(002)      COMP-X.
              10 UNIX-VERMELHO-X   PIC 9(002)      COMP-X.
              10 UNIX-PISCA-X      PIC 9(002)      COMP-X.
              10 UNIX-HIGH-X       PIC 9(002)      COMP-X.
              10 UNIX-REVERSO-X    PIC 9(002)      COMP-X.
              10 UNIX-APAGADO-X    PIC 9(002)      COMP-X.
           05 UNISYS               PIC X(004)      VALUE X"1C1F1D1E".
           05 UNIX                 PIC X(004)      VALUE "[[[]".
           05 DSP                  PIC X(080)      VALUE SPACES.
           05 SCR-POS.
              10 SCR-LIN           PIC 9(002)      VALUE ZEROS COMP-X.
              10 SCR-COL           PIC 9(002)      VALUE ZEROS COMP-X.
           05 SCR-SIZE             PIC 9(004)      VALUE ZEROS COMP-X.
           05 FIM-LINHA            PIC 9(004)      VALUE ZEROS COMP.
           05 IND-DSP              PIC 9(004)      VALUE ZEROS COMP.
           05 IND-BUF              PIC 9(004)      VALUE ZEROS COMP.
           05 IND-VAR              PIC 9(004)      VALUE ZEROS COMP.
           05 IND-CONTROL          PIC 9(004)      VALUE ZEROS COMP.
           05 IND-CONTROL-SAVE     PIC 9(004)      VALUE ZEROS COMP.
           05 IND-ERASE            PIC 9(004)      VALUE ZEROS COMP.
           05 IND-ALIN             PIC 9(004)      VALUE ZEROS COMP.
           05 IND-FIM              PIC 9(004)      VALUE ZEROS COMP.
           05 IND-POSIT            PIC 9(004)      VALUE ZEROS COMP.
           05 IND-ACC              PIC 9(004)      VALUE ZEROS COMP.
           05 IND-TOP              PIC 9(004)      VALUE 1     COMP.
           05 IND-FIRST            PIC 9(004)      VALUE 13    COMP.
           05 LIMITE               PIC S9(004)     VALUE 2000  COMP.
           05 LENGTH-2000          PIC 9(004)      COMP-X VALUE 1920.
           05 TECLA                PIC 9(002)      VALUE ZEROS.
              COPY CWKEYS.
              88 FIM-TEMPO                         VALUE 98.
      *    05 MINUSCULAS           PIC X(026)      VALUE
      *       "abcdefghijklmnopqrstuvwxyz".
      *    05 MAIUSCULAS           PIC X(026)      VALUE
      *      "ABCDEFGHIJKLMNOPQRSTUVWXYZ".

      * Converte EBCDIC para ASCII
      *           ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *           ³ @ðñòóôõö÷øù`ç ³
      *           ³  0123456789-X ³
      *           ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  EBCDIC-CHAR             PIC X(13) VALUE
           X"40F0F1F2F3F4F5F6F7F8F960E7".
       01  ASCII-CHAR              PIC X(13) VALUE
           X"20303132333435363738392D58".

      *  ATTRIBUTOS - IBM-PC

       01  ATTR-PROCEDURE.
           05 VERDE                PIC X(001)      VALUE X"02".
           05 VERMELHO             PIC X(001)      VALUE X"4E".
           05 VERDE-PISCA          PIC X(001)      VALUE X"82".
           05 VERDE-HIGH           PIC X(001)      VALUE X"0A".
           05 VERDE-REVERSO        PIC X(001)      VALUE X"20".
           05 APAGADO              PIC X(001)      VALUE X"00".

       78  VERDE-DOS               VALUE X"02".

      *  ATTRIBUTOS - TB-27

       78 ALFA                     VALUE X"1F".
       78 ALFA-SKIP                VALUE X"1C".
       78 APAGA-TELA               VALUE X"0C".
       78 ERASE-EOS                VALUE X"27D1".
       78 ETX                      VALUE X"03".
       78 FIM-CAMPO                VALUE X"1E".
       78 FIM-TRAN                 VALUE X"11".
       78 FORM                     VALUE X"12".
       78 FORM-OFF                 VALUE X"27E7".
       78 FORM-ON                  VALUE X"27E6".
       78 INTENSO                  VALUE X"1A".
       01 INTENSO2      PIC X(001) VALUE X"3F".
       78 INVISIVEL                VALUE X"19".
       78 KEXX-INTENSO             VALUE X"9B".
       78 KEXX-MENSAGEM            VALUE X"FB".
       78 MENSAGEM                 VALUE X"17".
       78 NORMAL                   VALUE X"26".
       78 NOVA-LINHA               VALUE X"0D".
       78 NUMERICO                 VALUE X"1D".
       78 PISCA                    VALUE X"18".
       78 POS-EBCDIC               VALUE X"7DD7".
       78 POS-ASCII                VALUE X"277F".
       78 POS-PC                   VALUE X"1B22".
       78 PROTEGIDO                VALUE X"01".
       78 REVERSO                  VALUE X"0E".
       78 SUBLINHADO               VALUE X"0F".
       78 TAB-UNISYS               VALUE X"05".
       COPY CWUNIX.
       COPY CWSETF.
       77  CWUSER PIC X(6) VALUE "CWUSER".
       77  TB27PC PIC X(3) VALUE SPACES.
       77  FIELD  PIC 9(4) VALUE 0.
       77  F      PIC 9(4) VALUE 0.
       77  P      PIC 9(4) VALUE 0.
       77  FIELD-TYPE PIC X VALUE SPACE.
       01  SCREEN-CONTROL.
           05 OCCURS 2000.
              10 POS PIC 9(4).
       01  FUNCAO.
           05 COMANDO  PIC X(001).
              88 FUN-DISPLAY     VALUE "D" "d".
              88 FUN-ACCEPT      VALUE "A" "a".
              88 FUN-POSIT       VALUE "P" "p".
              88 FUN-CLEAR       VALUE X"00".
           05 PROGRAMA PIC X(008) VALUE 'CWTB27'.
           05 DP       PIC X(001) VALUE '.'.
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
                 88 NUMERICO-LK             VALUE "N" "/".
                 88 NUMERICO-BARRA-LK       VALUE "/".
              10 PIC-LK         PIC X(080).
              10 DATANAME-LK    PIC X(030).
              10 DATA-LK        PIC X(080).
              10 CTRL-LK        PIC X(080).
       01  TAMANHO-MATRIZ PIC 9(008) COMP-X.
       01  TIMEOUT-SEGUNDOS.
           05 TECLA-LK      PIC 9(003).
           05 FILLER        PIC X(015).
       01  TIMEOUT-TAMANHO  PIC 9(008) COMP-X.

       LINKAGE SECTION.

       COPY TB27.

       PROCEDURE DIVISION USING PARAMETROS-TB27.

       000-INICIO.

           DISPLAY "POPULATION" UPON ENVIRONMENT-NAME
           DISPLAY "777" UPON ENVIRONMENT-VALUE
           IF   TB27-SEND
           AND  TB27-BUFFER = SPACES
           AND  TB27-LENGTH = 0
                DISPLAY (1, 1) ERASE
                GOBACK
           END-IF
           ON 1
              CALL "CWUNIX" USING PARAMETROS-CWUNIX
              IF  CWUNIX-GUI
                  MOVE LOW-VALUES    TO PC-VERDE
              END-IF
              DISPLAY "TB27POSIT" UPON ENVIRONMENT-NAME
              ACCEPT  TB27POSIT   FROM ENVIRONMENT-VALUE
              INSPECT TB27POSIT CONVERTING MINUSCULAS TO MAIUSCULAS
              IF  TB27POSIT NOT = "ASCII"
                  MOVE "EBCDIC" TO TB27POSIT
              END-IF
              DISPLAY "TB27UPPER" UPON ENVIRONMENT-NAME
              ACCEPT  UPPER-OPT  FROM ENVIRONMENT-VALUE
              INSPECT UPPER-OPT  CONVERTING MINUSCULAS TO MAIUSCULAS
              IF   NOT UPPER-OFF
                   SET UPPER-ON TO TRUE
              END-IF
              DISPLAY "TERM"   UPON ENVIRONMENT-NAME
              ACCEPT  TERM     FROM ENVIRONMENT-VALUE
              IF  TERM = SPACES
                  MOVE DOS-ATTR  TO ATTR-PROCEDURE
              ELSE
                  DISPLAY "TB27NORMAL" UPON ENVIRONMENT-NAME
                  ACCEPT  WS-UNIX-X    FROM ENVIRONMENT-VALUE
                  IF  WS-UNIX-X NOT = SPACE
                      MOVE WS-UNIX TO UNIX-VERDE-X
                      PERFORM VARYING IND-BUF FROM 1 BY 1
                                    UNTIL IND-BUF > 2000
                              MOVE UNIX-VERDE
                                TO VERDE-UNIX (IND-BUF: 1)
                      END-PERFORM
                      MOVE 0 TO IND-BUF
                  END-IF
                  DISPLAY "TB27RED" UPON ENVIRONMENT-NAME
                  ACCEPT  WS-UNIX-X   FROM ENVIRONMENT-VALUE
                  IF  WS-UNIX-X NOT = SPACE
                      MOVE WS-UNIX TO UNIX-VERMELHO-X
                  END-IF
                  DISPLAY "TB27BLINK" UPON ENVIRONMENT-NAME
                  ACCEPT  WS-UNIX-X   FROM ENVIRONMENT-VALUE
                  IF  WS-UNIX-X NOT = SPACE
                      MOVE WS-UNIX TO UNIX-PISCA-X
                  END-IF
                  DISPLAY "TB27HIGH" UPON ENVIRONMENT-NAME
                  ACCEPT  WS-UNIX-X  FROM ENVIRONMENT-VALUE
                  IF  WS-UNIX-X NOT = SPACE
                      MOVE WS-UNIX TO UNIX-HIGH-X
                  END-IF
                  DISPLAY "TB27REVERSE" UPON ENVIRONMENT-NAME
                  ACCEPT  WS-UNIX-X     FROM ENVIRONMENT-VALUE
                  IF  WS-UNIX-X NOT = SPACE
                      MOVE WS-UNIX TO UNIX-REVERSO-X
                  END-IF
                  DISPLAY "TB27NULL" UPON ENVIRONMENT-NAME
                  ACCEPT  WS-UNIX-X  FROM ENVIRONMENT-VALUE
                  IF  WS-UNIX-X NOT = SPACE
                      MOVE WS-UNIX TO UNIX-APAGADO-X
                  END-IF
                  MOVE UNIX-ATTR TO ATTR-PROCEDURE
              END-IF
              PERFORM 010-PREPARA-ATTRIBUTOS THRU 010-99-FIM
              DISPLAY "STATION"  UPON ENVIRONMENT-NAME
              ACCEPT  STATION    FROM ENVIRONMENT-VALUE
              IF   STATION =  "   "
                   MOVE "001" TO STATION
                   DISPLAY STATION UPON ENVIRONMENT-VALUE
              END-IF
              DISPLAY "TB27F12" UPON ENVIRONMENT-NAME
              ACCEPT  F12-MODE  FROM ENVIRONMENT-VALUE
              IF   F12-MODE = SPACES
                   MOVE "CURSOR" TO F12-MODE
              END-IF
              DISPLAY "TB27UP" UPON ENVIRONMENT-NAME
              ACCEPT  UP-MODE  FROM ENVIRONMENT-VALUE
              INSPECT UP-MODE CONVERTING MINUSCULAS TO MAIUSCULAS
              PERFORM VARYING IND-POSIT FROM 1 BY 1
                        UNTIL IND-POSIT > 96
                      IF   TB27-POSIT-ASCII
                           MOVE ASCII-TB27  (IND-POSIT) TO IND-BUF
                      ELSE
                           MOVE EBCDIC-TB27 (IND-POSIT) TO IND-BUF
                      END-IF
                      MOVE IND-POSIT TO POS-TELA (IND-BUF)
              END-PERFORM
              DISPLAY "TB27INTENSO2" UPON ENVIRONMENT-NAME
              ACCEPT  TB27INTENSO2  FROM ENVIRONMENT-VALUE
              INSPECT TB27INTENSO2 CONVERTING MINUSCULAS TO MAIUSCULAS
              IF   TB27INTENSO2 = "OFF"
                   MOVE INTENSO TO INTENSO2
              END-IF
              DISPLAY "TB27PC" UPON ENVIRONMENT-NAME
              ACCEPT  TB27PC  FROM ENVIRONMENT-VALUE
              INSPECT TB27PC CONVERTING MINUSCULAS TO MAIUSCULAS.

           SET NAO-MANTER-BUFFER TO TRUE
           IF   TB27-SEND
                IF   TB27-SEND-FIXED
                     SET MANTER-BUFFER TO TRUE
                END-IF
                SET     FORM-MODE-OFF            TO TRUE
                MOVE    TB27-BUFFER              TO MASCARA
                PERFORM 010-PREPARA-ATTRIBUTOS THRU 010-99-FIM
           END-IF

           MOVE 2000 TO LIMITE

           IF   TB27-RECEIVE
                EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                          LINE 25 COLUMN 02 WIDTH 5
                          CAPTION " ~Xmt "
                          KEY F12
                          TAB-OFF
                END-EXEC
                PERFORM 030-RECEIVE   THRU 030-99-FIM
                EXEC COBOLware Object DROP END-EXEC
                PERFORM 060-RETURN    THRU 060-99-FIM
           END-IF

           MOVE STATION     TO TB27-LASTSUBFILE

           IF   FIM-TEMPO
                MOVE 1           TO TB27-EVENT
           ELSE
                MOVE 3           TO TB27-EVENT
           END-IF

           DISPLAY "EVENT"    UPON ENVIRONMENT-NAME
           DISPLAY TB27-EVENT UPON ENVIRONMENT-VALUE.

       000-FIM. GOBACK.

       010-PREPARA-ATTRIBUTOS.

           MOVE VERDE  TO WS-ATTRIBUTE
           IF   NAO-MANTER-BUFFER
                MOVE SPACES TO SCREEN-BUFFER
                               CONTROL-BUFFER
           END-IF
           MOVE SPACES TO FRONTEIRA
                          WS-CONTROL

           IF   TERM = SPACES
                IF  CWUNIX-GUI
                    MOVE LOW-VALUES    TO ATTRIBUTE-BUFFER
                ELSE
                    MOVE ALL VERDE-DOS TO ATTRIBUTE-BUFFER
                END-IF
           ELSE
                MOVE VERDE-UNIX TO ATTRIBUTE-BUFFER
           END-IF

           PERFORM VARYING IND-FIM
                      FROM TB27-LENGTH
                        BY -1
                     UNTIL MASCARA (IND-FIM: 1) NOT = SPACE
                   CONTINUE
           END-PERFORM

           MOVE 0 TO SKIPS
           IF   MASCARA (IND-FIM: 1) = ETX
                COMPUTE IND-BUF = IND-FIM - 1
                PERFORM UNTIL MASCARA (IND-BUF: 1) NOT = TAB-UNISYS
                        ADD 1 TO SKIPS
                        SUBTRACT 1 FROM IND-BUF
                END-PERFORM
           END-IF

           IF   IND-FIM < TB27-LENGTH
                ADD  1 TO IND-FIM
           END-IF

           MOVE 0      TO FIM-LINHA
                          IND-MASK
           PERFORM 020-PROCESSA-CARACTER THRU 020-99-FIM
              VARYING IND-BUF
                      FROM 1
                        BY 1
                     UNTIL IND-BUF  >= IND-FIM
                        OR IND-BUF  = 5000
                        OR IND-MASK > 2000
                        OR FRONTEIRA (IND-MASK: 1) = ETX OR FIM-TRAN

           PERFORM VARYING IND-FIM
                      FROM 2000
                        BY -1
                     UNTIL CONTROL-BUFFER (IND-FIM: 1) NOT = SPACE
                   CONTINUE
           END-PERFORM

           IF   IND-FIM < 2000
                ADD  1 TO IND-FIM
           END-IF

           CALL "CBL_WRITE_SCR_CHATTRS" USING X"0000"
                                              SCREEN-BUFFER
                                              ATTRIBUTE-BUFFER
                                              LENGTH-2000.

       010-99-FIM. EXIT.

       020-PROCESSA-CARACTER.

           ADD 1 TO FIM-LINHA IND-MASK
           EVALUATE MASCARA (IND-BUF: 1)
               WHEN  "û"
                     MOVE MENSAGEM TO MASCARA (IND-BUF: 1)
               WHEN  "ú"
                     MOVE INTENSO  TO MASCARA (IND-BUF: 1)
           END-EVALUATE
           IF  MASCARA (IND-BUF: 3) = X"3C1103"
               ADD 2 TO IND-BUF
               GO TO 020-99-FIM
           END-IF
           IF  MASCARA (IND-BUF: 2) = X"27D8"
               ADD 1 TO IND-BUF
               GO TO 020-99-FIM
           END-IF
           IF  MASCARA (IND-BUF: 2) = X"27D6"
               ADD 1 TO IND-BUF
               GO TO 020-99-FIM
           END-IF
           IF  MASCARA (IND-BUF: 1) = TAB-UNISYS
               GO TO 020-99-FIM
           END-IF

           EVALUATE TRUE
               WHEN MASCARA (IND-BUF: 1) = FORM
                AND FORM-MODE-OFF
                    SET FORM-MODE-ON TO TRUE
                    GO TO 020-99-FIM
               WHEN MASCARA (IND-BUF: 1) = FORM
                AND FORM-MODE-ON
                    SET FORM-MODE-OFF TO TRUE
                    GO TO 020-99-FIM
               WHEN (MASCARA (IND-BUF: 2) = FORM-ON)
                AND FORM-MODE-OFF
                    SET FORM-MODE-ON TO TRUE
                    ADD 1 TO IND-BUF
                    GO TO 020-99-FIM
               WHEN (MASCARA (IND-BUF: 2) = FORM-OFF)
                AND FORM-MODE-ON
                    SET FORM-MODE-OFF TO TRUE
                    ADD 1 TO IND-BUF
                    GO TO 020-99-FIM
           END-EVALUATE
           IF  MASCARA (IND-BUF: 1) = NOVA-LINHA
               MOVE " "   TO WS-CONTROL
               COMPUTE IND-MASK = (IND-MASK + 80 - FIM-LINHA)
               MOVE  0    TO FIM-LINHA
               GO TO 020-99-FIM
           END-IF
           IF  MASCARA (IND-BUF: 1) = APAGA-TELA
               MOVE VERDE  TO WS-ATTRIBUTE
               MOVE SPACES TO SCREEN-BUFFER FRONTEIRA
                              CONTROL-BUFFER
                              WS-CONTROL
               IF   TERM = SPACES
                    IF  CWUNIX-GUI
                        MOVE LOW-VALUES    TO ATTRIBUTE-BUFFER
                    ELSE
                        MOVE ALL VERDE-DOS TO ATTRIBUTE-BUFFER
                    END-IF
               ELSE
                    MOVE VERDE-UNIX TO ATTRIBUTE-BUFFER
               END-IF
               MOVE 0      TO FIM-LINHA IND-MASK
               GO TO 020-99-FIM
           END-IF
           IF  MASCARA (IND-BUF: 2) = ERASE-EOS
               ADD 1 TO IND-BUF
               ADD 1 TO IND-MASK
               IF  FORM-MODE-OFF
                   MOVE VERDE  TO WS-ATTRIBUTE
                   MOVE SPACES TO SCREEN-BUFFER       (IND-MASK: )
                                  CONTROL-BUFFER      (IND-MASK: )
                                  WS-CONTROL
                   IF   TERM = SPACES
                        IF  CWUNIX-GUI
                            MOVE LOW-VALUES
                              TO ATTRIBUTE-BUFFER (IND-MASK: )
                        ELSE
                            MOVE ALL VERDE-DOS
                              TO ATTRIBUTE-BUFFER (IND-MASK: )
                        END-IF
                   ELSE
                        MOVE VERDE-UNIX
                          TO ATTRIBUTE-BUFFER (IND-MASK: )
                   END-IF
               ELSE
                   MOVE    1               TO IND-CONTROL
                   PERFORM 331-ERASE-EOS THRU 331-99-FIM
               END-IF
               GO TO 020-99-FIM
           END-IF
           IF  MASCARA (IND-BUF: 2) = POS-EBCDIC OR POS-ASCII OR POS-PC
               PERFORM UNTIL (MASCARA (IND-BUF: 2) NOT = POS-EBCDIC)
                         AND (MASCARA (IND-BUF: 2) NOT = POS-ASCII)
                         AND (MASCARA (IND-BUF: 2) NOT = POS-PC)
                       ADD  2                    TO IND-BUF
                       MOVE MASKN (IND-BUF)      TO IND-POSIT
                       MOVE POS-TELA (IND-POSIT) TO COLX
                       ADD  1                    TO IND-BUF
                       MOVE MASKN (IND-BUF)      TO IND-POSIT
                       MOVE POS-TELA (IND-POSIT) TO LINX
                       COMPUTE IND-MASK = (LINX * 80) - 80 + COLX
                       MOVE COLX                 TO FIM-LINHA
                       ADD  1                    TO IND-BUF
               END-PERFORM
           END-IF
           IF  FIM-LINHA = 81
               MOVE 1 TO FIM-LINHA
               MOVE VERDE TO WS-ATTRIBUTE
               MOVE SPACE TO WS-CONTROL
           END-IF
           IF   MASK (IND-BUF) = KEXX-INTENSO
                MOVE INTENSO TO MASK (IND-BUF)
           END-IF
           IF   MASK (IND-BUF) = KEXX-MENSAGEM
                MOVE MENSAGEM TO MASK (IND-BUF)
           END-IF

           IF  MASCARA (IND-BUF: 1) = ETX
               MOVE SPACE TO SCREEN-BUFFER (IND-MASK: 1)
               IF   MASK (IND-BUF) = ETX OR FIM-TRAN
                    MOVE MASK (IND-BUF) TO FRONTEIRA (IND-MASK: 1)
               END-IF
               GO TO 020-99-FIM
           END-IF

           MOVE 0 TO APAGAR-WS
           EVALUATE TRUE
               WHEN MASK (IND-BUF) = ALFA
                                  OR NUMERICO
                                  OR ALFA-SKIP
                                  OR PROTEGIDO
                    MOVE MASK (IND-BUF) TO WS-CONTROL
      *             MOVE VERDE          TO WS-ATTRIBUTE
               WHEN MASK (IND-BUF) = SUBLINHADO
                    MOVE "S" TO WS-BLINK
                    GO TO 020-99-FIM
               WHEN MASK (IND-BUF) = INTENSO
                                  OR INTENSO2
                                  OR REVERSO
                    MOVE VERDE-HIGH TO WS-ATTRIBUTE
                    IF  MASK (IND-BUF) = REVERSO
                        MOVE " "   TO WS-CONTROL
                    END-IF
                    GO TO 020-99-FIM

               WHEN MASK (IND-BUF) = INVISIVEL

                    MOVE APAGADO TO WS-ATTRIBUTE2
                    COMPUTE IND-ERASE = IND-BUF - 1
                    IF  MASK(IND-ERASE) NOT = ALFA
                    AND ALFA-SKIP
                    AND NUMERICO
                        GO TO 020-99-FIM
                    END-IF

               WHEN MASK (IND-BUF) = NORMAL
                    MOVE VERDE TO WS-ATTRIBUTE
                    GO TO 020-99-FIM
               WHEN MASK (IND-BUF) = FIM-CAMPO
                                  OR MENSAGEM
                                  OR NOVA-LINHA
                    MOVE " "   TO WS-CONTROL
                    move 1 to apagar-ws
                    IF   WS-ATTRIBUTE = APAGADO
                         MOVE VERDE TO WS-ATTRIBUTE
                    END-IF
           END-EVALUATE
           MOVE MASK (IND-BUF) TO SCREEN-BUFFER (IND-MASK: 1)
           IF   WS-CONTROL NOT = SPACE
           AND  WS-CONTROL NOT = MASK (IND-BUF)
                MOVE WS-CONTROL TO CONTROL-BUFFER (IND-MASK: 1)
                IF   WS-BLINK = "1"
                     ADD 128 TO WS-CONTROL-NUM
                     MOVE SPACE TO WS-BLINK
                END-IF
           END-IF
           MOVE WS-ATTRIBUTE TO ATTRIBUTE-BUFFER (IND-MASK: 1)
           IF   MASK (IND-BUF) = INTENSO
                              OR INTENSO2
                              OR SUBLINHADO
                              OR MENSAGEM
                              OR PISCA
                              OR INVISIVEL
                              OR REVERSO
                              OR NORMAL
                              OR ETX
                              OR FIM-TRAN
                MOVE SPACE TO SCREEN-BUFFER (IND-MASK: 1)
                IF   MASK (IND-BUF) = ETX OR FIM-TRAN
                     MOVE MASK (IND-BUF) TO FRONTEIRA (IND-MASK: 1)
                END-IF
           END-IF
           INSPECT SCREEN-BUFFER (IND-MASK: 1)
                  CONVERTING UNISYS
                          TO UNIX
           INSPECT SCREEN-BUFFER (IND-MASK: 1)
                        CONVERTING EBCDIC-CHAR TO ASCII-CHAR

           IF   WS-ATTRIBUTE2 NOT = SPACE
                MOVE WS-ATTRIBUTE2 TO WS-ATTRIBUTE
                     ATTRIBUTE-BUFFER (IND-MASK: 1)
                MOVE SPACE TO WS-ATTRIBUTE2
           END-IF

           IF   APAGAR-WS = 1
                MOVE VERDE TO WS-ATTRIBUTE
           END-IF.

       020-99-FIM. EXIT.

       030-RECEIVE.

           IF CONTROL-BUFFER = SPACES
              IF  TB27-TIMEOUT = 0
                  ACCEPT SCREEN-BUFFER (1: 12)
                         LINE 01 COLUMN 02
              ELSE
                  ACCEPT SCREEN-BUFFER (1: 12)
                         LINE 01 COLUMN 02
                         TIME-OUT AFTER TB27-TIMEOUT
              END-IF
              IF   UPPER-ON
                   INSPECT SCREEN-BUFFER (1: 12)
                           CONVERTING MINUSCULAS TO MAIUSCULAS
              END-IF
              MOVE SCREEN-BUFFER (1: 12) TO TB27-BUFFER
              PERFORM VARYING TB27-LENGTH FROM LENGTH TB27-BUFFER
                                       BY -1
                                    UNTIL TB27-LENGTH = 0
                                       OR TB27-BUFFER (TB27-LENGTH:1)
                                          <> SPACE
                       CONTINUE
              END-PERFORM
              GOBACK
              GO TO 030-99-FIM
           END-IF

           IF TB27PC NOT = 'OFF'
              EXEC COBOLware GetSystem Program;PROGRAMA END-EXEC
              MOVE 0 TO TAMANHO-MATRIZ FIELD
              MOVE 1 TO F
              PERFORM VARYING IND-CONTROL  FROM 1 BY 1
                              UNTIL IND-CONTROL > IND-FIM
                      IF CONTROL-BUFFER (IND-CONTROL: 1) = ALFA
                      OR ALFA-SKIP
                      OR NUMERICO
                         ADD  1                     TO FIELD
                         INITIALIZE MATRIZ-E (FIELD)
                         MOVE "U"  TO MODE-LK (FIELD)
                         MOVE "fb" TO FB-LK (FIELD)
                         MOVE IND-CONTROL TO P
                         ADD LENGTH OF MATRIZ-E(1)  TO TAMANHO-MATRIZ
                         MOVE CONTROL-BUFFER (IND-CONTROL: 1)
                           TO FIELD-TYPE
                         COMPUTE SCR-LIN = (IND-CONTROL / 80) + 1
                         COMPUTE SCR-COL = IND-CONTROL -
                                           ((SCR-LIN - 1) * 80)
                         MOVE SCR-LIN  TO LIN-LK(FIELD)
                         MOVE SCR-COL  TO COL-LK(FIELD)
                         MOVE 0        TO SCR-SIZE
                         PERFORM TEST AFTER
                                 UNTIL CONTROL-BUFFER (IND-CONTROL: 1)
                                       NOT = FIELD-TYPE
                                 ADD 1 TO SCR-SIZE
                                          IND-CONTROL
                         END-PERFORM
                         MOVE  SCR-SIZE TO LEN-LK(FIELD)
                                          LENF-LK(FIELD)
                         IF  FIELD-TYPE = NUMERICO
                             MOVE ALL "Z" TO PIC-LK(FIELD)(1:SCR-SIZE)
                         ELSE
                             MOVE ALL "X" TO PIC-LK(FIELD)(1:SCR-SIZE)
                         END-IF
                         STRING PROGRAMA DELIMITED BY SPACE
                                '-'      DELIMITED BY SIZE
                                POS-LK(FIELD)   DELIMITED BY SIZE
                           INTO DATANAME-LK(FIELD)
                         MOVE SCREEN-BUFFER(P: SCR-SIZE)
                           TO DATA-LK(FIELD)
                         MOVE P        TO POS(FIELD)
                         ADD  SCR-SIZE TO F
                         IF   UPPER-ON
                              MOVE 'U' TO UPPLOW-LK(FIELD)
                         END-IF
                         IF  FIELD-TYPE = ALFA-SKIP
                             MOVE Dataname-LK(FIELD) TO CWSETF-FIELD
                             SET CWSETF-PROTECT TO TRUE
                             CALL "CWSETF" USING PARAMETROS-CWSETF
                             INITIALIZE PARAMETROS-CWSETF
                             MOVE LOW-VALUE TO CWSETF-OPTION
                         END-IF
                      END-IF
              END-PERFORM
              SET FUN-ACCEPT TO TRUE
              MOVE SPACES TO TB27-BUFFER
              IF SKIPS NOT = 0
              AND SKIPS < FIELD
                 ADD 1 TO SKIPS
                 MOVE Dataname-LK(SKIPS) TO CWSETF-FIELD
                 CALL "CWSETF" USING PARAMETROS-CWSETF
                 INITIALIZE PARAMETROS-CWSETF
                 MOVE LOW-VALUE TO CWSETF-OPTION
                 SUBTRACT 1 FROM SKIPS
              END-IF
              IF  TB27-TIMEOUT = 0
                  CALL CWUSER USING FUNCAO MATRIZ TAMANHO-MATRIZ
              ELSE
                  MOVE TB27-TIMEOUT TO TIMEOUT-SEGUNDOS
                  MOVE 3            TO TIMEOUT-TAMANHO
                  CALL CWUSER USING FUNCAO MATRIZ TAMANHO-MATRIZ
                                    TIMEOUT-SEGUNDOS
                                    TIMEOUT-TAMANHO
              END-IF
              PERFORM VARYING F FROM 1 BY 1 UNTIL F > FIELD
                      MOVE POS(FIELD)     TO P
                      MOVE LEN-LK(FIELD)  TO SCR-SIZE
                      MOVE DATA-LK(FIELD) TO SCREEN-BUFFER(P: SCR-SIZE)
              END-PERFORM
              ACCEPT TECLA FROM ESCAPE KEY
              GO TO 030-TEST-KEY
           END-IF
           MOVE 0 TO IND-TOP

           PERFORM VARYING IND-FIRST
                      FROM 1
                      BY   1
                     UNTIL CONTROL-BUFFER (IND-FIRST: 1) NOT = SPACE
                       AND CONTROL-BUFFER (IND-FIRST: 1) NOT = ALFA-SKIP
                   IF   IND-TOP = 0
                   AND  CONTROL-BUFFER (IND-FIRST: 1) NOT = SPACE
                        MOVE IND-FIRST TO IND-TOP
                   END-IF
           END-PERFORM

           MOVE    IND-FIRST   TO IND-CONTROL
           PERFORM 040-ENTER THRU 040-99-FIM SKIPS TIMES
           MOVE    0           TO SKIPS

           PERFORM TEST AFTER UNTIL ESC OR F12 OR F2 OR FIM-TEMPO
                   COMPUTE SCR-LIN = (IND-CONTROL / 80) + 1
                   COMPUTE SCR-COL = IND-CONTROL -
                                     ((SCR-LIN - 1) * 80)
                   MOVE 0 TO SCR-SIZE
                   PERFORM VARYING IND-ACC
                              FROM IND-CONTROL
                              BY   1
                              UNTIL CONTROL-BUFFER (IND-ACC: 1) = SPACE
                           ADD 1 TO SCR-SIZE
                   END-PERFORM
                   COMPUTE LIMITE = IND-CONTROL + SCR-SIZE
                   EVALUATE TRUE
                        WHEN ATTRIBUTE-BUFFER (IND-CONTROL: 1) = APAGADO
                         IF  TB27-TIMEOUT = 0
                             ACCEPT SCREEN-BUFFER(IND-CONTROL: SCR-SIZE)
                                     LINE   SCR-LIN
                                     COLUMN SCR-COL
                                      WITH UPDATE AUTO-SKIP SECURE
                                           PROMPT
                                      BACKGROUND-COLOR 0
                                      FOREGROUND-COLOR 2
                             ELSE
                             ACCEPT SCREEN-BUFFER(IND-CONTROL: SCR-SIZE)
                                     LINE   SCR-LIN
                                     COLUMN SCR-COL
                                      WITH UPDATE AUTO-SKIP SECURE
                                           PROMPT
                                      BACKGROUND-COLOR 0
                                      FOREGROUND-COLOR 2
                                      TIME-OUT AFTER TB27-TIMEOUT
                         END-IF
                        WHEN OTHER
                             IF  CONTROL-BUFFER (IND-CONTROL: 1) =
                                                              NUMERICO
                                 PERFORM 034-SHIFT-NUMERICO
                                    THRU 034-99-FIM
                                 MOVE SCREEN-BUFFER
                                      (IND-CONTROL: SCR-SIZE)
                                   TO ALINHADOR
                                 DISPLAY ALINHADOR
                                         LINE   SCR-LIN
                                         COLUMN SCR-COL
                                         WITH SIZE SCR-SIZE
                             END-IF
                         IF  TB27-TIMEOUT = 0
                             ACCEPT SCREEN-BUFFER
                                    (IND-CONTROL: SCR-SIZE)
                                     LINE   SCR-LIN
                                     COLUMN SCR-COL
                                      WITH UPDATE AUTO-SKIP
                                          REVERSE-VIDEO PROMPT
                         ELSE
                             ACCEPT SCREEN-BUFFER
                                    (IND-CONTROL: SCR-SIZE)
                                     LINE   SCR-LIN
                                     COLUMN SCR-COL
                                      WITH UPDATE AUTO-SKIP
                                          REVERSE-VIDEO PROMPT
                                          TIME-OUT AFTER TB27-TIMEOUT
                         END-IF
                            IF  CONTROL-BUFFER (IND-CONTROL: 1) =
                                                                NUMERICO
                                PERFORM 035-ALINHA-NUMERICO
                                   THRU 035-99-FIM
                            END-IF
                   END-EVALUATE
                   EVALUATE IND-CONTROL
                       WHEN 0         SET F12        TO TRUE
                       WHEN OTHER     ACCEPT TECLA FROM ESCAPE KEY
                   END-EVALUATE
                   MOVE    SCREEN-BUFFER (IND-CONTROL: SCR-SIZE) TO DSP
                   IF   UPPER-ON
                        INSPECT DSP (1: SCR-SIZE)
                                CONVERTING MINUSCULAS TO MAIUSCULAS
                   END-IF
                   MOVE    DSP TO SCREEN-BUFFER (IND-CONTROL: SCR-SIZE)
                   IF   ATTRIBUTE-BUFFER (IND-CONTROL: 1) NOT = APAGADO
      *>                IF  CONTROL-BUFFER (IND-CONTROL: 1) = NUMERICO
      *>                    PERFORM VARYING IND-DSP
      *>                            FROM 1 BY 1
      *>                            UNTIL IND-DSP > SCR-SIZE
      *>                               OR DSP (IND-DSP: 1) NOT = "0"
      *>                            MOVE SPACE TO DSP (IND-DSP: 1)
      *>                    END-PERFORM
      *>                END-IF
                        SUBTRACT 1 FROM SCR-LIN SCR-COL
                        CALL "CBL_WRITE_SCR_CHATTRS"
                        USING SCR-POS
                              DSP
                              ATTRIBUTE-BUFFER (IND-CONTROL: SCR-SIZE)
                              SCR-SIZE
                        ADD 1 TO SCR-LIN SCR-COL
                   END-IF
                   EVALUATE TRUE
                       WHEN ESC
                            MOVE IND-FIRST TO IND-CONTROL
                       WHEN F7
                            PERFORM 335-ERASE-FIELD THRU 335-99-FIM
                       WHEN F8
                            PERFORM 331-ERASE-EOS THRU 331-99-FIM
                       WHEN F9
                            MOVE IND-FIRST TO IND-CONTROL
                            PERFORM 040-ENTER THRU 040-99-FIM
                            PERFORM 050-UP    THRU 050-99-FIM
                       WHEN F10
                            MOVE IND-FIM TO IND-CONTROL
                            PERFORM 050-UP    THRU 050-99-FIM
                       WHEN F11
                            PERFORM 070-APAGA-CAMPOS THRU 070-99-FIM
                       WHEN ENTER-KEY
                            PERFORM 040-ENTER THRU 040-99-FIM
                       WHEN CURSOR-UP
                            PERFORM 050-UP    THRU 050-99-FIM
                       WHEN PAGE-DOWN
                            ADD 80 TO IND-CONTROL
                            IF   IND-CONTROL > IND-FIM
                                 MOVE 1 TO IND-CONTROL
                            END-IF
                            IF   CONTROL-BUFFER (IND-CONTROL: 1) = SPACE
                                                   OR PISCA
                                                   OR PROTEGIDO
                                 PERFORM 040-ENTER THRU 040-99-FIM
                            ELSE
                                 PERFORM 050-UP    THRU 050-99-FIM
                                 PERFORM 040-ENTER THRU 040-99-FIM
                            END-IF
                       WHEN PAGE-UP
                            SUBTRACT 80 FROM IND-CONTROL
                            IF   IND-CONTROL < 1
                                 MOVE IND-FIM TO IND-CONTROL
                            END-IF
                            IF   CONTROL-BUFFER (IND-CONTROL: 1) = SPACE
                                                   OR PISCA
                                                   OR PROTEGIDO
                                 PERFORM 050-UP    THRU 050-99-FIM
                            ELSE
                                 PERFORM 040-ENTER THRU 040-99-FIM
                                 PERFORM 050-UP    THRU 050-99-FIM
                            END-IF
                   END-EVALUATE
           END-PERFORM.

       030-TEST-KEY.

           IF   ESC
                MOVE "FIM" TO TB27-BUFFER (5: 3)
                GOBACK
           END-IF.

       030-99-FIM. EXIT.

       331-ERASE-EOS.

           PERFORM VARYING IND-ERASE FROM IND-CONTROL
                        BY 1
                       UNTIL IND-ERASE > IND-FIM
                    IF   CONTROL-BUFFER (IND-ERASE: 1) = NUMERICO
                    OR   ALFA
                         MOVE SPACE TO SCREEN-BUFFER
                                       (IND-ERASE: 1)
                    END-IF
           END-PERFORM

           CALL "CBL_WRITE_SCR_CHATTRS" USING X"0000"
                                        SCREEN-BUFFER
                                        ATTRIBUTE-BUFFER
                                        LENGTH-2000.

       331-99-FIM. EXIT.

       335-ERASE-FIELD.

           PERFORM VARYING IND-ERASE FROM IND-CONTROL BY 1
                   UNTIL  CONTROL-BUFFER (IND-ERASE: 1) = SPACES
                          MOVE SPACE TO SCREEN-BUFFER
                                       (IND-ERASE: 1)
           END-PERFORM

           CALL "CBL_WRITE_SCR_CHATTRS" USING X"0000"
                                        SCREEN-BUFFER
                                        ATTRIBUTE-BUFFER
                                        LENGTH-2000.

       335-99-FIM. EXIT.

       034-SHIFT-NUMERICO.

           MOVE IND-CONTROL TO IND-POSIT
           MOVE SCREEN-BUFFER (IND-CONTROL: SCR-SIZE) TO ALINHADOR
           MOVE SPACES TO SCREEN-BUFFER (IND-CONTROL: SCR-SIZE)
           PERFORM VARYING IND-ALIN FROM 1 BY 1
                     UNTIL ALINHADOR (IND-ALIN: 1) NOT = SPACE
                        OR IND-ALIN > SCR-SIZE
                   CONTINUE
           END-PERFORM
           PERFORM VARYING IND-ALIN FROM IND-ALIN BY 1
                     UNTIL IND-ALIN > SCR-SIZE
                   MOVE ALINHADOR (IND-ALIN: 1) TO SCREEN-BUFFER
                                                   (IND-POSIT: 1)
                   ADD 1 TO IND-POSIT
           END-PERFORM.

       034-99-FIM. EXIT.

       035-ALINHA-NUMERICO.

           COMPUTE IND-POSIT = IND-CONTROL + SCR-SIZE - 1
           MOVE SCREEN-BUFFER (IND-CONTROL: SCR-SIZE) TO ALINHADOR
           MOVE SPACES TO SCREEN-BUFFER (IND-CONTROL: SCR-SIZE)
           PERFORM VARYING IND-ALIN FROM 18 BY -1
                     UNTIL ALINHADOR (IND-ALIN: 1) NOT = SPACE
                        OR IND-ALIN = 0
                   CONTINUE
           END-PERFORM
           PERFORM VARYING IND-ALIN FROM IND-ALIN BY -1
                     UNTIL IND-ALIN = 0
                   MOVE ALINHADOR (IND-ALIN: 1) TO SCREEN-BUFFER
                                                   (IND-POSIT: 1)
                   SUBTRACT 1 FROM IND-POSIT
           END-PERFORM.

       035-99-FIM. EXIT.

       040-ENTER.

           PERFORM TEST AFTER VARYING IND-CONTROL
                        FROM IND-CONTROL
                          BY 1
                       UNTIL CONTROL-BUFFER (IND-CONTROL: 1) = SPACE
                   IF   IND-CONTROL = IND-FIM
                        MOVE 1 TO IND-CONTROL
                   END-IF
           END-PERFORM

           PERFORM TEST AFTER VARYING IND-CONTROL
                        FROM IND-CONTROL
                          BY 1
                       UNTIL (CONTROL-BUFFER (IND-CONTROL: 1) = ALFA
                                                           OR NUMERICO)
                         OR (SKIPS NOT = 0
                        AND CONTROL-BUFFER (IND-CONTROL: 1) = ALFA-SKIP)
                   IF   IND-CONTROL NOT < IND-FIM
                        MOVE 1   TO IND-CONTROL
                        GO TO 040-ENTER
                   END-IF
           END-PERFORM.

       040-99-FIM. EXIT.

       050-UP.

           IF   IND-CONTROL < IND-TOP
                MOVE IND-TOP TO IND-CONTROL
                GO TO 050-99-FIM
           END-IF

           PERFORM VARYING IND-CONTROL
                      FROM IND-CONTROL
                        BY -1
                       UNTIL CONTROL-BUFFER (IND-CONTROL: 1) = SPACE
                   IF   IND-CONTROL < 2
                        MOVE IND-FIM TO IND-CONTROL
                   END-IF
           END-PERFORM

           IF   UP-MODE-OFF
                PERFORM TEST AFTER VARYING IND-CONTROL
                        FROM IND-CONTROL
                          BY -1
                       UNTIL CONTROL-BUFFER (IND-CONTROL: 1) = ALFA
                                                            OR NUMERICO
                   IF   IND-CONTROL < 2
                        MOVE IND-FIM TO IND-CONTROL
                   END-IF
                END-PERFORM
           ELSE
                PERFORM TEST AFTER VARYING IND-CONTROL
                        FROM IND-CONTROL
                          BY -1
                       UNTIL CONTROL-BUFFER (IND-CONTROL: 1) = ALFA
                                                            OR ALFA-SKIP
                                                            OR NUMERICO
                   IF   IND-CONTROL < 2
                        MOVE IND-FIM TO IND-CONTROL
                   END-IF
                END-PERFORM
           END-IF

           MOVE 0 TO IND-CONTROL-SAVE

           PERFORM VARYING IND-CONTROL
                      FROM IND-CONTROL
                        BY -1
                       UNTIL CONTROL-BUFFER (IND-CONTROL: 1) = SPACE
                   IF   IND-CONTROL = 1
                        MOVE IND-FIM TO IND-CONTROL
                   END-IF
                   IF   CONTROL-BUFFER (IND-CONTROL: 1) = ALFA
                                                       OR ALFA-SKIP
                                                       OR NUMERICO
                        MOVE IND-CONTROL TO IND-CONTROL-SAVE
                   END-IF
           END-PERFORM

           IF   IND-CONTROL-SAVE NOT = 0
                MOVE IND-CONTROL-SAVE TO IND-CONTROL
                GO TO 050-99-FIM
           END-IF.

       050-99-FIM. EXIT.

       060-RETURN.

           MOVE SPACES TO TB27-BUFFER
           MOVE 0      TO IND-VAR
           PERFORM VARYING IND-BUF  FROM 1  BY 1
              UNTIL IND-BUF = IND-FIM
      *          OR FRONTEIRA (IND-BUF: 1) = FIM-TRAN
      *          OR ETX
                 IF   CONTROL-BUFFER (IND-BUF: 1) = NUMERICO
                 OR   ALFA
                 OR   ALFA-SKIP
                 OR   PROTEGIDO
                      IF   IND-BUF > LIMITE
                      AND  NOT XMIT-ALL
                           MOVE SPACE TO SCREEN-BUFFER (IND-BUF: 1)
                      END-IF
                      ADD 1 TO IND-VAR
                      MOVE SCREEN-BUFFER      (IND-BUF: 1)
                        TO TB27-BUFFER        (IND-VAR: 1)
                      IF   TB27-BUFFER        (IND-VAR: 1) = X"00"
                           MOVE SPACE TO TB27-BUFFER        (IND-VAR: 1)
                      END-IF
                 END-IF
           END-PERFORM.

       060-99-FIM. EXIT.

       070-APAGA-CAMPOS.

           INSPECT ATTRIBUTE-BUFFER CONVERTING VERMELHO TO VERDE
           MOVE 0      TO IND-VAR
           PERFORM VARYING IND-BUF
               FROM 1
                 BY 1
              UNTIL IND-BUF = IND-MASK OR IND-FIM
              IF   CONTROL-BUFFER (IND-BUF: 1) = NUMERICO
              OR   ALFA
              OR   ALFA-SKIP
                   IF   IND-BUF > LIMITE
                        MOVE SPACE TO SCREEN-BUFFER (IND-BUF: 1)
                   END-IF
                   ADD 1 TO IND-VAR
                   MOVE SPACE TO SCREEN-BUFFER (IND-BUF: 1)
              END-IF
           END-PERFORM

           CALL "CBL_WRITE_SCR_CHATTRS" USING X"0000"
                                              SCREEN-BUFFER
                                              ATTRIBUTE-BUFFER
                                              LENGTH-2000.

       070-99-FIM. EXIT.
       END PROGRAM CWTB27.
