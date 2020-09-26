000010$Set CallFH"FHREDIR" Gnt()                                        FILENAME
000020 IDENTIFICATION DIVISION.                                         FILENAME
000030 PROGRAM-ID.    EXTRATOR-FILENAME.                                FILENAME
000040 AUTHOR.        COBOLware Services Ltda.                          FILENAME
000050****************************************************************  FILENAME
000060* Extrator do arquivo FILENAME                                 *  FILENAME
000070* Converte de indexado p/sequencial c/campos de valor editados *  FILENAME
000080* Produzido tendo como base o programa FNAME                   *  FILENAME
000090*                                     http://www.COBOLware.com *  FILENAME
000100****************************************************************  FILENAME
000110 DATE-WRITTEN.  01/08/2017 11:57:04.                              FILENAME
000120 ENVIRONMENT DIVISION.                                            FILENAME
000130 CONFIGURATION SECTION.                                           FILENAME
000140 SPECIAL-NAMES. DECIMAL-POINT IS COMMA.                           FILENAME
000150 INPUT-OUTPUT SECTION.                                            FILENAME
000160 FILE-CONTROL.                                                    FILENAME
000170     SELECT FILENAME ASSIGN TO DISK                               FILENAME
000180            ORGANIZATION  IS INDEXED                              FILENAME
000190            RECORD KEY IS FILENAME-CHAVE IN FILENAME-REG          FILENAME
000200            ALTERNATE RECORD KEY IS FILENAME-DESCRICAO IN         FILENAME
000210     FILENAME-REG                                                 FILENAME
000220            WITH DUPLICATES                                       FILENAME
000230            FILE STATUS IS FS-FILENAME.                           FILENAME
000240     SELECT XFILENAME ASSIGN TO DISK                              FILENAME
000250            FILE STATUS IS FS-XFILENAME                           FILENAME
000260            ORGANIZATION IS LINE  SEQUENTIAL.                     FILENAME
000270 DATA DIVISION.                                                   FILENAME
000280 FILE SECTION.                                                    FILENAME
000290 FD  XFILENAME                                                    FILENAME
000300     LABEL RECORD STANDARD                                        FILENAME
000310     VALUE OF FILE-ID IS "txt/filename.txt".                      FILENAME
000320 01 XFILENAME-REG.                                                FILENAME
000330     05 FILENAME-CHAVE.                                           FILENAME
000340        10 TXT-CODIGO                 PIC  9(005).                FILENAME
000350     05 TXT-DESCRICAO                 PIC  X(030).                FILENAME
000360     05 TXT-PRECO                     PIC  9(008)V99.             FILENAME
000370     05 TXT-TIPO                      PIC  9(001).                FILENAME
000380     05 FILENAME-OPCOES.                                          FILENAME
000390        10 TXT-IMPORTADO              PIC  9(001).                FILENAME
000400        10 TXT-GARANTIA               PIC  9(001).                FILENAME
000410        10 TXT-DURAVEL                PIC  9(001).                FILENAME
000420 FD  FILENAME                                                     FILENAME
000430     LABEL RECORD IS STANDARD                                     FILENAME
000440         VALUE OF FILE-ID IS LB-FILENAME.                         FILENAME
000450 01 FILENAME-REG.                                                 FILENAME
000460     05 FILENAME-CHAVE.                                           FILENAME
000470        10 FILENAME-CODIGO            PIC  9(005).                FILENAME
000480     05 FILENAME-DESCRICAO            PIC  X(030).                FILENAME
000490     05 FILENAME-PRECO                PIC  9(008)V99.             FILENAME
000500     05 FILENAME-TIPO                 PIC  9(001).                FILENAME
000510     05 FILENAME-OPCOES.                                          FILENAME
000520        10 FILENAME-IMPORTADO         PIC  9(001).                FILENAME
000530        10 FILENAME-GARANTIA          PIC  9(001).                FILENAME
000540        10 FILENAME-DURAVEL           PIC  9(001).                FILENAME
000550 WORKING-STORAGE SECTION.                                         FILENAME
000560 77  ONE                              PIC  9(01)     VALUE 1.     FILENAME
000570 77  LD-FILENAME                      PIC  9(12)     VALUE 0.     FILENAME
000580 77  CWRK                             PIC  9(09)     VALUE 0.     FILENAME
000590 77  CWOBS                            PIC  X(5000)   VALUE SPACES.FILENAME
000600 77  MSG                              PIC  X(080)    VALUE SPACES.FILENAME
000610 77  LD-FILENAME-ED                   PIC ZZZ.ZZZ.ZZZ.ZZ9B.       FILENAME
000620 77  FS-XFILENAME                     PIC  X(02)     VALUE "00".  FILENAME
000630 77  FS-FILENAME                      PIC  X(02)     VALUE "00".  FILENAME
000640 77  LB-FILENAME                      PIC  X(255)    value        FILENAME
000650     "P:\COBWARE\FileName".                                       FILENAME
000660 PROCEDURE DIVISION.                                              FILENAME
000670 000-INICIO.                                                      FILENAME
000680     DISPLAY "Extrator do arquivo FILENAME".                      FILENAME
000690     DISPLAY "Copyright (C) 2017 COBOLware Services Ltda."        FILENAME
000700     DISPLAY "http://www.COBOLware.com".                          FILENAME
000710     MOVE "Extrator de FILENAME iniciado" TO CWOBS                FILENAME
000720     CALL "CWCLOG" USING Z"x" CWOBS ON EXCEPTION CONTINUE END-CALLFILENAME
000730     OPEN INPUT FILENAME                                          FILENAME
000740          OUTPUT XFILENAME                                        FILENAME
000750     IF   FS-FILENAME NOT = "00"                                  FILENAME
000760          PERFORM ERRO-STATUS THRU FIM-ERRO-STATUS                FILENAME
000770          STOP RUN.                                               FILENAME
000780     IF   FS-XFILENAME > "09"                                     FILENAME
000790          MOVE SPACES TO CWOBS                                    FILENAME
000800          STRING "Erro no OPEN XFILENAME = " FS-XFILENAME         FILENAME
000810                 DELIMITED BY SIZE                                FILENAME
000820            INTO CWOBS                                            FILENAME
000830          CALL "CWCLOG" USING Z" Z" CWOBS                         FILENAME
000840            ON EXCEPTION CONTINUE END-CALL                        FILENAME
000850     END-IF                                                       FILENAME
000860     CALL X"91" USING X"00" X"2F" XFILENAME                       FILENAME
000870     DISPLAY "Extraindo FILENAME...".                             FILENAME
000880 LER-FILENAME.                                                    FILENAME
000890     READ FILENAME                                                FILENAME
000900          AT END                                                  FILENAME
000910             GO TO FIM-LER-FILENAME.                              FILENAME
000920     IF   FS-FILENAME > "09"                                      FILENAME
000930           DISPLAY "Erro na leitura de FILENAME, "                FILENAME
000940                   " Status=" FS-FILENAME                         FILENAME
000950           MOVE SPACES TO CWOBS                                   FILENAME
000960           STRING "Erro no READ FILENAME = " FS-FILENAME          FILENAME
000970                  DELIMITED BY SIZE                               FILENAME
000980             INTO CWOBS                                           FILENAME
000990           CALL "CWCLOG" USING Z" Z" CWOBS                        FILENAME
001000             ON EXCEPTION CONTINUE END-CALL                       FILENAME
001010           GO TO FIM-LER-FILENAME.                                FILENAME
001020*    MOVE CORR FILENAME-REG TO XFILENAME-REG                      COBOLware
001030     MOVE FILENAME-CODIGO TO TXT-CODIGO                           FILENAME
001040     MOVE FILENAME-DESCRICAO TO TXT-DESCRICAO                     FILENAME
001050     MOVE FILENAME-PRECO TO TXT-PRECO                             FILENAME
001060     MOVE FILENAME-TIPO TO TXT-TIPO                               FILENAME
001070     MOVE FILENAME-IMPORTADO TO TXT-IMPORTADO                     FILENAME
001080     MOVE FILENAME-GARANTIA TO TXT-GARANTIA                       FILENAME
001090     MOVE FILENAME-DURAVEL TO TXT-DURAVEL                         FILENAME
001100     INSPECT XFILENAME-REG                                        FILENAME
001110             REPLACING ALL LOW-VALUE BY SPACE                     FILENAME
001120     INSPECT XFILENAME-REG                                        FILENAME
001130             CONVERTING X"0D0A0C" TO X"202020"                    FILENAME
001140     WRITE XFILENAME-REG                                          FILENAME
001150     ADD 1 TO LD-FILENAME                                         FILENAME
001160     GO TO LER-FILENAME.                                          FILENAME
001170 FIM-LER-FILENAME.                                                FILENAME
001180     MOVE LD-FILENAME TO LD-FILENAME-ED                           FILENAME
001190     STRING "Extrator de FILENAME encerrado,"                     FILENAME
001200            LD-FILENAME-ED(1:) "registros extraidos "             FILENAME
001210            DELIMITED BY SIZE                                     FILENAME
001220            LB-FILENAME                                           FILENAME
001230            DELIMITED BY SPACE                                    FILENAME
001240       INTO CWOBS                                                 FILENAME
001250     CALL "CWPACK" USING CWOBS LENGTH CWOBS                       FILENAME
001260       ON EXCEPTION CONTINUE END-CALL                             FILENAME
001270     CALL "CWCLOG" USING Z"x" CWOBS                               FILENAME
001280       ON EXCEPTION CONTINUE END-CALL                             FILENAME
001290     CLOSE FILENAME                                               FILENAME
001300     CLOSE XFILENAME                                              FILENAME
001310     DISPLAY " Total de registros de FILENAME: " LD-FILENAME      FILENAME
001320     STOP RUN.                                                    FILENAME
001330 ERRO-STATUS.                                                     FILENAME
001340     MOVE SPACES TO CWOBS                                         FILENAME
001350     EVALUATE TRUE                                                FILENAME
001360         WHEN FS-FILENAME = "11"                                  FILENAME
001370               STRING "Volume " DELIMITED BY SIZE                 FILENAME
001380                      LB-FILENAME DELIMITED BY SPACE              FILENAME
001390                      " ignorado." DELIMITED BY SIZE              FILENAME
001400                 INTO CWOBS                                       FILENAME
001410          WHEN FS-FILENAME = "30" OR "35"                         FILENAME
001420               STRING "Arquivo " DELIMITED BY SIZE                FILENAME
001430                      LB-FILENAME DELIMITED BY SPACE              FILENAME
001440                      " inexistente." DELIMITED BY SIZE           FILENAME
001450                 INTO CWOBS                                       FILENAME
001460          WHEN FS-FILENAME = "39"                                 FILENAME
001470                STRING "Estrutura de " DELIMITED BY SIZE          FILENAME
001480                       LB-FILENAME DELIMITED BY SPACE             FILENAME
001490                       " divergente." DELIMITED BY SIZE           FILENAME
001500                INTO CWOBS                                        FILENAME
001510           WHEN OTHER                                             FILENAME
001520                STRING "Erro " FS-FILENAME " "                    FILENAME
001530                        LB-FILENAME                               FILENAME
001540                       DELIMITED BY SIZE                          FILENAME
001550                  INTO CWOBS                                      FILENAME
001560      END-EVALUATE                                                FILENAME
001570      CALL "CWCLOG" USING "x" CWOBS                               FILENAME
001580        ON EXCEPTION CONTINUE END-CALL                            FILENAME
001590      DISPLAY CWOBS WITH SIZE 80.                                 FILENAME
001600 FIM-ERRO-STATUS. EXIT.                                           FILENAME
