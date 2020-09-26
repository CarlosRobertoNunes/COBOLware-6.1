       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWCHEC.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/10/2005.
       SECURITY.      *************************************************
                      *                                               *
                      *  Verifica validade de codigos                 *
                      *                                               *
                      *************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 ERRO                  COMP-X PIC  9(002) VALUE 0.
           05 CNPJ-CEI-CIC          COMP-3 PIC  9(014) VALUE 0.
           05 SOMA                         PIC  9(004) VALUE 0.
           05 D                            PIC  9(002) VALUE 0.
           05 I                            PIC S9(002) VALUE 0.
           05 C                            PIC  9(002) VALUE 0.
           05 C1                           PIC  9(001) VALUE 0.
           05 ERRO-CARD                    PIC  9(001) VALUE 0.
           05 CARD                         PIC  9(016) VALUE 0.
           05 CARD-ED                      PIC  9999B9999B9999B9999.
           05 ED                           PIC  X(018) VALUE SPACES.

       COPY CWDCEI.
       COPY CWDCIC.
       COPY CWDCNP.
       COPY CWDPIS.
       COPY CWDTSE.
       COPY CWIEOK.

       LINKAGE SECTION.

       COPY CWCHEC.

       PROCEDURE DIVISION USING PARAMETROS-CWCHEC.

       000-INICIO.

           MOVE 0 TO ERRO

           IF   CWCHEC-CEI NOT = 0
                MOVE CWCHEC-CEI TO CWDCEI-CEI CNPJ-CEI-CIC
                CALL "CWDCEI" USING PARAMETROS-CWDCEI
                IF  CWDCEI-RETORNO NOT = "0"
                    MOVE SPACES TO CWCHEC-CEI-EDITADO
                    ADD  1      TO ERRO
                ELSE
                    CALL "CWECNP" USING CNPJ-CEI-CIC ED
                    MOVE ED (2: )    TO CWCHEC-CEI-EDITADO
                END-IF
           END-IF

           IF   CWCHEC-CIC NOT = 0
                MOVE CWCHEC-CIC TO CWDCIC-CIC CNPJ-CEI-CIC
                CALL "CWDCIC" USING PARAMETROS-CWDCIC
                IF  CWDCIC-RETORNO NOT = "00"
                    MOVE SPACES TO CWCHEC-CIC-EDITADO
                    ADD  2      TO ERRO
                ELSE
                    CALL "CWECNP" USING CNPJ-CEI-CIC ED
                    MOVE ED (5: )    TO CWCHEC-CIC-EDITADO
                END-IF
           END-IF

           IF   CWCHEC-CNPJ NOT = 0
                MOVE CWCHEC-CNPJ TO CWDCNP-CNPJ CNPJ-CEI-CIC
                CALL "CWDCNP" USING PARAMETROS-CWDCNP
                IF  CWDCNP-RETORNO NOT = "00"
                    MOVE SPACES TO CWCHEC-CNPJ-EDITADO
                    ADD  4      TO ERRO
                ELSE
                    CALL "CWECNP" USING CNPJ-CEI-CIC CWCHEC-CNPJ-EDITADO
                END-IF
           END-IF

           IF   CWCHEC-PIS-PASEP NOT = 0
                MOVE CWCHEC-PIS-PASEP TO CWDPIS-PIS-PASEP
                CALL "CWDPIS" USING PARAMETROS-CWDPIS
                IF  CWDPIS-RETORNO NOT = "0"
                    MOVE SPACES TO CWCHEC-PIS-PASEP-EDITADO
                    ADD  8      TO ERRO
                ELSE
                    MOVE CWDPIS-PIS-PASEP-ED TO CWCHEC-PIS-PASEP-EDITADO
                END-IF
           END-IF

           IF   CWCHEC-TITULO-ELEITOR NOT = 0
                MOVE CWCHEC-TITULO-ELEITOR TO CWDTSE-TITULO
                CALL "CWDTSE" USING PARAMETROS-CWDTSE
                IF  CWDTSE-RETORNO NOT = "0"
                    MOVE SPACES TO CWCHEC-TITULO-EDITADO
                    ADD  16     TO ERRO
                ELSE
                    MOVE CWDTSE-TITULO-ED TO CWCHEC-TITULO-EDITADO
                END-IF
           END-IF.

           IF   CWCHEC-UF NOT = SPACES
                INSPECT CWCHEC-UF CONVERTING MINUSCULAS TO MAIUSCULAS
                MOVE SPACES TO CWCHEC-UF-EXTENSO
                PERFORM 100-UF-EDIT THRU 100-99-FIM
                IF  CWCHEC-UF-EXTENSO = SPACES
                    ADD  32     TO ERRO
                END-IF
           END-IF

           IF   CWCHEC-IE NOT = SPACES
                MOVE CWCHEC-UF TO CWIEOK-UF
                MOVE CWCHEC-IE TO CWIEOK-IE
                CALL "CWIEOK" USING PARAMETROS-CWIEOK
                IF  CWIEOK-RETORNO NOT = 0
                    ADD  64     TO ERRO
                ELSE
                    MOVE CWIEOK-IE-ED TO CWCHEC-IE-EDITADA
                EnD-IF
           END-IF

           IF   CWCHEC-CARD NOT = SPACES
                MOVE ZERO   TO CARD ERRO-CARD
                MOVE 17   TO C
                PERFORM VARYING I FROM LENGTH CWCHEC-CARD BY -1
                                       UNTIL I = 0
                        IF CWCHEC-CARD (I:1) NUMERIC
                           IF C = 1
                              MOVE 1 TO ERRO-CARD
                              EXIT PERFORM
                           END-IF
                           SUBTRACT 1 FROM C
                           MOVE CWCHEC-CARD (I:1) TO CARD (C:1)
                        ELSE
                           IF CWCHEC-CARD (I:1) NOT = SPACE
                              MOVE 1 TO ERRO-CARD
                              EXIT PERFORM
                           END-IF
                        END-IF
                END-PERFORM
                MOVE CARD TO CARD-ED
                COMPUTE I = LENGTH CARD - 1
                PERFORM VARYING I FROM I BY -2
                                    UNTIL I = -1
                                      OR ERRO-CARD = 1
                        MOVE CARD (I: 1) TO C1
                        COMPUTE D = C1 * 2
                        IF D > 9
                           SUBTRACT 9 FROM D
                        END-IF
                        MOVE D  TO C1
                        MOVE C1 TO CARD (I: 1)
                END-PERFORM
                MOVE ZERO TO SOMA
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH CARD
                        MOVE CARD (I: 1) TO C
                        ADD  C           TO SOMA
                END-PERFORM
                IF   SOMA > 149
                OR  (SOMA (4: 1) NOT = '0')
                     MOVE 1 TO ERRO-CARD
                END-IF
                IF   ERRO-CARD NOT = ZERO
                     ADD 128 TO ERRO
                ELSE
                     MOVE CARD-ED TO CWCHEC-CARD
                END-IF
           END-IF

           MOVE ERRO TO CWCHEC-ERROR-LEVEL.

       000-99-FIM. GOBACK.

       100-UF-EDIT.

           EVALUATE CWCHEC-UF
            WHEN "AC"
                 MOVE "Acre                Rio Branco     Norte       "
                   TO CWCHEC-UF-DADOS
            WHEN "AL"
                 MOVE "Alagoas             Macei¢         Nordeste    "
                   TO CWCHEC-UF-DADOS
            WHEN "AM"
                 MOVE "Amazonas            Manaus         Norte       "
                   TO CWCHEC-UF-DADOS
            WHEN "AP"
                 MOVE "Amap                Macap          Norte       "
                   TO CWCHEC-UF-DADOS
            WHEN "BA"
                 MOVE "Bahia               Salvador       Nordeste    "
                   TO CWCHEC-UF-DADOS
            WHEN "CE"
                 MOVE "Cear                Fortaleza      Nordeste    "
                   TO CWCHEC-UF-DADOS
            WHEN "DF"
                 MOVE "Distrito Federal    Bras¡lia       Centro-Oeste"
                   TO CWCHEC-UF-DADOS
            WHEN "ES"
                 MOVE "Esp¡rito Santo      Vit¢ria        Sudeste     "
                   TO CWCHEC-UF-DADOS
            WHEN "GO"
                 MOVE "Goi s               Goaƒnia        Centro-Oeste"
                   TO CWCHEC-UF-DADOS
            WHEN "MA"
                 MOVE "MaranhÆo            SÆo Lu¡s       Nordeste    "
                   TO CWCHEC-UF-DADOS
            WHEN "MG"
                 MOVE "Minas Gerais        Belo Horizonte Sudeste     "
                   TO CWCHEC-UF-DADOS
            WHEN "MS"
                 MOVE "Mato Grosso do Sul  Campo Grande   Centro-Oeste"
                   TO CWCHEC-UF-DADOS
            WHEN "MT"
                 MOVE "Mato Grosso         Cuaib          Centro-Oeste"
                   TO CWCHEC-UF-DADOS
            WHEN "PA"
                 MOVE "Par                 Bel‚m          Norte       "
                   TO CWCHEC-UF-DADOS
            WHEN "PB"
                 MOVE "Para¡ba             JoÆo Pessoa    Nordeste    "
                   TO CWCHEC-UF-DADOS
            WHEN "PE"
                 MOVE "Pernambuco          Recife         Nordeste    "
                   TO CWCHEC-UF-DADOS
            WHEN "PI"
                 MOVE "Piau¡               Teresina       Nordeste    "
                   TO CWCHEC-UF-DADOS
            WHEN "PR"
                 MOVE "Paran               Curitiba       Sul         "
                   TO CWCHEC-UF-DADOS
            WHEN "RJ"
                 MOVE "Rio de Janeiro      Rio de Janeiro Sudeste     "
                   TO CWCHEC-UF-DADOS
            WHEN "RN"
                 MOVE "Rio Grande do Norte Natal          Nordeste    "
                   TO CWCHEC-UF-DADOS
            WHEN "RO"
                 MOVE "Rond“nia            Porto Velho    Norte       "
                   TO CWCHEC-UF-DADOS
            WHEN "RR"
                 MOVE "Roraima             Boa Vista      Norte       "
                   TO CWCHEC-UF-DADOS
            WHEN "RS"
                 MOVE "Rio Grande do Sul   Porto Alegre   Sul         "
                   TO CWCHEC-UF-DADOS
            WHEN "SC"
                 MOVE "Santa Catarina      Florian¢polis  Sul         "
                   TO CWCHEC-UF-DADOS
            WHEN "SE"
                 MOVE "Sergipe             Aracaj£        Nordeste    "
                   TO CWCHEC-UF-DADOS
            WHEN "SP"
                 MOVE "SÆo Paulo           SÆo Paulo      Sudeste     "
                   TO CWCHEC-UF-DADOS
            WHEN "TO"
                 MOVE "Tocantins           Palmas         Norte       "
                   TO CWCHEC-UF-DADOS
           END-EVALUATE.

       100-99-FIM. EXIT.
       END PROGRAM CWCHEC.
