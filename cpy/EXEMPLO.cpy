           EVALUATE CLIC-LINHA
              WHEN "DETALHE"
                   MOVE MATR                                TO C01L06
                   MOVE DV                                  TO C02L06
                   MOVE NOME                                TO C03L06
                   MOVE SEXO                                TO C04L06
                   MOVE CARGO                               TO C05L06
                   MOVE SALARIO                             TO C06L06
                   MOVE TIPO(1)                             TO C07L06
                   MOVE TIPO(2)                             TO C08L06
                   MOVE TIPO(3)                             TO C09L06
                   MOVE TIPO(4)                             TO C10L06
                   MOVE TIPO(5)                             TO C11L06
                   MOVE ADMISSAO                            TO C12L06
                   MOVE DD-DEM                              TO C13L06
                   MOVE "/"                                 TO C14L06
                   MOVE MM-DEM                              TO C15L06
                   MOVE "/"                                 TO C16L06
                   MOVE AA-DEM                              TO C17L06
              WHEN "TOTAIS"
                   MOVE TOTAL-GERAL                         TO C01L07
           END-EVALUATE
