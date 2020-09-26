                    perform test after until conect-error = 0
                            DISPLAY "SQLERRMC" UPON ENVIRONMENT-NAME
                            DISPLAY  SPACES    UPON ENVIRONMENT-VALUE
                            if  conect-error = 1
                                PERFORM CONECTAR THRU FIM-CONECTAR
                                move zero to conect-error
                            end-if
