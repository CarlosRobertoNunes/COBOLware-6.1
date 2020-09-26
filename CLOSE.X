                            DISPLAY "SQLERRMC" UPON ENVIRONMENT-NAME
                            ACCEPT   SQLERRMC  FROM ENVIRONMENT-VALUE
                            IF   SQLERRMC(1:9) = 'ORA-03114'
                                              OR 'ORA-01033'
                                 CANCEL "CWORCN"
                                 move 1 to conect-error
                                 DISPLAY  SPACES UPON ENVIRONMENT-VALUE
                            END-IF
                     end-perform
