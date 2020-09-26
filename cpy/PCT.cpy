       FD  PCT
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-PCT.

       01  PCT-RECORD.
           05 PCT-KEY.
              06 PCT-TRANSACTION  PIC X(04).
              06 PCT-STEP         PIC 9(04) COMP-X.
           05 PCT-PROGRAM         PIC X(08).
           05 PCT-GROUP           PIC X(08).
           05 PCT-DESCRIPTION     PIC X(50).

