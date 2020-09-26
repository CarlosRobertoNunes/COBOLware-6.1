       FD  STARTS
           LABEL RECORD IS STANDARD
           RECORD VARYING FROM 56 TO 32829 DEPENDING ON SZ-STARTS
           VALUE OF FILE-ID IS LB-STARTS.

       01  STARTS-RECORD.
           05 STARTS-KEY.
              06 STARTS-ENTRY                 PIC  9(008) COMP-5.
           05                                 pic  9(004) comp-5.
              88 starts-AFTER                      VALUE 5003.
              88 starts-AT                         VALUE 5005.
           05 STARTS-HOURS                    PIC  9(008) COMP-5.
           05 STARTS-INTERVAL                 PIC  9(004) COMP-5.
           05 STARTS-MINUTES                  PIC  9(008) COMP-5.
           05                                 pic  9(004) comp-5.
              88 starts-NOCHECK                    VALUE 5047.
           05                                 pic  9(004) comp-5.
              88 starts-PROTECT                    VALUE 5064.
           05 STARTS-REQID                    PIC  X(008).
           05 STARTS-RTERMID                  PIC  X(004).
           05 STARTS-RTRANSID                 PIC  X(004).
           05 STARTS-SECONDS                  PIC  9(008) COMP-5.
           05 STARTS-TRANSID                  PIC  X(004).
           05 STARTS-LENGTH                   PIC  9(008) COMP-5.
           05 STARTS-STARTCODE                PIC  X(002).
           05 STARTS-EIBAID                   PIC  X(001).
           05 STARTS-TECLA                    PIC  9(002) COMP-X.
           05 STARTS-FROM.
              06 STARTS-FROM-BYTE             PIC  X(001)
              OCCURS 0 TO 32767 DEPENDING ON STARTS-LENGTH.

