       identification division.
       program-id.         FTPprogram.
       author.             Fano "Raz" / Product Group / Micro Focus.
       date-written.       05/11/2006.
      ******************************************************************
      * This demo uses the Windows Internet (WinINet) application
      * programming interface (API) to interact with FTP protocols
      * to access Internet resources. It is using WinInet functions
      * to accomplish the following tasks:
      *     1. open Internet session (InternetOpen)
      *     2. log on to FTP server (InternetConnect)
      *     3. determine current directory (FtpGetCurrentDirectory)
      *     4. change directory (FtpSetCurrentDirectory)
      *     5. create new directory (FtpCreateDirectory)
      *     6. upload file (FtpPutFile)
      *     7. rename file (FtpRenameFile)
      *     8. download file (FtpGetFile)
      *     9. delete file (FtpDeleteFile)
      *    10. delete directory (FtpRemoveDirectory)
      *    11. end Internet handle (InternetCloseHandle)
      *
      * NOTE: The purpose of the demo is more focused on how to use
      *       use WinInet functions rather than efficiency. It does
      *       not determine the cause of any errors. It simply sees
      *       if the function succeeded or not.
      *
      *       Please visit the Microsoft MSDN site for further
      *       details on MS Windows Internet (WinInet) at
      * http://msdn.microsoft.com/library/en-us/wininet/wininet/portal.asp
      *
      ******************************************************************
       environment division.
       configuration section.
       special-names.
           call-convention 66 is cc66.     *> dynamic link

       data division.
       working-storage section.

       77  wininetPtr procedure-pointer.   *> Wininet.dll pointer

      *> InternetOpen parameters --->
       77  szAgent pic x(256).             *> agent name passed to server during each call
       77  dwAccessType pic xxxx comp-5.   *>
      *#define INTERNET_OPEN_TYPE_PRECONFIG    0   // use registry configuration
       78  INTERNET-OPEN-TYPE-PRECONFIG  value 0.
      *#define INTERNET_OPEN_TYPE_DIRECT       1   // direct to net
       78  INTERNET-OPEN-TYPE-DIRECT     value 1.
      *#define INTERNET_OPEN_TYPE_PROXY        3   // via named proxy
       78  INTERNET-OPEN-TYPE-PROXY      value 3.
       77  szProxyName pic x(256).         *> name of proxy server(s) when INTERNET-OPEN-TYPE-PROXY
       77  szProxyBypass pic x(256).       *> host name or ip address when INTERNET-OPEN-TYPE-PROXY
       77  dwFlags pic xxxx comp-5.        *> numeric values defined in header file
      *#define INTERNET_FLAG_ASYNC             0x10000000  // Makes only asynchronous requests on handles descended from the handle returned from this function.
       78  INTERNET-FLAG-ASYNC           value h'10000000'.
      *#define INTERNET_FLAG_FROM_CACHE        0x01000000  // Does not make network requests. All entities are returned from the cache.
       78  INTERNET-FLAG-FROM-CACHE      value h'01000000'.
      *#define INTERNET_FLAG_OFFLINE           0x01000000  // Identical to INTERNET_FLAG_FROM_CACHE.
       78  INTERNET-FLAG-OFFLINE         value h'01000000'.
      *#define INTERNET_FLAG_NO_CACHE_WRITE    0x04000000  // don't write this item to the cache
       78  INTERNET-FLAG-NO-CACHE-WRITE  value h'04000000'.
      *#define INTERNET_FLAG_DONT_CACHE        INTERNET_FLAG_NO_CACHE_WRITE
       78  INTERNET-FLAG-DONT-CACHE      value h'04000000'.
       77  hInternet pic xxxx comp-5.      *> handle
      *> InternetOpen parameters <---

      *> InternetConnect parameters --->
      *77  hInternet pic xxxx comp-5.      *> handle
       77  szServerName pic x(256).        *> host name of Internet server
       77  nServerPort pic 9(4) comp-5.    *> TCP/IP port on server
      *#define INTERNET_INVALID_PORT_NUMBER    0           // use the protocol-specific default
       78  INTERNET-INVALID-PORT-NUMBER  value 0.
      *#define INTERNET_DEFAULT_FTP_PORT       21          // default for FTP servers
       78  INTERNET-DEFAULT-FTP-PORT     value 21.
      *#define INTERNET_DEFAULT_GOPHER_PORT    70          //    "     "  gopher "
       78  INTERNET-DEFAULT-GOPHER-PORT  value 70.
      *#define INTERNET_DEFAULT_HTTP_PORT      80          //    "     "  HTTP   "
       78  INTERNET-DEFAULT-HTTP-PORT    value 80.
      *#define INTERNET_DEFAULT_HTTPS_PORT     443         //    "     "  HTTPS  "
       78  INTERNET-DEFAULT-HTTPS-PORT   value 443.
      *#define INTERNET_DEFAULT_SOCKS_PORT     1080        // default for SOCKS firewall servers.
       78  INTERNET-DEFAULT-SOCKS-PORT   value 1080.
       77  szUsername pic x(256).          *> name of user to log on
       77  szPassword pic x(256).          *> password to use to log on
       77  dwService pic xxxx comp-5.      *> Type of service to access
      *#define INTERNET_SERVICE_FTP      1 // FTP service
       78  INTERNET-SERVICE-FTP    value 1.
      *#define INTERNET_SERVICE_GOPHER   2 // Gopher service
       78  INTERNET-SERVICE-GOPHER value 2.
      *#define INTERNET_SERVICE_HTTP     3 // HTTP service
       78  INTERNET-SERVICE-HTTP   value 3.
      *77  dwFlags pic xxxx comp-5.        *> numeric values defined in header file
      *#define INTERNET_FLAG_PASSIVE           0x08000000  // used for FTP connections
       78  INTERNET-FLAG-PASSIVE         value h'08000000'.
       77  dwContext pointer.              *> Context pointer
       77  hConnect pic xxxx comp-5.       *> handle
      *> InternetConnect parameters <---

      *> FtpGetCurrentDirectory parameters --->
      *77  hConnect pic xxxx comp-5.      *> handle
       77  szCurrentDirectory pic x(256).  *> absolute path of current directory.
       77  dwCurrentDirectory pic xxxx comp-5. *> number of chars copied into buffer
       77  fileStatus pic s9(9) comp-5 value 0.
       78  1FALSE value 0.
       78  1TRUE  value 1.
      *> FtpGetCurrentDirectory parameters <---

      *> FtpSetCurrentDirectory parameters --->
      *77  hConnect pic xxxx comp-5.      *> handle
      *77  szCurrentDirectory pic x(256).  *> absolute path of current directory.
      *77  fileStatus pic s9(9) comp-5 value 0.
      *> FtpSetCurrentDirectory parameters <---

      *> FtpCreateDirectory parameters --->
      *77  hConnect pic xxxx comp-5.      *> handle
      *77  szCurrentDirectory pic x(256).  *> absolute path of current directory.
      *77  fileStatus pic s9(9) comp-5 value 0.
      *> FtpCreateDirectory parameters <---

      *> FtpPutFile parameters --->
      *77  hConnect pic xxxx comp-5.       *> handle
       77  szLocalFile pic x(256).     *> drive, path, filename, null terminated.
       77  szNewRemoteFile pic x(256). *> remote filename, null terminated
      *77  dwFlags pic xxxx comp-5.        *> numeric values defined in header file
      * Note: these are 'C' hexadecimal literals.
      *#define FTP_TRANSFER_TYPE_ASCII     0x00000001
       78  FTP-TRANSFER-TYPE-ASCII   value h'00000001'.
      *#define FTP_TRANSFER_TYPE_BINARY    0x00000002
       78  FTP-TRANSFER-TYPE-BINARY  value h'00000002'.
      *    Note: these hexadecimal literals are appropriate for
      *          initializing numeric items like "dwFlags".
      *    There are two (2) formats for hexadecimal literals in Micro
      *    Focus COBOL. The examples above are numeric.
      *    To set up a hexadecimal literal for initializing a character
      *    variable, you would use a "x" prefix.
       78  An-A-Constant value x'41'.
      *    This would be appropriate for initializing a pic x data item
      *77  dwContext pointer.             *> Context pointer
      *77  fileStatus pic s9(9) comp-5 value 0.
      *> FtpPutFile parameters <---

      *> FtpRenameFile parameters --->
      *77  hConnect pic xxxx comp-5.       *> handle
       77  szExisting pic x(256). *> existing filename, null terminated
       77  szNew pic x(256).      *> new filename, null terminated
      *77  fileStatus pic s9(9) comp-5 value 0.
      *> FtpRenameFile parameters --->

      *> FtpGetFile parameters --->
      *77  hConnect pic xxxx comp-5.       *> handle
       77  szRemoteFile pic x(256). *> filename, null terminated
       77  szNewFile pic x(256).     *> drive, path, filename, null terminated.
       77  dwFlagsAttributes pic xxxx comp-5.
       78  FILE-ATTRIBUTE-READONLY            value h"00000001".
       78  FILE-ATTRIBUTE-HIDDEN              value h"00000002".
       78  FILE-ATTRIBUTE-SYSTEM              value h"00000004".
       78  FILE-ATTRIBUTE-DIRECTORY           value h"00000010".
       78  FILE-ATTRIBUTE-ARCHIVE             value h"00000020".
       78  FILE-ATTRIBUTE-NORMAL              value h"00000080".
       78  FILE-ATTRIBUTE-TEMPORARY           value h"00000100".
       78  FILE-ATTRIBUTE-COMPRESSED          value h"00000800".
       78  FILE-ATTRIBUTE-OFFLINE             value h"00001000".
      *77  dwFlags pic xxxx comp-5.        *> numeric values defined in header file
      *#define FTP_TRANSFER_TYPE_ASCII     0x00000001
      *78  FTP-TRANSFER-TYPE-ASCII   value h'00000001'.
      *#define FTP_TRANSFER_TYPE_BINARY    0x00000002
      *78  FTP-TRANSFER-TYPE-BINARY  value h'00000002'.
      *77  dwContext pointer.             *> Context pointer
      *77  fileStatus pic s9(9) comp-5 value 0.
      *> FtpPutFile parameters <---

      *> FtpDeleteFile parameters --->
      *77  hConnect pic xxxx comp-5.       *> handle
       77  szFileName pic x(256). *> patially/fully qualified filename - null terminated
      *77  fileStatus pic s9(9) comp-5 value 0.
      *> FtpDeleteFile parameters --->

      *> FtpRemoveDirectory parameters --->
      *77  hConnect pic xxxx comp-5.       *> handle
       77  szDirectory pic x(256). *> patially/fully qualified directory - null terminated
      *77  fileStatus pic s9(9) comp-5 value 0.
      *> FtpRemoveDirectory parameters --->

       1 ftpServer pic x(256).
       1 userID pic x(256).
       1 userPwd pic x(256).
       1 displayDir pic x(256).
       1 aDir pic x(256).
       1 saveDir pic x(256).
       1 continueFlag pic x.
         88 continueYes values 'y', 'Y'.
       1 i pic 999.

       procedure division.
           perform StartProgram
           perform OpenInternet
           perform ConnectInternet
           perform GetCurrentDirectory
           perform ChangeDirectory until fileStatus = 1TRUE
           perform GetCurrentDirectory
           perform CreateDirectory
           perform PutFile
           perform RenameFile
           perform GetFile
           perform DeleteFile
           perform ReturnParentDirectory
           perform RemoveDirectory
           perform CloseAllHandles
           goback
           .

       StartProgram.
           display '[FTPprogram]'
           display space
           set wininetPtr to entry 'wininet'
           if wininetPtr = NULL
               display '***Unable to find wininet.dll'
               display space
               display 'End of program'
               goback
           end-if
           display 'Starting...'
           .

       OpenInternet.
           display space
           display 'Opening Internet session...'
           move z'FTPprogram' to szAgent
           move INTERNET-OPEN-TYPE-DIRECT to dwAccessType
           move x'00' to szProxyName
           move x'00' to szProxyBypass
           move INTERNET-FLAG-NO-CACHE-WRITE to dwFlags
           call cc66 'InternetOpenA'
               using by reference  szAgent
                     by value      dwAccessType
                     by reference  szProxyName
                     by reference  szProxyBypass
                     by value      dwFlags
               returning hInternet
           end-call
           if hInternet = zero
               display '*** winAPI InternetOpen failed'
               display space
               display '...Ending'
               goback
           else
               display '+ winAPI InternetOpen succeeded'
           end-if
           .

       ConnectInternet.
           display space
           display 'Connecting to FTP server...'
      *    move z'ftp.microfocus.com' to szServerName  *> or below --->
           display 'Enter FTP server name (e.g. ftp.microfocus.com): '
               with no advancing
           accept ftpServer
           string
               ftpServer delimited by space
               x'00' delimited by size
               into szServerName
      *    <---
      *    move z'userid' to szUsername  *> or below --->
           display 'Userid (blank = anonymous): ' with no advancing
           accept userID
           string
               userID delimited by space
               x'00' delimited by size
               into szUsername
      *    <---
      *    move z'password' to szPassword  *> or below --->
           display 'Password (blank = email address): '
               with no advancing
           accept userPwd
           string
               userPwd delimited by space
               x'00' delimited by size
               into szPassword
      *    <---
           move INTERNET-DEFAULT-FTP-PORT to nServerPort
           move INTERNET-SERVICE-FTP to dwService
           move INTERNET-FLAG-PASSIVE to dwFlags
           set dwContext to NULL
           call cc66 'InternetConnectA'
               using by value      hInternet
                     by reference  szServerName
                     by value      nServerPort
                     by reference  szUsername
                     by reference  szPassword
                     by value      dwService
                     by value      dwFlags
                     by value      dwContext
               returning hConnect
           end-call
           if hConnect = zero
               display '*** winAPI InternetConnect failed'
               perform CloseInternetHandle
               goback
           else
               display '+ winAPI InternetConnect succeeded'
           end-if
           .

       GetCurrentDirectory.
           display space
           display 'Determining current directory...'
           move x'00' to szCurrentDirectory
           move length of szCurrentDirectory to dwCurrentDirectory
           call cc66 'FtpGetCurrentDirectoryA'
               using by value      hConnect
                     by reference  szCurrentDirectory
                     by reference  dwCurrentDirectory
               returning fileStatus
           end-call
           if fileStatus = 1FALSE
               display '*** winAPI FtpGetCurrentDirectory failed'
               perform CloseAllHandles
               goback
           else
               perform DisplayCurrentDirectory
               display '+ winAPI FtpGetCurrentDirectory succeeded'
           end-if
           move zero to fileStatus
           .

       DisplayCurrentDirectory.
           string
               '  --> ' delimited by size
               szServerName delimited by x'00'
               szCurrentDirectory delimited by x'00'
               x'00' delimited by size
               into displayDir
           end-string
           perform varying i from 1 by 1
               until displayDir(i:1) = x'00'
                   continue
           end-perform
           display displayDir(1:i - 1) *> display up to before x'00'
           .

       ChangeDirectory.
           display space
           display 'Changing directory...'
           display 'Enter directory name (e.g. pub or pub/upload): '
               with no advancing
           accept aDir
           string
               '/' delimited by size
               aDir delimited by space
               x'00' delimited by size
               into szCurrentDirectory
           end-string
           move szCurrentDirectory to saveDir
           perform SetCurrentDirectory
           .

       SetCurrentDirectory.
           call cc66 'FtpSetCurrentDirectoryA'
               using by value      hConnect
                     by reference  szCurrentDirectory
               returning fileStatus
           end-call
           if fileStatus = 1FALSE
               display '*** winAPI FtpSetCurrentDirectory failed'
               display 'Enter Y or y to continue or else to quit: '
                   with no advancing
               accept continueFlag
               if not continueYes
                   perform CloseAllHandles
                   goback
               end-if
           else
               perform DisplayCurrentDirectory
               display '+ winAPI FtpSetCurrentDirectory succeeded'
           end-if
           .

       CreateDirectory.
           display space
           display 'Creating temp directory...'
           move z'temp' to aDir
           call cc66 'FtpCreateDirectoryA'
               using by value      hConnect
                     by reference  aDir
               returning fileStatus
           end-call
           if fileStatus = 1FALSE
               display '*** winAPI FtpCreateDirectory failed'
               perform CloseAllHandles
               goback
           else
               display '+ winAPI FtpCreateDirectory succeeded'
               string
                   saveDir delimited by x'00'
                   '/' delimited by size
                   aDir delimited by x'00'
                   x'00' delimited by size
                   into szCurrentDirectory
               end-string
               perform SetCurrentDirectory
           end-if
           .

       PutFile.
           display space
           display 'Uploading Upload.txt as ftpDeleteMe.txt ...'
           move z'Upload.txt' to szLocalFile
           move z'ftpDeleteMe.txt' to szNewRemoteFile
           move FTP-TRANSFER-TYPE-ASCII to dwFlags
           set dwContext to NULL
           call cc66 'FtpPutFileA'
               using by value      hConnect
                     by reference  szLocalFile
                     by reference  szNewRemoteFile
                     by value      dwFlags
                     by value      dwContext
               returning fileStatus
           end-call
           if fileStatus = 1FALSE
               display '*** winAPI FtpPutfile failed'
               perform CloseAllHandles
               goback
           else
               display '+ winAPI FtpPutfile succeeded'
           end-if
           .

       RenameFile.
           display space
           display 'Renaming ftpDeleteMe.txt to NewFile.txt ...'
           move z'ftpDeleteMe.txt' to szExisting
           move z'NewFile.txt' to szNew
           call cc66 'FtpRenameFileA'
               using by value      hConnect
                     by reference  szExisting
                     by reference  szNew
               returning fileStatus
           end-call
           if fileStatus = 1FALSE
               display '*** winAPI FtpRenameFile failed'
               perform CloseAllHandles
               goback
           else
               display '+ winAPI FtpRenameFile succeeded'
           end-if
           .

       GetFile.
           display space
           display 'Downloading NewFile.txt as FromFTP.txt ...'
           move z'NewFile.txt' to szRemoteFile
           move z'FromFTP.txt' to szNewFile
           move FILE-ATTRIBUTE-NORMAL to dwFlagsAttributes
           move FTP-TRANSFER-TYPE-ASCII to dwFlags
           set dwContext to NULL
           call cc66 'FtpGetFileA'
               using by value      hConnect
                     by reference  szRemoteFile
                     by reference  szNewFile
                     by value      1FALSE  *> overwrite if file exists
                     by value      dwFlagsAttributes
                     by value      dwFlags
                     by value      dwContext
               returning fileStatus
           end-call
           if fileStatus = 1FALSE
               display '*** winAPI FtpGetFile failed'
               perform CloseAllHandles
               goback
           else
               display '+ winAPI FtpGetFile succeeded'
           end-if
           .

       DeleteFile.
           display space
           display 'Deleting NewFile.txt ...'
           move z'NewFile.txt' to szFileName
           call cc66 'FtpDeleteFileA'
               using by value      hConnect
                     by reference  szFileName
               returning fileStatus
           end-call
           if fileStatus = 1FALSE
               display '*** winAPI FtpDeleteFile failed'
               perform CloseAllHandles
               goback
           else
               display '+ winAPI FtpDeleteFile succeeded'
           end-if
           .

       ReturnParentDirectory.
           display space
           display 'Returning to parent directory...'
           move saveDir to szCurrentDirectory
           perform SetCurrentDirectory
           .

       RemoveDirectory.
           display space
           display 'Deleting temp directory...'
           move aDir to szDirectory *> temp
           call cc66 'FtpRemoveDirectoryA'
               using by value      hConnect
                     by reference  szDirectory
               returning fileStatus
           end-call
           if fileStatus = 1FALSE
               display '*** winAPI FtpRemoveDirectory failed'
               perform CloseAllHandles
               goback
           else
               display '+ winAPI FtpRemoveDirectory succeeded'
           end-if
           .

       CloseAllHandles.
           display space
           display 'Closing Internet connection...'
           call cc66 'InternetCloseHandle'
               using by value      hConnect
               returning fileStatus
           end-call
           if fileStatus = 1FALSE
               display '*** winAPI InternetCloseHandle failed'
               perform CloseAllHandles
               goback
           else
               display '+ winAPI InternetCloseHandle succeeded'
           end-if
           perform CloseInternetHandle
           .

       CloseInternetHandle.
           display space
           display 'Closing Internet session...'
           call cc66 'InternetCloseHandle'
               using by value      hInternet
               returning fileStatus
           end-call
           if fileStatus = 1FALSE
               display '*** winAPI InternetCloseHandle failed'
               perform CloseAllHandles
               goback
           else
               display '+ winAPI InternetCloseHandle succeeded'
           end-if
           display space
           display '...Ending'
           .
