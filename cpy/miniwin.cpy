      $set constant UNICODE (0)
      $IF UNICODE = 0
       78 Suffix                value "A".
      $ELSE
       78 Suffix                value "W".
      $END

      *
      * API entry-points
      *
       78 BeginPaint        value "BeginPaint".
       78 CreateWindow      value "CreateWindow".
       78 CreateWindowEx    value "CreateWindowEx" & Suffix.
       78 CreateMDIWindow   value "CreateMDIWindow" & Suffix.
       78 DefWindowProc     value "DefWindowProc" & Suffix.
       78 DefMDIChildProc   value "DefMDIChildProc" & Suffix.
       78 DefFrameProc      value "DefFrameProc" & Suffix.
       78 DialogBoxParam    value "DialogBoxParam" & Suffix.
       78 DispatchMessage   value "DispatchMessage" & Suffix.
       78 DrawText          value "DrawText" & Suffix.
       78 EndDialog         value "EndDialog".
       78 EndPaint          value "EndPaint".
       78 FillRect          value "FillRect".
       78 FindWindow        value "FindWindow" & Suffix.
       78 GetClientRect     value "GetClientRect".
       78 GetMessage        value "GetMessage" & Suffix.
       78 GetStockObject    value "GetStockObject".
       78 GetMenu           value "GetMenu".
       78 GetSubMenu        value "GetSubMenu".
       78 LoadCursor        value "LoadCursor" & Suffix.
       78 LoadIcon          value "LoadIcon" & Suffix.
       78 PostMessage       value "PostMessage" & Suffix.
       78 PostQuitMessage   value "PostQuitMessage".
       78 RegisterClass     value "RegisterClass" & Suffix.
       78 ShowWindow        value "ShowWindow".
       78 TranslateMessage  value "TranslateMessage".
       78 TranslateMDISysAccel value "TranslateMDISysAccel".
       78 UpdateWindow      value "UpdateWindow".
       78 LoadLibrary       value "LoadLibrary" & Suffix.
       78 HINSTANCE-ERROR       value 32.
       78 FreeLibrary       value "FreeLibrary".
       78 GetOpenFileName   value "GetOpenFileName" & Suffix.
       78 GetSaveFileName   value "GetSaveFileName" & Suffix.
       78 FormatMessage     value "FormatMessage" & Suffix.
       78 MessageBox        value "MessageBox" & Suffix.
       78 GetVersionEx      value "GetVersionEx" & Suffix.
       78 SendMessage       value "SendMessage" & Suffix.
       78 GetClientRect     value "GetClientRect".
       78 MoveWindow        value "MoveWindow".
       78 WinHelp           value "WinHelp" & Suffix.
       78 GetLocaleInfo     value "GetLocaleInfo" & Suffix.
       78 lstrcpy           value "lstrcpy" & Suffix.
       78 lstrcat           value "lstrcat" & Suffix.
       78 lstrlen           value "lstrlen" & Suffix.
       78 ShellExecute      value "ShellExecute" & Suffix.
       78 GetCurrentDirectory value "GetCurrentDirectory" & Suffix.
       78 GetUserName       value "GetUserName" & Suffix.
       78 GetComputerName   value "GetComputerName" & Suffix.
       78 CreateProcess     value "CreateProcess" & Suffix.
       78 WaitForSingleObject value "WaitForSingleObject".
       78 GetExitCodeProcess value "GetExitCodeProcess".
       78 GetLastError       value "GetLastError".
       78 WritePrivateProfileString
          value "WritePrivateProfileString" & Suffix.
       78 WritePrivateProfileStringW
          value "WritePrivateProfileString" & "W".
       78 GetPrivateProfileString
          value "GetPrivateProfileString" & Suffix.

       *> API to access the registry.
       78 RegCreateKeyEx value "RegCreateKeyEx" & suffix.
       78 RegOpenKeyEx value "RegOpenKeyEx" & suffix.
       78 RegSetValueEx value "RegSetValueEx" & suffix.
       78 RegQueryInfoKey value "RegQueryInfoKey" & suffix.
       78 RegCloseKey value "RegCloseKey".
       78 RegQueryValueEx value "RegQueryValueEx" & suffix.

      *
      * Environment constants
      *
       78 Byte-Len              value 4.
       78 Dword-Len             value 4.
       78 Dword9-Len            value DWord-Len * 2.
       78 No-Of-Bits            value 32.
       78 Word-Len              value 4.  
       78 Word9-Len             value Word-Len * 2.

      *
      * Dialog COMMANDS
      *
       78 IDOK                  value 1.

