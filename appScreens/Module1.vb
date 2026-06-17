Imports System.IO
Imports Microsoft.Win32

Module Module1

    Public bTrace As Boolean = False
    Public profile As String = ""
    Public Const SupervisorRole As String = "SUPERVISOR"
    Public Const screenEventTo As String = "APTRA Advance NDC"
    Public currentScreen As String = ""
    Public lastUrl As String = ""
    Public isClosePage As Boolean = False
    Public isExpetionClosePageNDC As Boolean = False

    Private Function GetTraceLevelName(level As Integer) As String
        Select Case level
            Case 1
                Return "WARN"
            Case 2
                Return "ERROR"
            Case 3
                Return "DEBUG"
            Case Else
                Return "INFO"
        End Select
    End Function

    Private Function SanitizeTraceText(texto As String) As String
        If texto Is Nothing Then Return ""

        Dim limpio As String = texto
        limpio = limpio.Replace("APTRA Advance NDC", "NDC")
        limpio = limpio.Replace("APTRA", "NDC")

        Return limpio
    End Function

    Public Sub Trace(ByVal strTexto As String, Optional ByVal level As Integer = 0)
        Dim sTrace As String = ""

        Try
            sTrace = ReadIni("TRACE", "SCREENS", ConfigManager.ConfigFile)
            If sTrace = "TRUE" Then
                Dim logDir As String = "C:\appMain\log"
                If Not Directory.Exists(logDir) Then Directory.CreateDirectory(logDir)

                Dim prefix As String = Format(Now(), "dd-MM-yyyy HH:mm:ss.fff tt") &
                    "      [" & GetTraceLevelName(level) & "] [PID:" & Process.GetCurrentProcess().Id & "] "

                strTexto = prefix & SanitizeTraceText(strTexto) & vbCrLf
                File.AppendAllText(Path.Combine(logDir, Format(Now(), "yyyyMMdd") & "_appScreens.log"), strTexto)
            End If
        Catch ex As Exception
            Try
                File.AppendAllText("C:\appMain\log\trace_error.log", Format(Now(), "dd-MM-yyyy HH:mm:ss.fff tt") & "      [ERROR] " & SanitizeTraceText(ex.Message) & vbCrLf)
            Catch
            End Try
        End Try
    End Sub

    Public Function ObtenerArchivosMSGOrdenados(rutaCarpeta As String) As List(Of String)
        Dim archivosOrdenados As New List(Of String)

        If IO.Directory.Exists(rutaCarpeta) Then
            archivosOrdenados = IO.Directory.GetFiles(rutaCarpeta, "*.msg") _
            .OrderBy(Function(f) IO.File.GetCreationTime(f)) _
            .ToList()
        End If

        Return archivosOrdenados
    End Function

#Region "Tools INI"

    Declare Function WritePrivateProfileString Lib "kernel32" Alias "WritePrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As String, ByVal lpString As String, ByVal lpFileName As String) As Integer
    Declare Function GetPrivateProfileString Lib "kernel32" Alias "GetPrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As String, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal nSize As Integer, ByVal lpFileName As String) As Integer

    '*************************************Read INI************************************
    '********************************************************************************
    Public Function ReadIni(ByVal seccion As String, ByVal Campo As String, ByVal strFile As String) As String
        Dim ret As Integer
        Dim sRetVal As String
        Dim sRetur As String = ""
        '
        Try
            sRetVal = New String(Chr(0), 255)
            '
            ret = GetPrivateProfileString(seccion, Campo, "", sRetVal, Len(sRetVal), strFile)
            sRetur = Left(sRetVal, ret)
        Catch ex As Exception
            Trace("[ReadIni] [Error] " + ex.Message)
        End Try

        Return sRetur
    End Function

    '*
    '*************************************Escribe INI************************************
    '********************************************************************************
    Public Sub writeINI(ByVal seccion As String, ByVal Campo As String, ByVal valor As String, ByVal strFile As String)
        Dim intAnswer As Long
        Try
            intAnswer = WritePrivateProfileString(seccion, Campo, valor, strFile)
        Catch ex As Exception
            Trace("[writeINI] [Error] " + ex.Message)
        End Try

    End Sub



#End Region

#Region "Fix IE"
    Public Sub SetBrowserFeatureControl()
        ' http://msdn.microsoft.com/en-us/library/ee330720(v=vs.85).aspx

        ' FeatureControl settings are per-process
        Dim fileName = System.IO.Path.GetFileName(Process.GetCurrentProcess().MainModule.FileName)

        ' make the control is not running inside Visual Studio Designer
        If [String].Compare(fileName, "devenv.exe", True) = 0 OrElse [String].Compare(fileName, "XDesProc.exe", True) = 0 Then
            Return
        End If

        SetBrowserFeatureControlKey("FEATURE_BROWSER_EMULATION", fileName, GetBrowserEmulationMode())
        ' Webpages containing standards-based !DOCTYPE directives are displayed in IE10 Standards mode.
        SetBrowserFeatureControlKey("FEATURE_AJAX_CONNECTIONEVENTS", fileName, 1)
        SetBrowserFeatureControlKey("FEATURE_ENABLE_CLIPCHILDREN_OPTIMIZATION", fileName, 1)
        SetBrowserFeatureControlKey("FEATURE_MANAGE_SCRIPT_CIRCULAR_REFS", fileName, 1)
        SetBrowserFeatureControlKey("FEATURE_DOMSTORAGE ", fileName, 1)
        SetBrowserFeatureControlKey("FEATURE_GPU_RENDERING ", fileName, 1)
        SetBrowserFeatureControlKey("FEATURE_IVIEWOBJECTDRAW_DMLT9_WITH_GDI  ", fileName, 0)
        SetBrowserFeatureControlKey("FEATURE_DISABLE_LEGACY_COMPRESSION", fileName, 1)
        SetBrowserFeatureControlKey("FEATURE_LOCALMACHINE_LOCKDOWN", fileName, 0)
        SetBrowserFeatureControlKey("FEATURE_BLOCK_LMZ_OBJECT", fileName, 0)
        SetBrowserFeatureControlKey("FEATURE_BLOCK_LMZ_SCRIPT", fileName, 0)
        SetBrowserFeatureControlKey("FEATURE_DISABLE_NAVIGATION_SOUNDS", fileName, 1)
        SetBrowserFeatureControlKey("FEATURE_SCRIPTURL_MITIGATION", fileName, 1)
        SetBrowserFeatureControlKey("FEATURE_SPELLCHECKING", fileName, 0)
        SetBrowserFeatureControlKey("FEATURE_STATUS_BAR_THROTTLING", fileName, 1)
        SetBrowserFeatureControlKey("FEATURE_TABBED_BROWSING", fileName, 1)
        SetBrowserFeatureControlKey("FEATURE_VALIDATE_NAVIGATE_URL", fileName, 1)
        SetBrowserFeatureControlKey("FEATURE_WEBOC_DOCUMENT_ZOOM", fileName, 1)
        SetBrowserFeatureControlKey("FEATURE_WEBOC_POPUPMANAGEMENT", fileName, 0)
        SetBrowserFeatureControlKey("FEATURE_WEBOC_MOVESIZECHILD", fileName, 1)
        SetBrowserFeatureControlKey("FEATURE_ADDON_MANAGEMENT", fileName, 0)
        SetBrowserFeatureControlKey("FEATURE_WEBSOCKET", fileName, 1)
        SetBrowserFeatureControlKey("FEATURE_WINDOW_RESTRICTIONS ", fileName, 0)
        SetBrowserFeatureControlKey("FEATURE_XMLHTTP", fileName, 1)
    End Sub

    Private Function GetBrowserEmulationMode() As UInt32
        Dim browserVersion As Integer = 7
        Using ieKey = Registry.LocalMachine.OpenSubKey("SOFTWARE\Microsoft\Internet Explorer", RegistryKeyPermissionCheck.ReadSubTree, System.Security.AccessControl.RegistryRights.QueryValues)
            Dim version = ieKey.GetValue("svcVersion")
            If Nothing = version Then
                version = ieKey.GetValue("Version")
                If Nothing = version Then
                    Throw New ApplicationException("Microsoft Internet Explorer Is required!")
                End If
            End If
            Integer.TryParse(version.ToString().Split("."c)(0), browserVersion)
        End Using

        Dim mode As UInt32 = 10000
        ' Internet Explorer 10. Webpages containing standards-based !DOCTYPE directives are displayed in IE10 Standards mode. Default value for Internet Explorer 10.
        Select Case browserVersion
            Case 7
                mode = 7000
                ' Webpages containing standards-based !DOCTYPE directives are displayed in IE7 Standards mode. Default value for applications hosting the WebBrowser Control.
                Exit Select
            Case 8
                mode = 8000
                ' Webpages containing standards-based !DOCTYPE directives are displayed in IE8 mode. Default value for Internet Explorer 8
                Exit Select
            Case 9
                mode = 9000
                ' Internet Explorer 9. Webpages containing standards-based !DOCTYPE directives are displayed in IE9 mode. Default value for Internet Explorer 9.
                Exit Select
            Case 10
                mode = 10000
                ' Internet Explorer 10. Webpages containing standards-based !DOCTYPE directives are displayed in IE9 mode. Default value for Internet Explorer 9.
                Exit Select
            Case 11
                mode = 11001
                ' Internet Explorer 11. Webpages containing standards-based !DOCTYPE directives are displayed in IE9 mode. Default value for Internet Explorer 9.
                Exit Select
            Case Else
                ' use IE10 mode by default
                Exit Select
        End Select

        Return mode
    End Function

    Private Sub SetBrowserFeatureControlKey(ByVal feature As String, ByVal appName As String, ByVal value As UInteger)
        'If Environment.Is64BitOperatingSystem Then

        '    Using key = Registry.LocalMachine.CreateSubKey([String].Concat("Software\Wow6432Node\Microsoft\Internet Explorer\Main\FeatureControl\", feature), RegistryKeyPermissionCheck.ReadWriteSubTree)
        '        key.SetValue(appName, DirectCast(value, UInt32), RegistryValueKind.DWord)
        '    End Using
        'End If


        Using key = Registry.CurrentUser.CreateSubKey([String].Concat("Software\Microsoft\Internet Explorer\Main\FeatureControl\", feature), RegistryKeyPermissionCheck.ReadWriteSubTree)
            key.SetValue(appName, DirectCast(value, UInt32), RegistryValueKind.DWord)
        End Using
    End Sub
#End Region


End Module
