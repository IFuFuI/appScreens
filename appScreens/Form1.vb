Imports System.IO
Imports System.Net.Mime.MediaTypeNames
Imports System.Security.Permissions
Imports System.Threading.Tasks

<PermissionSet(SecurityAction.Demand, Name:="FullTrust")>
<System.Runtime.InteropServices.ComVisibleAttribute(True)>
Public Class Form1

    ' sCurrent: Pagina actual
    Dim sCurrent As String = ""
    ' sCurrentData: Datos NDC de la pagina actula
    Dim sCurrentData As String = ""
    ' pageException: pagina de Exepción
    Dim pageException As String = String.Empty
    ' fdkBack: variable para saber que FDK es de regreso
    Dim fdkBack As String = String.Empty
    ' sErrorImpresora: Error de impresora
    Dim sErrorImpresora As String = String.Empty
    ' sErrorCdm: Error de CDM (dispensador)
    Dim sErrorCdm As String = String.Empty
    ' isHostError: Error de línea host/NDC, evitar mostrar retiro si se detecta en transacción
    Dim isHostError As Boolean = False

    ' dllInterfaceNdc: Clase para manejar interface NDC
    Dim dllInterfaceNdc As New appInterfaceNDC.Class1

    ' ndcH: Código de operación cadena NDC H
    Dim ndcH As String
    ' ndcF: Código de operación cadena NDC F
    Dim ndcF As String
    ' ndcG: Código de operación cadena NDC G
    Dim ndcG As String
    ' ndcI: Código de operación cadena NDC I
    Dim ndcI As String
    ' ndcJ: Código de operación cadena NDC J
    Dim ndcJ As String
    ' ndcO: Código de operación cadena NDC O
    Dim ndcO As String
    ' ndcD: Código de operación cadena NDC D
    Dim ndcD As String
    ' ndcE: Código de operación cadena NDC E
    Dim ndcE As String
    ' ndcB: Código de operación cadena NDC B
    Dim ndcB As String
    ' ndcC: Código de operación cadena NDC C
    Dim ndcC As String
    ' ndcK: Código de operación cadena NDC K
    Dim ndcK As String
    ' ndcL: Código de operación cadena NDC L
    Dim ndcL As String
    ' ndcM: Código de operación cadena NDC M
    Dim ndcM As String
    ' ndcN: Código de operación cadena NDC N
    Dim ndcN As String
    ' sComision: Código de operación cadena NDC comision
    Dim sComision As String
    ' bNDCPageActive: Indica que el NDC está mostrando una pantalla, ignorar 701
    Dim bNDCPageActive As Boolean = False
    ' sExp852OriginalUrl: URL original (menu) cuando se redirige a exp-852
    Dim sExp852OriginalUrl As String = ""
    ' sExp850OriginalUrl: URL original (menu) cuando se redirige a exp-850 por error de impresora
    Dim sExp850OriginalUrl As String = ""
    ' sLastMenuUrl: Último menú principal mostrado (puede ser menu.html, menuWO-504.html, etc.)
    Dim sLastMenuUrl As String = ""

    ' ============== Win32 API para región transparente ==============
    Private Declare Function CreateRectRgn Lib "gdi32" (ByVal X1 As Integer, ByVal Y1 As Integer, ByVal X2 As Integer, ByVal Y2 As Integer) As IntPtr
    Private Declare Function CombineRgn Lib "gdi32" (ByVal hDestRgn As IntPtr, ByVal hSrcRgn1 As IntPtr, ByVal hSrcRgn2 As IntPtr, ByVal nCombineMode As Integer) As Integer
    Private Declare Function SetWindowRgn Lib "user32" (ByVal hWnd As IntPtr, ByVal hRgn As IntPtr, ByVal bRedraw As Boolean) As Integer
    Private Declare Function DeleteObject Lib "gdi32" (ByVal hObject As IntPtr) As Boolean
    Private Const RGN_DIFF As Integer = 4
    Private transparentRegionApplied As Boolean = False
    ' ================================================================

    Private Sub frmsstWait_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Dim sheight As Integer = 768
        Dim swidgth As Integer = 1024

        Trace("Init appScreens V06.04")
        writeINI("LG", "RESULT", "", ConfigManager.WorkFile)
        writeINI("SCREENS", "NUM", "", ConfigManager.WorkFile)
        writeINI("SCREENS", "DATA", "", ConfigManager.WorkFile)
        writeINI("NDC", "EVENT", "", ConfigManager.strRutaInterface)
        writeINI("NDC", "EVENT_SUPER", "", ConfigManager.strRutaInterface)
        writeINI("NDC", "SUPER", "", ConfigManager.strRutaInterface)
        writeINI("NDC", "READ", "", ConfigManager.strRutaInterface)

        Try
            Dim sheightStr = ReadIni("PARAM", "HEIGHT", ConfigManager.ConfigAtmClientFile)
            If Not Integer.TryParse(sheightStr, sheight) Then sheight = 768

            Dim sWigthStr = ReadIni("PARAM", "WIDGTH", ConfigManager.ConfigAtmClientFile)
            If Not Integer.TryParse(sWigthStr, swidgth) Then swidgth = 1024

            Me.Size = New Size(CInt(swidgth), CInt(sheight))

            Dim sPotition As String = "1"
            sPotition = ReadIni("PARAM", "SCREEN_POTITION", ConfigManager.ConfigAtmClientFile)

            If sPotition = "1" Then Me.Location = New Point(0, 0)
            If sPotition = "2" Then Me.Location = New Point(1024, 0)
            If sPotition = "3" Then Me.Location = New Point(-1024, 0)

        Catch ex As Exception
            Trace("Error set parameters")
        End Try

        Try
            WebBrowser1.ScrollBarsEnabled = False
            WebBrowser1.ScriptErrorsSuppressed() = True
            WebBrowser1.Height = CInt(sheight)
            WebBrowser1.Width = CInt(swidgth)
            WebBrowser1.AllowWebBrowserDrop = False
            WebBrowser1.IsWebBrowserContextMenuEnabled = False
            WebBrowser1.WebBrowserShortcutsEnabled = False
            WebBrowser1.ObjectForScripting = Me

            SetBrowserFeatureControl()

            AddHandler WebBrowser1.DocumentCompleted, AddressOf WebBrowser1_DocumentCompleted
        Catch ex As Exception
            Trace("Error init browser")
        End Try

        tmInterfasSuper.Enabled = True
        Me.Hide()
    End Sub

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        Dim sValue As String
        Dim sData As String
        Dim sDataEnable As String = String.Empty

        Dim sPage As String = String.Empty

        Timer1.Enabled = False

        sData = ReadIni("SCREENS", "DATA", ConfigManager.WorkFile)
        sValue = ReadIni("SCREENS", "NUM", ConfigManager.WorkFile)

        If sValue <> "" Then
            ClickEnVentana.MoverMouse00()
            tmMsgDevices.Enabled = False

            If sValue <> sCurrent Then
                Trace("[Timer1_Tick] [Info] Screen: " + sValue)

                If sValue = "701" AndAlso bNDCPageActive Then
                    Trace("701 ignorado - pantalla NDC activa")
                Else
                    ProcesarPantalla(sValue)
                End If

            End If

            writeINI("SCREENS", "NUM", "", ConfigManager.WorkFile)
        End If

        Try
            If sData <> "" Then
                ClickEnVentana.MoverMouse00()
                tmMsgDevices.Enabled = False
                If sData <> sCurrentData Then
                    sCurrentData = sData
                    If sCurrentData = "KEYPIN" Then sCurrentData = ""
                    If "0123456789*#".Contains(sCurrentData) Then sCurrentData = ""

                    Trace("DATA 2: " + sData)
                    Trace("DATA 2 currentScreen: " + currentScreen)

                    Select Case sData
                        Case "KEYPIN"
                            Trace("sCurrent: " + sCurrent)
                            sDataEnable = ReadIni(sCurrent, "DATA", ConfigManager.ScreensFile)
                            Trace("sDataEnable: " + sDataEnable)
                            If sDataEnable = "PIN" Then
                                WebBrowser1.Document.InvokeScript("recibeData")
                            End If
                        Case Else
                            Trace("DATA desconocido: " + sData + " screen: " + currentScreen)
                    End Select
                End If

                writeINI("SCREENS", "DATA", "", ConfigManager.WorkFile)
            End If
        Catch ex As Exception
            Trace("Error Screens: " + ex.Message)
        End Try

        Timer1.Enabled = True
    End Sub

    Public Sub wb_beep()
        Try
        Catch ex As Exception
            Trace("Error en wb_beep: " & ex.Message)
        End Try
    End Sub

    Public Sub wb_RevisaEstadusDevices()
        Try
            Trace("wb_RevisaEstadusDevices called")
            sErrorImpresora = ""
            Task.Run(Sub()
                         Try
                             Dim anyZero As Boolean = AreAnyCassettesEmpty()
                             Trace("AreAnyCassettesEmpty returned: " & anyZero)
                             If Me IsNot Nothing AndAlso Not Me.IsDisposed Then
                                 Me.BeginInvoke(Sub()
                                                    Try
                                                        If WebBrowser1 Is Nothing OrElse WebBrowser1.Document Is Nothing Then
                                                            Trace("WebBrowser1.Document is Nothing in wb_RevisaEstadusDevices")
                                                            Return
                                                        End If

                                                        If anyZero Then
                                                            Trace("Calling hideBtnRetiro from wb_RevisaEstadusDevices")
                                                            WebBrowser1.Document.InvokeScript("hideBtnRetiro")
                                                        Else
                                                            Trace("Calling showBtnRetiro from wb_RevisaEstadusDevices")
                                                            WebBrowser1.Document.InvokeScript("showBtnRetiro")
                                                        End If
                                                    Catch ex As Exception
                                                        Trace("Error invoking cassette script: " & ex.Message)
                                                    End Try
                                                End Sub)
                             End If
                         Catch ex As Exception
                             Trace("Error tarea comprobación cassettes: " & ex.Message)
                         End Try
                     End Sub)

            tmMsgDevices.Enabled = True
        Catch ex As Exception
            Trace("Error en wb_RevisaEstadusDevices: " & ex.Message)
        End Try
    End Sub

    Private Function AreAnyCassettesEmpty() As Boolean
        Try
            Dim cass1 As String = ReadIni("CDM_COUNTER", "CASSETTE1", ConfigManager.workFileDevices).Trim()
            Dim cass2 As String = ReadIni("CDM_COUNTER", "CASSETTE2", ConfigManager.workFileDevices).Trim()
            Dim cass3 As String = ReadIni("CDM_COUNTER", "CASSETTE3", ConfigManager.workFileDevices).Trim()
            Dim cass4 As String = ReadIni("CDM_COUNTER", "CASSETTE4", ConfigManager.workFileDevices).Trim()

            Return (cass1 = "0" OrElse cass2 = "0" OrElse cass3 = "0" OrElse cass4 = "0" OrElse IsCdmError())
        Catch ex As Exception
            Trace("Error AreAnyCassettesEmpty: " & ex.Message)
            Return False
        End Try
    End Function

    Private Function IsPrinterError() As Boolean
        Try
            Dim status As String = ReadIni("PTR STATUS", "fwDevice", ConfigManager.workFileDevices).Trim()
            Return status.ToUpper() = "HWERROR"
        Catch ex As Exception
            Trace("Error IsPrinterError: " & ex.Message)
            Return False
        End Try
    End Function

    Private Function IsCdmError() As Boolean
        Try
            Dim status As String = ReadIni("CDM STATUS", "fwDevice", ConfigManager.workFileDevices).Trim().ToUpper()

            ' Validación agresiva para evitar parpadeos, asume error si no es ONLINE/OK/READY
            If status <> "ONLINE" AndAlso status <> "OK" AndAlso status <> "READY" AndAlso status <> "" Then
                Return True
            End If

            Return False
        Catch ex As Exception
            Trace("Error IsCdmError: " & ex.Message)
            Return False
        End Try
    End Function

    Public Sub clickPage(sPotition As String)
        Try
            dllInterfaceNdc.showScreenNDC(300)
        Catch ex As Exception
            Trace("Error al mandar interface")
        End Try

        sCurrent = ""
        sCurrentData = ""
        Me.Hide()

        Threading.Thread.Sleep(300)

        Select Case sPotition
            Case "1"
                ClickEnVentana.SimularClickEnVentana(screenEventTo, 177, 296)
            Case "2"
                ClickEnVentana.SimularClickEnVentana(screenEventTo, 177, 440)
            Case "3"
                ClickEnVentana.SimularClickEnVentana(screenEventTo, 177, 574)
            Case "4"
                ClickEnVentana.SimularClickEnVentana(screenEventTo, 177, 713)
            Case "5"
                ClickEnVentana.SimularClickEnVentana(screenEventTo, 850, 296)
            Case "6"
                ClickEnVentana.SimularClickEnVentana(screenEventTo, 850, 440)
            Case "7"
                ClickEnVentana.SimularClickEnVentana(screenEventTo, 850, 574)
            Case "8"
                ClickEnVentana.SimularClickEnVentana(screenEventTo, 850, 713)
        End Select

        If pageException <> "" And pageException = sPotition Then
            pageException = ""
            sExp850OriginalUrl = ""
            sExp852OriginalUrl = ""
            bNDCPageActive = False
            Trace("Ejectua excepción FDK.. navega a currentScreen: " + currentScreen)
            WebBrowser1.Navigate(currentScreen)
            Me.Show()
        End If

        If fdkBack <> "" And fdkBack = sPotition Then
            fdkBack = ""
            Trace("Ejectua back FDK.. navega a lastScreen: " + lastUrl)
            WebBrowser1.Navigate(lastUrl)
            Me.Show()
        End If
    End Sub

    Public Function getVar(sVar As String) As String
        Dim sretur As String = String.Empty

        Select Case sVar
            Case "B" : sretur = ndcB
            Case "C" : sretur = ndcC
            Case "D" : sretur = ndcD
            Case "E" : sretur = ndcE
            Case "F" : sretur = ndcF
            Case "G" : sretur = ndcG
            Case "H" : sretur = ndcH
            Case "I" : sretur = ndcI
            Case "J" : sretur = ndcJ
            Case "O" : sretur = ndcO
            Case "K" : sretur = ndcK
            Case "L" : sretur = ndcL
            Case "M" : sretur = ndcM
            Case "N" : sretur = ndcN
            Case "comision" : sretur = sComision
            Case "t1"
                Try
                    Dim texto As String = ndcG
                    Dim partes() As String = texto.Split(New Char() {" "c}, StringSplitOptions.RemoveEmptyEntries)
                    sretur = partes(0)
                Catch ex As Exception
                    Trace("Error getVar parsear t1: " + ex.Message)
                End Try
            Case "t2"
                Try
                    Dim texto As String = ndcG
                    Dim partes() As String = texto.Split(New Char() {" "c}, StringSplitOptions.RemoveEmptyEntries)
                    sretur = partes(1)
                Catch ex As Exception
                    Trace("Error getVar parsear t2: " + ex.Message)
                End Try
            Case Else
                sretur = ""
        End Select

        Trace("getVar: " + sretur)
        Return sretur
    End Function

    Public Sub setBack()
        Try
            Trace("SetBack")
            WebBrowser1.Navigate(currentScreen)
            Me.Show()
        Catch ex As Exception
            Trace("Error setBack: " + ex.Message)
        End Try
    End Sub

    Private Sub tmInterfasSuper_Tick(sender As Object, e As EventArgs) Handles tmInterfasSuper.Tick
        Dim sDato As String
        Dim sSuper As String

        tmInterfasSuper.Enabled = False

        sDato = ReadIni("NDC", "EVENT", ConfigManager.strRutaInterface)
        sSuper = ReadIni("NDC", "SUPER", ConfigManager.strRutaInterface)

        If sDato = "1" Or sSuper = SupervisorRole Then
            Trace("Entra a configuracion sDato: " + sDato)
            Trace("Entra a configuracion sSuper: " + sSuper)

            writeINI("NDC", "EVENT", "", ConfigManager.strRutaInterface)
            writeINI("NDC", "SUPER", "", ConfigManager.strRutaInterface)

            ocultarPantalla()

            Try
                writeINI("NDC", "EVENT", "", ConfigManager.strRutaInterface)

                Dim psi As New ProcessStartInfo()
                psi.FileName = "wscript.exe"
                psi.Arguments = """" & "C:\appMain\application\startConfig.vbs" & """"
                psi.WindowStyle = ProcessWindowStyle.Hidden
                psi.CreateNoWindow = True
                psi.UseShellExecute = False

                Process.Start(psi)

                Trace("Se abre configuracion y se oculta appscreen")
            Catch ex As Exception
                Trace("Error al abrir configuracion: " + ex.Message)
            End Try
        End If

        If sDato = "ENDSUPER" Then
            Trace("Termina supervisor")
            writeINI("NDC", "EVENT", "", ConfigManager.strRutaInterface)
            killSupervisor()
        End If

        tmInterfasSuper.Enabled = True
    End Sub

    Private Sub checkMsg_Tick(sender As Object, e As EventArgs) Handles checkMsg.Tick
        checkMsg.Enabled = False
        procesaDatosNDC()
        checkMsg.Enabled = True
    End Sub

    Private Sub procesaDatosNDC()
        Dim archivos = ObtenerArchivosMSGOrdenados(ConfigManager.rutaMsg)
        Dim sUrl As String = String.Empty
        Dim bPantalla As Boolean = False
        Dim sImagen As String = String.Empty
        Dim sPage As String = ""
        Dim contenido As String

        For Each archivo In archivos
            Try
                contenido = IO.File.ReadAllText(archivo)
            Catch ex As Exception
                Trace("Error leer contenido: " + ex.Message)
                Exit Sub
            End Try

            If String.IsNullOrWhiteSpace(contenido) Then
                Try
                    File.Delete(archivo)
                Catch ex As Exception
                    Trace("Error al borrar archivo")
                End Try
                Continue For
            End If

            tmOut.Enabled = False
            writeINI("APP", "TIMEOUT", "", ConfigManager.WorkFile)
            Dim ndc = MensajeNDC.Parse(contenido)

            Trace("TipoMensaje: " & ndc.TipoMensaje)
            Trace("Secuencia: " & ndc.Secuencia)
            Trace("CodigoTransaccion: " & ndc.CodigoTransaccion)
            Trace("Pantalla: " & ndc.Pantalla)
            Trace("Layout: " & ndc.Layout)

            Trace("Msg NDC: " & contenido)

            Dim esError As Boolean = False

            If contenido.Contains("CODIGO RESPUESTA") AndAlso Not contenido.Contains("CODIGO RESPUESTA 000") Then
                Trace("Encontro un error regresado por Host, no muestra gracias: ")
                esError = True
            End If

            If contenido.Contains("TRANSACCION RECHAZADA") OrElse (contenido.Contains("CODIGO RESPUESTA") AndAlso Not contenido.Contains("CODIGO RESPUESTA 000")) Then
                Trace("Encontro un error regresado por Host, no muestra gracias: ")
                esError = True
            End If

            If contenido.Contains("CODIGO DE ERRORR") Then
                Trace("Encontro un error CODIGO DE ERRORR regresado por Host, no muestra gracias: ")
                esError = True
            End If

            If esError Then
                isExpetionClosePageNDC = True
                isHostError = True
                Trace("procesaDatosNDC: host error detectado => isHostError=True")
                If WebBrowser1 IsNot Nothing AndAlso WebBrowser1.Document IsNot Nothing Then
                    Try
                        WebBrowser1.Document.InvokeScript("hideBtnRetiro")
                    Catch ex As Exception
                        Trace("Error invoking hideBtnRetiro desde procesaDatosNDC: " & ex.Message)
                    End Try
                End If
                Me.Hide()
            End If

            If contenido.Contains("MONTO O DENOMINACION NO PERMITIDO") Then
                Trace("MONTO O DENOMINACION NO PERMITIDO: PW0003")
                sPage = "PW0003"
                sUrl = ReadIni(sPage, "PAGE", ConfigManager.ScreensFile)
                Trace("sURL Msg: " + sUrl)
            End If

            If ndc.Layout <> "" Then
                sPage = ndc.Layout
                bPantalla = True
                sPage = sPage.Replace("P", "")
                sUrl = ReadIni(sPage, "PAGE", ConfigManager.ScreensFile)
                Trace("sURL Msg: " + sUrl)
            End If

            If sUrl <> "" Then
                ndcD = ndc.DatoD
                ndcE = ndc.DatoE
                ndcF = ndc.DatoF
                ndcG = ndc.DatoG
                ndcH = ndc.DatoH
                ndcI = ndc.DatoI
                ndcJ = ndc.DatoJ
                ndcB = ndc.DatoO
                ndcC = ndc.DatoB
                ndcO = ndc.DatoC
                ndcK = ndc.DatoK
                ndcL = ndc.DatoL
                ndcM = ndc.DatoM
                ndcN = ndc.DatoN
                sComision = ndc.MontoFO

                If ndcD <> "" Then Trace("D: " & ndcD)
                If ndcE <> "" Then Trace("E: " & ndcE)
                If ndcF <> "" Then Trace("F: " & ndcF)
                If ndcG <> "" Then Trace("G: " & ndcG)
                If ndcH <> "" Then Trace("H: " & ndcH)
                If ndcI <> "" Then Trace("I: " & ndcI)
                If ndcJ <> "" Then Trace("J: " & ndcJ)
                If ndcO <> "" Then Trace("O: " & ndcO)
                If ndcB <> "" Then Trace("B: " & ndcB)
                If ndcC <> "" Then Trace("C: " & ndcC)
                If ndcK <> "" Then Trace("C: " & ndcK)
                If ndcL <> "" Then Trace("C: " & ndcL)
                If ndcM <> "" Then Trace("C: " & ndcM)
                If ndcN <> "" Then Trace("C: " & ndcN)
                If sComision <> "" Then Trace("Comisión: " & sComision)
            End If

            Try
                If File.Exists(archivo) Then
                    File.Delete(archivo)
                End If
            Catch ex As Exception
                Trace("Error al borrar archivo")
            End Try
        Next

        If sUrl <> "" Then
            ' >>> PREVENCIÓN DE PARPADEO <<<
            ' Si hubo error (isExpetionClosePageNDC = True), NO hacemos Show()
            ' Mantenemos oculto un segundo hasta que APTRA mande la orden 852.
            If Not isExpetionClosePageNDC Then
                Me.Show()
            End If

            Trace("Muestra Pantalla DatosNDC")
            ClickEnVentana.MoverMouse00()
            Dim sClose As String = String.Empty

            sClose = ReadIni(sPage, "CLOSE", ConfigManager.ScreensFile)

            If sClose = "TRUE" Then
                isClosePage = True
            Else
                isClosePage = False
            End If

            tmOut.Enabled = True
            lastUrl = currentScreen
            currentScreen = sUrl
            bNDCPageActive = True

            Dim sUrlLower As String = sUrl.ToLower()
            Dim esPaginaMenu As Boolean = sUrlLower.Contains("menu") AndAlso Not sUrlLower.Contains("menumore")

            If esPaginaMenu Then
                sLastMenuUrl = sUrl

                If AreAnyCassettesEmpty() OrElse IsCdmError() Then
                    Trace("Sin efectivo o Error CDM - redirigiendo a exp-852 proactivamente")
                    sExp852OriginalUrl = sUrl
                    sExp850OriginalUrl = ""

                    currentScreen = sUrl
                    pageException = "8"
                    isExpetionClosePageNDC = False

                    Dim sUrlBase As String = sUrl.Substring(0, sUrl.LastIndexOf("\") + 1)
                    sUrl = sUrlBase & "exp-852.html"
                ElseIf IsPrinterError() Then
                    Trace("Error impresora - redirigiendo a exp-850")
                    sExp850OriginalUrl = sUrl
                    currentScreen = sUrl
                    pageException = "8"
                    isExpetionClosePageNDC = False

                    Dim sUrlBase850 As String = sUrl.Substring(0, sUrl.LastIndexOf("\") + 1)
                    sUrl = sUrlBase850 & "exp-850.html"
                Else
                    sExp852OriginalUrl = ""
                    sExp850OriginalUrl = ""
                End If
            Else
                sExp852OriginalUrl = ""
                sExp850OriginalUrl = ""
            End If

            Trace("Navega page Url DatosNDC: " + sUrl)
            WebBrowser1.Navigate(sUrl)
        Else
            sImagen = ReadIni(sPage, "PIC", ConfigManager.ScreensFile)
            If sImagen <> "" Then
                Try
                    dllInterfaceNdc.showScreenNDC(CInt(sImagen))
                    Trace("Muestra Pantalla PIC")
                    ocultarPantalla()
                Catch ex As Exception
                    Trace("Error al mandar interface en PIC " + ex.Message)
                End Try
            Else
                If bPantalla = True Then
                    ocultarPantalla()
                End If
            End If
        End If
    End Sub

    Private Sub ProcesarPantalla(sValue As String)
        Dim sUrl As String = String.Empty
        Dim sImagen As String = String.Empty
        Dim sDataEnable As String = String.Empty
        Dim sIdioma As String

        If sValue = "850" AndAlso sExp850OriginalUrl <> "" Then
            Trace("850 ignorado - exp-850 ya activo proactivamente")
            isExpetionClosePageNDC = False
            Exit Sub
        End If

        If sValue = "852" AndAlso sExp852OriginalUrl <> "" Then
            Trace("852 ignorado - exp-852 ya activo proactivamente")
            isExpetionClosePageNDC = False
            Exit Sub
        End If

        pageException = ""

        If sValue = "welcome" Then
            sValue = "500"
            bNDCPageActive = False

            If Not Me.Visible Then
                Me.Visible = True
            End If
        End If

        If sValue = "513" OrElse sValue = "500" Then
            bNDCPageActive = False
            sExp850OriginalUrl = ""
            sExp852OriginalUrl = ""
            sErrorImpresora = ""
            sErrorCdm = ""
            isHostError = False
            Trace("ProcesarPantalla: clear isHostError (pantalla 513/500)")
            If WebBrowser1 IsNot Nothing AndAlso WebBrowser1.Document IsNot Nothing Then
                Try
                    WebBrowser1.Document.InvokeScript("showBtnRetiro")
                Catch ex As Exception
                    Trace("Error invoking showBtnRetiro desde ProcesarPantalla: " & ex.Message)
                End Try
            End If
        End If

        ' >>> EVITA QUE SE OCULTE LA PANTALLA CUANDO LLEGA EL CÓDIGO 850 / 852 <<<
        If sValue = "850" OrElse sValue = "852" Then
            isExpetionClosePageNDC = False
        End If

        If isExpetionClosePageNDC Then
            Trace("Es Close por excepcion NDC oculata pantalla")
            isExpetionClosePageNDC = False
            Me.Hide()
            Exit Sub
        End If

        If sValue = "513" And isClosePage Then
            Trace("Es Close y ya tiene una pantalla de salida")
            isClosePage = False
            Exit Sub
        End If

        Try
            sUrl = ReadIni(sValue, "PAGE", ConfigManager.ScreensFile)

            Try
                Dim sWB As String = WebBrowser1.Url.ToString.Replace("file:///", "")
                If Me.Visible Then
                    If sUrl = sWB Then
                        Exit Sub
                    End If
                End If
            Catch ex As Exception
                Trace("Error al comparar paginas: " + ex.Message)
            End Try

            pageException = ReadIni(sValue, "FDKEXP", ConfigManager.ScreensFile)
            fdkBack = ReadIni(sValue, "BACKFDK", ConfigManager.ScreensFile)
            Trace("Page: " + sUrl)

            If pageException <> "" Then Trace("Pagina de excepcion: fdk" + pageException)
            If fdkBack <> "" Then Trace("Boton de regreso: fdk" + fdkBack)

            tmOut.Enabled = False
            writeINI("APP", "TIMEOUT", "", ConfigManager.WorkFile)

            If sUrl <> "" Then
                sCurrent = sValue
                sIdioma = ReadIni("LG", "RESULT", ConfigManager.WorkFile)
                sDataEnable = ReadIni(sValue, "DATA", ConfigManager.ScreensFile)

                If sIdioma <> "" Then
                    Dim iPoint As Integer
                    Dim sTU As String = ""

                    iPoint = sUrl.IndexOf(".")

                    If iPoint > 0 Then
                        sTU = Mid(sUrl, 1, iPoint)
                        sUrl = sTU + "_" + sIdioma + ".html"
                    End If
                End If

                ClickEnVentana.MoverMouse00()
                sCurrentData = ""
                If sValue <> "500" Then
                    tmOut.Enabled = True
                End If

                If pageException = "" Then
                    If Not (sUrl.Contains("wait") Or sUrl.Contains("read")) Then
                        lastUrl = currentScreen
                        currentScreen = sUrl
                    End If
                End If

                Dim sUrlFinal As String = sUrl
                If sValue = "701" AndAlso sLastMenuUrl <> "" AndAlso sUrl.ToLower().EndsWith("menu.html") Then
                    sUrlFinal = sLastMenuUrl
                    Trace("701 redirigido a menú correcto: " & sUrlFinal)
                End If

                Trace("Navega page Url: " + sUrlFinal)
                bNDCPageActive = False
                WebBrowser1.Navigate(sUrlFinal)
                Me.Show()
            Else
                sImagen = ReadIni(sValue, "PIC", ConfigManager.ScreensFile)

                If sImagen <> "" Then
                    Try
                        dllInterfaceNdc.showScreenNDC(CInt(sImagen))
                    Catch ex As Exception
                        Trace("Error al mandar interface en PIC")
                    End Try
                Else
                    If sValue = "701" Then
                        Trace("Pagina 701 anidada")
                    Else
                        If sValue <> "back" Then
                            If sValue <> "hide" Then Trace("Pagina No se encuentra")
                            sValue = "hide"
                        End If
                    End If
                End If
            End If

            If sValue = "hide" Then
                ocultarPantalla()
            Else
                If sValue = "back" Then
                    Trace("Muestra página actual")
                    If currentScreen <> "" Then
                        WebBrowser1.Navigate(currentScreen)
                    Else
                        Me.Hide()
                    End If
                End If
                Me.Show()
            End If
        Catch ex As Exception
            Trace("Error ProcesarPantalla: " + ex.Message)
        End Try
    End Sub

    Private Sub ocultarPantalla()
        Trace("Oculta Pantalla")
        tmMsgDevices.Enabled = False
        Me.Hide()
        sCurrent = ""
        sCurrentData = ""
    End Sub

    Private Sub tmOut_Tick(sender As Object, e As EventArgs) Handles tmOut.Tick
        tmOut.Enabled = False
        Trace("TimeOut")
        Me.Hide()
        writeINI("APP", "TIMEOUT", "ON", ConfigManager.WorkFile)
    End Sub

    Private Sub Form1_KeyDown(sender As Object, e As KeyEventArgs) Handles Me.KeyDown
        If e.Control AndAlso e.KeyCode = Keys.S Then
            e.Handled = True
            Try
                Trace("Termina Applicaion")
                Dim exeWait As String = "C:\appMain\application\DSC Kill.cmd"
                Process.Start(exeWait)
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub tmMsgDevices_Tick(sender As Object, e As EventArgs) Handles tmMsgDevices.Tick
        tmMsgDevices.Enabled = False

        checkEstusImpresora()
        checkEstusCdm()
        checkEstusCassettes()
        killSupervisor()

        tmMsgDevices.Enabled = True
    End Sub

    Private Sub checkEstusImpresora()
        Dim sEstatusPtr As String = ReadIni("PTR STATUS", "fwDevice", ConfigManager.workFileDevices)
        Try
            If sEstatusPtr <> sErrorImpresora Then
                sErrorImpresora = sEstatusPtr
                Trace("Cambio Estatus Impresora: " + sEstatusPtr)

                Dim sUrlActual As String = currentScreen.ToLower()
                Dim esWelcome As Boolean = sUrlActual.Contains("welcome") OrElse sUrlActual = ""
                Dim esMenuMore As Boolean = sUrlActual.Contains("menumore")

                If esWelcome OrElse esMenuMore Then
                    If sErrorImpresora = "HWERROR" Then
                        WebBrowser1.Document.InvokeScript("showErrPrinter")
                    Else
                        WebBrowser1.Document.InvokeScript("hideErrPrinter")
                    End If
                    Trace("Invoke Welcome ptr")
                Else
                    Trace("Cambio impresora detectado pero no en welcome - no invoke (" & currentScreen & ")")
                End If
            End If
        Catch ex As Exception
            Trace("Error al hacer el invoke Welcome: " + ex.Message)
        End Try
    End Sub

    Private Sub checkEstusCdm()
        Dim sEstatusCdm As String = ReadIni("CDM STATUS", "fwDevice", ConfigManager.workFileDevices)
        Try
            If sEstatusCdm <> sErrorCdm Then
                sErrorCdm = sEstatusCdm
                Trace("Cambio Estatus Disppensador: " + sEstatusCdm)

                ' Invoca scripts SOLO para mostrar advertencias visuales en el menú
                If IsCdmError() Then
                    WebBrowser1.Document.InvokeScript("showErrCdm")
                    WebBrowser1.Document.InvokeScript("showErrNoCash")
                Else
                    WebBrowser1.Document.InvokeScript("hideErrCdm")
                    WebBrowser1.Document.InvokeScript("hideErrNoCash")
                End If

                ' En caso de host error o dispensador error forzamos ocultar retiro
                If isHostError Then
                    Trace("checkEstusCdm: host error activo, ocultando btn retiro")
                    WebBrowser1.Document.InvokeScript("hideBtnRetiro")
                    WebBrowser1.Document.InvokeScript("showErrNoCash")
                End If

                Trace("Invoke Welcome cmd")

                ' En cualquier pantalla actualizamos el estado de retiro según el estado de dispensador o host
                If WebBrowser1 IsNot Nothing AndAlso WebBrowser1.Document IsNot Nothing Then
                    If isHostError OrElse IsCdmError() Then
                        Trace("checkEstusCdm: cdm o host error, ocultando btn retiro")
                        WebBrowser1.Document.InvokeScript("hideBtnRetiro")
                    Else
                        Trace("checkEstusCdm: cdm OK y host OK, mostrando btn retiro")
                        WebBrowser1.Document.InvokeScript("showBtnRetiro")
                    End If
                End If
            End If
        Catch ex As Exception
            Trace("Error al hacer el invoke Welcome: " + ex.Message)
        End Try
    End Sub

    Private Sub checkEstusCassettes()
        Try
            Dim cass1 As String = ReadIni("CDM_COUNTER", "CASSETTE1", ConfigManager.workFileDevices).Trim()
            Dim cass2 As String = ReadIni("CDM_COUNTER", "CASSETTE2", ConfigManager.workFileDevices).Trim()
            Dim cass3 As String = ReadIni("CDM_COUNTER", "CASSETTE3", ConfigManager.workFileDevices).Trim()
            Dim cass4 As String = ReadIni("CDM_COUNTER", "CASSETTE4", ConfigManager.workFileDevices).Trim()

            Trace("Cassettes: C1=" & cass1 & " C2=" & cass2 & " C3=" & cass3 & " C4=" & cass4)
            Trace("IsCdmError?: " & IsCdmError().ToString())

            If WebBrowser1 Is Nothing OrElse WebBrowser1.Document Is Nothing Then
                Trace("WebBrowser1.Document is Nothing, cannot invoke script")
                Return
            End If

            If cass1 = "0" OrElse cass2 = "0" OrElse cass3 = "0" OrElse cass4 = "0" OrElse IsCdmError() OrElse isHostError Then
                Trace("Invocando hideBtnRetiro + showErrNoCash (cassettes en 0 o error fisico en dispensador o error host)")
                WebBrowser1.Document.InvokeScript("hideBtnRetiro")
                WebBrowser1.Document.InvokeScript("showErrNoCash")
            Else
                Trace("Invocando showBtnRetiro + hideErrNoCash (cassettes con dinero)")
                WebBrowser1.Document.InvokeScript("showBtnRetiro")
                WebBrowser1.Document.InvokeScript("hideErrNoCash")
            End If
        Catch ex As Exception
            Trace("Error en checkEstusCassettes: " & ex.Message)
        End Try
    End Sub

    Private Sub killSupervisor()
        Try
            Dim nombreProceso As String = "appConfigurador.exe"
            Dim procesos() As Process = Process.GetProcessesByName(System.IO.Path.GetFileNameWithoutExtension(nombreProceso))
            If procesos.Length > 0 Then
                Trace("Se encontrol supervisor en welcome, se cierra")
                For Each p As Process In procesos
                    Try
                        p.Kill()
                    Catch ex As Exception
                        Trace("Error al terminar el proceso: " & ex.Message)
                    End Try
                Next
            End If
        Catch ex As Exception
            Trace("Error al buscar proceso: " & ex.Message)
        End Try
    End Sub

    Private Sub Form1_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing
        Trace("Cerrando appScreens")
    End Sub

    Private Sub WebBrowser1_DocumentCompleted(sender As Object, e As WebBrowserDocumentCompletedEventArgs)
        Try
            If e.Url.AbsolutePath = WebBrowser1.Url.AbsolutePath Then
                Trace("DocumentCompleted: " & e.Url.ToString())

                Threading.Thread.Sleep(100)

                If WebBrowser1.Document IsNot Nothing Then
                    Try
                        Dim anyZero As Boolean = AreAnyCassettesEmpty()
                        Trace("DocumentCompleted - AreAnyCassettesEmpty: " & anyZero)

                        If anyZero OrElse isHostError OrElse IsCdmError() Then
                            Trace("DocumentCompleted - Calling hideBtnRetiro (cassettes/host/cdm error)")
                            WebBrowser1.Document.InvokeScript("hideBtnRetiro")
                        Else
                            Trace("DocumentCompleted - Calling showBtnRetiro")
                            WebBrowser1.Document.InvokeScript("showBtnRetiro")
                        End If
                    Catch ex As Exception
                        Trace("Error invoking cassette script in DocumentCompleted: " & ex.Message)
                    End Try

                    If e.Url.ToString().ToLower().Contains("transferenciaentrecuentas-554") OrElse e.Url.ToString().ToLower().Contains("-554.") Then
                        ApplyTransparentRegion()
                    Else
                        RestoreFullRegion()
                    End If

                    If e.Url.ToString().ToLower().Contains("menumore") Then
                        Try
                            sErrorImpresora = ""
                            If IsPrinterError() Then
                                Trace("DocumentCompleted menuMore - showErrPrinter")
                                WebBrowser1.Document.InvokeScript("showErrPrinter")
                            Else
                                WebBrowser1.Document.InvokeScript("hideErrPrinter")
                            End If
                        Catch ex As Exception
                            Trace("Error invoking printer script in DocumentCompleted menuMore: " & ex.Message)
                        End Try
                    End If
                End If
            End If
        Catch ex As Exception
            Trace("Error WebBrowser1_DocumentCompleted: " & ex.Message)
        End Try
    End Sub

    Private Sub ApplyTransparentRegion()
        Try
            If transparentRegionApplied Then
                Return
            End If

            Dim holeX As Integer = 0
            Dim holeY As Integer = 430
            Dim holeWidth As Integer = Me.Width
            Dim holeHeight As Integer = 60

            Dim fullRegion As IntPtr = CreateRectRgn(0, 0, Me.Width, Me.Height)
            Dim holeRegion As IntPtr = CreateRectRgn(holeX, holeY, holeX + holeWidth, holeY + holeHeight)
            Dim combinedRegion As IntPtr = CreateRectRgn(0, 0, Me.Width, Me.Height)
            CombineRgn(combinedRegion, fullRegion, holeRegion, RGN_DIFF)
            SetWindowRgn(Me.Handle, combinedRegion, True)

            DeleteObject(fullRegion)
            DeleteObject(holeRegion)

            transparentRegionApplied = True
            Trace("[Transparent Region] Aplicada para pantalla 554 - Hueco en X=" & holeX & " Y=" & holeY & " W=" & holeWidth & " H=" & holeHeight)

        Catch ex As Exception
            Trace("[Transparent Region] Error aplicando región: " & ex.Message)
        End Try
    End Sub

    Private Sub RestoreFullRegion()
        Try
            If Not transparentRegionApplied Then
                Return
            End If

            Dim fullRegion As IntPtr = CreateRectRgn(0, 0, Me.Width, Me.Height)
            SetWindowRgn(Me.Handle, fullRegion, True)

            transparentRegionApplied = False
            Trace("[Transparent Region] Restaurada región completa")

        Catch ex As Exception
            Trace("[Transparent Region] Error restaurando región: " & ex.Message)
        End Try
    End Sub

End Class