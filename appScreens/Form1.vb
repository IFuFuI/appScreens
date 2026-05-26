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

    Dim ultimoLogCassettes As DateTime = DateTime.MinValue

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

    Dim omitirProximo701 As Boolean = False
    Dim tiempoBloqueo701 As DateTime = DateTime.MinValue
    Dim advertenciaMostrada As Boolean = False
    Dim timeoutCount As Integer = 0

    ' Variables para la lógica dinámica de FastCash y Denominaciones <<<
    Dim urlFastCashGlobal As String = ""
    Dim globalMultiplosDin As String = ""
    Dim globalMultiploMinimoDin As String = ""
    Dim globalMontoMaximoDin As String = ""
    Dim bFaltaBilletesDesdeMenu As Boolean = False

    ' >>> BANDERAS DE SINCRONIZACIÓN Y PROTECCIÓN <<<
    Dim aptraIsOnExceptionScreen As Boolean = False ' Destraba el cajero en fallas de hardware
    Dim keepCustomErrorPageActive As Boolean = False ' Protege las pantallas de error

    ' ============== Win32 API para región transparente ==============
    Private Declare Function CreateRectRgn Lib "gdi32" (ByVal X1 As Integer, ByVal Y1 As Integer, ByVal X2 As Integer, ByVal Y2 As Integer) As IntPtr
    Private Declare Function CombineRgn Lib "gdi32" (ByVal hDestRgn As IntPtr, ByVal hSrcRgn1 As IntPtr, ByVal hSrcRgn2 As IntPtr, ByVal nCombineMode As Integer) As Integer
    Private Declare Function SetWindowRgn Lib "user32" (ByVal hWnd As IntPtr, ByVal hRgn As IntPtr, ByVal bRedraw As Boolean) As Integer
    Private Declare Function DeleteObject Lib "gdi32" (ByVal hObject As IntPtr) As Boolean
    Private Const RGN_DIFF As Integer = 4
    Private transparentRegionApplied As Boolean = False

    ' Variables para almacenar la configuración del INI justo antes de navegar
    Dim currentHoleRequired As Boolean = False
    Dim currentHoleY As Integer = 0
    Dim currentHoleHeight As Integer = 0

    Private Sub frmsstWait_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Trace("Init appScreens V06.04 - Escalamiento Automático")
        writeINI("LG", "RESULT", "", ConfigManager.WorkFile)
        writeINI("SCREENS", "NUM", "", ConfigManager.WorkFile)
        writeINI("SCREENS", "DATA", "", ConfigManager.WorkFile)
        writeINI("NDC", "EVENT", "", ConfigManager.strRutaInterface)
        writeINI("NDC", "EVENT_SUPER", "", ConfigManager.strRutaInterface)
        writeINI("NDC", "SUPER", "", ConfigManager.strRutaInterface)
        writeINI("NDC", "READ", "", ConfigManager.strRutaInterface)

        Try
            ' Detectar la resolución REAL del monitor actual (Aqui yo pongo las resoluciones NCR=1024x768 o Hyosung=1280x800)
            Dim realScreenWidth As Integer = Screen.PrimaryScreen.Bounds.Width
            Dim realScreenHeight As Integer = Screen.PrimaryScreen.Bounds.Height

            ' El Formulario cubre toda la pantalla real
            Me.Size = New Size(realScreenWidth, realScreenHeight)
            Me.BackColor = Color.White
            Me.Location = New Point(0, 0)

            'Configuracion de posición desde INI, para casos donde el cliente quiera mover la pantalla a otro monitor o hacerla ventana
            Dim sPotition As String = ReadIni("PARAM", "SCREEN_POTITION", ConfigManager.ConfigAtmClientFile)
            If sPotition = "2" Then Me.Location = New Point(realScreenWidth, 0)
            If sPotition = "3" Then Me.Location = New Point(-realScreenWidth, 0)

        Catch ex As Exception
            Trace("Error set parameters")
        End Try

        Try
            ' El WebBrowser llena todo el formulario para que el HTML pueda estirarse en su interior
            WebBrowser1.Dock = DockStyle.Fill
            WebBrowser1.ScrollBarsEnabled = False
            WebBrowser1.ScriptErrorsSuppressed() = True

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

    Private Sub LoadHoleConfigFromIni(screenId As String)
        Try
            Dim reqTransparent As String = ReadIni(screenId, "TRANSPARENT", ConfigManager.ScreensFile).Trim().ToUpper()
            If reqTransparent = "TRUE" Then
                currentHoleRequired = True

                Dim iniHoleY As String = ReadIni(screenId, "HOLEY", ConfigManager.ScreensFile).Trim()
                Dim iniHoleHeight As String = ReadIni(screenId, "HOLEHEIGHT", ConfigManager.ScreensFile).Trim()

                If Not Integer.TryParse(iniHoleY, currentHoleY) Then currentHoleY = 430
                If Not Integer.TryParse(iniHoleHeight, currentHoleHeight) Then currentHoleHeight = 60

                Trace("Hole Config Guardada para " & screenId & ": Y=" & currentHoleY & " H=" & currentHoleHeight)
            Else
                currentHoleRequired = False
            End If
        Catch ex As Exception
            currentHoleRequired = False
            Trace("Error leyendo config de Hueco: " & ex.Message)
        End Try
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

                Dim allowScreen As Boolean = True

                If sValue = "701" AndAlso DateTime.Now < tiempoBloqueo701 Then
                    Trace("Ignorando 701 por periodo de gracia (Falta de billetes activa).")
                    allowScreen = False
                ElseIf sValue <> "701" Then
                    tiempoBloqueo701 = DateTime.MinValue
                End If

                If sValue = "701" AndAlso bNDCPageActive AndAlso allowScreen Then
                    Trace("701 recibido con NDC activo, currentScreen=" & currentScreen)

                    If pageException <> "" AndAlso Not isExpetionClosePageNDC Then
                        Trace("701 ignorado - estamos mostrando una pantalla de excepcion proactiva (" & pageException & ")")
                        allowScreen = False
                    ElseIf currentScreen.ToLower().Contains("menu") Then
                        Trace("701 con menu activo, forzando bNDCPageActive=False y procesando")
                        bNDCPageActive = False
                    ElseIf currentScreen = "pantalla_nativa_texto" Then
                        Trace("Primer 701 recibido despues de pantalla nativa, ignorando para mostrar texto")
                        currentScreen = "pantalla_nativa_texto_esperando_menu"
                        allowScreen = False
                    ElseIf currentScreen = "pantalla_nativa_texto_esperando_menu" Then
                        Trace("Segundo 701 recibido, asumiendo fin de pantalla nativa y forzando menu")
                        bNDCPageActive = False
                    Else
                        Trace("701 ignorado - pantalla NDC activa y no es menu")
                        allowScreen = False
                    End If
                End If

                If allowScreen Then
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
                            ' Si estamos en LenguajeSelector (Screen 150), ocultar appscreens inmediatamente
                            If sCurrent = "150" Then
                                ocultarPantalla()
                            ElseIf sDataEnable = "PIN" Then
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
                             Dim isPrinterErr As Boolean = IsPrinterError()
                             Trace("wb_RevisaEstadusDevices - anyZero=" & anyZero & " printerErr=" & isPrinterErr)
                             If Me IsNot Nothing AndAlso Not Me.IsDisposed Then
                                 Me.BeginInvoke(Sub()
                                                    Try
                                                        If WebBrowser1 Is Nothing OrElse WebBrowser1.Document Is Nothing Then
                                                            Trace("WebBrowser1.Document is Nothing in wb_RevisaEstadusDevices")
                                                            Return
                                                        End If

                                                        If isPrinterErr Then
                                                            WebBrowser1.Document.InvokeScript("showErrPrinter")
                                                        Else
                                                            WebBrowser1.Document.InvokeScript("hideErrPrinter")
                                                        End If

                                                        If isHostError Then
                                                            WebBrowser1.Document.InvokeScript("showErrHost")
                                                        Else
                                                            WebBrowser1.Document.InvokeScript("hideErrHost")
                                                        End If

                                                        If anyZero Then
                                                            Trace("Calling hideBtnRetiro from wb_RevisaEstadusDevices")
                                                            WebBrowser1.Document.InvokeScript("hideBtnRetiro")
                                                            WebBrowser1.Document.InvokeScript("showErrNoCash")
                                                        Else
                                                            Trace("Calling showBtnRetiro from wb_RevisaEstadusDevices")
                                                            WebBrowser1.Document.InvokeScript("showBtnRetiro")
                                                            WebBrowser1.Document.InvokeScript("hideErrNoCash")
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
            Dim c1 As Boolean = TieneBilletes("1")
            Dim c2 As Boolean = TieneBilletes("2")
            Dim c3 As Boolean = TieneBilletes("3")
            Dim c4 As Boolean = TieneBilletes("4")

            ' Si NINGUNO tiene billetes, están todos vacíos
            Dim allEmpty As Boolean = Not (c1 OrElse c2 OrElse c3 OrElse c4)
            Trace("AreAnyCassettesEmpty: C1=" & c1 & " C2=" & c2 & " C3=" & c3 & " C4=" & c4 & " allEmpty=" & allEmpty)

            Return allEmpty OrElse IsCdmError()
        Catch ex As Exception
            Trace("Error AreAnyCassettesEmpty: " & ex.Message)
            Return False
        End Try
    End Function

    Private Function IsPrinterError() As Boolean
        Try
            Dim status As String = ReadIni("PTR STATUS", "fwDevice", ConfigManager.workFileDevices).Trim().ToUpper()
            Return status = "HWERROR" OrElse status = "NODEVICE" OrElse status = "OFFLINE"
        Catch ex As Exception
            Trace("Error IsPrinterError: " & ex.Message)
            Return False
        End Try
    End Function

    Private Function IsCdmError() As Boolean
        Try
            Dim statusDevice As String = ReadIni("CDM STATUS", "Device", ConfigManager.workFileDevices).Trim().ToUpper()
            Dim statusFwDevice As String = ReadIni("CDM STATUS", "fwDevice", ConfigManager.workFileDevices).Trim().ToUpper()

            Dim status As String = If(statusDevice <> "", statusDevice, statusFwDevice)

            ' Ignoramos estados transitorios donde el dispensador está en movimiento post-retiro
            If status <> "ONLINE" AndAlso status <> "OK" AndAlso status <> "READY" AndAlso status <> "" AndAlso status <> "BUSY" AndAlso status <> "DISPENSING" Then
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

        Dim esExcepcion As Boolean = (pageException <> "" AndAlso pageException = sPotition)
        Dim esBack As Boolean = (fdkBack <> "" AndAlso fdkBack = sPotition)

        If esExcepcion Then
            pageException = ""
            sExp850OriginalUrl = ""
            sExp852OriginalUrl = ""
            bNDCPageActive = False
            Trace("Ejectua excepción FDK.. navega a currentScreen: " + currentScreen)

            If aptraIsOnExceptionScreen Then
                Trace("Enviando click para destrabar pantalla 850/851/852 nativa.")
                Me.Hide()
                Threading.Thread.Sleep(300)
                Select Case sPotition
                    Case "1" : ClickEnVentana.SimularClickEnVentana(screenEventTo, 177, 296)
                    Case "2" : ClickEnVentana.SimularClickEnVentana(screenEventTo, 177, 440)
                    Case "3" : ClickEnVentana.SimularClickEnVentana(screenEventTo, 177, 574)
                    Case "4" : ClickEnVentana.SimularClickEnVentana(screenEventTo, 177, 713)
                    Case "5" : ClickEnVentana.SimularClickEnVentana(screenEventTo, 850, 296)
                    Case "6" : ClickEnVentana.SimularClickEnVentana(screenEventTo, 850, 440)
                    Case "7" : ClickEnVentana.SimularClickEnVentana(screenEventTo, 850, 574)
                    Case "8" : ClickEnVentana.SimularClickEnVentana(screenEventTo, 850, 713)
                End Select
                Threading.Thread.Sleep(100)
                aptraIsOnExceptionScreen = False
            Else
                Trace("No se reportó pantalla de excepción nativa, es un error local, no enviamos clic físico.")
            End If

            WebBrowser1.Navigate(currentScreen)
            Me.Show()

        ElseIf esBack Then
            fdkBack = ""
            Trace("Ejectua back FDK.. navega a lastScreen: " + lastUrl)
            WebBrowser1.Navigate(lastUrl)
            Me.Show()
        Else
            Me.Hide()
            Threading.Thread.Sleep(300)

            Select Case sPotition
                Case "1" : ClickEnVentana.SimularClickEnVentana(screenEventTo, 177, 296)
                Case "2" : ClickEnVentana.SimularClickEnVentana(screenEventTo, 177, 440)
                Case "3" : ClickEnVentana.SimularClickEnVentana(screenEventTo, 177, 574)
                Case "4" : ClickEnVentana.SimularClickEnVentana(screenEventTo, 177, 713)
                Case "5" : ClickEnVentana.SimularClickEnVentana(screenEventTo, 850, 296)
                Case "6" : ClickEnVentana.SimularClickEnVentana(screenEventTo, 850, 440)
                Case "7" : ClickEnVentana.SimularClickEnVentana(screenEventTo, 850, 574)
                Case "8" : ClickEnVentana.SimularClickEnVentana(screenEventTo, 850, 713)
            End Select
        End If
    End Sub

    Public Sub clickPageWait(sPotition As String)
        clickPage(sPotition)
        Trace("FDK transaccional específico.. mostrando wait proactivamente")
        ProcesarPantalla("300")
    End Sub

    Public Function getVar(sVar As String) As String
        Dim sretur As String = String.Empty

        Select Case sVar
            ' --- INICIO MULTILENGUAJE: Retornar idioma actual a JS ---
            Case "idioma"
                Dim sIdioma As String = ReadIni("LG", "RESULT", ConfigManager.WorkFile)
                If sIdioma = "I" Then
                    sretur = "EN"
                ElseIf sIdioma = "M" Then
                    sretur = "MY"
                Else
                    sretur = "ES" ' Por defecto Español
                End If
            ' --- FIN MULTILENGUAJE ---

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
                    ' Intentar primero con ndcD, si está vacío o nulo, usar ndcG
                    Dim texto As String = If(Not String.IsNullOrEmpty(ndcD), ndcD, ndcG)

                    If Not String.IsNullOrEmpty(texto) Then
                        Dim partes() As String = texto.Split(New Char() {" "c}, StringSplitOptions.RemoveEmptyEntries)
                        If partes.Length > 0 Then sretur = partes(0)
                    End If
                Catch ex As Exception
                    Trace("Error getVar parsear t1: " + ex.Message)
                End Try

            Case "t2"
                Try
                    ' Intentar primero con ndcD, si está vacío o nulo, usar ndcG
                    Dim texto As String = If(Not String.IsNullOrEmpty(ndcD), ndcD, ndcG)

                    If Not String.IsNullOrEmpty(texto) Then
                        Dim partes() As String = texto.Split(New Char() {" "c}, StringSplitOptions.RemoveEmptyEntries)
                        ' Validar que exista la segunda parte antes de asignarla
                        If partes.Length > 1 Then sretur = partes(1)
                    End If
                Catch ex As Exception
                    Trace("Error getVar parsear t2: " + ex.Message)
                End Try
            Case "multiplosDin"
                sretur = globalMultiplosDin
            Case "montoMaxDin"
                sretur = globalMontoMaximoDin
            Case "multiploMin"
                CalcularValoresDinamicos()
                sretur = globalMultiploMinimoDin
            Case "faltaBilletesDesdeMenu"
                sretur = If(bFaltaBilletesDesdeMenu, "1", "0")
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
        Dim contenido As String = String.Empty

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

            Dim esFaltaDenominacion As Boolean = contenido.Contains("MONTO O DENOMINACION NO PERMITIDO") OrElse contenido.Contains("NO CONTAMOS CON BILLETES DE LA DENOMINACION SOLICITADA")
            Dim esError As Boolean = False

            If contenido.Contains("TRANSACCION RECHAZADA") OrElse (contenido.Contains("CODIGO RESPUESTA") AndAlso Not contenido.Contains("CODIGO RESPUESTA 000")) Then
                If Not esFaltaDenominacion Then
                    If contenido.Contains("TIPO DE TRANSACCION") Then
                        Trace("Encontro un error regresado por Host TIPO DE TRANSACCION, no muestra gracias: ")

                        If sLastMenuUrl <> "" Then
                            Try
                                Dim basePath As String = sLastMenuUrl.Substring(0, sLastMenuUrl.LastIndexOf("\") + 1)
                                sLastMenuUrl = basePath & "menuWO-506-noPrint.html"
                                Trace("Ticket impreso, actualizando sLastMenuUrl a: " & sLastMenuUrl)
                            Catch ex As Exception
                                Trace("Error al mutar url de menú: " & ex.Message)
                            End Try
                        End If

                    Else
                        esError = True
                    End If
                End If
            End If

            If contenido.Contains("CODIGO DE ERRORR") Then
                esError = True
            End If

            Dim esLayoutProtegido As Boolean = False
            If ndc.Layout = "P5280" OrElse ndc.Layout = "P5080" OrElse ndc.Layout = "P3520" OrElse ndc.Layout = "P3530" OrElse ndc.Layout = "P3540" Then
                esLayoutProtegido = True
            ElseIf ndc.Layout = "P3570" Then
                If contenido.Contains("LM") OrElse contenido.Contains("L") OrElse contenido.Contains(Chr(15) & "LM") OrElse contenido.Contains(Chr(15) & "L") Then
                    esLayoutProtegido = True
                Else
                    esLayoutProtegido = False
                End If
            End If

            Dim esRetiroSinTarjeta As Boolean = False
            If ndc.CodigoTransaccion = "055" AndAlso (contenido.Contains("PEM:034") OrElse contenido.Contains("TIPO DE TRANSACCION 110700")) Then
                esRetiroSinTarjeta = True
                esLayoutProtegido = True
                Trace("ESCUDO INTELIGENTE: Protección activada por Retiro sin Tarjeta (055 + PEM:034)")
            End If

            If esLayoutProtegido AndAlso esError Then
                Trace("ESCUDO INTELIGENTE: Ignorando error del Host en layout protegido: " & ndc.Layout)
                esError = False
            ElseIf esError Then
                Trace("Encontro un error real regresado por Host, procediendo a ocultar appScreens.")
                isHostError = True
                Trace("Bandera isHostError activada para interceptar pantalla 513")
            End If

            If contenido.Contains("TARJETA INVALIDA") OrElse contenido.Contains(" 014") Then
                Trace("Detectado error 014 (Tarjeta Inválida). Activando isHostError.")
                isHostError = True
                esError = True
            End If

            Dim hasFallbackPlanB As Boolean = False
            If ndc.Layout = "" AndAlso ndc.CodigoTransaccion <> "" Then
                Dim checkFallback As String = ReadIni(ndc.CodigoTransaccion, "PAGE", ConfigManager.ScreensFile)
                If checkFallback <> "" Then
                    hasFallbackPlanB = True
                End If
            End If

            If esError Then
                isExpetionClosePageNDC = True
                isHostError = True
                sExp852OriginalUrl = currentScreen
                sExp850OriginalUrl = ""
                pageException = "8"
                Trace("procesaDatosNDC: host error detectado => isHostError=True, sExp852OriginalUrl=" & sExp852OriginalUrl)

                If hasFallbackPlanB Then
                    Trace("Flashazo prevenido: Mantenemos appScreens visible porque existe página custom para " & ndc.CodigoTransaccion)
                Else
                    Me.Hide()
                End If
            End If

            If contenido.Contains("TRANSACCION RECHAZADA") AndAlso contenido.Contains("TIPO DE TRANSACCION") AndAlso Not esFaltaDenominacion Then
                If Not hasFallbackPlanB AndAlso ndc.Layout = "" Then
                    Me.Hide()
                End If
            End If

            If esFaltaDenominacion Then
                CalcularValoresDinamicos()
                bFaltaBilletesDesdeMenu = False
                sPage = "746"
                Trace("Falta de billetes detectada (Host). Múltiplos calculados: " & globalMultiplosDin)

                sUrl = ReadIni(sPage, "PAGE", ConfigManager.ScreensFile)
                LoadHoleConfigFromIni(sPage)
                Trace("sURL Msg: " & sUrl)

                If sUrl = "" Then
                    tiempoBloqueo701 = DateTime.Now.AddSeconds(6)
                    Trace("Iniciando cooldown de 6 segundos para ignorar el Menú (701).")
                End If
            End If

            If ndc.Layout <> "" Then
                sPage = ndc.Layout
                bPantalla = True
                sPage = sPage.Replace("P", "")
                sUrl = ReadIni(sPage, "PAGE", ConfigManager.ScreensFile)
                LoadHoleConfigFromIni(sPage)
                Trace("sURL Msg: " & sUrl)
            ElseIf ndc.CodigoTransaccion <> "" Then
                Dim fallbackUrl As String = ReadIni(ndc.CodigoTransaccion, "PAGE", ConfigManager.ScreensFile)

                If ndc.CodigoTransaccion = "055" AndAlso Not esRetiroSinTarjeta Then
                    fallbackUrl = "" ' Anulamos la ruta del INI
                    isHostError = True ' Engañamos al 513 para que oculte y NO muestre thanks
                    Me.Hide()
                    Trace("Se detectó 055 pero es flujo CON tarjeta física. Ocultando appScreens.")
                End If

                If fallbackUrl <> "" Then
                    sPage = ndc.CodigoTransaccion
                    bPantalla = True
                    sUrl = fallbackUrl
                    LoadHoleConfigFromIni(sPage)
                    Trace("sURL Msg Fallback State (" & sPage & "): " & sUrl)

                    keepCustomErrorPageActive = True
                    isHostError = False
                    isExpetionClosePageNDC = False
                End If
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
            Dim sUrlLower As String = sUrl.ToLower()
            Dim esPaginaMenu As Boolean = sUrlLower.Contains("menu") AndAlso Not sUrlLower.Contains("menumore")
            Dim esFastCash As Boolean = sUrlLower.Contains("fastcash")

            ' Prevenir que el menú tape la pantalla de impresión
            Dim esImpresionPrematura As Boolean = contenido.Contains("TIPO DE TRANSACCION") AndAlso esPaginaMenu

            ' Si es una impresión prematura, NO mostramos la pantalla aún
            If Not isExpetionClosePageNDC AndAlso Not esImpresionPrematura Then
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

            If esFastCash Then
                urlFastCashGlobal = sUrl
            End If

            If esPaginaMenu Then
                sLastMenuUrl = sUrl
            End If

            ' Agregamos la condición para que no dispare alertas si está imprimiendo
            If (esPaginaMenu OrElse esFastCash) AndAlso Not advertenciaMostrada AndAlso Not esImpresionPrematura Then
                Dim fallaDispensador As Boolean = AreAnyCassettesEmpty() OrElse IsCdmError()
                Dim fallaImpresora As Boolean = IsPrinterError()

                Dim c1 As Boolean = TieneBilletes("1")
                Dim c2 As Boolean = TieneBilletes("2")
                Dim c3 As Boolean = TieneBilletes("3")
                Dim c4 As Boolean = TieneBilletes("4")
                Dim faltaAlgunaDenominacion As Boolean = (Not c1 OrElse Not c2 OrElse Not c3 OrElse Not c4) AndAlso Not fallaDispensador

                If fallaDispensador AndAlso fallaImpresora Then
                    Trace("Ambos errores detectados - redirigiendo a exp-851 proactivamente")
                    advertenciaMostrada = True
                    sExp852OriginalUrl = sUrl
                    sExp850OriginalUrl = ""
                    currentScreen = sUrl
                    pageException = "8"
                    isExpetionClosePageNDC = False
                    Me.Show()
                    Dim sUrlBase As String = sUrl.Substring(0, sUrl.LastIndexOf("\") + 1)
                    sUrl = sUrlBase & "exp-851.html"
                    currentHoleRequired = False
                ElseIf fallaDispensador Then
                    Trace("Sin efectivo o Error CDM - redirigiendo a exp-852 proactivamente")
                    advertenciaMostrada = True
                    sExp852OriginalUrl = sUrl
                    sExp850OriginalUrl = ""
                    currentScreen = sUrl
                    pageException = "8"
                    isExpetionClosePageNDC = False
                    Me.Show()
                    Dim sUrlBase As String = sUrl.Substring(0, sUrl.LastIndexOf("\") + 1)
                    sUrl = sUrlBase & "exp-852.html"
                    currentHoleRequired = False
                ElseIf fallaImpresora Then
                    Trace("Error impresora - redirigiendo a exp-850")
                    advertenciaMostrada = True
                    sExp850OriginalUrl = sUrl
                    sExp852OriginalUrl = ""
                    currentScreen = sUrl
                    pageException = "8"
                    isExpetionClosePageNDC = False
                    Me.Show()
                    Dim sUrlBase850 As String = sUrl.Substring(0, sUrl.LastIndexOf("\") + 1)
                    sUrl = sUrlBase850 & "exp-850.html"
                    currentHoleRequired = False
                ElseIf faltaAlgunaDenominacion AndAlso esFastCash Then
                    CalcularValoresDinamicos()
                    bFaltaBilletesDesdeMenu = True
                    Dim pantallaFalta As String = "746"
                    Trace("Falta denominación detectada PROACTIVAMENTE en FastCash. Múltiplos: " & globalMultiplosDin & " Max: " & globalMontoMaximoDin)

                    advertenciaMostrada = True
                    sExp852OriginalUrl = ""
                    sExp850OriginalUrl = ""
                    currentScreen = sUrl

                    pageException = ""
                    isExpetionClosePageNDC = False
                    Me.Show()

                    Dim urlFalta As String = ReadIni(pantallaFalta, "PAGE", ConfigManager.ScreensFile)
                    If urlFalta <> "" Then
                        sUrl = urlFalta
                        LoadHoleConfigFromIni(pantallaFalta)
                    End If
                Else
                    If esFastCash Then
                        Trace("Es FastCash sin fallo de Hardware. Forzando Me.Show() para evitar pantalla en blanco.")
                        isExpetionClosePageNDC = False
                        Me.Show()
                    End If
                    sExp852OriginalUrl = ""
                    sExp850OriginalUrl = ""
                End If
            End If

            ' Abortamos la navegación prematura y nos ocultamos
            If esImpresionPrematura Then
                Trace("Impresion en curso (TIPO DE TRANSACCION). Ocultamos appScreens y esperamos al 701 para mostrar el menu.")
                Me.Hide()
                bNDCPageActive = False
            Else
                Trace("Navega page Url DatosNDC: " + sUrl)
                WebBrowser1.Navigate(sUrl)
            End If

        Else
            sImagen = ReadIni(sPage, "PIC", ConfigManager.ScreensFile)
            If sImagen <> "" Then
                Try
                    ' --- INICIO MULTILENGUAJE: Hot-Swap nativo para NDC Host ---
                    Dim sIdAct As String = ReadIni("LG", "RESULT", ConfigManager.WorkFile)
                    Dim langFolder As String = "ES" ' Por defecto español
                    If sIdAct = "I" Then langFolder = "EN"
                    If sIdAct = "M" Then langFolder = "MY"

                    ' >>> RUTA IMPORTANTE <<< Ajusta esta ruta a tu carpeta Media real
                    Dim rutaBase As String = "C:\appMain\html\Media\"
                    Dim nombreBase As String = "Pic" & sImagen.PadLeft(3, "0"c)

                    ' Copia la imagen sin importar la extensión
                    Dim extensiones() As String = {".png", ".gif", ".jpg"}
                    For Each ext In extensiones
                        Dim archivoOrigen As String = rutaBase & langFolder & "\" & nombreBase & ext
                        Dim archivoDestino As String = rutaBase & nombreBase & ext

                        Try
                            If File.Exists(archivoOrigen) Then
                                File.Copy(archivoOrigen, archivoDestino, True)
                            End If
                        Catch exCopy As Exception
                            Trace("Error copiando " & ext & ": " & exCopy.Message)
                        End Try
                    Next
                    ' --- FIN MULTILENGUAJE ---

                    dllInterfaceNdc.showScreenNDC(CInt(sImagen))
                    Trace("Muestra Pantalla PIC")
                    ocultarPantalla()

                    bNDCPageActive = True
                    currentScreen = "pantalla_nativa_pic"
                Catch ex As Exception
                    Trace("Error al mandar interface en PIC " + ex.Message)
                End Try
            Else
                If bPantalla = True Then
                    ocultarPantalla()

                    bNDCPageActive = True
                    currentScreen = "pantalla_nativa_texto"
                End If
            End If
        End If
    End Sub

    Private Sub ProcesarPantalla(sValue As String)
        Dim sUrl As String = String.Empty
        Dim sImagen As String = String.Empty
        Dim sDataEnable As String = String.Empty
        ' Dim sIdioma As String -> Ya no lo necesitamos como variable local directa, se maneja abajo.

        If sValue = "850" AndAlso sExp850OriginalUrl <> "" Then
            Trace("850 ignorado - exp-850 ya activo proactivamente. Marcando IsOnExceptionScreen=True")
            isExpetionClosePageNDC = False
            aptraIsOnExceptionScreen = True
            Exit Sub
        End If

        If sValue = "851" AndAlso sExp852OriginalUrl <> "" Then
            Trace("851 ignorado - exp-851 ya activo proactivamente. Marcando IsOnExceptionScreen=True")
            isExpetionClosePageNDC = False
            aptraIsOnExceptionScreen = True
            Exit Sub
        End If

        If sValue = "852" AndAlso sExp852OriginalUrl <> "" Then
            Trace("852 ignorado - exp-852 ya activo proactivamente. Marcando IsOnExceptionScreen=True")
            isExpetionClosePageNDC = False
            aptraIsOnExceptionScreen = True
            Exit Sub
        End If

        If (sValue = "200" OrElse sValue = "300") AndAlso currentScreen.ToLower().Contains("confirmacionpagotdc") AndAlso IsPrinterError() Then
            Trace("Interceptando " & sValue & ": Mantenemos oculto appScreens para dejar visible la pregunta nativa (Falla Impresora)")
            sValue = "hide"
        End If

        ' >>> FIX: Cuando sale la pantalla de excepción (nativa) por segunda o más veces,
        ' encendemos la bandera para obligar a que el clic del usuario se envíe a APTRA.
        If sValue = "850" OrElse sValue = "851" OrElse sValue = "852" Then
            aptraIsOnExceptionScreen = True

            ' >>> FIX DEFINITIVO: Le avisamos al sistema proactivo que el cliente YA VIO el error nativo.
            ' Esto bloquea que NUNCA MÁS se vuelva a mostrar en la sesión (ni en FastCash ni en otro flujo).
            advertenciaMostrada = True
            Trace("Pantalla de excepción nativa FDK " & sValue & " detectada. Se marca advertenciaMostrada = True para no repetir.")
            ' <<< FIN FIX
        Else
            aptraIsOnExceptionScreen = False
        End If

        pageException = ""

        ' >>> HACK TEMPORAL PARA PRUEBAS DESDE EL INI (SE EJECUTA SIEMPRE) <<<
        ' Verificar si el usuario cambió el idioma manualmente en el INI
        Dim testLangFromIni As String = ReadIni("LG", "RESULT", ConfigManager.WorkFile)
        If testLangFromIni = "I" OrElse testLangFromIni = "M" Then
            ' Si el INI tiene una letra distinta a vacío, disparamos el reemplazo masivo
            GuardarIdiomaSeleccionado(testLangFromIni)
        End If
        ' >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

        ' >>> BARRERA PROTECTORA DE PANTALLA CUSTOM <<<
        If sValue = "welcome" OrElse sValue = "500" OrElse sValue = "701" OrElse sValue = "hide" Then
            keepCustomErrorPageActive = False
        End If

        If sValue = "513" AndAlso keepCustomErrorPageActive Then
            Trace("513 ignorado proactivamente para mantener visible la pantalla de error custom.")
            Exit Sub
        End If

        If sValue = "welcome" Then
            sValue = "500"
            bNDCPageActive = False

            ' --- INICIO MULTILENGUAJE: Limpiar idioma al regresar a la pantalla de inicio ---
            writeINI("LG", "RESULT", "", ConfigManager.WorkFile)

            ' Restaurar TODAS las imágenes al idioma por defecto (ES) masivamente
            Dim rutaBase As String = "C:\appMain\html\Media\"
            Dim rutaOrigenES As String = Path.Combine(rutaBase, "ES")

            Try
                If Directory.Exists(rutaOrigenES) Then
                    Dim archivosES As String() = Directory.GetFiles(rutaOrigenES, "Pic*.*")
                    For Each archivo In archivosES
                        Dim nombreArchivo As String = Path.GetFileName(archivo)
                        Dim archivoDestino As String = Path.Combine(rutaBase, nombreArchivo)
                        Try
                            File.Copy(archivo, archivoDestino, True)
                        Catch ex As Exception
                            Trace("Error restaurando imagen ES: " & nombreArchivo & " - " & ex.Message)
                        End Try
                    Next
                    Trace("Imágenes restauradas masivamente a Español (ES) en el reset de sesión.")
                End If
            Catch ex As Exception
                Trace("Error global restaurando imágenes ES: " & ex.Message)
            End Try

            ' Restaurar imágenes globales desde carpeta GLOBAL (Pic501, etc)
            Dim rutaGlobal As String = Path.Combine(rutaBase, "GLOBAL")
            Try
                If Directory.Exists(rutaGlobal) Then
                    Dim archivosGlobal As String() = Directory.GetFiles(rutaGlobal, "Pic*.*")
                    For Each archivo In archivosGlobal
                        Dim nombreArchivo As String = Path.GetFileName(archivo)
                        Dim archivoDestino As String = Path.Combine(rutaBase, nombreArchivo)
                        Try
                            File.Copy(archivo, archivoDestino, True)
                        Catch ex As Exception
                            Trace("Error restaurando imagen GLOBAL: " & nombreArchivo & " - " & ex.Message)
                        End Try
                    Next
                    Trace("Imágenes globales restauradas en el reset de sesión.")
                End If
            Catch ex As Exception
                Trace("Error global restaurando imágenes GLOBAL: " & ex.Message)
            End Try
            ' --- FIN MULTILENGUAJE ---

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
            isExpetionClosePageNDC = False
            advertenciaMostrada = False
            aptraIsOnExceptionScreen = False
            Trace("ProcesarPantalla: clear isHostError y exception NDC (pantalla 513/500)")
            If WebBrowser1 IsNot Nothing AndAlso WebBrowser1.Document IsNot Nothing Then
                Try
                    WebBrowser1.Document.InvokeScript("showBtnRetiro")
                Catch ex As Exception
                End Try
            End If
        ElseIf sValue = "701" Then
            bNDCPageActive = False
            Trace("ProcesarPantalla: 701 recibido, conservando memoria de errores")
        End If

        If sValue = "850" OrElse sValue = "852" OrElse sValue = "851" Then
            isExpetionClosePageNDC = False
        End If

        If isExpetionClosePageNDC Then
            If sValue = "701" Then
                Trace("701 en estado de excepción NDC: reanuda el flujo sin ocultar pantalla")
                isExpetionClosePageNDC = False
            Else
                Trace("Es Close por excepcion NDC oculata pantalla")
                isExpetionClosePageNDC = False
                Me.Hide()
                Exit Sub
            End If
        End If

        If sValue = "513" And isClosePage Then
            If currentScreen.ToLower().Contains("thanksdisp") Then
                Trace("Es Close pero venimos de thanksDisp, permitiendo transición nativa a 513")
                isClosePage = False
            Else
                Trace("Es Close y ya tiene una pantalla de salida")
                isClosePage = False
                Exit Sub
            End If
        End If

        If sValue = "513" Then
            If isHostError Then
                Trace("513 interceptado: Error de Host detectado. Forzando ocultar para ver pantalla nativa.")
                sValue = "hide"
                isHostError = False
            ElseIf currentScreen.ToLower().Contains("menu") OrElse
                   Not Me.Visible OrElse
                   currentScreen = "pantalla_nativa_pic" OrElse
                   currentScreen = "pantalla_nativa_texto" OrElse
                   currentScreen = "pantalla_nativa_texto_esperando_menu" OrElse
                   currentScreen = "" Then

                Trace("513 interceptado: Cancelación desde menú o flujo nativo. Mantenemos oculto.")
                sValue = "hide"
            End If
        End If

        If sValue = "500" OrElse sValue = "hide" Then
            isHostError = False
        End If

        Try
            sUrl = ReadIni(sValue, "PAGE", ConfigManager.ScreensFile)

            If sValue <> "back" AndAlso sValue <> "hide" Then
                LoadHoleConfigFromIni(sValue)
            End If

            Try
                If WebBrowser1 IsNot Nothing AndAlso WebBrowser1.Url IsNot Nothing Then
                    Dim sWB As String = WebBrowser1.Url.ToString.Replace("file:///", "")
                    If Me.Visible Then
                        If sUrl = sWB Then
                            Exit Sub
                        End If
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
                sDataEnable = ReadIni(sValue, "DATA", ConfigManager.ScreensFile)

                ' --- INICIO MULTILENGUAJE: Comentamos la carga de páginas con sufijos _I y _M para que siempre cargue el HTML limpio ---
                ' sIdioma = ReadIni("LG", "RESULT", ConfigManager.WorkFile)
                ' If sIdioma <> "" Then
                '     Dim iPoint As Integer
                '     Dim sTU As String = ""
                '     iPoint = sUrl.IndexOf(".")
                '     If iPoint > 0 Then
                '         sTU = Mid(sUrl, 1, iPoint)
                '         sUrl = sTU + "_" + sIdioma + ".html"
                '     End If
                ' End If
                ' --- FIN MULTILENGUAJE ---

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
                        ' La copia masiva (Hot-Swap) ya se realiza en GuardarIdiomaSeleccionado() 
                        ' o al momento del reset de sesión (welcome).
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

    Public Sub ocultarPantalla()
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
        Dim sEstatusPtr As String = ReadIni("PTR STATUS", "fwDevice", ConfigManager.workFileDevices).Trim().ToUpper()
        Try
            If sEstatusPtr <> sErrorImpresora Then
                sErrorImpresora = sEstatusPtr
                Trace("Cambio Estatus Impresora: " + sEstatusPtr)

                Dim sUrlActual As String = currentScreen.ToLower()
                Dim esWelcome As Boolean = sUrlActual.Contains("welcome") OrElse sUrlActual = ""
                Dim esMenuMore As Boolean = sUrlActual.Contains("menumore")

                If esWelcome OrElse esMenuMore Then
                    ' >>> FIX: Evaluamos HWERROR, NODEVICE y OFFLINE
                    If sErrorImpresora = "HWERROR" OrElse sErrorImpresora = "NODEVICE" OrElse sErrorImpresora = "OFFLINE" Then
                        WebBrowser1.Document.InvokeScript("showErrPrinter")
                    Else
                        WebBrowser1.Document.InvokeScript("hideErrPrinter")
                    End If
                    ' <<< FIN FIX

                    If AreAnyCassettesEmpty() Then
                        WebBrowser1.Document.InvokeScript("hideBtnRetiro")
                    Else
                        WebBrowser1.Document.InvokeScript("showBtnRetiro")
                    End If

                    Trace("Invoke Welcome ptr")
                Else
                    Trace("Cambio impresora detectado pero no en welcome/menuMore - no invoke (" & currentScreen & ")")
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

                If IsCdmError() Then
                    WebBrowser1.Document.InvokeScript("showErrCdm")
                    WebBrowser1.Document.InvokeScript("showErrNoCash")
                Else
                    WebBrowser1.Document.InvokeScript("hideErrCdm")
                    WebBrowser1.Document.InvokeScript("hideErrNoCash")
                End If

                If isHostError Then
                    Trace("checkEstusCdm: host error activo, ocultando btn retiro")
                    WebBrowser1.Document.InvokeScript("showErrHost")
                    WebBrowser1.Document.InvokeScript("showErrNoCash")
                Else
                    WebBrowser1.Document.InvokeScript("hideErrHost")
                End If

                Trace("Invoke Welcome cmd")

                If WebBrowser1 IsNot Nothing AndAlso WebBrowser1.Document IsNot Nothing Then
                    If IsCdmError() Then
                        Trace("checkEstusCdm: cdm error, ocultando btn retiro")
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
            Dim c1 As Boolean = TieneBilletes("1")
            Dim c2 As Boolean = TieneBilletes("2")
            Dim c3 As Boolean = TieneBilletes("3")
            Dim c4 As Boolean = TieneBilletes("4")

            ' --- Log cada 60 min ---
            Dim imprimirLog As Boolean = False
            If DateTime.Now >= ultimoLogCassettes.AddMinutes(60) Then
                imprimirLog = True
                ultimoLogCassettes = DateTime.Now ' Reiniciamos el reloj
            End If

            ' Solo imprimimos el estado si el temporizador lo permite
            If imprimirLog Then
                Trace("Cassettes (Tiene Billetes): C1=" & c1 & " C2=" & c2 & " C3=" & c3 & " C4=" & c4)
                Trace("IsCdmError?: " & IsCdmError().ToString())
            End If

            If WebBrowser1 Is Nothing OrElse WebBrowser1.Document Is Nothing Then
                If imprimirLog Then Trace("WebBrowser1.Document is Nothing, cannot invoke script")
                Return
            End If

            Dim allCassetteEmpty As Boolean = Not (c1 OrElse c2 OrElse c3 OrElse c4)

            If allCassetteEmpty OrElse IsCdmError() Then
                If imprimirLog Then Trace("Invocando hideBtnRetiro + showErrNoCash (4 cassettes en 0 y/o error dispensador)")
                WebBrowser1.Document.InvokeScript("hideBtnRetiro")
                WebBrowser1.Document.InvokeScript("showErrNoCash")
            Else
                If imprimirLog Then Trace("Invocando showBtnRetiro (cassettes con dinero)")
                WebBrowser1.Document.InvokeScript("showBtnRetiro")
                WebBrowser1.Document.InvokeScript("hideErrNoCash")
            End If

            If IsPrinterError() Then
                WebBrowser1.Document.InvokeScript("showErrPrinter")
            Else
                WebBrowser1.Document.InvokeScript("hideErrPrinter")
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
                    ' Inicio del apartado responsive de las pantallas HTML
                    Try
                        ' Calculamos cuánto hay que estirar la pantalla a lo ancho y alto basándonos en el 1024x768 original
                        Dim scaleX As String = (Me.Width / 1024.0).ToString(System.Globalization.CultureInfo.InvariantCulture)
                        Dim scaleY As String = (Me.Height / 768.0).ToString(System.Globalization.CultureInfo.InvariantCulture)

                        ' Inyectamos el CSS dinámicamente al Body sin modificar los archivos físicos
                        ' Inyectamos el CSS dinámicamente forzando que no haya márgenes y obligando a adaptar la altura al 100%
                        Dim cssScale As String = $"; transform: scale({scaleX}, {scaleY}); transform-origin: top left; width: 1024px !important; height: 768px !important; min-height: 768px !important; margin: 0px !important; padding: 0px !important; overflow: hidden !important; background-size: 100% 100% !important; background-repeat: no-repeat !important; background-position: left top !important;"
                        Dim bodyElement As HtmlElement = WebBrowser1.Document.Body
                        If bodyElement IsNot Nothing Then
                            bodyElement.Style = bodyElement.Style & cssScale
                        End If
                    Catch ex As Exception
                        Trace("Error inyectando reescalado HTML: " & ex.Message)
                    End Try
                    ' Fin del responsive

                    Try
                        Dim anyZero As Boolean = AreAnyCassettesEmpty()
                        Trace("DocumentCompleted - AreAnyCassettesEmpty: " & anyZero)

                        ' Inyectar idioma a JavaScript
                        Try
                            Dim sIdiomaIni As String = ReadIni("LG", "RESULT", ConfigManager.WorkFile)

                            Dim sIdiomaJS As String = "ES"
                            If sIdiomaIni = "I" Then
                                sIdiomaJS = "EN"
                            ElseIf sIdiomaIni = "M" Then
                                sIdiomaJS = "MY"
                            End If

                            ' Inyectar script usando InvokeScript
                            If WebBrowser1.Document IsNot Nothing Then
                                Try
                                    WebBrowser1.Document.InvokeScript("definirIdioma", New Object() {sIdiomaJS})
                                Catch ex2 As Exception
                                End Try
                            End If
                        Catch ex As Exception
                        End Try

                        If IsCdmError() Then
                            WebBrowser1.Document.InvokeScript("showErrCdm")
                        Else
                            WebBrowser1.Document.InvokeScript("hideErrCdm")
                        End If

                        If IsPrinterError() Then
                            WebBrowser1.Document.InvokeScript("showErrPrinter")
                        Else
                            WebBrowser1.Document.InvokeScript("hideErrPrinter")
                        End If

                        If anyZero Then
                            Trace("DocumentCompleted - Calling hideBtnRetiro (cassettes/cdm error)")
                            WebBrowser1.Document.InvokeScript("hideBtnRetiro")
                        Else
                            Trace("DocumentCompleted - Calling showBtnRetiro")
                            WebBrowser1.Document.InvokeScript("showBtnRetiro")
                        End If
                    Catch ex As Exception
                        Trace("Error invoking cassette script in DocumentCompleted: " & ex.Message)
                    End Try

                    If currentHoleRequired Then
                        ApplyTransparentRegion(currentHoleY, currentHoleHeight)
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

    Private Sub ApplyTransparentRegion(holeY As Integer, holeHeight As Integer)
        Try
            If transparentRegionApplied Then
                RestoreFullRegion()
            End If

            ' 1. Calculamos los factores de escala
            Dim scaleY As Double = Me.Height / 768.0

            ' 2. Reescalamos las coordenadas del INI 
            Dim realHoleY As Integer = CInt(holeY * scaleY)
            Dim realHoleHeight As Integer = CInt(holeHeight * scaleY)

            Dim realHoleX As Integer = 0
            Dim realHoleWidth As Integer = Me.Width ' Cubre todo el ancho del nuevo monitor

            Dim fullRegion As IntPtr = CreateRectRgn(0, 0, Me.Width, Me.Height)
            Dim holeRegion As IntPtr = CreateRectRgn(realHoleX, realHoleY, realHoleX + realHoleWidth, realHoleY + realHoleHeight)
            Dim combinedRegion As IntPtr = CreateRectRgn(0, 0, Me.Width, Me.Height)

            CombineRgn(combinedRegion, fullRegion, holeRegion, RGN_DIFF)
            SetWindowRgn(Me.Handle, combinedRegion, True)

            DeleteObject(fullRegion)
            DeleteObject(holeRegion)

            transparentRegionApplied = True
            Trace($"[Transparent Region] Aplicada Reescalada - Original Y={holeY} | Nuevo Y={realHoleY} Alto={realHoleHeight}")

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

    Private Sub CalcularValoresDinamicos()
        Try
            Dim rutaConfigAtm As String = ConfigManager.ConfigAtmClientFile
            Dim d1 As String = ReadIni("CDM", "DENOM_C1", rutaConfigAtm).Trim()
            Dim d2 As String = ReadIni("CDM", "DENOM_C2", rutaConfigAtm).Trim()
            Dim d3 As String = ReadIni("CDM", "DENOM_C3", rutaConfigAtm).Trim()
            Dim d4 As String = ReadIni("CDM", "DENOM_C4", rutaConfigAtm).Trim()

            If d1 = "" Then d1 = "50"
            If d2 = "" Then d2 = "100"
            If d3 = "" Then d3 = "200"
            If d4 = "" Then d4 = "500"

            Dim disponibles As New List(Of Integer)()

            If TieneBilletes("1") AndAlso Not disponibles.Contains(CInt(d1)) Then disponibles.Add(CInt(d1))
            If TieneBilletes("2") AndAlso Not disponibles.Contains(CInt(d2)) Then disponibles.Add(CInt(d2))
            If TieneBilletes("3") AndAlso Not disponibles.Contains(CInt(d3)) Then disponibles.Add(CInt(d3))
            If TieneBilletes("4") AndAlso Not disponibles.Contains(CInt(d4)) Then disponibles.Add(CInt(d4))

            If disponibles.Count = 0 Then
                globalMultiplosDin = "las denominaciones disponibles"
                globalMontoMaximoDin = "8,000.00"
                globalMultiploMinimoDin = "50"
                Return
            End If

            disponibles.Sort()

            globalMultiploMinimoDin = disponibles(0).ToString()

            Dim maxDenom As Integer = disponibles.Last()
            Dim maxMonto As Integer = maxDenom * 40
            If maxMonto > 8000 Then maxMonto = 8000
            globalMontoMaximoDin = maxMonto.ToString("N2")

            Dim disponiblesStr As List(Of String) = disponibles.Select(Function(n) "$" & n.ToString()).ToList()
            globalMultiplosDin = String.Join(", ", disponiblesStr)

        Catch ex As Exception
            Trace("Error al calcular valores dinámicos: " & ex.Message)
            globalMultiplosDin = "las denominaciones disponibles"
            globalMontoMaximoDin = "8,000.00"
        End Try
    End Sub

    Private Function TieneBilletes(numeroCasetero As String) As Boolean
        Try
            Dim logicoStr As String = ReadIni("CDM_COUNTER", "CASSETTE" & numeroCasetero, ConfigManager.workFileDevices).Trim()
            Dim logCount As Integer = -1

            If Integer.TryParse(logicoStr, logCount) AndAlso logCount <= 0 Then
                Return False
            End If

            Dim fisicoStr As String = ReadIni("CDM CASSETTES STATUS", "cassette" & numeroCasetero & "_status", ConfigManager.workFileDevices).Trim().ToUpper()

            If fisicoStr = "EMPTY" Then
                Return False
            End If

            Return True
        Catch ex As Exception
            Trace("Error al verificar billetes del casetero " & numeroCasetero & ": " & ex.Message)
            Return True
        End Try
    End Function

    Public Sub regresarFastCash()
        Try
            Trace("Regresando a FastCash manualmente a petición del usuario")
            If urlFastCashGlobal <> "" Then
                currentScreen = urlFastCashGlobal
                WebBrowser1.Navigate(urlFastCashGlobal)
                Me.Show()
            End If
        Catch ex As Exception
            Trace("Error en regresarFastCash: " & ex.Message)
        End Try
    End Sub

    ' --- INICIO MULTILENGUAJE: Función para que el HTML guarde el idioma que elige el usuario ---
    Public Sub GuardarIdiomaSeleccionado(ByVal idioma As String)
        Try
            writeINI("LG", "RESULT", idioma, ConfigManager.WorkFile)
            Trace("Idioma cambiado por el usuario a: " & idioma)

            ' Reemplazar TODAS las imágenes en bloque al momento de elegir el idioma
            Dim rutaBase As String = "C:\appMain\html\Media\"
            Dim langFolder As String = "ES"
            If idioma = "I" Then langFolder = "EN"
            If idioma = "M" Then langFolder = "MY"

            Dim rutaOrigen As String = Path.Combine(rutaBase, langFolder)

            If Directory.Exists(rutaOrigen) Then
                Dim archivosOrigen As String() = Directory.GetFiles(rutaOrigen, "Pic*.*")
                For Each archivo In archivosOrigen
                    Dim nombreArchivo As String = Path.GetFileName(archivo)
                    Dim archivoDestino As String = Path.Combine(rutaBase, nombreArchivo)
                    Try
                        File.Copy(archivo, archivoDestino, True)
                    Catch ex As Exception
                        Trace("Error reemplazando masivamente: " & nombreArchivo & " - " & ex.Message)
                    End Try
                Next
                Trace("Reemplazo masivo de recursos a idioma " & langFolder & " completado.")
            End If

        Catch ex As Exception
            Trace("Error al guardar idioma: " & ex.Message)
        End Try
    End Sub
    ' --- FIN MULTILENGUAJE ---

End Class