Imports System.IO
Imports System.Net.Mime.MediaTypeNames
Imports System.Security.Permissions
Imports System.Text.RegularExpressions
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
    Dim sinEfectivoEstable As Boolean = False
    Dim inicioRecuperacionEfectivo As DateTime = DateTime.MinValue

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
    Dim sMenuActivo As String = ""

    Dim webBrowserWaitRegreso As WebBrowser = Nothing
    Dim waitRegresoCargado As Boolean = False
    Dim waitRegresoActivo As Boolean = False
    Dim regresoNativo024Activo As Boolean = False
    Dim regresoMenuMorePendiente As Boolean = False
    Dim timerOcultarWaitRegreso As Timer = Nothing
    Dim timerOcultarWaitTimeout As Timer = Nothing
    Dim timerOcultarReadCardWelcome As Timer = Nothing
    Dim timerOcultarTimeoutInicial As Timer = Nothing
    Dim timerMenuAdvertenciaHardware As Timer = Nothing
    Dim toqueRegresoNativoDetectado As Boolean = False
    Dim idCoberturaRender As Integer = 0

    Dim omitirProximo701 As Boolean = False
    Dim tiempoBloqueo701 As DateTime = DateTime.MinValue
    Dim advertenciaHardwareActiva As Boolean = False
    Dim advertenciaDenominacionMostrada As Boolean = False
    Dim bAdvertenciaFicticiaMostrada As Boolean = False
    Dim menuPendienteTrasAdvertenciaFicticia As String = ""
    Dim menuAdvertenciaHardwarePendiente As Boolean = False
    Dim menuAdvertenciaHardwareUrl As String = ""
    Dim transicionMenuNdcPendiente As Boolean = False
    Dim transicionRetiroFastCashPendiente As Boolean = False
    Dim timer701TransicionMenu As Timer = Nothing
    Dim navegacionFastCashCubierta As Boolean = False
    Dim timeoutCount As Integer = 0

    ' Variables para la lógica dinámica de FastCash y Denominaciones <<<
    Dim urlFastCashGlobal As String = ""
    Dim globalMultiplosDin As String = ""
    Dim globalMultiploMinimoDin As String = ""
    Dim globalMontoMaximoDin As String = ""
    Dim bFaltaBilletesDesdeMenu As Boolean = False
    Dim sFastCashActivo As String = ""
    Dim bFastCashWaitActivo As Boolean = False
    Dim proteccionMontosRSTHasta As DateTime = DateTime.MinValue
    Dim proteccionMontosRSTPendienteHasta As DateTime = DateTime.MinValue
    Dim retiroSinTarjetaWelcomeHasta As DateTime = DateTime.MinValue
    Dim proteccionWelcomeHostHasta As DateTime = DateTime.MinValue
    Dim serviceOutController As ServiceOutController = Nothing
    Dim ultimoLogOperativo As DateTime = DateTime.MinValue
    Dim pantallaAntesTimeout As String = ""
    Dim backVisualPendiente As Boolean = False
    Dim backVisualUrlPendiente As String = ""
    Dim timeoutVisualEsperandoBack As Boolean = False
    Dim timeoutSobrePantallaBase As Boolean = False
    Dim timeoutOcultamientoPendiente As Boolean = False
    Dim timeoutWaitMostradoPorToque As Boolean = False
    Dim coberturaTarjetaWelcomeActiva As Boolean = False
    Dim readCardWelcomeUrl As String = ""

    ' >>> BANDERAS DE SINCRONIZACIÓN Y PROTECCIÓN <<<
    Dim baseScreenIsOnException As Boolean = False
    Dim keepCustomErrorPageActive As Boolean = False ' Protege las pantallas de error
    Dim keepCancelPageActive As Boolean = False ' Protege la cancelacion custom hasta regresar a welcome
    Dim pantallaHostReferenciaProtegida As String = ""
    Dim urlHostReferenciaProtegida As String = ""
    Dim proteccionHostReferenciaHasta As DateTime = DateTime.MinValue

    ' ============== Win32 API para región transparente ==============
    Private Declare Function CreateRectRgn Lib "gdi32" (ByVal X1 As Integer, ByVal Y1 As Integer, ByVal X2 As Integer, ByVal Y2 As Integer) As IntPtr
    Private Declare Function CombineRgn Lib "gdi32" (ByVal hDestRgn As IntPtr, ByVal hSrcRgn1 As IntPtr, ByVal hSrcRgn2 As IntPtr, ByVal nCombineMode As Integer) As Integer
    Private Declare Function SetWindowRgn Lib "user32" (ByVal hWnd As IntPtr, ByVal hRgn As IntPtr, ByVal bRedraw As Boolean) As Integer
    Private Declare Function DeleteObject Lib "gdi32" (ByVal hObject As IntPtr) As Boolean
    Private Const RGN_DIFF As Integer = 4
    Private transparentRegionApplied As Boolean = False

    Private Const WH_MOUSE_LL As Integer = 14
    Private Const WM_LBUTTONUP As Integer = &H202

    Private Delegate Function LowLevelMouseProc(nCode As Integer, wParam As IntPtr, lParam As IntPtr) As IntPtr

    <System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential)>
    Private Structure MousePoint
        Public X As Integer
        Public Y As Integer
    End Structure

    <System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential)>
    Private Structure MouseHookData
        Public Point As MousePoint
        Public MouseData As UInteger
        Public Flags As UInteger
        Public Time As UInteger
        Public ExtraInfo As UIntPtr
    End Structure

    <System.Runtime.InteropServices.DllImport("user32.dll", SetLastError:=True)>
    Private Shared Function SetWindowsHookEx(idHook As Integer, callback As LowLevelMouseProc, moduleHandle As IntPtr, threadId As UInteger) As IntPtr
    End Function

    <System.Runtime.InteropServices.DllImport("user32.dll", SetLastError:=True)>
    Private Shared Function UnhookWindowsHookEx(hookHandle As IntPtr) As Boolean
    End Function

    <System.Runtime.InteropServices.DllImport("user32.dll")>
    Private Shared Function CallNextHookEx(hookHandle As IntPtr, nCode As Integer, wParam As IntPtr, lParam As IntPtr) As IntPtr
    End Function

    <System.Runtime.InteropServices.DllImport("kernel32.dll", CharSet:=System.Runtime.InteropServices.CharSet.Auto, SetLastError:=True)>
    Private Shared Function GetModuleHandle(moduleName As String) As IntPtr
    End Function

    Private mouseHookHandle As IntPtr = IntPtr.Zero
    Private mouseHookCallback As LowLevelMouseProc = Nothing

    ' Variables para almacenar la configuración del INI justo antes de navegar
    Dim currentHoleRequired As Boolean = False
    Dim currentHoleY As Integer = 0
    Dim currentHoleHeight As Integer = 0

    Private Sub frmsstWait_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Trace("Inicio appScreens V06.04 - Escalamiento automatico")
        writeINI("LG", "RESULT", "", ConfigManager.WorkFile)
        writeINI("SCREENS", "NUM", "", ConfigManager.WorkFile)
        writeINI("SCREENS", "DATA", "", ConfigManager.WorkFile)
        writeINI("NDC", "EVENT", "", ConfigManager.strRutaInterface)
        writeINI("NDC", "EVENT_SUPER", "", ConfigManager.strRutaInterface)
        writeINI("NDC", "SUPER", "", ConfigManager.strRutaInterface)
        writeINI("NDC", "READ", "", ConfigManager.strRutaInterface)
        AsegurarParametroTimeoutHideDelay()

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
            Trace("Error al cargar parametros iniciales")
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
            InicializarWaitRegreso()
            serviceOutController = New ServiceOutController(Me, WebBrowser1)
            InstalarDetectorToqueRegreso()
        Catch ex As Exception
            Trace("Error al inicializar navegador local")
        End Try

        tmInterfasSuper.Enabled = True
        LimpiarMensajesObsoletosInicio()
        Me.Hide()
    End Sub

    Private Sub InstalarDetectorToqueRegreso()
        Try
            If mouseHookHandle <> IntPtr.Zero Then Exit Sub

            mouseHookCallback = AddressOf ProcesarToqueGlobal
            mouseHookHandle = SetWindowsHookEx(WH_MOUSE_LL, mouseHookCallback, GetModuleHandle(Nothing), 0)

            If mouseHookHandle = IntPtr.Zero Then
                Trace("No se pudo instalar detector de toque regreso. Win32=" &
                      System.Runtime.InteropServices.Marshal.GetLastWin32Error().ToString())
            Else
                Trace("Detector de toque regreso instalado")
            End If
        Catch ex As Exception
            Trace("Error instalando detector de toque regreso: " & ex.Message)
        End Try
    End Sub

    Private Sub DesinstalarDetectorToqueRegreso()
        Try
            If mouseHookHandle <> IntPtr.Zero Then
                UnhookWindowsHookEx(mouseHookHandle)
                mouseHookHandle = IntPtr.Zero
            End If
        Catch ex As Exception
            Trace("Error desinstalando detector de toque regreso: " & ex.Message)
        End Try
    End Sub

    Private Function EsToqueDeRegresoNativo(x As Integer, y As Integer) As Boolean
        Dim fdkBackNativo As Integer = 4
        Dim valorFdk As String = ReadIni("024", "BACKFDK", ConfigManager.ScreensFile).Trim()

        If valorFdk = "" Then
            valorFdk = ReadIni("PARAM", "NATIVE_BACK_FDK", ConfigManager.ScreensFile).Trim()
        End If

        If valorFdk <> "" Then Integer.TryParse(valorFdk, fdkBackNativo)
        If fdkBackNativo < 1 OrElse fdkBackNativo > 8 Then fdkBackNativo = 4

        Dim xReferencia As Integer = If(fdkBackNativo <= 4, 177, 850)
        Dim posicionesY() As Integer = {296, 440, 574, 713}
        Dim yReferencia As Integer = posicionesY((fdkBackNativo - 1) Mod 4)
        Dim pantalla As Rectangle = ClickEnVentana.ObtenerRectanguloVentana(screenEventTo)
        If pantalla.IsEmpty Then
            pantalla = Screen.FromHandle(Me.Handle).Bounds
        End If
        Dim escalaX As Double = pantalla.Width / 1024.0
        Dim escalaY As Double = pantalla.Height / 768.0
        Dim centroX As Integer = pantalla.Left + CInt(xReferencia * escalaX)
        Dim centroY As Integer = pantalla.Top + CInt(yReferencia * escalaY)
        Dim toleranciaX As Integer = Math.Max(120, CInt(200 * escalaX))
        Dim toleranciaY As Integer = Math.Max(55, CInt(85 * escalaY))

        Return Math.Abs(x - centroX) <= toleranciaX AndAlso
               Math.Abs(y - centroY) <= toleranciaY
    End Function

    Private Function ProcesarToqueGlobal(nCode As Integer, wParam As IntPtr, lParam As IntPtr) As IntPtr
        Try
            If nCode >= 0 AndAlso wParam.ToInt32() = WM_LBUTTONUP AndAlso
               regresoNativo024Activo AndAlso Not toqueRegresoNativoDetectado Then

                Dim datos As MouseHookData = DirectCast(
                    System.Runtime.InteropServices.Marshal.PtrToStructure(lParam, GetType(MouseHookData)),
                    MouseHookData)
                Dim esRegreso As Boolean = EsToqueDeRegresoNativo(datos.Point.X, datos.Point.Y)

                Trace("Toque durante 024: x=" & datos.Point.X & " y=" & datos.Point.Y &
                      " regreso=" & esRegreso.ToString())

                If esRegreso Then
                    toqueRegresoNativoDetectado = True
                    Trace("Regreso nativo detectado antes de 701; mostrando wait")

                    If Me.InvokeRequired Then
                        Me.BeginInvoke(New MethodInvoker(AddressOf MostrarWaitRegresoDesdeToque))
                    Else
                        MostrarWaitRegresoDesdeToque()
                    End If
                End If
            End If

            If nCode >= 0 AndAlso wParam.ToInt32() = WM_LBUTTONUP AndAlso
               timeoutVisualEsperandoBack AndAlso Not timeoutOcultamientoPendiente AndAlso
               Not timeoutWaitMostradoPorToque Then

                timeoutWaitMostradoPorToque = True
                Trace("Toque durante timeout visual; mostrando wait preventivo")

                If Me.InvokeRequired Then
                    Me.BeginInvoke(New MethodInvoker(AddressOf MostrarWaitRegresoDesdeTimeout))
                Else
                    MostrarWaitRegresoDesdeTimeout()
                End If
            End If
        Catch ex As Exception
            Trace("Error procesando toque nativo: " & ex.Message)
        End Try

        Return CallNextHookEx(mouseHookHandle, nCode, wParam, lParam)
    End Function

    Private Sub MostrarWaitRegresoDesdeToque()
        MostrarWaitRegreso()
    End Sub

    Private Sub MostrarWaitRegresoDesdeTimeout()
        Try
            If Not timeoutVisualEsperandoBack Then Exit Sub

            If MostrarWaitRegreso() Then
                If timerOcultarWaitTimeout IsNot Nothing Then
                    timerOcultarWaitTimeout.Stop()
                    timerOcultarWaitTimeout.Interval = 2500
                    timerOcultarWaitTimeout.Start()
                End If
                Trace("Timeout visual: wait preventivo visible")
            Else
                timeoutWaitMostradoPorToque = False
            End If
        Catch ex As Exception
            Trace("Error mostrando wait preventivo timeout: " & ex.Message)
            timeoutWaitMostradoPorToque = False
        End Try
    End Sub


    Private Function EsMenuHtmlActual() As Boolean
        Try
            Dim cs As String = currentScreen.Trim().ToLower()

            If cs = "" Then Return False

            ' Estados internos de AppScreens NO son pantallas HTML reales.
            If cs.StartsWith("pantalla_nativa_") Then Return False

            ' Solo consideramos menú cuando currentScreen apunta a un HTML real de menú.
            If Not cs.EndsWith(".html") Then Return False

            Return cs.Contains("\menu") OrElse cs.Contains("/menu")
        Catch ex As Exception
            Return False
        End Try
    End Function

    Private Sub InicializarWaitRegreso()
        Try
            If webBrowserWaitRegreso IsNot Nothing Then Exit Sub

            Dim sWaitUrl As String = ReadIni("300", "PAGE", ConfigManager.ScreensFile).Trim()
            If sWaitUrl = "" Then
                Trace("No se pudo precargar wait de regreso: estado 300 sin PAGE")
                Exit Sub
            End If

            webBrowserWaitRegreso = New WebBrowser()
            webBrowserWaitRegreso.Dock = DockStyle.Fill
            webBrowserWaitRegreso.ScrollBarsEnabled = False
            webBrowserWaitRegreso.ScriptErrorsSuppressed = True
            webBrowserWaitRegreso.AllowWebBrowserDrop = False
            webBrowserWaitRegreso.IsWebBrowserContextMenuEnabled = False
            webBrowserWaitRegreso.WebBrowserShortcutsEnabled = False
            webBrowserWaitRegreso.ObjectForScripting = Me
            webBrowserWaitRegreso.Visible = False

            Me.Controls.Add(webBrowserWaitRegreso)
            webBrowserWaitRegreso.BringToFront()
            AddHandler webBrowserWaitRegreso.DocumentCompleted, AddressOf WebBrowserWaitRegreso_DocumentCompleted

            timerOcultarWaitRegreso = New Timer()
            timerOcultarWaitRegreso.Interval = 350
            AddHandler timerOcultarWaitRegreso.Tick, AddressOf TimerOcultarWaitRegreso_Tick

            timerOcultarWaitTimeout = New Timer()
            timerOcultarWaitTimeout.Interval = 2500
            AddHandler timerOcultarWaitTimeout.Tick, AddressOf TimerOcultarWaitTimeout_Tick

            timerOcultarReadCardWelcome = New Timer()
            timerOcultarReadCardWelcome.Interval = 3000
            AddHandler timerOcultarReadCardWelcome.Tick, AddressOf TimerOcultarReadCardWelcome_Tick

            timerOcultarTimeoutInicial = New Timer()
            timerOcultarTimeoutInicial.Interval = 1000
            AddHandler timerOcultarTimeoutInicial.Tick, AddressOf TimerOcultarTimeoutInicial_Tick

            webBrowserWaitRegreso.Navigate(sWaitUrl)
            Trace("Precargando wait de regreso: " & sWaitUrl)
        Catch ex As Exception
            Trace("Error inicializando wait de regreso: " & ex.Message)
        End Try
    End Sub

    Private Sub PrepararWaitRegreso()
        Try
            If webBrowserWaitRegreso Is Nothing Then
                InicializarWaitRegreso()
            End If

            If webBrowserWaitRegreso Is Nothing OrElse waitRegresoCargado Then Exit Sub

            Dim sWaitUrl As String = ReadIni("300", "PAGE", ConfigManager.ScreensFile).Trim()
            If sWaitUrl <> "" Then
                webBrowserWaitRegreso.Navigate(sWaitUrl)
            End If
        Catch ex As Exception
            Trace("Error preparando wait de regreso: " & ex.Message)
        End Try
    End Sub

    Private Function UsarCoberturaSinOcultarPrincipal() As Boolean
        Dim valor As String = ReadIni("PARAM", "TRANSITION_RENDER_FIX", ConfigManager.ScreensFile).Trim().ToUpper()
        Return valor <> "FALSE" AndAlso valor <> "0" AndAlso valor <> "OFF"
    End Function

    Private Sub DejarWaitComoPrimeraCapaOculta()
        Try
            PrepararWaitRegreso()

            If webBrowserWaitRegreso Is Nothing OrElse Not waitRegresoCargado Then Exit Sub

            If timerOcultarWaitRegreso IsNot Nothing Then timerOcultarWaitRegreso.Stop()

            RestoreFullRegion()
            WebBrowser1.Visible = UsarCoberturaSinOcultarPrincipal()
            webBrowserWaitRegreso.Visible = True
            webBrowserWaitRegreso.BringToFront()
            Trace("024: formulario oculto preparado para abrir directamente con wait.html")
        Catch ex As Exception
            Trace("Error preparando primera capa de wait: " & ex.Message)
        End Try
    End Sub

    Private Function MostrarWaitRegreso() As Boolean
        Try
            PrepararWaitRegreso()

            If webBrowserWaitRegreso Is Nothing OrElse Not waitRegresoCargado Then
                Trace("Wait de regreso aun no esta cargado; se conserva el HTML actual")
                Return False
            End If

            If timerOcultarWaitRegreso IsNot Nothing Then timerOcultarWaitRegreso.Stop()

            RestoreFullRegion()
            waitRegresoActivo = True
            idCoberturaRender += 1
            WebBrowser1.Visible = UsarCoberturaSinOcultarPrincipal()
            webBrowserWaitRegreso.Visible = True
            webBrowserWaitRegreso.BringToFront()
            Me.Show()
            Me.BringToFront()
            Me.Update()
            Trace("TRANSICION cobertura visible id=" & idCoberturaRender.ToString())
            Return True
        Catch ex As Exception
            Trace("Error mostrando wait de regreso: " & ex.Message)
            Return False
        End Try
    End Function

    Private Sub ProgramarOcultarWaitRegreso()
        If Not waitRegresoActivo OrElse timerOcultarWaitRegreso Is Nothing Then Exit Sub

        timerOcultarWaitRegreso.Stop()
        timerOcultarWaitRegreso.Start()
    End Sub

    Private Sub OcultarWaitRegreso()
        If timerOcultarWaitRegreso IsNot Nothing Then timerOcultarWaitRegreso.Stop()

        If UsarCoberturaSinOcultarPrincipal() Then
            Dim idActual As Integer = idCoberturaRender
            waitRegresoActivo = False

            Try
                Me.BeginInvoke(New MethodInvoker(Sub() RetirarCoberturaRender(idActual)))
            Catch ex As Exception
                RetirarCoberturaRender(idActual)
            End Try
            Exit Sub
        End If

        If WebBrowser1 IsNot Nothing Then
            WebBrowser1.Visible = True
            WebBrowser1.Update()
        End If
        If webBrowserWaitRegreso IsNot Nothing Then webBrowserWaitRegreso.Visible = False
        If WebBrowser1 IsNot Nothing Then WebBrowser1.BringToFront()
        waitRegresoActivo = False
    End Sub

    Private Sub RetirarCoberturaRender(idEsperado As Integer)
        If idEsperado <> idCoberturaRender OrElse waitRegresoActivo Then Exit Sub

        If webBrowserWaitRegreso IsNot Nothing Then webBrowserWaitRegreso.Visible = False
        If WebBrowser1 IsNot Nothing Then
            WebBrowser1.Visible = True
            WebBrowser1.BringToFront()
            WebBrowser1.Invalidate(True)
            WebBrowser1.Update()
        End If

        Trace("TRANSICION cobertura retirada id=" & idEsperado.ToString())
    End Sub

    Private Sub TimerOcultarWaitRegreso_Tick(sender As Object, e As EventArgs)
        OcultarWaitRegreso()
        Trace("Wait de regreso retirado; menu HTML ya estaba listo")
    End Sub

    Private Sub TimerOcultarWaitTimeout_Tick(sender As Object, e As EventArgs)
        Try
            If timerOcultarWaitTimeout IsNot Nothing Then timerOcultarWaitTimeout.Stop()

            If timeoutVisualEsperandoBack Then
                Dim retornoPantallaBase As Boolean = timeoutSobrePantallaBase

                If webBrowserWaitRegreso IsNot Nothing Then webBrowserWaitRegreso.Visible = False
                waitRegresoActivo = False
                Me.Hide()
                timeoutVisualEsperandoBack = False
                timeoutSobrePantallaBase = False
                timeoutOcultamientoPendiente = False
                timeoutWaitMostradoPorToque = False

                If retornoPantallaBase Then
                    Trace("Timeout visual: wait retirado; pantalla base activa")
                Else
                    Trace("Timeout visual: wait preventivo retirado sin back")
                End If
            End If
        Catch ex As Exception
            Trace("Error retirando wait preventivo timeout: " & ex.Message)
        End Try
    End Sub

    Private Sub TimerOcultarTimeoutInicial_Tick(sender As Object, e As EventArgs)
        Try
            If timerOcultarTimeoutInicial IsNot Nothing Then timerOcultarTimeoutInicial.Stop()

            If timeoutOcultamientoPendiente AndAlso timeoutVisualEsperandoBack Then
                timeoutOcultamientoPendiente = False
                OcultarAppScreensPorTimeout()
            End If
        Catch ex As Exception
            Trace("Error ocultando timeout visual diferido: " & ex.Message, 2)
        End Try
    End Sub

    Private Sub TimerOcultarReadCardWelcome_Tick(sender As Object, e As EventArgs)
        Try
            If timerOcultarReadCardWelcome IsNot Nothing Then timerOcultarReadCardWelcome.Stop()

            If coberturaTarjetaWelcomeActiva Then
                FinalizarCoberturaTarjetaWelcome("fallback")
            End If
        Catch ex As Exception
            Trace("Error ocultando readCard desde welcome: " & ex.Message, 2)
        End Try
    End Sub

    Private Sub WebBrowserWaitRegreso_DocumentCompleted(sender As Object, e As WebBrowserDocumentCompletedEventArgs)
        Try
            If webBrowserWaitRegreso IsNot Nothing AndAlso webBrowserWaitRegreso.Url IsNot Nothing AndAlso
               e.Url.AbsolutePath = webBrowserWaitRegreso.Url.AbsolutePath Then
                AplicarResponsiveHtml(webBrowserWaitRegreso)
                waitRegresoCargado = True
                Trace("wait.html de regreso precargado")
            End If
        Catch ex As Exception
            Trace("Error al completar wait de regreso: " & ex.Message)
        End Try
    End Sub

    Private Function WebBrowserTienePaginaCargada(sUrl As String) As Boolean
        Try
            If WebBrowser1 Is Nothing OrElse WebBrowser1.Url Is Nothing OrElse sUrl.Trim() = "" Then
                Return False
            End If

            Dim rutaActual As String = WebBrowser1.Url.LocalPath
            Dim rutaDestino As String = sUrl.Trim()
            Dim uriDestino As Uri = Nothing

            If Uri.TryCreate(rutaDestino, UriKind.Absolute, uriDestino) AndAlso uriDestino.IsFile Then
                rutaDestino = uriDestino.LocalPath
            End If

            rutaActual = IO.Path.GetFullPath(rutaActual)
            rutaDestino = IO.Path.GetFullPath(rutaDestino)

            Return String.Equals(rutaActual, rutaDestino, StringComparison.OrdinalIgnoreCase)
        Catch ex As Exception
            Trace("Error comparando pagina cargada: " & ex.Message)
            Return False
        End Try
    End Function

    Private Sub IniciarTransicionMenuNdc()
        transicionMenuNdcPendiente = True

        If timer701TransicionMenu IsNot Nothing Then
            timer701TransicionMenu.Stop()
        End If

        Trace("Transicion menu/NDC iniciada; los 701 prematuros se diferiran")
    End Sub

    Private Sub Posponer701TransicionMenu()
        If timer701TransicionMenu Is Nothing Then
            timer701TransicionMenu = New Timer()
            AddHandler timer701TransicionMenu.Tick, AddressOf Timer701TransicionMenu_Tick
        End If

        timer701TransicionMenu.Interval = If(transicionRetiroFastCashPendiente, 8000, 500)

        If Not timer701TransicionMenu.Enabled Then
            timer701TransicionMenu.Start()
        End If

        Trace("701 diferido " & timer701TransicionMenu.Interval &
              " ms mientras se espera la pantalla NDC final")
    End Sub

    Private Sub CompletarTransicionMenuNdc()
        If timer701TransicionMenu IsNot Nothing Then
            timer701TransicionMenu.Stop()
        End If

        If transicionMenuNdcPendiente Then
            Trace("Respuesta NDC recibida; se descarta el 701 intermedio")
        End If

        transicionMenuNdcPendiente = False
        transicionRetiroFastCashPendiente = False
    End Sub

    Private Sub Timer701TransicionMenu_Tick(sender As Object, e As EventArgs)
        timer701TransicionMenu.Stop()

        If Not transicionMenuNdcPendiente Then Exit Sub

        transicionMenuNdcPendiente = False
        transicionRetiroFastCashPendiente = False
        Trace("No llego una pantalla NDC durante la espera; procesando 701 diferido")
        ProcesarPantalla("701")
    End Sub

    Private Function DebeDiferirMenuPorAdvertenciaHardware(esPaginaMenu As Boolean, esImpresionPrematura As Boolean) As Boolean
        Try
            If Not esPaginaMenu OrElse esImpresionPrematura Then Return False
            If advertenciaHardwareActiva OrElse pageException <> "" Then Return False

            Return IsPrinterError() OrElse AreAnyCassettesEmpty(False) OrElse IsCdmError()
        Catch ex As Exception
            Trace("Error evaluando prioridad de advertencia hardware: " & ex.Message, 2)
            Return False
        End Try
    End Function

    Private Sub CancelarMenuAdvertenciaHardwarePendiente(origen As String)
        Try
            If timerMenuAdvertenciaHardware IsNot Nothing Then timerMenuAdvertenciaHardware.Stop()

            If menuAdvertenciaHardwarePendiente Then
                Trace("Menu pendiente por advertencia cancelado: origen=" & origen)
            End If

            menuAdvertenciaHardwarePendiente = False
            menuAdvertenciaHardwareUrl = ""
        Catch ex As Exception
            Trace("Error cancelando menu pendiente por advertencia: " & ex.Message, 2)
        End Try
    End Sub

    Private Sub ProgramarMenuAdvertenciaHardware(sUrl As String)
        Try
            If timerMenuAdvertenciaHardware Is Nothing Then
                timerMenuAdvertenciaHardware = New Timer()
                AddHandler timerMenuAdvertenciaHardware.Tick, AddressOf TimerMenuAdvertenciaHardware_Tick
            End If

            menuAdvertenciaHardwarePendiente = True
            menuAdvertenciaHardwareUrl = sUrl

            timerMenuAdvertenciaHardware.Stop()
            timerMenuAdvertenciaHardware.Interval = 700
            timerMenuAdvertenciaHardware.Start()

            MostrarWaitRegreso()
            Trace("Menu diferido por prioridad de advertencia hardware: " & sUrl)
        Catch ex As Exception
            Trace("Error programando menu pendiente por advertencia: " & ex.Message, 2)
            menuAdvertenciaHardwarePendiente = False
            menuAdvertenciaHardwareUrl = ""
            WebBrowser1.Navigate(sUrl)
        End Try
    End Sub

    Private Sub TimerMenuAdvertenciaHardware_Tick(sender As Object, e As EventArgs)
        Try
            If timerMenuAdvertenciaHardware IsNot Nothing Then timerMenuAdvertenciaHardware.Stop()
            If Not menuAdvertenciaHardwarePendiente Then Exit Sub

            Dim sUrl As String = menuAdvertenciaHardwareUrl
            menuAdvertenciaHardwarePendiente = False
            menuAdvertenciaHardwareUrl = ""

            If advertenciaHardwareActiva OrElse pageException <> "" OrElse
               (WebBrowser1 IsNot Nothing AndAlso WebBrowser1.Url IsNot Nothing AndAlso
                WebBrowser1.Url.LocalPath.ToLowerInvariant().Contains("exp-85")) Then
                Trace("Menu diferido descartado; advertencia hardware activa")
                Exit Sub
            End If

            If sUrl = "" Then Exit Sub

            Trace("Menu diferido liberado sin advertencia hardware: " & sUrl)
            WebBrowser1.Navigate(sUrl)
        Catch ex As Exception
            Trace("Error liberando menu pendiente por advertencia: " & ex.Message, 2)
        End Try
    End Sub

    Private Function EsUrlFastCash(url As String) As Boolean
        Dim u As String = If(url, "").Trim().ToLower()
        Return u.EndsWith("\fastcash.html") OrElse
               u.EndsWith("/fastcash.html") OrElse
               u.EndsWith("\fastcash2.html") OrElse
               u.EndsWith("/fastcash2.html")
    End Function

    Private Function EsRetiroEfectivoFdk7Actual(sPosicion As String) As Boolean
        Try
            If sPosicion <> "7" OrElse WebBrowser1 Is Nothing OrElse WebBrowser1.Document Is Nothing Then
                Return False
            End If

            Dim botonRetiro As HtmlElement = WebBrowser1.Document.GetElementById("btnContinuar2")
            If botonRetiro Is Nothing Then Return False

            Dim src As String = If(botonRetiro.GetAttribute("src"), "").ToLower()
            Return src.Contains("retiro") OrElse src.Contains("1-19")
        Catch ex As Exception
            Trace("Error identificando boton de retiro FDK7: " & ex.Message)
            Return False
        End Try
    End Function

    Private Function EsUrlFaltaBilletesDinamico(url As String) As Boolean
        Dim u As String = If(url, "").Trim().ToLower()
        Return u.EndsWith("\faltabilletesdinamico.html") OrElse
               u.EndsWith("/faltabilletesdinamico.html")
    End Function

    Private Function EsUrlMontosRST(url As String) As Boolean
        Dim u As String = If(url, "").Trim().ToLower()
        Return u.EndsWith("\montos_rst.html") OrElse
               u.EndsWith("/montos_rst.html")
    End Function

    Private Function EsUrlWelcome(url As String) As Boolean
        Dim u As String = If(url, "").Trim().ToLower()
        Return u.EndsWith("\welcome.html") OrElse
               u.EndsWith("/welcome.html")
    End Function

    Private Function EsNdcMontosRST(contenido As String, url As String) As Boolean
        Return EsUrlMontosRST(url) AndAlso EsContenidoMontosRST(contenido)
    End Function

    Private Function EsContenidoMontosRST(contenido As String) As Boolean
        Dim texto As String = NormalizarTextoHost(contenido)
        Return texto.Contains("VALIDACION DE DENOMINACION") AndAlso
               texto.Contains("PARA RETIROS SIN TARJETA") AndAlso
               texto.Contains("ESTATUS OK")
    End Function

    Private Sub ActivarProteccionMontosRST()
        proteccionMontosRSTHasta = DateTime.Now.AddSeconds(4)
        proteccionMontosRSTPendienteHasta = DateTime.MinValue
        retiroSinTarjetaWelcomeHasta = DateTime.MinValue
        Trace("Proteccion Montos_RST activa por 4 segundos")
    End Sub

    Private Sub RetirarCoberturaMontosRST(motivo As String)
        Try
            If waitRegresoActivo Then
                OcultarWaitRegreso()
                Trace("Cobertura Montos_RST retirada por " & motivo)
            End If

            If EsUrlMontosRST(currentScreen) Then
                If WebBrowser1 IsNot Nothing Then
                    WebBrowser1.Visible = True
                    WebBrowser1.BringToFront()
                End If

                Me.Show()
                Me.TopMost = True
                Me.Activate()
                Trace("Montos_RST permanece visible tras retirar cobertura")
            Else
                Me.Hide()
            End If
        Catch ex As Exception
            Trace("Error retirando cobertura Montos_RST: " & ex.Message)
        End Try
    End Sub

    Private Function ProteccionMontosRSTActiva() As Boolean
        Return DateTime.Now <= proteccionMontosRSTHasta
    End Function

    Private Function ProteccionMontosRSTPendiente() As Boolean
        Return DateTime.Now <= proteccionMontosRSTPendienteHasta
    End Function

    Private Sub MarcarRetiroSinTarjetaDesdeWelcome()
        LimpiarProteccionWelcomeHost()
        retiroSinTarjetaWelcomeHasta = DateTime.Now.AddSeconds(8)
        proteccionMontosRSTPendienteHasta = DateTime.Now.AddMilliseconds(3500)
        Trace("Retiro sin tarjeta desde welcome detectado; se cubre transicion hacia Montos_RST")
    End Sub

    Private Sub LimpiarRetiroSinTarjetaDesdeWelcome()
        retiroSinTarjetaWelcomeHasta = DateTime.MinValue
        proteccionMontosRSTPendienteHasta = DateTime.MinValue
    End Sub

    Private Function RetiroSinTarjetaDesdeWelcomePendiente() As Boolean
        Return DateTime.Now <= retiroSinTarjetaWelcomeHasta
    End Function

    Private Sub ActivarProteccionWelcomeHost()
        proteccionWelcomeHostHasta = DateTime.Now.AddSeconds(8)
        Trace("Proteccion welcome contra MSG NDC residual activa")
    End Sub

    Private Sub LimpiarProteccionWelcomeHost()
        proteccionWelcomeHostHasta = DateTime.MinValue
    End Sub

    Private Function ProteccionWelcomeHostActiva() As Boolean
        Try
            If proteccionWelcomeHostHasta = DateTime.MinValue Then Return False
            If EsUrlWelcome(currentScreen) Then Return True
            If sCurrent = "500" Then Return True

            If WebBrowser1 IsNot Nothing AndAlso WebBrowser1.Url IsNot Nothing Then
                If EsUrlWelcome(WebBrowser1.Url.LocalPath) Then Return True
            End If

            Return DateTime.Now <= proteccionWelcomeHostHasta
        Catch ex As Exception
            Trace("Error evaluando proteccion welcome host: " & ex.Message, 1)
        End Try

        Return False
    End Function

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

    Private Sub BloquearNavegacionPorRechazoHost(ByRef sUrl As String, ByRef sPage As String, ByRef bPantalla As Boolean)
        isExpetionClosePageNDC = True
        isHostError = True
        keepCustomErrorPageActive = False
        isClosePage = False
        currentHoleRequired = False
        sUrl = ""
        sPage = ""
        bPantalla = False
        Me.Hide()
    End Sub

    Private Sub ActivarProteccionHostReferencia(pantalla As String, url As String)
        pantallaHostReferenciaProtegida = pantalla
        urlHostReferenciaProtegida = url
        proteccionHostReferenciaHasta = DateTime.Now.AddSeconds(2)
        Trace("Proteccion host referencia activa: pantalla=" & pantalla & " url=" & url)
    End Sub

    Private Sub LimpiarProteccionHostReferencia()
        pantallaHostReferenciaProtegida = ""
        urlHostReferenciaProtegida = ""
        proteccionHostReferenciaHasta = DateTime.MinValue
    End Sub

    Private Function ProteccionHostReferenciaActiva() As Boolean
        If pantallaHostReferenciaProtegida = "" Then Return False

        If DateTime.Now > proteccionHostReferenciaHasta Then
            Trace("Proteccion host referencia expirada: pantalla=" & pantallaHostReferenciaProtegida)
            LimpiarProteccionHostReferencia()
            Return False
        End If

        If urlHostReferenciaProtegida <> "" AndAlso currentScreen <> "" AndAlso
           Not currentScreen.Equals(urlHostReferenciaProtegida, StringComparison.OrdinalIgnoreCase) Then
            LimpiarProteccionHostReferencia()
            Return False
        End If

        Return True
    End Function

    Private Function EsUrlMenuHost(sUrl As String) As Boolean
        Try
            Dim urlLower As String = sUrl.Trim().ToLower()
            If urlLower = "" Then Return False
            If Not urlLower.EndsWith(".html") Then Return False
            If urlLower.Contains("menumore") Then Return False

            Return urlLower.Contains("\menu") OrElse urlLower.Contains("/menu")
        Catch ex As Exception
            Return False
        End Try
    End Function

    Private Function ResolverUrlHostNdc(ndc As MensajeNDC) As String
        If ndc Is Nothing Then Return ""

        If ndc.Layout <> "" Then
            Return ReadIni(ndc.Layout.Replace("P", ""), "PAGE", ConfigManager.ScreensFile)
        End If

        Dim pantallaHost As String = ExtraerPantallaHostDesdeReferencia(ndc)
        If pantallaHost <> "" Then
            Dim urlPantallaHost As String = ReadIni(pantallaHost, "PAGE", ConfigManager.ScreensFile)
            If urlPantallaHost <> "" Then Return urlPantallaHost
        End If

        If ndc.CodigoTransaccion <> "" Then
            Return ReadIni(ndc.CodigoTransaccion, "PAGE", ConfigManager.ScreensFile)
        End If

        Return ""
    End Function

    Private Function ResolverPageHostNdc(ndc As MensajeNDC) As String
        If ndc Is Nothing Then Return ""

        If ndc.Layout <> "" Then
            Dim layoutPage As String = ndc.Layout.Replace("P", "")
            If ReadIni(layoutPage, "PAGE", ConfigManager.ScreensFile) <> "" Then Return layoutPage
        End If

        Dim pantallaHost As String = ExtraerPantallaHostDesdeReferencia(ndc)
        If pantallaHost <> "" AndAlso ReadIni(pantallaHost, "PAGE", ConfigManager.ScreensFile) <> "" Then
            Return pantallaHost
        End If

        If ndc.CodigoTransaccion <> "" AndAlso
           ReadIni(ndc.CodigoTransaccion, "PAGE", ConfigManager.ScreensFile) <> "" Then
            Return ndc.CodigoTransaccion
        End If

        Return ""
    End Function

    Private Function ExtraerPantallaHostDesdeReferencia(ndc As MensajeNDC) As String
        If ndc Is Nothing Then Return ""

        Dim candidatos As String() = {ndc.Referencia, ndc.CodigoPresentacion}

        For Each candidato In candidatos
            If String.IsNullOrWhiteSpace(candidato) Then Continue For

            Dim soloDigitos As String = Regex.Replace(candidato, "\D", "")
            If soloDigitos.Length < 6 Then Continue For

            If soloDigitos.EndsWith("701") Then
                Dim pantalla As String = soloDigitos.Substring(soloDigitos.Length - 6, 3)
                If pantalla <> "000" AndAlso pantalla <> "701" Then Return pantalla
            End If
        Next

        Return ""
    End Function

    Private Sub AplicarDatosNdc(ndc As MensajeNDC)
        If ndc Is Nothing Then Exit Sub

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
        sComision = If(ndc.MontoFO <> "", ndc.MontoFO, ndc.DatoE)
    End Sub

    Private Sub RegistrarMenuHostSinNavegar(ndc As MensajeNDC, archivo As String)
        Try
            Dim urlHost As String = ResolverUrlHostNdc(ndc)

            If EsUrlMenuHost(urlHost) Then
                AplicarDatosNdc(ndc)
                sMenuActivo = urlHost
                sLastMenuUrl = urlHost
                Trace("Excepcion FDK activa: menu host conservado sin navegar (" & urlHost & ") desde " & archivo)
            End If
        Catch ex As Exception
            Trace("Error conservando menu host durante excepcion FDK: " & ex.Message)
        End Try
    End Sub

    Private Function GetMaxMsgAgeSeconds() As Integer
        Dim maxAgeSeconds As Integer = 300

        Try
            Dim iniValue As String = ReadIni("PARAM", "MAX_MSG_AGE_SECONDS", ConfigManager.ConfigAtmClientFile).Trim()
            If iniValue <> "" Then
                Integer.TryParse(iniValue, maxAgeSeconds)
            End If
        Catch ex As Exception
            Trace("Error leyendo MAX_MSG_AGE_SECONDS: " & ex.Message)
        End Try

        If maxAgeSeconds < 30 Then maxAgeSeconds = 300
        Return maxAgeSeconds
    End Function

    Private Function EsArchivoMsgObsoleto(archivo As String) As Boolean
        Try
            Dim lastWrite As DateTime = File.GetLastWriteTime(archivo)
            Dim edadSegundos As Double = DateTime.Now.Subtract(lastWrite).TotalSeconds

            If edadSegundos > GetMaxMsgAgeSeconds() Then
                Trace("MSG obsoleto ignorado/eliminado: " & archivo & " LastWrite=" & lastWrite.ToString("yyyy-MM-dd HH:mm:ss") & " EdadSeg=" & CInt(edadSegundos))
                Return True
            End If
        Catch ex As Exception
            Trace("Error evaluando antiguedad MSG: " & ex.Message)
        End Try

        Return False
    End Function

    Private Function HayExcepcionEsperandoRespuesta() As Boolean
        Return pageException <> "" AndAlso Not isExpetionClosePageNDC
    End Function

    Private Sub EliminarArchivoMsgSeguro(archivo As String)
        Try
            If File.Exists(archivo) Then
                SecureTraceArchive.ArchiveFileBeforeDelete(archivo)
                File.Delete(archivo)
            End If
        Catch ex As Exception
            Trace("Error al borrar archivo MSG: " & ex.Message)
        End Try
    End Sub

    Private Sub LimpiarMensajesObsoletosInicio()
        Try
            Dim archivos = ObtenerArchivosMSGOrdenados(ConfigManager.rutaMsg)
            For Each archivo In archivos
                If EsArchivoMsgObsoleto(archivo) Then
                    EliminarArchivoMsgSeguro(archivo)
                End If
            Next
        Catch ex As Exception
            Trace("Error limpiando MSG obsoletos al iniciar: " & ex.Message)
        End Try
    End Sub

    Private Function ExtraerUltimoBloqueNdc(contenidoOriginal As String, archivo As String) As String
        Try
            Dim patronInicio As String = "4" & Chr(28)
            Dim indices As New List(Of Integer)()
            Dim index As Integer = contenidoOriginal.IndexOf(patronInicio, StringComparison.Ordinal)

            While index >= 0
                If index = 0 OrElse contenidoOriginal(index - 1) = Chr(10) OrElse contenidoOriginal(index - 1) = Chr(13) Then
                    indices.Add(index)
                End If

                index = contenidoOriginal.IndexOf(patronInicio, index + patronInicio.Length, StringComparison.Ordinal)
            End While

            If indices.Count > 1 Then
                Trace("MSG con multiples bloques NDC (" & indices.Count & ") detectado: " & archivo & ". Se procesa solo el ultimo para evitar transacciones viejas mezcladas.")
                Return contenidoOriginal.Substring(indices(indices.Count - 1))
            End If
        Catch ex As Exception
            Trace("Error separando bloques NDC: " & ex.Message)
        End Try

        Return contenidoOriginal
    End Function

    Private Function NormalizarTextoHost(texto As String) As String
        If String.IsNullOrEmpty(texto) Then Return ""

        Dim limpio As String = texto.ToUpperInvariant()

        limpio = limpio.Replace(vbCr, " ")
        limpio = limpio.Replace(vbLf, " ")
        limpio = limpio.Replace(vbTab, " ")
        limpio = limpio.Replace(Chr(12), " ")
        limpio = limpio.Replace(Chr(15), " ")
        limpio = limpio.Replace(Chr(27), " ")
        limpio = limpio.Replace(Chr(28), " ")
        limpio = limpio.Replace(Chr(29), " ")
        limpio = limpio.Replace(Chr(160), " ")

        limpio = limpio.Replace("Á", "A")
        limpio = limpio.Replace("É", "E")
        limpio = limpio.Replace("Í", "I")
        limpio = limpio.Replace("Ó", "O")
        limpio = limpio.Replace("Ú", "U")

        While limpio.Contains("  ")
            limpio = limpio.Replace("  ", " ")
        End While

        Return limpio.Trim()
    End Function

    Private Function EsFaltaDenominacionHost(contenido As String) As Boolean
        Dim textoHost As String = NormalizarTextoHost(contenido)

        Return textoHost.Contains("MONTO O DENOMINACION NO PERMITIDO") OrElse
            textoHost.Contains("NO CONTAMOS CON BILLETES DE LA DENOMINACION SOLICITADA")
    End Function

    Private Function EsFaltaDenominacionPostDispensacion(ndc As MensajeNDC, contenido As String) As Boolean
        Dim textoHost As String = NormalizarTextoHost(contenido)

        Return ndc.CodigoTransaccion = "056" AndAlso
            (textoHost.Contains("NO SE PUDO DISPENSAR") OrElse textoHost.Contains("POSIBLE REVERSO"))
    End Function

    Private Function EsRechazoRetiroSinEfectivoHost(ndc As MensajeNDC, contenido As String) As Boolean
        Dim textoHost As String = NormalizarTextoHost(contenido)

        Return ndc.CodigoTransaccion = "054" AndAlso
            (textoHost.Contains("RECHAZO RETIRO-SIN EFECTIVO") OrElse
             (textoHost.Contains("RECHAZO RETIRO") AndAlso textoHost.Contains("SIN EFECTIVO")))
    End Function

    Private Function ExtraerCodigoRechazoHost(contenido As String) As String
        If String.IsNullOrWhiteSpace(contenido) Then Return ""

        Dim coincidencia As Global.System.Text.RegularExpressions.Match =
            Global.System.Text.RegularExpressions.Regex.Match(
                NormalizarTextoHost(contenido),
                "\bTRANSACCION\s+RECHAZADA\s*:?\s*(\d{3})\b",
                Global.System.Text.RegularExpressions.RegexOptions.IgnoreCase)

        If coincidencia.Success Then Return coincidencia.Groups(1).Value
        Return ""
    End Function

    Private Function ObtenerUrlRetiroNoCompletado() As String
        Try
            Dim welcomeUrl As String = ReadIni("500", "PAGE", ConfigManager.ScreensFile)

            If welcomeUrl <> "" Then
                Dim pagesPath As String = IO.Path.GetDirectoryName(welcomeUrl)
                If pagesPath <> "" Then
                    Return IO.Path.Combine(pagesPath, "RetiroNoCompletado.html")
                End If
            End If
        Catch ex As Exception
            Trace("Error obteniendo ruta RetiroNoCompletado: " & ex.Message)
        End Try

        Return "C:\appMain\html\pages\RetiroNoCompletado.html"
    End Function

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        Dim sValue As String = String.Empty
        Dim sData As String = String.Empty
        Dim sDataEnable As String = String.Empty

        Timer1.Enabled = False

        Try
            Dim servicioActivo As Boolean = ServicioFueraDeOperacionActivo()
            RegistrarLogOperativo()
            If servicioActivo Then Exit Sub

            sData = ReadIni("SCREENS", "DATA", ConfigManager.WorkFile).Trim()
            sValue = ReadIni("SCREENS", "NUM", ConfigManager.WorkFile).Trim()
            LogScreensNum(sValue)

            If timeoutVisualEsperandoBack AndAlso Not timeoutOcultamientoPendiente AndAlso
               (sValue <> "" OrElse sData <> "") Then
                MostrarWaitRetornoTimeout(sValue, sData)
            End If

            If coberturaTarjetaWelcomeActiva AndAlso
               (sValue = "150" OrElse sData = "KEYPIN") Then

                FinalizarCoberturaTarjetaWelcome(If(sValue = "150", "150", "KEYPIN"))

                If sValue = "150" Then
                    writeINI("SCREENS", "NUM", "", ConfigManager.WorkFile)
                    sValue = ""
                End If
            End If

            If sValue <> "" Then
                ClickEnVentana.MoverMouse00()
                tmMsgDevices.Enabled = False

                If sValue <> sCurrent OrElse EsComandoRepetible(sValue) Then
                    Trace("SCREENS.NUM recibido: " + sValue)

                    Dim allowScreen As Boolean = True

                    If (sValue = "150" OrElse sData = "KEYPIN") AndAlso
                       RetiroSinTarjetaDesdeWelcomePendiente() Then
                        LimpiarRetiroSinTarjetaDesdeWelcome()
                        Trace("Flujo PIN/tarjeta detectado; se cancela cobertura Montos_RST pendiente")
                    End If

                    If sValue = "300" AndAlso EsUrlWelcome(currentScreen) AndAlso
                       RetiroSinTarjetaDesdeWelcomePendiente() Then
                        proteccionMontosRSTPendienteHasta = DateTime.Now.AddMilliseconds(3500)
                        Trace("Ventana Montos_RST pendiente tras 300 de retiro sin tarjeta")
                    End If

                    If sValue = "500" OrElse sValue = "welcome" OrElse
                       sValue = "513" OrElse sValue = "hide" Then
                        LimpiarRetiroSinTarjetaDesdeWelcome()
                    End If

                    If (sValue = "300" OrElse sValue = "701") AndAlso
                       ProteccionMontosRSTActiva() Then
                        RetirarCoberturaMontosRST("estado " & sValue)
                        Trace("Ignorando " & sValue & " durante proteccion Montos_RST activa")
                        allowScreen = False
                    ElseIf sValue = "701" AndAlso ProteccionMontosRSTPendiente() Then
                        ActivarProteccionMontosRST()
                        RetirarCoberturaMontosRST("701 inmediato")
                        Trace("Ignorando 701 inmediato por ventana pendiente Montos_RST")
                        allowScreen = False
                    ElseIf bFaltaBilletesDesdeMenu AndAlso EsUrlFaltaBilletesDinamico(currentScreen) AndAlso
                       (sValue = "300" OrElse sValue = "701") Then
                        Trace("Ignorando " & sValue & " mientras FaltaBilletesDinamico está activo desde FastCash")
                        allowScreen = False
                    ElseIf sValue = "300" AndAlso sFastCashActivo <> "" AndAlso
                           EsUrlFastCash(currentScreen) AndAlso Not bFaltaBilletesDesdeMenu Then
                        bFastCashWaitActivo = True
                        Trace("FastCash activo entra a wait; si llega 701 se regresará a FastCash")
                    End If

                    If sValue = "701" AndAlso DateTime.Now < tiempoBloqueo701 Then
                        Trace("Ignorando 701 por periodo de gracia (Falta de billetes activa).")
                        allowScreen = False
                    ElseIf sValue <> "701" Then
                        tiempoBloqueo701 = DateTime.MinValue
                    End If

                    If sValue = "701" AndAlso bNDCPageActive AndAlso allowScreen Then
                        Trace("701 recibido con bNDCPageActive=True, currentScreen=" & currentScreen)

                        If pageException <> "" AndAlso Not isExpetionClosePageNDC Then
                            Trace("701 ignorado: pageException activa (" & pageException & ")")
                            allowScreen = False
                        ElseIf EsMenuHtmlActual() Then
                            Trace("701 con menu HTML activo, forzando bNDCPageActive=False y procesando")
                            bNDCPageActive = False
                        ElseIf currentScreen = "pantalla_nativa_texto" Then
                            If ProteccionMontosRSTActiva() Then
                                RetirarCoberturaMontosRST("701 nativo")
                            End If
                            Trace("701 recibido despues de pantalla nativa texto. Se ignora para no montar menu encima de pantalla nativa.")
                            currentScreen = "pantalla_nativa_texto_esperando_701"
                            allowScreen = False

                        ElseIf currentScreen = "pantalla_nativa_texto_esperando_701" Then
                            Trace("701 adicional despues de pantalla nativa texto. Se ignora; el flujo nativo sigue activo.")
                            allowScreen = False

                        Else
                            Trace("701 ignorado: bNDCPageActive=True y currentScreen no es menu")
                            allowScreen = False
                        End If
                    End If

                    If allowScreen Then
                        ProcesarPantalla(sValue)
                    End If
                End If

                writeINI("SCREENS", "NUM", "", ConfigManager.WorkFile)
            End If

            If sData <> "" Then
                ClickEnVentana.MoverMouse00()
                tmMsgDevices.Enabled = False
                If sData <> sCurrentData Then
                    sCurrentData = sData
                    If sCurrentData = "KEYPIN" Then sCurrentData = ""
                    If "0123456789*#".Contains(sCurrentData) Then sCurrentData = ""

                    Trace("SCREENS.DATA recibido: " + sData)
                    Trace("SCREENS.DATA currentScreen: " + currentScreen)

                    Select Case sData
                        Case "KEYPIN"
                            Trace("sCurrent: " + sCurrent)
                            sDataEnable = ReadIni(sCurrent, "DATA", ConfigManager.ScreensFile)
                            Trace("sDataEnable: " + sDataEnable)
                            If sDataEnable = "PIN" Then
                                WebBrowser1.Document.InvokeScript("recibeData")
                            End If
                        Case Else
                            Trace("SCREENS.DATA desconocido: " + sData + " currentScreen=" + currentScreen)
                    End Select
                End If

                writeINI("SCREENS", "DATA", "", ConfigManager.WorkFile)
            End If
        Catch ex As Exception
            Trace("Error procesando SCREENS: " + ex.Message)
        Finally
            Timer1.Enabled = True
        End Try
    End Sub

    Public Sub wb_beep()
        Try
        Catch ex As Exception
            Trace("Error en wb_beep: " & ex.Message)
        End Try
    End Sub

    Private Sub InvokeScriptOpcional(nombreScript As String)
        Try
            If WebBrowser1 IsNot Nothing AndAlso WebBrowser1.Document IsNot Nothing Then
                WebBrowser1.Document.InvokeScript(nombreScript)
            End If
        Catch ex As Exception
        End Try
    End Sub

    Private Sub AplicarEstadoDispositivosEnHtml(origen As String, Optional registrar As Boolean = False)
        Try
            If WebBrowser1 Is Nothing OrElse WebBrowser1.Document Is Nothing Then
                If registrar Then Trace("Estado visual no aplicado: documento no disponible. origen=" & origen)
                Return
            End If

            Dim sinEfectivo As Boolean = AreAnyCassettesEmpty(False)
            Dim errorImpresora As Boolean = IsPrinterError()
            Dim errorDispensador As Boolean = IsCdmError()
            Dim esWelcome As Boolean = WebBrowser1.Url IsNot Nothing AndAlso
                WebBrowser1.Url.LocalPath.ToLower().Contains("welcome")
            Dim mostrarAvisoSinEfectivo As Boolean = If(esWelcome,
                                                        ObtenerSinEfectivoEstable(registrar),
                                                        sinEfectivo)

            InvokeScriptOpcional(If(errorImpresora, "showErrPrinter", "hideErrPrinter"))
            InvokeScriptOpcional(If(errorImpresora, "hideBtnConsultaSaldo", "showBtnConsultaSaldo"))
            InvokeScriptOpcional(If(errorDispensador, "showErrCdm", "hideErrCdm"))
            InvokeScriptOpcional(If(isHostError, "showErrHost", "hideErrHost"))
            InvokeScriptOpcional(If(mostrarAvisoSinEfectivo OrElse isHostError, "showErrNoCash", "hideErrNoCash"))
            InvokeScriptOpcional(If(sinEfectivo OrElse isHostError, "hideBtnRetiro", "showBtnRetiro"))

            If registrar Then
                Trace("Estado visual aplicado: origen=" & origen &
                      " sinEfectivo=" & sinEfectivo.ToString() &
                      " avisoSinEfectivo=" & mostrarAvisoSinEfectivo.ToString() &
                      " impresora=" & errorImpresora.ToString() &
                      " dispensador=" & errorDispensador.ToString() &
                      " host=" & isHostError.ToString())
            End If
        Catch ex As Exception
            Trace("Error aplicando estado visual de dispositivos: " & ex.Message)
        End Try
    End Sub

    Public Sub wb_RevisaEstadusDevices()
        Try
            Trace("Revision de dispositivos solicitada desde HTML")
            sErrorImpresora = ""
            AplicarEstadoDispositivosEnHtml("solicitud_html", True)
            tmMsgDevices.Enabled = True
        Catch ex As Exception
            Trace("Error en wb_RevisaEstadusDevices: " & ex.Message)
        End Try
    End Sub

    Private Function AreAnyCassettesEmpty(Optional registrarDetalle As Boolean = True) As Boolean
        Try
            Dim c1 As Boolean = TieneBilletes("1")
            Dim c2 As Boolean = TieneBilletes("2")
            Dim c3 As Boolean = TieneBilletes("3")
            Dim c4 As Boolean = TieneBilletes("4")

            ' Si NINGUNO tiene billetes, están todos vacíos
            Dim allEmpty As Boolean = Not (c1 OrElse c2 OrElse c3 OrElse c4)
            If registrarDetalle Then
                Trace("Estado cassettes: C1=" & c1 & " C2=" & c2 & " C3=" & c3 & " C4=" & c4 & " todosVacios=" & allEmpty)
            End If

            Return allEmpty OrElse IsCdmError()
        Catch ex As Exception
            Trace("Error al revisar cassettes: " & ex.Message)
            Return False
        End Try
    End Function

    Private Function ObtenerSinEfectivoEstable(Optional registrar As Boolean = False) As Boolean
        Dim lecturaSinEfectivo As Boolean = AreAnyCassettesEmpty(False)

        If lecturaSinEfectivo Then
            sinEfectivoEstable = True
            inicioRecuperacionEfectivo = DateTime.MinValue
        ElseIf sinEfectivoEstable Then
            If inicioRecuperacionEfectivo = DateTime.MinValue Then
                inicioRecuperacionEfectivo = DateTime.Now
                If registrar Then Trace("Recuperacion de efectivo pendiente de confirmacion")
            ElseIf DateTime.Now.Subtract(inicioRecuperacionEfectivo).TotalSeconds >= 5 Then
                sinEfectivoEstable = False
                inicioRecuperacionEfectivo = DateTime.MinValue
                Trace("Recuperacion de efectivo confirmada")
            End If
        End If

        Return sinEfectivoEstable
    End Function

    Private Function IsPrinterError() As Boolean
        Try
            Dim status As String = ReadIni("PTR STATUS", "fwDevice", ConfigManager.workFileDevices).Trim().ToUpper()
            Return status = "HWERROR" OrElse status = "NODEVICE" OrElse status = "OFFLINE"
        Catch ex As Exception
            Trace("Error al revisar impresora: " & ex.Message)
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
            Trace("Error al revisar dispensador: " & ex.Message)
            Return False
        End Try
    End Function

    Public Sub clickPage(sPotition As String)
        Dim esRegresoMenuMore As Boolean =
            sPotition = "4" AndAlso
            currentScreen.ToLower().Contains("menumore")
        Dim esInicioRetiroFastCash As Boolean = EsRetiroEfectivoFdk7Actual(sPotition)
        Dim esRetiroSinTarjetaWelcome As Boolean =
            sPotition = "8" AndAlso EsUrlWelcome(currentScreen)

        Try
            dllInterfaceNdc.showScreenNDC(300)
        Catch ex As Exception
            Trace("Error al mandar interface")
        End Try

        sCurrent = ""
        sCurrentData = ""

        If esRetiroSinTarjetaWelcome Then
            MarcarRetiroSinTarjetaDesdeWelcome()
        End If

        Dim esExcepcion As Boolean = (pageException <> "" AndAlso pageException = sPotition)
        Dim esBack As Boolean = (fdkBack <> "" AndAlso fdkBack = sPotition)
        Dim urlRetornoExcepcion As String = currentScreen

        If esExcepcion AndAlso sMenuActivo <> "" Then
            urlRetornoExcepcion = sMenuActivo
        End If

        If esExcepcion Then
            pageException = ""
            sExp850OriginalUrl = ""
            sExp852OriginalUrl = ""
            bNDCPageActive = False
            advertenciaHardwareActiva = False
            Trace("Ejectua excepción FDK.. navega a menu guardado: " + urlRetornoExcepcion)

            If baseScreenIsOnException Then
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
                baseScreenIsOnException = False
            Else
                Trace("No se reportó pantalla de excepción nativa, es un error local, no enviamos clic físico.")
            End If

            If urlRetornoExcepcion <> "" Then
                Dim coberturaExpVisible As Boolean = MostrarWaitRegreso()
                If coberturaExpVisible Then
                    Trace("EXP: wait visible para regreso a menu")
                Else
                    Trace("EXP: wait no disponible para regreso a menu", 1)
                End If

                currentScreen = urlRetornoExcepcion
                WebBrowser1.Navigate(urlRetornoExcepcion)
                Me.Show()
            Else
                Trace("Excepcion FDK sin pagina de retorno; appScreens permanece oculto")
            End If

        ElseIf esBack AndAlso Not esRegresoMenuMore Then
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

            If esInicioRetiroFastCash Then
                transicionRetiroFastCashPendiente = True
                IniciarTransicionMenuNdc()
                MostrarWaitRegreso()
                Trace("Retiro desde menu: wait visible hasta recibir FastCash final")
            End If

            If esRegresoMenuMore Then
                regresoMenuMorePendiente = True
                Timer1.Interval = 30
                MostrarWaitRegreso()
                Trace("Regreso desde menuMore: wait mostrado inmediatamente despues de enviar FDK4")
            End If
        End If
    End Sub

    Public Sub clickPageWait(sPotition As String)
        clickPage(sPotition)
        Trace("FDK transaccional especifico: mostrando espera")
        ProcesarPantalla("300")
    End Sub

    ''' <summary>
    ''' Boton "Si" de exp-852-fic.html: es un click LOCAL, no se manda nada al host/switch.
    ''' Solo destapa el menu real que ya estaba listo antes de mostrar la advertencia ficticia.
    ''' </summary>
    Public Sub clickPageFicticia()
        Try
            Dim sUrlPendiente As String = menuPendienteTrasAdvertenciaFicticia
            menuPendienteTrasAdvertenciaFicticia = ""

            Trace("exp-852-fic: click ficticio 'Si'; se muestra el menu pendiente=" & sUrlPendiente & " sin notificar al host")

            If sUrlPendiente <> "" Then
                currentScreen = sUrlPendiente
                WebBrowser1.Navigate(sUrlPendiente)
            End If
        Catch ex As Exception
            Trace("Error en clickPageFicticia: " & ex.Message)
        End Try
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
            Case "multiplosRSTDin"
                CalcularValoresDinamicos()
                sretur = globalMultiplosDin
            Case "montoMaxRSTDin"
                CalcularValoresDinamicos()
                sretur = globalMontoMaximoDin
            Case "cajeroSinEfectivo"
                sretur = If(AreAnyCassettesEmpty(), "1", "0")
            Case "multiploMin"
                CalcularValoresDinamicos()
                sretur = globalMultiploMinimoDin
            Case "faltaBilletesDesdeMenu"
                sretur = If(bFaltaBilletesDesdeMenu, "1", "0")
            Case Else
                sretur = ""
        End Select

        Trace("Valor solicitado por HTML: " + sretur)
        Return sretur
    End Function

    Public Sub setBack()
        Try
            Trace("Boton regresar configurado")
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

        Try
            If ServicioFueraDeOperacionActivo() Then Exit Sub

            sDato = ReadIni("NDC", "EVENT", ConfigManager.strRutaInterface)
            sSuper = ReadIni("NDC", "SUPER", ConfigManager.strRutaInterface)

            If sDato = "1" Or sSuper = SupervisorRole Then
                Trace("Intento de configuracion: dato=" + sDato)
                Trace("Intento de configuracion: supervisor=" + sSuper)

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

                    Trace("Configuracion abierta; appScreens oculto")
                Catch ex As Exception
                    Trace("Error al abrir configuracion: " + ex.Message)
                End Try
            End If

            If sDato = "ENDSUPER" Then
                Trace("Supervisor finalizado")
                writeINI("NDC", "EVENT", "", ConfigManager.strRutaInterface)
                killSupervisor()
            End If
        Catch ex As Exception
            Trace("Error procesando interface supervisor: " & ex.Message)
        Finally
            tmInterfasSuper.Enabled = True
        End Try
    End Sub

    Private Sub checkMsg_Tick(sender As Object, e As EventArgs) Handles checkMsg.Tick
        checkMsg.Enabled = False

        Try
            If ServicioFueraDeOperacionActivo() Then Exit Sub

            procesaDatosNDC()
        Catch ex As Exception
            Trace("Error procesando mensajes NDC: " & ex.Message)
        Finally
            checkMsg.Enabled = True
        End Try
    End Sub

    Private Sub procesaDatosNDC()
        Dim archivos = ObtenerArchivosMSGOrdenados(ConfigManager.rutaMsg)
        Dim sUrl As String = String.Empty
        Dim bPantalla As Boolean = False
        Dim sImagen As String = String.Empty
        Dim sPage As String = ""
        Dim contenido As String = ""
        Dim hostErrorEnLote As Boolean = False
        Dim protegerHostReferenciaEnLote As Boolean = False
        Dim pantallaHostReferenciaEnLote As String = ""
        Dim urlHostReferenciaEnLote As String = ""

        For Each archivo In archivos
            If EsArchivoMsgObsoleto(archivo) Then
                EliminarArchivoMsgSeguro(archivo)
                Continue For
            End If

            Try
                contenido = IO.File.ReadAllText(archivo)
            Catch ex As Exception
                Trace("Error leer contenido: " + ex.Message)
                Exit Sub
            End Try

            contenido = ExtraerUltimoBloqueNdc(contenido, archivo)

            If String.IsNullOrWhiteSpace(contenido) Then
                EliminarArchivoMsgSeguro(archivo)
                Continue For
            End If

            Dim ndc = MensajeNDC.Parse(contenido)

            If HayExcepcionEsperandoRespuesta() Then
                RegistrarMenuHostSinNavegar(ndc, archivo)
                Trace("MSG no navegado por excepcion FDK activa; se conserva contexto y se omite HTML. fdk=" & pageException & " archivo=" & archivo)
                EliminarArchivoMsgSeguro(archivo)
                Continue For
            End If

            tmOut.Enabled = False
            writeINI("APP", "TIMEOUT", "", ConfigManager.WorkFile)

            Trace("NDC recibido: archivo=" & archivo & " tipo=" & ndc.TipoMensaje & " sec=" & ndc.Secuencia & " txn=" & ndc.CodigoTransaccion & " pantalla=" & ndc.Pantalla & " layout=" & ndc.Layout)

            Trace("Msg NDC: " & contenido)

            Dim esFaltaDenominacion As Boolean = EsFaltaDenominacionHost(contenido)
            Dim urlHostCandidata As String = ResolverUrlHostNdc(ndc)
            Dim pageHostCandidata As String = ResolverPageHostNdc(ndc)
            Dim pantallaHostReferencia As String = ExtraerPantallaHostDesdeReferencia(ndc)
            Dim urlPantallaHostReferencia As String = ""
            If pantallaHostReferencia <> "" Then
                urlPantallaHostReferencia = ReadIni(pantallaHostReferencia, "PAGE", ConfigManager.ScreensFile)
            End If
            Dim usarPantallaHostReferencia As Boolean =
                urlPantallaHostReferencia <> "" AndAlso Not EsUrlMenuHost(urlPantallaHostReferencia)
            Dim esMenuHostSegunIni As Boolean = EsUrlMenuHost(urlHostCandidata)
            Dim esError As Boolean = False
            Dim textoHost As String = NormalizarTextoHost(contenido)
            Dim codigoRechazo As String = ExtraerCodigoRechazoHost(contenido)
            Dim tieneRechazoHost As Boolean = textoHost.Contains("TRANSACCION RECHAZADA")
            Dim tieneRespuestaNoExitosa As Boolean =
                Not String.IsNullOrWhiteSpace(ndc.codigoRespuesta) AndAlso ndc.codigoRespuesta <> "000"
            Dim rechazoHostReal As Boolean =
                tieneRechazoHost AndAlso (codigoRechazo = "" OrElse codigoRechazo <> "000")
            Dim hostReportaProblema As Boolean = rechazoHostReal OrElse tieneRespuestaNoExitosa
            Dim esMontosRSTResidualEnWelcome As Boolean =
                EsContenidoMontosRST(contenido) AndAlso
                EsUrlWelcome(currentScreen) AndAlso
                Not RetiroSinTarjetaDesdeWelcomePendiente() AndAlso
                Not ProteccionMontosRSTPendiente()

            If esMontosRSTResidualEnWelcome Then
                Trace("MSG Montos_RST residual ignorado en welcome; se mantiene appScreens visible. archivo=" & archivo)
                LimpiarRetiroSinTarjetaDesdeWelcome()
                LimpiarProteccionWelcomeHost()
                EliminarArchivoMsgSeguro(archivo)
                Continue For
            End If

            If tieneRechazoHost OrElse tieneRespuestaNoExitosa Then
                Trace("Decision NDC host: txn=" & ndc.CodigoTransaccion &
                      " layout=" & ndc.Layout &
                      " rechazo=" & codigoRechazo &
                      " respuesta=" & ndc.codigoRespuesta &
                      " pageHost=" & pageHostCandidata &
                      " urlHost=" & urlHostCandidata &
                      " hostProblema=" & hostReportaProblema.ToString())
            End If

            If hostReportaProblema AndAlso
               tieneRechazoHost AndAlso
               contenido.Contains("TIPO DE TRANSACCION") AndAlso
               Not esFaltaDenominacion AndAlso
               Not usarPantallaHostReferencia AndAlso
               Not esMenuHostSegunIni AndAlso
               ProteccionWelcomeHostActiva() Then

                Trace("MSG NDC residual ignorado por proteccion welcome; se mantiene appScreens visible. archivo=" & archivo)
                isHostError = False
                isExpetionClosePageNDC = False
                pageException = ""
                sExp852OriginalUrl = ""
                sExp850OriginalUrl = ""
                EliminarArchivoMsgSeguro(archivo)
                Continue For
            End If

            If hostReportaProblema Then
                If Not esFaltaDenominacion Then
                    If usarPantallaHostReferencia Then
                        sPage = pantallaHostReferencia
                        sUrl = urlPantallaHostReferencia
                        bPantalla = True
                        protegerHostReferenciaEnLote = True
                        pantallaHostReferenciaEnLote = pantallaHostReferencia
                        urlHostReferenciaEnLote = urlPantallaHostReferencia
                        LoadHoleConfigFromIni(sPage)
                        Trace("Host envio rechazo con pantalla " & sPage & " configurada; se permite HTML: " & sUrl)
                    ElseIf esMenuHostSegunIni Then
                        Trace("Host envio rechazo/codigo no-000 con PAGE de menu por INI; se permite menu host: " & urlHostCandidata)
                    ElseIf contenido.Contains("TIPO DE TRANSACCION") Then
                        Trace("Encontro un error regresado por Host TIPO DE TRANSACCION; se oculta appScreens y no navega HTML local.")

                        If sMenuActivo <> "" OrElse sLastMenuUrl <> "" Then
                            Try
                                Dim menuBase As String = If(sMenuActivo <> "", sMenuActivo, sLastMenuUrl)
                                Dim basePath As String = menuBase.Substring(0, menuBase.LastIndexOf("\") + 1)

                                sMenuActivo = basePath & "menuWO-506-noPrint.html"
                                sLastMenuUrl = sMenuActivo

                                Trace("Ticket impreso, actualizando menu activo a: " & sMenuActivo)
                            Catch ex As Exception
                                Trace("Error al mutar url de menú: " & ex.Message)
                            End Try
                        End If
                    End If

                    If Not esMenuHostSegunIni AndAlso Not usarPantallaHostReferencia Then
                        esError = True
                    End If
                End If
            End If

            If Not String.IsNullOrWhiteSpace(ndc.codigoError) Then
                esError = True
            End If

            Dim esLayoutProtegido As Boolean = False
            If ndc.Layout = "P5280" OrElse ndc.Layout = "P5080" OrElse ndc.Layout = "P3520" OrElse ndc.Layout = "P3530" OrElse ndc.Layout = "P3540" OrElse ndc.Layout = "P6550" OrElse ndc.Layout = "P2655" Then
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

            ' Prestamo digital: el switch puede reportar el mismo rechazo (TIPO DE TRANSACCION +
            ' respuesta no exitosa) con distinto layout segun el puerto/switch que conteste.
            ' Tanto P2655 como P6550 tienen pantalla propia para este caso (INI ya mapea a su
            ' HTML correcto), pero el host manda el 513 de cierre casi de inmediato, asi que hay
            ' que proteger esa pantalla para que no la borre.
            Dim esRechazoPrestamoDigitalConPantallaPropia As Boolean =
                (ndc.Layout = "P2655" OrElse ndc.Layout = "P6550") AndAlso
                contenido.Contains("TIPO DE TRANSACCION") AndAlso
                tieneRespuestaNoExitosa

            If esLayoutProtegido AndAlso esError Then
                Trace("ESCUDO INTELIGENTE: Ignorando error del Host en layout protegido: " & ndc.Layout)
                esError = False
            ElseIf esError Then
                Trace("Encontro un error real regresado por Host, procediendo a ocultar appScreens.")
                isHostError = True
                Trace("Bandera isHostError activada para interceptar pantalla 513")
            End If

            Dim esError014 As Boolean =
                textoHost.Contains("TARJETA INVALIDA") OrElse
                ndc.codigoRespuesta = "014" OrElse
                ndc.codigoError = "014" OrElse
                codigoRechazo = "014"

            If esError014 Then
                Trace("Detectado error 014 real (Tarjeta Inválida). Activando isHostError.")
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
                hostErrorEnLote = True
                sExp852OriginalUrl = currentScreen
                sExp850OriginalUrl = ""
                pageException = "8"
                BloquearNavegacionPorRechazoHost(sUrl, sPage, bPantalla)
                Trace("procesaDatosNDC: host error detectado => isHostError=True, sExp852OriginalUrl=" & sExp852OriginalUrl)
                Trace("Rechazo de host: se bloquea navegación HTML local y se deja visible la pantalla nativa.")
            End If

            If contenido.Contains("TRANSACCION RECHAZADA") AndAlso contenido.Contains("TIPO DE TRANSACCION") AndAlso Not esFaltaDenominacion Then
                If Not hasFallbackPlanB AndAlso ndc.Layout = "" Then
                    Me.Hide()
                End If
            End If

            Dim omitirFaltaDenominacionPostDispensacion As Boolean = False
            Dim esRechazoRetiroSinEfectivo As Boolean = EsRechazoRetiroSinEfectivoHost(ndc, contenido)

            If esFaltaDenominacion Then
                omitirFaltaDenominacionPostDispensacion = EsFaltaDenominacionPostDispensacion(ndc, contenido)

                If omitirFaltaDenominacionPostDispensacion Then
                    Trace("Falta de billetes por reverso/no dispensado; no se muestra MontoNoPermitido.")
                    sUrl = ""
                    sPage = ""
                    bPantalla = False
                Else
                    CalcularValoresDinamicos()
                    bFaltaBilletesDesdeMenu = False
                    sPage = "PW0003"
                    Trace("Falta de billetes detectada por Host; no se muestra FaltaBilletesDinamico fuera de FastCash.")

                    sUrl = ReadIni(sPage, "PAGE", ConfigManager.ScreensFile)
                    LoadHoleConfigFromIni(sPage)
                    Trace("sURL Msg: " & sUrl)

                    If sUrl = "" Then
                        tiempoBloqueo701 = DateTime.Now.AddSeconds(6)
                        Trace("Iniciando cooldown de 6 segundos para ignorar el Menú (701).")
                    End If
                End If
            End If

            If esRechazoRetiroSinEfectivo Then
                sPage = "RetiroNoCompletado"
                sUrl = ObtenerUrlRetiroNoCompletado()
                bPantalla = False
                keepCustomErrorPageActive = True
                isHostError = False
                isExpetionClosePageNDC = False
                Trace("Rechazo retiro sin efectivo detectado; mostrando pantalla custom: " & sUrl)
            End If

            If Not esRechazoRetiroSinEfectivo AndAlso Not omitirFaltaDenominacionPostDispensacion AndAlso Not esError AndAlso ndc.Layout <> "" AndAlso sUrl = "" Then
                sPage = ndc.Layout
                bPantalla = True
                sPage = sPage.Replace("P", "")
                sUrl = ReadIni(sPage, "PAGE", ConfigManager.ScreensFile)
                LoadHoleConfigFromIni(sPage)
                Trace("sURL Msg: " & sUrl)

                If esRechazoPrestamoDigitalConPantallaPropia AndAlso sUrl <> "" Then
                    protegerHostReferenciaEnLote = True
                    pantallaHostReferenciaEnLote = sPage
                    urlHostReferenciaEnLote = sUrl
                    Trace("Prestamo digital con pantalla propia (" & sUrl & "); se protege contra 513 inmediato")
                End If
            ElseIf Not esRechazoRetiroSinEfectivo AndAlso Not omitirFaltaDenominacionPostDispensacion AndAlso Not esError AndAlso ndc.CodigoTransaccion <> "" AndAlso sUrl = "" Then
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

            Dim hostRegresoAMenuExitoso As Boolean =
                ndc.codigoRespuesta = "000" AndAlso
                ((sUrl <> "" AndAlso EsUrlMenuHost(sUrl)) OrElse EsUrlMenuHost(urlHostCandidata))

            If hostRegresoAMenuExitoso Then
                If isHostError OrElse isExpetionClosePageNDC OrElse pageException <> "" Then
                    Trace("Host regreso a menu exitoso; limpiando bandera de error host")
                End If

                isHostError = False
                isExpetionClosePageNDC = False
                pageException = ""
            End If

            If Not hostErrorEnLote AndAlso sUrl <> "" AndAlso EsUrlMenuHost(sUrl) Then
                If isHostError OrElse isExpetionClosePageNDC OrElse pageException <> "" Then
                    Trace("Menu recibido desde host; limpiando bandera de error host")
                End If

                isHostError = False
                isExpetionClosePageNDC = False
                pageException = ""
            End If

            If sUrl <> "" Then
                AplicarDatosNdc(ndc)
            End If

            Try
                EliminarArchivoMsgSeguro(archivo)
            Catch ex As Exception
                Trace("Error al borrar archivo")
            End Try
        Next

        If hostErrorEnLote Then
            Trace("Host error en lote: navegación final cancelada para no montar HTML sobre pantalla nativa.")
            ocultarPantalla()
            Exit Sub
        End If

        If sUrl <> "" Then
            CompletarTransicionMenuNdc()

            Dim sUrlLower As String = sUrl.ToLower()
            Dim esPaginaMenu As Boolean = sUrlLower.Contains("menu") AndAlso Not sUrlLower.Contains("menumore")
            Dim esFastCash As Boolean = EsUrlFastCash(sUrl)

            ' Prevenir que el menú tape la pantalla de impresión.
            ' Algunos mensajes de impresión no traen "TIPO DE TRANSACCION";
            ' por ejemplo consulta de saldo con impresión puede venir como P6610
            ' y contener "IMPRESION CONSULTA DE SALDO" + bloque de recibo.
            Dim esMensajeConTicket As Boolean =
                contenido.Contains("IMPRESION") OrElse
                contenido.Contains("FOLIO.") OrElse
                contenido.Contains("NUM-AUTORIZACION") OrElse
                contenido.Contains("COMISION POR USO ATM") OrElse
                contenido.Contains("TARJETA:") OrElse
                contenido.Contains("CTA. DE AHORRO") OrElse
                contenido.Contains("SALDO TOT.") OrElse
                contenido.Contains("DISPONIBLE:") OrElse
                contenido.Contains(Chr(29) & "2")

            Dim esImpresionPrematura As Boolean = esPaginaMenu AndAlso esMensajeConTicket

            If esFastCash Then
                If waitRegresoActivo Then
                    navegacionFastCashCubierta = True
                Else
                    navegacionFastCashCubierta = MostrarWaitRegreso()
                End If
            End If

            ' Si es una impresión prematura, NO mostramos la pantalla aún
            If Not isExpetionClosePageNDC AndAlso Not esImpresionPrematura Then
                If Not esFastCash OrElse Not navegacionFastCashCubierta Then
                    Me.Show()
                End If
            End If

            Trace("Mostrando pantalla HTML por mensaje NDC")
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

            If protegerHostReferenciaEnLote Then
                ActivarProteccionHostReferencia(pantallaHostReferenciaEnLote, urlHostReferenciaEnLote)
            End If

            If EsNdcMontosRST(contenido, sUrl) Then
                ActivarProteccionMontosRST()
                RetirarCoberturaMontosRST("mensaje Montos_RST")
            End If

            If esFastCash Then
                urlFastCashGlobal = sUrl
                sFastCashActivo = sUrl
                bFastCashWaitActivo = False
            End If

            If esPaginaMenu Then
                ' Guardamos el menú real que mandó el host/switch.
                ' Este se mantiene como menú activo hasta que otro menú lo reemplace.
                sMenuActivo = sUrl
                sLastMenuUrl = sUrl
                Trace("Menu activo actualizado por host: " & sMenuActivo)
            End If

            ' Agregamos la condición para que no dispare alertas si está imprimiendo
            If (esPaginaMenu OrElse esFastCash) AndAlso Not esImpresionPrematura Then
                Dim fallaDispensador As Boolean = AreAnyCassettesEmpty() OrElse IsCdmError()
                Dim c1 As Boolean = TieneBilletes("1")
                Dim c2 As Boolean = TieneBilletes("2")
                Dim c3 As Boolean = TieneBilletes("3")
                Dim c4 As Boolean = TieneBilletes("4")
                Dim faltaAlgunaDenominacion As Boolean = (Not c1 OrElse Not c2 OrElse Not c3 OrElse Not c4) AndAlso Not fallaDispensador

                If faltaAlgunaDenominacion AndAlso esFastCash AndAlso
                       Not advertenciaDenominacionMostrada Then
                    CalcularValoresDinamicos()
                    bFaltaBilletesDesdeMenu = True
                    bFastCashWaitActivo = False
                    Dim pantallaFalta As String = "746"
                    Trace("Falta denominación detectada PROACTIVAMENTE en FastCash. Múltiplos: " & globalMultiplosDin & " Max: " & globalMontoMaximoDin)

                    advertenciaDenominacionMostrada = True
                    sExp852OriginalUrl = ""
                    sExp850OriginalUrl = ""
                    currentScreen = sUrl

                    pageException = ""
                    isExpetionClosePageNDC = False
                    Me.Show()

                    Dim urlFalta As String = ReadIni(pantallaFalta, "PAGE", ConfigManager.ScreensFile)
                    If urlFalta <> "" Then
                        sUrl = urlFalta
                        currentScreen = urlFalta
                        LoadHoleConfigFromIni(pantallaFalta)
                    End If
                Else
                    If esFastCash Then
                        Trace("FastCash sin falla de hardware; se mantiene visible appScreens")
                        isExpetionClosePageNDC = False
                        Me.Show()
                    End If
                    sExp852OriginalUrl = ""
                    sExp850OriginalUrl = ""
                End If
            End If

            ' Abortamos la navegación prematura y nos ocultamos
            If esImpresionPrematura Then
                Trace("Impresion en curso detectada por ticket/recibo; mostrando espera y conservando menu activo para 701: " & sUrl)

                ' Aunque no naveguemos todavía al menú, este ya es el nuevo menú activo.
                ' Ejemplo: menu661.html después de imprimir consulta de saldo.
                sMenuActivo = sUrl
                sLastMenuUrl = sUrl
                Trace("Menu activo actualizado por impresion: " & sMenuActivo)

                Dim sWaitUrl As String = ReadIni("300", "PAGE", ConfigManager.ScreensFile)

                If sWaitUrl <> "" Then
                    currentScreen = sWaitUrl
                    WebBrowser1.Navigate(sWaitUrl)
                    Me.Show()
                Else
                    Trace("No se encontro PAGE para estado 300; se oculta appScreens como respaldo")
                    Me.Hide()
                End If

                bNDCPageActive = False
            Else
                Trace("Navega page Url DatosNDC: " + sUrl)

                If EsUrlFastCash(sUrl) Then
                    If waitRegresoActivo Then
                        navegacionFastCashCubierta = True
                    Else
                        navegacionFastCashCubierta = MostrarWaitRegreso()
                    End If
                Else
                    navegacionFastCashCubierta = False
                End If

                If esPaginaMenu AndAlso Not bAdvertenciaFicticiaMostrada AndAlso
                   (AreAnyCassettesEmpty() OrElse IsCdmError()) AndAlso
                   ReadIni("852FIC", "PAGE", ConfigManager.ScreensFile).Trim() <> "" Then

                    Dim sUrlFic As String = ReadIni("852FIC", "PAGE", ConfigManager.ScreensFile).Trim()
                    bAdvertenciaFicticiaMostrada = True
                    menuPendienteTrasAdvertenciaFicticia = sUrl
                    Trace("Advertencia hardware local (ficticia): interceptando menu con exp-852-fic, menu real pendiente=" & sUrl)
                    currentScreen = sUrlFic
                    WebBrowser1.Navigate(sUrlFic)
                ElseIf DebeDiferirMenuPorAdvertenciaHardware(esPaginaMenu, esImpresionPrematura) Then
                    ProgramarMenuAdvertenciaHardware(sUrl)
                Else
                    WebBrowser1.Navigate(sUrl)
                End If
            End If


        Else
            ' No hubo PAGE para este layout/código; dejamos pasar pantalla nativa.
            ' Limpiamos menu previo para evitar que un 701/513 posterior regrese al menu anterior.
            sLastMenuUrl = ""

            ' La respuesta del host ya llego (aunque sea pantalla nativa sin HTML propio).
            ' Cancelamos el 701 diferido para no forzar un regreso al menu.
            CompletarTransicionMenuNdc()

            sImagen = ReadIni(sPage, "PIC", ConfigManager.ScreensFile)
            If sImagen <> "" Then
                Try
                    dllInterfaceNdc.showScreenNDC(CInt(sImagen))
                    Trace("Mostrando pantalla nativa por PIC")
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

                    If EsContenidoMontosRST(contenido) Then
                        ActivarProteccionMontosRST()
                        RetirarCoberturaMontosRST("texto nativo Montos_RST")
                    End If
                End If
            End If
        End If
    End Sub

    Private Sub ProcesarPantalla(sValue As String)
        Dim sUrl As String = String.Empty
        Dim sImagen As String = String.Empty
        Dim sDataEnable As String = String.Empty
        Dim sIdioma As String

        If sValue = "300" Then
            If ProteccionMontosRSTActiva() Then
                RetirarCoberturaMontosRST("300 residual")
                Trace("300 residual ignorado durante proteccion Montos_RST activa")
                Exit Sub
            End If

            If EsMenuHtmlActual() Then
                IniciarTransicionMenuNdc()
            End If

            If RetiroSinTarjetaDesdeWelcomePendiente() AndAlso ProteccionMontosRSTPendiente() Then
                MostrarWaitRegreso()
                Trace("300 de retiro sin tarjeta: wait como cobertura; se omite navegar WebBrowser a wait.html")
                Exit Sub
            End If

            MostrarWaitRegreso()
        ElseIf sValue = "701" AndAlso transicionMenuNdcPendiente Then
            Posponer701TransicionMenu()
            Exit Sub
        ElseIf sValue = "500" OrElse sValue = "welcome" OrElse
               sValue = "513" OrElse sValue = "hide" Then
            CompletarTransicionMenuNdc()
        End If

        If sValue = "024" Then
            regresoNativo024Activo = EsMenuHtmlActual()
            toqueRegresoNativoDetectado = False

            If regresoNativo024Activo Then
                PrepararWaitRegreso()
                Timer1.Interval = 30
                Trace("024 desde menu: wait preparado y polling de regreso a 30ms")
            End If
        ElseIf sValue = "701" Then
            Timer1.Interval = 300

            If (regresoNativo024Activo OrElse regresoMenuMorePendiente) AndAlso Not waitRegresoActivo Then
                MostrarWaitRegreso()
            End If

            regresoNativo024Activo = False
            regresoMenuMorePendiente = False
            toqueRegresoNativoDetectado = False
        ElseIf sValue = "500" OrElse sValue = "welcome" OrElse sValue = "513" Then
            regresoNativo024Activo = False
            regresoMenuMorePendiente = False
            toqueRegresoNativoDetectado = False
            Timer1.Interval = 300
            OcultarWaitRegreso()

            If timeoutVisualEsperandoBack Then
                timeoutVisualEsperandoBack = False
                timeoutWaitMostradoPorToque = False
                If timerOcultarWaitTimeout IsNot Nothing Then timerOcultarWaitTimeout.Stop()
                Trace("Timeout visual: wait retirado por regreso directo a " & sValue)
            End If
        End If

        Dim esEstadoAdvertenciaHardware As Boolean =
            sValue = "850" OrElse sValue = "851" OrElse sValue = "852"

        If menuAdvertenciaHardwarePendiente AndAlso Not esEstadoAdvertenciaHardware Then
            If sValue = "701" Then
                Trace("701 ignorado durante prioridad de advertencia hardware")
                Exit Sub
            End If

            CancelarMenuAdvertenciaHardwarePendiente(sValue)
        End If

        If esEstadoAdvertenciaHardware AndAlso pageException <> "" AndAlso
           WebBrowser1 IsNot Nothing AndAlso WebBrowser1.Url IsNot Nothing AndAlso
           WebBrowser1.Url.LocalPath.ToLower().Contains("exp-85") Then
            Trace("Estado " & sValue & " ignorado: el HTML de excepcion ya esta visible")
            isExpetionClosePageNDC = False
            baseScreenIsOnException = True
            Exit Sub
        End If

        If sValue = "850" AndAlso sExp850OriginalUrl <> "" Then
            Trace("850 ignorado: exp-850 ya estaba activo; se mantiene bandera de excepcion nativa")
            isExpetionClosePageNDC = False
            baseScreenIsOnException = True
            Exit Sub
        End If

        If sValue = "850" AndAlso sExp852OriginalUrl <> "" AndAlso
   WebBrowser1.Url IsNot Nothing AndAlso
   WebBrowser1.Url.LocalPath.ToLower().EndsWith("exp-851.html") Then

            Trace("850 ignorado: exp-851 ya estaba activo por falla combinada")
            isExpetionClosePageNDC = False
            baseScreenIsOnException = True
            Exit Sub
        End If

        If sValue = "851" AndAlso sExp852OriginalUrl <> "" Then
            Trace("851 ignorado: exp-851 ya estaba activo; se mantiene bandera de excepcion nativa")
            isExpetionClosePageNDC = False
            baseScreenIsOnException = True
            Exit Sub
        End If

        If sValue = "852" AndAlso sExp852OriginalUrl <> "" Then
            Trace("852 ignorado: exp-852 ya estaba activo; se mantiene bandera de excepcion nativa")
            isExpetionClosePageNDC = False
            baseScreenIsOnException = True
            Exit Sub
        End If

        If esEstadoAdvertenciaHardware Then
            CancelarMenuAdvertenciaHardwarePendiente(sValue)

            If advertenciaHardwareActiva Then
                Trace("Estado " & sValue & " duplicado: la advertencia actual sigue activa")
                Exit Sub
            End If

            Dim estadoRecibido As String = sValue
            Dim fallaDispensadorActual As Boolean = AreAnyCassettesEmpty() OrElse IsCdmError()
            Dim fallaImpresoraActual As Boolean = IsPrinterError()

            If fallaDispensadorActual AndAlso fallaImpresoraActual Then
                sValue = "851"
            ElseIf fallaDispensadorActual Then
                sValue = "852"
            ElseIf fallaImpresoraActual Then
                sValue = "850"
            End If

            MostrarWaitRegreso()

            advertenciaHardwareActiva = True
            ' El switch ya mando su propia advertencia real (850/851/852);
            ' no hace falta que ademas se muestre la version ficticia local.
            bAdvertenciaFicticiaMostrada = True

            If sValue = "850" Then
                sExp850OriginalUrl = currentScreen
                sExp852OriginalUrl = ""
            Else
                sExp852OriginalUrl = currentScreen
                sExp850OriginalUrl = ""
            End If

            Trace("Advertencia de hardware: estado recibido=" & estadoRecibido &
                  " HTML seleccionado=exp-" & sValue &
                  " impresora=" & fallaImpresoraActual.ToString() &
                  " cdm=" & fallaDispensadorActual.ToString())
        End If

        If (sValue = "200" OrElse sValue = "300") AndAlso currentScreen.ToLower().Contains("confirmacionpagotdc") AndAlso IsPrinterError() Then
            Trace("Interceptando " & sValue & ": Mantenemos oculto appScreens para dejar visible la pregunta nativa (Falla Impresora)")
            sValue = "hide"
        End If

        ' >>> FIX: Cuando sale la pantalla de excepción (nativa) por segunda o más veces,
        ' encendemos la bandera para obligar a que el clic del usuario se envíe al flujo nativo.
        If sValue = "850" OrElse sValue = "851" OrElse sValue = "852" Then
            baseScreenIsOnException = True
        Else
            baseScreenIsOnException = False
        End If

        pageException = ""

        If sValue = "welcome" OrElse sValue = "500" OrElse sValue = "hide" Then
            keepCancelPageActive = False
        End If

        If keepCancelPageActive AndAlso (sValue = "513" OrElse sValue = "701") Then
            Trace(sValue & " ignorado para mantener visible la pantalla custom de cancelacion.")
            Exit Sub
        End If

        ' >>> BARRERA PROTECTORA DE PANTALLA CUSTOM <<<
        If sValue = "welcome" OrElse sValue = "500" OrElse sValue = "701" OrElse sValue = "hide" Then
            keepCustomErrorPageActive = False
        End If

        If sValue = "513" AndAlso keepCustomErrorPageActive Then
            Trace("513 ignorado proactivamente para mantener visible la pantalla de error custom.")
            Exit Sub
        End If

        If sValue = "513" AndAlso ProteccionHostReferenciaActiva() Then
            Trace("513 ignorado para mantener pantalla NDC por referencia: pantalla=" &
                  pantallaHostReferenciaProtegida & " url=" & urlHostReferenciaProtegida)
            Exit Sub
        End If

        If sValue = "welcome" OrElse sValue = "500" OrElse sValue = "200" OrElse sValue = "hide" Then
            LimpiarProteccionHostReferencia()
        End If

        If sValue = "welcome" Then
            CancelarMenuAdvertenciaHardwarePendiente("welcome")
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
            isExpetionClosePageNDC = False
            bFaltaBilletesDesdeMenu = False
            bFastCashWaitActivo = False
            sFastCashActivo = ""
            urlFastCashGlobal = ""
            baseScreenIsOnException = False

            If sValue = "500" Then
                ActivarProteccionWelcomeHost()
                CancelarMenuAdvertenciaHardwarePendiente("500")
                advertenciaHardwareActiva = False
                advertenciaDenominacionMostrada = False
                bAdvertenciaFicticiaMostrada = False
                menuPendienteTrasAdvertenciaFicticia = ""
                sMenuActivo = ""
                sLastMenuUrl = ""
                Trace("Nueva sesion detectada")
            End If

            Trace("Limpieza de banderas por estado " & sValue & ": hostError/excepcionNDC")
            If WebBrowser1 IsNot Nothing AndAlso WebBrowser1.Document IsNot Nothing Then
                Try
                    WebBrowser1.Document.InvokeScript("showBtnRetiro")
                Catch ex As Exception
                End Try
            End If
        ElseIf sValue = "701" Then
            bNDCPageActive = False
            Dim errorHostMenuLimpiado As Boolean = False

            If isHostError AndAlso (EsUrlMenuHost(currentScreen) OrElse EsUrlMenuHost(sMenuActivo) OrElse EsUrlMenuHost(sLastMenuUrl)) Then
                isHostError = False
                isExpetionClosePageNDC = False
                pageException = ""
                errorHostMenuLimpiado = True
                Trace("701/menu: limpiando error host para restaurar opciones del menu")
            End If

            If errorHostMenuLimpiado Then
                AplicarEstadoDispositivosEnHtml("701_menu", True)
            End If

            Trace("Estado 701 recibido; se conserva memoria de errores y menu activo")
        End If

        If sValue = "850" OrElse sValue = "852" OrElse sValue = "851" Then
            isExpetionClosePageNDC = False
        End If

        If isExpetionClosePageNDC Then
            If sValue = "701" Then
                Trace("701 recibido durante cierre por excepcion NDC; se reanuda flujo")
                isExpetionClosePageNDC = False
            Else
                Trace("Cierre por excepcion NDC; se oculta appScreens")
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
                Trace("513 interceptado por error de host; se oculta appScreens para mostrar pantalla nativa")
                sValue = "hide"
                isHostError = False
            ElseIf currentScreen.ToLower().Contains("menu") Then
                Dim pageCancel As String = ReadIni("513CANCEL", "PAGE", ConfigManager.ScreensFile).Trim()
                Dim picCancel As String = ReadIni("513CANCEL", "PIC", ConfigManager.ScreensFile).Trim()

                If pageCancel <> "" OrElse picCancel <> "" Then
                    Trace("513 desde menu redirigido a pantalla custom 513CANCEL")
                    sValue = "513CANCEL"
                    keepCancelPageActive = True
                    keepCustomErrorPageActive = True
                Else
                    Trace("513 desde menu sin 513CANCEL configurado; appScreens permanece oculto")
                    sValue = "hide"
                End If
            ElseIf Not Me.Visible OrElse
                   currentScreen = "pantalla_nativa_pic" OrElse
                   currentScreen = "pantalla_nativa_texto" OrElse
                   currentScreen = "pantalla_nativa_texto_esperando_701" OrElse
                   currentScreen = "" Then

                Trace("513 interceptado por cancelacion/flujo nativo; appScreens permanece oculto")
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
                            sCurrent = sValue
                            If DebeUsarTimeoutVisual(sValue) Then
                                ReiniciarTimeoutAppScreens("misma pantalla " & sValue, True)
                            End If
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

            DetenerTimeoutAppScreens(True)

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
                If DebeUsarTimeoutVisual(sValue) Then
                    ReiniciarTimeoutAppScreens("pantalla " & sValue)
                End If

                If pageException = "" Then
                    If Not (sUrl.Contains("wait") Or sUrl.Contains("read")) Then
                        lastUrl = currentScreen
                        currentScreen = sUrl
                    End If
                End If

                Dim sUrlFinal As String = sUrl

                If sValue = "701" AndAlso bFastCashWaitActivo AndAlso sFastCashActivo <> "" AndAlso
                   Not bFaltaBilletesDesdeMenu Then
                    sUrlFinal = sFastCashActivo
                    bFastCashWaitActivo = False
                    Trace("701 desde wait con FastCash activo; regresando a FastCash=" & sUrlFinal)
                ElseIf sValue = "701" AndAlso sMenuActivo <> "" AndAlso sUrl.ToLower().EndsWith("menu.html") Then
                    sUrlFinal = sMenuActivo
                    Trace("701 base INI=" & sUrl & " redirigido a menu activo=" & sUrlFinal)
                End If

                ' Aseguramos que currentScreen refleje la URL REAL navegada,
                ' no el menu.html base.
                If pageException = "" AndAlso
                   Not (sUrlFinal.ToLower().Contains("wait") OrElse sUrlFinal.ToLower().Contains("read")) Then
                    currentScreen = sUrlFinal
                End If

                Trace("Navega page Url: " + sUrlFinal)
                bNDCPageActive = False

                If sValue = "701" AndAlso WebBrowserTienePaginaCargada(sUrlFinal) Then
                    Trace("701: reutilizando menu HTML ya renderizado, sin recargar WebBrowser")
                    ProgramarOcultarWaitRegreso()
                Else
                    WebBrowser1.Navigate(sUrlFinal)
                End If
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
                            If sValue <> "hide" Then Trace("Estado sin PAGE/PIC configurado")
                            sValue = "hide"
                        End If
                    End If
                End If
            End If

            If sValue = "hide" Then
                If timeoutVisualEsperandoBack AndAlso waitRegresoActivo Then
                    Trace("Timeout visual: retorno nativo cubierto con wait")
                ElseIf CubrirInsercionTarjetaDesdeWelcome() Then
                    Trace("Hide desde welcome cubierto con readCard.html")
                Else
                    ocultarPantalla()
                End If
            Else
                If sValue = "back" Then
                    MostrarPantallaBackVisual()
                Else
                    Me.Show()
                    Me.TopMost = True
                    Me.Activate()
                    WebBrowser1.BringToFront()
                End If
            End If
        Catch ex As Exception
            Trace("Error ProcesarPantalla: " + ex.Message)
        End Try
    End Sub

    Private Sub ocultarPantalla()
        Trace("Ocultando appScreens")
        coberturaTarjetaWelcomeActiva = False
        If timerOcultarReadCardWelcome IsNot Nothing Then timerOcultarReadCardWelcome.Stop()
        CancelarOcultamientoTimeoutInicial()
        CancelarMenuAdvertenciaHardwarePendiente("ocultar")
        timeoutSobrePantallaBase = False
        timeoutVisualEsperandoBack = False
        timeoutWaitMostradoPorToque = False
        If timerOcultarWaitTimeout IsNot Nothing Then timerOcultarWaitTimeout.Stop()
        tmMsgDevices.Enabled = False
        Me.Hide()

        If regresoNativo024Activo Then
            DejarWaitComoPrimeraCapaOculta()
        End If

        sCurrent = ""
        sCurrentData = ""
    End Sub

    Private Function CubrirInsercionTarjetaDesdeWelcome() As Boolean
        Try
            If coberturaTarjetaWelcomeActiva Then Return False
            If Not Me.Visible Then Return False
            If Not EsUrlWelcome(currentScreen) Then Return False

            LimpiarProteccionWelcomeHost()

            Dim urlReadCard As String = ReadIni("200", "PAGE", ConfigManager.ScreensFile).Trim()
            If urlReadCard = "" Then Return False

            DetenerTimeoutAppScreens(True)
            RestoreFullRegion()

            If webBrowserWaitRegreso IsNot Nothing Then
                webBrowserWaitRegreso.Visible = False
            End If
            waitRegresoActivo = False

            coberturaTarjetaWelcomeActiva = True
            readCardWelcomeUrl = urlReadCard
            sCurrent = ""
            sCurrentData = ""

            If WebBrowser1 IsNot Nothing Then
                WebBrowser1.Visible = True
                WebBrowser1.BringToFront()
                WebBrowser1.Navigate(urlReadCard)
            End If

            Me.Show()
            Me.TopMost = True
            Me.Activate()
            Trace("ReadCard mostrado por salida de welcome")
            Return True
        Catch ex As Exception
            Trace("Error cubriendo salida de welcome: " & ex.Message, 2)
            coberturaTarjetaWelcomeActiva = False
            Return False
        End Try
    End Function

    Private Sub FinalizarCoberturaTarjetaWelcome(origen As String)
        Try
            If Not coberturaTarjetaWelcomeActiva Then Exit Sub

            coberturaTarjetaWelcomeActiva = False
            If timerOcultarReadCardWelcome IsNot Nothing Then timerOcultarReadCardWelcome.Stop()

            Trace("ReadCard desde welcome finalizado: origen=" & origen)
            ocultarPantalla()
        Catch ex As Exception
            Trace("Error finalizando readCard desde welcome: " & ex.Message, 2)
        End Try
    End Sub

    Private Sub tmOut_Tick(sender As Object, e As EventArgs) Handles tmOut.Tick
        OcultarPorTimeoutAppScreens()
    End Sub

    Private Sub Form1_KeyDown(sender As Object, e As KeyEventArgs) Handles Me.KeyDown
        If e.Control AndAlso e.KeyCode = Keys.S Then
            e.Handled = True
            Try
                Trace("Finalizando aplicacion")
                Dim exeWait As String = "C:\appMain\application\DSC Kill.cmd"
                Process.Start(exeWait)
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub tmMsgDevices_Tick(sender As Object, e As EventArgs) Handles tmMsgDevices.Tick
        tmMsgDevices.Enabled = False

        Try
            If ServicioFueraDeOperacionActivo() Then Exit Sub

            checkEstusImpresora()
            checkEstusCdm()
            checkEstusCassettes()
            killSupervisor()
        Catch ex As Exception
            Trace("Error revisando dispositivos: " & ex.Message)
        Finally
            tmMsgDevices.Enabled = True
        End Try
    End Sub

    Private Function ServicioFueraDeOperacionActivo() As Boolean
        If serviceOutController Is Nothing Then Return False

        Return serviceOutController.RevisarEstado()
    End Function

    Private Function EsComandoRepetible(valor As String) As Boolean
        Dim comando As String = valor.Trim().ToLowerInvariant()

        Return comando = "hide" OrElse comando = "back"
    End Function

    Private Function DebeUsarTimeoutVisual(valor As String) As Boolean
        Dim pantalla As String = valor.Trim().ToLowerInvariant()

        Return pantalla <> "" AndAlso
               pantalla <> "500" AndAlso
               pantalla <> "hide" AndAlso
               pantalla <> "back"
    End Function

    Private Sub ReiniciarTimeoutAppScreens(origen As String, Optional registrar As Boolean = False)
        Try
            If serviceOutController IsNot Nothing AndAlso serviceOutController.IsActive Then Exit Sub

            CancelarOcultamientoTimeoutInicial()
            tmOut.Enabled = False
            writeINI("APP", "TIMEOUT", "", ConfigManager.WorkFile)
            tmOut.Enabled = True
            If registrar Then Trace("Timeout visual rearmado: origen=" & origen)
        Catch ex As Exception
            Trace("Error rearmando timeout visual: " & ex.Message, 2)
        End Try
    End Sub

    Private Sub DetenerTimeoutAppScreens(Optional limpiarBandera As Boolean = False)
        Try
            CancelarOcultamientoTimeoutInicial()
            tmOut.Enabled = False

            If limpiarBandera Then
                writeINI("APP", "TIMEOUT", "", ConfigManager.WorkFile)
            End If
        Catch ex As Exception
            Trace("Error deteniendo timeout visual: " & ex.Message, 2)
        End Try
    End Sub

    Private Sub CancelarOcultamientoTimeoutInicial()
        Try
            If timerOcultarTimeoutInicial IsNot Nothing Then timerOcultarTimeoutInicial.Stop()
            timeoutOcultamientoPendiente = False
        Catch ex As Exception
        End Try
    End Sub

    Private Function ObtenerDelayOcultarTimeoutMs() As Integer
        Try
            Dim valor As String = ReadIni("PARAM", "TIMEOUT_HIDE_DELAY_MS", ConfigManager.ScreensFile).Trim()
            Dim delayMs As Integer = 1000
            Dim delayConfigurado As Integer = 0

            If valor <> "" AndAlso Integer.TryParse(valor, delayConfigurado) Then
                delayMs = delayConfigurado
                If delayMs < 0 Then delayMs = 0
                If delayMs > 5000 Then delayMs = 5000
            End If

            Return delayMs
        Catch ex As Exception
            Return 1000
        End Try
    End Function

    Private Sub AsegurarParametroTimeoutHideDelay()
        Try
            Dim valor As String = ReadIni("PARAM", "TIMEOUT_HIDE_DELAY_MS", ConfigManager.ScreensFile).Trim()
            If valor <> "" AndAlso valor <> "1500" Then Exit Sub

            Dim directorio As String = Path.GetDirectoryName(ConfigManager.ScreensFile)
            If directorio <> "" AndAlso Not Directory.Exists(directorio) Then
                Directory.CreateDirectory(directorio)
            End If

            writeINI("PARAM", "TIMEOUT_HIDE_DELAY_MS", "1000", ConfigManager.ScreensFile)
            Trace("Parametro TIMEOUT_HIDE_DELAY_MS inicializado en appScreens.ini")
        Catch ex As Exception
            Trace("No se pudo inicializar TIMEOUT_HIDE_DELAY_MS: " & ex.Message, 1)
        End Try
    End Sub

    Private Sub OcultarAppScreensPorTimeout()
        Try
            Trace("Timeout visual: appScreens oculto")
            tmMsgDevices.Enabled = False
            Me.Hide()

            If regresoNativo024Activo Then
                DejarWaitComoPrimeraCapaOculta()
            End If
        Catch ex As Exception
            Trace("Error ocultando appScreens por timeout: " & ex.Message, 2)
        End Try
    End Sub

    Private Function TimeoutOcurrioSobrePantallaBase() As Boolean
        Try
            If Not Me.Visible Then Return True

            Dim pantalla As String = currentScreen.Trim().ToLowerInvariant()

            Return pantalla = "" OrElse pantalla.StartsWith("pantalla_nativa_")
        Catch ex As Exception
            Return False
        End Try
    End Function

    Private Sub FinalizarTimeoutSobrePantallaBase(origen As String)
        Try
            DetenerTimeoutAppScreens(True)
            timeoutWaitMostradoPorToque = False
            pantallaAntesTimeout = ""
            backVisualPendiente = False
            backVisualUrlPendiente = ""

            If waitRegresoActivo AndAlso timerOcultarWaitTimeout IsNot Nothing Then
                timerOcultarWaitTimeout.Stop()
                timerOcultarWaitTimeout.Interval = 900
                timerOcultarWaitTimeout.Start()
                Trace("Timeout visual: retorno a pantalla base cubierto con wait. origen=" & origen)
                Return
            End If

            timeoutSobrePantallaBase = False
            timeoutVisualEsperandoBack = False
            Me.Hide()
            Trace("Timeout visual: retorno a pantalla base; appScreens permanece oculto. origen=" & origen)
        Catch ex As Exception
            Trace("Error finalizando timeout sobre pantalla base: " & ex.Message, 2)
            timeoutVisualEsperandoBack = False
            timeoutSobrePantallaBase = False
            Me.Hide()
        End Try
    End Sub

    Private Sub OcultarPorTimeoutAppScreens()
        Try
            DetenerTimeoutAppScreens(False)

            If serviceOutController IsNot Nothing AndAlso serviceOutController.IsActive Then Exit Sub

            timeoutSobrePantallaBase = TimeoutOcurrioSobrePantallaBase()

            If timeoutSobrePantallaBase Then
                pantallaAntesTimeout = ""
                currentScreen = "pantalla_nativa_texto"
                bNDCPageActive = True
            ElseIf currentScreen <> "" Then
                pantallaAntesTimeout = currentScreen
            End If

            sCurrent = ""
            sCurrentData = ""
            writeINI("APP", "TIMEOUT", "ON", ConfigManager.WorkFile)
            timeoutVisualEsperandoBack = True
            timeoutWaitMostradoPorToque = False

            Dim delayOcultamiento As Integer = ObtenerDelayOcultarTimeoutMs()

            If timeoutSobrePantallaBase OrElse delayOcultamiento <= 0 OrElse Not Me.Visible Then
                timeoutOcultamientoPendiente = False
                OcultarAppScreensPorTimeout()
            ElseIf timerOcultarTimeoutInicial IsNot Nothing Then
                timeoutOcultamientoPendiente = True
                timerOcultarTimeoutInicial.Stop()
                timerOcultarTimeoutInicial.Interval = delayOcultamiento
                timerOcultarTimeoutInicial.Start()
                Trace("Timeout visual: ocultamiento diferido " & delayOcultamiento.ToString() & "ms")
            Else
                timeoutOcultamientoPendiente = False
                OcultarAppScreensPorTimeout()
            End If
        Catch ex As Exception
            Trace("Error ocultando por timeout visual: " & ex.Message, 2)
        End Try
    End Sub

    Private Sub MostrarWaitRetornoTimeout(valorPantalla As String, valorData As String)
        Try
            If Not timeoutVisualEsperandoBack Then Exit Sub
            If waitRegresoActivo Then Exit Sub

            Dim origen As String = If(valorPantalla <> "", valorPantalla, valorData)
            Dim tieneHtml As Boolean = False
            If valorPantalla <> "" AndAlso valorPantalla <> "back" AndAlso valorPantalla <> "hide" Then
                Dim seccionIni As String = If(valorPantalla = "welcome", "500", valorPantalla)
                tieneHtml = ReadIni(seccionIni, "PAGE", ConfigManager.ScreensFile).Trim() <> ""
            End If

            If MostrarWaitRegreso() Then
                If timerOcultarWaitTimeout IsNot Nothing AndAlso Not tieneHtml AndAlso valorPantalla <> "back" Then
                    timerOcultarWaitTimeout.Stop()
                    timerOcultarWaitTimeout.Interval = 900
                    timerOcultarWaitTimeout.Start()
                End If

                timeoutWaitMostradoPorToque = True
                Trace("Timeout visual: wait mostrado por retorno. origen=" & origen &
                      " html=" & tieneHtml.ToString())
            Else
                Trace("Timeout visual: wait no disponible en retorno. origen=" & origen, 1)
            End If
        Catch ex As Exception
            Trace("Error mostrando wait por retorno timeout: " & ex.Message, 2)
        End Try
    End Sub

    Private Sub MostrarPantallaBackVisual()
        Try
            CancelarOcultamientoTimeoutInicial()

            If timeoutSobrePantallaBase Then
                FinalizarTimeoutSobrePantallaBase("back")
                Return
            End If

            Dim pantallaBack As String = pantallaAntesTimeout
            If pantallaBack = "" Then pantallaBack = currentScreen

            If pantallaBack = "" Then
                Trace("Back visual sin HTML previo; appScreens permanece oculto", 1)
                timeoutSobrePantallaBase = False
                timeoutVisualEsperandoBack = False
                Me.Hide()
                Return
            End If

            currentScreen = pantallaBack
            timeoutSobrePantallaBase = False
            timeoutVisualEsperandoBack = False
            timeoutWaitMostradoPorToque = False
            If timerOcultarWaitTimeout IsNot Nothing Then timerOcultarWaitTimeout.Stop()

            Dim coberturaVisible As Boolean = waitRegresoActivo
            If Not coberturaVisible Then
                coberturaVisible = MostrarWaitRegreso()
            End If

            If coberturaVisible Then
                Trace("Back visual: cobertura wait visible")
            Else
                Trace("Back visual: wait no disponible; regreso directo", 1)
            End If

            If WebBrowserTienePaginaCargada(pantallaBack) Then
                backVisualPendiente = False
                backVisualUrlPendiente = ""

                If WebBrowser1 IsNot Nothing Then
                    WebBrowser1.Visible = True
                End If

                Me.Show()
                Me.TopMost = True
                Me.Activate()
                ReiniciarTimeoutAppScreens("back", True)
                If coberturaVisible Then
                    ProgramarOcultarWaitRegreso()
                    Trace("Back visual: HTML listo; cobertura wait programada para retiro")
                ElseIf WebBrowser1 IsNot Nothing Then
                    WebBrowser1.BringToFront()
                End If
                Trace("Back visual: reutilizando HTML cargado")
                Return
            End If

            backVisualPendiente = True
            backVisualUrlPendiente = pantallaBack

            If WebBrowser1 IsNot Nothing Then
                WebBrowser1.Visible = UsarCoberturaSinOcultarPrincipal()
                WebBrowser1.Navigate(pantallaBack)
            End If

            Trace("Back visual: cargando HTML pendiente")
        Catch ex As Exception
            Trace("Error mostrando back visual: " & ex.Message, 2)
            Me.Hide()
        End Try
    End Sub

    Private Sub RegistrarLogOperativo()
        Try
            If DateTime.Now.Subtract(ultimoLogOperativo).TotalMinutes < 30 Then Exit Sub

            ultimoLogOperativo = DateTime.Now

            Dim pantallaActual As String = sCurrent
            If pantallaActual = "" Then pantallaActual = "NA"

            Dim htmlActual As String = currentScreen
            If htmlActual <> "" Then htmlActual = IO.Path.GetFileName(htmlActual)
            If htmlActual = "" Then htmlActual = "NA"

            Dim servicioActivo As Boolean = False
            If serviceOutController IsNot Nothing Then servicioActivo = serviceOutController.IsActive

            Trace("appScreens operativo. visible=" & Me.Visible.ToString() &
                  " pantalla=" & pantallaActual &
                  " html=" & htmlActual &
                  " timeoutActivo=" & tmOut.Enabled.ToString() &
                  " servicio=" & servicioActivo.ToString())
        Catch ex As Exception
            Trace("Error registrando estado operativo: " & ex.Message, 1)
        End Try
    End Sub

    Private Sub checkEstusImpresora()
        Dim sEstatusPtr As String = ReadIni("PTR STATUS", "fwDevice", ConfigManager.workFileDevices).Trim().ToUpper()
        Try
            If sEstatusPtr <> sErrorImpresora Then
                sErrorImpresora = sEstatusPtr
                Trace("Cambio Estatus Impresora: " + sEstatusPtr)
            End If
        Catch ex As Exception
            Trace("Error revisando estado de impresora: " + ex.Message)
        End Try
    End Sub

    Private Sub checkEstusCdm()
        Dim sEstatusCdm As String = ReadIni("CDM STATUS", "fwDevice", ConfigManager.workFileDevices)
        Try
            If sEstatusCdm <> sErrorCdm Then
                sErrorCdm = sEstatusCdm
                Trace("Cambio estatus dispensador: " + sEstatusCdm)
            End If
        Catch ex As Exception
            Trace("Error revisando estado de dispensador: " + ex.Message)
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

            AplicarEstadoDispositivosEnHtml("temporizador", imprimirLog)

        Catch ex As Exception
            Trace("Error en checkEstusCassettes: " & ex.Message)
        End Try
    End Sub

    Private Sub killSupervisor()
        Try
            Dim nombreProceso As String = "appConfigurador.exe"
            Dim procesos() As Process = Process.GetProcessesByName(System.IO.Path.GetFileNameWithoutExtension(nombreProceso))
            If procesos.Length > 0 Then
                Trace("Supervisor detectado en welcome; se cierra proceso supervisor")
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
        DesinstalarDetectorToqueRegreso()
        Trace("Cerrando appScreens")
    End Sub

    Private Sub AplicarResponsiveHtml(Optional navegador As WebBrowser = Nothing)
        Try
            Dim navegadorObjetivo As WebBrowser = navegador
            If navegadorObjetivo Is Nothing Then navegadorObjetivo = WebBrowser1
            If navegadorObjetivo Is Nothing OrElse navegadorObjetivo.Document Is Nothing Then Exit Sub

            Dim viewportWidth As Integer = Math.Max(1, navegadorObjetivo.ClientSize.Width)
            Dim viewportHeight As Integer = Math.Max(1, navegadorObjetivo.ClientSize.Height)
            Dim scaleX As String = (viewportWidth / 1024.0).ToString(System.Globalization.CultureInfo.InvariantCulture)
            Dim scaleY As String = (viewportHeight / 768.0).ToString(System.Globalization.CultureInfo.InvariantCulture)
            Dim viewportWidthText As String = viewportWidth.ToString(System.Globalization.CultureInfo.InvariantCulture)
            Dim viewportHeightText As String = viewportHeight.ToString(System.Globalization.CultureInfo.InvariantCulture)
            Dim escalaUniformeValor As Double = Math.Min(viewportWidth / 1024.0, viewportHeight / 768.0)
            Dim escalaUniforme As String = escalaUniformeValor.ToString(System.Globalization.CultureInfo.InvariantCulture)
            Dim contenidoWidth As Double = 1024.0 * escalaUniformeValor
            Dim contenidoHeight As Double = 768.0 * escalaUniformeValor
            Dim offsetX As String = ((viewportWidth - contenidoWidth) / 2.0).ToString(System.Globalization.CultureInfo.InvariantCulture)
            Dim offsetY As String = ((viewportHeight - contenidoHeight) / 2.0).ToString(System.Globalization.CultureInfo.InvariantCulture)
            Dim modoResponsive As String = ReadIni("PARAM", "RESPONSIVE_RENDER_MODE", ConfigManager.ScreensFile).Trim().ToUpper()

            If modoResponsive <> "LEGACY" AndAlso viewportWidth = 1024 AndAlso viewportHeight = 768 Then
                Trace("Responsive aplicado: modo=NATIVO viewport=1024x768 escala=1")
                Exit Sub
            End If

            Dim scriptBase As String =
                "(function(){" &
                "var d=document,b=d.body,e=d.documentElement;if(!b||!e){return;}" &
                "var s=d.getElementById('appScreensResponsiveStyle');" &
                "if(!s){s=d.createElement('style');s.id='appScreensResponsiveStyle';s.type='text/css';" &
                "s.styleSheet?s.styleSheet.cssText='html,body{margin:0!important;padding:0!important;overflow:hidden!important;width:100%!important;height:100%!important;}body{background-size:100% 100%!important;background-repeat:no-repeat!important;background-position:left top!important;}':s.appendChild(d.createTextNode('html,body{margin:0!important;padding:0!important;overflow:hidden!important;width:100%!important;height:100%!important;}body{background-size:100% 100%!important;background-repeat:no-repeat!important;background-position:left top!important;}'));" &
                "(d.getElementsByTagName('head')[0]||e).appendChild(s);}" &
                "e.style.margin='0px';e.style.padding='0px';e.style.width='" & viewportWidthText & "px';e.style.height='" & viewportHeightText & "px';e.style.overflow='hidden';" &
                "b.style.margin='0px';b.style.padding='0px';b.style.width='" & viewportWidthText & "px';b.style.height='" & viewportHeightText & "px';b.style.minHeight='" & viewportHeightText & "px';b.style.overflow='hidden';" &
                "b.style.backgroundSize='100% 100%';b.style.backgroundRepeat='no-repeat';b.style.backgroundPosition='left top';"

            Dim scriptEscala As String
            If modoResponsive <> "FIT" Then
                scriptEscala =
                    "var r=d.getElementById('appScreensResponsiveRoot');" &
                    "if(!r){r=d.createElement('div');r.id='appScreensResponsiveRoot';while(b.firstChild){r.appendChild(b.firstChild);}b.appendChild(r);}" &
                    "r.style.position='absolute';r.style.left='0px';r.style.top='0px';r.style.width='1024px';r.style.height='768px';r.style.overflow='hidden';" &
                    "r.style.zoom='';r.style.transformOrigin='top left';r.style.msTransformOrigin='top left';r.style.transform='scale(" & scaleX & "," & scaleY & ")';r.style.msTransform='scale(" & scaleX & "," & scaleY & ")';"
            Else
                scriptEscala =
                    "var r=d.getElementById('appScreensResponsiveRoot'),v=d.getElementById('appScreensResponsiveViewport');" &
                    "if(!r){r=d.createElement('div');r.id='appScreensResponsiveRoot';while(b.firstChild){r.appendChild(b.firstChild);}}" &
                    "if(!v){v=d.createElement('div');v.id='appScreensResponsiveViewport';b.appendChild(v);v.appendChild(r);}else if(r.parentNode!==v){v.appendChild(r);}" &
                    "v.style.position='absolute';v.style.left='" & offsetX & "px';v.style.top='" & offsetY & "px';v.style.width='" & contenidoWidth.ToString(System.Globalization.CultureInfo.InvariantCulture) & "px';v.style.height='" & contenidoHeight.ToString(System.Globalization.CultureInfo.InvariantCulture) & "px';v.style.overflow='hidden';" &
                    "r.style.position='absolute';r.style.left='0px';r.style.top='0px';r.style.width='1024px';r.style.height='768px';r.style.overflow='hidden';" &
                    "r.style.transform='none';r.style.msTransform='none';r.style.zoom='" & escalaUniforme & "';"
            End If

            Dim script As String = scriptBase & scriptEscala &
                "if(window.scrollTo){window.scrollTo(0,0);}" &
                "})();"

            navegadorObjetivo.Document.InvokeScript("eval", New Object() {script})
            Trace("Responsive aplicado: modo=" & If(modoResponsive = "FIT", "FIT", "AJUSTE_COMPLETO") &
                  " viewport=" & viewportWidth.ToString() & "x" & viewportHeight.ToString() &
                  " escala=" & If(modoResponsive = "FIT", escalaUniforme, scaleX & "x" & scaleY))
        Catch ex As Exception
            Trace("Error inyectando reescalado HTML: " & ex.Message)
        End Try
    End Sub

    Private Sub WebBrowser1_DocumentCompleted(sender As Object, e As WebBrowserDocumentCompletedEventArgs)
        Try
            If e.Url.AbsolutePath = WebBrowser1.Url.AbsolutePath Then
                Trace("DocumentCompleted: " & e.Url.ToString())

                Threading.Thread.Sleep(100)

                If WebBrowser1.Document IsNot Nothing Then
                    AplicarResponsiveHtml()

                    AplicarEstadoDispositivosEnHtml("carga_documento", True)

                    If currentHoleRequired Then
                        ApplyTransparentRegion(currentHoleY, currentHoleHeight)
                    Else
                        RestoreFullRegion()
                    End If

                    If coberturaTarjetaWelcomeActiva AndAlso
                       readCardWelcomeUrl <> "" AndAlso
                       String.Equals(WebBrowser1.Url.LocalPath, readCardWelcomeUrl, StringComparison.OrdinalIgnoreCase) Then

                        If timerOcultarReadCardWelcome IsNot Nothing Then
                            timerOcultarReadCardWelcome.Stop()
                            timerOcultarReadCardWelcome.Start()
                        Else
                            coberturaTarjetaWelcomeActiva = False
                            ocultarPantalla()
                        End If

                        Trace("ReadCard desde welcome cargado; esperando PIN")
                    End If

                    If e.Url.ToString().ToLower().Contains("menumore") Then
                        Try
                            sErrorImpresora = ""
                            If IsPrinterError() Then
                                Trace("MenuMore cargado: mostrando error de impresora")
                                WebBrowser1.Document.InvokeScript("showErrPrinter")
                            Else
                                WebBrowser1.Document.InvokeScript("hideErrPrinter")
                            End If
                        Catch ex As Exception
                            Trace("Error al ejecutar script de impresora en menuMore: " & ex.Message)
                        End Try
                    End If

                    If waitRegresoActivo Then
                        If timeoutVisualEsperandoBack Then
                            timeoutVisualEsperandoBack = False
                            timeoutWaitMostradoPorToque = False
                            If timerOcultarWaitTimeout IsNot Nothing Then timerOcultarWaitTimeout.Stop()
                            Trace("Timeout visual: wait retirado por HTML cargado")
                        End If

                        OcultarWaitRegreso()

                        If navegacionFastCashCubierta Then
                            Trace("Cobertura de espera retirada; FastCash ya esta listo")
                        Else
                            Trace("Wait de regreso retirado despues de cargar el HTML final")
                        End If

                        navegacionFastCashCubierta = False
                    End If

                    If backVisualPendiente AndAlso
                       backVisualUrlPendiente <> "" AndAlso
                       String.Equals(WebBrowser1.Url.LocalPath, backVisualUrlPendiente, StringComparison.OrdinalIgnoreCase) Then

                        backVisualPendiente = False
                        backVisualUrlPendiente = ""
                        WebBrowser1.Visible = True
                        WebBrowser1.BringToFront()
                        Me.Show()
                        Me.TopMost = True
                        Me.Activate()
                        ReiniciarTimeoutAppScreens("back cargado", True)
                        Trace("Back visual: HTML mostrado despues de carga")
                    End If
                End If
            End If
        Catch ex As Exception
            Trace("Error al completar carga de documento HTML: " & ex.Message)
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

            If fisicoStr = "EMPTY" OrElse fisicoStr = "MISSING" OrElse fisicoStr = "INOPERABLE" Then
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
                bFaltaBilletesDesdeMenu = False
                bFastCashWaitActivo = False
                sFastCashActivo = urlFastCashGlobal
                currentScreen = urlFastCashGlobal
                WebBrowser1.Navigate(urlFastCashGlobal)
                Me.Show()
            End If
        Catch ex As Exception
            Trace("Error en regresarFastCash: " & ex.Message)
        End Try
    End Sub

End Class
