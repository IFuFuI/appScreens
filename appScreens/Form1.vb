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
    ' sErrorCdm: Error de impresora
    Dim sErrorCdm As String = String.Empty

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
    ' ndcI: Código de operación cadena NDC I
    Dim ndcJ As String
    ' ndcJ: Código de operación cadena NDC J
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
            'Trace("Config screen parameters")
            Dim sheightStr = ReadIni("PARAM", "HEIGHT", ConfigManager.ConfigAtmClientFile)
            If Not Integer.TryParse(sheightStr, sheight) Then sheight = 768

            Dim sWigthStr = ReadIni("PARAM", "WIDGTH", ConfigManager.ConfigAtmClientFile)
            If Not Integer.TryParse(sWigthStr, swidgth) Then swidgth = 1024


            'Trace("Configure Size: W-" + swidgth + " H-" + sheight)
            Me.Size = New Size(CInt(swidgth), CInt(sheight))

            Dim sPotition As String = "1"
            sPotition = ReadIni("PARAM", "SCREEN_POTITION", ConfigManager.ConfigAtmClientFile)

            If sPotition = 1 Then Me.Location = New Point(0, 0)
            If sPotition = 2 Then Me.Location = New Point(1024, 0)
            If sPotition = 3 Then Me.Location = New Point(-1024, 0)

        Catch ex As Exception
            Trace("Error set parameters")
        End Try

        'Try
        '    CheckForIllegalCrossThreadCalls = False ' DESACTIVA ERROR POR SUBPROCESO
        'Catch ex As Exception
        '    Trace("Error CheckForIllegalCrossThreadCalls")
        'End Try

        Try
            'Init Browser APP web
            WebBrowser1.ScrollBarsEnabled = False
            WebBrowser1.ScriptErrorsSuppressed() = True
            WebBrowser1.Height = CInt(sheight)
            WebBrowser1.Width = CInt(swidgth)
            WebBrowser1.AllowWebBrowserDrop = False
            WebBrowser1.IsWebBrowserContextMenuEnabled = False
            WebBrowser1.WebBrowserShortcutsEnabled = False
            WebBrowser1.ObjectForScripting = Me

            'Trace("Set security IE")
            SetBrowserFeatureControl()

            ' Agregar manejador DocumentCompleted para chequear cassettes cuando el documento esté listo
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
                    ' Resetear también para dígitos y teclas especiales para permitir repetidos
                    If "0123456789*#".Contains(sCurrentData) Then sCurrentData = ""

                    Trace("DATA 2: " + sData)
                    Trace("DATA 2 currentScreen: " + currentScreen)

                    Select Case sData
                        Case "KEYPIN"
                            Trace("sCurrent: " + sCurrent)
                            sDataEnable = ReadIni(sCurrent, "DATA", ConfigManager.ScreensFile)
                            Trace("sDataEnable: " + sDataEnable)
                            If sDataEnable = "PIN" Then
                                'Trace("Invoca metodo")
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

    ''' <summary>
    ''' Manda sonido de beep a la página web
    ''' </summary>
    Public Sub wb_beep()
        Try
            'Trace("beep")
            'Console.Beep(1000, 200)
        Catch ex As Exception
            Trace("Error en wb_beep: " & ex.Message)
        End Try

    End Sub

    ''' <summary>
    ''' Interface para revisar los estatus de los dispositivos a la página web
    ''' </summary>
    Public Sub wb_RevisaEstadusDevices()
        Try
            Trace("wb_RevisaEstadusDevices called")
            sErrorImpresora = ""
            ' Ejecutar comprobación inmediata de cassettes en segundo plano
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

            Return (cass1 = "0" OrElse cass2 = "0" OrElse cass3 = "0" OrElse cass4 = "0")
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

    ''' <summary>
    ''' Evento a la pagina NDC.
    ''' </summary>
    ''' <param name="sPotition">posicion de la FDK</param>
    Public Sub clickPage(sPotition As String)

        ' Si estamos en exp-852 (sin efectivo), el Continuar regresa al menú sin mandar FDK al host
        If sExp852OriginalUrl <> "" Then
            Trace("exp-852 Continuar - navegando a menu: " & sExp852OriginalUrl)
            Dim sMenuUrl As String = sExp852OriginalUrl
            sExp852OriginalUrl = ""
            bNDCPageActive = False
            Me.Show()
            WebBrowser1.Navigate(sMenuUrl)
            Exit Sub
        End If

        ' exp-850 (error impresora): NO interceptamos aquí.
        ' Dejamos que el FDK llegue a APTRA normalmente (para que APTRA actualice su estado)
        ' La navegación al menú la hace pageException (FDKEXP=8 en config de screen 850)

        Try
            'If pageException <> "" Then
            dllInterfaceNdc.showScreenNDC(300)
            'End If
        Catch ex As Exception
            Trace("Error al mandar interface")
        End Try


        sCurrent = ""
        sCurrentData = ""
        Me.Hide()

        Threading.Thread.Sleep(300)

        Select Case sPotition
            Case "1"
                'Trace("FDK 1")
                ClickEnVentana.SimularClickEnVentana(screenEventTo, 177, 296)
            Case "2"
                'Trace("FDK 2")
                ClickEnVentana.SimularClickEnVentana(screenEventTo, 177, 440)
            Case "3"
                'Trace("FDK 3")
                ClickEnVentana.SimularClickEnVentana(screenEventTo, 177, 574)
            Case "4"
                'Trace("FDK 4")
                ClickEnVentana.SimularClickEnVentana(screenEventTo, 177, 713)
            Case "5"
                'Trace("FDK 5")
                ClickEnVentana.SimularClickEnVentana(screenEventTo, 850, 296)
            Case "6"
                ''Trace("FDK 6")
                ClickEnVentana.SimularClickEnVentana(screenEventTo, 850, 440)
            Case "7"
                'Trace("FDK 7")
                ClickEnVentana.SimularClickEnVentana(screenEventTo, 850, 574)
            Case "8"
                'Trace("FDK 8")
                ClickEnVentana.SimularClickEnVentana(screenEventTo, 850, 713)
        End Select

        If pageException <> "" And pageException = sPotition Then
            pageException = ""
            sExp850OriginalUrl = ""
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

        'Me.Show()

    End Sub

    ''' <summary>
    ''' Obtiene el valor de una variable NDC.
    ''' </summary>
    ''' <param name="sVar">Nombre de la variable</param>
    ''' <returns>Valor de la variable</returns>
    Public Function getVar(sVar As String) As String
        Dim sretur As String = String.Empty

        Select Case sVar
            Case "B"
                sretur = ndcB
            Case "C"
                sretur = ndcC
            Case "D"
                sretur = ndcD
            Case "E"
                sretur = ndcE
            Case "F"
                sretur = ndcF
            Case "G"
                sretur = ndcG
            Case "H"
                sretur = ndcH
            Case "I"
                sretur = ndcI
            Case "J"
                sretur = ndcJ
            Case "O"
                sretur = ndcO
            Case "K"
                sretur = ndcK
            Case "L"
                sretur = ndcL
            Case "M"
                sretur = ndcM
            Case "N"
                sretur = ndcN
            Case "comision"
                sretur = sComision
            Case "t1"

                Try
                    Dim texto As String = ndcG
                    Dim partes() As String = texto.Split(New Char() {" "c}, StringSplitOptions.RemoveEmptyEntries)

                    Dim valor1 As String = partes(0)
                    Dim valor2 As String = partes(1)

                    sretur = valor1
                Catch ex As Exception
                    Trace("Error getVar parsear t1: " + ex.Message)
                End Try

            Case "t2"

                Try
                    Dim texto As String = ndcG
                    Dim partes() As String = texto.Split(New Char() {" "c}, StringSplitOptions.RemoveEmptyEntries)

                    Dim valor1 As String = partes(0)
                    Dim valor2 As String = partes(1)

                    sretur = valor2
                Catch ex As Exception
                    Trace("Error getVar parsear t2: " + ex.Message)
                End Try

            Case Else
                sretur = ""
        End Select

        Trace("getVar: " + sretur)
        Return sretur
    End Function

    ''' <summary>
    ''' Regresa a la pantalla anterior.
    ''' </summary>
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

            ' Procesar o registrar
            'Trace("Procesando: " & archivo)

            If String.IsNullOrWhiteSpace(contenido) Then
                'Trace("Archivo vacío: " & archivo)
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

            ' Verificar si hay errores reales (no 000 que es éxito)
            Dim esError As Boolean = False

            If contenido.Contains("CODIGO RESPUESTA") AndAlso Not contenido.Contains("CODIGO RESPUESTA 000") Then
                Trace("Encontro un error regresado por Host, no muestra gracias: ")
                esError = True
            End If

            If contenido.Contains("TIPO DE TRANSACCION") AndAlso Not contenido.Contains("CODIGO RESPUESTA 000") Then
                Trace("Encontro un error regresado por Host TIPO DE TRANSACCION, no muestra gracias: ")
                esError = True
            End If

            If contenido.Contains("CODIGO DE ERRORR") Then
                Trace("Encontro un error CODIGO DE ERRORR regresado por Host, no muestra gracias: ")
                esError = True
            End If

            If esError Then
                isExpetionClosePageNDC = True
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


            ' Eliminar el archivo después de procesarlo
            Try
                If File.Exists(archivo) Then
                    File.Delete(archivo)
                End If
            Catch ex As Exception
                Trace("Error al borrar archivo")
            End Try
        Next


        If sUrl <> "" Then
            Me.Show()
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

            ' Si va a mostrar el menú (cualquier variante) y no hay efectivo, redirigir a pantalla sin efectivo
            Dim sUrlLower As String = sUrl.ToLower()
            Dim esPaginaMenu As Boolean = sUrlLower.Contains("menu") AndAlso Not sUrlLower.Contains("menumore")
            If esPaginaMenu Then
                sLastMenuUrl = sUrl
                If AreAnyCassettesEmpty() Then
                    Trace("Sin efectivo - redirigiendo a exp-852")
                    sExp852OriginalUrl = sUrl
                    sExp850OriginalUrl = ""
                    Dim sUrlBase As String = sUrl.Substring(0, sUrl.LastIndexOf("\") + 1)
                    sUrl = sUrlBase & "exp-852.html"
                ElseIf IsPrinterError() Then
                    Trace("Error impresora - redirigiendo a exp-850")
                    sExp850OriginalUrl = sUrl
                    ' currentScreen = menú correcto, para que pageException (FDK8) navegue ahí
                    currentScreen = sUrl
                    ' Setear pageException directamente (por si el timer 850 llegó antes y ya fue ignorado)
                    pageException = "8"
                    ' Limpiar flag de excepción NDC para que 701 no oculte el menú después
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
        ' Lógica para procesar la pantalla
        Dim sUrl As String = String.Empty
        Dim sImagen As String = String.Empty
        Dim sDataEnable As String = String.Empty
        Dim sIdioma As String

        ' IMPORTANTE: este check va ANTES de pageException = "" para no borrar el valor seteado por DatosNDC
        If sValue = "850" AndAlso sExp850OriginalUrl <> "" Then
            Trace("850 ignorado - exp-850 ya activo proactivamente")
            isExpetionClosePageNDC = False
            Exit Sub
        End If

        pageException = ""

        If sValue = "welcome" Then
            sValue = "500"
            bNDCPageActive = False

            If Me.Visible Then
                'Exit Sub
            Else
                Me.Visible = True
            End If
        End If

        If sValue = "513" OrElse sValue = "500" Then
            bNDCPageActive = False
            sExp850OriginalUrl = ""
            sExp852OriginalUrl = ""
            ' Resetear cache de estados para que welcome vuelva a invocar showErr/hideErr correctamente
            sErrorImpresora = ""
            sErrorCdm = ""
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

                    If sUrl.Contains("wait") Or sUrl.Contains("read") Then
                        'Trace("No se guarda wait")
                    Else
                        'Trace("Set CurrentScreen y lastScreen")
                        lastUrl = currentScreen
                        currentScreen = sUrl
                    End If

                End If

                ' Si el 701 resuelve a menu.html pero tenemos un menú específico guardado, usarlo
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
                'Levanta ejectuable de IDLEE
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

                ' Solo invocar showErrPrinter/hideErrPrinter en welcome y menuMore
                ' En otras páginas (menú principal, transacciones) el invoke rompe el flujo
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
                If sErrorCdm = "HWERROR" Then
                    WebBrowser1.Document.InvokeScript("showErrCdm")
                Else
                    If sErrorCdm = "ONLINE" Then
                    End If
                    WebBrowser1.Document.InvokeScript("hideErrCdm")
                End If
                Trace("Invoke Welcome cmd")
            End If
        Catch ex As Exception
            Trace("Error al hacer el invoke Welcome: " + ex.Message)
        End Try
    End Sub

    Private Sub checkEstusCassettes()
        Try
            ' Leemos los valores. Usamos .Trim() para limpiar espacios invisibles
            Dim cass1 As String = ReadIni("CDM_COUNTER", "CASSETTE1", ConfigManager.workFileDevices).Trim()
            Dim cass2 As String = ReadIni("CDM_COUNTER", "CASSETTE2", ConfigManager.workFileDevices).Trim()
            Dim cass3 As String = ReadIni("CDM_COUNTER", "CASSETTE3", ConfigManager.workFileDevices).Trim()
            Dim cass4 As String = ReadIni("CDM_COUNTER", "CASSETTE4", ConfigManager.workFileDevices).Trim()

            Trace("Cassettes: C1=" & cass1 & " C2=" & cass2 & " C3=" & cass3 & " C4=" & cass4)

            ' Validar que el WebBrowser tiene un Document cargado
            If WebBrowser1 Is Nothing OrElse WebBrowser1.Document Is Nothing Then
                Trace("WebBrowser1.Document is Nothing, cannot invoke script")
                Return
            End If

            ' Si cualquiera está en "0"
            If cass1 = "0" OrElse cass2 = "0" OrElse cass3 = "0" OrElse cass4 = "0" Then
                Trace("Invocando hideBtnRetiro (cassettes en 0)")
                WebBrowser1.Document.InvokeScript("hideBtnRetiro")
            Else
                Trace("Invocando showBtnRetiro (cassettes con dinero)")
                WebBrowser1.Document.InvokeScript("showBtnRetiro")
            End If
        Catch ex As Exception
            Trace("Error en checkEstusCassettes: " & ex.Message)
        End Try
    End Sub

    Private Sub killSupervisor()
        Try
            Dim nombreProceso As String = "appConfigurador.exe" ' Reemplaza con el nombre real del proceso
            ' Buscar procesos con ese nombre
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
            ' Solo procesar cuando el documento principal esté cargado (no iframes)
            If e.Url.AbsolutePath = WebBrowser1.Url.AbsolutePath Then
                Trace("DocumentCompleted: " & e.Url.ToString())

                ' Dar un pequeño delay para asegurar que todo el DOM está listo
                Threading.Thread.Sleep(100)

                ' Invocar comprobación de cassettes directamente
                If WebBrowser1.Document IsNot Nothing Then
                    Try
                        Dim anyZero As Boolean = AreAnyCassettesEmpty()
                        Trace("DocumentCompleted - AreAnyCassettesEmpty: " & anyZero)

                        If anyZero Then
                            Trace("DocumentCompleted - Calling hideBtnRetiro")
                            WebBrowser1.Document.InvokeScript("hideBtnRetiro")
                        Else
                            Trace("DocumentCompleted - Calling showBtnRetiro")
                            WebBrowser1.Document.InvokeScript("showBtnRetiro")
                        End If
                    Catch ex As Exception
                        Trace("Error invoking cassette script in DocumentCompleted: " & ex.Message)
                    End Try

                    ' Aplicar región transparente para pantalla 554
                    If e.Url.ToString().ToLower().Contains("transferenciaentrecuentas-554") OrElse e.Url.ToString().ToLower().Contains("-554.") Then
                        ApplyTransparentRegion()
                    Else
                        ' Restaurar región completa en otras pantallas
                        RestoreFullRegion()
                    End If

                    ' Si es menuMore y hay error de impresora, invocar showErrPrinter al cargar
                    If e.Url.ToString().ToLower().Contains("menumore") Then
                        Try
                            sErrorImpresora = "" ' resetear cache para que checkEstusImpresora vuelva a disparar
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

    ''' <summary>
    ''' Aplica una región transparente a la ventana para crear un "hueco" donde se vea el texto de APTRA debajo
    ''' </summary>
    Private Sub ApplyTransparentRegion()
        Try
            If transparentRegionApplied Then
                Return ' Ya aplicada, no aplicar de nuevo
            End If

            ' Coordenadas del Label Interactive5 de APTRA: X=320, Y=432, Size=125
            ' Crear franja horizontal completa para ver todo el texto del monto
            Dim holeX As Integer = 0  ' Empezar desde el borde izquierdo
            Dim holeY As Integer = 430  ' Altura donde está el texto (más abajo)
            Dim holeWidth As Integer = Me.Width  ' Todo el ancho de la pantalla
            Dim holeHeight As Integer = 60  ' Alto reducido para solo ver la línea del monto

            ' Crear región completa del form
            Dim fullRegion As IntPtr = CreateRectRgn(0, 0, Me.Width, Me.Height)

            ' Crear región del hueco transparente
            Dim holeRegion As IntPtr = CreateRectRgn(holeX, holeY, holeX + holeWidth, holeY + holeHeight)

            ' Combinar: fullRegion - holeRegion = región con hueco
            Dim combinedRegion As IntPtr = CreateRectRgn(0, 0, Me.Width, Me.Height)
            CombineRgn(combinedRegion, fullRegion, holeRegion, RGN_DIFF)

            ' Aplicar la región a la ventana
            SetWindowRgn(Me.Handle, combinedRegion, True)

            ' Limpiar recursos (no borrar combinedRegion porque Windows la usa)
            DeleteObject(fullRegion)
            DeleteObject(holeRegion)

            transparentRegionApplied = True
            Trace("[Transparent Region] Aplicada para pantalla 554 - Hueco en X=" & holeX & " Y=" & holeY & " W=" & holeWidth & " H=" & holeHeight)

        Catch ex As Exception
            Trace("[Transparent Region] Error aplicando región: " & ex.Message)
        End Try
    End Sub

    ''' <summary>
    ''' Restaura la región completa de la ventana (sin huecos transparentes)
    ''' </summary>
    Private Sub RestoreFullRegion()
        Try
            If Not transparentRegionApplied Then
                Return ' No hay región aplicada, no hacer nada
            End If

            ' Crear región completa del form (sin huecos)
            Dim fullRegion As IntPtr = CreateRectRgn(0, 0, Me.Width, Me.Height)

            ' Aplicar la región completa a la ventana
            SetWindowRgn(Me.Handle, fullRegion, True)

            transparentRegionApplied = False
            Trace("[Transparent Region] Restaurada región completa")

        Catch ex As Exception
            Trace("[Transparent Region] Error restaurando región: " & ex.Message)
        End Try
    End Sub


End Class


