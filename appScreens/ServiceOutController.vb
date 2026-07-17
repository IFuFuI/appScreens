Imports System.IO
Imports System.Windows.Forms

Public Class ServiceOutController
    Private Const IniSection As String = "SERVICE_OUT"
    Private Const IniKey As String = "ACC"
    Private Const ShowCommand As String = "SHOW"
    Private Const HideCommand As String = "HIDE"
    Private Const ServicePage As String = "C:\appMain\html\pages\out_of_service.html"

    Private ReadOnly ownerForm As Form
    Private ReadOnly baseBrowser As WebBrowser
    Private overlayBrowser As WebBrowser = Nothing
    Private active As Boolean = False
    Private lastMissingPageLog As DateTime = DateTime.MinValue

    Public Sub New(formulario As Form, navegadorBase As WebBrowser)
        ownerForm = formulario
        baseBrowser = navegadorBase
        AsegurarParametroIni()
    End Sub

    Public ReadOnly Property IsActive As Boolean
        Get
            Return active
        End Get
    End Property

    Public Function RevisarEstado() As Boolean
        Try
            Dim command As String = ReadIni(IniSection, IniKey, ConfigManager.WorkFile).Trim().ToUpperInvariant()

            If command = ShowCommand Then
                LimpiarComando()
                Mostrar()
            ElseIf command = HideCommand Then
                LimpiarComando()
                Ocultar()
            End If

            If active Then MantenerAlFrente()
        Catch ex As Exception
            Trace("Error revisando modo fuera de servicio: " & ex.Message, 2)
        End Try

        Return active
    End Function

    Private Sub Mostrar()
        Try
            CrearOverlaySiHaceFalta()

            If Not File.Exists(ServicePage) Then
                RegistrarHtmlFaltante()
            End If

            If overlayBrowser.Url Is Nothing OrElse
               Not String.Equals(overlayBrowser.Url.LocalPath, ServicePage, StringComparison.OrdinalIgnoreCase) Then
                overlayBrowser.Navigate(ServicePage)
            End If

            active = True
            overlayBrowser.Visible = True
            MantenerAlFrente()
            Trace("Modo fuera de servicio activado")
        Catch ex As Exception
            Trace("Error activando modo fuera de servicio: " & ex.Message, 2)
        End Try
    End Sub

    Private Sub Ocultar()
        Try
            active = False

            If overlayBrowser IsNot Nothing Then
                overlayBrowser.Visible = False
            End If

            If baseBrowser IsNot Nothing Then
                baseBrowser.Visible = True
                baseBrowser.BringToFront()
            End If

            Trace("Modo fuera de servicio desactivado")
        Catch ex As Exception
            Trace("Error desactivando modo fuera de servicio: " & ex.Message, 2)
        End Try
    End Sub

    Private Sub CrearOverlaySiHaceFalta()
        If overlayBrowser IsNot Nothing Then Exit Sub

        overlayBrowser = New WebBrowser()
        overlayBrowser.Dock = DockStyle.Fill
        overlayBrowser.ScrollBarsEnabled = False
        overlayBrowser.ScriptErrorsSuppressed = True
        overlayBrowser.AllowWebBrowserDrop = False
        overlayBrowser.IsWebBrowserContextMenuEnabled = False
        overlayBrowser.WebBrowserShortcutsEnabled = False

        ownerForm.Controls.Add(overlayBrowser)
    End Sub

    Private Sub MantenerAlFrente()
        If ownerForm Is Nothing OrElse overlayBrowser Is Nothing Then Exit Sub

        overlayBrowser.Visible = True
        overlayBrowser.BringToFront()
        ownerForm.TopMost = True
        ownerForm.Show()
        ownerForm.Activate()
    End Sub

    Private Sub LimpiarComando()
        writeINI(IniSection, IniKey, "", ConfigManager.WorkFile)
    End Sub

    Private Sub AsegurarParametroIni()
        Try
            Dim currentValue As String = ReadIni(IniSection, IniKey, ConfigManager.WorkFile)

            If currentValue = "" Then
                writeINI(IniSection, IniKey, "", ConfigManager.WorkFile)
            End If
        Catch ex As Exception
            Trace("No se pudo inicializar parametro fuera de servicio: " & ex.Message, 1)
        End Try
    End Sub

    Private Sub RegistrarHtmlFaltante()
        If DateTime.Now.Subtract(lastMissingPageLog).TotalMinutes < 1 Then Exit Sub

        lastMissingPageLog = DateTime.Now
        Trace("No se encontro out_of_service.html", 1)
    End Sub
End Class
