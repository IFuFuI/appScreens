Imports System.IO
Imports System.Configuration

''' <summary>
''' Administra la configuración de la aplicación de manera dinámica
''' </summary>
Public Class ConfigManager

#Region "Propiedades de Configuración"

    Public Shared ReadOnly Property WorkFile As String
        Get
            Return GetConfigValue("WorkFile", "C:\appMain\work\work.ini")
        End Get
    End Property

    Public Shared ReadOnly Property workFileDevices As String
        Get
            Return GetConfigValue("WorkFileDevices", "C:\appMain\work\Device_Status.ini")
        End Get
    End Property

    Public Shared ReadOnly Property ScreensFile As String
        Get
            Return GetConfigValue("ScreensFile", "C:\appMain\config\appScreens.ini")
        End Get
    End Property

    Public Shared ReadOnly Property ConfigAtmClientFile As String
        Get
            Return GetConfigValue("ConfigAtmClientFile", "C:\appMain\config\appConfigAtm.ini")
        End Get
    End Property

    Public Shared ReadOnly Property ConfigFile As String
        Get
            Return GetConfigValue("ConfigFile", "C:\appMain\config\appConfig.ini")
        End Get
    End Property

    Public Shared ReadOnly Property LogPath As String
        Get
            Return GetConfigValue("LogPath", "C:\appMain\log\")
        End Get
    End Property
    Public Shared ReadOnly Property rutaMsg As String
        Get
            Return GetConfigValue("rutaMsg", "C:\appMain\msg")
        End Get
    End Property

    Public Shared ReadOnly Property strRutaInterface As String
        Get
            Return GetConfigValue("strRutaInterface", "C:\appMain\work\mvInterface.ini")
        End Get
    End Property

#End Region

#Region "Métodos Privados"

    ''' <summary>
    ''' Obtiene un valor de configuración del app.config o usa un valor por defecto
    ''' </summary>
    Private Shared Function GetConfigValue(key As String, defaultValue As String) As String
        Try
            Dim configValue As String = ConfigurationManager.AppSettings(key)
            If String.IsNullOrEmpty(configValue) Then
                Return defaultValue
            End If

            ' Expandir variables de entorno si existen
            Return Environment.ExpandEnvironmentVariables(configValue)
        Catch ex As Exception
            ' En caso de error, usar valor por defecto
            Return defaultValue
        End Try
    End Function

    ''' <summary>
    ''' Valida que las rutas de archivos existan y sean accesibles
    ''' </summary>
    Public Shared Function ValidateConfiguration() As Boolean
        Try
            ' Validar que los directorios existan o puedan crearse
            ValidateDirectory(Path.GetDirectoryName(WorkFile))
            ValidateDirectory(Path.GetDirectoryName(ScreensFile))
            ValidateDirectory(Path.GetDirectoryName(ConfigAtmClientFile))
            ValidateDirectory(Path.GetDirectoryName(ConfigFile))
            ValidateDirectory(LogPath)

            Return True
        Catch ex As Exception
            Trace("[ConfigManager] [Error] Configuration validation failed: " + ex.Message)
            Return False
        End Try
    End Function

    ''' <summary>
    ''' Valida o crea un directorio si no existe
    ''' </summary>
    Private Shared Sub ValidateDirectory(directoryPath As String)
        If Not String.IsNullOrEmpty(directoryPath) AndAlso Not Directory.Exists(directoryPath) Then
            Directory.CreateDirectory(directoryPath)
        End If
    End Sub

#End Region

End Class
