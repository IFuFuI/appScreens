Imports System.Runtime.InteropServices

Public Class ClickEnVentana

    ' Buscar ventana por título
    <DllImport("user32.dll", SetLastError:=True)>
    Private Shared Function FindWindow(lpClassName As String, lpWindowName As String) As IntPtr
    End Function

    ' Obtener rectángulo de ventana
    <DllImport("user32.dll")>
    Private Shared Function GetWindowRect(hWnd As IntPtr, ByRef lpRect As RECT) As Boolean
    End Function

    ' Mover cursor
    <DllImport("user32.dll")>
    Private Shared Function SetCursorPos(x As Integer, y As Integer) As Boolean
    End Function

    ' Simular clic
    <DllImport("user32.dll")>
    Private Shared Sub mouse_event(dwFlags As Integer, dx As Integer, dy As Integer, dwData As Integer, dwExtraInfo As IntPtr)
    End Sub

    ' Constantes
    Private Const MOUSEEVENTF_LEFTDOWN As Integer = &H2
    Private Const MOUSEEVENTF_LEFTUP As Integer = &H4

    ' Estructura RECT
    Private Structure RECT
        Public Left As Integer
        Public Top As Integer
        Public Right As Integer
        Public Bottom As Integer
    End Structure

    ''' <summary>
    ''' Simula clic en coordenadas relativas dentro de una ventana
    ''' </summary>
    Public Shared Sub SimularClickEnVentana(tituloVentana As String, offsetX As Integer, offsetY As Integer)
        Dim hWnd As IntPtr = FindWindow(Nothing, tituloVentana)

        If hWnd = IntPtr.Zero Then
            'Trace("Ventana no encontrada: " & tituloVentana)
            Return
        End If

        Dim rect As RECT
        If GetWindowRect(hWnd, rect) Then
            Dim xAbs As Integer = rect.Left + offsetX
            Dim yAbs As Integer = rect.Top + offsetY

            ' Mover cursor y simular clic
            SetCursorPos(xAbs, yAbs)
            mouse_event(MOUSEEVENTF_LEFTDOWN, xAbs, yAbs, 0, IntPtr.Zero)
            mouse_event(MOUSEEVENTF_LEFTUP, xAbs, yAbs, 0, IntPtr.Zero)

            ' Mostrar posición
            'Trace($"FDK precionado en posición absoluta ({xAbs}, {yAbs})")
        Else
            'Trace("No se pudo obtener el rectángulo de la ventana.")
        End If




    End Sub

    Public Shared Sub MoverMouse00()
        SetCursorPos(0, 0) ' Mueve el mouse a la esquina superior izquierda de la pantalla
    End Sub



End Class