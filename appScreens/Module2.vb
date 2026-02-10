Imports System.Runtime.InteropServices
Imports System.Text

Module VentanasActivas

    ' Delegado para enumerar ventanas
    Private Delegate Function EnumWindowsProc(hWnd As IntPtr, lParam As IntPtr) As Boolean

    ' API para enumerar ventanas
    <DllImport("user32.dll")>
    Private Function EnumWindows(lpEnumFunc As EnumWindowsProc, lParam As IntPtr) As Boolean
    End Function

    ' API para obtener el texto (título) de la ventana
    <DllImport("user32.dll", SetLastError:=True, CharSet:=CharSet.Auto)>
    Private Function GetWindowText(hWnd As IntPtr, lpString As StringBuilder, nMaxCount As Integer) As Integer
    End Function

    ' API para verificar si la ventana es visible
    <DllImport("user32.dll")>
    Private Function IsWindowVisible(hWnd As IntPtr) As Boolean
    End Function

    Public Sub getVentans()
        Trace("Ventanas activas visibles:")
        EnumWindows(AddressOf MostrarTituloVentana, IntPtr.Zero)
        Trace("Fin de listado.")
        Console.ReadLine()
    End Sub

    Private Function MostrarTituloVentana(hWnd As IntPtr, lParam As IntPtr) As Boolean
        If IsWindowVisible(hWnd) Then
            Dim titulo As New StringBuilder(256)
            GetWindowText(hWnd, titulo, titulo.Capacity)
            Dim textoVentana As String = titulo.ToString().Trim()

            If Not String.IsNullOrEmpty(textoVentana) Then
                Trace("- " & textoVentana)
            End If
        End If
        Return True ' Continuar enumerando
    End Function

End Module
