Imports System.Text

Imports System.Text.RegularExpressions

Public Class MensajeNDC
    ' Propiedades principales
    Public Property TipoMensaje As String
    Public Property Secuencia As String
    Public Property Reservado As String
    Public Property CodigoTransaccion As String
    Public Property Estado As String
    Public Property Referencia As String
    Public Property Pantalla As String
    Public Property Layout As String
    ' Solo para logging: conserva todos los comandos H/P recibidos sin cambiar
    ' las propiedades Pantalla/Layout que usa la logica actual.
    Public Property TodasLasPantallas As List(Of String)
    Public Property TodosLosLayouts As List(Of String)
    Public Property DatoB As String
    Public Property DatoC As String
    Public Property DatoE As String
    Public Property DatoD As String
    Public Property DatoF As String
    Public Property DatoG As String
    Public Property DatoH As String
    Public Property DatoI As String
    Public Property DatoK As String
    Public Property DatoJ As String
    Public Property DatoL As String
    Public Property DatoN As String
    Public Property DatoM As String
    Public Property DatoO As String
    Public Property CodigoFinal As String
    Public Property Tarjeta As String
    Public Property Importe As String
    Public Property Comision As String
    Public Property NuevoSaldo As String
    Public Property AID As String
    Public Property ARQC As String
    Public Property ARPC As String
    Public Property NumAutorizacion As String
    Public Property Producto As String
    Public Property CodigoServicio As String
    Public Property TerminalCap As String
    Public Property PEM As String
    Public Property Donativo As String
    Public Property ReciboCompleto As String
    Public Property CodigoPantalla As String
    Public Property IdTransaccion As String
    Public Property CodigoLayout As String
    Public Property CodigoPresentacion As String
    Public Property ComandosESC As List(Of String)
    Public Property MontoFO As String
    Public Property codigoRespuesta As String
    Public Property codigoTipoTxn As String
    Public Property codigoError As String



    ' Método para parsear el mensaje NDC
    Public Shared Function Parse(mensaje As String) As MensajeNDC
        Dim ndc As New MensajeNDC()
        ndc.TodasLasPantallas = New List(Of String)
        ndc.TodosLosLayouts = New List(Of String)
        Dim campos() As String = mensaje.Split(Chr(28))

        If campos.Length > 0 Then ndc.TipoMensaje = campos(0)
        If campos.Length > 1 Then ndc.Secuencia = campos(1)
        If campos.Length > 2 Then ndc.Reservado = campos(2)
        If campos.Length > 3 Then ndc.CodigoTransaccion = campos(3)
        If campos.Length > 4 Then ndc.Estado = campos(4)

        If campos.Length > 2 Then ndc.CodigoPantalla = campos(2)
        If campos.Length > 3 Then ndc.IdTransaccion = campos(3)
        If campos.Length > 4 Then ndc.CodigoLayout = campos(4)
        If campos.Length > 5 Then ndc.CodigoPresentacion = campos(5)


        ' Campo 5: Referencia + presentación
        If campos.Length > 5 Then
            Dim partes() As String = campos(5).Split(Chr(12)) ' FF
            ndc.Referencia = partes(0).Trim()

            If partes.Length > 1 Then
                Dim bloquePresentacion As String = partes(1)
                Dim subBloques() As String = bloquePresentacion.Split(Chr(15)) ' SI

                For Each bloque In subBloques
                    Dim comandos() As String = bloque.Split(Chr(27)) ' ESC
                    For Each cmd In comandos
                        If cmd.StartsWith("H") Then
                            ndc.Pantalla = cmd
                            ndc.TodasLasPantallas.Add(cmd)
                        End If
                        If cmd.StartsWith("P") Then
                            ndc.Layout = cmd
                            ndc.TodosLosLayouts.Add(cmd)
                        End If
                        If cmd.StartsWith("C") AndAlso cmd.Length > 2 Then ndc.DatoC = cmd.Substring(2).Trim()
                        If cmd.StartsWith("B") AndAlso cmd.Length > 2 Then ndc.DatoB = cmd.Substring(2).Trim()
                        If cmd.StartsWith("D") AndAlso cmd.Length > 2 Then ndc.DatoD = cmd.Substring(2).Trim()
                        If cmd.StartsWith("E") AndAlso cmd.Length > 2 Then ndc.DatoE = cmd.Substring(2).Trim()
                        If cmd.StartsWith("F") AndAlso cmd.Length > 2 Then ndc.DatoF = cmd.Substring(2).Trim()
                        If cmd.StartsWith("G") AndAlso cmd.Length > 2 Then ndc.DatoG = cmd.Substring(2).Trim()
                        If cmd.StartsWith("O") AndAlso cmd.Length > 2 Then ndc.DatoO = cmd.Substring(2).Trim()
                        If cmd.StartsWith("I") AndAlso cmd.Length > 2 Then ndc.DatoI = cmd.Substring(2).Trim()
                        If cmd.StartsWith("J") AndAlso cmd.Length > 2 Then ndc.DatoJ = cmd.Substring(2).Trim()
                        If cmd.StartsWith("H") AndAlso cmd.Length > 2 Then ndc.DatoH = cmd.Substring(2).Trim()
                        If cmd.StartsWith("K") AndAlso cmd.Length > 2 Then ndc.DatoK = cmd.Substring(2).Trim()
                        If cmd.StartsWith("L") AndAlso cmd.Length > 2 Then ndc.DatoL = cmd.Substring(2).Trim()
                        If cmd.StartsWith("M") AndAlso cmd.Length > 2 Then ndc.DatoM = cmd.Substring(2).Trim()
                        If cmd.StartsWith("N") AndAlso cmd.Length > 2 Then ndc.DatoN = cmd.Substring(2).Trim()
                    Next
                Next
            End If
        End If

        If campos.Length > 6 Then ndc.CodigoFinal = campos(6)

        ' Campo 7: Recibo extendido
        If campos.Length > 7 Then
            ndc.ReciboCompleto = campos(7)
            Dim lineas() As String = ndc.ReciboCompleto.Split({vbCrLf, vbLf}, StringSplitOptions.RemoveEmptyEntries)

            For Each linea In lineas
                If linea.Contains("TARJ:") Then ndc.Tarjeta = linea.Replace("TARJ:", "").Trim()
                If linea.Contains("IMPORTE:") Then ndc.Importe = linea.Replace("IMPORTE:", "").Trim()
                If linea.Contains("COMISION/FEE:") Then ndc.Comision = linea.Replace("COMISION/FEE:", "").Trim()
                If linea.Contains("SU NUEVO SALDO:") Then ndc.NuevoSaldo = linea.Replace("SU NUEVO SALDO:", "").Trim()
                If linea.Contains("AID:") Then ndc.AID = linea.Replace("AID:", "").Trim()
                If linea.Contains("ARQC:") Then ndc.ARQC = linea.Replace("ARQC:", "").Trim()
                If linea.Contains("ARPC:") Then ndc.ARPC = linea.Replace("ARPC:", "").Trim()
                If linea.Contains("NUMAUTH:") Then ndc.NumAutorizacion = linea.Replace("NUMAUTH:", "").Trim()
                If linea.Contains("PROD:") Then ndc.Producto = linea.Replace("PROD:", "").Trim()
                If linea.Contains("CODSER:") Then ndc.CodigoServicio = linea.Replace("CODSER:", "").Trim()
                If linea.Contains("TERMCAP:") Then ndc.TerminalCap = linea.Replace("TERMCAP:", "").Trim()
                If linea.Contains("PEM:") Then ndc.PEM = linea.Replace("PEM:", "").Trim()
                If linea.Contains("DONATIVO:") Then ndc.Donativo = linea.Replace("DONATIVO:", "").Trim()
            Next
        End If

        ' Procesar bloque de presentación
        If mensaje.Contains(Chr(12)) Then
            Dim bloque = mensaje.Split(Chr(12)).Last()
            Dim comandos = bloque.Split(Chr(15)) ' subcomandos

            ndc.ComandosESC = New List(Of String)
            For Each cmd In comandos
                If cmd.Contains("") Then
                    ndc.ComandosESC.Add(cmd.Trim())
                End If
                If cmd.Contains("FO") Then
                    ndc.MontoFO = cmd.Replace("FO", "").Trim()
                End If
            Next
        End If
        ' Los codigos deben extraerse desde su etiqueta completa. Nunca se debe
        ' buscar un numero suelto en todo el NDC porque puede aparecer en
        ' NUMAUTH, saldos, folios u otros campos validos.
        Dim respuestaMatch As Match = Regex.Match(
            mensaje,
            "\bCODIGO\s+(?:DE\s+)?RESPUESTA\s*:?\s*(\d{3})\b",
            RegexOptions.IgnoreCase)
        If respuestaMatch.Success Then
            ndc.codigoRespuesta = respuestaMatch.Groups(1).Value
        End If

        ' Se acepta ERROR y ERRORR porque ambos formatos existen en mensajes
        ' historicos del host.
        Dim errorMatch As Match = Regex.Match(
            mensaje,
            "\bCODIGO\s+DE\s+ERRORR?\s*:?\s*(\d{3})\b",
            RegexOptions.IgnoreCase)
        If errorMatch.Success Then
            ndc.codigoError = errorMatch.Groups(1).Value
        End If

        Return ndc
    End Function
End Class
