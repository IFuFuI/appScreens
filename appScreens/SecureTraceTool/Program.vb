Imports System.IO
Imports System.IO.Compression
Imports System.Security.Cryptography
Imports System.Text
Imports System.Windows.Forms

Module Program
    <STAThread>
    Public Sub Main()
        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault(False)
        Application.Run(New MainForm())
    End Sub
End Module

Public Class MainForm
    Inherits Form

    Private ReadOnly publicKeyText As New TextBox()
    Private ReadOnly privateKeyText As New TextBox()
    Private ReadOnly encryptedFileText As New TextBox()
    Private ReadOnly decryptPrivateKeyText As New TextBox()
    Private ReadOnly outputFileText As New TextBox()
    Private ReadOnly logText As New TextBox()

    Public Sub New()
        Me.Text = "Secure Trace Tool - v2 GZip"
        Me.StartPosition = FormStartPosition.CenterScreen
        Me.MinimumSize = New Drawing.Size(760, 520)
        Me.Size = New Drawing.Size(860, 600)

        Dim root As New TableLayoutPanel()
        root.Dock = DockStyle.Fill
        root.RowCount = 2
        root.ColumnCount = 1
        root.RowStyles.Add(New RowStyle(SizeType.Percent, 100))
        root.RowStyles.Add(New RowStyle(SizeType.Absolute, 130))
        root.Padding = New Padding(12)

        Dim tabs As New TabControl()
        tabs.Dock = DockStyle.Fill
        tabs.TabPages.Add(BuildKeyTab())
        tabs.TabPages.Add(BuildDecryptTab())

        logText.Dock = DockStyle.Fill
        logText.Multiline = True
        logText.ReadOnly = True
        logText.ScrollBars = ScrollBars.Vertical

        root.Controls.Add(tabs, 0, 0)
        root.Controls.Add(logText, 0, 1)
        Me.Controls.Add(root)

        publicKeyText.Text = "C:\appMain\config\secure_trace_public.xml"
        privateKeyText.Text = Path.Combine(
            Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments),
            "secure_trace_private.xml")
    End Sub

    Private Function BuildKeyTab() As TabPage
        Dim page As New TabPage("Generar llaves")
        Dim panel As TableLayoutPanel = BuildFieldsPanel()

        AddPathRow(panel, 0, "Llave publica para el cajero", publicKeyText, AddressOf BrowsePublicKeySave)
        AddPathRow(panel, 1, "Llave privada para descifrar", privateKeyText, AddressOf BrowsePrivateKeySave)

        Dim generateButton As New Button()
        generateButton.Text = "Generar llaves"
        generateButton.Width = 150
        generateButton.Height = 34
        AddHandler generateButton.Click, AddressOf GenerateKeys

        panel.Controls.Add(generateButton, 1, 2)
        page.Controls.Add(panel)
        Return page
    End Function

    Private Function BuildDecryptTab() As TabPage
        Dim page As New TabPage("Descifrar")
        Dim panel As TableLayoutPanel = BuildFieldsPanel()

        AddPathRow(panel, 0, "Archivo cifrado .bin", encryptedFileText, AddressOf BrowseEncryptedFile)
        AddPathRow(panel, 1, "Llave privada", decryptPrivateKeyText, AddressOf BrowsePrivateKeyOpen)
        AddPathRow(panel, 2, "Guardar MSG como", outputFileText, AddressOf BrowseOutputFile)

        Dim decryptButton As New Button()
        decryptButton.Text = "Descifrar"
        decryptButton.Width = 150
        decryptButton.Height = 34
        AddHandler decryptButton.Click, AddressOf DecryptFile

        panel.Controls.Add(decryptButton, 1, 3)
        page.Controls.Add(panel)
        Return page
    End Function

    Private Function BuildFieldsPanel() As TableLayoutPanel
        Dim panel As New TableLayoutPanel()
        panel.Dock = DockStyle.Fill
        panel.ColumnCount = 3
        panel.RowCount = 5
        panel.Padding = New Padding(16)
        panel.ColumnStyles.Add(New ColumnStyle(SizeType.Absolute, 190))
        panel.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 100))
        panel.ColumnStyles.Add(New ColumnStyle(SizeType.Absolute, 100))

        For i As Integer = 0 To 4
            panel.RowStyles.Add(New RowStyle(SizeType.Absolute, 46))
        Next

        Return panel
    End Function

    Private Sub AddPathRow(panel As TableLayoutPanel, row As Integer, labelText As String, textBox As TextBox, browseHandler As EventHandler)
        Dim label As New Label()
        label.Text = labelText
        label.Dock = DockStyle.Fill
        label.TextAlign = Drawing.ContentAlignment.MiddleLeft

        textBox.Dock = DockStyle.Fill
        textBox.Margin = New Padding(0, 9, 8, 0)

        Dim button As New Button()
        button.Text = "Buscar"
        button.Dock = DockStyle.Fill
        button.Margin = New Padding(0, 7, 0, 7)
        AddHandler button.Click, browseHandler

        panel.Controls.Add(label, 0, row)
        panel.Controls.Add(textBox, 1, row)
        panel.Controls.Add(button, 2, row)
    End Sub

    Private Sub BrowsePublicKeySave(sender As Object, e As EventArgs)
        Using dialog As New SaveFileDialog()
            dialog.Filter = "XML (*.xml)|*.xml|Todos los archivos (*.*)|*.*"
            dialog.FileName = Path.GetFileName(publicKeyText.Text)
            dialog.InitialDirectory = SafeInitialDirectory(publicKeyText.Text)

            If dialog.ShowDialog(Me) = DialogResult.OK Then publicKeyText.Text = dialog.FileName
        End Using
    End Sub

    Private Sub BrowsePrivateKeySave(sender As Object, e As EventArgs)
        Using dialog As New SaveFileDialog()
            dialog.Filter = "XML (*.xml)|*.xml|Todos los archivos (*.*)|*.*"
            dialog.FileName = Path.GetFileName(privateKeyText.Text)
            dialog.InitialDirectory = SafeInitialDirectory(privateKeyText.Text)

            If dialog.ShowDialog(Me) = DialogResult.OK Then privateKeyText.Text = dialog.FileName
        End Using
    End Sub

    Private Sub BrowseEncryptedFile(sender As Object, e As EventArgs)
        Using dialog As New OpenFileDialog()
            dialog.Filter = "Secure trace (*.bin;*.zip)|*.bin;*.zip|BIN (*.bin)|*.bin|ZIP (*.zip)|*.zip|Todos los archivos (*.*)|*.*"
            dialog.InitialDirectory = SafeInitialDirectory(encryptedFileText.Text)

            If dialog.ShowDialog(Me) = DialogResult.OK Then
                encryptedFileText.Text = dialog.FileName
                If String.IsNullOrWhiteSpace(outputFileText.Text) Then
                    outputFileText.Text = Path.Combine(
                        Path.GetDirectoryName(dialog.FileName),
                        Path.GetFileNameWithoutExtension(dialog.FileName) & "_decrypted.txt")
                End If
            End If
        End Using
    End Sub

    Private Sub BrowsePrivateKeyOpen(sender As Object, e As EventArgs)
        Using dialog As New OpenFileDialog()
            dialog.Filter = "XML (*.xml)|*.xml|Todos los archivos (*.*)|*.*"
            dialog.InitialDirectory = SafeInitialDirectory(decryptPrivateKeyText.Text)

            If dialog.ShowDialog(Me) = DialogResult.OK Then decryptPrivateKeyText.Text = dialog.FileName
        End Using
    End Sub

    Private Sub BrowseOutputFile(sender As Object, e As EventArgs)
        Using dialog As New SaveFileDialog()
            dialog.Filter = "Texto (*.txt)|*.txt|MSG (*.msg)|*.msg|Todos los archivos (*.*)|*.*"
            dialog.FileName = Path.GetFileName(outputFileText.Text)
            dialog.InitialDirectory = SafeInitialDirectory(outputFileText.Text)

            If dialog.ShowDialog(Me) = DialogResult.OK Then outputFileText.Text = dialog.FileName
        End Using
    End Sub

    Private Function SafeInitialDirectory(pathValue As String) As String
        Try
            If Not String.IsNullOrWhiteSpace(pathValue) Then
                Dim directory As String = Path.GetDirectoryName(pathValue)
                If Not String.IsNullOrWhiteSpace(directory) AndAlso IO.Directory.Exists(directory) Then Return directory
            End If
        Catch
        End Try

        Return Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
    End Function

    Private Sub GenerateKeys(sender As Object, e As EventArgs)
        Try
            Dim publicPath As String = publicKeyText.Text.Trim()
            Dim privatePath As String = privateKeyText.Text.Trim()

            If String.IsNullOrWhiteSpace(publicPath) OrElse String.IsNullOrWhiteSpace(privatePath) Then
                Throw New InvalidOperationException("Selecciona ruta para llave publica y privada.")
            End If

            EnsureParentDirectory(publicPath)
            EnsureParentDirectory(privatePath)

            Using rsa As New RSACryptoServiceProvider(2048)
                rsa.PersistKeyInCsp = False
                File.WriteAllText(publicPath, rsa.ToXmlString(False), Encoding.UTF8)
                File.WriteAllText(privatePath, rsa.ToXmlString(True), Encoding.UTF8)
            End Using

            AppendLog("Llaves generadas.")
            AppendLog("Publica para el cajero: " & publicPath)
            AppendLog("Privada para autorizados: " & privatePath)
        Catch ex As Exception
            AppendLog("Error generando llaves: " & ex.Message)
            MessageBox.Show(Me, ex.Message, "No se pudieron generar las llaves", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub

    Private Sub DecryptFile(sender As Object, e As EventArgs)
        Try
            Dim encryptedPath As String = encryptedFileText.Text.Trim()
            Dim privatePath As String = decryptPrivateKeyText.Text.Trim()
            Dim outputPath As String = outputFileText.Text.Trim()

            If String.IsNullOrWhiteSpace(encryptedPath) OrElse Not File.Exists(encryptedPath) Then
                Throw New InvalidOperationException("Selecciona un archivo .bin existente.")
            End If

            If String.IsNullOrWhiteSpace(privatePath) OrElse Not File.Exists(privatePath) Then
                Throw New InvalidOperationException("Selecciona la llave privada.")
            End If

            If String.IsNullOrWhiteSpace(outputPath) Then
                outputPath = Path.Combine(
                    Path.GetDirectoryName(encryptedPath),
                    Path.GetFileNameWithoutExtension(encryptedPath) & "_decrypted.txt")
                outputFileText.Text = outputPath
            End If

            EnsureParentDirectory(outputPath)

            Dim tempOutputPath As String = outputPath & ".tmp"
            If File.Exists(tempOutputPath) Then File.Delete(tempOutputPath)

            Dim metadata As SecureTraceMetadata = SecureTraceCodec.DecryptToFile(encryptedPath, privatePath, tempOutputPath)

            If File.Exists(outputPath) Then File.Delete(outputPath)
            File.Move(tempOutputPath, outputPath)

            AppendLog("Archivo descifrado: " & outputPath)
            AppendLog("Entradas: " & metadata.EntryCount.ToString())
            AppendLog("Nombre original: " & metadata.OriginalName)
            AppendLog("Archivado UTC: " & metadata.ArchivedUtc.ToString("yyyy-MM-dd HH:mm:ss.fff"))
            MessageBox.Show(Me, "Archivo descifrado correctamente.", "Listo", MessageBoxButtons.OK, MessageBoxIcon.Information)
        Catch ex As Exception
            AppendLog("Error descifrando: " & ex.Message)
            MessageBox.Show(Me, ex.Message, "No se pudo descifrar", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub

    Private Sub EnsureParentDirectory(pathValue As String)
        Dim directory As String = Path.GetDirectoryName(pathValue)
        If Not String.IsNullOrWhiteSpace(directory) AndAlso Not IO.Directory.Exists(directory) Then
            IO.Directory.CreateDirectory(directory)
        End If
    End Sub

    Private Sub AppendLog(message As String)
        logText.AppendText(DateTime.Now.ToString("HH:mm:ss") & "  " & message & Environment.NewLine)
    End Sub
End Class

Public Structure SecureTraceMetadata
    Public OriginalName As String
    Public ArchivedUtc As DateTime
    Public EntryCount As Integer
End Structure

Public Module SecureTraceCodec
    Private ReadOnly LogMagic As Byte() = Encoding.ASCII.GetBytes("ASTRLOG1")
    Private ReadOnly OuterMagic As Byte() = Encoding.ASCII.GetBytes("ASTRACE1")
    Private ReadOnly InnerMagic As Byte() = Encoding.ASCII.GetBytes("ASTP1")

    Public Function DecryptToFile(encryptedPath As String, privateKeyPath As String, outputPath As String) As SecureTraceMetadata
        If Path.GetExtension(encryptedPath).Equals(".zip", StringComparison.OrdinalIgnoreCase) Then
            Return DecryptZipToFile(encryptedPath, privateKeyPath, outputPath)
        End If

        Dim allBytes As Byte() = File.ReadAllBytes(encryptedPath)

        If StartsWith(allBytes, LogMagic) Then
            Return DecryptLogToFile(allBytes, privateKeyPath, outputPath)
        End If

        Dim metadata As SecureTraceMetadata = DecryptSingleToFile(encryptedPath, privateKeyPath, outputPath)
        metadata.EntryCount = 1
        Return metadata
    End Function

    Private Function DecryptLogToFile(allBytes As Byte(), privateKeyPath As String, outputPath As String) As SecureTraceMetadata
        Dim entryCount As Integer = 0
        Dim firstArchivedUtc As DateTime = DateTime.MinValue

        Using output As New StreamWriter(outputPath, False, Encoding.UTF8)
            Using reader As New BinaryReader(New MemoryStream(allBytes), Encoding.UTF8)
                AssertBytesEqual(ReadExactBytes(reader, LogMagic.Length), LogMagic, "encabezado acumulado")

                While reader.BaseStream.Position < reader.BaseStream.Length
                    Dim remaining As Long = reader.BaseStream.Length - reader.BaseStream.Position
                    If remaining < 4 Then Throw New InvalidOperationException("El archivo acumulado esta incompleto.")

                    Dim recordLength As Integer = reader.ReadInt32()
                    If recordLength <= 0 OrElse recordLength > 104857600 Then
                        Throw New InvalidOperationException("Una entrada del archivo acumulado tiene longitud invalida.")
                    End If

                    If reader.BaseStream.Length - reader.BaseStream.Position < recordLength Then
                        Throw New InvalidOperationException("Una entrada del archivo acumulado esta incompleta.")
                    End If

                    Dim recordBytes As Byte() = ReadExactBytes(reader, recordLength)
                    Dim tempEncrypted As String = Path.GetTempFileName()
                    Dim tempPlain As String = Path.GetTempFileName()

                    Try
                        File.WriteAllBytes(tempEncrypted, recordBytes)
                        Dim recordMetadata As SecureTraceMetadata = DecryptSingleToFile(tempEncrypted, privateKeyPath, tempPlain)
                        Dim payload As String = File.ReadAllText(tempPlain, Encoding.UTF8)

                        entryCount += 1
                        If firstArchivedUtc = DateTime.MinValue Then firstArchivedUtc = recordMetadata.ArchivedUtc

                        output.WriteLine("============================================================")
                        output.WriteLine("SECURE TRACE ENTRY " & entryCount.ToString("000000"))
                        output.WriteLine("Archivo original: " & recordMetadata.OriginalName)
                        output.WriteLine("Archivado UTC: " & recordMetadata.ArchivedUtc.ToString("yyyy-MM-dd HH:mm:ss.fff"))
                        output.WriteLine("------------------------- CONTENIDO ------------------------")
                        output.WriteLine(payload)
                        output.WriteLine("----------------------- FIN CONTENIDO ----------------------")
                        output.WriteLine()
                    Finally
                        Try
                            If File.Exists(tempEncrypted) Then File.Delete(tempEncrypted)
                            If File.Exists(tempPlain) Then File.Delete(tempPlain)
                        Catch
                        End Try
                    End Try
                End While
            End Using
        End Using

        If entryCount = 0 Then Throw New InvalidOperationException("El archivo acumulado no contiene entradas.")

        Dim metadata As New SecureTraceMetadata()
        metadata.OriginalName = entryCount.ToString() & " entradas"
        metadata.ArchivedUtc = firstArchivedUtc
        metadata.EntryCount = entryCount
        Return metadata
    End Function

    Private Function DecryptZipToFile(encryptedPath As String, privateKeyPath As String, outputPath As String) As SecureTraceMetadata
        Dim entryCount As Integer = 0
        Dim firstArchivedUtc As DateTime = DateTime.MinValue

        Using output As New StreamWriter(outputPath, False, Encoding.UTF8)
            Using zip As ZipArchive = ZipFile.OpenRead(encryptedPath)
                For Each entry As ZipArchiveEntry In zip.Entries.OrderBy(Function(item) item.FullName)
                    If entry.Length <= 0 Then Continue For
                    If Not entry.FullName.EndsWith(".bin", StringComparison.OrdinalIgnoreCase) Then Continue For

                    Dim tempEncrypted As String = Path.GetTempFileName()
                    Dim tempPlain As String = Path.GetTempFileName()

                    Try
                        Using entryStream As Stream = entry.Open()
                            Using fileStream As New FileStream(tempEncrypted, FileMode.Create, FileAccess.Write, FileShare.None)
                                entryStream.CopyTo(fileStream)
                            End Using
                        End Using

                        Dim recordMetadata As SecureTraceMetadata = DecryptSingleToFile(tempEncrypted, privateKeyPath, tempPlain)
                        Dim payload As String = File.ReadAllText(tempPlain, Encoding.UTF8)

                        entryCount += 1
                        If firstArchivedUtc = DateTime.MinValue Then firstArchivedUtc = recordMetadata.ArchivedUtc

                        output.WriteLine("============================================================")
                        output.WriteLine("SECURE TRACE ENTRY " & entryCount.ToString("000000"))
                        output.WriteLine("Archivo original: " & recordMetadata.OriginalName)
                        output.WriteLine("Archivado UTC: " & recordMetadata.ArchivedUtc.ToString("yyyy-MM-dd HH:mm:ss.fff"))
                        output.WriteLine("------------------------- CONTENIDO ------------------------")
                        output.WriteLine(payload)
                        output.WriteLine("----------------------- FIN CONTENIDO ----------------------")
                        output.WriteLine()
                    Finally
                        Try
                            If File.Exists(tempEncrypted) Then File.Delete(tempEncrypted)
                            If File.Exists(tempPlain) Then File.Delete(tempPlain)
                        Catch
                        End Try
                    End Try
                Next
            End Using
        End Using

        If entryCount = 0 Then Throw New InvalidOperationException("El ZIP no contiene entradas cifradas.")

        Dim metadata As New SecureTraceMetadata()
        metadata.OriginalName = entryCount.ToString() & " entradas"
        metadata.ArchivedUtc = firstArchivedUtc
        metadata.EntryCount = entryCount
        Return metadata
    End Function

    Private Function DecryptSingleToFile(encryptedPath As String, privateKeyPath As String, outputPath As String) As SecureTraceMetadata
        Dim allBytes As Byte() = File.ReadAllBytes(encryptedPath)

        Dim encryptedKey As Byte()
        Dim iv As Byte()
        Dim cipherBytes As Byte()
        Dim tag As Byte()
        Dim bodyLength As Integer
        Dim secureTraceVersion As Integer

        Using reader As New BinaryReader(New MemoryStream(allBytes), Encoding.UTF8)
            AssertBytesEqual(ReadExactBytes(reader, OuterMagic.Length), OuterMagic, "encabezado")

            secureTraceVersion = reader.ReadInt32()
            If secureTraceVersion <> 1 AndAlso secureTraceVersion <> 2 Then Throw New InvalidOperationException("Version no soportada: " & secureTraceVersion)

            Dim encryptedKeyLength As Integer = reader.ReadInt32()
            Dim ivLength As Integer = reader.ReadInt32()
            Dim cipherLength As Integer = reader.ReadInt32()
            Dim tagLength As Integer = reader.ReadInt32()

            If encryptedKeyLength <= 0 OrElse ivLength <> 16 OrElse cipherLength <= 0 OrElse tagLength <> 32 Then
                Throw New InvalidOperationException("El archivo cifrado tiene longitudes invalidas.")
            End If

            encryptedKey = ReadExactBytes(reader, encryptedKeyLength)
            iv = ReadExactBytes(reader, ivLength)
            cipherBytes = ReadExactBytes(reader, cipherLength)
            tag = ReadExactBytes(reader, tagLength)
            bodyLength = allBytes.Length - tagLength
        End Using

        If bodyLength <= 0 Then Throw New InvalidOperationException("El archivo cifrado esta incompleto.")

        Dim keyMaterial As Byte()
        Using rsa As New RSACryptoServiceProvider(2048)
            rsa.PersistKeyInCsp = False
            rsa.FromXmlString(File.ReadAllText(privateKeyPath))
            keyMaterial = rsa.Decrypt(encryptedKey, True)
        End Using

        If keyMaterial.Length <> 64 Then Throw New InvalidOperationException("La llave privada no corresponde al archivo.")

        Dim aesKey(31) As Byte
        Dim hmacKey(31) As Byte
        Buffer.BlockCopy(keyMaterial, 0, aesKey, 0, 32)
        Buffer.BlockCopy(keyMaterial, 32, hmacKey, 0, 32)

        Dim bodyBytes(bodyLength - 1) As Byte
        Buffer.BlockCopy(allBytes, 0, bodyBytes, 0, bodyLength)

        Using hmac As New HMACSHA256(hmacKey)
            Dim expectedTag As Byte() = hmac.ComputeHash(bodyBytes)
            AssertBytesEqual(tag, expectedTag, "firma")
        End Using

        Dim plainPackage As Byte()
        Using aes As Aes = Aes.Create()
            aes.KeySize = 256
            aes.BlockSize = 128
            aes.Mode = CipherMode.CBC
            aes.Padding = PaddingMode.PKCS7
            aes.Key = aesKey
            aes.IV = iv

            Using decryptor As ICryptoTransform = aes.CreateDecryptor()
                plainPackage = decryptor.TransformFinalBlock(cipherBytes, 0, cipherBytes.Length)
            End Using
        End Using

        If secureTraceVersion = 2 Then
            plainPackage = Decompress(plainPackage)
        End If

        Array.Clear(keyMaterial, 0, keyMaterial.Length)
        Array.Clear(aesKey, 0, aesKey.Length)
        Array.Clear(hmacKey, 0, hmacKey.Length)

        Using reader As New BinaryReader(New MemoryStream(plainPackage), Encoding.UTF8)
            AssertBytesEqual(ReadExactBytes(reader, InnerMagic.Length), InnerMagic, "paquete")

            Dim nameLength As Integer = reader.ReadInt32()
            If nameLength < 0 OrElse nameLength > 1024 Then Throw New InvalidOperationException("Nombre original invalido.")

            Dim originalName As String = Encoding.UTF8.GetString(ReadExactBytes(reader, nameLength))
            Dim archiveTicks As Long = reader.ReadInt64()
            Dim createdTicks As Long = reader.ReadInt64()
            Dim lastWriteTicks As Long = reader.ReadInt64()
            Dim payloadLength As Integer = reader.ReadInt32()

            If payloadLength < 0 Then Throw New InvalidOperationException("Contenido invalido.")

            Dim payload As Byte() = ReadExactBytes(reader, payloadLength)
            File.WriteAllBytes(outputPath, payload)
            File.SetCreationTimeUtc(outputPath, New DateTime(createdTicks, DateTimeKind.Utc))
            File.SetLastWriteTimeUtc(outputPath, New DateTime(lastWriteTicks, DateTimeKind.Utc))

            Dim metadata As New SecureTraceMetadata()
            metadata.OriginalName = originalName
            metadata.ArchivedUtc = New DateTime(archiveTicks, DateTimeKind.Utc)
            metadata.EntryCount = 1
            Return metadata
        End Using
    End Function

    Private Function StartsWith(bytes As Byte(), prefix As Byte()) As Boolean
        If bytes Is Nothing OrElse bytes.Length < prefix.Length Then Return False

        For i As Integer = 0 To prefix.Length - 1
            If bytes(i) <> prefix(i) Then Return False
        Next

        Return True
    End Function

    Private Function Decompress(compressedPackage As Byte()) As Byte()
        Using input As New MemoryStream(compressedPackage)
            Using gzip As New GZipStream(input, CompressionMode.Decompress)
                Using output As New MemoryStream()
                    gzip.CopyTo(output)
                    Return output.ToArray()
                End Using
            End Using
        End Using
    End Function

    Private Function ReadExactBytes(reader As BinaryReader, count As Integer) As Byte()
        Dim bytes As Byte() = reader.ReadBytes(count)
        If bytes.Length <> count Then Throw New InvalidOperationException("Archivo incompleto.")
        Return bytes
    End Function

    Private Sub AssertBytesEqual(actual As Byte(), expected As Byte(), name As String)
        If actual.Length <> expected.Length Then Throw New InvalidOperationException("No coincide " & name & ".")

        Dim diff As Integer = 0
        For i As Integer = 0 To actual.Length - 1
            diff = diff Or (actual(i) Xor expected(i))
        Next

        If diff <> 0 Then Throw New InvalidOperationException("No coincide " & name & ".")
    End Sub
End Module
