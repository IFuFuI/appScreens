Imports System.IO
Imports System.Security.Cryptography
Imports System.Text

Public Module SecureTraceArchive
    Private ReadOnly LogMagic As Byte() = Encoding.ASCII.GetBytes("ASTRLOG1")
    Private ReadOnly OuterMagic As Byte() = Encoding.ASCII.GetBytes("ASTRACE1")
    Private ReadOnly InnerMagic As Byte() = Encoding.ASCII.GetBytes("ASTP1")
    Private missingPublicKeyLogged As Boolean = False

    Public Sub ArchiveFileBeforeDelete(archivo As String)
        If Not ConfigManager.SecureTraceArchiveEnabled Then Exit Sub
        If String.IsNullOrWhiteSpace(archivo) OrElse Not File.Exists(archivo) Then Exit Sub

        Try
            Dim publicKeyPath As String = ConfigManager.SecureTracePublicKeyFile

            If Not File.Exists(publicKeyPath) Then
                If Not missingPublicKeyLogged Then
                    missingPublicKeyLogged = True
                    Trace("Secure trace no guardado: no existe llave publica " & publicKeyPath, 1)
                End If

                Exit Sub
            End If

            Dim plainBytes As Byte() = File.ReadAllBytes(archivo)
            Dim packagedBytes As Byte() = BuildPlainPackage(archivo, plainBytes)
            Dim publicKeyXml As String = File.ReadAllText(publicKeyPath)
            Dim encryptedBytes As Byte() = EncryptPackage(packagedBytes, publicKeyXml)
            Dim destinationPath As String = BuildArchivePath()

            AppendEncryptedRecord(destinationPath, encryptedBytes)
            Trace("Secure trace guardado: " & destinationPath)
        Catch ex As Exception
            Trace("Secure trace no guardado: " & ex.Message, 1)
        End Try
    End Sub

    Private Function BuildArchivePath() As String
        Dim archiveDir As String = ConfigManager.SecureTraceArchiveDirectory
        If String.IsNullOrWhiteSpace(archiveDir) Then archiveDir = "C:\appMain\packages\upload\data"
        If Not Directory.Exists(archiveDir) Then Directory.CreateDirectory(archiveDir)

        Dim stamp As String = DateTime.Now.ToString("yyyyMMdd")

        Return Path.Combine(archiveDir, "trace_" & stamp & ".log")
    End Function

    Private Sub AppendEncryptedRecord(destinationPath As String, encryptedRecord As Byte())
        Using stream As New FileStream(destinationPath, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.Read)
            If stream.Length = 0 Then
                stream.Write(LogMagic, 0, LogMagic.Length)
            Else
                Dim header(LogMagic.Length - 1) As Byte
                stream.Read(header, 0, header.Length)

                If Not BytesEqual(header, LogMagic) Then
                    Throw New InvalidOperationException("El archivo acumulado no tiene formato secure trace.")
                End If
            End If

            stream.Seek(0, SeekOrigin.End)

            Using writer As New BinaryWriter(stream, Encoding.UTF8)
                writer.Write(encryptedRecord.Length)
                writer.Write(encryptedRecord)
            End Using
        End Using
    End Sub

    Private Function BytesEqual(a As Byte(), b As Byte()) As Boolean
        If a Is Nothing OrElse b Is Nothing OrElse a.Length <> b.Length Then Return False

        Dim diff As Integer = 0
        For i As Integer = 0 To a.Length - 1
            diff = diff Or (a(i) Xor b(i))
        Next

        Return diff = 0
    End Function

    Private Function BuildPlainPackage(archivo As String, plainBytes As Byte()) As Byte()
        Using output As New MemoryStream()
            Using writer As New BinaryWriter(output, Encoding.UTF8)
                Dim nameBytes As Byte() = Encoding.UTF8.GetBytes(Path.GetFileName(archivo))

                writer.Write(InnerMagic)
                writer.Write(nameBytes.Length)
                writer.Write(nameBytes)
                writer.Write(DateTime.UtcNow.Ticks)
                writer.Write(File.GetCreationTimeUtc(archivo).Ticks)
                writer.Write(File.GetLastWriteTimeUtc(archivo).Ticks)
                writer.Write(plainBytes.Length)
                writer.Write(plainBytes)
            End Using

            Return output.ToArray()
        End Using
    End Function

    Private Function EncryptPackage(plainPackage As Byte(), publicKeyXml As String) As Byte()
        Dim aesKey(31) As Byte
        Dim hmacKey(31) As Byte
        Dim iv(15) As Byte

        Using rng As New RNGCryptoServiceProvider()
            rng.GetBytes(aesKey)
            rng.GetBytes(hmacKey)
            rng.GetBytes(iv)
        End Using

        Dim cipherBytes As Byte()
        Using aes As Aes = Aes.Create()
            aes.KeySize = 256
            aes.BlockSize = 128
            aes.Mode = CipherMode.CBC
            aes.Padding = PaddingMode.PKCS7
            aes.Key = aesKey
            aes.IV = iv

            Using encryptor As ICryptoTransform = aes.CreateEncryptor()
                cipherBytes = encryptor.TransformFinalBlock(plainPackage, 0, plainPackage.Length)
            End Using
        End Using

        Dim keyMaterial(aesKey.Length + hmacKey.Length - 1) As Byte
        Buffer.BlockCopy(aesKey, 0, keyMaterial, 0, aesKey.Length)
        Buffer.BlockCopy(hmacKey, 0, keyMaterial, aesKey.Length, hmacKey.Length)

        Dim encryptedKey As Byte()
        Using rsa As New RSACryptoServiceProvider(2048)
            rsa.PersistKeyInCsp = False
            rsa.FromXmlString(publicKeyXml)
            encryptedKey = rsa.Encrypt(keyMaterial, True)
        End Using

        Array.Clear(aesKey, 0, aesKey.Length)
        Array.Clear(keyMaterial, 0, keyMaterial.Length)

        Using body As New MemoryStream()
            Using writer As New BinaryWriter(body, Encoding.UTF8)
                writer.Write(OuterMagic)
                writer.Write(1)
                writer.Write(encryptedKey.Length)
                writer.Write(iv.Length)
                writer.Write(cipherBytes.Length)
                writer.Write(32)
                writer.Write(encryptedKey)
                writer.Write(iv)
                writer.Write(cipherBytes)
            End Using

            Dim bodyBytes As Byte() = body.ToArray()
            Dim tag As Byte()

            Using hmac As New HMACSHA256(hmacKey)
                tag = hmac.ComputeHash(bodyBytes)
            End Using

            Array.Clear(hmacKey, 0, hmacKey.Length)

            Using output As New MemoryStream()
                output.Write(bodyBytes, 0, bodyBytes.Length)
                output.Write(tag, 0, tag.Length)
                Return output.ToArray()
            End Using
        End Using
    End Function
End Module
