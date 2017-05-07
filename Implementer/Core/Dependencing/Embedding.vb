Imports System.IO
Imports Mono.Cecil
Imports Helper.RandomizeHelper
Imports Helper.CecilHelper
Imports Implementer.Core.Dependencing.DependenciesInfos
Imports Helper.UtilsHelper
Imports Implementer.Core.Obfuscation.Builder

Namespace Core.Dependencing
    Public NotInheritable Class Embedding
        Inherits Source

#Region " Fields "
        Private m_encrypt As Boolean
        Private m_compress As Boolean
        Private m_files As IEnumerable(Of String)
#End Region

#Region " Constructor "
        Friend Sub New(assDef As AssemblyDefinition, files As IEnumerable(Of String), framewk As String, CompressEncrypt As CompressEncryptMode, Optional ByVal EnabledPack As Boolean = False)
            AssemblyDef = assDef
            m_files = files
            Frmwk = framewk
            Pack = EnabledPack

            Select Case CompressEncrypt
                Case CompressEncryptMode.Both
                    m_encrypt = True
                    m_compress = True
                Case CompressEncryptMode.Compress
                    m_compress = True
                Case CompressEncryptMode.Encrypt
                    m_encrypt = True
            End Select
        End Sub

#End Region

#Region " Methods "

        Friend Sub InjectFiles()
            For Each f In m_files
                Dim assname = AssemblyDefinition.ReadAssembly(f)
                Dim resNameGUID = CStr(assname.FullName.GetHashCode)
                If Pack Then resNameGUID = Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(resNameGUID))

                Dim encryptedBytes = File.ReadAllBytes(f)
                Dim resourceName = String.Format("{0}.{1}", resNameGUID, "resources")

                If m_encrypt Then Array.Reverse(encryptedBytes)
                If m_compress Then encryptedBytes = Functions.GZipedByte(encryptedBytes)

                Injecter.InjectResource(AssemblyDef.MainModule, resourceName, ResourceType.Embedded, encryptedBytes)
            Next
        End Sub

        Friend Sub CreateResolverClass()
            Try
                Dim reposit As New Stub(Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic)
                With reposit
                    .ResolveTypeFromFile(ResourcesEmbeddingStub(.className, .funcName1, m_encrypt, m_compress))
                    .InjectToCctor(AssemblyDef)
                    .DeleteDll()
                End With
            Catch ex As Exception
                MsgBox(ex.ToString)
            End Try
        End Sub
#End Region

    End Class
End Namespace
