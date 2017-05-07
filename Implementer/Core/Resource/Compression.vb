Imports System.IO
Imports Mono.Cecil
Imports Helper.RandomizeHelper
Imports Helper.CecilHelper
Imports Helper.UtilsHelper
Imports Implementer.Core.Obfuscation.Builder

Namespace Core.Resources
    Public Class Compression
        Inherits Source

#Region " Fields "
        Private m_encrypt As Boolean
        Private m_compress As Boolean
#End Region

#Region " Constructor "
        Friend Sub New(assDef As AssemblyDefinition, frwk As String, encrypt As Boolean, compress As Boolean, EnabledPack As Boolean)
            AssemblyDef = assDef
            Frmwk = frwk
            m_encrypt = encrypt
            m_compress = compress
            Pack = EnabledPack

            ResName = Randomizer.GenerateNew & ".resources"
        End Sub

        Friend Sub New(frwk As String, encrypt As Boolean, compress As Boolean, EnabledPack As Boolean)
            Frmwk = frwk
            m_encrypt = encrypt
            m_compress = compress
            Pack = EnabledPack

            ResName = Randomizer.GenerateNew & ".resources"
        End Sub
#End Region

#Region " Methods "
        Friend Sub CreateResolverClass(SevenZip As Boolean)
            Try
                Dim m_NamespaceName = String.Empty
                If Not AssemblyDef Is Nothing Then m_NamespaceName = Finder.FindDefaultNamespace(AssemblyDef, Pack)

                Dim reposit As New Stub(Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic)
                With reposit
                    .ResolveTypeFromFile(If(SevenZip = False, ResourcesStub(.className, .funcName1, .funcName2, .funcName3, .funcName4, m_encrypt, m_compress), _
                                      SevenZipStub(.className, .funcName1, .funcName2, .funcName3, .funcName4, m_encrypt, m_compress)), m_NamespaceName, Randomizer.GenerateNew, Randomizer.GenerateNew, Randomizer.GenerateNew, Randomizer.GenerateNew, Randomizer.GenerateNew)
                    .InjectToCctor(AssemblyDef)
                    .DeleteDll()
                End With
            Catch ex As Exception
                MsgBox(ex.ToString)
            End Try
        End Sub

        Friend Sub InjectSevenzipLibrary(assDef As AssemblyDefinition)
            Dim encryptedBytes = My.Resources.SevenzipLib
            If m_encrypt Then Array.Reverse(encryptedBytes)
            If m_compress Then encryptedBytes = Functions.GZipedByte(encryptedBytes)

            Injecter.InjectResource(assDef.MainModule, ResName, ResourceType.Embedded, encryptedBytes)
        End Sub

        Friend Sub CompressInjectResources(assDef As AssemblyDefinition)
            Dim tempAsm = AssemblyDefinition.CreateAssembly(New AssemblyNameDefinition(Randomizer.GenerateNewAlphabetic, New Version()), Randomizer.GenerateNewAlphabetic, ModuleKind.Dll)
            For Each resou As Mono.Cecil.Resource In assDef.MainModule.Resources
                tempAsm.MainModule.Resources.Add(resou)
            Next

            Dim encryptedBytes As Byte() = Nothing
            Using stream As New MemoryStream()
                tempAsm.Write(stream)
                encryptedBytes = stream.ToArray()
                If m_encrypt Then Array.Reverse(encryptedBytes)
                If m_compress Then encryptedBytes = Functions.GZipedByte(encryptedBytes)
            End Using

            assDef.MainModule.Resources.Clear()
            Injecter.InjectResource(assDef.MainModule, ResName, ResourceType.Embedded, encryptedBytes.ToArray)
        End Sub

#End Region

    End Class
End Namespace