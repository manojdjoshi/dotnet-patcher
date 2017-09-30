Imports System.IO
Imports System.Text
Imports Mono.Cecil
Imports Helper.CryptoHelper
Imports Implementer.Core.Versions
Imports Helper.RandomizeHelper
Imports Helper.UtilsHelper
Imports Helper.CecilHelper
Imports SevenZipLib
Imports Implementer.Core.Obfuscation.Builder
Imports Implementer.Core.IconChanger
Imports Implementer.Core.ManifestRequest
Imports System.Drawing
Imports System.IO.Compression

Namespace Core.Packer
    Friend Class Pack
        Inherits Source

#Region " Fields "
        Private m_FilePathToPack As String = String.Empty
        Private m_reverse As Boolean
        Private m_polyXor As New Crypt
#End Region

#Region " Properties "
        Public Property OutputFilePath() As String
#End Region

#Region " Constructor "
        Friend Sub New(FilePathToPack As String)
            m_FilePathToPack = FilePathToPack
            m_reverse = Randomizer.GenerateBoolean
        End Sub
#End Region

#Region " Methods "
        Friend Function CreateStub(framework$, SevenZipResPath$) As String
            Dim tmpFile = Functions.GetTempFolder & "\" & New FileInfo(m_FilePathToPack).Name.Replace(".exe", Randomizer.GenerateNewAlphabetic & ".exe")
            Try
                Frmwk = framework
                Dim targetRuntime As TargetRuntime = TargetRuntime.Net_4_0
                Select Case Environment.Version.Major
                    Case 2
                        targetRuntime = TargetRuntime.Net_2_0
                    Case 3
                        targetRuntime = TargetRuntime.Net_2_0
                    Case 4
                        targetRuntime = TargetRuntime.Net_4_0
                End Select

                Dim mainmodule = AssemblyDefinition.ReadAssembly(m_FilePathToPack).MainModule
                mainmodule.Runtime = targetRuntime
                Dim parameters As New ModuleParameters With {.Architecture = mainmodule.Architecture,
                                                             .Kind = mainmodule.Kind,
                                                             .Runtime = mainmodule.Runtime}

                Dim asm = AssemblyDefinition.CreateAssembly(mainmodule.Assembly.Name, mainmodule.Name, parameters)
                Dim asmModule = asm.MainModule

                For Each m In mainmodule.AssemblyReferences
                    asmModule.AssemblyReferences.Add(m)
                Next

                For Each m In mainmodule.ModuleReferences
                    asmModule.ModuleReferences.Add(m)
                Next

                asmModule.Attributes = (asmModule.Attributes Or (mainmodule.Attributes And ModuleAttributes.Required32Bit))

                File.Copy(m_FilePathToPack, tmpFile, True)

                Dim EncodedResName = GetEncodedFileName(tmpFile)

                Dim PackerLoader = New Stub(Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic)

                With PackerLoader
                    .ReferencedZipperAssembly = New ZipInfos(SevenZipResPath, My.Resources.SevenzipLib, "SevenZipLib", "SevenZipHelper", "Decompress")
                    .ResolveTypeFromFile(PackerStub(PackerLoader, EncodedResName, m_polyXor, m_reverse))
                    .InjectType(asm)
                    completedMethods.Add(.GetMethod1)
                End With

                Dim byt = m_polyXor.Encrypt(File.ReadAllBytes(tmpFile))

                If m_reverse Then Array.Reverse(byt)

                Injecter.InjectResource(asm.MainModule, EncodedResName, ResourceType.Embedded, CompressWithSevenZip(byt).ToArray)
                'Injecter.InjectResource(asm.MainModule, EncodedResName, ResourceType.Embedded, CompressWithGzip(byt).ToArray)

                asm.MainModule.Assembly.EntryPoint = Enumerable.FirstOrDefault(PackerLoader.resolvedTypeDef.Methods, Function(mtd) (mtd.Name = "Main"))
                asm.Write(m_FilePathToPack)

                Return m_FilePathToPack

            Catch ex As Exception
                MsgBox("Error : Packer CreateExecutable : " & vbNewLine & ex.ToString)
            Finally
                Try
                    File.Delete(tmpFile)
                    File.Delete(SevenZipResPath)
                Catch ex As Exception
                End Try
            End Try

            Return m_FilePathToPack
        End Function

        Public Sub ReplaceIcon(newIconByte As Icon)
            Replacer.ReplaceFromIcon(m_FilePathToPack, newIconByte)
        End Sub

        Public Sub InjectAssemblyVersionInfos(vInfos As Infos)
            Injector.InjectAssemblyVersionInfos(m_FilePathToPack, vInfos)
        End Sub

        Public Sub InjectManifest(reqLevel$)
            ManifestWriter.ApplyManifest(m_FilePathToPack, reqLevel)
        End Sub

        Private Function GetEncodedFileName(asmName As String) As String
            Dim asm = AssemblyDefinition.ReadAssembly(asmName)
            Dim compressedName = Convert.ToBase64String(Encoding.Default.GetBytes(asm.FullName.ToLower))
            compressedName &= ".resources"
            Return compressedName
        End Function

        Private Function CompressWithSevenZip(raw As Byte()) As Byte()
            Return SevenZipHelper.Compress(raw)
        End Function

        Private Function CompressWithGzip(raw As Byte()) As Byte()
            Using memory As New MemoryStream()
                Using gzip As New DeflateStream(memory, CompressionMode.Compress)
                    gzip.Write(raw, 0, raw.Length)
                End Using
                Return memory.ToArray()
            End Using
        End Function
#End Region

    End Class
End Namespace
