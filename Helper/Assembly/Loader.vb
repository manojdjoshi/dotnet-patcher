Imports System.Reflection
Imports System.IO
Imports Helper.RandomizeHelper

Namespace AssemblyHelper
    Public Class Loader

        Private Shared m_AssPath As String
        Private Shared m_AssList As List(Of String)

        Shared Sub New()
            m_AssList = New List(Of String)
        End Sub

#Region " Methods "
        Public Shared Function Minimal(AssPath$) As Data

            Dim tempAppDomain As AppDomain = Nothing
            Dim fName = Randomizer.GenerateAlphabetic(10)
            Dim fileName = Path.GetFileName(AssPath)
            Dim Npath As String = String.Format("{0}{1}\", Path.GetTempPath, fName)
            Directory.CreateDirectory(Npath)
            Dim tempAssemblyFilePath As String = (Npath & fileName)
            File.Copy(AssPath, tempAssemblyFilePath, True)

            Dim AsmPath = Directory.GetParent(AssPath).FullName
            m_AssPath = AsmPath

            Dim AssData As New Data
            Try
                For Each f In Directory.GetFiles(AsmPath, "*.dll")
                    If Not File.Exists(Directory.GetParent(Assembly.GetExecutingAssembly.Location).FullName & "\" & New FileInfo(f).Name) Then
                        m_AssList.Add(Directory.GetParent(Assembly.GetExecutingAssembly.Location).FullName & "\" & New FileInfo(f).Name)
                        File.Copy(f, Directory.GetParent(Assembly.GetExecutingAssembly.Location).FullName & "\" & New FileInfo(f).Name, True)
                    End If
                Next

                tempAppDomain = AppDomain.CreateDomain(Randomizer.GenerateAlphabetic(10), Nothing, Directory.GetParent(Assembly.GetExecutingAssembly.Location).FullName, "", False)

                Dim assemblyBuffer As Byte() = File.ReadAllBytes(tempAssemblyFilePath)
                Dim anObject As Object = Nothing
                anObject = tempAppDomain.CreateInstanceAndUnwrap(Assembly.GetExecutingAssembly.GetName.Name, "Helper.AssemblyHelper.Infos")

                Dim assemblyInspector As IAssemblyInfos = TryCast(anObject, IAssemblyInfos)

                Dim AssName = String.Empty
                Dim FrmwkVersion = String.Empty
                Dim AssVersion = String.Empty
                Dim IsWpf As Boolean
                Dim Location = String.Empty
                Dim EntryPoint As MethodInfo = Nothing
                Dim AssemblyReferences As AssemblyName() = Nothing
                Dim ManifestResourceNames As IEnumerable(Of String) = Nothing
                Dim ManifestResourceStreams As New List(Of Stream)
                Dim TypesClass As IEnumerable(Of Type) = Nothing
                Dim HasSerializableAttribute As Boolean = False
                Dim Result As Data.Message

                assemblyInspector.GetAssemblyInfo(assemblyBuffer, AssName, FrmwkVersion, AssVersion, IsWpf, EntryPoint, AssemblyReferences, ManifestResourceNames, ManifestResourceStreams, TypesClass, HasSerializableAttribute, Result)

                With AssData
                    .AssName = AssName
                    .FrameworkVersion = FrmwkVersion
                    .AssVersion = AssVersion
                    .IsWpf = IsWpf
                    .Location = AssPath
                    .EntryPoint = EntryPoint
                    .AssemblyReferences = AssemblyReferences
                    .Result = Result
                End With

            Catch exception As Exception
                MsgBox(exception.ToString)
            Finally
                CleanDomain(tempAppDomain, tempAssemblyFilePath, Npath)
            End Try
            Return AssData
        End Function

        Public Shared Function Full(AssPath$) As DataFull

            Dim tempAppDomain As AppDomain = Nothing
            Dim fName = Randomizer.GenerateAlphabetic(10)
            Dim fileName = Path.GetFileName(AssPath)
            Dim Npath As String = String.Format("{0}{1}\", Path.GetTempPath, fName)
            Directory.CreateDirectory(Npath)
            Dim tempAssemblyFilePath As String = (Npath & fileName)
            File.Copy(AssPath, tempAssemblyFilePath, True)

            Dim AsmPath = Directory.GetParent(AssPath).FullName
            m_AssPath = AsmPath

            Dim AssData As New DataFull
            Try
                For Each f In Directory.GetFiles(AsmPath, "*.dll")
                    If Not File.Exists(Directory.GetParent(Assembly.GetExecutingAssembly.Location).FullName & "\" & New FileInfo(f).Name) Then
                        m_AssList.Add(Directory.GetParent(Assembly.GetExecutingAssembly.Location).FullName & "\" & New FileInfo(f).Name)
                        File.Copy(f, Directory.GetParent(Assembly.GetExecutingAssembly.Location).FullName & "\" & New FileInfo(f).Name, True)
                    End If
                Next
                tempAppDomain = AppDomain.CreateDomain(Randomizer.GenerateAlphabetic(10), Nothing, Directory.GetParent(Assembly.GetExecutingAssembly.Location).FullName, "", False)

                Dim assemblyBuffer As Byte() = File.ReadAllBytes(tempAssemblyFilePath)
                Dim anObject As Object = Nothing
                anObject = tempAppDomain.CreateInstanceAndUnwrap(Assembly.GetExecutingAssembly.GetName.Name, "Helper.AssemblyHelper.Infos")

                Dim assemblyInspector As IAssemblyInfos = TryCast(anObject, IAssemblyInfos)

                Dim AssName = String.Empty
                Dim FrmwkVersion = String.Empty
                Dim AssVersion = String.Empty
                Dim IsWpf As Boolean
                Dim Location = String.Empty
                Dim EntryPoint As MethodInfo = Nothing
                Dim AssemblyReferences As AssemblyName() = Nothing
                Dim ManifestResourceNames As IEnumerable(Of String) = Nothing
                Dim ManifestResourceStreams As New List(Of Stream)
                Dim TypesClass As IEnumerable(Of Type) = Nothing
                Dim HasSerializableAttribute As Boolean = False
                Dim Result As DataFull.Message

                assemblyInspector.GetAssemblyInfo(assemblyBuffer, AssName, FrmwkVersion, AssVersion, IsWpf, EntryPoint, AssemblyReferences, ManifestResourceNames, ManifestResourceStreams, TypesClass, HasSerializableAttribute, Result, True)

                With AssData
                    .AssName = AssName
                    .FrameworkVersion = FrmwkVersion
                    .AssVersion = AssVersion
                    .IsWpf = IsWpf
                    .Location = New FileInfo(AssPath).DirectoryName
                    .EntryPoint = EntryPoint
                    .AssemblyReferences = AssemblyReferences
                    .ManifestResourceNames = ManifestResourceNames
                    .ManifestResourceStreams = ManifestResourceStreams
                    .TypesClass = TypesClass
                    .HasSerializableAttribute = HasSerializableAttribute
                    .Result = Result
                End With

            Catch exception As Exception
                MsgBox(exception.ToString)
            Finally
                CleanDomain(tempAppDomain, tempAssemblyFilePath, Npath)
            End Try
            Return AssData
        End Function

        Private Shared Sub CleanDomain(tempAppDomain As AppDomain, tempAssemblyFilePath$, path$)
            If Not tempAppDomain Is Nothing Then
                AppDomain.Unload(tempAppDomain)

                For Each f In Directory.GetFiles(path)
                    File.Delete(f)
                Next
                For Each f In m_AssList
                    If File.Exists(f) Then
                        Try
                            File.Delete(f)
                        Catch ex As Exception
                        End Try
                    End If
                Next
                If Directory.Exists(path) Then
                    Directory.Delete(path)
                End If

                m_AssList.Clear()
            End If
        End Sub

        Public Shared Function GenerateInfos(ByVal Title As String, ByVal Description As String, ByVal Company As String, ByVal Product As String, ByVal Copyright As String, ByVal Trademark As String, ByVal Version As String) As String
            Return "Imports System.Reflection" & vbNewLine & vbNewLine _
            & "<" & "Assembly: AssemblyTitle(""" & Title & """)>" & vbNewLine _
            & "<Assembly: AssemblyDescription(""" & Description & """)>" & vbNewLine _
            & "<" & "Assembly: AssemblyCompany(""" & Company & """)>" & vbNewLine _
            & "<Assembly: AssemblyProduct(""" & Product & """)>" & vbNewLine _
            & "<Assembly: AssemblyCopyright(""" & Copyright & """)>" & vbNewLine _
            & "<" & "Assembly: AssemblyTrademark(""" & Trademark & """)>" & vbNewLine _
            & "<Assembly: AssemblyVersion(""" & Version & """)>" & vbNewLine _
            & "<Assembly: AssemblyFileVersion(""" & Version & """)>"
        End Function
#End Region

    End Class
End Namespace