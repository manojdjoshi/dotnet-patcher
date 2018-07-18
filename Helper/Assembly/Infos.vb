Imports System.Reflection
Imports System.IO

Namespace AssemblyHelper

    <Serializable>
    Public Class Infos
        Implements IAssemblyInfos

#Region " Methods "

        Private Sub AssemblyInfo(assemblyBuffer As Byte(), ByRef AssName$, ByRef FrmwkVersion$, ByRef AssVersion$, ByRef IsWpfApp As Boolean, ByRef EntryPoint As MethodInfo, ByRef AssemblyReferences As AssemblyName(), ByRef ManifestResourceNames As IEnumerable(Of String), ByRef ManifestResourceStreams As List(Of Stream), ByRef TypesClass As IEnumerable(Of Type), ByRef HasSerializableAttribute As Boolean, ByRef Result As Data.Message, Optional ByVal LoadMaxInfos As Boolean = False)
            Try
                Dim assembly = AppDomain.CurrentDomain.Load(assemblyBuffer)

                Dim manifest = assembly.ManifestModule
                AssName = manifest.ScopeName
                AssVersion = assembly.GetName.Version.ToString()

                Dim frameworkName = String.Empty
                Dim frameworkDisplayName = String.Empty
                Dim customAttributes = assembly.GetCustomAttributesData()
                For Each att In customAttributes
                    For Each attca In att.NamedArguments
                        If attca.MemberInfo.Name.ToString = "FrameworkDisplayName" Then
                            If att.ConstructorArguments.Count <> 0 Then
                                If Not att.ConstructorArguments(0).Value Is Nothing Then
                                    If att.ConstructorArguments(0).Value.ToString().ToLower.Contains(",version=") Then
                                        FrmwkVersion = att.ConstructorArguments(0).Value.ToString().Split("=")(1).Replace(",Client", String.Empty).Replace(",Profile", String.Empty).Trim
                                        Exit For
                                    End If
                                End If
                            End If
                        End If
                    Next
                Next

                Dim isWpfProg = assembly.GetReferencedAssemblies().Any(Function(x) x.Name.ToLower = "system.xaml") AndAlso
        assembly.GetManifestResourceNames().Any(Function(x) x.ToLower.EndsWith(".g.resources"))

                IsWpfApp = isWpfProg
                EntryPoint = assembly.EntryPoint
                AssemblyReferences = assembly.GetReferencedAssemblies

                If LoadMaxInfos = True Then

                    ManifestResourceNames = assembly.GetManifestResourceNames

                    For Each r In ManifestResourceNames
                        Dim resourceStream As Stream = assembly.GetManifestResourceStream(r)
                        If Not resourceStream Is Nothing Then
                            ManifestResourceStreams.Add(resourceStream)
                        End If
                    Next

                    TypesClass = assembly.ManifestModule.GetTypes
                    If Not TypesClass Is Nothing Then
                        TypesClass = assembly.GetTypes.Where(Function(t) t.IsClass)
                        For Each typ In TypesClass
                            If HasSerializableAttribute Then Exit For
                            If typ.Attributes.HasFlag(TypeAttributes.Serializable) Then
                                HasSerializableAttribute = True
                                Exit For
                            End If
                        Next
                    End If
                End If
                Result = Data.Message.Success
            Catch ex As ReflectionTypeLoadException
                Result = Data.Message.Failed
            Catch ex As FileNotFoundException
                Result = Data.Message.Failed
            Catch ex As FileLoadException
                Result = Data.Message.Failed
            Catch ex As NotSupportedException
                Result = Data.Message.Failed
            Catch ex As BadImageFormatException
                Result = Data.Message.Failed
            Finally

            End Try
        End Sub

        Public Sub GetAssemblyInfo(assembly() As Byte, ByRef AssName$, ByRef FrmwkVersion$, ByRef AssVersion$, ByRef IsWpfApp As Boolean, ByRef EntryPoint As MethodInfo, ByRef AssemblyReferences As AssemblyName(), ByRef ManifestResourceNames As IEnumerable(Of String), ByRef ManifestResourceStreams As List(Of Stream), ByRef TypesClass As IEnumerable(Of Type), ByRef HasSerializableAttribute As Boolean, ByRef Result As Data.Message, Optional ByVal LoadMaxInfos As Boolean = False) Implements IAssemblyInfos.GetAssemblyInfo
            AssemblyInfo(assembly, AssName, FrmwkVersion, AssVersion, IsWpfApp, EntryPoint, AssemblyReferences, ManifestResourceNames, ManifestResourceStreams, TypesClass, HasSerializableAttribute, Result, LoadMaxInfos)
        End Sub

#End Region

    End Class

End Namespace
