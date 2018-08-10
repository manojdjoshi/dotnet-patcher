Imports Mono.Cecil
Imports Mono.Cecil.Cil

Namespace CecilHelper
    Public NotInheritable Class Utils

#Region " Methods "

        Public Shared Function HasUnsafeInstructions(member As MethodDefinition) As Boolean
            If member.HasBody Then
                If member.Body.HasVariables Then
                    Return member.Body.Variables.Any(Function(x) x.VariableType.IsPointer)
                End If
            End If
            Return False
        End Function

        Public Shared Function RemoveCustomAttributeByName(member As AssemblyDefinition, CaName$) As Boolean
            If member.HasCustomAttributes Then
                Dim caList = Enumerable.Where(member.CustomAttributes, Function(ca) ca.AttributeType.Name = CaName)
                Dim caCount = caList.Count
                If caCount <> 0 Then
                    Dim Finded = caList.First
                    If Not Finded Is Nothing Then
                        Return member.CustomAttributes.Remove(Finded)
                    End If
                End If
            End If
            Return False
        End Function

        Public Shared Function RemoveCustomAttributeByName(member As MethodDefinition, CaName$) As Boolean
            If member.HasCustomAttributes Then
                Dim caList = Enumerable.Where(member.CustomAttributes, Function(ca) ca.AttributeType.Name = CaName)
                Dim caCount = caList.Count
                If caCount <> 0 Then
                    Dim Finded = caList.First
                    If Not Finded Is Nothing Then
                        Return member.CustomAttributes.Remove(Finded)
                    End If
                End If
            End If
            Return False
        End Function

        Public Shared Function isStronglyTypedResourceBuilder(td As TypeDefinition) As Boolean
            If td.HasCustomAttributes Then
                For Each ca In (From c In td.CustomAttributes
                                Where c IsNot Nothing AndAlso c.AttributeType.Name = "GeneratedCodeAttribute" AndAlso c.HasConstructorArguments AndAlso c.ConstructorArguments(0).Value = "System.Resources.Tools.StronglyTypedResourceBuilder"
                                Select c)
                    Return True
                Next
            End If
            Return False
        End Function

        Public Shared Function IsSettingStr(md As MethodDefinition, str$) As Boolean
            If md.IsGetter Then
                Return md.Name.ToLower = "get_" & str.ToLower
            ElseIf md.IsSetter Then
                Return md.Name.ToLower = "set_" & str.ToLower
            End If
            Return False
        End Function

        Public Shared Function IsDebuggerNonUserCode(assDef As AssemblyDefinition) As Boolean
            Return assDef.MainModule.EntryPoint.DeclaringType.Namespace.EndsWith(".My")
        End Function

        Public Shared Function MakeGeneric(method As MethodReference, genericarg As TypeReference) As MethodReference
            Dim genericTypeRef = New GenericInstanceMethod(method)
            genericTypeRef.GenericArguments.Add(genericarg)
            Return genericTypeRef
        End Function

        Public Shared Function HasSerializableAttributes(path) As Boolean
            Dim ass As AssemblyDefinition = AssemblyDefinition.ReadAssembly(path)
            Dim HasAtt As Boolean = False
            For Each mo In ass.Modules
                If HasAtt Then Exit For
                For Each ty In mo.GetTypes
                    If ty.Attributes.HasFlag(TypeAttributes.Serializable) Then
                        HasAtt = True
                        Exit For
                    End If
                Next
            Next
            Return HasAtt
        End Function
#End Region

    End Class
End Namespace