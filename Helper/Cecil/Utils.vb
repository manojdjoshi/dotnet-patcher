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

        'Public Shared Function RemoveCustomAttributeByName(member As FieldDefinition, CaName$) As Boolean
        '    If member.HasCustomAttributes Then
        '        Dim caList = Enumerable.Where(member.CustomAttributes, Function(ca) ca.AttributeType.Name = CaName)
        '        Dim caCount = caList.Count
        '        If caCount <> 0 Then
        '            Dim Finded = caList.First
        '            If Not Finded Is Nothing Then
        '                Return member.CustomAttributes.Remove(Finded)
        '            End If
        '        End If
        '    End If
        '    Return False
        'End Function

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

        Public Shared Sub UpdateAssemblyReference(ByVal typeDef As TypeDefinition, ByVal from As String, ByVal [to] As String)
            UpdateCustomAttributeRef(typeDef)
            If typeDef.HasGenericParameters Then
                Dim parameter As GenericParameter
                For Each parameter In typeDef.GenericParameters
                    UpdateCustomAttributeRef(parameter)
                Next
            End If
            Dim definition As MethodDefinition
            For Each definition In typeDef.Methods
                If definition.HasParameters Then
                    Dim definition2 As ParameterDefinition
                    For Each definition2 In definition.Parameters
                        UpdateCustomAttributeRef(definition2)
                    Next
                End If
                If definition.HasGenericParameters Then
                    Dim parameter2 As GenericParameter
                    For Each parameter2 In definition.GenericParameters
                        UpdateCustomAttributeRef(parameter2)
                    Next
                End If
                UpdateCustomAttributeRef(definition.MethodReturnType)
                UpdateCustomAttributeRef(definition)
            Next
            Dim definition3 As FieldDefinition
            For Each definition3 In typeDef.Fields
                UpdateCustomAttributeRef(definition3)
            Next
            Dim definition4 As PropertyDefinition
            For Each definition4 In typeDef.Properties
                UpdateCustomAttributeRef(definition4)
            Next
            Dim definition5 As EventDefinition
            For Each definition5 In typeDef.Events
                UpdateCustomAttributeRef(definition5)
            Next
            Dim definition6 As TypeDefinition
            For Each definition6 In typeDef.NestedTypes
                UpdateAssemblyReference(definition6, from, [to])
            Next
            Dim definition7 As MethodDefinition
            For Each definition7 In typeDef.Methods
                If definition7.HasBody Then
                    Dim instruction As Instruction
                    For Each instruction In definition7.Body.Instructions
                        If TypeOf instruction.Operand Is String Then
                            Dim operand As String = CStr(instruction.Operand)
                            If operand.Contains(from) Then
                                operand = operand.Replace(from, [to])
                            End If
                            instruction.Operand = operand
                        End If
                    Next
                End If
            Next
        End Sub

        Public Shared Sub UpdateCustomAttributeArgs(ByVal arg As CustomAttributeArgument)
            If TypeOf arg.Value Is TypeReference Then
                Dim reference As TypeReference = TryCast(arg.Value, TypeReference)

                If TypeOf reference.Scope Is AssemblyNameReference Then
                    Dim scope As AssemblyNameReference = TryCast(reference.Scope, AssemblyNameReference)
                    'MsgBox(scope.FullName)
                    'Dim setting As AssemblySetting
                    'For Each setting In Me.settings
                    '    If (setting.Assembly.Name.Name = scope.Name) Then
                    '        reference.Scope = setting.Assembly.Name
                    '    End If
                    'Next
                End If
            ElseIf TypeOf arg.Value Is CustomAttributeArgument() Then
                Dim argument As CustomAttributeArgument
                For Each argument In TryCast(arg.Value, CustomAttributeArgument())
                    UpdateCustomAttributeArgs(argument)
                Next
            End If
        End Sub

        Public Shared Sub UpdateCustomAttributeRef(ByVal ca As ICustomAttributeProvider)
            If ca.HasCustomAttributes Then
                Dim attribute As CustomAttribute
                For Each attribute In ca.CustomAttributes
                    Dim argument As CustomAttributeArgument
                    For Each argument In attribute.ConstructorArguments
                        UpdateCustomAttributeArgs(argument)
                    Next
                    Dim argument2 As CustomAttributeNamedArgument
                    For Each argument2 In attribute.Fields
                        UpdateCustomAttributeArgs(argument2.Argument)
                    Next
                    Dim argument3 As CustomAttributeNamedArgument
                    For Each argument3 In attribute.Properties
                        UpdateCustomAttributeArgs(argument3.Argument)
                    Next
                Next
            End If
        End Sub



#End Region

    End Class
End Namespace