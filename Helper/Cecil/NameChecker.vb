Imports Mono.Cecil

Namespace CecilHelper
    Public NotInheritable Class NameChecker

#Region " Methods "
        ''' <summary>
        ''' INFO : Verifying if typeDefinition is renamable
        ''' </summary>
        ''' <param name="type"></param>
        Public Shared Function IsRenamable(type As TypeDefinition) As Boolean
            If Not type.BaseType Is Nothing Then
                If type.BaseType.IsArray OrElse type.BaseType.IsGenericInstance Then
                    Return False
                End If
            End If

            If Not type.IsSerializable Then
                type.Attributes = (type.Attributes And (Not TypeAttributes.VisibilityMask)) Or (If(type.DeclaringType Is Nothing, TypeAttributes.NotPublic, TypeAttributes.NestedAssembly) And TypeAttributes.VisibilityMask)
            End If

            Return Not type.FullName = "<Module>" AndAlso Not type.IsImport AndAlso Not type.IsSerializable AndAlso Not type.HasGenericParameters
        End Function

        ''' <summary>
        ''' INFO : Verifying if methodDefinition is renamable
        ''' </summary>
        ''' <param name="method"></param>
        Public Shared Function IsRenamable(method As MethodDefinition, Optional ByVal Force As Boolean = False) As Boolean
            If method IsNot Nothing Then
                If Force Then
                    If method.HasBody Then
                        If Finder.AccessorMethods(method.DeclaringType).Contains(method) Then
                            Return Not Finder.FindGenericParameter(method) AndAlso Not Finder.HasCustomAttributeByName(method, "DebuggerHiddenAttribute")
                        End If
                    End If
                End If
                Return Not method.IsRuntimeSpecialName AndAlso Not method.IsRuntime AndAlso Not method.IsSpecialName AndAlso Not method.IsConstructor AndAlso Not method.HasOverrides AndAlso Not method.IsVirtual AndAlso Not method.IsAbstract AndAlso Not method.HasGenericParameters AndAlso Not method.IsHideBySig AndAlso Not method.Name.EndsWith("GetEnumerator")
            End If
            Return False
        End Function

        ''' <summary>
        ''' INFO : Verifying if eventDefinition is renamable
        ''' </summary>
        ''' <param name="Events"></param>
        Public Shared Function IsRenamable(Events As EventDefinition) As Boolean
            If Events IsNot Nothing Then
                Return If(Not Events.IsSpecialName AndAlso Not Events.IsRuntimeSpecialName AndAlso Not Events.IsDefinition AndAlso Not Events.DeclaringType.IsSerializable, True, False)
            End If
            Return False
        End Function

        ''' <summary>
        ''' INFO : Verifying if propertyDefinition is renamable
        ''' </summary>
        ''' <param name="prop"></param>
        Public Shared Function IsRenamable(prop As PropertyDefinition) As Boolean
            If prop IsNot Nothing Then
                Dim HasGetMethod As Boolean = If(prop.GetMethod Is Nothing, False, True)
                Dim HasSetMethod As Boolean = If(prop.SetMethod Is Nothing, False, True)

                Dim renamable As Boolean = True

                If HasGetMethod And HasSetMethod Then
                    renamable = If(prop.GetMethod.IsAbstract = False AndAlso prop.GetMethod.IsHideBySig = False AndAlso prop.SetMethod.IsAbstract = False AndAlso prop.SetMethod.IsHideBySig = False, True, False)
                ElseIf HasGetMethod And HasSetMethod = False Then
                    renamable = If(prop.GetMethod.IsAbstract = False AndAlso prop.GetMethod.IsHideBySig = False, True, False)
                ElseIf HasGetMethod = False And HasSetMethod Then
                    renamable = If(prop.SetMethod.IsAbstract = False AndAlso prop.SetMethod.IsHideBySig = False, True, False)
                End If

                Dim IsSerializable As Boolean = Finder.HasCustomAttributeByName(prop, "XmlIgnoreAttribute") = False AndAlso prop.DeclaringType.IsSerializable
                Return renamable AndAlso Not prop.IsRuntimeSpecialName AndAlso Not prop.IsSpecialName AndAlso Not IsSerializable AndAlso Not prop.DeclaringType.IsGenericInstance AndAlso Not prop.DeclaringType.HasGenericParameters
            End If
            Return False
        End Function

        ''' <summary>
        ''' INFO : Verifying if fieldDefinition is renamable
        ''' </summary>
        ''' <param name="field"></param>
        Public Shared Function IsRenamable(field As FieldDefinition) As Boolean
            Dim IsSerializable As Boolean = Finder.HasCustomAttributeByName(field, "XmlIgnoreAttribute") = False AndAlso field.DeclaringType.IsSerializable
            If (Not field.IsRuntimeSpecialName AndAlso Not field.DeclaringType.HasGenericParameters AndAlso Not IsSerializable AndAlso Not field.DeclaringType.IsGenericInstance) And Not field.IsPInvokeImpl AndAlso Not field.IsSpecialName Then
                Return True
            End If
            Return False
        End Function
#End Region

    End Class
End Namespace

