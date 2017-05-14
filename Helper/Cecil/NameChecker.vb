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
            Return Not type.FullName = "<Module>" AndAlso Not type.IsImport AndAlso Not type.IsSerializable AndAlso Not type.HasGenericParameters
        End Function

        ''' <summary>
        ''' INFO : Verifying if methodDefinition is renamable
        ''' </summary>
        ''' <param name="method"></param>
        Public Shared Function IsRenamable(method As MethodDefinition, Optional ByVal Force As Boolean = False) As Boolean
            If Force Then
                If method.HasBody Then
                    If Finder.AccessorMethods(method.DeclaringType).Contains(method) Then
                        Return Not Finder.FindGenericParameter(method) AndAlso Not Finder.FindCustomAttributeByName(method, "DebuggerHiddenAttribute")
                    End If
                End If
            End If
            'Return method IsNot Nothing AndAlso Not (method.IsRuntimeSpecialName OrElse method.IsRuntime OrElse method.IsSpecialName OrElse method.IsConstructor OrElse method.HasOverrides OrElse method.IsAbstract OrElse method.HasGenericParameters OrElse method.DeclaringType.IsSerializable OrElse method.Name.EndsWith("GetEnumerator"))
            Return method IsNot Nothing AndAlso Not (method.IsRuntimeSpecialName OrElse method.IsRuntime OrElse method.IsSpecialName OrElse method.IsConstructor OrElse method.HasOverrides OrElse method.IsVirtual OrElse method.IsAbstract OrElse method.HasGenericParameters OrElse method.Name.EndsWith("GetEnumerator"))
        End Function

        ''' <summary>
        ''' INFO : Verifying if eventDefinition is renamable
        ''' </summary>
        ''' <param name="Events"></param>
        Public Shared Function IsRenamable(Events As EventDefinition) As Boolean
            Return If(Not Events.IsSpecialName OrElse Not Events.IsRuntimeSpecialName OrElse Not Events.IsDefinition OrElse Not Events.DeclaringType.IsSerializable, True, False)
        End Function

        ''' <summary>
        ''' INFO : Verifying if propertyDefinition is renamable
        ''' </summary>
        ''' <param name="prop"></param>
        Public Shared Function IsRenamable(prop As PropertyDefinition) As Boolean
            Return prop IsNot Nothing AndAlso Not (prop.IsRuntimeSpecialName OrElse prop.IsSpecialName OrElse prop.DeclaringType.IsSerializable OrElse prop.DeclaringType.IsGenericInstance OrElse prop.DeclaringType.HasGenericParameters)
        End Function

        ''' <summary>
        ''' INFO : Verifying if fieldDefinition is renamable
        ''' </summary>
        ''' <param name="field"></param>
        Public Shared Function IsRenamable(field As FieldDefinition) As Boolean
            If (Not field.IsRuntimeSpecialName AndAlso Not field.DeclaringType.HasGenericParameters AndAlso Not field.DeclaringType.IsSerializable AndAlso Not field.DeclaringType.IsGenericInstance) And Not field.IsPInvokeImpl AndAlso Not field.IsSpecialName Then
                Return True
            End If
            Return False
        End Function
#End Region

    End Class
End Namespace

