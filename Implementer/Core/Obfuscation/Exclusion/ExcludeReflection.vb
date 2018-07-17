Imports Mono.Cecil.Cil
Imports Mono.Cecil.Rocks
Imports Mono.Cecil

Namespace Core.Obfuscation.Exclusion

    Public NotInheritable Class ExcludeReflection

#Region " Properties "
        Public Shared HasItems As Boolean
#End Region

#Region " Methods "
        Public Shared Sub AnalyzeCodes(assDef As AssemblyDefinition, m_exclude As ExcludeList)
            For Each modul In assDef.Modules
                If modul.HasTypes Then
                    For Each type In modul.GetAllTypes
                        If type.HasMethods Then
                            For Each method In type.Methods
                                If method.HasBody AndAlso method.Body.Instructions.Count <> 0 Then
                                    For i = 0 To method.Body.Instructions.Count - 1
                                        Dim Inst = method.Body.Instructions(i)
                                        If TypeOf Inst.Operand Is MethodReference Then
                                            Try
                                                Dim refer As MethodReference = TryCast(Inst.Operand, MethodReference)
                                                Dim id = refer.DeclaringType.FullName & "::" & refer.Name
                                                If ExclusionReflection.Reflections.ContainsKey(id) Then
                                                    Dim Rmtd = ExclusionReflection.Reflections(id)
                                                    Dim memInst As Instruction = Nothing
                                                    Dim mem = ReflectionAnalyzer.StackTrace(i, method.Body, Rmtd, method.Module, memInst)
                                                    If Not mem Is Nothing Then
                                                        m_exclude.AddTo(New ExclusionState(True, TryCast(mem, TypeDefinition), ExclusionState.mType.Types, False, False, False, False, True, False, False))
                                                        m_exclude.AddTo(New ExclusionState(True, TryCast(refer, MethodDefinition), ExclusionState.mType.Methods, False, False, False, False, False, True, False))
                                                        HasItems = True
                                                    End If
                                                End If
                                            Catch ex As Exception
                                            End Try
                                        End If
                                    Next
                                End If
                            Next
                        End If
                    Next
                End If
            Next
        End Sub

        Public Shared Sub CleanUp()
            HasItems = False
        End Sub

#End Region

    End Class

End Namespace
