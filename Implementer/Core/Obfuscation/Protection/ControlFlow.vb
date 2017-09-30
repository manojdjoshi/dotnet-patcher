Imports Mono.Cecil
Imports Mono.Cecil.Rocks
Imports Mono.Cecil.Cil
Imports Helper.CecilHelper
Imports Implementer.Core.Obfuscation.Exclusion

Namespace Core.Obfuscation.Protection
    ''' <summary>
    ''' By Furious from DotnetVitamin
    ''' </summary>
    Public NotInheritable Class ControlFlow

#Region " Fields "
        Private Shared Types As New List(Of TypeDefinition)
        Private Shared r As Random
#End Region

#Region " Constructor "
        Shared Sub New()
            r = New Random
        End Sub
#End Region

#Region " Methods "
        Friend Shared Function DoJob(asm As AssemblyDefinition, Exclude As ExcludeList) As AssemblyDefinition
            For Each m As ModuleDefinition In asm.Modules
                Types.AddRange(m.GetAllTypes())
                For Each type As TypeDefinition In Types
                    If Exclude.isControlFlowExclude(type) = False Then
                        IterateType(type)
                    End If
                Next
                Types.Clear()
            Next
            Return asm
        End Function

        Private Shared Sub IterateType(td As TypeDefinition)
            Dim publicMethods As New List(Of MethodDefinition)
            publicMethods.AddRange(From m In td.Methods Where (m.HasBody AndAlso m.Body.Instructions.Count > 0 AndAlso Not m.DeclaringType.BaseType Is Nothing AndAlso Utils.HasUnsafeInstructions(m) = False))
            'publicMethods.AddRange(From m In td.Methods Where (m.HasBody AndAlso m.Body.Instructions.Count > 2 AndAlso m.Body.Variables.Count > 1 AndAlso Not m.DeclaringType.BaseType Is Nothing AndAlso Not m.DeclaringType.BaseType.Name = "ApplicationSettingsBase" AndAlso Not m.DeclaringType.BaseType.Name = "WindowsFormsApplicationBase" AndAlso Not Finder.HasCustomAttributeByName(m.DeclaringType, "EditorBrowsableAttribute") AndAlso Not m.IsConstructor AndAlso Utils.HasUnsafeInstructions(m) = False))

            Try
                If Not publicMethods.Count = 0 Then
                    For Each md In publicMethods
                        If publicMethods.Contains(md) Then
                            If md.Body.ExceptionHandlers.Count = 0 Then
                                md.Body.SimplifyMacros

                                'For i = 0 To md.Body.Instructions.Count - 1
                                '    Dim instruct = md.Body.Instructions(i)
                                '    If instruct.OpCode = OpCodes.Call Then
                                '        If instruct.Operand.ToString.ToLower.Contains("void") Then

                                '            If instruct.Previous.OpCode = OpCodes.Ldarg Then
                                '                Dim loc As New VariableDefinition(td.Module.Assembly.MainModule.Import(GetType(Integer)))
                                '                md.Body.Variables.Add(loc)

                                '                md.Body.Instructions.Insert(i - 1, Instruction.Create(OpCodes.Ldc_I4, r.Next))
                                '                md.Body.Instructions.Insert(i, Instruction.Create(OpCodes.Stloc_S, loc))
                                '                md.Body.Instructions.Insert(i + 1, Instruction.Create(OpCodes.Ldloc_S, loc))
                                '                md.Body.Instructions.Insert(i + 2, Instruction.Create(OpCodes.Ldc_I4, r.Next))
                                '                md.Body.Instructions.Insert(i + 3, Instruction.Create(OpCodes.Ldarg_0))
                                '                md.Body.Instructions.Insert(i + 4, Instruction.Create(OpCodes.Nop))
                                '                md.Body.Instructions.Insert(i + 6, Instruction.Create(OpCodes.Nop))

                                '                md.Body.Instructions.Insert(i + 3, Instruction.Create(OpCodes.Bne_Un_S, md.Body.Instructions(i + 4)))
                                '                md.Body.Instructions.Insert(i + 5, Instruction.Create(OpCodes.Br_S, md.Body.Instructions(i + 8)))
                                '                md.Body.Instructions.Insert(i + 8, Instruction.Create(OpCodes.Br_S, md.Body.Instructions(i + 9)))

                                '            End If
                                '        End If
                                '        'ElseIf instruct.OpCode = OpCodes.Callvirt Then
                                '        '    If instruct.Operand.ToString.ToLower.Contains("int32") Then
                                '        '        If instruct.Previous.OpCode = OpCodes.Ldloc Then

                                '        '            Dim loc As New VariableDefinition(td.Module.Assembly.MainModule.Import(GetType(Integer)))
                                '        '            md.Body.Variables.Add(loc)

                                '        '            md.Body.Instructions.Insert(i - 1, Instruction.Create(OpCodes.Ldc_I4, r.Next))
                                '        '            md.Body.Instructions.Insert(i, Instruction.Create(OpCodes.Stloc_S, loc))
                                '        '            md.Body.Instructions.Insert(i + 1, Instruction.Create(OpCodes.Ldloc_S, loc))
                                '        '            md.Body.Instructions.Insert(i + 2, Instruction.Create(OpCodes.Ldc_I4, r.Next))
                                '        '            md.Body.Instructions.Insert(i + 3, Instruction.Create(OpCodes.Ldarg_0))
                                '        '            md.Body.Instructions.Insert(i + 4, Instruction.Create(OpCodes.Nop))
                                '        '            md.Body.Instructions.Insert(i + 6, Instruction.Create(OpCodes.Nop))

                                '        '            md.Body.Instructions.Insert(i + 3, Instruction.Create(OpCodes.Beq_S, md.Body.Instructions(i + 4)))
                                '        '            md.Body.Instructions.Insert(i + 5, Instruction.Create(OpCodes.Br_S, md.Body.Instructions(i + 8)))
                                '        '            md.Body.Instructions.Insert(i + 8, Instruction.Create(OpCodes.Br_S, md.Body.Instructions(i + 9)))

                                '        '        End If
                                '        '    End If
                                '        'ElseIf instruct.OpCode = OpCodes.Ldfld Then

                                '        '    If instruct.Previous.OpCode = OpCodes.Ldarg Then
                                '        '        Dim loc As New VariableDefinition(td.Module.Assembly.MainModule.Import(GetType(Integer)))
                                '        '        md.Body.Variables.Add(loc)

                                '        '        md.Body.Instructions.Insert(i - 1, Instruction.Create(OpCodes.Ldc_I4, r.Next))
                                '        '        md.Body.Instructions.Insert(i, Instruction.Create(OpCodes.Stloc_S, loc))
                                '        '        md.Body.Instructions.Insert(i + 1, Instruction.Create(OpCodes.Ldloc_S, loc))
                                '        '        md.Body.Instructions.Insert(i + 2, Instruction.Create(OpCodes.Ldc_I4, r.Next))
                                '        '        md.Body.Instructions.Insert(i + 3, Instruction.Create(OpCodes.Ldarg_0))
                                '        '        md.Body.Instructions.Insert(i + 4, Instruction.Create(OpCodes.Nop))
                                '        '        md.Body.Instructions.Insert(i + 6, Instruction.Create(OpCodes.Nop))

                                '        '        md.Body.Instructions.Insert(i + 3, Instruction.Create(OpCodes.Beq_S, md.Body.Instructions(i + 4)))
                                '        '        md.Body.Instructions.Insert(i + 5, Instruction.Create(OpCodes.Br_S, md.Body.Instructions(i + 8)))
                                '        '        md.Body.Instructions.Insert(i + 8, Instruction.Create(OpCodes.Br_S, md.Body.Instructions(i + 9)))
                                '        '    End If

                                '    End If
                                'Next


                                Dim incGroups As New InstructionGroups
                                Dim item1 As New InstructionGroup
                                Dim incremId As Integer = 0
                                Dim incremStackUsage As Integer = 0
                                Dim flag As Boolean = False
                                For i = 0 To md.Body.Instructions.Count - 1
                                    Dim Instruct = md.Body.Instructions(i)
                                    Dim stacks As Integer
                                    Dim pops As Integer = 0
                                    Msil.CalculateStackUsage(Instruct, stacks, pops)
                                    item1.Add(Instruct)
                                    incremStackUsage = (incremStackUsage + (stacks - pops))
                                    If (((stacks = 0) AndAlso (Not Instruct.OpCode = OpCodes.Nop)) AndAlso ((incremStackUsage = 0) OrElse (Instruct.OpCode = OpCodes.Ret))) Then
                                        If Not flag Then
                                            Dim item2 As New InstructionGroup
                                            item2.ID = incremId
                                            incremId += 1
                                            item2.nextGroup = (item2.ID + 1)
                                            incGroups.Add(item2)
                                            item2 = New InstructionGroup
                                            item2.ID = incremId
                                            incremId += 1
                                            item2.nextGroup = (item2.ID + 1)
                                            incGroups.Add(item2)
                                            flag = True
                                        End If
                                        item1.ID = incremId
                                        incremId += 1
                                        item1.nextGroup = (item1.ID + 1)
                                        incGroups.Add(item1)
                                        item1 = New InstructionGroup
                                    End If
                                Next
                                If (incGroups.Count <> 1) Then
                                    Dim item3 As InstructionGroup = incGroups.getLast
                                    incGroups.Scramble(incGroups)
                                    md.Body.Instructions.Clear()
                                    Dim local As New VariableDefinition(td.Module.Assembly.MainModule.Import(GetType(Integer)))
                                    md.Body.Variables.Add(local)
                                    Dim target As Instruction = Instruction.Create(OpCodes.Nop)
                                    Dim instruction3 As Instruction = Instruction.Create(OpCodes.Br, target)
                                    md.Body.Instructions.Add(Instruction.Create(OpCodes.Ldc_I4_0))
                                    md.Body.Instructions.Add(Instruction.Create(OpCodes.Stloc, local))
                                    md.Body.Instructions.Add(Instruction.Create(OpCodes.Br, instruction3))
                                    md.Body.Instructions.Add(target)
                                    Dim group4 As InstructionGroup
                                    For Each group4 In incGroups
                                        If (Not group4 Is item3) Then
                                            md.Body.Instructions.Add(Instruction.Create(OpCodes.Ldloc, local))
                                            md.Body.Instructions.Add(Instruction.Create(OpCodes.Ldc_I4, group4.ID))
                                            'md.Body.Instructions.Add(ReturnLdciInstruction(group4.ID))

                                            md.Body.Instructions.Add(Instruction.Create(OpCodes.Ceq))
                                            Dim instruction4 As Instruction = Instruction.Create(OpCodes.Nop)
                                            md.Body.Instructions.Add(Instruction.Create(OpCodes.Brfalse, instruction4))
                                            Dim instruction5 As Instruction
                                            For Each instruction5 In group4
                                                md.Body.Instructions.Add(instruction5)
                                            Next
                                            md.Body.Instructions.Add(Instruction.Create(OpCodes.Ldc_I4, group4.nextGroup))
                                            'md.Body.Instructions.Add(ReturnLdciInstruction(group4.nextGroup))

                                            md.Body.Instructions.Add(Instruction.Create(OpCodes.Stloc, local))
                                            md.Body.Instructions.Add(instruction4)
                                        End If
                                    Next
                                    md.Body.Instructions.Add(Instruction.Create(OpCodes.Ldloc, local))
                                    md.Body.Instructions.Add(Instruction.Create(OpCodes.Ldc_I4, incGroups.Count - 1))
                                    'md.Body.Instructions.Add(ReturnLdciInstruction(CInt((incGroups.Count - 1))))

                                    md.Body.Instructions.Add(Instruction.Create(OpCodes.Ceq))
                                    md.Body.Instructions.Add(Instruction.Create(OpCodes.Brfalse, instruction3))
                                    md.Body.Instructions.Add(Instruction.Create(OpCodes.Br, item3.Item(0)))
                                    md.Body.Instructions.Add(instruction3)
                                    Dim instruction6 As Instruction
                                    For Each instruction6 In item3
                                        md.Body.Instructions.Add(instruction6)
                                    Next
                                End If
                                md.Body.OptimizeMacros
                                md.Body.ComputeOffsets()
                                md.Body.ComputeHeader()
                            End If
                        End If
                    Next
                End If
            Catch ex As Exception
                MsgBox("ControlFlow Error : " & vbNewLine & ex.ToString)
            End Try
            publicMethods.Clear()
        End Sub
#End Region

        'Private Shared Function ReturnLdciInstruction(num As Integer) As Instruction
        '    If num >= 0 AndAlso num < 9 Then
        '        Select Case num
        '            Case 0
        '                Return Instruction.Create(OpCodes.Ldc_I4_0)
        '            Case 1
        '                Return Instruction.Create(OpCodes.Ldc_I4_1)
        '            Case 2
        '                Return Instruction.Create(OpCodes.Ldc_I4_2)
        '            Case 3
        '                Return Instruction.Create(OpCodes.Ldc_I4_3)
        '            Case 4
        '                Return Instruction.Create(OpCodes.Ldc_I4_4)
        '            Case 5
        '                Return Instruction.Create(OpCodes.Ldc_I4_5)
        '            Case 6
        '                Return Instruction.Create(OpCodes.Ldc_I4_6)
        '            Case 7
        '                Return Instruction.Create(OpCodes.Ldc_I4_7)
        '            Case 8
        '                Return Instruction.Create(OpCodes.Ldc_I4_8)
        '        End Select
        '    End If
        '    Return Instruction.Create(OpCodes.Ldc_I4, num)
        'End Function

    End Class

End Namespace
