Imports Mono.Cecil
Imports Mono.Cecil.Rocks
Imports Mono.Cecil.Cil
Imports Helper.CecilHelper
Imports Implementer.Core.Obfuscation.Exclusion

Namespace Core.Obfuscation.Protection
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
            publicMethods.AddRange(From m In td.Methods Where (m.HasBody AndAlso m.Body.Instructions.Count > 2 AndAlso m.Body.Variables.Count > 1 AndAlso Not m.DeclaringType.BaseType Is Nothing AndAlso Not m.DeclaringType.BaseType.Name = "ApplicationSettingsBase" AndAlso Not m.DeclaringType.BaseType.Name = "WindowsFormsApplicationBase" AndAlso Not Finder.HasCustomAttributeByName(m.DeclaringType, "EditorBrowsableAttribute") AndAlso Not m.IsConstructor))

            Try
                If Not publicMethods.Count = 0 Then
                    For Each md In publicMethods
                        If publicMethods.Contains(md) Then
                            If Utils.HasUnsafeInstructions(md) = False Then
                                If md.Body.ExceptionHandlers.Count = 0 Then
                                    Using optim As New Msil(md.Body)
                                        Dim incGroups As New InstructionGroups
                                        Dim item As New InstructionGroup
                                        Dim num As Integer = 0
                                        Dim num2 As Integer = 0
                                        Dim flag As Boolean = False
                                        For i = 0 To md.Body.Instructions.Count - 1
                                            Dim Instruct = md.Body.Instructions(i)
                                            Dim num3 As Integer
                                            Dim pops As Integer = 0
                                            Msil.CalculateStackUsage(Instruct, num3, pops)
                                            item.Add(Instruct)
                                            num2 = (num2 + (num3 - pops))
                                            If (((num3 = 0) AndAlso (Not Instruct.OpCode = OpCodes.Nop)) AndAlso ((num2 = 0) OrElse (Instruct.OpCode = OpCodes.Ret))) Then
                                                If Not flag Then
                                                    Dim group2 As New InstructionGroup
                                                    group2.ID = num
                                                    num += 1
                                                    group2.nextGroup = (group2.ID + 1)
                                                    incGroups.Add(group2)
                                                    group2 = New InstructionGroup
                                                    group2.ID = num
                                                    num += 1
                                                    group2.nextGroup = (group2.ID + 1)
                                                    incGroups.Add(group2)
                                                    flag = True
                                                End If
                                                item.ID = num
                                                num += 1
                                                item.nextGroup = (item.ID + 1)
                                                incGroups.Add(item)
                                                item = New InstructionGroup
                                            End If
                                        Next
                                        If (incGroups.Count <> 1) Then
                                            Dim group3 As InstructionGroup = incGroups.getLast
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
                                                If (Not group4 Is group3) Then
                                                    md.Body.Instructions.Add(Instruction.Create(OpCodes.Ldloc, local))
                                                    md.Body.Instructions.Add(Instruction.Create(OpCodes.Ldc_I4, group4.ID))
                                                    md.Body.Instructions.Add(Instruction.Create(OpCodes.Ceq))
                                                    Dim instruction4 As Instruction = Instruction.Create(OpCodes.Nop)
                                                    md.Body.Instructions.Add(Instruction.Create(OpCodes.Brfalse, instruction4))
                                                    Dim instruction5 As Instruction
                                                    For Each instruction5 In group4
                                                        md.Body.Instructions.Add(instruction5)
                                                    Next
                                                    md.Body.Instructions.Add(Instruction.Create(OpCodes.Ldc_I4, group4.nextGroup))
                                                    md.Body.Instructions.Add(Instruction.Create(OpCodes.Stloc, local))
                                                    md.Body.Instructions.Add(instruction4)
                                                End If
                                            Next
                                            md.Body.Instructions.Add(Instruction.Create(OpCodes.Ldloc, local))
                                            md.Body.Instructions.Add(Instruction.Create(OpCodes.Ldc_I4, CInt((incGroups.Count - 1))))
                                            md.Body.Instructions.Add(Instruction.Create(OpCodes.Ceq))
                                            md.Body.Instructions.Add(Instruction.Create(OpCodes.Brfalse, instruction3))
                                            md.Body.Instructions.Add(Instruction.Create(OpCodes.Br, group3.Item(0)))
                                            md.Body.Instructions.Add(instruction3)
                                            Dim instruction6 As Instruction
                                            For Each instruction6 In group3
                                                md.Body.Instructions.Add(instruction6)
                                            Next
                                        End If
                                        optim.FixBranchOffsets()
                                        optim.MethodBody.SimplifyMacros()
                                    End Using
                                End If
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

    End Class

End Namespace
