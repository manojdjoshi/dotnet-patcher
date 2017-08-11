Imports System.Runtime.InteropServices
Imports Mono.Cecil
Imports Mono.Cecil.Cil

Namespace CecilHelper
    Public Class Msil
        Implements IDisposable

#Region " Properties "
        Property MethodBody As MethodBody
        Property ProcessedInstructions As List(Of ProcessedIL)
#End Region

#Region " Delegates "
        Public Delegate Sub ListIterateAllDelegate(Of T)(ByVal list As IList(Of T), ByVal index As Integer, ByVal value As T)
        Public Delegate Function ListIterateDelegate(Of T)(ByVal list As IList(Of T), ByVal index As Integer, ByVal value As T) As Boolean
        Public Delegate Function EnumerableIterateDelegate(Of T)(ByVal index As Integer, ByVal value As T) As Boolean
        Public Delegate Sub EnumerableIterateAllDelegate(Of T)(ByVal index As Integer, ByVal value As T)
#End Region

#Region " Constructor "
        Public Sub New(methodBody As MethodBody)
            _MethodBody = methodBody
            CollectInstructions()
        End Sub
#End Region

#Region " Methods "
        Public Sub Append(newInstruction As Instruction)
            _MethodBody.Instructions.Add(newInstruction)
            ProcessedInstructions.Add(New ProcessedIL(newInstruction))
        End Sub

        Public Sub CalculateOffsets()
            Dim num As Integer = 0
            Dim i As Integer
            For i = 0 To Me.ProcessedInstructions.Count - 1
                Me.ProcessedInstructions.Item(i).Instruction.Offset = num
                num = (num + Me.ProcessedInstructions.Item(i).Instruction.GetSize)
            Next i
        End Sub

        Private Sub CollectInstructions()
            ProcessedInstructions = New List(Of ProcessedIL)
            Dim i%
            For i = 0 To Me._MethodBody.Instructions.Count - 1
                _MethodBody.Instructions.Item(i).OpCode = Me.SimplifyOpCode(Me._MethodBody.Instructions.Item(i).OpCode)
                ProcessedInstructions.Add(New ProcessedIL(Me.MethodBody.Instructions.Item(i)))
            Next i
        End Sub

        Public Function containsTryCatch() As Boolean
            Return _MethodBody.ExceptionHandlers.Count <> 0
        End Function

        Public Function IsSettingStr(str$) As Boolean
            If _MethodBody.Method.IsGetter Then
                Return _MethodBody.Method.Name.ToLower = "get_" & str.ToLower
            ElseIf _MethodBody.Method.IsSetter Then
                Return _MethodBody.Method.Name.ToLower = "set_" & str.ToLower
            End If
            Return False
        End Function

        Public Sub FixBranchOffsets()
            For Each instruct In ProcessedInstructions
                If (instruct.OriginalOffset <> -1) Then
                    If ((Not instruct.Instruction.Operand Is Nothing) AndAlso TypeOf instruct.Instruction.Operand Is Instruction) Then
                        Dim operand = TryCast(instruct.Instruction.Operand, Instruction)
                        For Each instruction3 In ProcessedInstructions
                            If (operand.Offset = instruction3.OriginalOffset) Then
                                instruct.Instruction.OpCode = SimplifyOpCode(instruct.Instruction.OpCode)
                                instruct.Instruction.Operand = instruction3.Instruction
                            End If
                        Next
                    End If
                    If (instruct.Instruction.OpCode.OperandType = OperandType.InlineSwitch) Then
                        Dim instructionArray As Instruction() = TryCast(instruct.Instruction.Operand, Instruction())
                        Dim i%
                        For i = 0 To instructionArray.Length - 1
                            For Each instruction4 In ProcessedInstructions
                                If (instructionArray(i).Offset = instruction4.OriginalOffset) Then
                                    instructionArray(i) = instruction4.Instruction
                                End If
                            Next
                        Next i
                        instruct.Instruction.Operand = instructionArray
                    End If
                End If
            Next
        End Sub

        Private Function GetIndex(instruct As Instruction) As Integer
            Dim i%
            For i = 0 To ProcessedInstructions.Count - 1
                If (ProcessedInstructions.Item(i).Instruction Is instruct) Then
                    Return i
                End If
            Next i
            Return -1
        End Function

        Private Function GetOpCodeByName(name As String) As OpCode?
            Dim info As Reflection.FieldInfo
            For Each info In GetType(OpCodes).GetFields
                If (info.Name.ToLower = name) Then
                    Return New OpCode?(DirectCast(info.GetValue(Nothing), OpCode))
                End If
            Next
            Return Nothing
        End Function

        Public Sub InsertAfter(targetInstruction As Instruction, newInstruction As Instruction)
            Dim index% = (GetIndex(targetInstruction) + 1)
            _MethodBody.Instructions.Insert(index, newInstruction)
            UpdateExceptionHandlers(newInstruction, (targetInstruction.Offset + targetInstruction.GetSize))
            ProcessedInstructions.Insert(index, New ProcessedIL(newInstruction, True))
        End Sub

        Public Sub InsertBefore(targetInstruction As Instruction, newInstruction As Instruction)
            Dim index% = GetIndex(targetInstruction)
            Me._MethodBody.Instructions.Insert(index, newInstruction)
            Me.UpdateExceptionHandlers(newInstruction, targetInstruction.Offset)
            Me.ProcessedInstructions.Insert(index, New ProcessedIL(newInstruction, True))
        End Sub

        Private Function OptimizeOpCode(opcode As OpCode) As OpCode
            If (opcode.OperandType = OperandType.InlineBrTarget) Then
                Dim name As String = (opcode.Name.ToLower & "_s")
                Dim opCodeByName As OpCode? = Me.GetOpCodeByName(name)
                If opCodeByName.HasValue Then
                    Return opCodeByName.Value
                End If
            End If
            Return opcode
        End Function

        Public Sub Replace(targetInstruction As Instruction, newInstruction As Instruction)
            Dim index% = GetIndex(targetInstruction)
            UpdateExceptionHandlers(newInstruction, targetInstruction.Offset)
            ProcessedInstructions.RemoveAt(index)
            ProcessedInstructions.Insert(index, New ProcessedIL(newInstruction, targetInstruction.Offset))
            _MethodBody.Instructions.RemoveAt(index)
            _MethodBody.Instructions.Insert(index, newInstruction)
        End Sub

        Private Function SimplifyOpCode(opcode As OpCode) As OpCode
            If (opcode.OperandType = OperandType.ShortInlineBrTarget) Then
                Dim name = opcode.Name.Remove((opcode.Name.Length - 2)).ToLower
                Dim opCodeByName As OpCode? = Me.GetOpCodeByName(name)
                If opCodeByName.HasValue Then
                    Return opCodeByName.Value
                End If
            End If
            Return opcode
        End Function

        Public Shared Sub CalculateStackUsage(ByVal inst As Instruction, <Out> ByRef pushes As Integer, <Out> ByRef pops As Integer)
            Dim hasReturnValue As Boolean = False
            Dim opCode As OpCode = inst.OpCode
            If (opCode.FlowControl = FlowControl.Call) Then
                If (opCode.Code <> Code.Jmp) Then
                    pushes = 0
                    pops = 0
                    Dim methodSig As IMethodSignature
                    Dim operand As Object = inst.Operand
                    Dim method As MethodReference = TryCast(operand, MethodReference)

                    If (Not method Is Nothing) Then
                        methodSig = method
                    Else
                        methodSig = TryCast(operand, IMethodSignature)
                    End If
                    If (Not methodSig Is Nothing) Then
                        Dim implicitThis As Boolean = (methodSig.HasThis AndAlso Not methodSig.ExplicitThis)
                        If (methodSig.ReturnType.MetadataType <> MetadataType.Void OrElse ((opCode.Code = Code.Newobj) AndAlso methodSig.HasThis)) Then
                            pushes += 1
                        End If
                        pops = (pops + methodSig.Parameters.Count)

                        Dim paramsAfterSentinel As New List(Of TypeReference)
                        For Each p In methodSig.Parameters
                            If p.ParameterType.IsSentinel Then
                                paramsAfterSentinel.Add(p.ParameterType)
                            End If
                        Next
                        If (Not paramsAfterSentinel Is Nothing) Then
                            pops = (pops + paramsAfterSentinel.Count)
                        End If
                        If (implicitThis AndAlso (opCode.Code <> Code.Newobj)) Then
                            pops += 1
                        End If
                        If (opCode.Code = Code.Calli) Then
                            pops += 1
                        End If
                    End If
                End If
            Else

                pushes = 0
                pops = 0
                Select Case opCode.StackBehaviourPush
                    Case StackBehaviour.Push1, StackBehaviour.Pushi, StackBehaviour.Pushi8, StackBehaviour.Pushr4, StackBehaviour.Pushr8, StackBehaviour.Pushref
                        pushes += 1
                        Exit Select
                    Case StackBehaviour.Push1_push1
                        pushes = (pushes + 2)
                        Exit Select
                End Select
                Select Case opCode.StackBehaviourPop
                    Case StackBehaviour.Pop0, StackBehaviour.Push0, StackBehaviour.Push1, StackBehaviour.Push1_push1, StackBehaviour.Pushi, StackBehaviour.Pushi8, StackBehaviour.Pushr4, StackBehaviour.Pushr8, StackBehaviour.Pushref, StackBehaviour.Varpush
                        Exit Select
                    Case StackBehaviour.Pop1, StackBehaviour.Popi, StackBehaviour.Popref
                        pops += 1
                        Return
                    Case StackBehaviour.Pop1_pop1, StackBehaviour.Popi_pop1, StackBehaviour.Popi_popi, StackBehaviour.Popi_popi8, StackBehaviour.Popi_popr4, StackBehaviour.Popi_popr8, StackBehaviour.Popref_pop1, StackBehaviour.Popref_popi
                        pops = (pops + 2)
                        Return
                    Case StackBehaviour.Popi_popi_popi, StackBehaviour.Popref_popi_popi, StackBehaviour.Popref_popi_popi8, StackBehaviour.Popref_popi_popr4, StackBehaviour.Popref_popi_popr8, StackBehaviour.Popref_popi_popref
                        pops = (pops + 3)
                        Return
                    Case StackBehaviour.Varpop
                        If hasReturnValue Then
                            pops += 1
                        End If
                        Exit Select
                    Case StackBehaviour.PopAll
                        pops = -1
                        Return
                    Case Else
                        Return
                End Select
            End If
        End Sub

        Private Sub UpdateExceptionHandlers(instruction As Instruction, offset As Integer)
            For Each handler As ExceptionHandler In _MethodBody.ExceptionHandlers
                If (handler.TryStart.Offset = offset) Then
                    handler.TryStart = instruction
                End If
                If (handler.TryEnd.Offset = offset) Then
                    handler.TryEnd = instruction
                End If
                If (handler.HandlerStart.Offset = offset) Then
                    handler.HandlerStart = instruction
                End If
                If (handler.HandlerEnd.Offset = offset) Then
                    handler.HandlerEnd = instruction
                End If
                If ((Not handler.FilterStart Is Nothing) AndAlso (handler.FilterStart.Offset = offset)) Then
                    handler.FilterStart = instruction
                End If
            Next
        End Sub

        Public Shared Sub Iterate(Of T)(ByVal list As IEnumerable(Of T), ByVal handler As EnumerableIterateDelegate(Of T))
            Dim index As Integer = 0
            Dim local As T
            For Each local In list
                If Not handler.Invoke(index, local) Then
                    Exit For
                End If
                index += 1
            Next
        End Sub

        Public Shared Sub Iterate(Of T)(ByVal list As IList(Of T), ByVal handler As ListIterateDelegate(Of T))
            Iterate(Of T)(list, 0, -1, False, handler)
        End Sub

        Public Shared Sub IterateAll(Of T)(ByVal list As IList(Of T), ByVal handler As ListIterateAllDelegate(Of T))
            Iterate(Of T)(list, 0, -1, False, Function(ByVal list2 As IList(Of T), ByVal index As Integer, ByVal value As T)
                                                  handler.Invoke(list2, index, value)
                                                  Return True
                                              End Function)
        End Sub

        Public Shared Sub Iterate(Of T)(ByVal list As IList(Of T), ByVal startIndex As Integer, ByVal endIndex As Integer, ByVal reverseOrder As Boolean, ByVal handler As ListIterateDelegate(Of T))
            If reverseOrder Then
                Dim i As Integer = (If((endIndex < 0), list.Count, endIndex) - 1)
                Do While (i >= startIndex)
                    If Not handler.Invoke(list, i, list.Item(i)) Then
                        Return
                    End If
                    i -= 1
                Loop
            Else
                Dim j As Integer
                For j = startIndex To If((endIndex < 0), list.Count, endIndex) - 1
                    If Not handler.Invoke(list, j, list.Item(j)) Then
                        Return
                    End If
                Next j
            End If
        End Sub

        Public Shared Sub OptimizeMacros(instructions As IList(Of Instruction))
            For Each InSt In instructions
                Dim arg As ParameterDefinition
                Dim local As VariableDefinition
                Select Case InSt.OpCode.Code
                    Case Code.Ldarg, Code.Ldarg_S
                        arg = TryCast(InSt.Operand, ParameterDefinition)
                        If arg Is Nothing Then
                            Exit Select
                        End If
                        If arg.Index = 0 Then
                            InSt.OpCode = OpCodes.Ldarg_0
                            InSt.Operand = Nothing
                        ElseIf arg.Index = 1 Then
                            InSt.OpCode = OpCodes.Ldarg_1
                            InSt.Operand = Nothing
                        ElseIf arg.Index = 2 Then
                            InSt.OpCode = OpCodes.Ldarg_2
                            InSt.Operand = Nothing
                        ElseIf arg.Index = 3 Then
                            InSt.OpCode = OpCodes.Ldarg_3
                            InSt.Operand = Nothing
                        ElseIf Byte.MinValue <= arg.Index AndAlso arg.Index <= Byte.MaxValue Then
                            InSt.OpCode = OpCodes.Ldarg_S
                        End If
                        Exit Select
                    Case Code.Ldarga
                        arg = TryCast(InSt.Operand, ParameterDefinition)
                        If arg Is Nothing Then
                            Exit Select
                        End If
                        If Byte.MinValue <= arg.Index AndAlso arg.Index <= Byte.MaxValue Then
                            InSt.OpCode = OpCodes.Ldarga_S
                        End If
                        Exit Select
                    Case Code.Ldc_I4, Code.Ldc_I4_S
                        Dim i4 As Integer
                        If TypeOf InSt.Operand Is Integer Then
                            i4 = CInt(InSt.Operand)
                        ElseIf TypeOf InSt.Operand Is SByte Then
                            i4 = CSByte(InSt.Operand)
                        Else
                            Exit Select
                        End If
                        Select Case i4
                            Case 0
                                InSt.OpCode = OpCodes.Ldc_I4_0
                                InSt.Operand = Nothing
                                Exit Select
                            Case 1
                                InSt.OpCode = OpCodes.Ldc_I4_1
                                InSt.Operand = Nothing
                                Exit Select
                            Case 2
                                InSt.OpCode = OpCodes.Ldc_I4_2
                                InSt.Operand = Nothing
                                Exit Select
                            Case 3
                                InSt.OpCode = OpCodes.Ldc_I4_3
                                InSt.Operand = Nothing
                                Exit Select
                            Case 4
                                InSt.OpCode = OpCodes.Ldc_I4_4
                                InSt.Operand = Nothing
                                Exit Select
                            Case 5
                                InSt.OpCode = OpCodes.Ldc_I4_5
                                InSt.Operand = Nothing
                                Exit Select
                            Case 6
                                InSt.OpCode = OpCodes.Ldc_I4_6
                                InSt.Operand = Nothing
                                Exit Select
                            Case 7
                                InSt.OpCode = OpCodes.Ldc_I4_7
                                InSt.Operand = Nothing
                                Exit Select
                            Case 8
                                InSt.OpCode = OpCodes.Ldc_I4_8
                                InSt.Operand = Nothing
                                Exit Select
                            Case -1
                                InSt.OpCode = OpCodes.Ldc_I4_M1
                                InSt.Operand = Nothing
                                Exit Select
                            Case Else
                                If SByte.MinValue <= i4 AndAlso i4 <= SByte.MaxValue Then
                                    InSt.OpCode = OpCodes.Ldc_I4_S
                                    InSt.Operand = CSByte(i4)
                                End If
                                Exit Select
                        End Select
                        Exit Select
                    Case Code.Ldloc, Code.Ldloc_S
                        local = TryCast(InSt.Operand, VariableDefinition)
                        If local Is Nothing Then
                            Exit Select
                        End If
                        If local.Index = 0 Then
                            InSt.OpCode = OpCodes.Ldloc_0
                            InSt.Operand = Nothing
                        ElseIf local.Index = 1 Then
                            InSt.OpCode = OpCodes.Ldloc_1
                            InSt.Operand = Nothing
                        ElseIf local.Index = 2 Then
                            InSt.OpCode = OpCodes.Ldloc_2
                            InSt.Operand = Nothing
                        ElseIf local.Index = 3 Then
                            InSt.OpCode = OpCodes.Ldloc_3
                            InSt.Operand = Nothing
                        ElseIf Byte.MinValue <= local.Index AndAlso local.Index <= Byte.MaxValue Then
                            InSt.OpCode = OpCodes.Ldloc_S
                        End If
                        Exit Select
                    Case Code.Ldloca
                        local = TryCast(InSt.Operand, VariableDefinition)
                        If local Is Nothing Then
                            Exit Select
                        End If
                        If Byte.MinValue <= local.Index AndAlso local.Index <= Byte.MaxValue Then
                            InSt.OpCode = OpCodes.Ldloca_S
                        End If
                        Exit Select
                    Case Code.Starg
                        arg = TryCast(InSt.Operand, ParameterDefinition)
                        If arg Is Nothing Then
                            Exit Select
                        End If
                        If Byte.MinValue <= arg.Index AndAlso arg.Index <= Byte.MaxValue Then
                            InSt.OpCode = OpCodes.Starg_S
                        End If
                        Exit Select
                    Case Code.Stloc, Code.Stloc_S
                        local = TryCast(InSt.Operand, VariableDefinition)
                        If local Is Nothing Then
                            Exit Select
                        End If
                        If local.Index = 0 Then
                            InSt.OpCode = OpCodes.Stloc_0
                            InSt.Operand = Nothing
                        ElseIf local.Index = 1 Then
                            InSt.OpCode = OpCodes.Stloc_1
                            InSt.Operand = Nothing
                        ElseIf local.Index = 2 Then
                            InSt.OpCode = OpCodes.Stloc_2
                            InSt.Operand = Nothing
                        ElseIf local.Index = 3 Then
                            InSt.OpCode = OpCodes.Stloc_3
                            InSt.Operand = Nothing
                        ElseIf Byte.MinValue <= local.Index AndAlso local.Index <= Byte.MaxValue Then
                            InSt.OpCode = OpCodes.Stloc_S
                        End If
                        Exit Select
                End Select
            Next

            OptimizeBranches(instructions)
        End Sub

        Public Shared Sub OptimizeBranches(instructions As IList(Of Instruction))
            While True
                UpdateInstructionOffsets(instructions)

                Dim modified As Boolean = False
                For Each InSt In instructions
                    Dim shortOpCode As OpCode
                    Select Case InSt.OpCode.Code
                        Case Code.Beq
                            shortOpCode = OpCodes.Beq_S
                            Exit Select
                        Case Code.Bge
                            shortOpCode = OpCodes.Bge_S
                            Exit Select
                        Case Code.Bge_Un
                            shortOpCode = OpCodes.Bge_Un_S
                            Exit Select
                        Case Code.Bgt
                            shortOpCode = OpCodes.Bgt_S
                            Exit Select
                        Case Code.Bgt_Un
                            shortOpCode = OpCodes.Bgt_Un_S
                            Exit Select
                        Case Code.Ble
                            shortOpCode = OpCodes.Ble_S
                            Exit Select
                        Case Code.Ble_Un
                            shortOpCode = OpCodes.Ble_Un_S
                            Exit Select
                        Case Code.Blt
                            shortOpCode = OpCodes.Blt_S
                            Exit Select
                        Case Code.Blt_Un
                            shortOpCode = OpCodes.Blt_Un_S
                            Exit Select
                        Case Code.Bne_Un
                            shortOpCode = OpCodes.Bne_Un_S
                            Exit Select
                        Case Code.Br
                            shortOpCode = OpCodes.Br_S
                            Exit Select
                        Case Code.Brfalse
                            shortOpCode = OpCodes.Brfalse_S
                            Exit Select
                        Case Code.Brtrue
                            shortOpCode = OpCodes.Brtrue_S
                            Exit Select
                        Case Code.Leave
                            shortOpCode = OpCodes.Leave_S
                            Exit Select
                        Case Else
                            Continue For
                    End Select
                    Dim targetInstr = TryCast(InSt.Operand, Instruction)
                    If targetInstr Is Nothing Then
                        Continue For
                    End If

                    Dim afterShortInstr As Integer
                    If targetInstr.Offset >= InSt.Offset Then
                        afterShortInstr = CInt(InSt.Offset) + InSt.GetSize()
                    Else
                        Const operandSize As Integer = 1
                        afterShortInstr = CInt(InSt.Offset) + shortOpCode.Size + operandSize
                    End If

                    Dim displ As Integer = CInt(targetInstr.Offset) - afterShortInstr
                    If SByte.MinValue <= displ AndAlso displ <= SByte.MaxValue Then
                        InSt.OpCode = shortOpCode
                        modified = True
                    End If
                Next
                If Not modified Then
                    Exit While
                End If
            End While
        End Sub

        Public Shared Function UpdateInstructionOffsets(ByVal instructions As IList(Of Instruction)) As UInteger
            Dim offset As UInteger = 0
            IterateAll(instructions, Sub(ByVal list As IList(Of Instruction), ByVal index As Integer, ByVal instr As Instruction)
                                         instr.Offset = offset
                                         offset = (offset + CType(instr.GetSize, UInteger))
                                     End Sub)
            Return offset
        End Function


        Public Shared Sub SimplifyBranches(ByVal instructions As IList(Of Instruction))
            IterateAll(instructions, Sub(ByVal list As IList(Of Instruction), ByVal index As Integer, ByVal instr As Instruction)
                                         Select Case instr.OpCode.Code
                                             Case Code.Br_S
                                                 instr.OpCode = OpCodes.Br
                                                 Exit Select
                                             Case Code.Brfalse_S
                                                 instr.OpCode = OpCodes.Brfalse
                                                 Exit Select
                                             Case Code.Brtrue_S
                                                 instr.OpCode = OpCodes.Brtrue
                                                 Exit Select
                                             Case Code.Beq_S
                                                 instr.OpCode = OpCodes.Beq
                                                 Exit Select
                                             Case Code.Bge_S
                                                 instr.OpCode = OpCodes.Bge
                                                 Exit Select
                                             Case Code.Bgt_S
                                                 instr.OpCode = OpCodes.Bgt
                                                 Exit Select
                                             Case Code.Ble_S
                                                 instr.OpCode = OpCodes.Ble
                                                 Exit Select
                                             Case Code.Blt_S
                                                 instr.OpCode = OpCodes.Blt
                                                 Exit Select
                                             Case Code.Bne_Un_S
                                                 instr.OpCode = OpCodes.Bne_Un
                                                 Exit Select
                                             Case Code.Bge_Un_S
                                                 instr.OpCode = OpCodes.Bge_Un
                                                 Exit Select
                                             Case Code.Bgt_Un_S
                                                 instr.OpCode = OpCodes.Bgt_Un
                                                 Exit Select
                                             Case Code.Ble_Un_S
                                                 instr.OpCode = OpCodes.Ble_Un
                                                 Exit Select
                                             Case Code.Blt_Un_S
                                                 instr.OpCode = OpCodes.Blt_Un
                                                 Exit Select
                                             Case Code.Leave_S
                                                 instr.OpCode = OpCodes.Leave
                                                 Exit Select
                                         End Select
                                     End Sub)
        End Sub

#End Region

#Region "IDisposable Support"
        Private disposedValue As Boolean

        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not Me.disposedValue Then
                If disposing Then
                    ' TODO: supprimez l'état managé (objets managés).
                End If
                If ProcessedInstructions.Count <> 0 Then ProcessedInstructions.Clear()
                _MethodBody = Nothing
            End If
            Me.disposedValue = True
        End Sub

        Public Sub Dispose() Implements IDisposable.Dispose
            Dispose(True)
            GC.SuppressFinalize(Me)
        End Sub
#End Region

    End Class

End Namespace


