Imports Mono.Cecil
Imports Mono.Cecil.Rocks
Imports Mono.Cecil.Cil
Imports Helper.RandomizeHelper
Imports Helper.CecilHelper
Imports Implementer.Core.Obfuscation.Builder
Imports Implementer.Core.Obfuscation.Exclusion

Namespace Core.Obfuscation.Protection
    Public NotInheritable Class Mild
        Inherits Source

#Region " Fields "
        Private Shared MdByString As Dictionary(Of String, MethodDefinition)
        Private Shared MdByInteger As Dictionary(Of Integer, MethodDefinition)
        Private Shared MdByLong As Dictionary(Of Long, MethodDefinition)
        Private Shared MdByDouble As Dictionary(Of Double, MethodDefinition)
        Private Shared MdBySingle As Dictionary(Of Single, MethodDefinition)
        Private Shared MdByByte As Dictionary(Of Byte, MethodDefinition)
        Private Shared MdByRef As Dictionary(Of MethodReference, MethodDefinition)
        Private Shared Types As List(Of TypeDefinition)
        Private Shared OpCodeDic As Dictionary(Of OpCode, String)
#End Region

#Region " Constructor "
        Shared Sub New()
            MdByString = New Dictionary(Of String, MethodDefinition)
            MdByInteger = New Dictionary(Of Integer, MethodDefinition)
            MdByLong = New Dictionary(Of Long, MethodDefinition)
            MdByDouble = New Dictionary(Of Double, MethodDefinition)
            MdBySingle = New Dictionary(Of Single, MethodDefinition)
            MdByByte = New Dictionary(Of Byte, MethodDefinition)
            MdByRef = New Dictionary(Of MethodReference, MethodDefinition)
            Types = New List(Of TypeDefinition)
            OpCodeDic = New Dictionary(Of OpCode, String) From {{OpCodes.Ldc_I4, "System.Int32"}, {OpCodes.Ldc_R4, "System.Single"}, {OpCodes.Ldc_R8, "System.Double"}}
        End Sub
#End Region


#Region " Methods "
        Friend Shared Function DoJob(asm As AssemblyDefinition, Framework$, Exclude As ExcludeList, Optional ByVal packIt As Boolean = False) As AssemblyDefinition
            AssemblyDef = asm
            Frmwk = Framework
            Pack = packIt

            For Each m As ModuleDefinition In asm.Modules
                Types.AddRange(m.GetAllTypes())
                For Each type As TypeDefinition In Types
                    If Exclude.isHideCallsExclude(type) = False Then
                        IterateType(type)
                    End If
                Next
                Types.Clear()
            Next

            MethodByClear()

            Return asm
        End Function

        Private Shared Sub MethodByClear()
            MdByString.Clear()
            MdByInteger.Clear()
            MdByLong.Clear()
            MdByDouble.Clear()
            MdBySingle.Clear()
            MdByByte.Clear()
            MdByRef.Clear()
        End Sub

        Private Shared Sub IterateType(td As TypeDefinition)
            Dim publicMethods As New List(Of MethodDefinition)()
            publicMethods.AddRange(From m In td.Methods Where (m.HasBody AndAlso m.Body.Instructions.Count >= 2 AndAlso Not completedMethods.Contains(m) AndAlso Not m.DeclaringType.BaseType Is Nothing AndAlso Not m.DeclaringType.BaseType.Name = "ApplicationSettingsBase" AndAlso Not m.DeclaringType.BaseType.Name = "WindowsFormsApplicationBase" AndAlso Not Finder.HasCustomAttributeByName(m.DeclaringType, "EditorBrowsableAttribute") AndAlso Utils.HasUnsafeInstructions(m) = False))
            Try
                For Each md In publicMethods
                    md.Body.SimplifyMacros
                    ProcessInstructions(md.Body)
                    md.Body.OptimizeMacros
                    md.Body.ComputeHeader()
                    md.Body.ComputeOffsets()
                Next
            Catch ex As Exception
                MsgBox(ex.ToString)
            End Try
            publicMethods.Clear()
        End Sub

        Private Shared Sub ProcessInstructions(body As MethodBody)
            Dim instructions = body.Instructions
            Dim il = body.GetILProcessor()
            Dim instructionsToExpand As List(Of Instruction) = New List(Of Instruction)()

            For Each instruction As Instruction In instructions
                Select Case instruction.OpCode
                    Case OpCodes.Ldc_I4
                        If isValidIntegerOperand(instruction) AndAlso Not Randomizer.invisibleChars.Contains(CInt(instruction.Operand)) Then
                            OpCodesFilter(instruction.OpCode, instruction, instructionsToExpand, body)
                        End If
                    Case OpCodes.Ldc_I8
                        If isValidLongOperand(instruction) Then
                            instructionsToExpand.Add(instruction)
                        End If
                    Case OpCodes.Ldc_R4
                        If isValidSingleOperand(instruction) Then
                            OpCodesFilter(instruction.OpCode, instruction, instructionsToExpand, body)
                        End If
                    Case OpCodes.Ldc_R8
                        If isValidDoubleOperand(instruction) Then
                            OpCodesFilter(instruction.OpCode, instruction, instructionsToExpand, body)
                        End If
                    Case OpCodes.Ldstr
                        If isValidStringOperand(instruction) Then
                            instructionsToExpand.Add(instruction)
                        End If
                    Case OpCodes.Newobj
                        If isValidNewObjOperand(instruction) Then
                            instructionsToExpand.Add(instruction)
                        End If
                End Select
            Next

            For Each instruction As Instruction In instructionsToExpand
                Select Case instruction.OpCode
                    Case OpCodes.Ldc_I4
                        Dim Value As Integer = CInt(instruction.Operand)
                        Dim mdFinal As MethodDefinition = Nothing
                        If MdByInteger.ContainsKey(Value) Then
                            mdFinal = MdByInteger.Item(Value)
                            mdFinal.Attributes = MethodAttributes.Static Or MethodAttributes.Public
                        Else
                            mdFinal = CreateMethod(Value, body.Method)
                            If Not mdFinal Is Nothing Then
                                mdFinal.Attributes = MethodAttributes.Static Or MethodAttributes.Private
                                MdByInteger.Add(Value, mdFinal)
                            End If
                        End If
                        If (Not mdFinal Is Nothing) Then
                            Dim Instruct = il.Create(OpCodes.Call, AssemblyDef.MainModule.Import(mdFinal))
                            il.Replace(instruction, Instruct)
                            completedMethods.Add(mdFinal)
                        End If
                    Case OpCodes.Ldc_I8
                        Dim Value As Long = CLng(instruction.Operand)
                        Dim mdFinal As MethodDefinition = Nothing
                        If MdByLong.ContainsKey(Value) Then
                            mdFinal = MdByLong.Item(Value)
                            mdFinal.Attributes = MethodAttributes.Static Or MethodAttributes.Public
                        Else
                            mdFinal = CreateMethod(Value, body.Method)
                            If Not mdFinal Is Nothing Then
                                mdFinal.Attributes = MethodAttributes.Static Or MethodAttributes.Private
                                MdByLong.Add(Value, mdFinal)
                            End If
                        End If
                        If (Not mdFinal Is Nothing) Then
                            Dim Instruct = il.Create(OpCodes.Call, AssemblyDef.MainModule.Import(mdFinal))
                            il.Replace(instruction, Instruct)
                            completedMethods.Add(mdFinal)
                        End If
                    Case OpCodes.Ldc_R4
                        Dim Value As Single = CSng(instruction.Operand)
                        Dim mdFinal As MethodDefinition = Nothing
                        If MdBySingle.ContainsKey(Value) Then
                            mdFinal = MdBySingle.Item(Value)
                            mdFinal.Attributes = MethodAttributes.Static Or MethodAttributes.Public
                        Else
                            mdFinal = CreateMethod(Value, body.Method)
                            If Not mdFinal Is Nothing Then
                                mdFinal.Attributes = MethodAttributes.Static Or MethodAttributes.Private
                                MdBySingle.Add(Value, mdFinal)
                            End If
                        End If
                        If (Not mdFinal Is Nothing) Then
                            Dim Instruct = il.Create(OpCodes.Call, AssemblyDef.MainModule.Import(mdFinal))
                            il.Replace(instruction, Instruct)
                            completedMethods.Add(mdFinal)
                        End If
                    Case OpCodes.Ldc_R8
                        Dim Value As Double = CDbl(instruction.Operand)
                        Dim mdFinal As MethodDefinition = Nothing
                        If MdByDouble.ContainsKey(Value) Then
                            mdFinal = MdByDouble.Item(Value)
                            mdFinal.Attributes = MethodAttributes.Static Or MethodAttributes.Public
                        Else
                            mdFinal = CreateMethod(Value, body.Method)
                            If Not mdFinal Is Nothing Then
                                mdFinal.Attributes = MethodAttributes.Static Or MethodAttributes.Private
                                MdByDouble.Add(Value, mdFinal)
                            End If
                        End If
                        If (Not mdFinal Is Nothing) Then
                            Dim Instruct = il.Create(OpCodes.Call, AssemblyDef.MainModule.Import(mdFinal))
                            il.Replace(instruction, Instruct)
                            completedMethods.Add(mdFinal)
                        End If
                    Case OpCodes.Ldstr
                        Dim Value As String = CStr(instruction.Operand)
                        Dim mdFinal As MethodDefinition = Nothing
                        If MdByString.ContainsKey(Value) Then
                            mdFinal = MdByString.Item(Value)
                            mdFinal.Attributes = MethodAttributes.Static Or MethodAttributes.Public
                        Else
                            mdFinal = CreateMethod(Value, body.Method)
                            If Not mdFinal Is Nothing Then
                                mdFinal.Attributes = MethodAttributes.Static Or MethodAttributes.Private
                                MdByString.Add(Value, mdFinal)
                            End If
                        End If
                        If (Not mdFinal Is Nothing) Then
                            Dim Instruct = il.Create(OpCodes.Call, AssemblyDef.MainModule.Import(mdFinal))
                            il.Replace(instruction, Instruct)
                            completedMethods.Add(mdFinal)
                        End If
                    Case OpCodes.Newobj
                        Dim Mref = DirectCast(instruction.Operand, MethodReference)
                        Dim mdFinal As MethodDefinition = Nothing

                        If MdByRef.ContainsKey(mRef) Then
                            mdFinal = MdByRef.Item(mRef)
                        Else
                            mdFinal = CreateReferenceMethod(Mref, body.Method)
                            If Not mdFinal Is Nothing Then
                                MdByRef.Add(mRef, mdFinal)
                            End If
                        End If
                        If (Not mdFinal Is Nothing) Then
                            Dim Instruct = il.Create(OpCodes.Call, AssemblyDef.MainModule.Import(mdFinal))
                            il.Replace(instruction, Instruct)
                            completedMethods.Add(mdFinal)
                        End If
                End Select
            Next
        End Sub

        Private Shared Sub OpCodesFilter(opcode As OpCode, Instruction As Instruction, instructionsToExpand As List(Of Instruction), body As MethodBody)
            Dim opCodeStr = OpCodeDic.Item(opcode)

            Dim instructNext = Instruction.Next
            If instructNext.OpCode = OpCodes.Stloc OrElse instructNext.OpCode = OpCodes.Ldloc Then
                Dim varIndex = CInt(instructNext.Operand.ToString.ToLower.Replace("v_", String.Empty))
                Dim varType = body.Variables(varIndex).VariableType
                If varType.ToString = opCodeStr Then
                    If Not Instruction.Operand Is Nothing Then
                        instructionsToExpand.Add(Instruction)
                    End If
                End If
            Else
                If Not instructNext.Operand Is Nothing Then
                    If instructNext.Operand.ToString.ToLower.EndsWith(opCodeStr.ToLower & ")") OrElse instructNext.Operand.ToString.ToLower.StartsWith(opCodeStr.ToLower) Then
                        If Not Instruction.Operand Is Nothing Then
                            instructionsToExpand.Add(Instruction)
                        End If
                    End If
                End If
            End If
        End Sub

        Private Shared Function CreateReferenceMethod(targetConstructor As MethodReference, md As MethodDefinition) As MethodDefinition
            If (targetConstructor.Parameters.Count <> 0) OrElse targetConstructor.DeclaringType.IsGenericInstance OrElse targetConstructor.HasGenericParameters Then
                Return Nothing
            End If
            Dim item As New MethodDefinition(Randomizer.GenerateNew, (MethodAttributes.CompilerControlled Or (MethodAttributes.FamANDAssem Or (MethodAttributes.Family Or MethodAttributes.Static))), AssemblyDef.MainModule.Import(targetConstructor.DeclaringType))
            item.Body = New MethodBody(item)
            item.IsPublic = True
            Dim ilProc As ILProcessor = item.Body.GetILProcessor()
            With ilProc
                .Body.MaxStackSize = 1
                .Body.InitLocals = True
                .Emit(OpCodes.Newobj, targetConstructor)
                .Emit(OpCodes.Ret)
            End With

            md.DeclaringType.Methods.Add(item)

            Return item
        End Function

        Private Shared Function CreateMethod(value As Object, md As MethodDefinition) As MethodDefinition
            Dim opc As OpCode = Nothing
            Select Case value.GetType
                Case GetType(String)
                    opc = OpCodes.Ldstr
                Case GetType(Integer)
                    opc = OpCodes.Ldc_I4
                Case GetType(Long)
                    opc = OpCodes.Ldc_I8
                Case GetType(Single)
                    opc = OpCodes.Ldc_R4
                Case GetType(Double)
                    opc = OpCodes.Ldc_R8
                Case Else
                    Return Nothing
            End Select
            Dim item As New MethodDefinition(Randomizer.GenerateNew, (MethodAttributes.CompilerControlled Or (MethodAttributes.FamANDAssem Or (MethodAttributes.Family Or MethodAttributes.Static))), AssemblyDef.MainModule.Import(value.GetType))
            item.Body = New MethodBody(item)
            item.IsPublic = True
            Dim ilProc As ILProcessor = item.Body.GetILProcessor()
            With ilProc
                .Body.MaxStackSize = 1
                .Body.InitLocals = True
                .Emit(opc, value)
                .Emit(OpCodes.Ret)
            End With

            md.DeclaringType.Methods.Add(item)

            Return item
        End Function

#End Region

    End Class

End Namespace
