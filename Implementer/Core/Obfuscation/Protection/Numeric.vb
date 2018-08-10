Imports Mono.Cecil
Imports Mono.Cecil.Rocks
Imports Mono.Cecil.Cil
Imports Helper.RandomizeHelper
Imports Helper.CecilHelper
Imports System.Text.RegularExpressions
Imports Helper.CryptoHelper
Imports System.Resources
Imports Implementer.Core.Obfuscation.Builder
Imports Implementer.Core.Obfuscation.Exclusion

Namespace Core.Obfuscation.Protection
    Public NotInheritable Class Numeric
        Inherits Source

#Region " Fields "
        Private Shared DecryptReadResources As Stub
        Private Shared DecryptInt As Stub
        Private Shared DecryptRPN As Stub
        Private Shared MethodByInteger As New Dictionary(Of Integer, MethodDefinition)
        Private Shared MethodByDouble As New Dictionary(Of Double, MethodDefinition)
        Private Shared MethodBySingle As New Dictionary(Of Single, MethodDefinition)
        Private Shared Types As New List(Of TypeDefinition)()
        Private Shared OpCodeDic As New Dictionary(Of OpCode, String) From {{OpCodes.Ldc_I4, "System.Int32"}, {OpCodes.Ldc_R4, "System.Single"}, {OpCodes.Ldc_R8, "System.Double"}}
#End Region

#Region " Methods "
        Friend Shared Function DoJob(asm As AssemblyDefinition, Framework$, encryptToRes As EncryptType, Exclude As ExcludeList, Optional ByVal packIt As Boolean = False) As AssemblyDefinition
            AssemblyDef = asm
            Frmwk = Framework
            Pack = packIt
            EncryptToResources = encryptToRes

            If encryptToRes = EncryptType.ToResources Then
                ResName = Randomizer.GenerateNew
                ResWriter = New ResourceWriter(ResName & ".resources")
                DecryptReadResources = New Stub(Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic)
                With DecryptReadResources
                    .ResolveTypeFromFile(ReadFromResourcesStub(.className, .funcName1), Finder.FindDefaultNamespace(asm, Pack), Randomizer.GenerateNew, Randomizer.GenerateNew)
                    .InjectType(asm)
                    completedMethods.Add(.GetMethod1)
                    completedMethods.Add(.GetMethod2)
                    completedMethods.Add(.GetMethod3)
                End With
            End If

            DecryptInt = New Stub(Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic)
            With DecryptInt
                .ResolveTypeFromFile(DecryptIntStub(.className, .funcName1), Finder.FindDefaultNamespace(asm, Pack), Randomizer.GenerateNew, Randomizer.GenerateNew)
                .InjectType(asm)
                completedMethods.Add(.GetMethod1)
            End With

            DecryptRPN = New Stub(Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic)
            With DecryptRPN
                .ResolveTypeFromFile(DecryptRPNStub(.className, .funcName1, .funcName2), Finder.FindDefaultNamespace(asm, Pack), Randomizer.GenerateNew, Randomizer.GenerateNew, Randomizer.GenerateNew)
                .InjectType(asm)
                completedMethods.Add(.GetMethod1)
                completedMethods.Add(.GetMethod2)
            End With

            For Each m As ModuleDefinition In asm.Modules
                Types.AddRange(m.GetAllTypes())
                For Each type As TypeDefinition In Types
                    If Exclude.isIntegerEncodExclude(type) = False Then
                        IterateType(type)
                    End If
                Next
                Types.Clear()
            Next

            If encryptToRes = EncryptType.ToResources Then
                If Not ResWriter Is Nothing Then ResWriter.Close()
                InjectResource()
            End If

            MethodByClear()
            DeleteStubs()
            CleanUp()

            Return asm
        End Function

        Private Shared Sub IterateType(td As TypeDefinition)
            Dim publicMethods As New List(Of MethodDefinition)()
            publicMethods.AddRange(From m In td.Methods Where (m.HasBody AndAlso m.Body.Instructions.Count >= 2 AndAlso Not completedMethods.Contains(m) AndAlso Not Finder.HasCustomAttributeByName(m.DeclaringType, "EditorBrowsableAttribute") AndAlso Utils.HasUnsafeInstructions(m) = False))
            Try
                For Each pm In publicMethods
                    pm.Body.SimplifyMacros
                    ProcessInstructions(pm.Body)
                    pm.Body.OptimizeMacros
                    pm.Body.ComputeHeader()
                    pm.Body.ComputeOffsets()
                Next
            Catch ex As Exception
                MsgBox("Numeric encoding Error : " & vbNewLine & ex.ToString)
            End Try
            publicMethods.Clear()
        End Sub


        Private Shared Sub ProcessInstructions(body As MethodBody)
            Dim instructions = body.Instructions
            Dim il = body.GetILProcessor()
            Dim instructionsToExpand As List(Of Instruction) = New List(Of Instruction)()

            For Each instruction As Instruction In instructions
                Select Case instruction.OpCode
                    Case OpCodes.Ldc_I4, OpCodes.Ldc_R4, OpCodes.Ldc_R8
                        OpCodesFilter(instruction.OpCode, instruction, instructionsToExpand, body)
                End Select
            Next

            For Each instruction As Instruction In instructionsToExpand
                Select Case instruction.OpCode
                    Case OpCodes.Ldc_I4
                        Dim mdFinal As MethodDefinition = Nothing
                        If Not MethodByInteger.TryGetValue(CInt(instruction.Operand), mdFinal) Then
                            If Randomizer.GenerateBoolean Then
                                If instruction.Operand < Integer.MaxValue AndAlso instruction.Operand > 9 Then

                                    mdFinal = New MethodDefinition(Randomizer.GenerateNew, (MethodAttributes.CompilerControlled Or (MethodAttributes.FamANDAssem Or (MethodAttributes.Family Or MethodAttributes.Static))), body.Method.DeclaringType.Module.Import(GetType(Integer)))
                                    mdFinal.Body = New MethodBody(mdFinal)
                                    mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(Integer))))

                                    If EncryptToResources = EncryptType.ToResources Then
                                        mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(String))))
                                        mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(String))))
                                        mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(Integer))))

                                        Dim integ = Randomizer.GenerateInvisible
                                        Dim encStr = Generator.IntEncrypt(CInt(instruction.Operand), integ)
                                        Dim dataKeyName = Randomizer.GenerateNew
                                        ResWriter.AddResource(dataKeyName, encStr)

                                        Dim ilProc = mdFinal.Body.GetILProcessor()
                                        With ilProc
                                            .Body.MaxStackSize = 4
                                            .Body.InitLocals = True

                                            .Emit(OpCodes.Ldstr, dataKeyName)
                                            .Emit(OpCodes.Stloc_1)
                                            .Emit(OpCodes.Ldloc_1)
                                            .Emit(OpCodes.Call, body.Method.Module.Import(DecryptReadResources.GetMethod1))
                                            .Emit(OpCodes.Stloc_2)
                                            .Emit(OpCodes.Ldloc_2)
                                            .Emit(OpCodes.Ldc_I4, integ)
                                            .Emit(OpCodes.Stloc_3)
                                            .Emit(OpCodes.Ldloc_3)
                                            .Emit(OpCodes.Call, AssemblyDef.MainModule.Import(DecryptInt.GetMethod1))
                                            .Emit(OpCodes.Stloc_0)
                                            .Emit(OpCodes.Ldloc_0)
                                            .Emit(OpCodes.Ret)
                                        End With

                                        body.Method.DeclaringType.Methods.Add(mdFinal)
                                        MethodByInteger.Add(CInt(instruction.Operand), mdFinal)
                                    Else
                                        Dim integ = Randomizer.GenerateInvisible
                                        Dim encStr = Generator.IntEncrypt(CInt(instruction.Operand), integ)

                                        mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(String))))
                                        mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(Integer))))

                                        Dim ilProc = mdFinal.Body.GetILProcessor()
                                        With ilProc
                                            .Body.MaxStackSize = 4
                                            .Body.InitLocals = True
                                            .Emit(OpCodes.Ldstr, encStr)
                                            .Emit(OpCodes.Stloc_1)
                                            .Emit(OpCodes.Ldloc_1)
                                            .Emit(OpCodes.Ldc_I4, integ)
                                            .Emit(OpCodes.Stloc_2)
                                            .Emit(OpCodes.Ldloc_2)
                                            .Emit(OpCodes.Call, AssemblyDef.MainModule.Import(DecryptInt.GetMethod1))
                                            .Emit(OpCodes.Stloc_0)
                                            .Emit(OpCodes.Ldloc_0)
                                            .Emit(OpCodes.Ret)
                                        End With

                                        body.Method.DeclaringType.Methods.Add(mdFinal)
                                        MethodByInteger.Add(CInt(instruction.Operand), mdFinal)
                                    End If
                                ElseIf CInt(instruction.Operand) >= 1 AndAlso CInt(instruction.Operand) < 10 Then
                                    mdFinal = New MethodDefinition(Randomizer.GenerateNew, (MethodAttributes.CompilerControlled Or (MethodAttributes.FamANDAssem Or (MethodAttributes.Family Or MethodAttributes.Static))), body.Method.DeclaringType.Module.Import(GetType(Integer)))
                                    mdFinal.Body = New MethodBody(mdFinal)
                                    mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(Integer))))

                                    Dim integ = Randomizer.GenerateInvisible
                                    Dim encStr = Generator.IntEncrypt(CInt(instruction.Operand), integ)

                                    Dim ilProc = mdFinal.Body.GetILProcessor()
                                    With ilProc
                                        .Body.MaxStackSize = 4
                                        .Body.InitLocals = True

                                        mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(String))))
                                        mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(Integer))))

                                        .Emit(OpCodes.Ldstr, encStr)
                                        .Emit(OpCodes.Stloc_1)
                                        .Emit(OpCodes.Ldloc_1)
                                        .Emit(OpCodes.Ldc_I4, integ)
                                        .Emit(OpCodes.Stloc_2)
                                        .Emit(OpCodes.Ldloc_2)
                                        .Emit(OpCodes.Call, AssemblyDef.MainModule.Import(DecryptInt.GetMethod1))
                                        .Emit(OpCodes.Stloc_0)
                                        .Emit(OpCodes.Ldloc_0)
                                        .Emit(OpCodes.Ret)
                                    End With

                                    body.Method.DeclaringType.Methods.Add(mdFinal)
                                    MethodByInteger.Add(CInt(instruction.Operand), mdFinal)
                                End If
                            Else
                                If instruction.Operand < Integer.MaxValue AndAlso instruction.Operand >= 2 Then
                                    Dim resultPrimes = PrimeFactors(instruction.Operand)
                                    Dim countPrimes = resultPrimes.Count
                                    If countPrimes >= 2 Then
                                        Dim num = CInt(instruction.Operand)
                                        Dim divider0 = 0
                                        Dim resultdivider0 = DetermineDiv(num, divider0)
                                        Dim StrDivider0 = resultdivider0 & " / " & divider0
                                        Dim divider1 = 0
                                        Dim resultdivider1 = DetermineDiv(num, divider1)
                                        Dim StrDivider1 = resultdivider1 & " / " & divider1
                                        Dim StrDivider = StrDivider0 & " - " & StrDivider1 & " + "

                                        Dim strPrimes = String.Empty
                                        strPrimes = String.Join(" ", resultPrimes).TrimEnd(" ")
                                        For k% = 0 To countPrimes - 2
                                            strPrimes &= " *"
                                        Next

                                        Dim InFix = (StrDivider & strPrimes).TrimEnd(" ")
                                        Dim postfix = String.Empty
                                        Dim bResult = InfixToPostfixConvert(InFix, postfix)
                                        postfix = postfix.TrimEnd(" ").Replace(" ", ",")

                                        mdFinal = New MethodDefinition(Randomizer.GenerateNew, (MethodAttributes.CompilerControlled Or (MethodAttributes.FamANDAssem Or (MethodAttributes.Family Or MethodAttributes.Static))), AssemblyDef.MainModule.Import(GetType(Integer)))
                                        mdFinal.Body = New MethodBody(mdFinal)
                                        mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(Integer))))

                                        mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(String))))
                                        mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(String()))))

                                        Dim ilProc = mdFinal.Body.GetILProcessor()
                                        With ilProc
                                            .Body.MaxStackSize = 4
                                            .Body.InitLocals = True
                                            .Emit(OpCodes.Ldstr, postfix)
                                            .Emit(OpCodes.Stloc_1)
                                            .Emit(OpCodes.Ldloc_1)
                                            .Emit(OpCodes.Call, AssemblyDef.MainModule.Import(DecryptRPN.GetMethod2))
                                            .Emit(OpCodes.Stloc_2)
                                            .Emit(OpCodes.Ldloc_2)
                                            .Emit(OpCodes.Call, AssemblyDef.MainModule.Import(DecryptRPN.GetMethod1))
                                            .Emit(OpCodes.Stloc_0)
                                            .Emit(OpCodes.Ldloc_0)
                                            .Emit(OpCodes.Ret)
                                        End With

                                        body.Method.DeclaringType.Methods.Add(mdFinal)
                                        MethodByInteger.Add(CInt(instruction.Operand), mdFinal)
                                    Else
                                        Dim divider0 = 0
                                        Dim resultdivider0 = DetermineDiv(CInt(instruction.Operand), divider0)
                                        Dim str = resultdivider0 & "," & divider0 & ",/"

                                        mdFinal = New MethodDefinition(Randomizer.GenerateNew, (MethodAttributes.CompilerControlled Or (MethodAttributes.FamANDAssem Or (MethodAttributes.Family Or MethodAttributes.Static))), AssemblyDef.MainModule.Import(GetType(Integer)))
                                        mdFinal.Body = New MethodBody(mdFinal)
                                        mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(Integer))))
                                        mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(String))))
                                        mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(String()))))

                                        Dim ilProc = mdFinal.Body.GetILProcessor()
                                        With ilProc
                                            .Body.MaxStackSize = 4
                                            .Body.InitLocals = True

                                            .Emit(OpCodes.Ldstr, str)
                                            .Emit(OpCodes.Stloc_1)
                                            .Emit(OpCodes.Ldloc_1)
                                            .Emit(OpCodes.Call, AssemblyDef.MainModule.Import(DecryptRPN.GetMethod2))
                                            .Emit(OpCodes.Stloc_2)
                                            .Emit(OpCodes.Ldloc_2)
                                            .Emit(OpCodes.Call, AssemblyDef.MainModule.Import(DecryptRPN.GetMethod1))
                                            .Emit(OpCodes.Stloc_0)
                                            .Emit(OpCodes.Ldloc_0)
                                            .Emit(OpCodes.Ret)
                                        End With

                                        body.Method.DeclaringType.Methods.Add(mdFinal)
                                        MethodByInteger.Add(CInt(instruction.Operand), mdFinal)
                                    End If
                                End If
                            End If
                        Else
                            mdFinal = MethodByInteger.Item(CInt(instruction.Operand))
                        End If
                        If (Not mdFinal Is Nothing) Then
                            If mdFinal.DeclaringType.IsNotPublic Then
                                mdFinal.DeclaringType.IsPublic = True
                            End If
                            Dim Instruct = il.Create(OpCodes.Call, AssemblyDef.MainModule.Import(mdFinal))
                            il.Replace(instruction, Instruct)
                            completedMethods.Add(mdFinal)
                        End If
                    Case OpCodes.Ldc_R4
                        If isValidOperand(instruction) AndAlso CSng(instruction.Operand) >= 0 Then
                            Dim mdFinal As MethodDefinition = Nothing
                            If Not MethodBySingle.TryGetValue(CSng(instruction.Operand), mdFinal) Then
                                Dim Sng As Single
                                If Single.TryParse(instruction.Operand, Sng) Then
                                    Dim pdefName = Randomizer.GenerateNew

                                    Dim pdef As New PropertyDefinition(pdefName, PropertyAttributes.None, AssemblyDef.MainModule.Import(GetType(Single)))
                                    body.Method.DeclaringType.Properties.Add(pdef)

                                    mdFinal = New MethodDefinition(("get_" & pdef.Name), MethodAttributes.Static Or MethodAttributes.Public, pdef.PropertyType)
                                    mdFinal.Body = New MethodBody(mdFinal)
                                    mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(Single))))

                                    pdef.GetMethod = mdFinal
                                    pdef.DeclaringType.Methods.Add(mdFinal)

                                    If Not pdef.DeclaringType.IsInterface Then
                                        Dim iLProcessor = mdFinal.Body.GetILProcessor
                                        With iLProcessor
                                            .Body.MaxStackSize = 1
                                            .Body.InitLocals = True

                                            .Emit(OpCodes.Ldc_R4, Sng)
                                            .Emit(OpCodes.Ret)
                                        End With
                                    Else
                                        mdFinal.IsAbstract = True
                                        mdFinal.IsVirtual = True
                                        mdFinal.IsNewSlot = True
                                    End If
                                    mdFinal.IsSpecialName = True
                                    mdFinal.IsGetter = True

                                    MethodBySingle.Add(CSng(instruction.Operand), mdFinal)
                                End If
                            Else
                                mdFinal = MethodBySingle.Item(CSng(instruction.Operand))
                            End If
                            If (Not mdFinal Is Nothing) Then
                                Dim Instruct = il.Create(OpCodes.Call, AssemblyDef.MainModule.Import(mdFinal))
                                il.Replace(instruction, Instruct)
                                completedMethods.Add(mdFinal)
                            End If
                        End If
                    Case OpCodes.Ldc_R8
                        If isValidOperand(instruction) AndAlso CDbl(instruction.Operand) >= 0 Then
                            Dim mdFinal As MethodDefinition = Nothing

                            If Not MethodByDouble.TryGetValue(CDbl(instruction.Operand), mdFinal) Then
                                Dim integ As Double
                                If Double.TryParse(instruction.Operand, integ) Then
                                    Dim pdefName = Randomizer.GenerateNew

                                    Dim pdef As New PropertyDefinition(pdefName, PropertyAttributes.None, AssemblyDef.MainModule.Import(GetType(Double)))
                                    body.Method.DeclaringType.Properties.Add(pdef)

                                    mdFinal = New MethodDefinition(("get_" & pdef.Name), MethodAttributes.Static Or MethodAttributes.Public, pdef.PropertyType)
                                    mdFinal.Body = New MethodBody(mdFinal)
                                    mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(Double))))

                                    pdef.GetMethod = mdFinal
                                    pdef.DeclaringType.Methods.Add(mdFinal)

                                    If Not pdef.DeclaringType.IsInterface Then
                                        Dim iLProcessor = mdFinal.Body.GetILProcessor
                                        With iLProcessor
                                            .Body.MaxStackSize = 1
                                            .Body.InitLocals = True
                                            .Emit(OpCodes.Ldc_R8, integ)
                                            .Emit(OpCodes.Ret)
                                        End With
                                    Else
                                        mdFinal.IsAbstract = True
                                        mdFinal.IsVirtual = True
                                        mdFinal.IsNewSlot = True
                                    End If
                                    mdFinal.IsSpecialName = True
                                    mdFinal.IsGetter = True

                                    MethodByDouble.Add(CDbl(instruction.Operand), mdFinal)
                                End If
                            Else
                                mdFinal = MethodByDouble.Item(CDbl(instruction.Operand))
                            End If
                            If (Not mdFinal Is Nothing) Then
                                Dim Instruct = il.Create(OpCodes.Call, AssemblyDef.MainModule.Import(mdFinal))
                                il.Replace(instruction, Instruct)
                                completedMethods.Add(mdFinal)
                            End If
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
                    If Not Instruction.Operand = 0 Then
                        instructionsToExpand.Add(Instruction)
                    End If
                End If
            Else
                If Not instructNext.Operand Is Nothing Then
                    If instructNext.Operand.ToString.ToLower.EndsWith(opCodeStr.ToLower & ")") OrElse instructNext.Operand.ToString.ToLower.StartsWith(opCodeStr.ToLower) Then
                        If Not Instruction.Operand = 0 Then
                            instructionsToExpand.Add(Instruction)
                        End If
                    End If
                End If
            End If
        End Sub

        Private Shared Function PrimeFactors(a As Integer) As List(Of Integer)
            Dim retval As New List(Of Integer)
            Dim b As Integer = 2
            While a > 1
                While a Mod b = 0
                    a /= b
                    retval.Add(b)
                End While
                b += 1
            End While
            Return retval
        End Function

        Private Shared Function DetermineDiv(real As Integer, ByRef div As Integer) As Integer
            Dim num As Integer = rand.Next(5, 40)
            div = num
            Dim v% = real
            Try
                v = (real * num)
            Catch ex As OverflowException
                div = 1
            End Try
            Return v
        End Function

        Private Shared Function InfixToPostfixConvert(ByRef infixBuffer As String, ByRef postfixBuffer As String) As Boolean
            Dim prior% = 0
            postfixBuffer = ""

            Dim s1 As New Stack(Of Char)

            For i% = 0 To infixBuffer.Length - 1
                Dim item As Char = infixBuffer.Chars(i)
                Select Case item
                    Case "+"c, "-"c, "*"c, "/"c
                        If (s1.Count <= 0) Then
                            s1.Push(item)
                        Else
                            If ((s1.Peek = "*"c) OrElse (s1.Peek = "/"c)) Then
                                prior = 1
                            Else
                                prior = 0
                            End If
                            If (prior = 1) Then
                                Select Case item
                                    Case "+"c, "-"c
                                        postfixBuffer = (postfixBuffer & CStr(s1.Pop))
                                        i -= 1
                                        Continue For
                                End Select
                                postfixBuffer = (postfixBuffer & CStr(s1.Pop))
                                i -= 1
                            Else
                                Select Case item
                                    Case "+"c, "-"c
                                        postfixBuffer = (postfixBuffer & CStr(s1.Pop))
                                        s1.Push(item)
                                        Continue For
                                End Select
                                s1.Push(item)
                            End If
                        End If
                        Exit Select
                    Case Else
                        postfixBuffer = (postfixBuffer & CStr(item))
                        Exit Select
                End Select
            Next

            Dim len% = s1.Count
            For j% = 0 To len - 1
                postfixBuffer = (postfixBuffer & CStr(s1.Pop))
            Next

            postfixBuffer = postfixBuffer.Replace("/", " / ").Replace("*", " * ").Replace("+", " + ").Replace("-", " - ")
            postfixBuffer = New Regex("[ ]{2,}", RegexOptions.None).Replace(postfixBuffer, " ")
            Return True
        End Function

        Private Shared Sub DeleteStubs()
            If Not DecryptReadResources Is Nothing Then DecryptReadResources.DeleteDll()
            If Not DecryptInt Is Nothing Then DecryptInt.DeleteDll()
            If Not DecryptRPN Is Nothing Then DecryptRPN.DeleteDll()
        End Sub

        Private Shared Sub MethodByClear()
            MethodByInteger.Clear()
            MethodByDouble.Clear()
            MethodBySingle.Clear()
        End Sub
#End Region

    End Class

End Namespace
