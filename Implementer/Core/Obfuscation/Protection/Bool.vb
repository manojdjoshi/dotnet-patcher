Imports Mono.Cecil
Imports Mono.Cecil.Rocks
Imports Mono.Cecil.Cil
Imports Helper.RandomizeHelper
Imports Helper.CecilHelper
Imports Helper.CryptoHelper
Imports System.Resources
Imports Implementer.Core.Obfuscation.Builder
Imports Implementer.Core.Obfuscation.Exclusion

Namespace Core.Obfuscation.Protection
    Public NotInheritable Class Bool
        Inherits Source

#Region " Fields "
        Private Shared DecryptReadResources As Stub
        Private Shared DecryptInt As Stub
        Private Shared DecryptOdd As Stub
        Private Shared DecryptPrime As Stub
        Private Shared MtdByInteger As New Dictionary(Of Integer, MethodDefinition)
        Private Shared Types As New List(Of TypeDefinition)()
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
                End With

                DecryptInt = New Stub(Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic)
                With DecryptInt
                    .ResolveTypeFromFile(DecryptIntStub(.className, .funcName1), Finder.FindDefaultNamespace(asm, Pack), Randomizer.GenerateNew, Randomizer.GenerateNew)
                    .InjectType(asm)
                    completedMethods.Add(.GetMethod1)
                End With
            End If

            DecryptOdd = New Stub(Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic)
            With DecryptOdd
                .ResolveTypeFromFile(DecryptOddStub(.className, .funcName1), Finder.FindDefaultNamespace(asm, Pack), Randomizer.GenerateNew, Randomizer.GenerateNew)
                .InjectType(asm)
                completedMethods.Add(.GetMethod1)
            End With

            DecryptPrime = New Stub(Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic)
            With DecryptPrime
                .ResolveTypeFromFile(DecryptPrimeStub(.className, .funcName1), Finder.FindDefaultNamespace(asm, Pack), Randomizer.GenerateNew, Randomizer.GenerateNew)
                .InjectType(asm)
                completedMethods.Add(.GetMethod1)
            End With

            For Each m As ModuleDefinition In asm.Modules
                Types.AddRange(m.GetAllTypes())
                For Each type As TypeDefinition In Types
                    If Exclude.isBooleanEncryptExclude(type) = False Then
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

        Private Shared Sub DeleteStubs()
            If Not DecryptReadResources Is Nothing Then DecryptReadResources.DeleteDll()
            If Not DecryptInt Is Nothing Then DecryptInt.DeleteDll()
            If Not DecryptOdd Is Nothing Then DecryptOdd.DeleteDll()
        End Sub

        Private Shared Sub MethodByClear()
            MtdByInteger.Clear()
        End Sub

        Private Shared Sub IterateType(td As TypeDefinition)
            Dim publicMethods As New List(Of MethodDefinition)()
            publicMethods.AddRange(From m In td.Methods Where (m.HasBody AndAlso m.Body.Instructions.Count >= 2 AndAlso Not completedMethods.Contains(m) AndAlso Not Finder.HasCustomAttributeByName(m.DeclaringType, "EditorBrowsableAttribute")))
            Try
                For Each md In publicMethods
                    md.Body.SimplifyMacros
                    ProcessInstructions(md.Body)
                    md.Body.OptimizeMacros
                    md.Body.ComputeOffsets()
                    md.Body.ComputeHeader()
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
                If instruction.OpCode = OpCodes.Ldc_I4 Then
                    If isValidBoolOperand(instruction) Then
                        Dim instructNext = instruction.Next
                        If isValidOperand(instructNext) Then
                            If instructNext.OpCode = OpCodes.Stloc Then
                                Dim varIndex = CInt(instructNext.Operand.ToString.ToLower.Replace("v_", String.Empty))
                                Dim varType = body.Variables(varIndex).VariableType
                                If varType.ToString = "System.Boolean" Then
                                    instructionsToExpand.Add(instruction)
                                End If
                            Else
                                If instructNext.Operand.ToString.ToLower.EndsWith("system.boolean)") OrElse instructNext.Operand.ToString.ToLower.StartsWith("system.boolean") Then
                                    instructionsToExpand.Add(instruction)
                                End If
                            End If
                        End If
                    End If
                End If
            Next

            For Each instruction As Instruction In instructionsToExpand
                Dim value% = CInt(instruction.Operand)
                Dim mdFinal As MethodDefinition = Nothing
                CreateMethod(mdFinal, value, body.Method)
                InjectMethodCall(mdFinal, instruction, il)
            Next
        End Sub

        Private Shared Sub InjectMethodCall(mdFinal As MethodDefinition, instruction As Instruction, ilProc As ILProcessor)
            If (Not mdFinal Is Nothing) Then
                If mdFinal.DeclaringType.IsNotPublic Then
                    mdFinal.DeclaringType.IsPublic = True
                End If
                Dim CallMethod = ilProc.Create(OpCodes.Call, AssemblyDef.MainModule.Import(mdFinal))
                ilProc.Replace(instruction, CallMethod)
                completedMethods.Add(mdFinal)
            End If
        End Sub


        Private Shared Sub CreateMethod(ByRef mDef As MethodDefinition, value%, ByRef md As MethodDefinition)
            If Randomizer.GenerateBoolean Then
                mDef = New MethodDefinition(Randomizer.GenerateNew, (MethodAttributes.CompilerControlled Or (MethodAttributes.FamANDAssem Or (MethodAttributes.Family Or MethodAttributes.Static))), AssemblyDef.MainModule.Import(GetType(Boolean)))
                mDef.Body = New MethodBody(mDef)

                If EncryptToResources = EncryptType.ToResources Then
                    Dim integ = Randomizer.GenerateInvisible

                    Dim encSt = Generator.IntEncrypt(testNumber(If(value = 0, False, True)), integ)
                    Dim dataKeyName = Randomizer.GenerateNew
                    ResWriter.AddResource(dataKeyName, encSt)

                    Dim ilProc As ILProcessor = mDef.Body.GetILProcessor()
                    With ilProc
                        .Body.MaxStackSize = 8
                        .Body.InitLocals = True

                        mDef.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(Boolean))))
                        .Emit(OpCodes.Ldstr, dataKeyName)
                        .Emit(OpCodes.Call, AssemblyDef.MainModule.Import(DecryptReadResources.GetMethod1))
                        .Emit(OpCodes.Ldc_I4, integ)
                        .Emit(OpCodes.Call, AssemblyDef.MainModule.Import(DecryptInt.GetMethod1))
                        .Emit(OpCodes.Call, AssemblyDef.MainModule.Import(DecryptOdd.GetMethod1))
                        .Emit(OpCodes.Stloc_0)
                        .Emit(OpCodes.Ldloc_0)
                        .Emit(OpCodes.Ret)
                    End With

                    md.DeclaringType.Methods.Add(mDef)
                Else
                    Dim encStr = testNumber(If(CInt(value) = 0, False, True))
                    Dim IlProc1 As ILProcessor = mDef.Body.GetILProcessor()
                    With IlProc1
                        .Body.MaxStackSize = 4
                        .Body.InitLocals = True
                        mDef.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(Boolean))))
                        .Emit(OpCodes.Ldc_I4, encStr)
                        .Emit(OpCodes.Call, AssemblyDef.MainModule.Import(DecryptOdd.GetMethod1))
                        .Emit(OpCodes.Stloc_0)
                        .Emit(OpCodes.Ldloc_0)
                        .Emit(OpCodes.Ret)
                    End With

                    md.DeclaringType.Methods.Add(mDef)
                End If
            Else
                Dim UnPrime = rand.Next(Generator.numberUnPrime.Length)
                Dim Prime = rand.Next(Generator.numberPrime.Length)
                Dim valFinale%

                If value = 0 Then
                    valFinale = Generator.numberUnPrime(UnPrime)
                ElseIf value = 1 Then
                    valFinale = Generator.numberPrime(Prime)
                End If

                mDef = New MethodDefinition(Randomizer.GenerateNew, (MethodAttributes.CompilerControlled Or (MethodAttributes.FamANDAssem Or (MethodAttributes.Family Or MethodAttributes.Static))), AssemblyDef.MainModule.Import(GetType(Boolean)))
                mDef.Body = New MethodBody(mDef)
                mDef.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(Boolean))))

                Dim IlProc1 As ILProcessor = mDef.Body.GetILProcessor()
                With IlProc1
                    .Body.MaxStackSize = 4
                    .Body.InitLocals = True

                    .Emit(OpCodes.Ldc_I4, valFinale)
                    .Emit(OpCodes.Call, AssemblyDef.MainModule.Import(DecryptPrime.GetMethod1))
                    .Emit(OpCodes.Stloc_0)
                    .Emit(OpCodes.Ldloc_0)
                    .Emit(OpCodes.Ret)
                End With

                md.DeclaringType.Methods.Add(mDef)
            End If
        End Sub

        Private Shared Function isOdd(num%) As Boolean
            Return num Mod 2 <> 0
        End Function

        Private Shared Function testNumber(isPair As Boolean) As Integer
            Dim n%
            Dim result As Boolean = False
            Do While result = False
                n = rand.Next(1000000, 99999999)
                result = If(isOdd(n) = isPair, True, False)
                If result Then
                    Exit Do
                End If
            Loop
            Return n
        End Function

#End Region

    End Class
End Namespace
