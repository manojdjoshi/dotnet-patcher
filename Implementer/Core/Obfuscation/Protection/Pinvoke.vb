Imports Mono.Cecil
Imports Mono.Cecil.Cil
Imports Mono.Cecil.Rocks
Imports Helper.CecilHelper
Imports Helper.RandomizeHelper
Imports Implementer.Engine.Processing
Imports Implementer.Core.Obfuscation.Builder
Imports Implementer.Core.Obfuscation.Exclusion
Imports System.IO
Imports Helper.UtilsHelper

Namespace Core.Obfuscation.Protection
    Public NotInheritable Class Pinvoke
        Inherits Source

#Region " Fields "
        Private Shared LoaderInvoke As Stub
        Private Shared PinvokeCreate As PinvokeModifier
        Private Shared ExportedDll As List(Of NativeDllFunction)
#End Region

#Region " Constructor "
        Shared Sub New()
            PinvokeCreate = New PinvokeModifier
            ExportedDll = New List(Of NativeDllFunction)
        End Sub
#End Region

#Region " Methods "

        Friend Shared Sub DoJob(asm As AssemblyDefinition, Framework$, Exclude As ExcludeList, Optional ByVal packIt As Boolean = False)
            AssemblyDef = asm
            Frmwk = Framework
            Pack = packIt

            Dim Types As New List(Of TypeDefinition)
            Dim HasPinvokeCalls As Boolean
            For Each mo As ModuleDefinition In asm.Modules
                For Each t In mo.GetAllTypes()
                    If t.Methods.Any(Function(f) f.IsPInvokeImpl) Then
                        HasPinvokeCalls = True
                    End If
                    Types.Add(t)
                Next
            Next

            If HasPinvokeCalls Then
                LoaderInvoke = New Stub(Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic,
                                        Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic)
                With LoaderInvoke
                    .ResolveTypeFromFile(DynamicInvokeStub(.className, .funcName1, .funcName2, .funcName3), Finder.FindDefaultNamespace(asm, Pack), Randomizer.GenerateNew, Randomizer.GenerateNew, Randomizer.GenerateNew, Randomizer.GenerateNew)
                    .InjectType(asm)
                    completedMethods.Add(.GetMethod1)
                    completedMethods.Add(.GetMethod2)
                    completedMethods.Add(.GetMethod3)
                End With

                PinvokeCreate.AddModuleRef(asm.MainModule)

                If Not File.Exists(Functions.GetTempFolder & "\dllexp.exe") Then
                    File.WriteAllBytes(Functions.GetTempFolder & "\dllexp.exe", My.Resources.dllexp)
                End If

                For Each type As TypeDefinition In Types
                    If Exclude.isHideCallsExclude(type) = False Then
                        IterateType(type)
                    End If
                Next

                LoaderInvoke.DeleteDll()
                PinvokeCreate.Dispose()
            End If

            Types.Clear()
            ExportedDll.Clear()
        End Sub

        Private Shared Sub IterateType(td As TypeDefinition)
            Dim publicMethods As New List(Of MethodDefinition)()
            publicMethods.AddRange(From m In td.Methods Where (m.HasBody AndAlso m.Body.Instructions.Count > 2 AndAlso Not completedMethods.Contains(m) AndAlso Utils.HasUnsafeInstructions(m) = False))

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
            Dim instructionsToExpand As List(Of Instruction) = New List(Of Instruction)()

            For Each instruction As Instruction In instructions
                Select Case instruction.OpCode
                    Case OpCodes.Call
                        If isValidPinvokeCallOperand(instruction) Then
                            instructionsToExpand.Add(instruction)
                        End If
                End Select
            Next

            For Each instruction As Instruction In instructionsToExpand
                Try
                    Dim originalReference As MethodReference = TryCast(instruction.Operand, MethodReference)
                    Dim originalMethod As MethodDefinition = originalReference.Resolve

                    If originalMethod.PInvokeInfo.EntryPoint.StartsWith("#") Then
                        originalMethod = Renamer.RenameMethod(originalMethod.DeclaringType, originalMethod)
                        completedMethods.Add(originalMethod)
                    Else
                        Dim functionName As String = If(originalMethod.PInvokeInfo.EntryPoint = String.Empty, originalMethod.Name, originalMethod.PInvokeInfo.EntryPoint)
                        Dim dllName As String = originalMethod.PInvokeInfo.Module.Name

                        Dim is86 = If(AssemblyDef.MainModule.Architecture = TargetArchitecture.I386, True, False)
                        Dim dllSourcePath = If(is86, System.Environment.GetFolderPath(System.Environment.SpecialFolder.System), System.Environment.SpecialFolder.SystemX86)

                        If Not File.Exists(originalMethod.PInvokeInfo.Module.Name) Then
                            Dim dllNameEndswithDll As Boolean = dllName.ToLower.EndsWith(".dll")
                            Dim dllEntireNew As String = If(dllNameEndswithDll, Path.Combine(dllSourcePath, dllName), Path.Combine(dllSourcePath, dllName) & ".dll")

                            If File.Exists(dllEntireNew) Then
                                Dim TmpPath = Path.GetTempFileName & ".txt"

                                If Not ExportedDll.Any(Function(f) f.FileName = dllEntireNew) Then
                                    Shell(Functions.GetTempFolder & "\dllexp.exe /from_files " & Chr(34) & dllEntireNew & Chr(34) & " /scomma " & Chr(34) & TmpPath & Chr(34), AppWinStyle.Hide, True)
                                    Dim lines = File.ReadAllLines(TmpPath)

                                    For Each line In lines
                                        Dim dllexp As New NativeDllFunction
                                        Dim lineSplitted = line.Split(",")

                                        dllexp.FunctionName = lineSplitted(0)
                                        dllexp.FileName = dllEntireNew
                                        dllexp.ExportedFunction = If(lineSplitted(6) = "Exported Function", True, False)

                                        ExportedDll.Add(dllexp)

                                        If ReadyToGeneratePinvoke(originalMethod, dllexp, functionName) Then
                                            PinvokeCreate.InitPinvokeInfos(originalMethod, body.Method.DeclaringType)
                                            PinvokeCreate.CreatePinvokeBody(LoaderInvoke)
                                            completedMethods.Add(originalMethod)
                                        Else
                                            Continue For
                                        End If
                                    Next
                                Else
                                    Dim vals = ExportedDll.Where(Function(f) f.FileName = dllEntireNew)
                                    For Each dllexp In vals

                                        If ReadyToGeneratePinvoke(originalMethod, dllexp, functionName) Then
                                            PinvokeCreate.InitPinvokeInfos(originalMethod, body.Method.DeclaringType)
                                            PinvokeCreate.CreatePinvokeBody(LoaderInvoke)
                                            completedMethods.Add(originalMethod)
                                        Else
                                            Continue For
                                        End If
                                    Next
                                End If
                            Else
                                Continue For
                            End If
                        Else
                            Continue For
                        End If
                    End If
                Catch ex As Exception
                    Continue For
                End Try
            Next
        End Sub

        Private Shared Function ReadyToGeneratePinvoke(originalMethod As MethodDefinition, dllexp As NativeDllFunction, functionName As String) As Boolean
            If dllexp.ExportedFunction Then
                If dllexp.FunctionName.ToLower = functionName.ToLower Then
                    Return True
                Else
                    If (dllexp.FunctionName.ToUpper.EndsWith("A") OrElse dllexp.FunctionName.ToUpper.EndsWith("W")) AndAlso dllexp.FunctionName.Substring(0, dllexp.FunctionName.Length - 1).ToLower = functionName.ToLower AndAlso dllexp.ExportedFunction Then
                        If originalMethod.PInvokeInfo.IsCharSetAnsi Then
                            originalMethod.Name = functionName & "A"
                            originalMethod.PInvokeInfo.EntryPoint = functionName & "A"
                            Return True
                        ElseIf originalMethod.PInvokeInfo.IsCharSetUnicode Then
                            originalMethod.Name = functionName & "W"
                            originalMethod.PInvokeInfo.EntryPoint = functionName & "W"
                            Return True
                        ElseIf originalMethod.PInvokeInfo.IsCharSetAuto Then
                            Return False
                        ElseIf originalMethod.PInvokeInfo.IsCharSetNotSpec Then
                            originalMethod.Name = functionName & "A"
                            originalMethod.PInvokeInfo.EntryPoint = functionName & "A"
                            Return True
                        End If
                    End If
                End If
            Else
                Return False
            End If
            Return False
        End Function

#End Region

    End Class

End Namespace

