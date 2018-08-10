Imports Mono.Cecil
Imports Mono.Cecil.Cil
Imports Mono.Cecil.Rocks
Imports Helper.CecilHelper
Imports Implementer.Core.Obfuscation.Exclusion

Namespace Core.Obfuscation.Protection
    Public NotInheritable Class Constants

#Region " Fields "
        Private Shared Rand As Random
        Private Shared Types As New List(Of TypeDefinition)
#End Region

#Region " Constructor "
        Shared Sub New()
            Rand = New Random
        End Sub
#End Region

#Region " Methods "
        Friend Shared Sub DoJob(AssDef As AssemblyDefinition, Exclude As ExcludeList)
            For Each m As ModuleDefinition In AssDef.Modules
                Types.AddRange(m.GetAllTypes())
                For Each type As TypeDefinition In Types
                    If Exclude.isIntegerEncodExclude(type) = False Then
                        IterateType(type)
                    End If
                Next
                Types.Clear()
            Next
        End Sub

        Private Shared Sub IterateType(td As TypeDefinition)
            Dim publicMethods As New List(Of MethodDefinition)()
            publicMethods.AddRange(From m In td.Methods Where (m.HasBody AndAlso Not Finder.HasCustomAttributeByName(m.DeclaringType, "EditorBrowsableAttribute") AndAlso Utils.HasUnsafeInstructions(m) = False))
            Try
                For Each md In publicMethods
                    md.Body.SimplifyMacros
                    ProcessInstructions(md.Body)
                    md.Body.OptimizeMacros
                    md.Body.ComputeHeader()
                    md.Body.ComputeOffsets()
                Next
            Catch ex As Exception
            End Try
            publicMethods.Clear()
        End Sub

        Private Shared Sub ProcessInstructions(body As MethodBody)
            Dim instructions = body.Instructions
            Dim il = body.GetILProcessor()
            Dim instructionsToExpand As List(Of Instruction) = New List(Of Instruction)()

            For Each instruction As Instruction In instructions
                If instruction.OpCode = OpCodes.Ldc_I4 Then
                    If Not instruction.Operand Is Nothing AndAlso CInt(instruction.Operand) < Integer.MaxValue Then
                        instructionsToExpand.Add(instruction)
                    End If
                End If
            Next

            Dim tRef As TypeReference = Nothing

            For Each instruction As Instruction In instructionsToExpand
                Dim value As Integer = CInt(instruction.Operand)
                Dim num As Integer = 0
                Select Case Rand.Next(1, 8)
                    Case 1
                        tRef = body.Method.DeclaringType.Module.Assembly.MainModule.Import(GetType(Integer))
                        num = 4
                        Exit Select
                    Case 2
                        tRef = body.Method.DeclaringType.Module.Assembly.MainModule.Import(GetType(SByte))
                        num = 1
                        Exit Select
                    Case 3
                        tRef = body.Method.DeclaringType.Module.Assembly.MainModule.Import(GetType(Byte))
                        num = 1
                        Exit Select
                    Case 4
                        tRef = body.Method.DeclaringType.Module.Assembly.MainModule.Import(GetType(Boolean))
                        num = 1
                        Exit Select
                    Case 5
                        tRef = body.Method.DeclaringType.Module.Assembly.MainModule.Import(GetType(Decimal))
                        num = 16
                        Exit Select
                    Case 6
                        tRef = body.Method.DeclaringType.Module.Assembly.MainModule.Import(GetType(Short))
                        num = 2
                        Exit Select
                    Case 7
                        tRef = body.Method.DeclaringType.Module.Assembly.MainModule.Import(GetType(Long))
                        num = 8
                        Exit Select
                End Select

                Try
                    Dim nmr% = Rand.Next(1, 1000)
                    Dim flag As Boolean = Convert.ToBoolean(Rand.Next(0, 2))
                    Select Case Rand.Next(1, 4)
                        Case 1
                            Dim newOp = ((value - num) + If(flag, -nmr, nmr))
                            Dim instruct = il.Create(OpCodes.Ldc_I4, newOp)
                            il.Replace(instruction, instruct)
                            Dim SizeOFInstruct = Instruction.Create(OpCodes.Sizeof, tRef)
                            il.InsertAfter(instruct, SizeOFInstruct)
                            Dim AddInstruct = Instruction.Create(OpCodes.Add)
                            il.InsertAfter(SizeOFInstruct, AddInstruct)
                            Dim Ldci4Instruct = Instruction.Create(OpCodes.Ldc_I4, nmr)
                            il.InsertAfter(AddInstruct, Ldci4Instruct)
                            Dim AddSubInstruct = Instruction.Create(If(flag, OpCodes.Add, OpCodes.Sub))
                            il.InsertAfter(Ldci4Instruct, AddSubInstruct)
                            Exit Select
                        Case 2
                            Dim newOp = ((value + num) + If(flag, -nmr, nmr))
                            Dim instruct = il.Create(OpCodes.Ldc_I4, newOp)
                            il.Replace(instruction, instruct)
                            Dim SizeOFInstruct = Instruction.Create(OpCodes.Sizeof, tRef)
                            il.InsertAfter(instruct, SizeOFInstruct)
                            Dim AddInstruct = Instruction.Create(OpCodes.Sub)
                            il.InsertAfter(SizeOFInstruct, AddInstruct)
                            Dim Ldci4Instruct = Instruction.Create(OpCodes.Ldc_I4, nmr)
                            il.InsertAfter(AddInstruct, Ldci4Instruct)
                            Dim AddSubInstruct = Instruction.Create(If(flag, OpCodes.Add, OpCodes.Sub))
                            il.InsertAfter(Ldci4Instruct, AddSubInstruct)
                            Exit Select
                        Case 3
                            Dim newOp = ((value + If(flag, -nmr, nmr)) * num)
                            Dim instruct = il.Create(OpCodes.Ldc_I4, newOp)
                            il.Replace(instruction, instruct)
                            Dim SizeOFInstruct = Instruction.Create(OpCodes.Sizeof, tRef)
                            il.InsertAfter(instruct, SizeOFInstruct)
                            Dim AddInstruct = Instruction.Create(OpCodes.Div)
                            il.InsertAfter(SizeOFInstruct, AddInstruct)
                            Dim Ldci4Instruct = Instruction.Create(OpCodes.Ldc_I4, nmr)
                            il.InsertAfter(AddInstruct, Ldci4Instruct)
                            Dim AddSubInstruct = Instruction.Create(If(flag, OpCodes.Add, OpCodes.Sub))
                            il.InsertAfter(Ldci4Instruct, AddSubInstruct)
                            Exit Select
                    End Select
                Catch ex As Exception
                    Continue For
                End Try
            Next
        End Sub
#End Region



    End Class
End Namespace
