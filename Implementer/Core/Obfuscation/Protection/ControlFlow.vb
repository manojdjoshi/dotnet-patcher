Imports Mono.Cecil
Imports Mono.Cecil.Rocks
Imports Mono.Cecil.Cil
Imports Helper.CecilHelper
Imports Implementer.Core.Obfuscation.Exclusion
Imports Implementer.Core.Obfuscation.Builder
Imports Helper.RandomizeHelper

Namespace Core.Obfuscation.Protection
    ''' <summary>
    ''' By Furious from DotnetVitamin
    ''' </summary>
    Public NotInheritable Class ControlFlow
        Inherits Source

#Region " Fields "
        Private Shared m_Types As List(Of TypeDefinition)
        Private Shared DecryptCtrFlow As Stub
#End Region

#Region " Constructor "
        Shared Sub New()
            m_Types = New List(Of TypeDefinition)
        End Sub
#End Region

#Region " Methods "
        Friend Shared Function DoJob(asm As AssemblyDefinition, Exclude As ExcludeList, Framwk$, packer As Boolean) As AssemblyDefinition
            AssemblyDef = asm
            Frmwk = Framwk
            Pack = packer

            DecryptCtrFlow = New Stub(Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic)
            With DecryptCtrFlow
                .ResolveTypeFromFile(DecryptCtrFlowStub(.className, .funcName1), Finder.FindDefaultNamespace(asm, packer), Randomizer.GenerateNew, Randomizer.GenerateNew)
                .InjectType(asm, True)
                completedMethods.Add(.GetMethod1)
            End With

            For Each m As ModuleDefinition In asm.Modules
                m_Types.AddRange(m.GetAllTypes())
                For Each type As TypeDefinition In m_Types
                    If Exclude.isControlFlowExclude(type) = False Then
                        IterateType(type)
                    End If
                Next
                m_Types.Clear()
            Next

            If Not DecryptCtrFlow Is Nothing Then DecryptCtrFlow.DeleteDll()
            CleanUp()

            Return asm
        End Function

        Private Shared Sub IterateType(td As TypeDefinition)
            Dim publicMethods As New List(Of MethodDefinition)
            publicMethods.AddRange(From m In td.Methods Where (m.HasBody AndAlso m.Body.Instructions.Count > 0 AndAlso m.Body.ExceptionHandlers.Count = 0 AndAlso Not m.DeclaringType.BaseType Is Nothing AndAlso Utils.HasUnsafeInstructions(m) = False AndAlso Not completedMethods.Contains(m)))
            'publicMethods.AddRange(From m In td.Methods Where (m.HasBody AndAlso m.Body.Instructions.Count > 2 AndAlso m.Body.Variables.Count > 1 AndAlso Not m.DeclaringType.BaseType Is Nothing AndAlso Not m.DeclaringType.BaseType.Name = "ApplicationSettingsBase" AndAlso Not m.DeclaringType.BaseType.Name = "WindowsFormsApplicationBase" AndAlso Not Finder.HasCustomAttributeByName(m.DeclaringType, "EditorBrowsableAttribute") AndAlso Not m.IsConstructor AndAlso Utils.HasUnsafeInstructions(m) = False))

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

            Dim incGroups As New InstructionGroups
            Dim item1 As New InstructionGroup
            Dim incremId As Integer = 0
            Dim incremStackUsage As Integer = 0
            Dim flag As Boolean = False

            For Each instruction As Instruction In instructions
                Dim Instruct = instruction
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
                body.Instructions.Clear()
                Dim local As New VariableDefinition(body.Method.DeclaringType.Module.Assembly.MainModule.Import(GetType(Integer)))
                body.Variables.Add(local)
                Dim target As Instruction = Instruction.Create(OpCodes.Nop)
                Dim instruction3 As Instruction = Instruction.Create(OpCodes.Br, target)
                body.Instructions.Add(Instruction.Create(OpCodes.Ldc_I4_0))
                body.Instructions.Add(Instruction.Create(OpCodes.Stloc, local))
                body.Instructions.Add(Instruction.Create(OpCodes.Br, instruction3))
                body.Instructions.Add(target)
                Dim group4 As InstructionGroup
                For Each group4 In incGroups
                    If (Not group4 Is item3) Then
                        body.Instructions.Add(Instruction.Create(OpCodes.Ldloc, local))
                        body.Instructions.Add(Instruction.Create(OpCodes.Ldc_I4, group4.ID))
                        body.Instructions.Add(Instruction.Create(OpCodes.Ceq))
                        Dim instruction4 As Instruction = Instruction.Create(OpCodes.Nop)
                        body.Instructions.Add(Instruction.Create(OpCodes.Brfalse, instruction4))
                        Dim instruction5 As Instruction
                        For Each instruction5 In group4
                            body.Instructions.Add(instruction5)
                        Next
                        body.Instructions.Add(Instruction.Create(OpCodes.Ldc_I4, group4.nextGroup))
                        body.Instructions.Add(Instruction.Create(OpCodes.Stloc, local))
                        body.Instructions.Add(instruction4)
                    End If
                Next
                body.Instructions.Add(Instruction.Create(OpCodes.Ldloc, local))
                body.Instructions.Add(Instruction.Create(OpCodes.Ldc_I4, incGroups.Count - 1))
                body.Instructions.Add(Instruction.Create(OpCodes.Ceq))
                body.Instructions.Add(Instruction.Create(OpCodes.Brfalse, instruction3))
                body.Instructions.Add(Instruction.Create(OpCodes.Br, item3.Item(0)))
                body.Instructions.Add(instruction3)
                Dim instruction6 As Instruction
                For Each instruction6 In item3
                    body.Instructions.Add(instruction6)
                Next
            End If
        End Sub
#End Region

    End Class

End Namespace
