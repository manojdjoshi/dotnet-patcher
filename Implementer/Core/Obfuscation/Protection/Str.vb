Imports Mono.Cecil
Imports Mono.Cecil.Rocks
Imports Mono.Cecil.Cil
Imports Helper.RandomizeHelper
Imports Helper.CecilHelper
Imports System.IO
Imports System.IO.Compression
Imports Implementer.Core.Obfuscation.Builder
Imports Implementer.Core.Obfuscation.Exclusion
Imports System.Text
Imports System.Security.Cryptography
Imports Helper.CryptoHelper

Namespace Core.Obfuscation.Protection
    Public NotInheritable Class Str
        Inherits Source

#Region " Fields "
        Private Shared DecryptReadStringResources As Stub
        Private Shared DecryptXor As Stub
        Private Shared DecryptBase64 As Stub
        Private Shared DecryptPrime As Stub
        Private Shared DecryptString As Stub
        Private Shared DecryptStringKey As Byte
        Private Shared DecryptStringFieldHost As TypeDefinition
        Private Shared DecryptStringLength As Integer
        Private Shared m_s As MemoryStream = Nothing
        Private Shared m_bw As BinaryWriter = Nothing
        Private Shared randSalt As Random
        Private Shared Types As New List(Of TypeDefinition)
        Private Shared m_IsDefaultEncoding As Boolean
#End Region

#Region " Properties "
        Shared Property objTarget() As Object
        Shared Property XorEncryptType() As Type
#End Region

#Region " Constructor "
        Shared Sub New()
            randSalt = New Random
            DecryptStringKey = New Random().Next(1, 255)
        End Sub
#End Region

#Region " Methods "
        Friend Shared Function DoJob(asm As AssemblyDefinition, Framework$, encryptToRes As EncryptType, Exclude As ExcludeList, Optional ByVal packIt As Boolean = False) As AssemblyDefinition
            AssemblyDef = asm
            Frmwk = Framework
            Pack = packIt

            EncryptToResources = encryptToRes

            If encryptToRes = EncryptType.ToResources Then
                ResName = Randomizer.GenerateNew
                m_s = New MemoryStream
                m_bw = New BinaryWriter(m_s)

                DecryptReadStringResources = New Stub(Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic)
                With DecryptReadStringResources
                    .ResolveTypeFromFile(ReadStringFromResourcesStub(.className, .funcName1, .funcName2, .funcName3), Finder.FindDefaultNamespace(asm, Pack), Randomizer.GenerateNew, Randomizer.GenerateNew, Randomizer.GenerateNew, Randomizer.GenerateNew)
                    .InjectType(asm)
                    completedMethods.Add(.GetMethod1)
                End With
            End If

            DecryptXor = New Stub(Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic)
            With DecryptXor
                .ResolveTypeFromFile(DecryptXorStub(.className, .funcName1), Finder.FindDefaultNamespace(asm, Pack), Randomizer.GenerateNew)
                .InjectType(asm)
                completedMethods.Add(.GetMethod1)
                _XorEncryptType = GenerateEncryptXor()
            End With

            DecryptBase64 = New Stub(Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic, Randomizer.GenerateNewAlphabetic)
            With DecryptBase64
                .ResolveTypeFromFile(FromBase64Stub(.className, .funcName1, .funcName2), Finder.FindDefaultNamespace(asm, Pack), Randomizer.GenerateNew, Randomizer.GenerateNew, Randomizer.GenerateNew)
                .InjectType(asm)
                completedMethods.Add(.GetMethod1)
                completedMethods.Add(.GetMethod2)
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
                    If Exclude.isStringEncryptExclude(type) = False Then
                        IterateType(type)
                    End If
                Next
                Types.Clear()
            Next

            If encryptToRes = EncryptType.ToResources Then
                Injecter.InjectResource(asm.MainModule, ResName, ResourceType.Embedded, CompressWithGStream(m_s.ToArray))
                m_bw.Close()
            End If

            DeleteStubs()
            CleanUp()

            Return asm
        End Function

        Private Shared Function writeData(str$) As Integer
            Dim IntegPosit% = m_bw.BaseStream.Position
            m_bw.Write(str)
            m_bw.Flush()
            Return IntegPosit
        End Function

        Private Shared Function encrypt(str As String) As Byte()
            Dim list As New List(Of Byte)
            str = xorEncrypt(str, ChrW(DecryptStringKey))
            Dim ch As Char
            For Each ch In str.ToCharArray
                list.Add(AscW(ch))
            Next
            Return list.ToArray
        End Function

        Private Shared Function xorEncrypt(str As String, key As Char) As String
            Dim result As String = String.Empty
            Dim i As Integer
            For i = 0 To str.Length - 1
                result = (result & (AscW(str.Chars(i)) Xor AscW(key)))
            Next i
            Return result
        End Function

        Private Shared Sub DeleteStubs()
            If Not DecryptReadStringResources Is Nothing Then DecryptReadStringResources.DeleteDll()
            If Not DecryptXor Is Nothing Then DecryptXor.DeleteDll()
            If Not DecryptBase64 Is Nothing Then DecryptBase64.DeleteDll()
        End Sub

        Private Shared Sub IterateType(td As TypeDefinition)
            Dim publicMethods As New List(Of MethodDefinition)()
            publicMethods.AddRange(From m In td.Methods Where (m.HasBody AndAlso m.Body.Instructions.Count >= 1 AndAlso Not completedMethods.Contains(m) AndAlso Not m.Name = "get_ResourceManager" AndAlso Not Utils.isStronglyTypedResourceBuilder(m.DeclaringType)))
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
                    Case OpCodes.Ldstr
                        If isValidStringOperand(instruction) Then
                            If Utils.IsSettingStr(body.Method, instruction.Operand.ToString) = False AndAlso Not instruction.Operand.ToString = ResName Then
                                instructionsToExpand.Add(instruction)
                            End If
                        End If
                End Select
            Next

            For Each instruction As Instruction In instructionsToExpand
                Dim str As String = instruction.Operand.ToString()

                Dim salt = randSalt.Next(1, 255)
                'Dim addProperty As Boolean = Randomizer.GenerateBoolean

                m_IsDefaultEncoding = False
                'm_IsDefaultEncoding = Randomizer.GenerateBoolean()

                Dim mdFinal As MethodDefinition = New MethodDefinition(Randomizer.GenerateNew, MethodAttributes.[Static] Or MethodAttributes.Private Or MethodAttributes.HideBySig, AssemblyDef.MainModule.Import(GetType(String)))
                mdFinal.Parameters.Add(New ParameterDefinition(AssemblyDef.MainModule.Import(GetType(Boolean))))
                mdFinal.Body = New MethodBody(mdFinal)
                mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(String))))

                Dim encXor = EncryptXor(EncodeTo_64(str, m_IsDefaultEncoding), salt)

                Dim ilProc As ILProcessor = mdFinal.Body.GetILProcessor()
                ilProc.Body.InitLocals = True

                Dim resEncrypted As Boolean = If((EncryptToResources = EncryptType.ToResources), True, False)

                'Dim resEncrypted As Boolean = If((EncryptToResources = EncryptType.ToResources) AndAlso (Finder.HasCustomAttributeByName(md, "ObfuscationAttribute") = False), True, False)
                If resEncrypted Then
                    'Dim si As Type = GetType(Reflection.ObfuscationAttribute)
                    'Dim ca As New CustomAttribute(AssemblyDef.MainModule.Import(si.GetConstructor(Type.EmptyTypes)))
                    'mdFinal.CustomAttributes.Add(ca)

                    mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(Integer))))
                    mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(String))))
                    ilProc.Emit(OpCodes.Ldc_I4, writeData(encXor))
                    ilProc.Emit(OpCodes.Stloc_1)
                    ilProc.Emit(OpCodes.Ldloc_1)
                    ilProc.Emit(OpCodes.Call, DecryptReadStringResources.GetMethod1)
                    ilProc.Emit(OpCodes.Stloc_2)
                    ilProc.Emit(OpCodes.Ldloc_2)
                Else
                    mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(String))))
                    ilProc.Emit(OpCodes.Ldstr, encXor)
                    ilProc.Emit(OpCodes.Stloc_1)
                    ilProc.Emit(OpCodes.Ldloc_1)
                End If

                mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(Integer))))
                mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(String))))
                mdFinal.Body.Variables.Add(New VariableDefinition(AssemblyDef.MainModule.Import(GetType(Byte()))))

                ilProc.Emit(OpCodes.Ldc_I4, salt)
                ilProc.Emit(OpCodes.Stloc, If(resEncrypted, 3, 2))
                ilProc.Emit(OpCodes.Ldloc, If(resEncrypted, 3, 2))
                ilProc.Emit(OpCodes.Call, DecryptXor.GetMethod1)
                ilProc.Emit(OpCodes.Stloc, If(resEncrypted, 4, 3))
                ilProc.Emit(OpCodes.Ldloc, If(resEncrypted, 4, 3))
                ilProc.Emit(OpCodes.Ldarg_0)
                ilProc.Emit(OpCodes.Call, DecryptBase64.GetMethod1)
                ilProc.Emit(OpCodes.Stloc, If(resEncrypted, 5, 4))
                ilProc.Emit(OpCodes.Ldloc, If(resEncrypted, 5, 4))
                ilProc.Emit(OpCodes.Ldarg_0)
                ilProc.Emit(OpCodes.Call, DecryptBase64.GetMethod2)
                ilProc.Emit(OpCodes.Stloc_0)
                ilProc.Emit(OpCodes.Ldloc_0)
                ilProc.Emit(OpCodes.Ret)

                body.Method.DeclaringType.Methods.Add(mdFinal)

                Dim UnPrime = rand.Next(Generator.numberUnPrime.Length)
                Dim Prime = rand.Next(Generator.numberPrime.Length)
                Dim valFinale% = If(m_IsDefaultEncoding, Generator.numberPrime(Prime), Generator.numberUnPrime(UnPrime))

                Dim instruct = il.Create(OpCodes.Ldc_I4, valFinale)
                il.Replace(instruction, instruct)
                Dim CallDecrypt = il.Create(OpCodes.Call, AssemblyDef.MainModule.Import(DecryptPrime.GetMethod1))
                il.InsertAfter(instruct, CallDecrypt)
                Dim CallMdFinal = il.Create(OpCodes.Call, mdFinal)
                il.InsertAfter(CallDecrypt, CallMdFinal)
                completedMethods.Add(mdFinal)
            Next
        End Sub

        Private Shared Function CompressWithGStream(raw As Byte()) As Byte()
            Using memory As New MemoryStream()
                Using gzip As New GZipStream(memory, CompressionMode.Compress, True)
                    gzip.Write(raw, 0, raw.Length)
                End Using
                Return memory.ToArray()
            End Using
        End Function

        Private Shared Function EncodeTo_64(toEncode$, defaultEnc As Boolean) As String
            Dim strByte = GetByte(toEncode, defaultEnc)
            Return ConvertToBase64String(strByte, defaultEnc)
        End Function

        Public Shared Function GetByte(s As String, defaultEnc As Boolean) As Byte()
            Return If(defaultEnc, Encoding.Default.GetBytes(s), Encoding.UTF8.GetBytes(s))
        End Function

        'Private Shared Function EncodeTo_64(toEncode$) As String
        '    Return Convert.ToBase64String(Text.Encoding.Unicode.GetBytes(toEncode))
        'End Function

        Private Shared Function ConvertToBase64String(data As Byte(), defaultEnc As Boolean) As String
            Dim builder = New StringBuilder()

            Using writer = New StringWriter(builder)
                Using transformation = New ToBase64Transform()
                    Dim bufferedOutputBytes = New Byte(transformation.OutputBlockSize - 1) {}
                    Dim i = 0
                    Dim inputBlockSize = transformation.InputBlockSize

                    While data.Length - i > inputBlockSize
                        transformation.TransformBlock(data, i, data.Length - i, bufferedOutputBytes, 0)
                        i += inputBlockSize
                        Dim str = If(defaultEnc, Encoding.Default.GetString(bufferedOutputBytes), Encoding.UTF8.GetString(bufferedOutputBytes))
                        writer.Write(str)
                    End While

                    bufferedOutputBytes = transformation.TransformFinalBlock(data, i, data.Length - i)
                    Dim strFinal = If(defaultEnc, Encoding.Default.GetString(bufferedOutputBytes), Encoding.UTF8.GetString(bufferedOutputBytes))
                    writer.Write(strFinal)
                    transformation.Clear()
                End Using

                writer.Close()
            End Using

            Return builder.ToString()
        End Function

        Private Shared Function GenerateEncryptXor() As Type
            _objTarget = DecryptXorType(DecryptXor.className, DecryptXor.funcName1)
            Return _objTarget
        End Function

        Private Shared Function EncryptXor(text$, key%) As String
            Return XorEncryptType.InvokeMember(DecryptXor.funcName1, Reflection.BindingFlags.InvokeMethod Or Reflection.BindingFlags.Default, Nothing, _objTarget, New Object() {text, key})
        End Function

        Overloads Shared Sub CleanUp()
            objTarget() = Nothing
            XorEncryptType() = Nothing
            Frmwk = String.Empty
        End Sub
#End Region

    End Class
End Namespace
