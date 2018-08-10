Imports Mono.Cecil
Imports Mono.Cecil.Cil
Imports Helper.RandomizeHelper
Imports Helper.CryptoHelper
Imports System.Resources
Imports System.IO
Imports Helper.AssemblyHelper
Imports Helper.CecilHelper
Imports Helper.CodeDomHelper


Namespace Core.Obfuscation.Builder
    Public Class Source

#Region " Enumerations "
        Public Enum EncryptType
            ByDefault = 0
            ToResources = 1
        End Enum
#End Region

#Region " Fields "
        Protected Shared AssemblyDef As AssemblyDefinition = Nothing
        Protected Shared Pack As Boolean
        Protected Shared Frmwk As String = String.Empty
        Protected Shared rand As Random
        Protected Shared EncryptToResources As EncryptType
        Protected Shared completedMethods As Mono.Collections.Generic.Collection(Of MethodDefinition)
        Protected Shared ResName As String = String.Empty
        Protected Shared ResWriter As ResourceWriter = Nothing

        Private Shared m_AddedNamespaceStart As String = String.Empty
        Private Shared m_AddedNamespaceEnd As String = String.Empty
#End Region

#Region " Constructor "

        Shared Sub New()
            completedMethods = New Mono.Collections.Generic.Collection(Of MethodDefinition)
            rand = New Random
        End Sub

#End Region

#Region " Methods "

        Private Shared Sub LoadNamespacesHeaders()
            Dim NamespaceDefault = Finder.FindDefaultNamespace(AssemblyDef, Pack)

            m_AddedNamespaceStart = "Namespace " & NamespaceDefault
            m_AddedNamespaceEnd = "End Namespace"

            If NamespaceDefault = String.Empty Then
                m_AddedNamespaceStart = String.Empty
                m_AddedNamespaceEnd = String.Empty
            End If

            If Pack Then
                m_AddedNamespaceStart = String.Empty
                m_AddedNamespaceEnd = String.Empty
            End If
        End Sub

        Protected Shared Function isValidOperand(instruct As Instruction) As Boolean
            If Not instruct.Operand Is Nothing Then
                Return True
            End If
            Return False
        End Function

        Protected Shared Function isValidBoolOperand(instruct As Instruction) As Boolean
            If Not instruct.Operand Is Nothing AndAlso (CInt(instruct.Operand) = 0 OrElse CInt(instruct.Operand) = 1) Then
                Return True
            End If
            Return False
        End Function

        Protected Shared Function isValidIntegerOperand(instruct As Instruction) As Boolean
            If Not instruct.Operand Is Nothing AndAlso Not Integer.Parse(instruct.Operand.ToString) = Nothing Then
                Return True
            End If
            Return False
        End Function

        Protected Shared Function isValidLongOperand(instruct As Instruction) As Boolean
            If Not instruct.Operand Is Nothing AndAlso Not Long.Parse(instruct.Operand.ToString) = Nothing Then
                Return True
            End If
            Return False
        End Function

        Protected Shared Function isValidSingleOperand(instruct As Instruction) As Boolean
            If Not instruct.Operand Is Nothing AndAlso Not Single.Parse(instruct.Operand.ToString) = Nothing Then
                Return True
            End If
            Return False
        End Function

        Protected Shared Function isValidDoubleOperand(instruct As Instruction) As Boolean
            If Not instruct.Operand Is Nothing AndAlso Not Double.Parse(instruct.Operand.ToString) = Nothing Then
                Return True
            End If
            Return False
        End Function

        Protected Shared Function isValidStringOperand(instruct As Instruction) As Boolean
            If Not instruct.Operand Is Nothing AndAlso Not String.IsNullOrWhiteSpace(CStr(instruct.Operand)) AndAlso Not CStr(instruct.Operand).Length = 0 Then
                Return True
            End If
            Return False
        End Function

        Protected Shared Function isValidPinvokeCallOperand(instruct As Instruction) As Boolean
            Dim originalReference As MethodReference = Nothing
            Try
                originalReference = TryCast(instruct.Operand, MethodReference)
                If Not originalReference Is Nothing Then
                    Dim originalMethod As MethodDefinition = originalReference.Resolve
                    If (Not originalMethod Is Nothing AndAlso Not originalMethod.DeclaringType Is Nothing) And originalMethod.IsPInvokeImpl Then
                        Return True
                    End If
                End If
            Catch ex As Exception
                Return False
            End Try
            Return False
        End Function

        Protected Shared Function isValidNewObjOperand(instruct As Instruction) As Boolean
            If Not instruct.Operand Is Nothing AndAlso Not DirectCast(instruct.Operand, MethodReference) Is Nothing Then
                Return True
            End If
            Return False
        End Function

        Protected Shared Function DecryptIntStub(ClassName$, DecryptIntFuncName$) As String
            LoadNamespacesHeaders()
            Dim str =
                "Imports System" & vbNewLine &
                "Imports Microsoft.VisualBasic" & vbNewLine _
                        & Loader.GenerateInfos(Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), "1.0.0.0") & vbNewLine _
                        & m_AddedNamespaceStart & vbNewLine _
                        & Generator.GenerateDecryptIntFunc(ClassName, DecryptIntFuncName) & vbNewLine _
                        & m_AddedNamespaceEnd
            Return Compiler.CreateStubFromString(ClassName, Frmwk, str)
        End Function

        Protected Shared Function DecryptOddStub(ClassName$, DecryptOddFuncName$) As String
            LoadNamespacesHeaders()
            Dim str =
                "Imports System" & vbNewLine &
                "Imports Microsoft.VisualBasic" & vbNewLine _
                      & Loader.GenerateInfos(Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), "1.0.0.0") & vbNewLine _
                      & m_AddedNamespaceStart & vbNewLine _
                      & Generator.GenerateDecryptOddFunc(ClassName, DecryptOddFuncName) & vbNewLine _
                      & m_AddedNamespaceEnd
            Return Compiler.CreateStubFromString(ClassName, Frmwk, str)
        End Function

        Protected Shared Function DecryptXorStub(ClassName$, DecryptXorFuncName$) As String
            LoadNamespacesHeaders()
            Dim str = "Imports System" & vbNewLine _
                      & Loader.GenerateInfos(Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), "1.0.0.0") & vbNewLine _
                      & m_AddedNamespaceStart & vbNewLine _
                      & Generator.GenerateDecryptXorFunc(ClassName, DecryptXorFuncName) & vbNewLine _
                      & m_AddedNamespaceEnd
            Return Compiler.CreateStubFromString(ClassName, Frmwk, str)
        End Function

        Protected Shared Function DecryptXorType(ClassName$, DecryptXorFuncName$) As Type
            LoadNamespacesHeaders()
            Dim str = "Imports System" & vbNewLine _
                      & Loader.GenerateInfos(Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), "1.0.0.0") & vbNewLine _
                      & Generator.GenerateDecryptXorFunc(ClassName, DecryptXorFuncName)
            Return Compiler.CreateTypeFromString(ClassName, Frmwk, str)
        End Function

        Protected Shared Function ReadFromResourcesStub(ClassName$, ReadFromResourcesFuncName$) As String
            LoadNamespacesHeaders()
            Dim str =
                "Imports System" & vbNewLine &
                "Imports Microsoft.VisualBasic" & vbNewLine &
                "Imports System.Resources" & vbNewLine _
                          & Loader.GenerateInfos(Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), "1.0.0.0") & vbNewLine _
                          & m_AddedNamespaceStart & vbNewLine _
                          & Generator.GenerateReadFromResourcesFunc(ClassName, ReadFromResourcesFuncName, ResName) & vbNewLine _
                          & m_AddedNamespaceEnd
            Return Compiler.CreateStubFromString(ClassName, Frmwk, str)
        End Function

        Protected Shared Function ReadStringFromResourcesStub(ClassName$, ResourceDecryptFunc$, Decompress0$, Decompress1$) As String
            LoadNamespacesHeaders()
            Dim ms$ = Randomizer.GenerateNewAlphabetic

            Dim str =
                "Imports System.Windows.Forms" & vbNewLine &
                "Imports System.Collections.Generic" & vbNewLine &
                "Imports System" & vbNewLine &
                "Imports System.IO" & vbNewLine &
                "Imports System.IO.Compression" & vbNewLine & vbNewLine _
                       & Loader.GenerateInfos(Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), "1.0.0.0") & vbNewLine _
                       & "Public Class " & ClassName & vbNewLine _
                       & "    Private Shared " & ms & " As Stream " & vbNewLine _
                       & "    Public Shared Function " & ResourceDecryptFunc & " (ByVal BaseStreamPos As Integer) As String" & vbNewLine _
                       & "        Dim br As New BinaryReader(" & ms & ")" & vbNewLine _
                       & "        br.BaseStream.Position = BaseStreamPos" & vbNewLine _
                       & "        Return br.ReadString" & vbNewLine _
                       & "    End Function" & vbNewLine _
                       & "    Shared Sub New" & vbNewLine _
                       & "        If " & ms & " Is Nothing Then" & vbNewLine _
                       & "           Dim by as Byte()" & vbNewLine _
                       & "           by = " & Decompress0 & "(Assembly.GetExecutingAssembly.GetManifestResourceStream(""" & ResName & """))" & vbNewLine _
                       & "           " & ms & " = New MemoryStream(by)" & vbNewLine _
                       & "        End If" & vbNewLine _
                       & "    End Sub" & vbNewLine _
                       & Generator.GenerateDeCompressWithGzipStreamFunc(Decompress0, Decompress1) & vbNewLine _
                       & "End Class"
            Return Compiler.CreateStubFromString(ClassName, Frmwk, str)
        End Function

        Protected Shared Sub InjectResource()
            Dim CompressRes As EmbeddedResource = New EmbeddedResource(ResName & ".resources", ManifestResourceAttributes.Private, File.ReadAllBytes(My.Application.Info.DirectoryPath & "\" & ResName & ".resources"))
            AssemblyDef.MainModule.Resources.Add(CompressRes)
        End Sub

        Protected Shared Function DecryptPrimeStub(className$, DecryptPrimeFuncName$) As String
            LoadNamespacesHeaders()
            'MsgBox("start : " & m_AddedNamespaceStart & vbNewLine & "End : " & m_AddedNamespaceEnd)
            Dim str =
                "Imports System.Collections.Generic" & vbNewLine &
                "Imports System" & vbNewLine &
                      Loader.GenerateInfos(Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), "1.0.0.0") & vbNewLine _
                    & m_AddedNamespaceStart & vbNewLine _
                    & "Public Class " & className & vbNewLine _
                    & Generator.GenereateDecryptPrimeFunc(DecryptPrimeFuncName) & vbNewLine _
                    & "End Class" & vbNewLine & vbNewLine _
                    & m_AddedNamespaceEnd

            Return Compiler.CreateStubFromString(className, Frmwk, str)
        End Function

        Protected Shared Function DecryptCtrFlowStub(ClassName$, DecryptCtrFlowFuncName As String) As String
            LoadNamespacesHeaders()

            Dim str =
                    "Imports System" & vbNewLine &
                      Loader.GenerateInfos(Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), "1.0.0.0") & vbNewLine _
                    & vbNewLine & "Namespace T" & Randomizer.GenerateAlphabetic(10) & vbNewLine _
                    & "Public Class " & ClassName & vbNewLine _
                    & "    Public Shared Function " & DecryptCtrFlowFuncName & " (Str As String) As Integer" & vbNewLine _
                    & "        Return Str.Length - 21" & vbNewLine _
                    & "    End Function" & vbNewLine _
                    & "End Class" & vbNewLine & vbNewLine _
                    & "End Namespace"

            Return Compiler.CreateStubFromString(ClassName, Frmwk, str)
        End Function


        Protected Shared Function DecryptRPNStub(ClassName$, DecryptRPNFuncName1$, DecryptRPNFuncName2$) As String
            LoadNamespacesHeaders()
            Dim str =
                "Imports System.Collections.Generic" & vbNewLine &
                "Imports System" & vbNewLine &
                      Loader.GenerateInfos(Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), "1.0.0.0") & vbNewLine _
                    & m_AddedNamespaceStart & vbNewLine _
                    & "Public Class " & ClassName & vbNewLine _
                    & Generator.GenerateDecryptRPNFunc(DecryptRPNFuncName1, DecryptRPNFuncName2) & vbNewLine _
                    & "End Class" & vbNewLine & vbNewLine _
                    & m_AddedNamespaceEnd

            Return Compiler.CreateStubFromString(ClassName, Frmwk, str)
        End Function

        Protected Shared Function FromBase64Stub(ClassName$, Base64FuncName$, GetStringFuncName$) As String
            LoadNamespacesHeaders()
            Dim str =
                "Imports System" & vbNewLine &
                "Imports System.Text" & vbNewLine &
                "Imports System.IO" & vbNewLine &
                "Imports System.Security.Cryptography" & vbNewLine _
                      & Loader.GenerateInfos(Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), "1.0.0.0") & vbNewLine _
                      & m_AddedNamespaceStart & vbNewLine _
                      & Generator.GenerateFromBase64Func(ClassName, Base64FuncName, GetStringFuncName) & vbNewLine _
                      & m_AddedNamespaceEnd
            Return Compiler.CreateStubFromString(ClassName, Frmwk, str)
        End Function

        Protected Shared Function SevenZipStub(ClassName$, initializeFuncName$, resolverName$, Decompress0$, Decompress1$, encrypt As Boolean, compress As Boolean) As String
            Dim reverseStr = If(encrypt = True, "                    Array.Reverse(d)", String.Empty)
            Dim DecompressStr0 = "                d = " & Decompress0 & "(cm.ToArray)"

            Dim str =
                "Imports System.Windows.Forms" & vbNewLine &
                "Imports System.Collections.Generic" & vbNewLine &
                "Imports System" & vbNewLine &
                "Imports System.Xml" & vbNewLine &
                "Imports System.IO" & vbNewLine &
                "Imports System.IO.Compression" & vbNewLine & vbNewLine _
                       & Loader.GenerateInfos(Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), "1.0.0.0") & vbNewLine _
                       & "Public Class " & ClassName & vbNewLine _
                       & "    Private Shared Function " & resolverName & " (ByVal sender As Object, ByVal args As ResolveEventArgs) As Assembly" & vbNewLine _
                       & "        Dim names As String() = Nothing" & vbNewLine _
                       & "        Dim k As String = Cstr(New AssemblyName(args.Name).Name)" & vbNewLine _
                       & "        'Dim directoryName As String = Path.GetDirectoryName(Assembly.GetEntryAssembly.Location)" & vbNewLine _
                       & "        'msgbox(directoryName)" & vbNewLine _
                       & "        'For each f As String in Directory.GetFiles(""*.dll"", Assembly.GetExecutingAssembly.Location)" & vbNewLine _
                       & "            'MsgbOx(Assembly.GetExecutingAssembly.Location)" & vbNewLine _
                       & "            'Try" & vbNewLine _
                       & "                'If f.EndsWith("".xmlserializers.dll"", StringComparison.OrdinalIgnoreCase) Then" & vbNewLine _
                       & "                    'Msgbox(""oui"")" & vbNewLine _
                       & "                    'Return Assembly.LoadFrom(f)" & vbNewLine _
                       & "                'End If" & vbNewLine _
                       & "             " & vbNewLine _
                       & "            'Catch Ex As Exception" & vbNewLine _
                       & "            'End Try" & vbNewLine _
                       & "        'Next" & vbNewLine _
                       & "        Dim ass As Assembly = Nothing" & vbNewLine _
                       & "        Dim d as Byte()" & vbNewLine _
                       & "        'Dim k As String = Cstr(New AssemblyName(args.Name).Name)" & vbNewLine _
                       & "        'MsGbox(k & vbnewline)" & vbNewLine _
                       & "        If k.EndsWith("".xmlserializers"", StringComparison.OrdinalIgnoreCase) Then" & vbNewLine _
                       & "            Return Nothing" & vbNewLine _
                       & "        End If" & vbNewLine _
                       & "        If k.EndsWith("".resources"", StringComparison.OrdinalIgnoreCase) Then" & vbNewLine _
                       & "            Return Nothing" & vbNewLine _
                       & "        End If" & vbNewLine _
                       & "        SyncLock hashtable" & vbNewLine _
                       & "            If hashtable.ContainsKey(""" & ResName & """) Then" & vbNewLine _
                       & "                Return Nothing" & vbNewLine _
                       & "            End If" & vbNewLine _
                       & "        Using cs As Stream = (GetType(System.Reflection.Assembly).GetMethod(""GetExecutingAssembly"").Invoke(Nothing, Nothing)).GetManifestResourceStream(""" & ResName & """)" & vbNewLine _
                       & "            If cs Is Nothing Then" & vbNewLine _
                       & "                Return Nothing" & vbNewLine _
                       & "            End If" & vbNewLine _
                       & "            Using cm As MemoryStream = New MemoryStream" & vbNewLine _
                       & "                Const bValue = 4096" & vbNewLine _
                       & "                Dim buffer As Byte() = New Byte(bValue - 1) {}" & vbNewLine _
                       & "                Dim count As Integer = cs.Read(buffer, 0, bValue)" & vbNewLine _
                       & "                Do" & vbNewLine _
                       & "                    cm.Write(buffer, 0, count)" & vbNewLine _
                       & "                    count = cs.Read(buffer, 0, bValue)" & vbNewLine _
                       & "                Loop While (count <> 0)" & vbNewLine _
                       & If(compress = True, DecompressStr0, "d = cm.ToArray()") & vbNewLine _
                       & "                " & reverseStr & vbNewLine _
                       & "                ass = Assembly.Load(d)" & vbNewLine _
                       & "                hashtable.Item(""" & ResName & """) = ass" & vbNewLine _
                       & "                Return ass" & vbNewLine _
                       & "                'Msgbox(""Test"" & vbnewline & ass.GetName.Name)" & vbNewLine _
                       & "            End Using" & vbNewLine _
                       & "        End Using" & vbNewLine _
                       & "        End SyncLock" & vbNewLine _
                       & "        Return Nothing" & vbNewLine _
                       & "    End Function" & vbNewLine _
                       & "    Shared Sub new" & vbNewLine _
                       & "        hashtable = New Dictionary(Of String, Assembly)" & vbNewLine _
                       & "    End Sub" & vbNewLine _
                       & If(compress = True, Generator.GenerateCompressWithGzipByteFunc(Decompress0, Decompress1), "") & vbNewLine _
                       & "    Private Shared hashtable As Dictionary(Of String, Assembly)" & vbNewLine _
                       & "    Public Shared Sub " & initializeFuncName & vbNewLine _
                       & "        AddHandler AppDomain.CurrentDomain.AssemblyResolve, New ResolveEventHandler(AddressOf " & resolverName & ")" & vbNewLine _
                       & "    End Sub" & vbNewLine _
                       & "End Class"

            Return Compiler.CreateStubFromString(ClassName, Frmwk, str)
        End Function

        Protected Shared Function PackerStub(Resolver As Stub, EncodedResName$, m_polyXor As Crypt, encrypt As Boolean) As String
            Dim ResourceAssembly = Randomizer.GenerateNewAlphabetic
            Dim decodeString = Randomizer.GenerateNewAlphabetic
            Dim Decrypt = Randomizer.GenerateNewAlphabetic
            Dim fromBase64 = Randomizer.GenerateNewAlphabetic

            Dim reverseStr As String = String.Empty
            If encrypt Then
                reverseStr = "                    Array.Reverse(byt)"
            End If

            Dim aesStr As String = "    Private Shared Function " & Decrypt & "(ByVal iByte As Byte()) As Byte()" & vbNewLine _
                        & "        Dim k as Byte() = " & Resolver.ReferencedZipperAssembly.refNewTypeName & ".pKey(""" & Convert.ToBase64String(SevenZipLib.SevenZipHelper.Compress(m_polyXor.key)) & """)" & vbNewLine _
                        & "        Dim O As Byte() = New Byte(iByte.Length - " & m_polyXor.SaltSize.ToString & " - 1) {}" & vbNewLine _
                        & "        Dim S As Byte() = New Byte(" & m_polyXor.SaltSize.ToString & " - 1) {}" & vbNewLine _
                        & "        Buffer.BlockCopy(iByte, iByte.Length - " & m_polyXor.SaltSize.ToString & ", S, 0, " & m_polyXor.SaltSize.ToString & ")" & vbNewLine _
                        & "        Array.Resize(Of Byte)(iByte, iByte.Length - " & m_polyXor.SaltSize.ToString & ")" & vbNewLine _
                        & "        For j As Integer = 0 To iByte.Length - 1" & vbNewLine _
                        & "            O(j) = CByte(iByte(j) Xor k(j Mod k.Length) Xor S(j Mod S.Length))" & vbNewLine _
                        & "        Next" & vbNewLine _
                        & "        Return O" & vbNewLine _
                        & "    End Function"

            Dim str = "Imports System.Windows.Forms" & vbNewLine &
                    "Imports System.Security.Cryptography" & vbNewLine &
                    "Imports System" & vbNewLine &
                    "Imports System.Threading" & vbNewLine &
                    "Imports System.Text" & vbNewLine &
                    "Imports System.IO" & vbNewLine &
                    "Imports System.Resources" & vbNewLine &
                    "Imports System.IO.Compression" & vbNewLine &
                    "Imports " & Resolver.ReferencedZipperAssembly.refNewNamespaceName & vbNewLine & vbNewLine _
          & Loader.GenerateInfos(Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), "1.0.0.0") & vbNewLine _
          & "Friend Class " & Resolver.className & vbNewLine & vbNewLine _
          & "    Private Delegate Function z() As Assembly" & vbNewLine _
          & "    <STAThread()> _" & vbNewLine _
          & "    Public Shared Sub Main(ByVal args As String())" & vbNewLine _
          & "        Dim yassembly As Assembly = " & ResourceAssembly & "((""app, version=0.0.0.0, culture=neutral, publickeytoken=null"").Replace("".resources"",""""))" & vbNewLine _
          & "        Dim yentryPoint As MethodInfo = yassembly.EntryPoint" & vbNewLine _
          & "        Dim yparameters As ParameterInfo() = yentryPoint.GetParameters" & vbNewLine _
          & "        Dim yobjArray As Object() = Nothing" & vbNewLine _
          & "        If ((Not yparameters Is Nothing) AndAlso (yparameters.Length > 0)) Then" & vbNewLine _
          & "            yobjArray = New Object() {args}" & vbNewLine _
          & "        End If" & vbNewLine _
          & "        yentryPoint.Invoke(Nothing, yobjArray)" & vbNewLine _
          & "    End Sub" & vbNewLine _
          & "    Private Shared Function " & decodeString & "(Byval Str as String) As String" & vbNewLine _
          & "        Return Encoding.Default.GetString(" & fromBase64 & "(Str))" & vbNewLine _
          & "    End Function" & vbNewLine _
          & "    Private Shared Function " & ResourceAssembly & "(nAss As String) As Assembly" & vbNewLine _
          & "        Dim asm As Assembly = Nothing" & vbNewLine _
          & "        Using st As Stream = DirectCast([Delegate].CreateDelegate(GetType(z), GetType(Assembly).GetMethod(""GetExecutingAssembly"", New Type() {})), z).Invoke.GetManifestResourceStream(nAss & "".resources"")" & vbNewLine _
          & "            If st Is Nothing Then" & vbNewLine _
          & "                Return asm" & vbNewLine _
          & "            End If" & vbNewLine _
          & "            Dim byt As Byte() = " & Resolver.ReferencedZipperAssembly.refNewTypeName & "." & Resolver.ReferencedZipperAssembly.refNewMethodName & "(New BinaryReader(st).ReadBytes(CInt(st.Length)))" & vbNewLine _
          & "            " & reverseStr & vbNewLine _
          & "            asm = Assembly.Load(" & Decrypt & "(byt))" & vbNewLine _
          & "        End Using" & vbNewLine _
          & "        Return asm" & vbNewLine _
          & "    End Function" & vbNewLine _
          & Generator.GenerateCompressWithGzipByteFunc(Resolver.funcName1, Resolver.funcName2) & vbNewLine _
          & "    Private Shared Function " & fromBase64 & "(ByVal iStr As String) As Byte()" & vbNewLine _
          & "        Return Convert.FromBase64String(iStr)" & vbNewLine _
          & "    End Function" & vbNewLine _
          & "    " & aesStr & vbNewLine _
          & "End Class"

            Dim dic As New Dictionary(Of String, Byte())
            dic.Add(Resolver.ReferencedZipperAssembly.fPath, Resolver.ReferencedZipperAssembly.refByte)

            Return Compiler.CreateStubFromString(Resolver.className, Frmwk, str.Replace("app, version=0.0.0.0, culture=neutral, publickeytoken=null", EncodedResName), dic)
        End Function

        Protected Shared Function ResourcesStub(ClassName$, initializeFuncName$, resolverName$, Decompress0$, Decompress1$, encrypt As Boolean, compress As Boolean) As String
            LoadNamespacesHeaders()

            Dim reverseStr = If(encrypt = True, "                    Array.Reverse(d)", String.Empty)
            Dim DecompressStr0 = "                d = " & Decompress0 & "(cm.ToArray)"

            Dim str =
                "Imports System.Windows.Forms" & vbNewLine &
                "Imports System.Collections.Generic" & vbNewLine &
                "Imports System" & vbNewLine &
                "Imports System.IO" & vbNewLine &
                "Imports System.IO.Compression" & vbNewLine & vbNewLine _
                       & Loader.GenerateInfos(Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), "1.0.0.0") & vbNewLine _
                       & m_AddedNamespaceStart & vbNewLine _
                       & "Public Class " & ClassName & vbNewLine _
                       & "    Private Shared Function " & resolverName & " (ByVal sender As Object, ByVal args As ResolveEventArgs) As Assembly" & vbNewLine _
                       & "        Dim names As String() = Nothing" & vbNewLine _
                       & "        Dim ass As Assembly = Nothing" & vbNewLine _
                       & "        Dim d as Byte()" & vbNewLine _
                       & "        If (ass Is Nothing) Then" & vbNewLine _
                       & "        Using cs As Stream = (GetType(System.Reflection.Assembly).GetMethod(""GetExecutingAssembly"").Invoke(Nothing, Nothing)).GetManifestResourceStream(""" & ResName & """)" & vbNewLine _
                       & "            If cs Is Nothing Then" & vbNewLine _
                       & "                Return Nothing" & vbNewLine _
                       & "            End If" & vbNewLine _
                       & "            Using cm As MemoryStream = New MemoryStream" & vbNewLine _
                       & "                Const bValue = 4096" & vbNewLine _
                       & "                Dim buffer As Byte() = New Byte(bValue - 1) {}" & vbNewLine _
                       & "                Dim count As Integer = cs.Read(buffer, 0, bValue)" & vbNewLine _
                       & "                Do" & vbNewLine _
                       & "                    cm.Write(buffer, 0, count)" & vbNewLine _
                       & "                    count = cs.Read(buffer, 0, bValue)" & vbNewLine _
                       & "                Loop While (count <> 0)" & vbNewLine _
                       & If(compress = True, DecompressStr0, "d = cm.ToArray()") & vbNewLine _
                       & "                " & reverseStr & vbNewLine _
                       & "                ass = Assembly.Load(d)" & vbNewLine _
                       & "                names = ass.GetManifestResourceNames" & vbNewLine _
                       & "            End Using" & vbNewLine _
                       & "        End Using" & vbNewLine _
                       & "        End If" & vbNewLine _
                       & "        If New List(Of String)(names).Contains(args.Name) Then" & vbNewLine _
                       & "            Return ass" & vbNewLine _
                       & "        End If" & vbNewLine _
                       & "        Return Nothing" & vbNewLine _
                       & "    End Function" & vbNewLine _
                       & If(compress = True, Generator.GenerateCompressWithGzipByteFunc(Decompress0, Decompress1), "") & vbNewLine _
                       & "    Public Shared Sub " & initializeFuncName & vbNewLine _
                       & "        AddHandler AppDomain.CurrentDomain.ResourceResolve, New ResolveEventHandler(AddressOf " & resolverName & ")" & vbNewLine _
                       & "    End Sub" & vbNewLine _
                       & "End Class" & vbNewLine _
                       & m_AddedNamespaceEnd

            Return Compiler.CreateStubFromString(ClassName, Frmwk, str)
        End Function

        Protected Shared Function AntiDebugStub(classname$, funcName$) As String
            LoadNamespacesHeaders()
            Dim FuncName2 = Randomizer.GenerateNewAlphabetic

            Dim str As String =
    "Imports System" & vbNewLine &
    "Imports System.Diagnostics" & vbNewLine &
    "Imports System.Threading" & vbNewLine &
     Loader.GenerateInfos(Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), "1.0.0.0") & vbNewLine _
    & m_AddedNamespaceStart & vbNewLine &
        "Friend Class " & classname & vbNewLine &
            "Public Shared Sub " & funcName & "()" & vbNewLine &
            "   If ((Not Environment.GetEnvironmentVariable(""COR_ENABLE_PROFILING"") Is Nothing) OrElse (Not Environment.GetEnvironmentVariable(""COR_PROFILER"") Is Nothing)) Then" & vbNewLine &
            "       Environment.FailFast(""Profiler detected"")" & vbNewLine &
            "   End If" & vbNewLine &
            "   Dim parameter As New Thread(New ParameterizedThreadStart(AddressOf " & FuncName2 & "))" & vbNewLine &
            "   Dim t As New Thread(New ParameterizedThreadStart(AddressOf " & FuncName2 & "))" & vbNewLine &
            "   parameter.IsBackground = True" & vbNewLine &
            "   t.IsBackground = True" & vbNewLine &
            "   parameter.Start(t)" & vbNewLine &
            "   Thread.Sleep(500)" & vbNewLine &
            "   t.Start(parameter)" & vbNewLine &
            "End Sub" & vbNewLine & vbNewLine &
            "Private Shared Sub " & FuncName2 & "(ByVal th As Object)" & vbNewLine &
            "   Thread.Sleep(&H3E8)" & vbNewLine &
            "   Dim t As Thread = DirectCast(th, Thread)" & vbNewLine &
            "   Do While True" & vbNewLine &
            "       If (Debugger.IsAttached OrElse Debugger.IsLogging) Then" & vbNewLine &
            "           Environment.FailFast(""Debugger detected (Managed)"")" & vbNewLine &
            "       End If" & vbNewLine &
            "       If Not t.IsAlive Then" & vbNewLine &
            "           Environment.FailFast(""Loop broken"")" & vbNewLine &
            "       End If" & vbNewLine &
            "       Thread.Sleep(&H3E8)" & vbNewLine &
            "   Loop" & vbNewLine &
            "End Sub" & vbNewLine & vbNewLine &
        "End Class" & vbNewLine & vbNewLine &
    m_AddedNamespaceEnd
            Return Compiler.CreateStubFromString(classname, Frmwk, str)
        End Function

        Protected Shared Function AntiTamperStub(className$, FuncName$) As String
            LoadNamespacesHeaders()
            Dim str As String =
                "Imports System.Security.Cryptography" & vbNewLine &
                "Imports System.Windows.Forms" & vbNewLine &
                "Imports System.Collections.Generic" & vbNewLine &
                "Imports System" & vbNewLine &
                "Imports System.IO" & vbNewLine &
                "Imports System.IO.Compression" & vbNewLine & vbNewLine _
                 & Loader.GenerateInfos(Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), "1.0.0.0") & vbNewLine _
                 & m_AddedNamespaceStart & vbNewLine _
                 & "Public Class " & className & vbNewLine _
                 & "    Public Shared Sub " & FuncName & " ()" & vbNewLine _
                 & "        Dim l As String = (GetType(System.Reflection.Assembly).GetMethod(""GetExecutingAssembly"").Invoke(Nothing, Nothing)).Location" & vbNewLine _
                 & "        Dim b As Stream = New StreamReader(l).BaseStream" & vbNewLine _
                 & "        Dim r As New BinaryReader(b)" & vbNewLine _
                 & "        Dim b0 As String = Nothing" & vbNewLine _
                 & "        Dim b1 As String = Nothing" & vbNewLine _
                 & "        b0 = BitConverter.ToString(Ctype(CryptoConfig.CreateFromName(" & Chr(34) & "MD5" & Chr(34) & "), HashAlgorithm).ComputeHash(r.ReadBytes((File.ReadAllBytes(l).Length - 16))))" & vbNewLine _
                 & "        b.Seek(-16, SeekOrigin.End)" & vbNewLine _
                 & "        b1 = BitConverter.ToString(r.ReadBytes(16))" & vbNewLine _
                 & "        If (b0 <> b1) Then" & vbNewLine _
                 & "            Throw New BadImageFormatException" & vbNewLine _
                 & "        End If" & vbNewLine _
                 & "    End Sub" & vbNewLine _
                 & "End Class" & vbNewLine _
                 & m_AddedNamespaceEnd
            Return Compiler.CreateStubFromString(className, Frmwk, str)
        End Function

        Protected Shared Function DynamicInvokeStub(className$, m_loadLibraryFuncName$, m_getMethProcFuncName$, m_invokeMethFuncName$) As String
            LoadNamespacesHeaders()
            Dim str = "Imports System.Threading" & vbNewLine &
                        "Imports System" & vbNewLine &
                        "Imports System.Reflection.Emit" & vbNewLine &
                        "Imports System.Runtime.InteropServices" & vbNewLine & vbNewLine _
                       & Loader.GenerateInfos(Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), "1.0.0.0") & vbNewLine _
                       & m_AddedNamespaceStart & vbNewLine _
                       & "Public Class " & className & vbNewLine _
                       & "    <DllImport(""kernel32.dll"", EntryPoint :=""LoadLibrary"")> _" & vbNewLine _
                       & "    Private Shared Function " & m_loadLibraryFuncName & "(hLib As String) As IntPtr" & vbNewLine _
                       & "    End Function" & vbNewLine _
                       & "    <DllImport(""kernel32.dll"", EntryPoint :=""GetProcAddress"",CharSet:=CharSet.Ansi, ExactSpelling:=True)> _" & vbNewLine _
                       & "    Private Shared Function " & m_getMethProcFuncName & "(hMod As IntPtr, pName As String) As IntPtr" & vbNewLine _
                       & "    End Function" & vbNewLine _
                       & "    Public Shared Function " & m_invokeMethFuncName & " (Of T As Class)(libF$, funcN$) As T" & vbNewLine _
                       & "        Dim ll As IntPtr = " & m_loadLibraryFuncName & "(libF)" & vbNewLine _
                       & "        Dim delegT As System.Delegate = Marshal.GetDelegateForFunctionPointer(" & m_getMethProcFuncName & "(ll, funcN), GetType(T))" & vbNewLine _
                       & "        Return TryCast(delegT, T)" & vbNewLine _
                       & "    End Function" & vbNewLine _
                       & "End Class" & vbNewLine _
                       & m_AddedNamespaceEnd
            Return Compiler.CreateStubFromString(className, Frmwk, str)
        End Function

        Protected Shared Function ResourcesEmbeddingStub(ClassName$, initializeFuncName$, encrypt As Boolean, compress As Boolean) As String
            LoadNamespacesHeaders()

            Dim resolverName = Randomizer.GenerateNewAlphabetic
            Dim Decompress0 = Randomizer.GenerateNewAlphabetic
            Dim Decompress1 = Randomizer.GenerateNewAlphabetic

            Dim reverseStr = If(encrypt = True, "                    Array.Reverse(b)", String.Empty)
            Dim DecompressStr0 = "                b = " & Decompress0 & "(b)"

            Dim str = "Imports Microsoft.VisualBasic" & vbNewLine &
                        "Imports System.Windows.Forms" & vbNewLine &
                        "Imports System.Runtime.InteropServices" & vbNewLine &
                        "Imports System.Collections.Generic" & vbNewLine &
                        "Imports System" & vbNewLine &
                        "Imports System.IO" & vbNewLine &
                        "Imports System.IO.Compression" & vbNewLine & vbNewLine _
                       & Loader.GenerateInfos(Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), Randomizer.GenerateAlphabetic(5), "1.0.0.0") & vbNewLine _
                       & m_AddedNamespaceStart & vbNewLine _
                       & "Public Class " & ClassName & vbNewLine _
                       & "    <DllImport(""kernel32"")> _" & vbNewLine _
                       & "    Private Shared Function MoveFileEx(ByVal existingFileName As String, ByVal newFileName As String, ByVal flags As Integer) As Boolean" & vbNewLine _
                       & "    End Function" & vbNewLine _
                       & "    Private Delegate Function z() As Assembly" & vbNewLine _
                       & "    Private Shared Function " & resolverName & " (ByVal sender As Object, ByVal args As ResolveEventArgs) As Assembly" & vbNewLine _
                       & If(Pack, "        Dim k As String = getEnc(Cstr(New AssemblyName(args.Name).FullName.GetHashCode))", "        Dim k As String = Cstr(New AssemblyName(args.Name).FullName.GetHashCode)") & vbNewLine _
                       & "        Dim ass As Assembly = Nothing" & vbNewLine _
                       & "        If Not k.Length = 0 Then" & vbNewLine _
                       & "        Dim baseResourceName As String =  k & "".resources""" & vbNewLine _
                       & "        Dim bn as boolean" & vbNewLine _
                       & "        SyncLock hashtable" & vbNewLine _
                       & "            If hashtable.ContainsKey(baseResourceName) Then" & vbNewLine _
                       & "                Return hashtable.Item(baseResourceName)" & vbNewLine _
                       & "            End If" & vbNewLine _
                       & "            Using st As Stream = DirectCast([Delegate].CreateDelegate(GetType(z), GetType(Assembly).GetMethod(""GetExecutingAssembly"", New Type() {})), z).Invoke.GetManifestResourceStream(baseResourceName)" & vbNewLine _
                       & "                If st Is Nothing Then" & vbNewLine _
                       & "                    Return ass" & vbNewLine _
                       & "                End If" & vbNewLine _
                       & "                Dim b As Byte() = New BinaryReader(st).ReadBytes(CInt(st.Length))" & vbNewLine _
                       & If(compress = True, DecompressStr0, "") & vbNewLine _
                       & "                " & reverseStr & vbNewLine _
                       & "                Try" & vbNewLine _
                       & "                    ass = Assembly.Load(b)" & vbNewLine _
                       & "                Catch ex1 As FileLoadException" & vbNewLine _
                       & "                    bn = True" & vbNewLine _
                       & "                Catch ex2 As BadImageFormatException" & vbNewLine _
                       & "                    bn = True" & vbNewLine _
                       & "                End Try" & vbNewLine _
                       & "                If bn Then" & vbNewLine _
                       & "                    Try" & vbNewLine _
                       & "                        Dim npath As String = String.Format(""{0}{1}\"", System.IO.Path.GetTempPath, k)" & vbNewLine _
                       & "                        Directory.CreateDirectory(npath)" & vbNewLine _
                       & "                        Dim nfileP As String = (npath & baseResourceName)" & vbNewLine _
                       & "                        If Not File.Exists(nfileP) Then" & vbNewLine _
                       & "                            Dim fStream As FileStream = File.OpenWrite(nfileP)" & vbNewLine _
                       & "                            fStream.Write(b, 0, b.Length)" & vbNewLine _
                       & "                            fStream.Close" & vbNewLine _
                       & "                            MoveFileEx(nfileP, Nothing, 4)" & vbNewLine _
                       & "                            MoveFileEx(npath, Nothing, 4)" & vbNewLine _
                       & "                        End If" & vbNewLine _
                       & "                        ass = Assembly.LoadFile(nfileP)" & vbNewLine _
                       & "                    Catch Ex As Exception" & vbNewLine _
                       & "                    End Try" & vbNewLine _
                       & "                End If" & vbNewLine _
                       & "                hashtable.Item(baseResourceName) = ass" & vbNewLine _
                       & "                Return ass" & vbNewLine _
                       & "            End Using" & vbNewLine _
                       & "        End SyncLock" & vbNewLine _
                       & "        End If" & vbNewLine _
                       & "        Return ass" & vbNewLine _
                       & "    End Function" & vbNewLine _
                       & "    Shared Sub new" & vbNewLine _
                       & "        hashtable = New Dictionary(Of String, Assembly)" & vbNewLine _
                       & "    End Sub" & vbNewLine _
                       & If(compress = True, Generator.GenerateCompressWithGzipByteFunc(Decompress0, Decompress1), "") & vbNewLine _
                       & "    Private Shared hashtable As Dictionary(Of String, Assembly)" & vbNewLine _
                       & "    Public Shared Sub " & initializeFuncName & vbNewLine _
                       & "        AddHandler AppDomain.CurrentDomain.AssemblyResolve, New ResolveEventHandler(AddressOf " & resolverName & ")" & vbNewLine _
                       & "    End Sub" & vbNewLine _
                       & "    Private Shared Function getEnc(Str$) As String" & vbNewLine _
                       & "        Return Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(Str))" & vbNewLine _
                       & "    End Function" & vbNewLine _
                       & "End Class" & vbNewLine & vbNewLine &
                       m_AddedNamespaceEnd

            Return Compiler.CreateStubFromString(ClassName, Frmwk, str)
        End Function

        Overloads Shared Sub CleanUp()
            Frmwk = String.Empty
            completedMethods.Clear()
            If Not ResWriter Is Nothing Then ResWriter.Dispose()
            If File.Exists(My.Application.Info.DirectoryPath & "\" & ResName & ".resources") Then
                File.Delete(My.Application.Info.DirectoryPath & "\" & ResName & ".resources")
            End If
            ResName = String.Empty
            Pack = False
        End Sub

#End Region

    End Class
End Namespace




