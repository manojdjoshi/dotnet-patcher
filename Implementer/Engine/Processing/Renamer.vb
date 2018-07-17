Imports Mono.Cecil
Imports Mono.Cecil.Cil
Imports Helper.RandomizeHelper
Imports Helper.CecilHelper

Namespace Engine.Processing
    ''' <summary>
    ''' INFO : This is the forth step of the renamer library. 
    '''        This is the core of the rename library !
    ''' </summary>
    Friend NotInheritable Class Renamer

#Region " Methods "
        ''' <summary>
        ''' INFO : Rename the method. Return methodDefinition member.
        ''' </summary>
        ''' <param name="method"></param>
        Friend Shared Function RenameMethod(type As TypeDefinition, method As MethodDefinition) As MethodDefinition
            Dim MethodOriginal = method.Name
            Dim MethodPublicObf = Randomizer.GenerateNew()

            If method.IsPInvokeImpl Then
                If method.PInvokeInfo.EntryPoint = String.Empty Then method.PInvokeInfo.EntryPoint = MethodOriginal
            End If

            If Not Finder.FindGenericParameter(method) AndAlso Not Finder.HasCustomAttributeByName(method, "DebuggerHiddenAttribute") Then
                method.Name = Mapping.RenameMethodMember(method, MethodPublicObf)
            End If

            Return method
        End Function

        ''' <summary>
        ''' INFO : Rename Parameters from method.
        ''' </summary>
        ''' <param name="method"></param>
        Friend Shared Sub RenameParameters(method As MethodDefinition)
            If method.HasParameters Then
                For Each ParDef As ParameterDefinition In method.Parameters
                    If ParDef.CustomAttributes.Count = 0 Then
                        ParDef.Name = Mapping.RenameParamMember(ParDef, Randomizer.GenerateNew())
                    End If
                Next
            End If
            If method.HasGenericParameters Then
                For Each GenPar As GenericParameter In method.GenericParameters
                    If GenPar.CustomAttributes.Count = 0 Then
                        GenPar.Name = Mapping.RenameGenericParamMember(GenPar, Randomizer.GenerateNew())
                    End If
                Next
            End If
        End Sub

        ''' <summary>
        ''' INFO : Rename Variables from method.
        ''' </summary>
        ''' <param name="method"></param>
        Friend Shared Sub RenameVariables(Method As MethodDefinition)
            If Method.HasBody Then
                For Each vari In Method.Body.Variables
                    vari.Name = Mapping.RenameVariableMember(vari, Randomizer.GenerateNew())
                Next
            End If
        End Sub

        ''' <summary>
        ''' INFO : Rename embedded Resources from Resources dir and updates method bodies.
        ''' </summary>
        ''' <param name="TypeDef"></param>
        ''' <param name="NamespaceOriginal"></param>
        ''' <param name="NamespaceObfuscated"></param>
        ''' <param name="TypeOriginal"></param>
        ''' <param name="TypeObfuscated"></param>
        Friend Shared Sub RenameResources(TypeDef As TypeDefinition, ByRef NamespaceOriginal$, ByRef NamespaceObfuscated$, TypeOriginal$, TypeObfuscated$)
            Dim ModuleDef As ModuleDefinition = TypeDef.Module


            'If ModuleDef.HasTypes Then
            '    Dim lst = ModuleDef.Types.ToList
            '    lst.Sort(Function(a, b)
            '                 If b.FullName.Length <> a.FullName.Length Then Return b.FullName.Length.CompareTo(a.FullName.Length)
            '                 Return b.FullName.CompareTo(a.FullName)
            '             End Function)

            '    For Each EmbRes As Resource In ModuleDef.Resources
            '        Dim nam = EmbRes.Name
            '        Mapping.NameToResource(nam) = EmbRes
            '        Dim idx = nam.LastIndexOf(".")
            '        If idx > 0 Then
            '            Mapping.NameToResource(nam.Substring(0, idx)) = EmbRes
            '        End If
            '    Next

            '    Dim OldInfoType = New Dictionary(Of String, TypeDefinition)
            '    For Each Info In lst
            '        OldInfoType(Info.FullName) = Info
            '    Next

            '    For Each types In ModuleDef.Types
            '        For Each meths In types.Methods
            '            If meths.HasBody Then
            '                Dim instrs = meths.Body.Instructions
            '                For i As Integer = 0 To instrs.Count - 1
            '                    Dim instr = instrs(i)
            '                    If instr.OpCode <> OpCodes.Ldstr Then Continue For
            '                    Dim codeString = CStr(instr.Operand)
            '                    If String.IsNullOrEmpty(codeString) Then Continue For
            '                    Dim res As Resource = Nothing
            '                    If Not Mapping.NameToResource.TryGetValue(codeString, res) Then Continue For
            '                    Dim typeInfo As TypeDefinition = Nothing
            '                    If Not OldInfoType.TryGetValue(codeString, typeInfo) Then Continue For
            '                    Dim newName = typeInfo.FullName




            '                    Dim renameCodeString As Boolean = IsCallingResourceManagerCtor(instrs, i, typeInfo)
            '                    If renameCodeString Then
            '                        MsgBox(instr.Operand.ToString & vbNewLine &
            '                               newName)
            '                        instr.Operand = newName
            '                    End If

            '                    'Dim renameCodeString As Boolean = [module].ObfuscatedFile.RenameResourcesInCode OrElse IsCallingResourceManagerCtor(instrs, i, typeInfo)
            '                    'If Not renameCodeString Then Logger.v("Possible resource name in code: '{0}' => '{1}' in method {2}", Utils.RemoveNewlines(codeString), newName, Utils.RemoveNewlines(method)) Else instr.Operand = newName Logger.v("Renamed resource string in code: '{0}' => '{1}' ({2})", Utils.RemoveNewlines(codeString), newName, Utils.RemoveNewlines(method))
            '                Next
            '            End If


            '        Next
            '    Next
            'End If





            For Each EmbRes As Resource In ModuleDef.Resources
                If Utils.isStronglyTypedResourceBuilder(TypeDef) Then
                    If NamespaceOriginal.EndsWith(".My.Resources") Then
                        If EmbRes.Name = NamespaceOriginal.Replace(".My.Resources", "") & "." & TypeOriginal & ".resources" Then
                            RenameResourceName(EmbRes, NamespaceObfuscated, TypeObfuscated)
                        End If
                    Else
                        If EmbRes.Name = NamespaceOriginal & "." & TypeOriginal & ".resources" Then
                            RenameResourceName(EmbRes, NamespaceObfuscated, TypeObfuscated)
                        End If
                    End If
                Else
                    If EmbRes.Name = NamespaceOriginal & "." & TypeOriginal & ".resources" Then
                        RenameResourceName(EmbRes, NamespaceObfuscated, TypeObfuscated)
                    Else

                        If EmbRes.Name.StartsWith(NamespaceOriginal) AndAlso Not EmbRes.Name.ToLower.EndsWith(".resources") Then

                            '###############################
                            'If Not NamespaceOriginal = String.Empty Then
                            '    'Dim newTypeName = NamespaceObfuscated & "." & TypeObfuscated
                            '    Dim oldstr = EmbRes.Name
                            '    oldstr = oldstr.Replace(NamespaceOriginal, NamespaceObfuscated)
                            '    Dim newstr = oldstr.Replace(TypeOriginal, TypeObfuscated)
                            '    EmbRes.Name = newstr

                            'End If


                            'Dim oldStr = EmbRes.Name
                            'If NamespaceOriginal.Contains(".") Then
                            '    Dim nms = NamespaceOriginal.Split(".")

                            '    For Each n In nms
                            '        oldStr = oldStr.Replace(n, NamespaceObfuscated)
                            '    Next
                            'Else

                            'End If

                            'Dim oldStr = EmbRes.Name
                            'If Not NamespaceOriginal = String.Empty Then
                            '    oldStr = oldStr.Replace(NamespaceOriginal, NamespaceObfuscated)
                            'End If

                            'Dim newstr = oldStr.Replace(TypeOriginal, TypeObfuscated)

                            ''MsgBox(newstr)
                            ''Dim newstr = oldStr.Replace(NamespaceOriginal, NamespaceObfuscated).Replace(TypeOriginal, TypeObfuscated)
                            ''MsgBox(EmbRes.Name & vbNewLine & newstr)

                            'EmbRes.Name = newstr
                            '"###########################################################"

                        End If
                        '######################################################
                        'If NamespaceOriginal = "MetroFramework" Then
                        '    MsgBox(NamespaceOriginal & "." & TypeOriginal & vbNewLine & EmbRes.Name)
                        'End If

                        'If EmbRes.Name.StartsWith(NamespaceOriginal & "." & TypeOriginal) Then
                        '    MsgBox(EmbRes.Name)
                        'End If
                        '##############################################################################"
                    End If
                End If
            Next

            'If TypeDef.HasMethods Then
            '    For Each method In TypeDef.Methods
            '        If method.HasBody Then
            '            For Each inst In method.Body.Instructions
            '                If inst.OpCode = OpCodes.Ldstr Then
            '                    If NamespaceOriginal.EndsWith(".My.Resources") Then
            '                        If inst.Operand.ToString() = (NamespaceOriginal.Replace(".My.Resources", "") & ".Resources") Then
            '                            inst.Operand = If(NamespaceObfuscated = String.Empty, TypeObfuscated, NamespaceObfuscated & "." & TypeObfuscated)
            '                        End If
            '                    Else
            '                        If inst.Operand.ToString() = (NamespaceOriginal & "." & TypeOriginal) Then
            '                            inst.Operand = If(NamespaceObfuscated = String.Empty, TypeObfuscated, NamespaceObfuscated & "." & TypeObfuscated)
            '                        End If
            '                    End If
            '                End If
            '            Next
            '        End If
            '    Next
            'End If


            Dim types = ModuleDef.Types
            For Each td In types
                If td.HasMethods Then
                    For Each method In TypeDef.Methods
                        If method.HasBody Then
                            For Each inst In method.Body.Instructions
                                If inst.OpCode = OpCodes.Ldstr Then
                                    If NamespaceOriginal.EndsWith(".My.Resources") Then
                                        If inst.Operand.ToString() = (NamespaceOriginal.Replace(".My.Resources", "") & ".Resources") Then
                                            inst.Operand = If(NamespaceObfuscated = String.Empty, TypeObfuscated, NamespaceObfuscated & "." & TypeObfuscated)
                                        End If
                                    Else
                                        If inst.Operand.ToString() = (NamespaceOriginal & "." & TypeOriginal) Then
                                            inst.Operand = If(NamespaceObfuscated = String.Empty, TypeObfuscated, NamespaceObfuscated & "." & TypeObfuscated)
                                        End If
                                    End If
                                End If
                            Next
                        End If
                    Next
                End If
            Next

        End Sub


        'Private Shared Function IsCallingResourceManagerCtor(ByVal instrs As IList(Of Instruction), ByVal ldstrIndex As Integer, ByVal typeInfo As TypeDefinition) As Boolean
        '    Try
        '        Dim index As Integer = ldstrIndex + 1
        '        Dim ldtoken = instrs(Math.Min(System.Threading.Interlocked.Increment(index), index - 1))
        '        If ldtoken.OpCode.Code <> Code.Ldtoken Then Return False


        '        'If Not New SigComparer().Equals(typeInfo.type.TypeDef, TryCast(ldtoken.Operand, ITypeDefOrRef)) Then Return False
        '        If Not CheckCalledMethod(instrs(Math.Min(System.Threading.Interlocked.Increment(index), index - 1)), "System.Type", "(System.RuntimeTypeHandle)") Then Return False
        '        If Not CheckCalledMethod(instrs(Math.Min(System.Threading.Interlocked.Increment(index), index - 1)), "System.Reflection.Assembly", "()") Then Return False
        '        Dim newobj = instrs(Math.Min(System.Threading.Interlocked.Increment(index), index - 1))
        '        If newobj.OpCode.Code <> Code.Newobj Then Return False
        '        If newobj.Operand.ToString() <> "System.Void System.Resources.ResourceManager::.ctor(System.String,System.Reflection.Assembly)" Then Return False
        '        Return True
        '    Catch __unusedArgumentOutOfRangeException1__ As ArgumentOutOfRangeException
        '        Return False
        '    Catch __unusedIndexOutOfRangeException2__ As IndexOutOfRangeException
        '        Return False
        '    End Try
        'End Function

        'Private Shared Function CheckCalledMethod(ByVal instr As Instruction, ByVal returnType As String, ByVal parameters As String) As Boolean
        '    If instr.OpCode.Code <> Code.[Call] AndAlso instr.OpCode.Code <> Code.Callvirt Then Return False
        '    Return IsMethod(TryCast(instr.Operand, MethodDefinition), returnType, parameters)
        'End Function

        'Private Shared Function IsMethod(ByVal method As MethodDefinition, ByVal returnType As String, ByVal parameters As String) As Boolean
        '    Return method IsNot Nothing AndAlso method.FullName = returnType & " " & method.DeclaringType.FullName & "::" & method.Name & parameters
        'End Function


        Private Shared Sub RenameResourceName(EmbRes As Resource, NamespaceObfuscated As String, TypeObfuscated As String)
            EmbRes.Name = If(NamespaceObfuscated = String.Empty, TypeObfuscated & ".resources", NamespaceObfuscated & "." & TypeObfuscated & ".resources")
        End Sub

        ''' <summary>
        ''' INFO : Rename embedded Resources from Resources dir and from ResourcesManager method.
        ''' </summary>
        ''' <param name="typeDef"></param>
        Friend Shared Sub RenameResourceManager(typeDef As TypeDefinition)
            For Each pr In (From p In typeDef.Properties
                            Where Not p.GetMethod Is Nothing AndAlso p.GetMethod.Name = "get_ResourceManager" AndAlso p.GetMethod.HasBody AndAlso p.GetMethod.Body.Instructions.Count <> 0
                            Select p)
                For Each instruction In pr.GetMethod.Body.Instructions
                    If TypeOf instruction.Operand Is String Then
                        Dim NewResManagerName$ = instruction.Operand
                        For Each EmbRes As EmbeddedResource In typeDef.Module.Resources
                            If EmbRes.Name = instruction.Operand & ".resources" Then
                                NewResManagerName = Randomizer.GenerateNew()
                                EmbRes.Name = NewResManagerName & ".resources"
                            End If
                        Next
                        instruction.Operand = NewResManagerName
                    End If
                Next
            Next
        End Sub

        Friend Shared Sub RenameSettings(mDef As MethodDefinition, originalN$, obfuscatedN$)
            If Not mDef Is Nothing Then
                If Not mDef.DeclaringType.BaseType Is Nothing AndAlso mDef.DeclaringType.BaseType.Name = "ApplicationSettingsBase" Then
                    If mDef.HasBody AndAlso mDef.Body.Instructions.Count <> 0 Then
                        For Each instruction In mDef.Body.Instructions
                            If TypeOf instruction.Operand Is String Then
                                Dim Name$ = instruction.Operand
                                If originalN = Name Then
                                    If mDef.Name.StartsWith("set_") Then
                                        mDef.Name = "set_" & obfuscatedN
                                    ElseIf mDef.Name.StartsWith("get_") Then
                                        mDef.Name = "get_" & obfuscatedN
                                    End If
                                    instruction.Operand = obfuscatedN
                                End If
                            End If
                        Next
                    End If
                End If
            End If
        End Sub

        ''' <summary>
        ''' INFO : Rename Property.
        ''' </summary>
        ''' <param name="prop"></param>
        ''' <param name="obfuscatedN"></param>
        Friend Shared Sub RenameProperty(ByRef prop As PropertyDefinition, obfuscatedN$)
            prop.Name = Mapping.RenamePropertyMember(prop, obfuscatedN)

            If Not prop.GetMethod Is Nothing Then
                Dim meth = RenameMethod(prop.DeclaringType, prop.GetMethod)
                RenameParameters(meth)
                RenameVariables(meth)
            End If
            If Not prop.SetMethod Is Nothing Then
                Dim meth = RenameMethod(prop.DeclaringType, prop.SetMethod)
                RenameParameters(meth)
                RenameVariables(meth)
            End If

            For Each m In (From p In prop.OtherMethods
                           Where Not p Is Nothing AndAlso NameChecker.IsRenamable(p)
                           Select p)
                Dim meth = RenameMethod(prop.DeclaringType, m)
                RenameParameters(meth)
                RenameVariables(meth)
            Next
        End Sub

        ''' <summary>
        ''' INFO : Rename Field.
        ''' </summary>
        ''' <param name="field"></param>
        ''' <param name="obfuscatedN"></param>
        Friend Shared Sub RenameField(field As FieldDefinition, obfuscatedN$)
            field.Name = Mapping.RenameFieldMember(field, obfuscatedN)
        End Sub

        ''' <summary>
        ''' INFO : Rename Event.
        ''' </summary>
        ''' <param name="events"></param>
        ''' <param name="obfuscatedN"></param>
        Friend Shared Sub RenameEvent(ByRef events As EventDefinition, obfuscatedN$)
            events.Name = Mapping.RenameEventMember(events, obfuscatedN)
        End Sub

        ''' <summary>
        ''' INFO : Rename CustomAttributes.
        ''' </summary>
        ''' <remarks>
        ''' REMARKS : Only AccessedThroughPropertyAttribute attribute is renamed to prevent de4Dot to retrieve original names.
        ''' </remarks>
        ''' <param name="type"></param>
        ''' <param name="prop"></param>
        ''' <param name="originalN"></param>
        ''' <param name="obfuscatedN"></param> 
        Friend Shared Sub RenameCustomAttributes(type As TypeDefinition, prop As PropertyDefinition, originalN$, obfuscatedN$)
            If type.HasFields Then
                For Each field As FieldDefinition In (From f In type.Fields
                                                      Where f.IsPrivate AndAlso f.HasCustomAttributes
                                                      Select f)
                    For Each ca In (From c In field.CustomAttributes
                                    Where c.AttributeType.Name = "AccessedThroughPropertyAttribute" AndAlso c.HasConstructorArguments AndAlso c.ConstructorArguments(0).Value = originalN
                                    Select c)
                        ca.ConstructorArguments(0) = New CustomAttributeArgument(ca.AttributeType, obfuscatedN)
                        RenameProperty(prop, obfuscatedN)
                        Exit For
                    Next
                Next
            End If
            RenameCustomAttributesValues(prop)
        End Sub

        Friend Shared Sub RenameCustomAttributesValues(member As Object)
            If member.HasCustomAttributes Then
                For Each ca As CustomAttribute In member.CustomAttributes
                    If Not ca Is Nothing Then
                        If ca.AttributeType.Name = "CategoryAttribute" OrElse ca.AttributeType.Name = "DescriptionAttribute" Then
                            If ca.HasConstructorArguments Then
                                ca.ConstructorArguments(0) = New CustomAttributeArgument(ca.AttributeType, Randomizer.GenerateNew())
                            End If
                        End If
                    End If
                Next
            End If
        End Sub

        Friend Shared Sub RenameInitializeComponentsValues(TypeDef As TypeDefinition, OriginalKeyName$, NewKeyName$, Properties As Boolean)
            Dim methodSearch As MethodDefinition = Finder.FindMethod(TypeDef, "InitializeComponent")
            If Not methodSearch Is Nothing Then
                If methodSearch.HasBody Then
                    If methodSearch.Body.Instructions.Count <> 0 Then
                        For Each instruction As Cil.Instruction In methodSearch.Body.Instructions
                            If TypeOf instruction.Operand Is String Then
                                If Properties Then
                                    If Not instruction.Previous Is Nothing Then
                                        If instruction.Previous.OpCode = OpCodes.Callvirt AndAlso instruction.Previous.Operand.ToString.EndsWith("get_" & OriginalKeyName & "()") Then
                                            If CStr(instruction.Operand) = OriginalKeyName Then
                                                instruction.Operand = NewKeyName
                                            End If
                                        End If
                                    End If
                                Else
                                    If Not instruction.Next Is Nothing Then
                                        If instruction.Next.OpCode = OpCodes.Callvirt AndAlso instruction.Next.ToString.EndsWith("set_Name(System.String)") Then
                                            If CStr(instruction.Operand) = OriginalKeyName Then
                                                instruction.Operand = NewKeyName
                                            End If
                                        End If
                                    End If
                                End If
                            End If
                        Next
                    End If
                End If
            End If
        End Sub
#End Region

    End Class
End Namespace
