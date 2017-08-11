Namespace Core.Obfuscation.Exclusion
    Public NotInheritable Class ExclusionState

#Region " Enumerations "
        Enum mType
            Namespaces = 0
            Types = 1
            Methods = 2
            Properties = 3
            Events = 4
            Fields = 5
        End Enum
#End Region

#Region " Properties "
        Public Property exclude As Boolean
        Public Property member As Object
        Public Property memberType As mType
        Public Property allEntities As Boolean
        Public Property stringEncrypt As Boolean
        Public Property integerEncoding As Boolean
        Public Property booleanEncrypt As Boolean
        Public Property Renaming As Boolean
        Public Property ControlFlow As Boolean
        Public Property hideCalls As Boolean
#End Region

#Region " Constructor "
        Public Sub New(exclud As Boolean, memb As Object, memberT As mType, Optional ByVal allEntit As Boolean = False,
               Optional ByVal stringEncr As Boolean = False, Optional ByVal integerEncod As Boolean = False,
               Optional ByVal booleanEncr As Boolean = False, Optional ByVal Renamin As Boolean = False,
               Optional ByVal ctrlFlow As Boolean = False, Optional ByVal HideCall As Boolean = False)
            _exclude = exclud
            _member = memb
            _memberType = memberT
            _allEntities = allEntit
            _stringEncrypt = stringEncr
            _integerEncoding = integerEncod
            _booleanEncrypt = booleanEncr
            _Renaming = Renamin
            _ControlFlow = ctrlFlow
            _hideCalls = HideCall
        End Sub

        Public Sub New(stringEncr As Boolean, integerEncod As Boolean, booleanEncr As Boolean, Renamin As Boolean, CtrlFlow As Boolean, HideCall As Boolean)
            _stringEncrypt = stringEncr
            _integerEncoding = integerEncod
            _booleanEncrypt = booleanEncr
            _Renaming = Renamin
            _ControlFlow = CtrlFlow
            _hideCalls = HideCall
        End Sub
#End Region

    End Class
End Namespace
