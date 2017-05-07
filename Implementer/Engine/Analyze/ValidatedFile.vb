Imports Helper.AssemblyHelper
Imports dnlib

Namespace Engine.Analyze
    Public NotInheritable Class ValidatedFile
        Inherits EventArgs

#Region " Properties "
        Public ReadOnly Property peInfos As PeReader
        Public ReadOnly Property isValid As Boolean
        Public ReadOnly Property assembly As Data
#End Region

#Region " Constructor "
        Public Sub New(isvalid As Boolean, peInfos As PeReader, ass As Data)
            _isValid = isvalid
            _assembly = ass
            _peInfos = peInfos
        End Sub
#End Region

    End Class
End Namespace